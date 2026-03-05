validation_specs_bk <- here("public-resources/FY26 HMIS-CSV-Machine-Readable-Specifications.xlsx")

# Read and reshape the multi-row header
import_cols_and_data_types <- function() {
  raw <- readxl::read_xlsx(validation_specs_bk, col_names = FALSE)
  raw[1, ] <- t(tidyr::fill(data.frame(t(raw[1, ])), everything()))
  
  header <- paste(raw[1,], raw[2,], sep = "_") |>
    gsub("NA_|_NA", "", x = _) # |>  # clean up where one row was empty
    # make.names(unique = TRUE)
  
  specs <- raw[-(1:2),] |> setNames(header)
  return(specs)
}

cols_and_data_types <- import_cols_and_data_types() %>%
  qDT()

# Read the Keys tab
csv_anchor_keys <- readxl::read_xlsx(validation_specs_bk, sheet = "Keys") |>
  fsubset(!is.na(AnchorID) & AnchorID != "N/A")

anchor_id_lookup <- csv_anchor_keys |> fselect(CSV, AnchorID)
key_date_lookup <- csv_anchor_keys |> fselect(CSV, KeyDate)

# Derive the unique ID column per CSV
unique_id_lookup <- cols_and_data_types |>
  fsubset(toupper(Notes) == "UNIQUE IDENTIFIER") |>
  fselect(CSV, Name) |>
  setNames(c("CSV", "UniqueID"))

# Parse the check-type columns into a long format rule table
# The multi-header gives you one group of three columns per check type (Source, Include AnchorID?, Key Fields). Pivot these long so each row is one column-×-check-type combination:
# Convert to long format: CSV, Name, check_type, Source, Include, AnchorID, Key Fields
reporting_info <- cols_and_data_types %>%
  fselect(-c(`DE#`, Type, List, Null, Notes, Order)) |>
  pivot_longer(
    cols      = -c(CSV, Name),  # keep the identity columns
    names_to  = c("check_type", ".value"),
    names_sep = "_"
  ) |>
  qDT() |>
  fsubset(Source != "N/A") |>
  fmutate(
    `Include AnchorID?` = gsub("N/A", NA, `Include AnchorID?`),
    `Key Fields` = gsub("N/A", NA, `Key Fields`),
    Source = gsub("N/A", NA, Source)
  )

# Resolve key fields and AnchorID
reporting_info <- reporting_info |>
  join(unique_id_lookup, on = "CSV", how = "left") |>
  join(key_date_lookup, on = "CSV", how = "left") |>
  join(anchor_id_lookup, on = "CSV", how = "left") |>
  fmutate(
    `Key Fields` = `Key Fields` %>%
      str_replace("UniqueID", UniqueID) %>%
      str_replace("KeyDate", KeyDate),
    AnchorID = fifelse(`Include AnchorID?` == TRUE, AnchorID, NA)
  ) |>
  fselect(-c(UniqueID, `Include AnchorID?`))

# pull in Evachecks info
specs_evachecks_issue_xwalk <- c(
  "Incorrect Data Type" = "Incorrect Data Type",
  "Non-Null Invalid" = "Invalid Non-Null Value",
  "Duplicate UniqueID" = "Duplicate unique identifiers",
  "Column Mispelled/Misordered/Missing/Extra" = "Incorrect or Misordered Columns",
  "Null Unless" = "Null Unless",
  "Impermissible Characters" = "Impermissible Characters",
  "String Length Limit Exceeded" = "Value length exceeds character limit",
  "Foreign Key Missing" = "Identifier does not match across files",
  "Unallowed Null" = "Nulls not allowed"
)

reporting_info <- reporting_info |>
  fmutate(
    Source = fcase(
      Source == "FSA", "file structure",
      Source == "DQ", "dq",
      Source == "PDDE", "pdde"
    ),
    Issue = specs_evachecks_issue_xwalk[check_type]
  ) |>
  join(evachecks, on = c("Source", "Issue")) |>
  join(
    readxl::read_xlsx(validation_specs_bk, sheet = "Detail Texts"),
    on = c("check_type" = "Check Type")
  ) 

  
# Read in CSV Lists from Specs
# This tab contains all the valid values for each list element
valid_values <- readxl::read_excel(validation_specs_bk, sheet = "CSV Lists FY2026") %>%
  fmutate(
    Value = as.numeric(gsub("\u00A0", "", Value)),
    List = ifelse(
      List == "1.1000000000000001", "1.1", 
      ifelse(
        List == "4.1399999999999997", "4.14",
        List
      )
    )
  )

# Convert to a named list, where the names are the unique values of `List` and the values are the vector of `Value`s.
valid_values <- split(valid_values$Value, valid_values$List)

# hashed cols are 32 chars long
hashed_cols <- 

# Read in Variable-List xwalk
# This tab includes all variables and the corresponding List element that determines valid values
# It also includes nuanced notes about validation, including conditional validations
validation_info <- cols_and_data_types %>%
  fselect(CSV, `DE#`, Name, Type, List, Null, Notes, Order, additional_notes) %>%
  fsubset(!CSV %in% c("AssessmentResults","AssessmentQuestions")) %>%
  fmutate(
    Type = gsub("\u00A0", "", Type),
    List = str_trim(ifelse(List == "1.1000000000000001", "1.1", List)),
    Name = gsub("\u00A0", "", Name),
    Name = ifelse(Name == "SSN[1]", "SSN", ifelse(Name == "Geocode[1]", "Geocode", Name)),
    valid_values = valid_values[List],
    Type = fifelse(
      Name %in% c("FirstName","MiddleName","LastName","NameSuffix","SSN"), 
      "S64", # see additional notes for these columns
      Type
    ), 
    is_str = substr(Type, 1, 1) == "S" & Type != "S",
    str_len_limit = fifelse(is_str, as.numeric(sub("S", "", "S32")), NA),
    nulls_allowed = Null == "Y" & !is.na(Null)
  ) 

# Special validation rules -----
special_validation_rules <- list(
  CEParticipation = list(
    "Unallowed Null" = list(
      PreventionAssessment = quote(is.na(PreventionAssessment) & AccessPoint == 1),
      CrisisAssessment     = quote(is.na(CrisisAssessment)     & AccessPoint == 1),
      HousingAssessment    = quote(is.na(HousingAssessment)    & AccessPoint == 1),
      DirectServices       = quote(is.na(DirectServices)       & AccessPoint == 1)
    )
  ),
  Client = list(
    "Non-Null Invalid" = list(
      RaceNone = function(dt) {
        race_cols_in_dt <- intersect(race_cols, names(dt))
        sum_race_cols <- rowSums(
          get_vars(dt, setdiff(race_cols_in_dt, "RaceNone")),
          na.rm = TRUE
        )
        (!is.na(dt$RaceNone) & sum_race_cols %between% c(1, 98)) |
          (is.na(dt$RaceNone) & (sum_race_cols == 0 | sum_race_cols >= 99))
      }
    )
  ),
  CurrentLivingSituation = list(
    "Unallowed Null" = list(
      VerifiedBy = quote(is.na(VerifiedBy) & ProjectType == 14)
    )
  ),
  Enrollment = list(
    "Non-Null Invalid" = list(
      VAMCStation   = quote(!VAMCStation %in% valid_values[["V6.1"]]),
      EnrollmentCoC = quote(!grepl("^[A-Za-z]{2}-[0-9]{3}$", EnrollmentCoC) | (ContinuumProject == 1 & .join == "x"))
    )
  ),
  Exit = list(
    "Non-Null Invalid" = list(
      SubsidyInformation = quote(
        (HousingAssessment == 1 & SubsidyInformation %in% c(1,2,3,4)) |
          (HousingAssessment == 2 & SubsidyInformation %in% c(11,12))
      ),
      SessionsInPlan     = quote(SessionsInPlan < 0),
      SessionCountAtExit = quote(SessionCountAtExit > 0)
    ),
    "Null Unless" = list(
      SessionCountAtExit = quote(CounselingReceived == 1)
    )
  ),
  Export = list(
    "Non-Null Invalid" = list(
      SourceID               = quote(SourceType == 1 & !grepl("^[A-Za-z]{2}-[0-9]{3}$", SourceID)),
      SourceContactPhone     = quote(!grepl("^[2-9][0-9]{2}[2-9][0-9]{2}[0-9]{4}$", SourceContactPhone)),
      SourceContactExtension = quote(!grepl("^[0-9]{1,5}$", SourceContactExtension)),
      SourceContactEmail     = quote(!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", SourceContactEmail))
    ),
    "Unallowed Null" = list(
      SourceName = quote(SourceType != 1 & is.na(SourceName))
    )
  ),
  Project = list(
    "Unallowed Null" = list(
      ProjectType            = quote(is.na(ProjectType) & ContinuumProject == 1),
      RRHSubType             = quote(is.na(RRHSubType) & ProjectType == 13),
      ResidentialAffiliation = quote(is.na(ResidentialAffiliation) & (ProjectType == 6 | (ProjectType == 13 & RRHSubType == 1)))
    )
  ),
  ProjectCoC = list(
    "Non-Null Invalid" = list(
      ZIP      = quote(!grepl("^[0-9]{5}$", ZIP)),
      Geocode  = quote(!grepl("^[0-9]{6}$", Geocode)),
      State    = quote(!grepl("^[a-zA-Z]{2}$", State))
    )
  ),
  Services = list(
    "Non-Null Invalid" = list(
      TypeProvided = function(dt) {
        record_type_list_lookup = c(
          "141" = "P1.2",
          "142" = "R14.2",
          "143" = "W1.2",
          "144" = "V2.2",
          "151" = "W2.2",
          "152" = "V3.3",
          "161" = "P2.2",
          "200" = "4.14",
          "210" = "V8.2",
          "300" = "C2.2"
        )
        valid_vals <- valid_values[record_type_list_lookup[as.character(dt$RecordType)]]
        !mapply(`%in%`, dt$TypeProvided, valid_vals)
      }
    )
  )
)

special_validation_rules_dt <- rbindlist(
  lapply(names(special_validation_rules), function(csv) {
    rbindlist(
      lapply(names(special_validation_rules[[csv]]), function(issue) {
        rbindlist(
          lapply(names(special_validation_rules[[csv]][[issue]]), function(name) {
            data.table(
              CSV   = csv,
              Issue = issue,
              Name  = name,
              rule  = list(special_validation_rules[[csv]][[issue]][[name]])
            )
          })
        )
      })
    )
  })
)

csv_join_prerequisites <- list(
  Inventory = list(
    list(tbl = "Project", on = "ProjectID", cols = "ProjectType")
  ),
  CurrentLivingSituation = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType")
  ),
  Enrollment = list(
    list(tbl = "ProjectCoC", on = c("ProjectID" = "ProjectID", "EnrollmentCoC" = "CoCCode"), column = TRUE),
    list(tbl = "Project",    on = "ProjectID",    cols = "ContinuumProject")
  )
)

# Get subsets of rules/info needed for various checks --------------------
# Include the special validation rules
add_special_validation_rules <- function(dt, issue_name) {
  dt %>%
    fmutate(Issue = issue_name) %>%
    join(
      special_validation_rules_dt %>% fsubset(Issue == issue_name), 
      on = c("CSV", "Name", "Issue"), 
      how = "anti"
    ) %>%
    rbind(
      special_validation_rules_dt %>% fsubset(Issue == issue_name), 
      fill = TRUE
    )
}


## String Length Limit Exceeded ------------
str_len_limit_rules <- validation_info %>%
  fsubset(is_str == TRUE, CSV, Name, str_len_limit) %>%
  add_special_validation_rules("String Length Limit Exceeded")

## Unallowed Null rules ------------
unallowed_null_info <- validation_info %>%
  fsubset(nulls_allowed == F) %>%
  add_special_validation_rules("Unallowed Null")

## Non-Null Invalid info ------------
non_null_invalid_info <- validation_info %>% 
  fsubset(whichNA(List, invert=TRUE)) %>%
  add_special_validation_rules("Non-Null Invalid")

# Null unless ------------
valid_list_lookup <- validation_info$Name
names(valid_list_lookup) <- validation_info$`DE#`
valid_list_lookup <- valid_list_lookup[!is.na(names(valid_list_lookup)) & !is.na(valid_list_lookup)]

null_unless <- function(col, cond) {
  (is.na(col) & cond) | (!is.na(col) & !cond)
}

clean_rule_for_null_unless <- function(Name, Notes) {
  stringi::stri_replace_all_fixed(
    gsub("^Null unless ", "", Notes),
    pattern     = names(valid_list_lookup),
    replacement = lookup,
    vectorize_all = FALSE
  ) %>%
    clean_text() %>%
    purrr::map2(Name, ., ~rlang::parse_expr(glue("null_unless({.x}, {.y})")))
}

null_unless_rules <- validation_info %>%
  fsubset(grepl("^Null unless", Notes), CSV, Name, Notes) %>%
  # See additional notes for these fields, whose Null checks within the 
  # Null Unless check only applies to Funders in c(13:19)
  join(
    funder_specific_null_unless_fields,
    on = c("CSV","Name"),
    column = "funder_specific"
  ) %>%
  fmutate(
    Notes = str_split_i(Notes, "\r\n", 1),
    Notes = fifelse(funder_specific == "y", paste0(Notes, " & Funder in c(13:19)"), Notes),
    rule = clean_rule_for_null_unless(Name, Notes)
  ) %>%
  add_special_validation_rules("Null Unless")
  

dq_null_unless_rules <- null_unless_rules %>%
  join(
    reporting_info %>% 
      fsubset(check_type == "Null Unless" & Source == "dq"), 
    on = c("CSV","Name"),
    how = "inner"
  )

pdde_null_unless_rules <- null_unless_rules %>%
  join(
    reporting_info %>% 
      fsubset(check_type == "Null Unless" & Source == "pdde"), 
    on = c("CSV","Name"),
    how = "inner"
  )
# Null Unless Function... ---------
# Will be used in DQ and PDDE

get_null_unless_issue_records <- function(csv_name, null_unless_rules, envir) {
  print(paste0("getting null unless issue records for csv ", csv_name))

  dt <- get(csv_name, envir = envir) %>%
    join_prereqs(csv_name, envir = envir)
  
  csv_null_unless_rules <- null_unless_rules %>%
    fsubset(CSV == csv_name & Name %in% names(dt))
  
  if(fnrow(csv_null_unless_rules) > 0) {
    rbindlist(lapply(seq_row(csv_null_unless_rules), function(i) {
      col <- csv_null_unless_rules[i, Name]
      rule <- csv_null_unless_rules[i, rule][[1]]
      
      invalid = eval(rule, dt)
      
      dt %>%
        fsubset(invalid == TRUE) %>%
        fmutate(Name = col)
    }))
  } else data.table()
}

foreign_key_checks <- validation_info %>%
  fsubset(grepl("Must match", Notes)) %>%
  fmutate(
    id_col   = sub("Must match a ([^ ]+) in [^ ]+\\.csv.*", "\\1", Notes),
    tbl_name = sub("Must match a [^ ]+ in ([^ ]+)\\.csv.*", "\\1", Notes)
  ) %>%
  join(
    reporting_info %>% fsubset(check_type == "Foreign Key Missing"), 
    on=c("CSV", "Name"),
    how = "inner"
  )

get_foreign_key_issues <- function(csv_name, reporting_source) {
  csv_foreign_key_checks <- foreign_key_checks %>%
    fsubset(
      CSV == csv_name & 
        Name %in% names(dt) & 
        Source == reporting_source
    )
  
  foreign_key_issues <- rbindlist(
    lapply(seq_row(csv_foreign_key_checks), function(i) {
      spec_row <- csv_foreign_key_checks[i]
      foreign_tbl <- get(spec_row$tbl_name)
      
      missing <- dt %>%
        join(
          foreign_tbl %>% fselect(spec_row$id_col),
          on   = setNames(spec_row$id_col, spec_row$Name),
          column = TRUE
        ) %>%
        fsubset(.join == "x") %>%
        fmutate(
          Name = spec_row$Name, 
          foreign_tbl = spec_row$tbl_name, 
        )
      
      # See additional_note for Inventory > ProjectID
      if(csv_name == "Inventory") {
        missing <- missing %>%
          funique(cols = "ProjectID")
      }
      return(missing)
    }),
    fill = TRUE
  )
}


join_prereqs <- function(dt, csv_name, envir) {
  prereqs <- csv_join_prerequisites[[csv_name]]
  if (!is.null(prereqs)) {
    for (prereq in prereqs) {
      foreign_dt <- get(prereq$tbl, envir=envir) |> fselect(c(unname(prereq$on), prereq$cols))
      dt <- dt |> 
        join(
          foreign_dt, 
          on =  prereq$on,
          how = prereq$how %||% "left",
          column = prereq$cols
        )
    }
  }
  return(dt)
}


add_reporting_info <- function(dt, reporting_source) {
  dt %>%
    join(column_priorities, on=c("CSV" = "File", "Name" = "Column")) %>%
    fmutate(check_type = factor(check_type)) %>%
    join(
      reporting_info %>% fsubset(Source == reporting_source),
      on=c("CSV", "Name", "check_type"),
      drop.dup.cols = "x"
    ) %>%
    fmutate(
      key_template = gsub("([A-Za-z0-9_.]+)", "\\1 {\\1}", `Key Fields`),
      detail_template = stringi::stri_replace_all_fixed(`Detail Text`, "{Key Field Info}", key_template) %>%
        stringi::stri_replace_all_fixed(., "{Value}", paste0("{", Name, "}"))
    ) %>%
    .[, Detail := as.character(glue_data(.SD, detail_template[1L])), by = detail_template] %>%
    .[, AnchorValue := if (is.na(AnchorID[1L]) || AnchorID[1L] == "") NA_character_
      else as.character(get(AnchorID[1L])),
      by = AnchorID] %>%
    fselect(c(issue_display_cols, "CSV", "Column" = "Name", "AnchorID", "AnchorValue")) %>%
    fmutate(
      Issue = factor(Issue),
      Type = factor(Type),
      Guidance = factor(Guidance),
      CSV = factor(CSV),
      Column = factor(Column),
      AnchorID = factor(AnchorID)
    )
}

# These are fields for whose Null checks (as part of the Null Unless checks)
# should only apply when Funder in c(13:19)
funder_specific_null_unless_fields <- list(
  Disabilities = c(
    "TCellCountAvailable", 
    "ViralLoadAvailable", 
    "AntiRetroviral"
  ),
  IncomeBenefits = c(
    "NoMedicaidReason",
    "NoMedicareReason",
    "NoSCHIPReason",
    "NoVHAReason",
    "NoEmployerProvidedReason",
    "NoCOBRAReason",
    "NoPrivatePayReason",
    "NoStateHealthInsReason",
    "NoIndianHealthServicesReason",
    "NoADAPReason",
    "NoRyanWhiteReason"
  )
) %>% 
  purrr::imap(\(names, csv) data.table(CSV = csv, Name = names)) |>
  rbindlist()