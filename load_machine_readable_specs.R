specs_prepped_path <- here("public-resources/specs_prepped.rds")

if(file.exists(specs_prepped_path)) {
  specs_prepped <- readRDS(specs_prepped_path)
  
  specs_rules <- specs_prepped$specs_rules
  reporting_info <- specs_prepped$reporting_info
  valid_values <- specs_prepped$valid_values
  rm(specs_prepped)
} else {

validation_specs_bk <- here("public-resources/FY26 HMIS-CSV-Machine-Readable-Specifications.xlsx")

# Import Specs -----------------------
## Import primary specs sheet (includes expected CSVs, columns, data types, and rule notes) ------
## We need to reformat to handle the double header
raw <- readxl::read_xlsx(validation_specs_bk, col_names = FALSE)
raw[1, ] <- t(tidyr::fill(data.frame(t(raw[1, ])), everything()))

header <- paste(raw[1,], raw[2,], sep = "_") |>
  gsub("NA_|_NA", "", x = _)

cols_and_data_types <- raw[-(1:2),] |> 
  setNames(header) |>
  qDT()

## Import Keys tab -----------------
# Includes the Anchor ID (EnrollmentID or ProjectID) used for each CSV, which is used to help the user look up data for that CSV
# Also includes the Key Date fields from that CSV, also used for lookups in the hmis system.
keys_and_anchors <- readxl::read_xlsx(validation_specs_bk, sheet = "Keys") |>
  qDT() |>
  fsubset(!is.na(AnchorID) & AnchorID != "N/A")

## Derive unique ID column per CSV ---------------
# (e.g. AssessmentID for Assessment.csv) 
unique_id_lookup <- cols_and_data_types |>
  fsubset(toupper(Notes) == "UNIQUE IDENTIFIER") |>
  fselect(CSV, Name) |>
  setNames(c("CSV", "UniqueID"))

# Prepare reporting_info dataset ------------------------
# This includes, for each csv+column+check combination, info needed for reporting 
# (report source - FSA, DQ, or PDDE - whether we need to include the Anchor ID 
# in the report, and any other key fields that will help the user)

## Pivot to long (CSV + Column + check) -----------------
# The multi-header gives you one group of three columns per check type:
#   Source, Include AnchorID?, Key Fields
# Pivot gives us: CSV, Name, check_type, Source, Include, AnchorID, Key Fields
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

## Resolve key fields and AnchorID --------------
reporting_info <- reporting_info |>
  join(unique_id_lookup, on = "CSV", how = "left") |>
  join(keys_and_anchors, on = "CSV", how = "left") |>
  fmutate(
    `Key Fields` = `Key Fields` %>%
      str_replace("UniqueID", UniqueID) %>%
      str_replace("KeyDate", KeyDate),
    AnchorID = fifelse(`Include AnchorID?` == TRUE, AnchorID, NA)
  ) |>
  fselect(-c(UniqueID, `Include AnchorID?`))

## pull in Evachecks info -------------
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
  frename(
    "check_priority" = Type,
    "reporting_notes" = Notes
  )


## Import Detail Text templates ------------
reporting_info <- reporting_info |>
  join(
    readxl::read_xlsx(validation_specs_bk, sheet = "Detail Texts"),
    on = c("check_type" = "Check Type")
  ) 
  
# Read in CSV Lists from Specs ---------------------
# This tab contains all the valid values for each list element
valid_values <- readxl::read_excel(validation_specs_bk, sheet = "CSV Lists FY2026") %>%
  fmutate(
    Value = gsub("\u00A0", "", Value),
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

# Read in Validation Info ----------------
# This tab includes all csvs, columns, data types, 
# corresponding valid values list info, and additional validation rule notes
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
  ) |>
  frename("validation_notes" = Notes)

# Special validation rules -----
# These could not be easily coded into the specs file itself
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
  Disabilities = list(
    "Non-Null Invalid" = list(
      DisabilityResponse = quote(
        (DisabilityType == 10 & !DisabilityResponse %in% valid_values[["4.10.2"]]) |
        (DisabilityType != 10 & !DisabilityResponse %in% valid_values[["1.8"]])
      )
    )
  ),
  Enrollment = list(
    "Non-Null Invalid" = list(
      # VAMCStation   = quote(!VAMCStation %in% valid_values[["V6.1"]]),
      EnrollmentCoC = quote(!grepl("^[A-Za-z]{2}-[0-9]{3}$", EnrollmentCoC) | (ContinuumProject == 1 & .join == "x"))
    )
  ),
  Exit = list(
    "Non-Null Invalid" = list(
      SubsidyInformation = quote(
        (HousingAssessment == 1 & !SubsidyInformation %in% c(1,2,3,4)) |
          (HousingAssessment == 2 & !SubsidyInformation %in% c(11,12))
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
      },
      SubTypeProvided = quote(
        (TypeProvided == 3 & !SubTypeProvided %in% valid_values[["V2.A"]]) |
          (TypeProvided == 4 & !SubTypeProvided %in% valid_values[["V2.B"]]) |
          (TypeProvided == 5 & !SubTypeProvided %in% valid_values[["V2.C"]])
      )
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

# Supplemental files needed for specific CSVs -----------
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
  ),
  Disabilities = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Funder",     on = "ProjectID",    cols = "Funder")
  ),
  IncomeBenefits = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Funder", on = "ProjectID", cols = "Funder")
  ),
  Organization = list(
    list(tbl = "Project", on = "OrganizationID", cols = "ProjectID")
  )
)

# Funder-specific Null Unless Fields -------------------
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


# Create Master Rules table ----------------
# Includes all validation and reporting info
# We express the rules in R code that will be evaluated at runtime.
specs_rules <- validation_info %>%
  join(reporting_info, on = c("CSV", "Name"), multiple=TRUE) %>%
  # Initialize an empty list column to hold the parsed expressions
  fmutate(rule_expr = list(NULL)) %>%
  join(
    funder_specific_null_unless_fields,
    on = c("CSV", "Name"),
    column = "funder_specific" # adds a column indicating 'x' or 'y' matches
  ) %>%
  fmutate(
    # Clean up the Notes string just for Null Unless checks
    validation_notes = fifelse(
      check_type == "Null Unless", 
      str_split_i(validation_notes, "\r\n", 1), 
      validation_notes
    ),
    # Append the Funder logic to the text BEFORE parsing
    validation_notes = fifelse(
      check_type == "Null Unless" & funder_specific == "y", 
      paste0(validation_notes, " & Funder %in% c(13:19)"), 
      validation_notes
    )
  )

## 1. Null Unless  ------------
specs_rules[
  check_type == "Null Unless", 
  rule_expr := Map(clean_rule_for_null_unless, Name, validation_notes)
]

## 2. String Length Limit Exceeded -----------
specs_rules[
  check_type == "String Length Limit Exceeded", 
  rule_expr := Map(function(col, limit) 
    rlang::parse_expr(glue::glue("vlengths({col}) > {limit}")),
    Name, 
    str_len_limit
  )
]

## 3. Unallowed Null -----------
specs_rules[
  check_type == "Unallowed Null", 
  rule_expr := lapply(Name, function(col) 
    rlang::parse_expr(glue::glue("is.na({col})"))
  )
]

## 4. Non-Null Invalid -----------
specs_rules[
  check_type == "Non-Null Invalid" & !is.na(List), 
  rule_expr := Map(function(col, lst)
    # Creates: !is.na(Name) & !(Name %in% valid_values[['ListID']])
    rlang::parse_expr(glue::glue("!is.na({col}) & !({col} %in% valid_values[['{lst}']])")),
    Name, 
    List
  )
]

## 5. Foreign Key Missing -----------
specs_rules[
  check_type == "Foreign Key Missing",
  `:=`(
    fk_id_col   = sub(".*Must match a ([^ ]+) in [^ ]+\\.csv.*", "\\1", validation_notes),
    foreign_tbl = sub(".*Must match a [^ ]+ in ([^ ]+)\\.csv.*", "\\1", validation_notes)
  )
]

specs_rules[
  check_type == "Foreign Key Missing", 
  rule_expr := Map(function(c, fc, ft)
   rlang::parse_expr(glue::glue("!is.na({c}) & !({c} %in% get('{ft}')[['{fc}']])")), 
   Name, 
   fk_id_col, 
   foreign_tbl
  )
]

# ignore User ID
specs_rules <- specs_rules[!(check_type == "Foreign Key Missing" & Name == "UserID")]

## 6. Duplicate UniqueID ---------
specs_rules[
  check_type == "Duplicate UniqueID", 
  rule_expr := lapply(Name, function(col)
    # Using all = TRUE ensures BOTH the original and the duplicate are flagged
    rlang::parse_expr(glue::glue("fduplicated({col}, all = TRUE)"))
  )
]


## 7. Special rules --------------
# overwrites the rule_expr column with the new special rule
specs_rules[
  special_validation_rules_dt,
  on = c("CSV", "Name", "check_type" = "Issue"),
  rule_expr := i.rule
]

saveRDS(
  list(
    specs_rules = specs_rules, 
    reporting_info = reporting_info, 
    valid_values = valid_values
  ), 
  specs_prepped_path
)
}