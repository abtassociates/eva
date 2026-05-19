specs_prepped_path <- here("public-resources/eva_specs_prepped.rds")

if(file.exists(specs_prepped_path)) {
  print("loading specs_prepped.rds file")
  specs_prepped <- readRDS(specs_prepped_path)
  
  specs_rules <- specs_prepped$specs_rules
  reporting_info <- specs_prepped$reporting_info
  valid_values <- specs_prepped$valid_values
  cols_and_data_types < specs_prepped$cols_and_data_types
  
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
# Pivot gives us: CSV, Name, issue_type, Source, Include, AnchorID, Key Fields
reporting_info <- cols_and_data_types %>%
  fselect(-c(`DE#`, Type, Null, Notes, Order)) |>
  pivot_longer(
    cols      = -c(CSV, List, Name),  # keep the identity columns
    names_to  = c("issue_type", ".value"),
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
  "Unallowed Null" = "Nulls not allowed",
  "Conditonally Invalid Value" = "Invalid Value"
)

reporting_info <- reporting_info |>
  fmutate(
    Source = fcase(
      Source == "FSA", "file structure",
      Source == "DQ", "dq",
      Source == "PDDE", "pdde"
    ),
    Issue = specs_evachecks_issue_xwalk[issue_type]
  ) |>
  join(evachecks, on = c("Source", "Issue")) |>
  frename(
    "reporting_notes" = Notes
  )

## Import Detail Text templates ------------
reporting_info <- reporting_info |>
  join(
    readxl::read_xlsx(validation_specs_bk, sheet = "Detail Texts"),
    on = c("issue_type" = "Issue Type")
  )
  
# Read in CSV Lists from Specs ---------------------
# This tab contains all the valid values for each list element
valid_values_df <- readxl::read_excel(validation_specs_bk, sheet = "CSV Lists FY2026") %>%
  fmutate(
    Value = gsub("\u00A0", "", Value),
    Value = trimws(Value),
    List = ifelse(
      List == "1.1000000000000001", "1.1", 
      ifelse(
        List == "4.1399999999999997", "4.14",
        List
      )
    )
  )

# Convert to a named list, where the names are the unique values of `List` and the values are the vector of `Value`s.
valid_values <- split(valid_values_df$Value, valid_values_df$List)

# Read in Validation Info ----------------
# This tab includes all csvs, columns, data types, 
# corresponding valid values list info, and additional validation rule notes
## Manual edits ----
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
    str_len_limit = fifelse(is_str, as.numeric(sub("S", "",Type)), NA),
    nulls_allowed = Null == "Y" & !is.na(Null),
    Notes = fcase(
      CSV == "Project" & Name == "RRHSubType", "Null unless ProjectType = 13",
      CSV == "Project" & Name == "ResidentialAffilitation", "Null unless ProjectType = 6 or (ProjectType = 13 and RRHSubType = 1)",
      Name == "ProjectType", NA,
      default = Notes
    ),
    Null = fifelse(Name == "ProjectType", NA, Null),
    List = fifelse(List == "(see note)", NA, List)
  ) |>
  frename("validation_notes" = Notes)

# DE-Variable xwalk/lookup
valid_list_lookup <- validation_info$Name
names(valid_list_lookup) <- validation_info$`DE#`
valid_list_lookup <- valid_list_lookup[!is.na(names(valid_list_lookup)) & !is.na(valid_list_lookup)]

# Special Cases: Value-Not-in-List  -----
# Special cases: these are cases that could not be easily codified directly from the specs
special_validation_rules <- list(
  CEParticipation = list(
    "Conditonally Invalid Value" = list(
      PreventionAssessment = quote((is.na(PreventionAssessment) & AccessPoint == 1) | (fcoalesce(AccessPoint, -1) != 1 & PreventionAssessment != 0)),
      CrisisAssessment     = quote((is.na(CrisisAssessment)     & AccessPoint == 1) | (fcoalesce(AccessPoint, -1) != 1 & CrisisAssessment != 0)),
      HousingAssessment    = quote((is.na(HousingAssessment)    & AccessPoint == 1) | (fcoalesce(AccessPoint, -1) != 1 & HousingAssessment != 0)),
      DirectServices       = quote((is.na(DirectServices)       & AccessPoint == 1) | (fcoalesce(AccessPoint, -1) != 1 & DirectServices != 0))
    )
  ),
  Client = list(
    "Conditonally Invalid Value" = list(
      RaceNone = quote(
        !is.na(RaceNone) & 
          Reduce(`|`, lapply(mget(intersect(race_cols, ls())), function(x) x %in% 1), init = FALSE)
      )
    )
  ),
  CurrentLivingSituation = list(
    "Unallowed Null" = list(
      VerifiedBy = quote(is.na(VerifiedBy) & ProjectType == 14)
    ),
    "Non-Null Invalid" = list(
      CurrentLivingSituation = quote(!CurrentLivingSituation %in% c(312,313,327,422,423,426,30,24))
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
      EnrollmentCoC = quote(!grepl("^[A-Za-z]{2}-[0-9]{3}$", EnrollmentCoC) | (ContinuumProject == 1 & .join == "x")),
      LivingSituation = quote(!LivingSituation %in% c(312,313,327,422,423,426,30,17,24,37))
    )
  ),
  Exit = list(
    "Non-Null Invalid" = list(
      SubsidyInformation = quote(
        (HousingAssessment == 1 & !SubsidyInformation %in% c(1,2,3,4)) |
          (HousingAssessment == 2 & !SubsidyInformation %in% c(11,12))
      ),
      SessionsInPlan     = quote(SessionsInPlan < 0),
      SessionCountAtExit = quote(SessionCountAtExit > 0),
      Destination = quote(!Destination %in% c(336,335,37))
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
        valid_vals <- valid_values[record_type_list_lookup[as.character(dt$RecordType)]]
        !mapply(`%in%`, dt$TypeProvided, valid_vals)
      },
      SubTypeProvided = quote(
        fcase(
          RecordType == 144 & TypeProvided == 3, !SubTypeProvided %in% valid_values[["V2.A"]],
          RecordType == 144 & TypeProvided == 4, !SubTypeProvided %in% valid_values[["V2.B"]],
          RecordType == 144 & TypeProvided == 5, !SubTypeProvided %in% valid_values[["V2.C"]],
          default = FALSE
        )
      )
    ),
    "Null Unless" = list(
      SubTypeProvided = quote(RecordType == 144 & TypeProvided %in% c(3, 4, 5))
    )
  )
)

invalid_non_null_dynamic_lists <- list(
  Disabilities_DisabilityResponse = quote(
    fifelse(DisabilityType == 10, "4.10.2", "1,8")
  ),
  Services_TypeProvided = quote(
    unname(record_type_list_lookup[as.character(RecordType)])
  ),
  Services_SubTypeProvided = quote(
    fcase(
      RecordType == 144 & TypeProvided == 3, "V2.A",
      RecordType == 144 & TypeProvided == 4, "V2.B",
      RecordType == 144 & TypeProvided == 5, "V2.C"
    )
  )
)

invalid_non_null_dynamic_lists_dt <- data.table(
  k = names(invalid_non_null_dynamic_lists),
  rule_text = unname(invalid_non_null_dynamic_lists)
)[
  , c("CSV", "Name") := tstrsplit(k, "_", fixed = TRUE)
][
  , k := NULL
]



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
              rule_expr  = list(special_validation_rules[[csv]][[issue]][[name]])
            )
          })
        )
      })
    )
  })
)

# Supplemental files needed for specific CSVs -----------
csv_join_prerequisites <- list(
  CurrentLivingSituation = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType")
  ),
  EmploymentEducation = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType"),
    list(tbl = "Funder",     on = "ProjectID",    cols = "Funder")
  ),
  Enrollment = list(
    list(tbl = "ProjectCoC", on = c("ProjectID" = "ProjectID", "EnrollmentCoC" = "CoCCode"), column = TRUE),
    list(tbl = "Project",    on = "ProjectID",    cols = c("ContinuumProject", "ProjectType")),
    list(tbl = "Funder",     on = "ProjectID",    cols = "Funder")
  ),
  Exit = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType"),
    list(tbl = "Funder",     on = "ProjectID",    cols = "Funder")
  ),
  Disabilities = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType"),
    list(tbl = "Funder",     on = "ProjectID",    cols = "Funder")
  ),
  HealthAndDV = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType"),
    list(tbl = "Funder",     on = "ProjectID",    cols = "Funder")
  ),
  IncomeBenefits = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType"),
    list(tbl = "Funder",     on = "ProjectID",    cols = "Funder")
  ),
  Inventory = list(
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType")
  ),
  Organization = list(
    list(tbl = "Project",    on = "OrganizationID", cols = "ProjectID")
  ),
  Services = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType"),
    list(tbl = "Funder", on = "ProjectID", cols = "Funder")
  ),
  YouthEducationStatus = list(
    list(tbl = "Enrollment", on = "EnrollmentID", cols = "ProjectID"),
    list(tbl = "Project",    on = "ProjectID",    cols = "ProjectType"),
    list(tbl = "Funder", on = "ProjectID", cols = "Funder")
  )
)

# Funder- and Project-Type specific Null Unless Fields -------------------
# These are fields for whose Null checks (as part of the Null Unless checks)
# should only apply when Funder in c(13:19)
null_unless_additional_reqs <-  readxl::read_xlsx(validation_specs_bk, sheet = "NullUnless - Additional Reqs") |>
  qDT() |>
  fselect(CSV, Name, Funder, ProjectType) |>
  fmutate(
    additional_reqs = fcase(
      !is.na(Funder) & !is.na(ProjectType), paste0("Funder in ", Funder, " & ProjectType in ", ProjectType),
      !is.na(Funder), paste0("Funder in ", Funder),
      !is.na(ProjectType), paste0("ProjectType in ", ProjectType)
    )
  )

# Create Master Rules table ----------------
# Includes all validation and reporting info
# We express the rules in R code that will be evaluated at runtime.
specs_rules <- validation_info %>%
  join(reporting_info, on = c("CSV", "Name"), multiple=TRUE) %>%
  # Initialize an empty list column to hold the parsed expressions
  fmutate(rule_expr = list(NULL))

## 1. Null Unless  ------------
specs_rules[
  issue_type == "Null Unless",
  validation_notes := str_split_i(validation_notes, "\r\n", 1)
][
  issue_type == "Null Unless",
  codified_rule := Map(clean_rule_for_null_unless, Name, validation_notes)
][
  issue_type == "Null Unless",
  `:=`(
    rule_expr = unlist(
      purrr::map2(Name, codified_rule, ~rlang::parse_expr(glue("null_unless({.x}, {.y})")))
    ),
    
    readable_validation_notes = humanize_rule(codified_rule, specs_rules, valid_values_df)
  )
]

## 2. String Length Limit Exceeded -----------
specs_rules[
  issue_type == "String Length Limit Exceeded", 
  rule_expr := Map(function(col, limit) 
    rlang::parse_expr(glue::glue("vlengths({col}) > {limit}")),
    Name, 
    str_len_limit
  )
]

## 3. Unallowed Null -----------
specs_rules[
  issue_type == "Unallowed Null", 
  rule_expr := lapply(Name, function(col) 
    rlang::parse_expr(glue::glue("is.na({col})"))
  )
]

## 4. Non-Null Invalid -----------
specs_rules[
  issue_type == "Non-Null Invalid" & !is.na(List), 
  rule_expr := Map(function(col, lst)
    # Creates: !is.na(Name) & !(Name %in% valid_values[['ListID']])
    rlang::parse_expr(glue::glue("!is.na({col}) & !({col} %in% valid_values[['{lst}']])")),
    Name, 
    List
  )
]

## 5. Foreign Key Missing -----------
specs_rules[
  issue_type == "Foreign Key Missing",
  `:=`(
    fk_id_col   = sub(".*Must match a ([^ ]+) in [^ ]+\\.csv.*", "\\1", validation_notes),
    foreign_tbl = sub(".*Must match a [^ ]+ in ([^ ]+)\\.csv.*", "\\1", validation_notes)
  )
]

specs_rules[
  issue_type == "Foreign Key Missing", 
  rule_expr := Map(function(c, fc, ft)
   rlang::parse_expr(glue::glue("!is.na({c}) & !({c} %in% get('{ft}')[['{fc}']])")), 
   Name, 
   fk_id_col, 
   foreign_tbl
  )
]

# ignore User ID
specs_rules <- specs_rules[!(issue_type == "Foreign Key Missing" & Name == "UserID")]

## 6. Duplicate UniqueID ---------
specs_rules[
  issue_type == "Duplicate UniqueID", 
  rule_expr := lapply(Name, function(col)
    # Using all = TRUE ensures BOTH the original and the duplicate are flagged
    rlang::parse_expr(glue::glue("fduplicated({col}, all = TRUE)"))
  )
]

## 7. Special rules --------------
# overwrites the rule_expr column with the new special rule
specs_rules[
  special_validation_rules_dt,
  on = c("CSV", "Name", "issue_type" = "Issue"),
  rule_expr := i.rule_expr
]

print("saving specs_prepped.rds file")
saveRDS(
  list(
    specs_rules = specs_rules, 
    reporting_info = reporting_info, 
    valid_values = valid_values,
    cols_and_data_types = cols_and_data_types
  ), 
  specs_prepped_path
)
}