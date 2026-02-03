# FY26 vlaidation specs
validation_specs_bk <- here("public-resources/FY26 HMIS-CSV-Machine-Readable-Specifications.xlsx")

# Read in CSV Lists
# This tab contains all the valid values for each list element
valid_values <- read_excel(validation_specs_bk, sheet = "CSV Lists FY2026") %>%
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
hashed_cols <- c("FirstName","MiddleName","LastName","NameSuffix","SSN")

# Read in Variable-List xwalk
# This tab includes all variables and the corresponding List element that determines valid values
# It also includes nuanced notes about validation, including conditional validations
validation_info <- read_excel(validation_specs_bk, sheet = "CSV Lists Data Dict FY2026") %>%
  fsubset(!CSV %in% files_to_ignore) %>%
  fmutate(
    Type = gsub("\u00A0", "", Type),
    List = str_trim(ifelse(List == "1.1000000000000001", "1.1", List)),
    Name = gsub("\u00A0", "", Name),
    Name = ifelse(Name == "SSN[1]", "SSN", ifelse(Name == "Geocode[1]", "Geocode", Name))
  ) %>%
  fselect(CSV, `DE#`, Name, Type, List, Null, Notes) %>%
  funique(cols = c("CSV", "Name")) %>% #Dedup since some variables can appear multiple times, one for each Data Element, e.g. LOSUnderThreshold
  qDT() %>%
  fmutate(
    valid_values = valid_values[List],
    Type = fifelse(Name %in% hashed_cols, "S32", Type),
    is_str = substr(Type, 1, 1) == "S" & Type != "S",
    str_len_limit = fifelse(is_str, as.numeric(sub("S", "", "S32")), NA),
    nulls_allowed = isTruthy(Null == "Y")
  )

# Null unless - standard
# Most "Null unless..." rules can be programmatically codified. 
# But some are more complicated and should be handled separately
lookup <- validation_info$Name
names(lookup) <- validation_info$`DE#`
lookup <- lookup[!is.na(names(lookup)) & !is.na(lookup)]

exceptions <- list(
  "Exit" = c("SessionCountAtExit"),
  "CurrentLivingSituation" = c("VerifiedBy"),
  "Inventory" = c("Availability", "ESBedType")
)

null_unless_rules <- validation_info %>%
  fmutate(
    Notes = str_split_i(Notes, "\r\n", 1),
    rule = stringi::stri_replace_all_fixed(
      gsub("^Null unless ", "", Notes),
      pattern     = names(lookup),
      replacement = lookup,
      vectorize_all = FALSE
    )
  ) %>%
  fmutate(
    rule = rule %>%
      # Replace "=" with "==" (but not "==", "!=", "<=", ">=")
      stringi::stri_replace_all_regex("(?<![=!<>])=(?!=)", "==") %>%
      # Replace "in" with "%in%"
      stringi::stri_replace_all_regex("\\bin\\b", "%in%") %>%
      # Replace AND/and/And with &
      stringi::stri_replace_all_regex("\\b(?:AND|and|And)\\b", "&") %>%
      # Replace OR/or/Or with |
      stringi::stri_replace_all_regex("\\b(?:OR|or|Or)\\b", "|") %>%
      # Replace "is not null" with !is.na(...)
      stringi::stri_replace_all_regex("(\\w+)\\s+is not null", "!is.na($1)") %>%
      # Replace "is null" with is.na(...)
      stringi::stri_replace_all_regex("(\\w+)\\s+is null", "is.na($1)") %>%
      # Replace "between" with %between%
      stringi::stri_replace_all_regex("\\bbetween\\b", "%between%")
  ) %>%
  fsubset(grepl("Null unless", Notes), CSV, Name, rule)

null_unless <- function(col, cond) {
  (is.na(col) & cond) | (!is.na(col) & !cond)
}

# Validate by csv ------------------------------
# For "type" column in specs, if string, make sure length doesn't exceed limit
# For "list", check if value is not in list's valid values
#   Note, some of these, like LivingSituation in Enrollment.csv, have additional restrictions. See Notes
# For "Null unless..." notes
#   It's invalid if it's non-null and the conditional variable is anything else. E.g., Inventory$Availability is invalid if it's non-null and Project Type is not 0 or 1.
#   It's invalid if it's null and conditional variable is as specified E.g., Inventory$Availability is invalid if it's null and Project Type is 0 or 1.

# Loop through all files that actually have validation conditions
for(csv_name in intersect(unique(cols_and_data_types$File), validation_info$CSV)) {
  print(glue("Checking FSA for {csv_name}"))
  logToConsole(session, glue("Checking FSA for {csv_name}"))
  dt <- get(csv_name)
  csv_validation_info <- validation_info[CSV == csv_name]
  
  # CHECK 1: Strings exceed limits-------------------
  cols_to_select <- intersect(csv_validation_info[is_str == T]$Name, names(dt))
  str_lengths <- dt %>%
    fselect(cols_to_select) %>%
    fmutate(across(.fns = vlengths))
  
  exceeds_limit <- mapply(
    function(col_data, limit) which(col_data > limit),
    str_lengths,
    csv_validation_info[is_str == T, str_len_limit]
  )
  
  exceeds_dt <- data.table(
    Column = names(exceeds_limit),
    row_ids = exceeds_limit,
    issueid = 10
  )
  
  # CHECK 2: Nulls where Nulls not allowed ----------------------
  unallowed_nulls <- dt %>%
    fselect(csv_validation_info[nulls_allowed == F]$Name) %>%
    fsummarize(across(.fns = \(x) list(whichNA(x))))

  unallowed_nulls_dt <- data.table(
    Column = names(unallowed_nulls),
    row_ids = unlist(unallowed_nulls, recursive = FALSE),
    issueid = 6
  )

  # CHECK 3: Values not in valid list --------------
  list_cols <- csv_validation_info[!is.na(List)]$Name
  invalid_vals <- lapply(list_cols, function(col_name) {
    col_validation_info <- csv_validation_info[Name == col_name]
    valid_vals <- col_validation_info$valid_values[[1]]
    data_vals <- dt[[col_name]] %>% na_omit()
    
    invalid <- !data_vals %in% valid_vals
    
    which(invalid)
  }) %>% setNames(list_cols)
  
  invalid_vals_dt <- data.table(
    Column = list_cols,
    row_ids = invalid_vals,
    issueid = 50
  ) # [lengths(row_ids) > 0]
  
  
  # CHECK 4: Duplicate Unique Identifiers -------------------
  id_col <-  csv_validation_info[toupper(Notes) == toupper("Unique identifier")]$Name
  dups <- dt %>%
    fcountv(cols = id_col) %>%
    fsubset(N > 1)
  
  duplicate_ids <- if(fnrow(dups) >0) {
    data.table(
      Column = id_col,
      row_ids = dups,
      Detail = glue("There are {fnrow(dups)} duplicates found for {id_col}."),
      issueid = 24
    ) 
  } else data.table()
  
  # CHECK 5: Foreign key checks ----------------------
  foreign_key_checks <- csv_validation_info %>%
    fsubset(grepl("Must match", Notes))
  
  foreign_key_issues <- if(fnrow(foreign_key_checks)) {
    foreign_key_checks[0, `:=`(
      id_col = sub("Must match a (.+) in (.+)\\.csv", "\\1", foreign_key_checks$Notes),
      tbl_name = sub("Must match a (.+) in (.+)\\.csv", "\\2", foreign_key_checks$Notes)
    )]
    
    foreign_tbl_data <- get(foreign_key_checks$tbl_name)
    
    data.table(
      Column = foreign_key_checks$Name,
      row_ids = dt %>%
        fmutate(row_id = seq_row()) %>%
        fsubset(foreign_key_checks$Name %in% foreign_tbl_data$id_col),
      Detail = glue("There is no matching {foreign_key_checks$Name} in {foreign_key_checks$tbl_name}"),
      issueid = 9
    )
  } else data.table()
    
  # CHECK 6: Null unless - standard
  # There are some rules that are harder to automatically codify and will be handled separately
  csv_null_unless_rules <- null_unless_rules %>%
    fsubset(
      CSV == csv_name & 
      !Name %in% exceptions[[csv_name]]
    )
  
  null_unless_issues <- if(fnrow(csv_null_unless_rules) > 0) {
    lapply(seq_row(csv_null_unless_rules), function(i) {
      col <- csv_null_unless_rules[i, Name]
      rule_expr <- str2lang(glue("null_unless({col}, {csv_null_unless_rules[i, rule]})"))
      
      dt %>%
        fmutate(invalid = eval(rule_expr, dt)) %>%
        fsummarize(
          Column = col,
          row_ids = list(which(invalid))
        )
    }) %>%
      rbindlist() %>%
      fsubset(lengths(row_ids) > 0) %>%
      fmutate(issueid = 50)
  } else data.table()
  
  
  # CHECK 6: Nuanced validation Notes ------------------
  if(csv_name == "Services") {
    # TypeProvided
    record_type_list_lookup <- c(
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
    
    invalid_typeprovided <- dt %>%
      fmutate(valid_vals = valid_values[record_type_list_lookup[as.character(RecordType)]]) %>%
      fsummarize(
        Column = "TypeProvided", 
        row_ids = toString(which(!TypeProvided %in% unlist(valid_vals))),
        issueid = 50
      )
    
    # OtherTypeProvided
    # Null unless RecordType = 144 and TypeProvided = 6 Or RecordType = 210 and TypeProvided = 12
    # othertypeprovided <- dt %>%
    #   fmutate(invalid = null_unless(OtherTypeProvided, ((RecordType == 144 & TypeProvided == 6) | (RecordType == 210 & TypeProvided == 12)))) %>%
    #   fsummarize(
    #     Column = "OtherTypeProvided",
    #     row_ids = toString(which(invalid)),
    #     issueid = 1005
    #   )
    
    invalid_services <- invalid_typeprovided
  } else if(csv_name == "Export") {
    validation_rules <- list(
      SourceID = quote(SourceType == 1 & !grepl("^[A-Za-z]{2}-[0-9]{3}$", SourceID)),
      SourceName = quote(SourceType != 1 & is.na(SourceName)),
      SourceContactPhone = quote(!grepl("^[2-9][0-9]{2}[2-9][0-9]{2}[0-9]{4}$", SourceContactPhone)),
      SourceContactExtension = quote(!grepl("^[0-9]{1,5}$", SourceContactExtension)),
      SourceContactEmail = quote(!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", SourceContactEmail))
    )
    
    # Execute in loop
    invalid_export <- lapply(names(validation_rules), function(col) {
      dt %>%
        fmutate(
          invalid = eval(validation_rules[[col]]),
          row_id = seq_row(.)
        ) %>%
        fsummarize(
          Column = col,
          row_ids = list(row_id[invalid])
        )
    }) %>%
      rbindlist() %>%
      fsubset(lengths(row_ids) > 0) %>%
      fmutate(issueid = 50)
  } else if(csv_name == "User") {
    validation_rules <- list(
      UserPhone = quote(!grepl("^[2-9][0-9]{2}[2-9][0-9]{2}[0-9]{4}$", UserPhone)),
      UserExtension = quote(!grepl("^[0-9]{1,5}$", UserExtension)),
      UserEmail = quote(!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", UserEmail))
    )
    
    # Execute in loop
    invalid_user <- lapply(names(validation_rules), function(col) {
      dt %>%
        fmutate(
          invalid = eval(validation_rules[[col]]),
          row_id = seq_row(.)
        ) %>%
        fsummarize(
          Column = col,
          row_ids = list(row_id[invalid])
        )
    }) %>%
      rbindlist() %>%
      fsubset(lengths(row_ids) > 0) %>%
      fmutate(issueid = 50)
  } else if(csv_name == "CurrentLivingSituation") {
    invalid_verifiedby <- dt %>%
      join(Enrollment %>% fselect(EnrollmentID, ProjectID), on = "EnrollmentID") %>%
      join(Project %>% fselect(ProjectID, ProjectType), on = "ProjectID") %>%
      fmutate(invalid = is.na(VerifiedBy) & ProjectType == 14) %>%
      fsummarize(
        Column = "VerifiedBy",
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    invalid_currentlivingsituation <- invalid_verifiedby
  } else if(csv_name == "Exit") {
    # SubsidyInformation
    invalid_SubsidyInformation <- dt %>%
      fmutate(invalid = 
                (HousingAssessment == 1 & SubsidyInformation %in% c(1,2,3,4)) |
                (HousingAssessment == 2 & SubsidyInformation %in% c(11,12))
      ) %>%
      fsummarize(
        Column = "SubsidyInformation", 
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    # SessionsInPlan
    invalid_SessionsInPlan <- dt %>%
      fmutate(invalid = SessionsInPlan < 0) %>%
      fsummarize(
        Column = "SessionsInPlan", 
        row_ids = toString(which(invalid)),
        issueid = 50
      )
   
    # SessionCountAtExit
    invalid_SessionCountAtExit <- dt %>%
      fmutate(
        invalid = null_unless(SessionCountAtExit, CounselingReceived == 1) | SessionCountAtExit > 0
      ) %>%
      fsummarize(
        Column = col,
        row_ids = list(which(invalid))
      ) %>%
      fsubset(lengths(row_ids) > 0) %>%
      fmutate(issueid = 50)
    
    invalid_exit <- rbindlist(list(
      invalid_SubsidyInformation,
      invalid_SessionsInPlan,
      invalid_SessionCountAtExit
    ))
# 
#     # null unless
#     null_unless_rules <- list(
#       AftercareProvided = quote(AftercareDate %between% list(ExitDate, ExitDate + 180)),
#       AftercareDate = quote(AftercareDate %between% list(ExitDate, ExitDate + 180) & !is.na(AftercareProvided)),
#       OtherDestination = quote(Destination == 17),
#       DestinationSubsidyType = quote(Destination == 435),
#       ExchangeForSexPastThreeMonths = quote(ExchangeForSex == 1),
#       CountOfExchangeForSex = quote(ExchangeForSex == 1),
#       AskedOrForcedToExchangeForSex = quote(ExchangeForSex == 1),
#       AskedOrForcedToExchangeForSexPastThreeMonths = quote(AskedOrForcedToExchangeForSex == 1),
#       CoercedToContinueWork = quote(WorkplaceViolenceThreats == 1 | WorkplacePromiseDifference == 1),
#       LaborExploitPastThreeMonths = quote(WorkplaceViolenceThreats == 1 | WorkplacePromiseDifference == 1),
#       EarlyExitReason = quote(ProjectCompletionStatus == 3),
#       IndividualCounseling = quote(CounselingReceived == 1),
#       FamilyCounseling = quote(CounselingReceived == 1),
#       GroupCounseling = quote(CounselingReceived == 1),
#       SessionCountAtExit = quote(CounselingReceived == 1),
#       EmailSocialMedia = quote(AftercareProvided == 1),
#       Telephone = quote(AftercareProvided == 1 == 1),
#       InPersonIndividual = quote(AftercareProvided == 1 == 1),
#       InPersonGroup = quote(AftercareProvided == 1 == 1)
#     )
#     
#     lapply(names(null_unless_rules), function(col) {
#       dt %>%
#         fmutate(
#           invalid = null_unless(col, eval(null_unless_rules[[col]]))
#         ) %>%
#         fsummarize(
#           Column = col,
#           row_ids = list(which(invalid))
#         )
#     }) %>%
#       rbindlist() %>%
#       fsubset(lengths(row_ids) > 0) %>%
#       fmutate(issueid = 1005)
  } else if(csv_name == "ProjectCoC") {
    validation_rules <- list(
      ZIP = quote(!grepl("^[0-9]{5}$", ZIP)),
      Geocode = quote(!grepl("^[0-9]{6}$", Geocode)),
      State = quote(!grepl("^[a-zA-Z]{2}$", State))
    )
    
    # Execute in loop
    invalid_projectcoc <- lapply(names(validation_rules), function(col) {
      dt %>%
        fmutate(
          invalid = eval(validation_rules[[col]])
        ) %>%
        fsummarize(
          Column = col,
          row_ids = list(which(invalid))
        )
    }) %>%
      rbindlist() %>%
      fsubset(lengths(row_ids) > 0) %>%
      fmutate(issueid = 1005)
  } else if(csv_name == "Enrollment") {
    # VAMCStation
    vamcstation <- dt %>%
      fmutate(invalid = !VAMCStation %in% valid_values["V6.1"]) %>%
      fsummarize(
        Column = "VAMCStation", 
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    enrollmentcoc <- dt %>%
      join(ProjectCoC, on = "ProjectID") %>%
      fgroup_by(ProjectID) %>%
      fmutate(has_matching_coc = any(EnrollmentCoC == CoCCode, na.rm=TRUE)) %>%
      fungroup() %>%
      fmutate(invalid = 
                !grepl("^[A-Za-z]{2}-(0-9){3}$", EnrollmentCoC) |
                !has_matching_coc
      ) %>%
      fsummarize(
        Column = "EnrollmentCoC", 
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    invalid_enrollment <- rbindlist(list(
      vamcstation,
      enrollmentcoc
    ))
    
    # null_unless_cols <- list(
    #   RentalSubsidyType = quote(LivingSituation == 435),
    #   ReasonNoServices = quote(EligibleForRHY == 0),
    #   RunawayYouth = quote(EligibleForRHY == 1),
    #   ReasonNotEnrolled = quote(ClientEnrolledInPATH == 0)
    # )
    # 
    # null_unless_enrollment <- lapply(null_unless_cols, function(col) {
    #   dt %>%
    #     fmutate(
    #       invalid = eval(validation_rules[[col]])
    #     ) %>%
    #     fsummarize(
    #       Column = col,
    #       row_ids = list(which(invalid))
    #     )
    # }) %>%
    #   rbindlist() %>%
    #   fmutate(issueid = 1005)
      
  } else if(csv_name == "Project"){
    #ProjectType
    invalid_ProjectType <- dt %>%
      fmutate(invalid = is.na(ProjectType) & ContinuumProject == 1) %>%
      fsummarize(
        Column = "ProjectType",
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    invalid_RRHSubType <- dt %>%
      fmutate(invalid = is.na(RRHSubType) & ProjectType == 13) %>%
      fsummarize(
        Column = "RRHSubType",
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    invalid_ResidentialAffiliation <- dt %>%
      fmutate(invalid = is.na(ResidentialAffiliation) & (ProjectType == 6 | (ProjectType == 13 & RRHSubType == 1))) %>%
      fsummarize(
        Column = "RRHSubType",
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    invalid_project <- rbindlist(list(
      invalid_ProjectType,
      invalid_RRHSubType,
      invalid_ResidentialAffiliation
    ))
  } else if(csv_name == "CEParticipation") {
    # PreventionAssessment	I	1.10	Y	Non-null if 2.09.1 = 1. If option not selected, should be set to 0
    # CrisisAssessment	I	1.10	Y	Non-null if 2.09.1 = 1. If option not selected, should be set to 0
    # HousingAssessment	I	1.10	Y	Non-null if 2.09.1 = 1. If option not selected, should be set to 0
    # DirectServices	I	1.10	Y	Non-null if 2.09.1 = 1. If option not selected, should be set to 0
    cols <- c(
      "PreventionAssessment",
      "CrisisAssessment",
      "HousingAssessment",
      "DirectServices"
    )
    x <- dt %>%
      fsubset(AccessPoint == 1, cols) %>%
      fsummarize(across(.fns = \(x) list(whichNA(x))))
    
    invalid_ceparticipation <- data.table(
      Column = names(x),
      row_ids = unlist(x, recursive = FALSE),
      issueid = 50
    )
    
  } else if(csv_name == "Client") {
    invalid_RaceNone <- dt %>%
      fselect(race_cols) %>%
      fmutate(
        invalid =
          (!is.na(RaceNone) & White == 1 | AmIndAKNative == 1 | Asian  == 1 | MidEastNAfrican == 1 | NativeHIPacific == 1 | HispanicLatinao == 1 | BlackAfAmerican == 1) |
          (is.na(RaceNone) & White %in% c(0,99) | AmIndAKNative %in% c(0,99) | Asian  %in% c(0,99) | MidEastNAfrican %in% c(0,99) | NativeHIPacific %in% c(0,99) | HispanicLatinao %in% c(0,99) | BlackAfAmerican %in% c(0,99))
      ) %>%
      fsummarize(
        Column = "RaceNone",
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    invalid_client <- invalid_RaceNone
  }
  
  
}

final_summary <- rbindlist(list(
  exceeds_dt,
  unallowed_nulls_dt,
  invalid_vals_dt,
  duplicate_ids,
  foreign_key_issues,
  null_unless_issues,
  # special checks
  invalid_services,
  invalid_export,
  invalid_user,
  invalid_currentlivingsituation,
  invalid_exit,
  invalid_projectcoc,
  invalid_enrollment,
  invalid_project,
  invalid_ceparticipation,
  invalid_client
))