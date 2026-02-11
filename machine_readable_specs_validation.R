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
hashed_cols <- c("FirstName","MiddleName","LastName","NameSuffix","SSN")

# Read in Variable-List xwalk
# This tab includes all variables and the corresponding List element that determines valid values
# It also includes nuanced notes about validation, including conditional validations
validation_info <- cols_and_data_types %>%
  fsubset(!CSV %in% c("AssessmentResults","AssessmentQuestions")) %>%
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

rows_to_show <- function(row_ids) {
  mapply(function(r) {
    n_problems <- length(r)
    if (n_problems == 0) "No problems"
    else if (n_problems == fnrow(dt)) "All rows affected"
    else if (n_problems > 3) paste("See, e.g., row", r[1])
    else paste("See rows", toString(r))
  }, row_ids, SIMPLIFY = TRUE)
}

# Validate by csv ------------------------------
# For "type" column in specs, if string, make sure length doesn't exceed limit
# For "list", check if value is not in list's valid values
#   Note, some of these, like LivingSituation in Enrollment.csv, have additional restrictions. See Notes
# For "Null unless..." notes
#   It's invalid if it's non-null and the conditional variable is anything else. E.g., Inventory$Availability is invalid if it's non-null and Project Type is not 0 or 1.
#   It's invalid if it's null and conditional variable is as specified E.g., Inventory$Availability is invalid if it's null and Project Type is 0 or 1.

# Loop through all files that actually have validation conditions
csv_issues <- list()
for(csv_name in unique(validation_info$CSV)) {
  print(glue("Checking FSA for {csv_name}"))
  logToConsole(session, glue("Checking FSA for {csv_name}"))
  dt <- get(csv_name)
  csv_validation_info <- validation_info[CSV == csv_name & Name %in% names(dt)]
  
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
  
  exceeds_dt <- if(any(lengths(exceeds_limit))) {
    data.table(
      Column = names(exceeds_limit),
      row_ids = exceeds_limit,
      issueid = 10
    )
  } else data.table()
  
  # CHECK 2: Nulls where Nulls not allowed ----------------------
  unallowed_nulls <- dt %>%
    fselect(csv_validation_info[nulls_allowed == F]$Name) %>%
    fsummarize(across(.fns = \(x) list(whichNA(x))))

  unallowed_nulls_dt <- data.table(
    Column = names(unallowed_nulls),
    row_ids = if(fnrow(unallowed_nulls)) unlist(unallowed_nulls, recursive = FALSE) else NA,
    issueid = 6
  )

  # CHECK 3: Values not in valid list --------------
  list_cols <- csv_validation_info[!is.na(List) ]$Name
  invalid_vals <- lapply(list_cols, function(col_name) {
    col_validation_info <- csv_validation_info[Name == col_name]
    valid_vals <- col_validation_info$valid_values[[1]]

    invalid <- !dt[[col_name]] %in% valid_vals & !is.na(dt[[col_name]])
    
    which(invalid)
  }) %>% setNames(list_cols)
  
  invalid_vals_dt <- data.table(
    Column = list_cols,
    row_ids = invalid_vals,
    issueid = 50
  ) %>% 
    fsubset(lengths(row_ids) > 0)
  
  
  # CHECK 4: Duplicate Unique Identifiers -------------------
  id_col <-  csv_validation_info[toupper(Notes) == toupper("Unique identifier")]$Name
  
  duplicate_ids <- if(fnrow(dups) >0) {
    data.table(
      Column = id_col,
      row_ids = list(which(fduplicated(dt[[id_col]]))),
      issueid = 24
    ) 
  } else data.table()
  
  # CHECK 5: Foreign key checks ----------------------
  foreign_key_checks <- csv_validation_info %>%
    fsubset(grepl("Must match", Notes)) %>%
    fmutate(
      id_col   = sub("Must match a ([^ ]+) in [^ ]+\\.csv.*", "\\1", Notes),
      tbl_name = sub("Must match a [^ ]+ in ([^ ]+)\\.csv.*", "\\1", Notes)
    )
  
  check_fk <- function(spec_row, dt) {
    foreign_tbl <- get(spec_row$tbl_name)
    
    missing <- dt %>%
      join(
        foreign_tbl,
        on   = setNames(spec_row$id_col, spec_row$Name),
        how  = "anti"
      )
    
    if (nrow(missing) == 0) return(NULL)
    
    data.table(
      Column  = spec_row$Name,
      row_ids  = list(seq_row(missing)),
      foreign_tbl = spec_row$tbl_name
    )
  }
  
  foreign_key_issues <- rbindlist(
    lapply(seq_row(foreign_key_checks), function(i) {
      check_fk(foreign_key_checks[i], dt)
    }),
    fill = TRUE
  )

  foreign_key_issues <- if(fnrow(foreign_key_issues) > 0) {
     foreign_key_issues %>%
      fmutate(
        issueid = 9,
        rows_to_show = rows_to_show(row_ids),
        Detail = str_squish(glue(
          "In the {csv_name}.csv file, there is one or more {Column}s with no 
        matching value in {foreign_tbl}. {rows_to_show}"))
      ) %>%
      fselect(Column, row_ids, issueid, Detail)
  } else data.table()
    
  # CHECK 6: Null unless - standard
  # There are some rules that are harder to automatically codify and will be handled separately
  csv_null_unless_rules <- null_unless_rules %>%
    fsubset(
      CSV == csv_name & 
      !Name %in% exceptions[[csv_name]] &
      Name %in% names(dt)
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
  
  # CHECK 7: Incorrect Columns ------------------------------
  # Checks existence AND order. Reports on missing, extra, and otherwise misordered
  ImportedColumns <- colnames(dt)
  CorrectColumns <- csv_validation_info$Name
  
  same_columns <- setequal(ImportedColumns, CorrectColumns)
  
  incorrect_columns <- if(same_columns) {
    data.table(
      Column = ImportedColumns[ImportedColumns != CorrectColumns],
      Detail = "Misordered"
    )
  } else {
    data.table(
      Column = c(
        setdiff(ImportedColumns, CorrectColumns), 
        setdiff(CorrectColumns, ImportedColumns)
      ),
      Detail = c("Extra", "Missing")
    )
  }
  
  incorrect_columns <- incorrect_columns %>%
    join(
      column_priorities %>% fselect(Column, DataTypeHighPriority),
      on = "Column", 
      how = 'left'
    ) %>%
    fmutate(
      issueid = 12,
      row_ids = NA,
      Type = fifelse(fcoalesce(DataTypeHighPriority, 0) == 0, "Error", "High Priority"),
      Detail = str_squish(glue(
        "In the {csv_name}.csv file {
        case_when(
          Detail == 'Extra' ~ paste(Column, 'is an extra column'),
          Detail == 'Missing' ~ paste('the ', Column, 'column is missing'),
          Detail == 'Misordered' ~ paste(Column, 'is misordered')
        )}"
      ))
    ) %>%
    fselect(Column, row_ids, issueid, Detail, Type)
  
  # CHECK 8: Unexpected Data Types
  # Unexpected data types -----------------------------------------------------
  # includes date and non-date
  
  # This maps the data types from the Specs with R's `class`:
  safe_as <- function(x, rclass) {
    tryCatch(
      get(paste0("as.", rclass))(x),
      error = function(e) rep(NA, length(x))
    )
  }
  
  unexpected_data_types <- csv_validation_info %>%
    fmutate(
      RClass = sapply(Name, function(col) class(dt[[col]])[1]),
      Type = fifelse(substr(Type, 1, 1) == "S", "S", Type),
      Expected_RClass = vapply(
        Type,
        function(t) data_type_mapping[[t]][["RClass"]],
        character(1)
      )
    ) %>%
    fsubset(Expected_RClass != RClass & Expected_RClass != "character") %>%
    fmutate(
      # get any examples that are non-null but cannot be converted to the column type
      row_ids = sapply(seq_row(.), function(i) {
        dt_col <- dt[[Name[i]]]
        converted <- safe_as(dt_col, Expected_RClass[i])
        
        return(which(!is.na(dt_col) & is.na(converted)))
      }),
      
      rows_to_show = rows_to_show(row_ids),
      
      Detail = str_squish(glue("
        In the {file}.csv file, the {Name} column should have a data type of {case_when(
          Type == 'I' ~ 'integer',
          Type == 'S' ~ 'string',
          grepl('M', Type) ~ 'decimal',
          TRUE ~ Type
        )} but in this file, it is {case_when(
          RClass == 'numeric' ~ 'integer',
          TRUE ~ RClass
        )}. {rows_to_show}"
      )),
      issueid = fifelse(Type %in% c("D", "T"), 11, 13)
    ) %>%
    fselect("Column" = Name, row_ids, issueid, Detail)
  
  
  # CHECK 9: Brackets / impermissible characters
  char_cols <- which(sapply(dt, is.character))
  if (length(char_cols) == 0) next
  
  m_mat <- as.matrix(dt[, ..char_cols, drop = FALSE])  # Convert relevant columns to matrix
  
  files_with_brackets <- if (any(grepl(bracket_regex, m_mat, perl=TRUE), na.rm=TRUE)) {
    data.table(
      File = file,
      Detail = str_squish("Found one or more brackets in your HMIS CSV Export. 
                See Impermissible Character Detail export for the precise location 
                of these characters."),
      issueid = 134
    ) 
  } else data.table()
  
  # CHECK 10: Special validation Notes ------------------
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
        row_ids = list(which(!TypeProvided %in% unlist(valid_vals))),
        issueid = 50
      )
    
    csv_issues[[csv_name]] <- invalid_typeprovided
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
    
    csv_issues[[csv_name]] <- invalid_export
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
    
    csv_issues[[csv_name]] <- invalid_user
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
    
    csv_issues[[csv_name]] <- invalid_currentlivingsituation
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
        row_ids = list(which(invalid)),
        issueid = 50
      )
   
    # SessionCountAtExit
    invalid_SessionCountAtExit <- dt %>%
      fmutate(
        invalid = null_unless(SessionCountAtExit, CounselingReceived == 1) | SessionCountAtExit > 0
      ) %>%
      fsummarize(
        Column = "SessionCountAtExit",
        row_ids = list(which(invalid))
      ) %>%
      fsubset(lengths(row_ids) > 0) %>%
      fmutate(issueid = 50)
    
    csv_issues[[csv_name]] <- rbindlist(list(
      invalid_SubsidyInformation,
      invalid_SessionsInPlan,
      invalid_SessionCountAtExit
    ))
    
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
      fmutate(issueid = 50)
    
    csv_issues[[csv_name]] <- invalid_projectcoc
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
    
    csv_issues[[csv_name]] <- invalid_enrollment
    
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
    
    csv_issues[[csv_name]] <- rbindlist(list(
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
    
    csv_issues[[csv_name]] <- invalid_ceparticipation
    
  } else if(csv_name == "Client") {
    invalid_RaceNone <- dt %>%
      fselect(race_cols) %>%
      fmutate(
        invalid =
          (!is.na(RaceNone) & (White == 1 | AmIndAKNative == 1 | Asian  == 1 | MidEastNAfrican == 1 | NativeHIPacific == 1 | HispanicLatinao == 1 | BlackAfAmerican == 1)) |
          (is.na(RaceNone) & White %in% c(0,99) & AmIndAKNative %in% c(0,99) & Asian %in% c(0,99) & MidEastNAfrican %in% c(0,99) & NativeHIPacific %in% c(0,99) & HispanicLatinao %in% c(0,99) & BlackAfAmerican %in% c(0,99))
      ) %>%
      fsummarize(
        Column = "RaceNone",
        row_ids = list(which(invalid)),
        issueid = 50
      )
    
    csv_issues[[csv_name]] <- invalid_RaceNone

  }

  # Compile the standard validation files
  csv_issues[[csv_name]] <- rbindlist(list(
    "string limit" = exceeds_dt,
    "unallowed nulls" = unallowed_nulls_dt,
    "invalid values" = invalid_vals_dt,
    "duplicate IDs" = duplicate_ids,
    "foreign keys" = foreign_key_issues,
    "null unless" = null_unless_issues,
    "missing, extra, misordered" = incorrect_columns,
    "data types" = unexpected_data_types,
    "brackets" = files_with_brackets,
    "special" = csv_issues[[csv_name]]
  ), fill=TRUE, idcol='issue_type') %>%
    fmutate(issue_type = factor(issue_type))
  
  to_dedup <- csv_issues[[csv_name]] %>% fsubset(issue_type %in% c("special","invalid values"))
  to_keep <- csv_issues[[csv_name]] %>% fsubset(!issue_type %in% c("special","invalid values"))
  
  csv_issues[[csv_name]] <- rbind(
    to_dedup %>% fslice(Column, how="max", order.by = issue_type),
    to_keep
  )
}

invalid_values <- rbindlist(
    Filter(fnrow, csv_issues),
    fill=TRUE,
    idcol="file"
  ) %>%
  fsubset(lengths(row_ids) >  0 & !is.na(row_ids)) %>%
  join(evachecks, on=c("issueid" = "ID")) %>%
  join(column_priorities, on="Column") %>%
  fmutate(
    Type = fcoalesce(Type, fcoalesce(Type_evachecks,  fifelse(DataTypeHighPriority == 1, "High Priority", "Error"))),
    rows_to_show = fifelse(is.na(Detail), rows_to_show(row_ids), NA),
    Detail = fifelse(
      !is.na(Detail),
      paste0(Detail, rows_to_show), 
      str_squish(glue("In the {file}.csv file, {Column} column: {rows_to_show}"))
    )
  ) %>%
  fselect(issue_display_cols)

rm(list = files_to_ignore)