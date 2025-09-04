######################
# PURPOSE: This program runs checks of the upload file's content.
# For example, it checks for incorrect date formats, missing columns,
# unexpected nulls, and more
######################

logToConsole(session, "Running file structure analysis")

# Prep --------------------------------------------------------------------

export_id_from_export <- session$userData$Export$ExportID

high_priority_columns <- cols_and_data_types %>%
  fsubset(DataTypeHighPriority == 1) %>%
  pull(Column) %>%
  funique()

# Brackets --------------------------------------------------------------
files_with_brackets <- data.table()
for(file in unique(cols_and_data_types$File)) {
  m <- get(file)  # Load dataset
  char_cols <- which(sapply(m, is.character))
  if (length(char_cols) == 0) next
  
  m_mat <- as.matrix(m[, ..char_cols, drop = FALSE])  # Convert relevant columns to matrix

  if (any(grepl(bracket_regex, m_mat, perl=TRUE), na.rm=TRUE)) {
    files_with_brackets <- data.table(
      File = file,
      Detail = str_squish("Found one or more brackets in your HMIS CSV Export. 
                See Impermissible Character Detail export for the precise location 
                of these characters.")
    ) %>%
      merge_check_info_dt(checkIDs = 134) %>%
      fselect(issue_display_cols)
    break
  }
}

# Incorrect Columns ------------------------------------------------------

check_columns <- function(file) {
  ImportedColumns <- colnames(get(file))
  
  CorrectColumns <- cols_and_data_types %>%
      fsubset(File == {{file}}) %>%
      pull(Column)
  
  extra_columns <- setdiff(ImportedColumns, CorrectColumns)
  missing_columns <- setdiff(CorrectColumns, ImportedColumns)
  
  if(length(extra_columns) || length(missing_columns)) {
    col_diffs <- data.table(
      ColumnName = c(missing_columns, extra_columns),
      Status = c(rep("Missing", length(missing_columns)),
                 rep("Extra", length(extra_columns)))
    ) %>%
    roworder(ColumnName) %>% 
    fmutate(
      Detail = str_squish(paste(
        "In the",
        file,
        "file,",
        fifelse(
          Status == "Extra",
          paste(ColumnName, "is an extra column"),
          paste("the", ColumnName, "column is missing")
        )
      ))
    )
    
    col_diffs_hp <- col_diffs %>%
      fsubset(ColumnName %in% c(high_priority_columns)) %>%
      merge_check_info_dt(checkIDs = 12) %>%
      fselect(issue_display_cols) %>%
      funique()

    col_diffs_error <- col_diffs %>%
      fsubset(!(ColumnName %in% c(high_priority_columns))) %>%
      merge_check_info_dt(checkIDs = 82) %>%
      fselect(issue_display_cols) %>%
      funique()

    return(
      rowbind(col_diffs_hp, col_diffs_error)
    )
  }
}
df_column_diffs <- rbindlist(lapply(unique(cols_and_data_types$File), check_columns))

# Unexpected data types -----------------------------------------------------
# includes date and non-date
unexpected_data_types <- function(file) {
  data <- get(file)
  cols_and_data_types %>% 
    fsubset((File == file) & (Column %in% colnames(data))) %>%
    fmutate(actual_type = sapply(Column, function(col) class(data[[col]])[1])) %>%
    fsubset(
      DataType != fcase(
        actual_type == "POSIXct", "datetime",
        actual_type == "Date", "date",
        default = actual_type
      )
    ) %>%
    fmutate(
      example_row = {
        # Only relevant if we expect numeric/integer but got character
        if (isTruthy(DataType == "numeric") && isTruthy(actual_type == "character")) {
          current_col_values <- data[[Column]]

          # Find rows that are NOT NA (i.e., not empty strings) but become NA upon numeric conversion
          # suppressWarnings() to prevent `NAs introduced by coercion` messages
          problematic_indices <- which(!is.na(current_col_values) & is.na(suppressWarnings(as.numeric(current_col_values))))
          problematic_indices[1]
        } else NA_integer_
      },
      
      Detail = fifelse(
        !(DataType %in% c("date", "datetime")),
        glue("In the {file} file, the {Column} column should have a data type of {case_when(
          DataType == 'numeric' ~ 'integer',
          DataType == 'character' ~ 'string',
          TRUE ~ DataType
        )} but in this file, it is {case_when(
          actual_type == 'numeric' ~ 'integer',
          TRUE ~ actual_type
        )}. See, for example, row {example_row}"),
        glue("Please check that the {Column} column in the {file} file has the correct {DataType} format.")
      ),
      checkID = fifelse(
        DataTypeHighPriority == 1, 
        fifelse(DataType %in% c("date", "datetime"), 11, 13),
        fifelse(DataType %in% c("date", "datetime"), 47, 48)
      )
    ) %>%
    merge_check_info_dt(checkIDs = funique(.$checkID)) %>%
    fselect(issue_display_cols)
}
df_unexpected_data_types <- rbindlist(lapply(unique(cols_and_data_types$File), unexpected_data_types))

check_for_bad_nulls <- function(file) {
  barefile <- get(file)
  if (fnrow(barefile) > 1) {
    # select nulls-not-allowed columns
    nulls_not_allowed_cols <- cols_and_data_types %>%
      fsubset(File == file & NullsAllowed == 0 & Column %in% names(get(file))) %>%
      pull(Column)

    # select subset of columns with nulls
    barefile <- barefile %>%
      fselect(nulls_not_allowed_cols) %>%
      mutate_all(~ifelse(is.na(.), 1, 0)) %>%
      select_if(~any(. == 1))

    if(fncol(barefile) > 0) {
      barefile %>%
        mutate(row_id = row_number()) %>%
        pivot_longer(
          cols = !row_id,
          names_to = "Column",
          values_to = "value") %>%
        group_by(Column) %>%
        mutate(row_ids = case_when(
          sum(value) == nrow(barefile) ~ "All rows affected", 
          sum(value) <= 3 ~ paste("See rows: ",
                                  paste(row_id[value == 1],
                                        collapse = ", ")),
          TRUE ~ paste("For example, see row", which(value == 1)[1])
        )) %>%
        ungroup() %>%
        distinct(Column, row_ids) %>%
        fselect(Column, row_ids) %>% 
        join(cols_and_data_types %>% 
                    fselect(Column, DataTypeHighPriority),
                  on = "Column", how = 'left') %>%
        merge_check_info_dt(checkIDs = 6) %>%
        fmutate(
          Type = fifelse(DataTypeHighPriority == 1, "High Priority", "Error"),
          Detail = str_squish(glue("The {Column} column in the {file} file contains nulls
                        or incorrect data types. {row_ids}"))
        ) %>%
        fselect(issue_display_cols) %>%
        funique()
    }
  }
}
df_nulls <- rbindlist(lapply(unique(cols_and_data_types$File), check_for_bad_nulls))

# Integrity Client --------------------------------------------------------
# CHECK: export ID differs
export_id_client <- Client %>%
  fsubset(as.character(ExportID) != export_id_from_export) %>%
  merge_check_info_dt(checkIDs = 49) %>%
  fmutate(
    Detail = str_squish(paste(
      "The Export file says the ExportID is",
      export_id_from_export,
      "but in your Client file, it is",
      ExportID
    ))
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

# CHECK: Invalid demographic values
# first, get a mapping of variables and their expected values
cols <- c("VeteranStatus", race_cols)

valid_values <- list(yes_no_enhanced, c(dkr_dnc, NA), yes_no, yes_no, yes_no, yes_no, 
                     yes_no, yes_no, yes_no)


# Only take existing columns - this solves the issue of misspelled demographic 
# columns
existing_cols <- base::intersect(cols, names(Client))

# Create a named list of valid values for existing columns
valid_values_named <- setNames(valid_values, cols)[existing_cols]

# looping through only the columns that are actually (not misspelled) in Client
# check if it's an unexpected, non-na value. That's what the [[.]] does -
# it refers to the particular column in the loop. Equivalent to pull(.)
# The ~ defines an anonymous function, as opposed to creating a specific function
get_unexpected_count <- function(col_name) {
  unexpected <- !Client[[col_name]] %in% valid_values_named[[col_name]]
  data.table(name = col_name, n = sum(unexpected))
}

valid_values_client <- existing_cols %>%
  lapply(., get_unexpected_count) %>%
  rbindlist() %>%
  fsubset(n > 0) %>%
  merge_check_info_dt(checkIDs = 50) %>%
  fmutate(Detail = paste(name, "has", n, "rows with invalid values")) %>% 
  fselect(issue_display_cols)

# CHECK: duplicate client ID
duplicate_client_id <- Client %>%
  fcount(PersonalID) %>%
  fsubset(N > 1) %>%
  merge_check_info_dt(checkIDs = 7) %>%
  fmutate(
    Detail = paste("There are", N, "duplicates for PersonalID", PersonalID)
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

# Integrity Enrollment ----------------------------------------------------
if (nrow(Enrollment) == 0) {
  no_enrollment_records <- data.table(
    Detail = "There are 0 enrollment records in the Enrollment.csv file"
  ) %>%
  merge_check_info_dt(checkIDs = 101)
} else {
  no_enrollment_records <- data.table()
}

duplicate_enrollment_id <- Enrollment %>%
  fcount(EnrollmentID) %>%
  fsubset(N > 1) %>% 
  merge_check_info_dt(checkIDs = 8) %>%
  fmutate(
    Detail = str_squish(
      paste0(
        "There are ",
        N,
        " duplicates found for EnrollmentID ",
        EnrollmentID,
        "."
      )
    )
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

personal_ids_in_client <- Client$PersonalID

foreign_key_no_primary_personalid_enrollment <- Enrollment %>%
  fsubset(!PersonalID %in% c(personal_ids_in_client)) %>%
  merge_check_info_dt(checkIDs = 9) %>%
  fmutate(
    Detail = str_squish(paste(
      "PersonalID",
      PersonalID,
      "is in the Enrollment file but not in the Client file."
    ))
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

projectids_in_project <- Project$ProjectID

foreign_key_no_primary_projectid_enrollment <- Enrollment %>%
  fsubset(!ProjectID %in% c(projectids_in_project)) %>%
  merge_check_info_dt(checkIDs = 10) %>%
  fmutate(
    Detail = str_squish(paste(
      "ProjectID",
      ProjectID,
      "is in the Enrollment file but not in the Project file."
    ))
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

disabling_condition_invalid <- Enrollment %>%
  fsubset(!DisablingCondition %in% c(yes_no_enhanced)) %>%
  merge_check_info_dt(checkIDs = 51) %>%
  fmutate(
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a DisablingCondition of",
      DisablingCondition,
      "which is an invalid value."
    ))
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

living_situation_invalid <- Enrollment %>%
  fsubset(!is.na(LivingSituation) &
    !LivingSituation %in% c(allowed_prior_living_sit)) %>%
  merge_check_info_dt(checkIDs = 52) %>%
  fmutate(
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a LivingSituation of",
      LivingSituation,
      "which is not a valid value."
    ))
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

rel_to_hoh_invalid <- Enrollment %>%
  fsubset(!RelationshipToHoH %in% c(1:5, 99) & !is.na(RelationshipToHoH)) %>%
  merge_check_info_dt(checkIDs = 53) %>%
  fmutate(
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a RelationshipToHoH of",
      RelationshipToHoH,
      "which is invalid value."
    ))
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

# Count (unique) HouseholdIDs within a Project
duplicate_household_id <- Enrollment %>%
  fselect(HouseholdID, ProjectID) %>%
  funique() %>%
  fcount(HouseholdID) %>%
  fsubset(N > 1 & !is.na(HouseholdID)) %>%
  merge_check_info_dt(checkIDs = 98) %>%
  fmutate(
    Detail = paste("HouseholdID", 
                   HouseholdID,
                   "is reused across",
                   N,
                   "Enrollments into different projects.")
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

# Integrity Living Situation ----------------------------------------------

nonstandard_destination <- Exit %>%
  fsubset(!is.na(Destination) &
           !Destination %in% c(allowed_destinations)) %>%
  merge_check_info_dt(checkIDs = 54) %>%
  fmutate(
    Detail = str_squish(paste("EnrollmentID",
                     EnrollmentID,
                     "has a Destination value of",
                     Destination,
                     "which is not a valid Destination response."))) %>%
  fselect(issue_display_cols)

nonstandard_CLS <- CurrentLivingSituation %>%
  fsubset(!is.na(CurrentLivingSituation) &
    !CurrentLivingSituation %in% c(allowed_current_living_sit)) %>%
  merge_check_info_dt(checkIDs = 55) %>%
  fmutate(
    Detail = str_squish(paste("EnrollmentID",
                     EnrollmentID,
                     "has a Current Living Situation value of",
                     CurrentLivingSituation,
                     "which is not a valid response."))) %>%
  fselect(issue_display_cols)

session$userData$file_structure_analysis_main(rbind(
  df_column_diffs,
  df_unexpected_data_types,
  df_nulls,
  disabling_condition_invalid,
  duplicate_client_id,
  duplicate_enrollment_id,
  duplicate_household_id,
  export_id_client,
  foreign_key_no_primary_personalid_enrollment,
  foreign_key_no_primary_projectid_enrollment,
  living_situation_invalid,
  no_enrollment_records,
  nonstandard_CLS,
  nonstandard_destination,
  rel_to_hoh_invalid,
  valid_values_client,
  files_with_brackets,
  ignore.attr=TRUE
  ) %>%
  fmutate(Type = factor(Type, levels = issue_levels)) %>%
  roworder(Type)
)

if(session$userData$file_structure_analysis_main() %>% 
   fsubset(Type == "High Priority") %>%
   nrow() > 0) {
  session$userData$valid_file(0)
  
  # if structural issues were found, reset gracefully
  showModal(
    modalDialog(
      "Your uploaded HMIS CSV Export has at least one High Priority File 
          Structure Error. To be able to read an uploaded hashed HMIS CSV 
          Export, Eva requires the .zip file to have zero High Priority File 
          Structure Errors. Thus, to use Eva, your upload must have zero High 
          Priority File Structure Errors. Please share the file structure 
          issues, prioritizing the High Priotity File Structure Errrors, 
          with your HMIS vendor to fix.",
      easyClose = TRUE,
      title = "Unsuccessful Upload: Your HMIS CSV Export is not
          structurally valid",
      footer = modalButton("OK")
    )
  )
  
  logMetadata(session, "Unsuccessful upload - not structurally valid")
  
  reset_postvalid_components(session)
} else{
  session$userData$valid_file(1)
}
