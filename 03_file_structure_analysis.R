######################
# PURPOSE: This program runs checks of the upload file's content.
# For example, it checks for incorrect date formats, missing columns,
# unexpected nulls, and more
######################

logToConsole(session, "Running file structure analysis")

# Prep --------------------------------------------------------------------

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
# Checks existence AND order. Reports on missing, extra, and otherwise misordered
check_incorrect_columns <- function(file) {
  ImportedColumns <- colnames(get(file))
  
  CorrectColumns <- cols_and_data_types %>%
      fsubset(File == {{file}}) %>%
      pull(Column)
  
  same_columns <- setequal(ImportedColumns, CorrectColumns)
  
  incorrect_columns <- if(same_columns) {
    data.table(
      Detail = "Misordered", 
      Column = ImportedColumns[ImportedColumns == CorrectColumns]
    )
  } else {
    data.table(
      Detail = c("Extra", "Missing"), 
      Column = c(
        setdiff(ImportedColumns, CorrectColumns), 
        setdiff(CorrectColumns, ImportedColumns)
      )
    )
  }
  incorrect_columns
}
df_incorrect_columns <- rbindlist(lapply(unique(cols_and_data_types$File), check_incorrect_columns)) %>%
  join(
    cols_and_data_types %>% fselect(Column, DataTypeHighPriority),
    on = "Column", 
    how = 'left'
  ) %>%
  merge_check_info_dt(checkIDs = 12) %>%
  fmutate(
    Type = fifelse(DataTypeHighPriority == 1, "High Priority", "Error"),
    Detail = str_squish(glue(
      "In the {file} file, the {
        fifelse(
          Detail == 'Extra',
          paste(Column, 'is an extra column'),
          paste('the', Column, 'column is missing')
        )}"
    ))
  )

# Unexpected data types -----------------------------------------------------
# includes date and non-date
unexpected_data_types <- function(file) {
  data <- get(file)
  cols_and_data_types %>% 
    fsubset((File == file) & (Column %in% colnames(data))) %>%
    fmutate(actual_type = sapply(Column, function(col) class(data[[col]])[1]))
}
df_unexpected_data_types <- rbindlist(lapply(unique(cols_and_data_types$File), unexpected_data_types)) %>%
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
        # Find rows that are NOT NA (i.e., not empty strings) but become NA upon numeric conversion
        # suppressWarnings() to prevent `NAs introduced by coercion` messages
        problematic_indices <- which(!is.na(data$Column) & is.na(suppressWarnings(as.numeric(data$Column))))
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


# ExportID Inconsistency --------------------------------------------------------
exportid_detail <- function(file, max_examples = 5L) {
  dt <- get(file)
  
  bad_rows <- which(dt$ExportID != session$userData$Export$ExportID)
  
  # if (length(bad_rows)) {
    shown <- bad_rows[seq_len(min(length(bad_rows), max_examples))]
    cols_and_data_types %>%
      fsubset((File == file) & (Column %in% colnames(dt))) %>%
      fmutate(
        has_many_issues_text = if(length(bad_rows) <= max_examples) '' else ', e.g.,',
        row_ids = paste(shown, collapse = ', ')
      )
  # } else {
  #   cols_and_data_types[0, `:=`(
  #     has_many_issues_text = NA,
  #     File = NA,
  #     row_ids = NA
  #   )]
  # }
}

inconsistent_export_ids <- lapply(cols_and_data_types$File, exportid_detail) %>%
  rbindlist() %>%
  na_omit() %>%
  merge_check_info_dt(checkIDs = 151) %>%
  fmutate(
    Detail = str_squish(glue(
      "One or more rows in {File}.csv have an ExportID that differs from the one in the Export.csv file. 
      See {has_many_issues_text} rows: {row_ids}",
    ))
  ) %>%
  fselect(issue_display_cols) %>%
  funique()


# Invalid values -----------------------------
source("detect_invalid_values.R", local=TRUE)
valid_values <- final_summary %>%
  merge_check_info(checkIDs = 50) %>%
  mutate(Detail = paste(name, "has", n, "rows with invalid values")) %>% 
  select(all_of(issue_display_cols))

# Integrity Enrollment ----------------------------------------------------
if (nrow(Enrollment) == 0) {
  no_enrollment_records <- data.table(
    Detail = "There are 0 enrollment records in the Enrollment.csv file"
  ) %>%
  merge_check_info_dt(checkIDs = 101)
} else {
  no_enrollment_records <- data.table()
}

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

session$userData$file_structure_analysis_main(rbind(
  df_incorrect_columns,
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
