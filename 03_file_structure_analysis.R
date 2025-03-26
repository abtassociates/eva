######################
# PURPOSE: This program runs checks of the upload file's content.
# For example, it checks for incorrect date formats, missing columns,
# unexpected nulls, and more
######################

logToConsole("Running file structure analysis")

# Prep --------------------------------------------------------------------

export_id_from_export <- Export() %>% pull(ExportID)

high_priority_columns <- cols_and_data_types %>%
  filter(DataTypeHighPriority == 1) %>%
  pull(Column) %>%
  unique()

# Brackets --------------------------------------------------------------
files_with_brackets <- data.frame()
for(file in unique(cols_and_data_types$File)) {
  m <- get(file)  # Load dataset
  char_cols <- which(sapply(m, is.character))
  if (length(char_cols) == 0) next
  
  m_mat <- as.matrix(m[, char_cols, drop = FALSE])  # Convert relevant columns to matrix
  if (any(grepl(bracket_regex, m_mat, perl=TRUE), na.rm=TRUE)) {
    files_with_brackets <- data.frame(
      File = file,
      Detail = str_squish("Found one or more brackets in your HMIS CSV Export. 
                See Impermissible Character Detail export for the precise location 
                of these characters.")
    ) %>%
      merge_check_info(checkIDs = 134) %>%
      select(all_of(issue_display_cols))
    break
  }
}

# Incorrect Columns ------------------------------------------------------

check_columns <- function(file) {
  ImportedColumns <- colnames(get(file))
  
  CorrectColumns <- cols_and_data_types %>%
      filter(File == {{file}}) %>%
      pull(Column)
  
  extra_columns <- setdiff(ImportedColumns, CorrectColumns)
  missing_columns <- setdiff(CorrectColumns, ImportedColumns)
  
  if(length(extra_columns) || length(missing_columns)) {
    col_diffs <- data.frame(
      ColumnName = c(missing_columns, extra_columns),
      Status = c(rep("Missing", length(missing_columns)),
                 rep("Extra", length(extra_columns)))
    ) %>%
    arrange(ColumnName) %>%
    mutate(
      Detail = str_squish(paste(
        "In the",
        file,
        "file,",
        if_else(
          Status == "Extra",
          paste(ColumnName, "is an extra column"),
          paste("the", ColumnName, "column is missing")
        )
      ))
    )
    
    col_diffs_hp <- col_diffs %>%
      filter(ColumnName %in% c(high_priority_columns)) %>%
      merge_check_info(checkIDs = 12) %>%
      select(all_of(issue_display_cols)) %>%
      unique()


    col_diffs_error <- col_diffs %>%
      filter(!(ColumnName %in% c(high_priority_columns))) %>%
      merge_check_info(checkIDs = 82) %>%
      select(all_of(issue_display_cols)) %>%
      unique()

    return(
      rbind(col_diffs_hp, col_diffs_error)
    )
  }
}
df_column_diffs <- map_df(unique(cols_and_data_types$File), check_columns)

# Unexpected data types -----------------------------------------------------
# includes date and non-date
unexpected_data_types <- function(file) {
  data <- get(file)
  cols_and_data_types %>% 
    filter(File == file, Column %in% colnames(data)) %>%
    rowwise() %>%
    mutate(
      actual_type = class(data[[Column]])[1]
    ) %>%
    ungroup() %>%
    filter(
      DataType != case_when(
        actual_type == "POSIXct" ~ "datetime",
        actual_type == "Date" ~ "date",
        TRUE ~ actual_type
      )
    ) %>%
    mutate(
      Detail = if_else(
        !(DataType %in% c("date", "datetime")),
        glue("In the {file} file, the {Column} column should have a data type of {case_when(
          DataType == 'numeric' ~ 'integer',
          DataType == 'character' ~ 'string',
          TRUE ~ DataType
        )} but in this file, it is {case_when(
          actual_type == 'numeric' ~ 'integer',
          TRUE ~ actual_type
        )}."),
        glue("Please check that the {Column} column in the {file} file has the correct {DataType} format.")
      ),
      checkID = if_else(
        DataTypeHighPriority == 1, 
        if_else(DataType %in% c("date", "datetime"), 11, 13),
        if_else(DataType %in% c("date", "datetime"), 47, 48)
      )
    ) %>%
    inner_join(evachecks, join_by(checkID == ID)) %>%
    select(all_of(issue_display_cols))
}
df_unexpected_data_types <- map_df(unique(cols_and_data_types$File), unexpected_data_types)

check_for_bad_nulls <- function(file) {
  barefile <- get(file)
  if (nrow(barefile) > 1) {
    # select nulls-not-allowed columns
    nulls_not_allowed_cols <- cols_and_data_types %>%
      filter(File == file & NullsAllowed == 0 & Column %in% names(get(file))) %>%
      pull(Column)

    # select subset of columns with nulls
    barefile <- barefile %>%
      select(all_of(nulls_not_allowed_cols)) %>%
      mutate_all(~ifelse(is.na(.), 1, 0)) %>%
      select_if(~any(. == 1))

    if(ncol(barefile) > 0) {
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
        select(Column, row_ids) %>% 
        left_join(cols_and_data_types %>% 
                    select(Column, DataTypeHighPriority),
                  by = "Column") %>%
        merge_check_info(checkIDs = 6) %>%
        mutate(
          Type = if_else(DataTypeHighPriority == 1, "High Priority", "Error"),
          Detail = str_squish(glue("The {Column} column in the {file} file contains nulls
                        or incorrect data types. {row_ids}"))
        ) %>%
        select(all_of(issue_display_cols)) %>%
        unique()
    }
  }
}
df_nulls <- map_df(unique(cols_and_data_types$File), check_for_bad_nulls)

# Integrity Client --------------------------------------------------------
# CHECK: export ID differs
export_id_client <- Client %>%
  filter(as.character(ExportID) != export_id_from_export) %>%
  merge_check_info(checkIDs = 49) %>%
  mutate(
    Detail = str_squish(paste(
      "The Export file says the ExportID is",
      export_id_from_export,
      "but in your Client file, it is",
      ExportID
    ))
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

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
  data.frame(name = col_name, n = sum(unexpected))
}

valid_values_client <- existing_cols %>%
  map_df(get_unexpected_count) %>%
  filter(n > 0) %>%
  merge_check_info(checkIDs = 50) %>%
  mutate(Detail = paste(name, "has", n, "rows with invalid values")) %>% 
  select(all_of(issue_display_cols))

# CHECK: duplicate client ID
duplicate_client_id <- Client %>%
  get_dupes(PersonalID) %>%
  merge_check_info(checkIDs = 7) %>%
  mutate(
    Detail = paste("There are", dupe_count, "duplicates for PersonalID", PersonalID)
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

# Integrity Enrollment ----------------------------------------------------
if (nrow(Enrollment) == 0) {
  no_enrollment_records <- data.frame(
    Detail = "There are 0 enrollment records in the Enrollment.csv file"
  ) %>%
  merge_check_info(checkIDs = 101)
} else {
  no_enrollment_records <- data.frame()
}

duplicate_enrollment_id <- Enrollment %>%
  get_dupes(EnrollmentID) %>%
  merge_check_info(checkIDs = 8) %>%
  mutate(
    Detail = str_squish(
      paste0(
        "There are ",
        dupe_count,
        " duplicates found for EnrollmentID ",
        EnrollmentID,
        "."
      )
    )
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

personal_ids_in_client <- Client %>% pull(PersonalID)

foreign_key_no_primary_personalid_enrollment <- Enrollment %>%
  filter(!PersonalID %in% c(personal_ids_in_client)) %>%
  merge_check_info(checkIDs = 9) %>%
  mutate(
    Detail = str_squish(paste(
      "PersonalID",
      PersonalID,
      "is in the Enrollment file but not in the Client file."
    ))
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

projectids_in_project <- Project %>% pull(ProjectID)

foreign_key_no_primary_projectid_enrollment <- Enrollment %>%
  filter(!ProjectID %in% c(projectids_in_project)) %>%
  merge_check_info(checkIDs = 10) %>%
  mutate(
    Detail = str_squish(paste(
      "ProjectID",
      ProjectID,
      "is in the Enrollment file but not in the Project file."
    ))
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

disabling_condition_invalid <- Enrollment %>%
  filter(!DisablingCondition %in% c(yes_no_enhanced)) %>%
  merge_check_info(checkIDs = 51) %>%
  mutate(
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a DisablingCondition of",
      DisablingCondition,
      "which is an invalid value."
    ))
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

living_situation_invalid <- Enrollment %>%
  filter(!is.na(LivingSituation) &
    !LivingSituation %in% c(allowed_prior_living_sit)) %>%
  merge_check_info(checkIDs = 52) %>%
  mutate(
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a LivingSituation of",
      LivingSituation,
      "which is not a valid value."
    ))
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

rel_to_hoh_invalid <- Enrollment %>%
  filter(!RelationshipToHoH %in% c(1:5, 99) & !is.na(RelationshipToHoH)) %>%
  merge_check_info(checkIDs = 53) %>%
  mutate(
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a RelationshipToHoH of",
      RelationshipToHoH,
      "which is invalid value."
    ))
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

# Group by HouseholdID and ProjectID, and count the number of unique PersonalIDs in each group
duplicate_household_id <- Enrollment %>%
  distinct(HouseholdID, ProjectID) %>%
  filter(!is.na(HouseholdID)) %>%
  get_dupes(HouseholdID) %>%
  merge_check_info(checkIDs = 98) %>%
  mutate(
    Detail = paste("HouseholdID", 
                   HouseholdID,
                   "is reused across",
                   dupe_count,
                   "Enrollments into different projects.")
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

# Integrity Living Situation ----------------------------------------------

nonstandard_destination <- Exit %>%
  filter(!is.na(Destination) &
           !Destination %in% c(allowed_destinations)) %>%
  merge_check_info(checkIDs = 54) %>%
  mutate(
    Detail = str_squish(paste("EnrollmentID",
                     EnrollmentID,
                     "has a Destination value of",
                     Destination,
                     "which is not a valid Destination response."))) %>%
  select(all_of(issue_display_cols))

nonstandard_CLS <- CurrentLivingSituation %>%
  filter(!is.na(CurrentLivingSituation) &
    !CurrentLivingSituation %in% c(allowed_current_living_sit)) %>%
  merge_check_info(checkIDs = 55) %>%
  mutate(
    Detail = str_squish(paste("EnrollmentID",
                     EnrollmentID,
                     "has a Current Living Situation value of",
                     CurrentLivingSituation,
                     "which is not a valid response."))) %>%
  select(all_of(issue_display_cols))

file_structure_analysis_main(rbind(
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
  files_with_brackets
  ) %>%
  mutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning"))) %>%
  arrange(Type)
)

if(file_structure_analysis_main() %>% 
filter(Type == "High Priority") %>% 
nrow() > 0) {
  valid_file(0)
} else{
  valid_file(1)
}
