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

# non-ascii --------------------------------------------------------------
non_ascii_files <- function() {
  # Initialize an empty list to store the results
  files_with_non_ascii <- list()
  non_ascii_or_bracket <- function(x) {
    str_detect(x, "[^ -~]|.\\[|\\]|\\<|\\>|\\{|\\}")
  }
  
  files_with_non_ascii <- lapply(unique(cols_and_data_types$File), function(file) {
    # Flag cells containing brackets or non-ascii chars
    non_ascii_data <- get(file) %>%
      mutate(across(everything(), non_ascii_or_bracket))
    
    if(any(non_ascii_data, na.rm = TRUE)) {
      # get the cells that contain a non-ascii or bracket char
      # Find rows that contain any non-ASCII characters
      non_ascii_cells <- which(as.matrix(non_ascii_data), arr.ind = TRUE)
      
      non_ascii_info <- mapply(function(row, col) {
        value <- get(file)[row, col]
        if (!is.na(value)) {
          chars <- unlist(
            stringi::stri_extract_all_regex(
              value, "[^ -~]|\\[|\\]|\\<|\\>|\\{|\\}"
            )
          )
          data.frame(
            File = file,
            Detail = paste0(
              "Found impermissible character(s) in ", 
              file, ".csv, ", 
              "column ", col, 
              ", line ", row, 
              ": ", paste(chars, collapse=", ")
            )
          )
        }
      }, non_ascii_cells[, "row"], non_ascii_cells[, "col"], SIMPLIFY = FALSE)
      
      # Convert the list of data frames to a single data frame
      non_ascii_info <- do.call(rbind, non_ascii_info)

      return(non_ascii_info)
    }
  })
  
  # Combine all data frames in the list into one data frame
  files_with_non_ascii <- bind_rows(files_with_non_ascii)
  
  return(files_with_non_ascii)
}

files_with_non_ascii <- non_ascii_files() 
if(nrow(files_with_non_ascii) > 0) {
  files_with_non_ascii <- files_with_non_ascii %>%
    merge_check_info(checkIDs = 134) %>%
    select(all_of(issue_display_cols))
}



# Incorrect Date Formats --------------------------------------------------

df_date_types <-
  problems %>%
  filter(str_detect(expected, "date") == TRUE) %>%
  mutate(
    File = str_remove(basename(file), ".csv")
  ) %>%
  left_join(cols_and_data_types, by = c("File", "col" = "ColumnNo")) %>%
  mutate(
    Detail = str_squish(paste(
      "Please check that the",
      Column,
      "column in the",
      File,
      "file has the correct date format."))
  )
  
incorrect_date_types_hp <- df_date_types %>%
  filter(Column %in% c(high_priority_columns)) %>%
  merge_check_info(checkIDs = 11) %>%
  select(all_of(issue_display_cols)) %>% unique()

incorrect_date_types_error <- df_date_types %>%
  filter(!(Column %in% c(high_priority_columns))) %>%
  merge_check_info(checkIDs = 47) %>%
  select(all_of(issue_display_cols)) %>% unique()

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

# Unexpected (non-date) data types -----------------------------------------------------
data_types <- problems %>%
  filter(str_detect(expected, "date", negate = TRUE)) %>%
  mutate(
    File = str_remove(basename(file), ".csv"),
  ) %>%
  left_join(cols_and_data_types, by = c("File", "col" = "ColumnNo")) %>%
  mutate(
    Detail = str_squish(paste0(
      "In the ",
      File,
      " file, the ",
      Column,
      " column should have a data type of ",
      case_when(
        DataType == "numeric" ~ "integer",
        DataType == "character" ~ "string",
        TRUE ~ DataType
      ),
      " but in this file, it is ",
      case_when(
        all(map_lgl(actual, is.numeric)) ~ "integer",
        all(map_lgl(actual, is.Date)) ~ "date",
        TRUE ~ "string"
      ),
      "."
    ))
  )

df_data_types <- rbind(
  data_types %>% 
    filter(DataTypeHighPriority == 1) %>% 
    merge_check_info(checkIDs = 13) %>%
    select(all_of(issue_display_cols)) %>%
    unique(),
  
  data_types %>% 
    filter(DataTypeHighPriority == 0) %>% 
    merge_check_info(checkIDs = 48) %>%
    select(all_of(issue_display_cols)) %>%
    unique()
)


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
cols <- c("VeteranStatus", "RaceNone", "AmIndAKNative", "Asian", "BlackAfAmerican", 
          "NativeHIPacific", "White", "MidEastNAfrican", "HispanicLatinaeo", 
          "Woman", "Man", "NonBinary", "Transgender", "CulturallySpecific",
          "DifferentIdentity", "Questioning", "GenderNone")

valid_values <- list(yes_no_enhanced, c(dkr_dnc, NA), yes_no, yes_no, yes_no, yes_no, 
                     yes_no, yes_no, yes_no, yes_no, yes_no, yes_no, yes_no,
                     yes_no, yes_no, yes_no, c(dkr_dnc, NA))


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
  no_enrollment_records <- data.frame(
    Issue = character(),
    Type = character(),
    Guidance = character(),
    Detail = character()
  )
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
  df_data_types,
  df_nulls,
  disabling_condition_invalid,
  duplicate_client_id,
  duplicate_enrollment_id,
  duplicate_household_id,
  export_id_client,
  foreign_key_no_primary_personalid_enrollment,
  foreign_key_no_primary_projectid_enrollment,
  incorrect_date_types_error,
  incorrect_date_types_hp,
  living_situation_invalid,
  no_enrollment_records,
  nonstandard_CLS,
  nonstandard_destination,
  rel_to_hoh_invalid,
  valid_values_client,
  files_with_non_ascii
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
