######################
# PURPOSE: This program runs checks of the upload file's content.
# For example, it checks for incorrect date formats, missing columns,
# unexpected nulls, and more
######################

logToConsole("Running file structure analysis")

# Prep --------------------------------------------------------------------

export_id_from_export <- Export %>% pull(ExportID)

high_priority_columns <- cols_and_data_types %>%
  filter(DataTypeHighPriority == 1) %>%
  pull(Column) %>%
  unique()


# Incorrect Date Formats --------------------------------------------------

df_date_types <-
  problems %>%
  filter(str_detect(expected, "date") == TRUE) %>%
  mutate(
    File = str_remove(basename(file), ".csv")
  ) %>%
  left_join(cols_and_data_types, by = c("File", "col" = "ColumnNo")) %>%
  mutate(
    Issue = "Incorrect Date Format",
    Type = if_else(Column %in% c(high_priority_columns), 
                   "High Priority", "Error"),
    Guidance = 
      str_squish("Dates in the HMIS CSV Export should be in yyyy-mm-dd or
      yyyy-mm-dd hh:mm:ss format, in alignment with the HMIS CSV Format
      Specifications. Please check the Specifications for the file and column
      identified in the Detail and ensure the correct date format is used in the
      export."),
    Detail = str_squish(paste(
      "Please check that the",
      Column,
      "column in the",
      File,
      "file has the correct date format."))
  ) %>%
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
        Issue = "Incorrect Columns",
        Type = if_else(
          ColumnName %in% c(high_priority_columns),
          "High Priority",
          "Warning"
        ),
        Guidance = str_squish(
          "Your HMIS CSV Export should contain - with identical, case-sensitive
        spelling - only the columns specified in the columns.csv file. Please
        remove any extra columns and make sure you have all the required
        columns."
        ),
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
      ) %>%
      select(all_of(issue_display_cols)) %>%
      unique()
  }
}

check_data_types <- function(quotedfile) {
  barefile <- get(quotedfile)
  if(nrow(barefile) > 0) {
    
    data_types <- as.data.frame(summary.default(barefile)) %>% 
      filter(Var2 != "Length" & 
               ((Var2 == "Class" & Freq %in% c("Date", "POSIXct")) |
                  Var2 == "Mode")) %>% 
      mutate(
        File = quotedfile,
        ImportedDataType = case_when(
          Var2 == "Class" & Freq == "Date" ~ "date",
          Var2 == "Class" & Freq == "POSIXct" ~ "datetime",
          Var2 == "Mode" ~ Freq,
          TRUE ~ "something's wrong"
        )) %>%
      group_by(Var1) %>%
      slice_min(order_by = Var2)  %>%
      ungroup() %>%
      select(File, "Column" = Var1, ImportedDataType)
    
    y <- cols_and_data_types %>% 
      left_join(data_types, by = c("File", "Column")) %>%
      filter(DataType != ImportedDataType) %>%
      mutate(
        Issue = "Incorrect Data Type",
        Type = if_else(DataTypeHighPriority == 1, "High Priority", "Error"),
        Guidance = 
          str_squish("Data types must align with the HMIS CSV Format
                     Specifications. Please review the Specifications for the
                     data types associated with the file and column listed in
                     the detail and make the necessary updates."),
        Detail = str_squish(paste0(
          "In the ",
          quotedfile,
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
            ImportedDataType == "numeric" ~ "integer",
            ImportedDataType == "character" ~ "string",
            TRUE ~ ImportedDataType
            ),
          "."
        ))
      ) %>%
      select(all_of(issue_display_cols))
    y
  }
}

check_for_bad_nulls <- function(file) {
  barefile <- get(file)
  total_rows = nrow(barefile)
  if (total_rows > 1) {
    # select nulls-not-allowed columns
    nulls_not_allowed_cols <- cols_and_data_types %>%
      filter(File == file & NullsAllowed == 0 & Column %in% names(get(file))) %>%
      pull(Column)

    # select subset of columns with nulls
    barefile <- get(file) %>%
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
          sum(value) == total_rows ~ "All rows affected", 
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
        mutate(
          Issue = "Nulls not allowed or incorrect data type",
          Type = if_else(DataTypeHighPriority == 1, "High Priority", "Error"),
          Guidance = 
            str_squish("Either there is a column with nulls where they are not
                       allowed, or there is a column with an incorrect data type.
                       Please review the HMIS CSV Format Specifications for the
                       data types and null requirements associated with the file
                       and column listed in the detail and make the necessary updates."),
          Detail = str_squish(glue("The {Column} column in the {file} file contains nulls
                        or incorrect data types. {row_ids}"))
        ) %>%
        select(all_of(issue_display_cols)) %>%
        unique()
    }
  }
}

# Integrity Structure -----------------------------------------------------

df_column_diffs <- map_df(unique(cols_and_data_types$File), check_columns)

df_data_types <- map_df(unique(cols_and_data_types$File), check_data_types)

df_nulls <- map_df(unique(cols_and_data_types$File), check_for_bad_nulls)

# Integrity Client --------------------------------------------------------

export_id_client <- Client %>%
  filter(as.character(ExportID) != export_id_from_export) %>%
  mutate(
    Issue = "ExportID mismatch",
    Type = "Error",
    Guidance = 
      str_squish("Per the HMIS CSV Formatting Specifications, the ExportID in
                 your Export and Client files must match. There should be one
                 unique ExportID that will be used to identify all CSV files
                 genereated as part of the same export process."),
    Detail = str_squish(paste(
      "The Export file says the ExportID is",
      export_id_from_export,
      "but in your Client file, it is",
      ExportID
    ))
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

valid_values_client <- Client %>%
  mutate(
    VeteranStatus = VeteranStatus %in% c(yes_no_enhanced),
    RaceNone = RaceNone %in% c(dkr_dnc) | is.na(RaceNone),
    AmIndAKNative = AmIndAKNative %in% c(yes_no),
    Asian = Asian %in% c(yes_no),
    BlackAfAmerican = BlackAfAmerican %in% c(yes_no),
    NativeHIPacific = NativeHIPacific %in% c(yes_no),
    White = White %in% c(yes_no),
    Ethnicity = Ethnicity %in% c(yes_no_enhanced),
    Female = Female %in% c(yes_no),
    Male = Male %in% c(yes_no),
    NoSingleGender = NoSingleGender %in% c(yes_no),
    Transgender = Transgender %in% c(yes_no),
    Questioning = Questioning %in% c(yes_no),
    GenderNone = GenderNone %in% c(dkr_dnc) | is.na(GenderNone)
  ) %>%
  group_by_all() %>%
  summarise(
    VeteranStatus = min(VeteranStatus, na.rm = FALSE),
    RaceNone = min(RaceNone, na.rm = FALSE),
    AmIndAKNative = min(AmIndAKNative, na.rm = FALSE),
    Asian = min(Asian, na.rm = FALSE),
    BlackAfAmerican = min(BlackAfAmerican, na.rm = FALSE),
    NativeHIPacific = min(NativeHIPacific, na.rm = FALSE),
    White = min(White, na.rm = FALSE),
    Ethnicity = min(Ethnicity, na.rm = FALSE),
    Female = min(Female, na.rm = FALSE),
    Male = min(Male, na.rm = FALSE),
    NoSingleGender = min(NoSingleGender, na.rm = FALSE),
    Transgender = min(Transgender, na.rm = FALSE),
    Questioning = min(Questioning, na.rm = FALSE),
    GenderNone = min(GenderNone, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  select(
    VeteranStatus,
    RaceNone,
    AmIndAKNative,
    Asian,
    BlackAfAmerican,
    NativeHIPacific,
    White,
    Ethnicity,
    Female,
    Male,
    NoSingleGender,
    Transgender,
    Questioning,
    GenderNone
  ) %>%
  pivot_longer(cols = everything()) %>%
  filter(value == 0) %>%
  count(name) %>%
  mutate(
    Issue = "Invalid value in Client file",
    Type = "Error",
    Guidance = 
      str_squish("All columns in the client file should contain only the values
                 listed in the HMIS CSV Format Specifications for that specific
                 column. Please review the Specifications for the column
                 identified in the Detail and ensure all values in the export
                 align with the associated values list found in 'Appendix B -
                 Lists' of the Specifications."),
    Detail = case_when(
      name == "VeteranStatus" ~ paste("VeteranStatus has", n,
                                      "rows with invalid values"),
      name == "RaceNone" ~ paste("RaceNone has", n,
                                 "rows with invalid values"),
      name == "AmIndAKNative" ~ paste("AmIndAKNative has", n,
                                      "rows with invalid values"),
      name == "Asian" ~ paste("Asian has", n,
                              "rows with invalid values"),
      name == "BlackAfAmerican" ~ paste("BlackAfAmerican has", n,
                                        "rows with invalid values"),
      name == "NativeHIPacific" ~ paste("NativeHIPacific has", n,
                                        "Rows with invalid values"),
      name == "White" ~ paste("White has", n,
                              "rows with invalid values"),
      name == "Ethnicity" ~ paste("Ethnicity has", n,
                                  "rows with invalid values"),
      name == "Female" ~ paste("Female has", n,
                               "rows with invalid values"),
      name == "Male" ~ paste("Male has", n,
                             "rows with invalid values"),
      name == "NoSingleGender" ~ paste("NoSingleGender has", n,
                                       "rows with invalid values"),
      name == "Transgender" ~ paste("Transgender has", n,
                                    "rows with invalid values"),
      name == "Questioning" ~ paste("Questioning has", n,
                                    "rows with invalid values"),
      name == "GenderNone" ~ paste("GenderNone has", n,
                                   "rows with invalid values")
    )
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

duplicate_client_id <- Client %>%
  get_dupes(PersonalID) %>%
  mutate(
    Issue = "Duplicate PersonalIDs in Client.csv",
    Type = "High Priority",
    Guidance = 
      str_squish("PersonalIDs should be unique in the Client file."),
    Detail = paste("There are", dupe_count, "for PersonalID", PersonalID)
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

# Integrity Enrollment ----------------------------------------------------
if(nrow(Enrollment) == 0) {
  no_enrollment_records <- data.frame(
    Issue = "No enrollment records",
    Type = "High Priority",
    Guidance = guidance_no_enrollments,
    Detail = "There are 0 enrollment records in the Enrollment.csv file"
  )
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
  mutate(
    Issue = "Duplicate EnrollmentIDs",
    Type = "High Priority",
    Guidance = 
      str_squish("EnrollmentIDs should be unique in the Enrollment.csv file."),
    Detail = str_squish(paste0(
      "There are ",
      dupe_count,
       " duplicates found for EnrollmentID ",
      EnrollmentID,
      "."))
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

personal_ids_in_client <- Client %>% pull(PersonalID)

foreign_key_no_primary_personalid_enrollment <- Enrollment %>%
  filter(!PersonalID %in% c(personal_ids_in_client)) %>%
  mutate(
    Issue = "PersonalID missing from Client.csv",
    Type = "High Priority",
    Guidance = 
      str_squish("Per the HMIS CSV Format Specifications, all PersonalIDs in the
                 Enrollment file should have a matching record in the Client file."),
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
  mutate(
    Issue = "ProjectID missing from Project.csv",
    Type = "High Priority",
    Guidance = 
      str_squish("Per the HMIS CSV Format Specifications, all ProjectIDs in the
                 Enrollment file should have a matching record in the Project
                 file."),
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
  mutate(
    Issue = "Invalid Disabling Condition",
    Type = "Error",
    Guidance = 
      str_squish("Please review the HMIS CSV Format Specifications for
                 DisablingCondition and ensure all values in the export align
                 with the associated values list found in 'Appendix B - Lists'
                 of the Specifications."),
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
  mutate(
    Issue = "Invalid Living Situation value",
    Type = "Error",
    Guidance = str_squish("Please review the HMIS CSV Format Specifications for
                          LivingSituation and ensure all values in the export
                          align with the associated values list found in 
                          'Appendix B - Lists' of the Specifications."),
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
  mutate(
    Issue = "Invalid RelationshipToHoH value",
    Type = "Error",
    Guidance = str_squish("Please review the HMIS CSV Format Specifications for
                          RelationshipToHoH and ensure all values in the export
                          align with the associated values list found in
                          'Appendix B - Lists' of the Specifications."),
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
  mutate(
    Issue = "HouseholdID not incrementing correctly",
    Type = "High Priority",
    Guidance = 
      str_squish("The HouseholdID must be unique to the household stay in a
                 project; reuse of the identification of the same or similar
                 household upon readmission into the project is not permitted.
                 Please review the HMIS Data Standards for more details."),
    Detail = paste("HouseholdID", 
                   HouseholdID,
                   "is reused across",
                   dupe_count,
                   "Enrollments into different projects.")
  ) %>%
  select(all_of(issue_display_cols)) %>%
  unique()

# move_in_date_invalid <- Enrollment %>%
#   left_join(Exit %>% select(EnrollmentID, ExitDate), by = "EnrollmentID") %>%
#   mutate(
#     Issue = if_else(
#       (
#         MoveInDate >= EntryDate &
#           MoveInDate <= coalesce(ExitDate, meta_HUDCSV_Export_Date))
#        |
#         is.na(MoveInDate),
#       "Nothing",
#       "Invalid MoveInDate"
#     ),
#     Type = "Error",
#     Guidance = paste(
#       "Enrollment ID", 
#       EnrollmentID, 
#       "has a Move-In Date of",
#       MoveInDate,
#       "which does not fall between the Entry Date of",
#       EntryDate,
#       "and the Exit Date (or end of the reporting period.)")
#   ) %>%
#   filter(Issue != "Nothing") %>%
#   select(all_of(issue_display_cols)) %>%
#   
#   
#   unique()

# Integrity Living Situation ----------------------------------------------

nonstandard_destination <- Exit %>%
  filter(!is.na(Destination) &
           !Destination %in% c(allowed_destinations)) %>%
  mutate(
    Issue = "Invalid Destination value",
    Type = "Error",
    Guidance = str_squish("Please review the HMIS CSV Format Specifications for
                          Destination and ensure all values in the export align
                          with the associated values list found in 'Appendix 
                          B - Lists' of the Specifications."),
    Detail = str_squish(paste("EnrollmentID",
                     EnrollmentID,
                     "has a Destination value of",
                     Destination,
                     "which is not a valid Destination response."))) %>%
  select(all_of(issue_display_cols))


nonstandard_CLS <- CurrentLivingSituation %>%
  filter(!is.na(CurrentLivingSituation) &
    !CurrentLivingSituation %in% c(allowed_current_living_sit)) %>%
  mutate(
    Issue = "Non-standard Current Living Situation",
    Type = "Error",
    Guidance = 
      str_squish("This column contains a value that may have been retired from 
                 an old version of the Data Standards or was miskeyed. Please
                 review the HMIS CSV Format Specifications for
                 CurrentLivingSituation and ensure all values in the export
                 align with the associated values list found in 'Appendix B -
                 Lists' of the Specifications."),
    Detail = str_squish(paste("EnrollmentID",
                     EnrollmentID,
                     "has a Current Living Situation value of",
                     CurrentLivingSituation,
                     "which is not a valid response."))) %>%
  select(all_of(issue_display_cols))

file_structure_analysis_main <- rbind(
  df_column_diffs,
  df_data_types,
  df_date_types,
  df_nulls,
  export_id_client,
  valid_values_client,
  duplicate_client_id,
  duplicate_enrollment_id,
  duplicate_household_id,
  foreign_key_no_primary_personalid_enrollment,
  foreign_key_no_primary_projectid_enrollment,
  disabling_condition_invalid,
  living_situation_invalid,
  rel_to_hoh_invalid,
  nonstandard_destination,
  nonstandard_CLS,
  no_enrollment_records
) %>%
  mutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning"))) %>%
  arrange(Type)

if(file_structure_analysis_main %>% filter(Type == "High Priority") %>% nrow() > 0) {
  structural_issues <- 1
} else{
  structural_issues <- 0
}
