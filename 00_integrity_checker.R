

# Prep --------------------------------------------------------------------

files <- c(
#  "Affiliation",
  "Assessment",
  # "AssessmentQuestions",
  # "AssessmentResults",
  "Client",
  "CurrentLivingSituation",
  # "Disabilities",
  "EmploymentEducation",
  "Enrollment",
  "EnrollmentCoC",
  "Event",
  "Exit",
  "Export",
  "Funder",
  "HealthAndDV",
  "IncomeBenefits",
  "Inventory",
  "Organization",
  "Project",
  "ProjectCoC",
  "Services",
  "User",
  "YouthEducationStatus"
)

display_cols <- c("Issue", "Type", "Guidance","Detail")

export_id_from_export <- Export %>% pull(ExportID)

cols_and_data_types <- read_csv("public_data/columns.csv", col_types = cols())

col_counts <- cols_and_data_types %>%
  group_by(File) %>%
  summarise(ColumnCount = n())

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
    Guidance = "Dates in the HMIS CSV Export should be in yyyy-mm-dd or
      yyyy-mm-dd hh:mm:ss format, in alignment with the HMIS CSV Format
      Specifications.",
    Detail = str_squish(paste(
      "Please check that the", Column, "column in the", File, "file has the correct date format."))
  ) %>%
  select(all_of(display_cols)) %>% unique()

# Incorrect Columns ------------------------------------------------------
check_columns <- function(file) {
  col_diffs <- tibble(
    ImportedColumns = 
      colnames(!!rlang::ensym(file)),
    CorrectColumns = 
      cols_and_data_types %>%
      filter(File == {{file}}) %>%
      pull(Column)
  ) %>%
  mutate(
    isExtra = !(ImportedColumns %in% CorrectColumns),
    isMissing = !(CorrectColumns %in% ImportedColumns),
    Issue = "Incorrect Columns",
    Type = if_else(CorrectColumns %in% c(high_priority_columns), 
                   "High Priority", "Warning"),
    Guidance = str_squish(
      "Your HMIS CSV Export should contain - with identical, case-sensitive spelling - all and only the columns specified in the columns.csv file. 
      Please remove any extra columns and make sure you have all missing columns."),
    Detail = str_squish(paste(
      "In the",
      file,
      "file,",
      if_else(isExtra,
              paste(ImportedColumns,"is an extra column"),
              paste("the",CorrectColumns,"column is missing")
      )
    ))
  ) %>%
  filter(isExtra | isMissing) %>%
  select(all_of(display_cols)) %>% 
  unique()
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
        Guidance = "Data types must all be correct",
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
          " but in this file, it is",
          case_when(
            ImportedDataType == "numeric" ~ "integer",
            ImportedDataType == "character" ~ "string",
            TRUE ~ ImportedDataType
            ),
          ". The PersonalID must be unique within the Client.csv."
        ))
      ) %>%
      select(all_of(display_cols))
    y
  }
}

check_for_bad_nulls <- function(quotedfile) {
  barefile <- get(quotedfile)
  if (nrow(barefile) > 1){
    barefile %>%
      mutate(across(everything(), ~ is.na(.x))) %>% 
      summarise(across(everything(), max)) %>%
      pivot_longer(cols = everything(), 
                   names_to = "Column", 
                   values_to = "NullsPresent") %>%
      mutate(File = quotedfile) %>%
      left_join(cols_and_data_types, by = c("File", "Column")) %>%
      filter(NullsAllowed == 0 & NullsPresent == 1) %>%
      mutate(
        Issue = "Nulls not allowed or incompatible data type in column",
        Type = if_else(DataTypeHighPriority == 1, "High Priority", "Error"),
        Guidance = "Certain columns cannot contain nulls or incompatible data types.",
        Detail = str_squish(paste(
          "The",
          Column,
          "column in the",
          File,
          "file contains nulls or incompatible data types."
        ))
      ) %>%
      select(all_of(display_cols))}
}

# Integrity Structure -----------------------------------------------------

df_column_diffs <- map_df(files, check_columns)
df_data_types <- map_df(files, check_data_types)
df_nulls <- map_df(files, check_for_bad_nulls)


# Integrity Client --------------------------------------------------------

export_id_client <- Client %>%
  filter(as.character(ExportID) != export_id_from_export) %>%
  mutate(
    Issue = "ExportID mismatch",
    Type = "Error",
    Guidance = "The ExportID in your Enrollment and Client files must match.",
    Detail = str_squish(paste(
      "The Export file says the ExportID is",
      export_id_from_export,
      "but in your Client file, it is",
      ExportID
    ))
  ) %>%
  select(all_of(display_cols)) %>%
  unique()

yes_no_enhanced <- c(0, 1, 8, 9, 99)
yes_no <- c(0, 1, 99)

valid_values_client <- Client %>%
  mutate(
    VeteranStatus = VeteranStatus %in% c(yes_no_enhanced),
    RaceNone = RaceNone %in% c(8, 9, 99) | is.na(RaceNone),
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
    GenderNone = GenderNone %in% c(8, 9, 99) | is.na(GenderNone)
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
    Guidance = "All columns in the client file should contain only valid values",
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
  select(all_of(display_cols))

duplicate_client_id <- Client %>%
  get_dupes(PersonalID) %>%
  mutate(
    Issue = "Duplicate PersonalIDs found in the Client file",
    Type = "High Priority",
    Guidance = "PersonalIDs should be unique in the Client file",
    Detail = paste("There are", dupe_count, "for PersonalID", PersonalID)
  ) %>%
  select(all_of(display_cols))

# Integrity Enrollment ----------------------------------------------------

duplicate_enrollment_id <- Enrollment %>%
  get_dupes(EnrollmentID) %>%
  mutate(
    Issue = "Duplicate EnrollmentIDs found in the Enrollment file",
    Type = "High Priority",
    Guidance = "EnrollmentIDs should be unique in the Enrollment file",
    Detail = str_squish(paste0(
      " There are ",
      dupe_count,
       " duplicates found for EnrollmentID",
      EnrollmentID,
      ". The EnrollmentID must be unique within the Enrollment.csv"))
  ) %>%
  select(all_of(display_cols))

personal_ids_in_client <- Client %>% pull(PersonalID)

foreign_key_no_primary_personalid_enrollment <- Enrollment %>%
  filter(!PersonalID %in% c(personal_ids_in_client)) %>%
  mutate(
    Issue = "Client in the Enrollment file not found in Client file",
    Type = "High Priority",
    Guidance = "All PersonalIDs in the Enrollment file should have a record in the Client file.",
    Detail = str_squish(paste(
      "PersonalID",
      PersonalID,
      "is in the Enrollment file but not in the Client file."
    ))
  ) %>%
  select(all_of(display_cols))

projectids_in_project <- Project %>% pull(ProjectID)

foreign_key_no_primary_projectid_enrollment <- Enrollment %>%
  filter(!ProjectID %in% c(projectids_in_project)) %>%
  mutate(
    Issue = "ProjectID in the Enrollment file not found in Project file",
    Type = "High Priority",
    Guidance = "All PerojectIDs in the Enrollment file should have a record in the Project file.",
    Detail = str_squish(paste(
      "ProjectID",
      ProjectID,
      "is in the Enrollment file but not in the Project file."
    ))
  ) %>%
  select(all_of(display_cols))

disabling_condition_invalid <- Enrollment %>%
  filter(!DisablingCondition %in% c(yes_no_enhanced)) %>%
  mutate(
    Issue = "Invalid Disabling Condition",
    Type = "Error",
    Guidance = "Disabling Condition should only have valid values",
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a Disabling Condition of",
      DisablingCondition,
      "which is an invalid value."
    ))
  ) %>%
  select(all_of(display_cols)) %>%
  unique()

allowed_living_situations <- 
  c(16, 1, 18, 15, 6, 7, 25, 4, 5, 29, 14, 2, 32, 13, 36, 12, 22, 35, 23, 26,
    27, 28, 19, 3, 31, 33, 34, 10, 20, 21, 11, 30, 17, 24, 37, 8, 9, 99)

living_situation_invalid <- Enrollment %>%
  filter(!is.na(LivingSituation) &
    (!LivingSituation %in% c(allowed_living_situations) |
       LivingSituation %in% c(12, 13, 22, 23, 26, 27, 30, 17, 24, 37))) %>%
  mutate(
    Issue = "Invalid Living Situation value",
    Type = "Error",
    Guidance = "LivingSituation may only contain valid values",
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a LivingSituation of",
      LivingSituation,
      "which is not a valid value."
    ))
  ) %>%
  select(all_of(display_cols)) 

rel_to_hoh_invalid <- Enrollment %>%
  filter(!RelationshipToHoH %in% c(1:5, 99) & !is.na(RelationshipToHoH)) %>%
  mutate(
    Issue = "Invalid RelationshipToHoH value",
    Type = "Error",
    Guidance = "RelationshipToHoH must be a valid value",
    Detail = str_squish(paste(
      "Enrollment ID",
      EnrollmentID,
      "has a RelationshipToHoH of",
      RelationshipToHoH,
      "which is invalid value."
    ))
  ) %>%
  select(all_of(display_cols)) %>%
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
#   select(all_of(display_cols)) %>%
#   
#   
#   unique()

# Integrity Living Situation ----------------------------------------------

nonstandard_destination <- Exit %>%
  filter(!Destination %in% c(allowed_living_situations) |
           Destination %in% c(35, 36, 37)) %>%
  mutate(
    Issue = "Invalid Destination value",
    Type = "Error",
    Guidance = "Destination values must be valid",
    Detail = str_squish(paste("EnrollmentID",
                     EnrollmentID,
                     "has a Destination value of",
                     Destination,
                     "which is not a valid Destination response."))) %>%
  select(all_of(display_cols))


nonstandard_CLS <- CurrentLivingSituation %>%
  filter(!is.na(CurrentLivingSituation) &
    (!CurrentLivingSituation %in% c(allowed_living_situations) |
       CurrentLivingSituation %in% c(12, 13, 22, 23, 26, 27, 30, 24))) %>%
  mutate(
    Issue = "Non-standard Current Living Situation",
    Type = "Error",
    Guidance = "This column contains a value that may have been retired from an old version of the Data Standards or was miskeyed. Please update the response to a current valid value.",
    Detail = str_squish(paste("EnrollmentID",
                     EnrollmentID,
                     "has a Current Living Situation value of",
                     CurrentLivingSituation,
                     "which is not a valid response."))) %>%
  select(all_of(display_cols))

integrity_main <- rbind(
  df_column_diffs,
  df_data_types,
  df_date_types,
  df_nulls,
  export_id_client,
  valid_values_client,
  duplicate_client_id,
  duplicate_enrollment_id,
  foreign_key_no_primary_personalid_enrollment,
  foreign_key_no_primary_projectid_enrollment,
  disabling_condition_invalid,
  living_situation_invalid,
  rel_to_hoh_invalid,
  nonstandard_destination,
  nonstandard_CLS
)

if(integrity_main %>% filter(Type == "High Priority") %>% nrow() > 0) {
  structural_issues <- 1
} else{
  structural_issues <- 0
}