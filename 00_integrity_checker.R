


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

export_id_from_export <- Export %>% pull(ExportID)

cols_and_data_types <- read_csv("public_data/columns.csv", col_types = cols())

col_counts <- cols_and_data_types %>%
  group_by(File) %>%
  summarise(ColumnCount = n())

# Creating Functions ------------------------------------------------------

check_column_counts <- function(file) {
  tibble(
    ImportedColumnCount = 
      ncol(!!rlang::ensym(file)),
    CorrectColumnCount = 
      col_counts %>%
      filter(File == {{file}}) %>%
      pull(ColumnCount)
  ) %>%
    mutate(
      Issue = case_when(
        ImportedColumnCount != CorrectColumnCount ~ 
          "Wrong Number of Columns"
      ),
      Guidance = case_when(
        ImportedColumnCount != CorrectColumnCount ~ paste(
          "The",
          file,
          "file has", 
          ImportedColumnCount,
          "columns when it should have",
          CorrectColumnCount
        )
      )
      
    ) %>%
    filter(Guidance != "all good") %>%
    mutate(Type = "High Priority",
           Issue = "Incorrect Column Name") %>%
    select(Issue, Type, Guidance)
  
}

check_column_names <- function(file) {
  tibble(
    ImportedColumns = 
      colnames(!!rlang::ensym(file)),
    CorrectColumns = 
      cols_and_data_types %>%
      filter(File == {{file}}) %>%
      pull(Column)
  ) %>%
    mutate(
      Issue = case_when(
        ImportedColumns != CorrectColumns ~ "Missing or Misspelled Column Name"
      ),
      Guidance = case_when(
        ImportedColumns != CorrectColumns ~ paste(
          "The", ImportedColumns, "column should be spelled like", CorrectColumns
        )
      )
      
    ) %>%
    filter(Guidance != "all good") %>%
    mutate(Type = "High Priority",
           Issue = "Incorrect Column Name") %>%
    select(Issue, Type, Guidance)
  
}

check_data_types <- function(barefile, quotedfile) {
  if(nrow(barefile) > 0) {
    
    if(!is.null(problems(barefile))){
      x <-
        tibble(problems(barefile)) %>%
        filter(expected == "date like ") %>%
        mutate(
          File = quotedfile,
          Issue = "Incorrect Date Format",
          Type = "High Priority",
          Guidance = paste(
            "Please check that the", col, "column in the", File,
            "file has the correct date format. Dates in the HMIS CSV Export 
            should be in yyyy-mm-dd or yyyy-mm-dd hh:mm:ss format, in alignment
            with the HMIS CSV Format Specifications.")
        ) %>%
        select(Issue, Type, Guidance) %>% unique()
    }
    
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
      mutate(
        Issue = if_else(DataType != ImportedDataType, "Incorrect Data Type", NULL), 
        Guidance = if_else(
          DataType != ImportedDataType,
          paste(
            "In the",
            quotedfile,
            "file, the",
            Column,
            "column should have a data type of",
            DataType,
            "but in this file, it is",
            ImportedDataType
          ),
          NULL
        ),
        Type = if_else(DataTypeHighPriority == 1, "High Priority", "Error")) %>%
      filter(!is.na(Issue)) %>%
      select(Issue, Type, Guidance)
    
    rbind(x, y)
  }
  
}

check_for_bad_nulls <- function(barefile, quotedfile) {
  
  if (nrow(barefile) > 1){
    barefile %>%
      mutate(across(everything(), ~ is.na(.x))) %>% 
      summarise(across(everything(), max)) %>%
      pivot_longer(cols = everything(), 
                   names_to = "Column", 
                   values_to = "NullsPresent") %>%
      mutate(File = quotedfile) %>%
      left_join(cols_and_data_types %>%
                  select(File, Column, NullsAllowed), by = c("File", "Column")) %>%
      filter(NullsAllowed == 0 & NullsPresent == 1) %>%
      mutate(
        Issue = "Nulls not allowed in this column",
        Type = "Error",
        Guidance = paste(
          "The",
          Column,
          "column in the",
          File,
          "file contains nulls where they are not allowed."
        )
      ) %>%
      select(Issue, Type, Guidance)}
}


# Running functions on all files ------------------------------------------

df_column_names <- map_df(files, check_column_names)
df_column_counts <- map_df(files, check_column_counts)

df_data_types <- rbind(
  check_data_types(Assessment,"Assessment"),
  check_data_types(Client,"Client"),
  check_data_types(CurrentLivingSituation,"CurrentLivingSituation"),
  check_data_types(EmploymentEducation,"EmploymentEducation"),
  check_data_types(Enrollment,"Enrollment"),
  check_data_types(EnrollmentCoC,"EnrollmentCoC"),
  check_data_types(Event,"Event"),
  check_data_types(Exit,"Exit"),
  check_data_types(Export,"Export"),
  check_data_types(Funder,"Funder"),
  check_data_types(HealthAndDV,"HealthAndDV"),
  check_data_types(IncomeBenefits,"IncomeBenefits"),
  check_data_types(Inventory,"Inventory"),
  check_data_types(Organization,"Organization"),
  check_data_types(Project,"Project"),
  check_data_types(ProjectCoC,"ProjectCoC"),
  check_data_types(Services,"Services"),
  check_data_types(User,"User"),
  check_data_types(YouthEducationStatus,"YouthEducationStatus")
) 

df_nulls <- rbind(
  check_for_bad_nulls(Assessment,"Assessment"),
  check_for_bad_nulls(Client,"Client"),
  check_for_bad_nulls(CurrentLivingSituation,"CurrentLivingSituation"),
  check_for_bad_nulls(EmploymentEducation,"EmploymentEducation"),
  check_for_bad_nulls(Enrollment,"Enrollment"),
  check_for_bad_nulls(EnrollmentCoC,"EnrollmentCoC"),
  check_for_bad_nulls(Event,"Event"),
  check_for_bad_nulls(Exit,"Exit"),
  check_for_bad_nulls(Export,"Export"),
  check_for_bad_nulls(Funder,"Funder"),
  check_for_bad_nulls(HealthAndDV,"HealthAndDV"),
  check_for_bad_nulls(IncomeBenefits,"IncomeBenefits"),
  check_for_bad_nulls(Inventory,"Inventory"),
  check_for_bad_nulls(Organization,"Organization"),
  check_for_bad_nulls(Project,"Project"),
  check_for_bad_nulls(ProjectCoC,"ProjectCoC"),
  check_for_bad_nulls(Services,"Services"),
  check_for_bad_nulls(User,"User"),
  check_for_bad_nulls(YouthEducationStatus,"YouthEducationStatus")
) 

# # Valid Values ------------------------------------------------------------
# 
# export_id_client <- Client %>%
#   mutate(
#     Issue = if_else(
#       as.character(ExportID) != export_id_from_export,
#       "ExportID mismatch",
#       "Nothing"
#     ),
#     Type = "Error",
#     Guidance = paste(
#       "The Export file says the ExportID is",
#       export_id_from_export,
#       "but in your Client file, it is",
#       ExportID
#     )
#   ) %>%
#   filter(Issue != "Nothing") %>%
#   select(Issue, Type, Guidance) %>%
#   unique()
# 
# yes_no_enhanced <- c(0, 1, 8, 9, 99)
# yes_no <- c(0, 1, 99)
# 
# valid_values_client <- Client %>%
#   mutate(
#     VeteranStatus = VeteranStatus %in% c(yes_no_enhanced),
#     RaceNone = RaceNone %in% c(8, 9, 99) | is.na(RaceNone),
#     AmIndAKNative = AmIndAKNative %in% c(yes_no),
#     Asian = Asian %in% c(yes_no),
#     BlackAfAmerican = BlackAfAmerican %in% c(yes_no),
#     NativeHIPacific = NativeHIPacific %in% c(yes_no),
#     White = White %in% c(yes_no),
#     Ethnicity = Ethnicity %in% c(yes_no_enhanced),
#     Female = Female %in% c(yes_no),
#     Male = Male %in% c(yes_no),
#     NoSingleGender = NoSingleGender %in% c(yes_no),
#     Transgender = Transgender %in% c(yes_no),
#     Questioning = Questioning %in% c(yes_no),
#     GenderNone = GenderNone %in% c(8, 9, 99) | is.na(GenderNone)
#   ) %>%
#   group_by_all() %>%
#   summarise(
#     VeteranStatus = min(VeteranStatus, na.rm = FALSE),
#     RaceNone = min(RaceNone, na.rm = FALSE),
#     AmIndAKNative = min(AmIndAKNative, na.rm = FALSE),
#     Asian = min(Asian, na.rm = FALSE),
#     BlackAfAmerican = min(BlackAfAmerican, na.rm = FALSE),
#     NativeHIPacific = min(NativeHIPacific, na.rm = FALSE),
#     White = min(White, na.rm = FALSE),
#     Ethnicity = min(Ethnicity, na.rm = FALSE),
#     Female = min(Female, na.rm = FALSE),
#     Male = min(Male, na.rm = FALSE),
#     NoSingleGender = min(NoSingleGender, na.rm = FALSE),
#     Transgender = min(Transgender, na.rm = FALSE),
#     Questioning = min(Questioning, na.rm = FALSE),
#     GenderNone = min(GenderNone, na.rm = FALSE)
#   ) %>%
#   ungroup() %>%
#   select(
#     VeteranStatus,
#     RaceNone,
#     AmIndAKNative,
#     Asian,
#     BlackAfAmerican,
#     NativeHIPacific,
#     White,
#     Ethnicity,
#     Female,
#     Male,
#     NoSingleGender,
#     Transgender,
#     Questioning,
#     GenderNone
#   ) %>%
#   pivot_longer(cols = everything()) %>%
#   filter(value == 0) %>%
#   count(name) %>%
#   mutate(
#     Issue = "Invalid value in Client file",
#     Type = "Error",
#     Guidance = case_when(
#       name == "VeteranStatus" ~ paste("VeteranStatus has", n,
#                                       "rows with invalid values"),
#       name == "RaceNone" ~ paste("RaceNone has", n,
#                                  "rows with invalid values"),
#       name == "AmIndAKNative" ~ paste("AmIndAKNative has", n,
#                                       "rows with invalid values"),
#       name == "Asian" ~ paste("Asian has", n,
#                               "rows with invalid values"),
#       name == "BlackAfAmerican" ~ paste("BlackAfAmerican has", n,
#                                         "rows with invalid values"),
#       name == "NativeHIPacific" ~ paste("NativeHIPacific has", n,
#                                         "Rows with invalid values"),
#       name == "White" ~ paste("White has", n,
#                               "rows with invalid values"),
#       name == "Ethnicity" ~ paste("Ethnicity has", n,
#                                   "rows with invalid values"),
#       name == "Female" ~ paste("Female has", n,
#                                "rows with invalid values"),
#       name == "Male" ~ paste("Male has", n,
#                              "rows with invalid values"),
#       name == "NoSingleGender" ~ paste("NoSingleGender has", n,
#                                        "rows with invalid values"),
#       name == "Transgender" ~ paste("Transgender has", n,
#                                     "rows with invalid values"),
#       name == "Questioning" ~ paste("Questioning has", n,
#                                     "rows with invalid values"),
#       name == "GenderNone" ~ paste("GenderNone has", n,
#                                    "rows with invalid values")
#     )
#   ) %>%
#   select(Issue, Type, Guidance)
# 
# duplicate_client_id <- Client %>%
#   get_dupes(PersonalID) %>%
#   mutate(
#     Issue = "Duplicate PersonalIDs found in the Client file",
#     Type = "High Priority",
#     Guidance = paste("There are", dupe_count, "for PersonalID", PersonalID)
#   ) %>%
#   select(Issue, Type, Guidance)
# 
# issues_client <-
#   rbind(
#     column_names_client,
#     data_types_client,
#     nulls_not_allowed_client,
#     date_formats_wrong_client,
#     valid_values_client,
#     export_id_client,
#     duplicate_client_id
#   )
# 
# # Enrollment --------------------------------------------------------------
# 
# duplicate_enrollment_id <- Enrollment %>%
#   get_dupes(EnrollmentID) %>%
#   mutate(
#     Issue = "Duplicate EnrollmentIDs found in the Enrollment file",
#     Type = "High Priority",
#     Guidance = paste("There are", dupe_count, "for EnrollmentID", EnrollmentID)
#   ) %>%
#   select(Issue, Type, Guidance)
# 
# personal_ids_in_client <- Client %>% pull(PersonalID)
# 
# foreign_key_no_primary_personalid_enrollment <- Enrollment %>%
#   filter(!PersonalID %in% c(personal_ids_in_client)) %>%
#   mutate(
#     Issue = "Client in the Enrollment file not found in Client file",
#     Type = "High Priority",
#     Guidance = paste(
#       "PersonalID",
#       PersonalID,
#       "is in the Enrollment file but not in the Client file"
#     )
#   ) %>%
#   select(Issue, Type, Guidance)
# 
# projectids_in_project <- Project %>% pull(ProjectID)
# 
# foreign_key_no_primary_projectid_enrollment <- Enrollment %>%
#   filter(!ProjectID %in% c(projectids_in_project)) %>%
#   mutate(
#     Issue = "ProjectID in the Enrollment file not found in Project file",
#     Type = "High Priority",
#     Guidance = paste(
#       "ProjectID",
#       ProjectID,
#       "is in the Enrollment file but not in the Project file"
#     )
#   ) %>%
#   select(Issue, Type, Guidance)
# 
# disabling_condition_invalid <- Enrollment %>%
#   mutate(
#     Issue = if_else(
#       !DisablingCondition %in% c(yes_no_enhanced),
#       "Disabling Condition contains an invalid value",
#       NULL
#     ),
#     Type = "Error",
#     Guidance = paste(
#       "EnrollmentID",
#       EnrollmentID,
#       "has an invalid value in the
#                      DisablingCondition column"
#     )
#   ) %>%
#   filter(!is.na(Issue)) %>%
#   select(Issue, Type, Guidance) %>%
#   unique()
# 
# all_living_situations <- c(
#   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 16, 18, 19, 20, 21, 25, 26, 28, 29,
#   31, 32, 33, 34, 35, 36, 99)
# 
# living_situation_invalid <- Enrollment %>%
#   mutate(
#     Issue = if_else(
#       LivingSituation %in% c(all_living_situations) |
#         is.na(LivingSituation),
#       "Nothing",
#       "Living Situation contains an invalid value"
#     ),
#     Type = "Error",
#     Guidance = paste(
#       "EnrollmentID",
#       EnrollmentID,
#       "has an invalid value in the
#                      LivingSituation column"
#     )
#   ) %>%
#   filter(Issue != "Nothing") %>%
#   select(Issue, Type, Guidance) %>%
#   unique()
# 
# rel_to_hoh_invalid <- Enrollment %>%
#   mutate(
#     Issue = if_else(
#       RelationshipToHoH %in% c(1:5, 99),
#       "Nothing",
#       "RelationshipToHoH contains an invalid value"
#     ),
#     Type = "Error",
#     Guidance = paste(
#       "EnrollmentID",
#       EnrollmentID,
#       "has an invalid value in the
#                      RelationshipToHoH column"
#     )
#   ) %>%
#   filter(Issue != "Nothing") %>%
#   select(Issue, Type, Guidance) %>%
#   unique()
# 
# move_in_date_invalid <- Enrollment %>%
#   left_join(Exit %>% select(EnrollmentID, ExitDate), by = "EnrollmentID") %>%
#   mutate(
#     Issue = if_else(
#       (
#         ymd(MoveInDate) >= ymd(EntryDate) &
#           ymd(MoveInDate) <= coalesce(ymd(ExitDate), today())
#       ) |
#         is.na(MoveInDate),
#       "Nothing",
#       "MoveInDate is not between the Entry Date and Exit Date"
#     ),
#     Type = "Error",
#     Guidance = paste("EnrollmentID", EnrollmentID, "has an invalid MoveInDate")
#   ) %>%
#   filter(Issue != "Nothing") %>%
#   select(Issue, Type, Guidance) %>%
#   unique()
# 
# issues_enrollment <- 
#   rbind(
#     duplicate_enrollment_id,
#     foreign_key_no_primary_personalid_enrollment,
#     foreign_key_no_primary_projectid_enrollment,
#     disabling_condition_invalid,
#     living_situation_invalid,
#     rel_to_hoh_invalid,
#     move_in_date_invalid
#   )

# Exit --------------------------------------------------------------------


