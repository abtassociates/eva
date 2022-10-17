


client_problems <- problems(Client)

files <- c(
  "Affiliation",
  "Assessment",
  "AssessmentQuestions",
  "AssessmentResults",
  "Client",
  "CurrentLivingSituation",
  "Disabilities",
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

column_names <- read_csv("public_data/columns.csv")


check_column_names <- function(file) {
  tibble(
    ImportedColumns = colnames(!!rlang::ensym(file)),
    CorrectColumns = column_names %>%
      filter(Files == {{file}}) %>%
      pull(Columns) %>% str_split(",") %>% unlist()
  ) %>%
    mutate(Guidance = if_else(
      ImportedColumns != CorrectColumns,
      paste(
        "The",
        ImportedColumns,
        "column should be spelled like",
        CorrectColumns
      ),
      "all good"
    )) %>%
    filter(Guidance != "all good") %>%
    mutate(Type = "High Priority",
           Issue = "Incorrect Column Name") %>%
    select(Issue, Type, Guidance)
  
}

column_names <- map_df(files, check_column_names)

# Affiliation -------------------------------------------------------------

# if (nrow(Affiliation) == 0) {
#   
# } else
# {
#   data_types_prep_affiliation <- Affiliation %>%
#     mutate(
#       AffiliationIDx = class(AffiliationID) == "character",
#       ProjectIDx = class(ProjectID) == "character",
#       ResProjectIDx = class(ResProjectID) == "character",
#       DateCreatedx = class(DateCreated)[1] == "POSIXct",
#       DateUpdatedx = class(DateUpdated)[1] == "POSIXct",
#       UserIDx = class(UserID) == "character",
#       DateDeletedx = class(DateDeleted)[1] == "POSIXct",
#       ExportIDx = class(ExportID) == "character"
#     ) %>%
#     select(ends_with("x")) %>%
#     head(1L) %>%
#     pivot_longer(cols = everything())
#   
#   if (min(data_types_prep_affiliation$value) == 0) {
#     data_types_affiliation <- data_types_prep_affiliation %>%
#       filter(value == FALSE) %>%
#       mutate(
#         Issue = "Incorrect data type",
#         Type = "High Priority",
#         name = str_trunc(name, nchar(name) - 1,
#                          side = "right",
#                          ellipsis = ""),
#         Guidance = paste("This file's", name,
#                          "column is not the correct data type")
#       ) %>%
#       select(Issue, Type, Guidance)
#   } else {
#     data_types_affiliation <- data.frame(Issue = character(),
#                                          Type = character(),
#                                          Guidance = character())
#   }
#   
#   eligible_for_affiliation <- Project %>%
#     filter(ProjectType %in% c(1, 2, 3, 8, 10, 13)) %>%
#     pull(ProjectID)
#   
#   possible_affiliations <- Project %>%
#     filter(ProjectType == 6) %>%
#     pull(ProjectID)
#   
#   project_not_sso <- Affiliation %>%
#     mutate(
#       Issue = if_else(
#         !ProjectID %in% c(eligible_for_affiliation),
#         "Affiliation Project Type is not Services Only",
#         "Nothing"
#       ),
#       Type = "Error",
#       Guidance = paste("ProjectID", ProjectID, "is not ProjectType 6.")
#     ) %>%
#     filter(Issue != "Nothing") %>%
#     select(Issue, Type, Guidance)
#   
#   affiliated_to_wrong_project_type <- Affiliation %>%
#     mutate(
#       Issue = if_else(
#         !ResProjectID %in% c(possible_affiliations),
#         "Affiliated to a non-residential Project",
#         "Nothing"
#       ),
#       Type = "Error",
#       Guidance = paste(
#         "ProjectID",
#         ProjectID,
#         "is affiliated with",
#         ResProjectID,
#         "which is not a residential project"
#       )
#     ) %>%
#     filter(Issue != "Nothing") %>%
#     select(Issue, Type, Guidance)
#   
#   export_id_affiliation <- Affiliation %>%
#     mutate(
#       Issue = if_else(
#         as.character(ExportID) != export_id_from_export,
#         "ExportID mismatch",
#         "Nothing"
#       ),
#       Type = "Error",
#       Guidance = paste(
#         "The Export file says the ExportID is",
#         export_id_from_export,
#         "but in your",
#         "Affiliation",
#         "file, it is",
#         ExportID
#       )
#     ) %>%
#     filter(Issue != "Nothing") %>%
#     select(Issue, Type, Guidance) %>%
#     unique()
# }
# 
# duplicate_affiliation_id <- Affiliation %>%
#   get_dupes(AffiliationID) %>%
#   mutate(
#     Issue = "Duplicate AffiliationIDs found in the Affiliation file",
#     Type = "High Priority",
#     Guidance = paste("There are", dupe_count, "for AffiliationID", AffiliationID)
#   ) %>%
#   select(Issue, Type, Guidance)
# 
# issues_affiliation <-
#   rbind(
#     column_names_affiliation,
#     affiliated_to_wrong_project_type,
#     data_types_affiliation,
#     project_not_sso,
#     export_id_affiliation,
#     duplicate_affiliation_id
#   )

# Client ------------------------------------------------------------------

column_names_client <- tibble(
  ImportedColumns = colnames(Client),
  CorrectColumns = column_names %>%
    filter(Files == "Client") %>%
    pull(Columns) %>% str_split(",") %>% unlist()
) %>%
  mutate(Guidance = if_else(
    ImportedColumns != CorrectColumns,
    paste(
      "The",
      ImportedColumns,
      "column should be spelled like",
      CorrectColumns
    ),
    "all good"
  )) %>%
  filter(Guidance != "all good") %>%
  mutate(Type = "High Priority",
         Issue = "Incorrect Column Name") %>%
  select(Issue, Type, Guidance)

date_data_types_client <- Client %>%
  mutate(CorrectDataType = !is.na(parse_date_time(DOB, orders = "ymd")))

data_types_prep_client <- Client %>%
  mutate(
    PersonalIDx = class(PersonalID) == "character",
    FirstNamex = class(FirstName) == "character",
    MiddleNamex = class(MiddleName) == "character",
    LastNamex = class(LastName) == "character",
    NameSuffixx = class(NameSuffix) == "character",
    NameDataQualityx = class(NameDataQuality) == "numeric",
    SSNx = class(SSN) == "character",
    SSNDataQualityx = class(SSNDataQuality) == "numeric",
    DOBx = class(DOB) == "Date",
    DOBDataQualityx = class(DOBDataQuality) == "numeric",
    AmIndAKNativex  = class(AmIndAKNative) == "numeric",
    Asianx = class(Asian) == "numeric",
    BlackAfAmericanx = class(BlackAfAmerican) == "numeric",
    NativeHIPacificx = class(NativeHIPacific) == "numeric",
    Whitex = class(White) == "numeric",
    RaceNonex = class(RaceNone) == "numeric",
    Ethnicityx = class(Ethnicity) == "numeric",
    Femalex = class(Female) == "numeric",
    Malex = class(Male) == "numeric",
    NoSingleGenderx = class(NoSingleGender) == "numeric",
    Transgenderx = class(Transgender) == "numeric",
    Questioningx = class(Questioning) == "numeric",
    GenderNonex = class(GenderNone) == "numeric",
    VeteranStatusx = class(VeteranStatus) == "numeric",
    YearEnteredServicex = class(YearEnteredService) == "numeric",
    YearSeparatedx = class(YearSeparated) == "numeric",
    WorldWarIIx = class(WorldWarII) == "numeric",
    KoreanWarx = class(KoreanWar) == "numeric",
    VietnamWarx = class(VietnamWar) == "numeric",
    DesertStormx = class(DesertStorm) == "numeric",
    AfghanistanOEFx = class(AfghanistanOEF) == "numeric",
    IraqOIFx = class(IraqOIF) == "numeric",
    IraqONDx = class(IraqOND) == "numeric",
    OtherTheaterx = class(OtherTheater) == "numeric",
    MilitaryBranchx = class(MilitaryBranch) == "numeric",
    DischargeStatusx = class(DischargeStatus) == "numeric",
    DateCreatedx = class(DateCreated)[1] == "POSIXct",
    DateUpdatedx = class(DateUpdated)[1] == "POSIXct",
    UserIDx = class(UserID) == "character",
    DateDeletedx = class(DateDeleted)[1] == "POSIXct",
    ExportIDx = class(ExportID) == "character"
  ) %>%
  select(ends_with("x"),-NameSuffix) %>%
  head(1L) %>%
  pivot_longer(cols = everything())

if (min(data_types_prep_client$value) == 0) {
  data_types_client <- data_types_prep_client %>%
    filter(value == FALSE) %>%
    mutate(
      Issue = "Incorrect data type",
      Type = "Error",
      name = str_trunc(name, nchar(name) - 1,
                       side = "right",
                       ellipsis = ""),
      Guidance = paste("This file's", name,
                       "column is not the correct data type")
    ) %>%
    select(Issue, Type, Guidance)
} else {
  data_types_client <- data.frame(Issue = character(),
                                  Type = character(),
                                  Guidance = character())
}

nulls_not_allowed_client <- Client %>%
  mutate(
    Nulls = across(everything(), ~ is.na(.x)),
    PersonalID = if_else(Nulls$PersonalID == TRUE, "Error", "Ok"),
    NameDataQuality = if_else(Nulls$NameDataQuality == TRUE, "Error", "Ok"),
    SSNDataQuality = if_else(Nulls$SSNDataQuality == TRUE, "Error", "Ok"),
    DOBDataQuality = if_else(Nulls$DOBDataQuality == TRUE, "Error", "Ok"),
    AmIndAKNative = if_else(Nulls$AmIndAKNative == TRUE, "Error", "Ok"),
    Asian = if_else(Nulls$Asian == TRUE, "Error", "Ok"),
    BlackAfAmerican = if_else(Nulls$BlackAfAmerican == TRUE, "Error", "Ok"),
    NativeHIPacific = if_else(Nulls$NativeHIPacific == TRUE, "Error", "Ok"),
    White = if_else(Nulls$White == TRUE, "Error", "Ok"),
    Ethnicity = if_else(Nulls$Ethnicity == TRUE, "Error", "Ok"),
    Female = if_else(Nulls$Female == TRUE, "Error", "Ok"),
    Male = if_else(Nulls$Male == TRUE, "Error", "Ok"),
    NoSingleGender = if_else(Nulls$NoSingleGender == TRUE, "Error", "Ok"),
    Transgender = if_else(Nulls$Transgender == TRUE, "Error", "Ok"),
    Questioning = if_else(Nulls$Questioning == TRUE, "Error", "Ok"),
    VeteranStatus = if_else(Nulls$VeteranStatus == TRUE, "Error", "Ok"),
    DateCreated = if_else(Nulls$DateCreated == TRUE, "Error", "Ok"),
    DateUpdated = if_else(Nulls$DateUpdated == TRUE, "Error", "Ok"),
    UserID = if_else(Nulls$UserID == TRUE, "Error", "Ok"),
    ExportID = if_else(Nulls$ExportID == TRUE, "Error", "Ok")
  ) %>%
  group_by(ExportID) %>%
  summarise(
    PersonalID = min(PersonalID),
    NameDataQuality = min(NameDataQuality),
    SSNDataQuality = min(SSNDataQuality),
    DOBDataQuality = min(DOBDataQuality),
    AmIndAKNative = min(AmIndAKNative),
    Asian = min(Asian),
    BlackAfAmerican = min(BlackAfAmerican),
    NativeHIPacific = min(NativeHIPacific),
    White = min(White),
    Ethnicity = min(Ethnicity),
    Female = min(Female),
    Male = min(Male),
    NoSingleGender = min(NoSingleGender),
    Transgender = min(Transgender),
    Questioning = min(Questioning),
    VeteranStatus = min(VeteranStatus),
    DateCreated = min(DateCreated),
    DateUpdated = min(DateUpdated),
    UserID = min(UserID)
  ) %>%
  pivot_longer(everything()) %>%
  filter(value == "Error") %>%
  mutate(
    Issue = "Nulls not allowed in this column",
    Type = "Error",
    Guidance = paste("Column", name, "has nulls in it where they are not allowed")
  ) %>%
  select(Issue, Type, Guidance)

if(!is.null(problems(Client))){
  date_formats_wrong_client_prep <-
    tibble(problems(Client)) %>%
    filter(expected == "date like ")}

if (exists("date_formats_wrong_client_prep")) {
  date_formats_wrong_client <- date_formats_wrong_client_prep %>%
    select(col) %>%
    unique() %>%
    mutate(
      Issue = "Wrong date format",
      Type = "High Priority",
      Guidance = paste("The", col, "column has an incorrect date format")
    ) %>%
    select(Issue, Type, Guidance)
}

export_id_client <- Client %>%
  mutate(
    Issue = if_else(
      as.character(ExportID) != export_id_from_export,
      "ExportID mismatch",
      "Nothing"
    ),
    Type = "Error",
    Guidance = paste(
      "The Export file says the ExportID is",
      export_id_from_export,
      "but in your Client file, it is",
      ExportID
    )
  ) %>%
  filter(Issue != "Nothing") %>%
  select(Issue, Type, Guidance) %>%
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
    Guidance = case_when(
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
  select(Issue, Type, Guidance)

duplicate_client_id <- Client %>%
  get_dupes(PersonalID) %>%
  mutate(
    Issue = "Duplicate PersonalIDs found in the Client file",
    Type = "High Priority",
    Guidance = paste("There are", dupe_count, "for PersonalID", PersonalID)
  ) %>%
  select(Issue, Type, Guidance)

issues_client <-
  rbind(
    column_names_client,
    data_types_client,
    nulls_not_allowed_client,
    date_formats_wrong_client,
    valid_values_client,
    export_id_client,
    duplicate_client_id
  )

# Enrollment --------------------------------------------------------------

duplicate_enrollment_id <- Enrollment %>%
  get_dupes(EnrollmentID) %>%
  mutate(
    Issue = "Duplicate EnrollmentIDs found in the Enrollment file",
    Type = "High Priority",
    Guidance = paste("There are", dupe_count, "for EnrollmentID", EnrollmentID)
  ) %>%
  select(Issue, Type, Guidance)

personal_ids_in_client <- Client %>% pull(PersonalID)

foreign_key_no_primary_personalid_enrollment <- Enrollment %>%
  filter(!PersonalID %in% c(personal_ids_in_client)) %>%
  mutate(
    Issue = "Client in the Enrollment file not found in Client file",
    Type = "High Priority",
    Guidance = paste(
      "PersonalID",
      PersonalID,
      "is in the Enrollment file but not in the Client file"
    )
  ) %>%
  select(Issue, Type, Guidance)

projectids_in_project <- Project %>% pull(ProjectID)

foreign_key_no_primary_projectid_enrollment <- Enrollment %>%
  filter(!ProjectID %in% c(projectids_in_project)) %>%
  mutate(
    Issue = "ProjectID in the Enrollment file not found in Project file",
    Type = "High Priority",
    Guidance = paste(
      "ProjectID",
      ProjectID,
      "is in the Enrollment file but not in the Project file"
    )
  ) %>%
  select(Issue, Type, Guidance)

disabling_condition_invalid <- Enrollment %>%
  mutate(
    Issue = if_else(
      !DisablingCondition %in% c(yes_no_enhanced),
      "Disabling Condition contains an invalid value",
      NULL
    ),
    Type = "Error",
    Guidance = paste(
      "EnrollmentID",
      EnrollmentID,
      "has an invalid value in the
                     DisablingCondition column"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(Issue, Type, Guidance) %>%
  unique()

all_living_situations <- c(
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 16, 18, 19, 20, 21, 25, 26, 28, 29,
  31, 32, 33, 34, 35, 36, 99)

living_situation_invalid <- Enrollment %>%
  mutate(
    Issue = if_else(
      LivingSituation %in% c(all_living_situations) |
        is.na(LivingSituation),
      "Nothing",
      "Living Situation contains an invalid value"
    ),
    Type = "Error",
    Guidance = paste(
      "EnrollmentID",
      EnrollmentID,
      "has an invalid value in the
                     LivingSituation column"
    )
  ) %>%
  filter(Issue != "Nothing") %>%
  select(Issue, Type, Guidance) %>%
  unique()

rel_to_hoh_invalid <- Enrollment %>%
  mutate(
    Issue = if_else(
      RelationshipToHoH %in% c(1:5, 99),
      "Nothing",
      "RelationshipToHoH contains an invalid value"
    ),
    Type = "Error",
    Guidance = paste(
      "EnrollmentID",
      EnrollmentID,
      "has an invalid value in the
                     RelationshipToHoH column"
    )
  ) %>%
  filter(Issue != "Nothing") %>%
  select(Issue, Type, Guidance) %>%
  unique()

move_in_date_invalid <- Enrollment %>%
  left_join(Exit %>% select(EnrollmentID, ExitDate), by = "EnrollmentID") %>%
  mutate(
    Issue = if_else(
      (
        ymd(MoveInDate) >= ymd(EntryDate) &
          ymd(MoveInDate) <= coalesce(ymd(ExitDate), today())
      ) |
        is.na(MoveInDate),
      "Nothing",
      "MoveInDate is not either null or between the Entry and Exit Dates"
    ),
    Type = "Error",
    Guidance = paste("EnrollmentID", EnrollmentID, "has an invalid MoveInDate")
  ) %>%
  filter(Issue != "Nothing") %>%
  select(Issue, Type, Guidance) %>%
  unique()

issues_enrollment <- 
  rbind(
    duplicate_enrollment_id,
    foreign_key_no_primary_personalid_enrollment,
    foreign_key_no_primary_projectid_enrollment,
    disabling_condition_invalid,
    living_situation_invalid,
    rel_to_hoh_invalid,
    move_in_date_invalid
  )

# Exit --------------------------------------------------------------------


