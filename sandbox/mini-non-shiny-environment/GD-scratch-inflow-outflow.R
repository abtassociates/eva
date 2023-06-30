


fy22_to_fy24_living_situation <- function(value){
  case_when(
    value %in% c(3, 19, 20, 28, 31, 33, 34, 38, 39) ~ 435,
    value %in% c(lh_livingsituation) ~ value + 100,
    value %in% c(4, 5, 6, 7, 15, 25) ~ value + 200,
    value %in% c(2, 12, 13, 14, 27, 29, 32, 35, 36) ~ value + 300,
    value %in% c(10, 11, 21, 22, 23, 26) ~ value + 400,
    value %in% c(8, 9, 17, 24, 30, 37, 99) ~ value,
    is.na(value) ~ NA,
    TRUE ~ 0
  )
}

ees_with_statuses <- Enrollment %>%
  select(
    PersonalID,
    HouseholdID,
    EnrollmentID,
    ProjectType,
    RelationshipToHoH,
    EntryDate,
    LivingSituation,
    LOSUnderThreshold,
    PreviousStreetESSH
  ) %>%
  mutate(
    LHAtEntry = if_else(
      ProjectType %in% c(lh_project_types) |
        (TRUE & TRUE), 1, 0
    ),
    FY24Translation = fy22_to_fy24_living_situation(LivingSituation)
      
  )
  
  