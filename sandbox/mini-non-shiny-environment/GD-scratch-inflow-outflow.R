

fy22_to_fy24_living_situation <- function(value){
  case_when(
    value %in% c(3, 19, 20, 28, 31, 33, 34, 38, 39) ~ 435,
    value %in% c(lh_livingsituation) ~ value + 100,
    value %in% c(4, 5, 6, 7, 15, 25) ~ value + 200,
    value %in% c(2, 12, 13, 14, 27, 29, 32, 35, 36) ~ value + 300,
    value %in% c(10, 11, 21, 22, 23, 26) ~ value + 400,
    value %in% c(8, 9, 17, 24, 30, 37, 99) ~ value,
    is.na(value) ~ NA,
    TRUE ~ 0 # 0 would mean something's wrong
  )
}

ees_with_statuses <- Enrollment %>%
  select(
    PersonalID,
    HouseholdID,
    EnrollmentID,
    ProjectType,
    RelationshipToHoH,
    LivingSituation,
    LOSUnderThreshold,
    PreviousStreetESSH,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    Destination
  ) %>%
  mutate(
    EntryStatusHomeless = if_else(
      ProjectType %in% c(lh_project_types) |
        (LivingSituation %in% c(homeless_livingsituation) & 
           ProjectType != 12) |
        (LivingSituation %in% c(not_homeless_livingsituation) & 
           ProjectType != 12 &
           LOSUnderThreshold == 1 &
           PreviousStreetESSH == 1), 1, 0
    ),
    HousedLeavers = if_else(
      EntryStatusHomeless == 1 &
        ProjectType %in% c(ph_project_types) &
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust >= meta_HUDCSV_Export_Start &
        MoveInDateAdjust <= meta_HUDCSV_Export_End &
        ProjectType != 12 &
        Destination %in% c(perm_livingsituation) &
        !is.na(ExitDate) &
        ExitDate >= meta_HUDCSV_Export_Start &
        ExitDate <= meta_HUDCSV_Export_End,
      1, 0
      ),
    HousedAll = if_else(
      EntryStatusHomeless == 1 &
        ProjectType %in% c(ph_project_types) &
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust >= meta_HUDCSV_Export_Start &
        MoveInDateAdjust <= meta_HUDCSV_Export_End &
        ProjectType != 12 &
        Destination %in% fy22_to_fy24_living_situation(c(perm_livingsituation)) &
        (is.na(ExitDate) |
           ExitDate >= meta_HUDCSV_Export_Start),
      1, 0
    ),
    
    FY24Translation = fy22_to_fy24_living_situation(LivingSituation)
      
  )
  
  