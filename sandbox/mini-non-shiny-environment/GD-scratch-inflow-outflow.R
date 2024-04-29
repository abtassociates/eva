

ees_with_statuses <- system_df %>%
  filter(ProjectType )
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
           PreviousStreetESSH == 1),
      1,
      0
    ),
    HousedLeavers = if_else(
      EntryStatusHomeless == 1 &
        ProjectType %in% c(ph_project_types) &
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust >= meta_HUDCSV_Export_Start() &
        MoveInDateAdjust <= meta_HUDCSV_Export_End() &
        ProjectType != 12 &
        Destination %in% c(perm_livingsituation) &
        !is.na(ExitDate) &
<<<<<<< HEAD
        ExitDate >= meta_HUDCSV_Export_Start() &
        ExitDate <= meta_HUDCSV_Export_End(),
      1, 0
      ),
=======
        ExitDate >= meta_HUDCSV_Export_Start &
        ExitDate <= meta_HUDCSV_Export_End,
      1,
      0
    ),
>>>>>>> math-fixes-g
    HousedAll = if_else(
      EntryStatusHomeless == 1 &
        ProjectType %in% c(ph_project_types) &
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust >= meta_HUDCSV_Export_Start() &
        MoveInDateAdjust <= meta_HUDCSV_Export_End() &
        ProjectType != 12 &
        Destination %in% c(perm_livingsituation) &
        (is.na(ExitDate) |
<<<<<<< HEAD
           ExitDate >= meta_HUDCSV_Export_Start()),
      1, 0
    ),
    
    FY24Translation = fy22_to_fy24_living_situation(LivingSituation)
      
=======
           ExitDate >= meta_HUDCSV_Export_Start),
      1,
      0
    )
>>>>>>> math-fixes-g
  )
  
  
