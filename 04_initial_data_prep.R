###############################
# PURPOSE: 
# This script creates the intermediate dataframes that are also used in various places, including:
#     Enrollment, merged with other datasets and with added columns for faster access
#     Project, merged with organization and with project type redefined to account for tracking method
#     Services, limited to bed-nights only (record type = 200 and non-null date provided)
#     validation, combining the Enrollment and Project, limited to non-null EntryDates, and limited to key fields
#
# Some definitions:
# PH = PSH + RRH
# household = one or more people who present for housing/homeless services
# served = the Entry to Exit Date range crosses the Report Date range
# entered = the Entry Date is inside the Report Date range
# moved_in = any stay in a non-PH project where the Entry to Exit Date range
#     crosses the Report Date range PLUS any stay in a PH project where the 
#     Move In Date to the Exit Date crosses the Report Date range
# hohs = heads of household
# adults = all adults in a household
# clients = all members of the household
##############################

logToConsole("Running initial data prep")

# add Organization info into project dataset to more easily pull this info

ProjectStaging <- Project %>%
  left_join(Organization %>%
              select(OrganizationID, OrganizationName),
            by = "OrganizationID")

# breaking out Projects into their participating times, adjusting ProjectIDs

ProjectsInHMIS <- ProjectStaging %>%
  left_join(
    HMISParticipation %>%
      filter(HMISParticipationType == 1) %>%
      select(
        ProjectID,
        HMISParticipationStatusStartDate,
        HMISParticipationStatusEndDate
      ) %>%
      unique(),
    by = "ProjectID"
  ) %>% # ^ changes granularity if there are any quit-starters
  mutate(
    OperatingDateRange =
      interval(
        OperatingStartDate,
        replace_na(OperatingEndDate, meta_HUDCSV_Export_End)
      ),
    ParticipatingDateRange =
      interval(
        HMISParticipationStatusStartDate,
        replace_na(HMISParticipationStatusEndDate, meta_HUDCSV_Export_End)
      )
  )

quit_and_start_projects <- ProjectsInHMIS %>%
  get_dupes(ProjectID) %>% distinct(ProjectID)

if(nrow(quit_and_start_projects) > 0){
  QuitStarters <-  ProjectsInHMIS %>%
    filter(ProjectID %in% c(quit_and_start_projects)) %>%
    group_by(ProjectID) %>%
    arrange(OperatingStartDate) %>%
    mutate(ProjectTimeID = paste0(ProjectID, letters[row_number()])) %>%
    ungroup()
  
  ProjectsInHMIS <- ProjectsInHMIS %>%
    left_join(QuitStarters %>%
                select(ProjectID, ProjectTimeID, ParticipatingDateRange),
              by = c("ProjectID", "ParticipatingDateRange"))
}

# this version of Project is similar to the FY22 version of Project, except the
# ProjectTimeID is the granularity. 

if("ProjectTimeID" %in% colnames(ProjectsInHMIS)){
  Project <- ProjectsInHMIS %>%
  mutate(ProjectTimeID = coalesce(ProjectTimeID, ProjectID)) %>%
    relocate(ProjectTimeID, .after = ProjectID)
} else{
  Project <- ProjectsInHMIS %>%
    mutate(ProjectTimeID = ProjectID) %>%
    relocate(ProjectTimeID, .after = ProjectID)
}

# This dataset is used when we need an unduplicated concise df for project
Project0 <<- Project %>% 
  select(ProjectID,
         ProjectName,
         OrganizationID,
         OrganizationName,
         ProjectType,
         RRHSubType) %>%
  unique()

# Enrollment --------------------------------------------------------------

EnrollmentStaging <- Enrollment %>%
  left_join(Exit %>% 
              select(EnrollmentID, Destination, DestinationSubsidyType, ExitDate),
            by = "EnrollmentID") %>%
  mutate(ExitAdjust = coalesce(ExitDate, meta_HUDCSV_Export_End),
         EnrollmentDateRange = interval(EntryDate, ExitAdjust))

# Truncating Enrollments based on Operating/Participating -----------------

EnrollmentOutside <- EnrollmentStaging %>%
  left_join(Project %>%
              select(ProjectID,
                     ProjectTimeID,
                     ProjectType,
                     ParticipatingDateRange,
                     OperatingDateRange), by = "ProjectID",
            relationship = "many-to-many") %>%
  # many-to-many bc there will be ees that match to 2 rows of the same ProjectID
  # and this is expected at this point in the code bc we want to sus out which
  # project period the enrollment should be attached to. these extra ees will
  # be excluded later
  mutate(
    EnrollmentvParticipating = case_when(
        EnrollmentDateRange %within% ParticipatingDateRange ~
          "Inside",
        int_start(EnrollmentDateRange) > int_end(ParticipatingDateRange) ~
          "Enrollment After Participating Period",
        int_start(EnrollmentDateRange) < int_start(ParticipatingDateRange) &
          int_end(EnrollmentDateRange) > int_start(ParticipatingDateRange) ~
          "Enrollment Crosses Participating Start",
        int_end(EnrollmentDateRange) < int_start(ParticipatingDateRange) ~
          "Enrollment Before Participating Period",
        int_start(EnrollmentDateRange) > int_start(ParticipatingDateRange) &
          int_end(EnrollmentDateRange) > int_end(ParticipatingDateRange) ~ 
          "Enrollment Crosses Participating End",
        int_start(EnrollmentDateRange) < int_start(ParticipatingDateRange) &
          int_end(EnrollmentDateRange) > int_end(ParticipatingDateRange) ~
          "Enrollment Crosses Participation Period"),
    EnrollmentvOperating = case_when(
      EnrollmentDateRange %within% OperatingDateRange ~
        "Inside",
      int_start(EnrollmentDateRange) > int_end(OperatingDateRange) ~
        "Enrollment After Operating Period",
      int_start(EnrollmentDateRange) < int_start(OperatingDateRange) &
        int_end(EnrollmentDateRange) > int_start(OperatingDateRange) ~
        "Enrollment Crosses Operating Start",
      int_end(EnrollmentDateRange) < int_start(OperatingDateRange) ~
        "Enrollment Before Operating Period",
      int_start(EnrollmentDateRange) > int_start(OperatingDateRange) &
        int_end(EnrollmentDateRange) > int_end(OperatingDateRange) ~ 
        "Enrollment Crosses Operating End",
      int_start(EnrollmentDateRange) < int_start(OperatingDateRange) &
        int_end(EnrollmentDateRange) > int_end(OperatingDateRange) ~
        "Enrollment Crosses Operating Period")
  ) %>%
  group_by(ProjectID, EnrollmentID) %>%
  arrange(ProjectTimeID) %>%
  slice(1L) %>%
  ungroup() %>%
  select(EnrollmentID, ProjectID, ProjectTimeID, ProjectType, EnrollmentDateRange,
         OperatingDateRange, ParticipatingDateRange, EnrollmentvParticipating,
         EnrollmentvOperating)

Enrollment <- EnrollmentStaging %>%
  left_join(EnrollmentOutside,
            by = c("EnrollmentID", "ProjectID", "EnrollmentDateRange")) %>%
  mutate(
    RawEntryDate = EntryDate,
    EntryDate = if_else(
      EnrollmentvOperating %in% c("Enrollment Crosses Operating Start",
                                  "Enrollment Crosses Operating Period") |
        EnrollmentvParticipating %in% c("Enrollment Crosses Participating Start",
                                        "Enrollment Crosses Participating Period"),
      max(int_start(ParticipatingDateRange),
          int_start(OperatingDateRange), na.rm = TRUE),
      EntryDate
    ),
    RawExitAdjust = ExitAdjust,
    RawExitDate = ExitDate,
    ExitDate = if_else(
      EnrollmentvOperating %in% c("Enrollment Crosses Operating End",
                                  "Enrollment Crosses Operating Period") |
        EnrollmentvParticipating %in% c("Enrollment Crosses Participating End",
                                        "Enrollment Crosses Participating Period"),
      min(int_end(ParticipatingDateRange), int_end(OperatingDateRange), na.rm = TRUE),
      ExitDate
    ),
    ExitAdjust = replace_na(ExitDate, meta_HUDCSV_Export_End)
  ) %>%
  select(
    EnrollmentID,
    ProjectID,
    ProjectTimeID,
    ProjectType,
    RawEntryDate,
    EntryDate,
    RawExitAdjust,
    ExitAdjust,
    RawExitDate,
    ExitDate,
    Destination,
    DestinationSubsidyType,
    EnrollmentvOperating,
    EnrollmentvParticipating,
    OperatingDateRange,
    ParticipatingDateRange
  ) %>%
  right_join(Enrollment %>%
               select(-EntryDate),
             by = c("EnrollmentID", "ProjectID"))

# Only contains EEs within Operating and Participating Dates --------------

EnrollmentAdjust <- Enrollment %>%
  filter(
    !EnrollmentvParticipating %in% c(
      "Enrollment After Participating Period",
      "Enrollment Before Participating Period"
    ) &
      !EnrollmentvOperating %in% c("Enrollment After Operating Period",
                                   "Enrollment Before Operating Period")
  ) 

# getting HH information
# only doing this for PH projects since Move In Date doesn't matter for ES, etc.

HHMoveIn <- Enrollment %>%
  filter(ProjectType %in% ph_project_types) %>%
  mutate(
    AssumedMoveIn = if_else(
      EntryDate < hc_psh_started_collecting_move_in_date &
        ProjectType %in% psh_project_types,
      1,
      0
    ),
    ValidMoveIn = case_when(
      AssumedMoveIn == 1 ~ EntryDate, # overwrites any MID where the Entry is 
      # prior to the date when PSH had to collect MID with the EntryDate (as
      # vendors were instructed to do in the mapping documentation)
      AssumedMoveIn == 0 &
        ProjectType %in% psh_project_types &
        EntryDate <= MoveInDate &
        ExitAdjust > MoveInDate ~ MoveInDate,
      # the Move-In Dates must fall between the Entry and ExitAdjust to be
      # considered valid and for PSH the hmid cannot = ExitDate
      MoveInDate <= ExitAdjust &
        MoveInDate >= EntryDate &
        ProjectType == rrh_project_type ~ MoveInDate
    )
  ) %>%
  filter(!is.na(ValidMoveIn)) %>%
  group_by(HouseholdID) %>%
  summarise(HHMoveIn = min(ValidMoveIn, na.rm = TRUE)) %>%
  ungroup() %>%
  select(HouseholdID, HHMoveIn) %>%
  unique()

HHEntry <- Enrollment %>%
  group_by(HouseholdID) %>%
  mutate(FirstEntry = min(EntryDate)) %>%
  ungroup() %>%
  select(HouseholdID, "HHEntry" = FirstEntry) %>%
  unique() %>%
  left_join(HHMoveIn, by = "HouseholdID")

Enrollment <- Enrollment %>%
  left_join(HHEntry, by = "HouseholdID") %>%
  mutate(MoveInDateAdjust = if_else(
    !is.na(HHMoveIn) &
      ymd(HHMoveIn) <= ExitAdjust,
    if_else(EntryDate <= ymd(HHMoveIn),
            ymd(HHMoveIn), EntryDate),
    NA
  ))


# Adding Age at Entry to Enrollment
small_client <- Client %>% select(PersonalID, DOB)
Enrollment <- Enrollment %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
  select(-DOB)

# to be used for system data analysis purposes. has been culled of enrollments
# that fall outside of participation/operation date ranges.

EnrollmentAdjust <- EnrollmentAdjust %>%
  left_join(HHEntry, by = "HouseholdID") %>%
  mutate(MoveInDateAdjust = if_else(
    !is.na(HHMoveIn) &
      ymd(HHMoveIn) <= ExitAdjust,
    if_else(EntryDate <= ymd(HHMoveIn),
            ymd(HHMoveIn), EntryDate),
    NA
  )) %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
  select(-DOB)
  

rm(HHEntry, HHMoveIn, small_client)

# Only BedNight Services --------------------------------------------------

Services <- Services %>%
  filter(RecordType == 200 & !is.na(DateProvided))

# Build Validation df for app ---------------------------------------------

validationProject <- Project %>%
  select(
    ProjectID,
    ProjectTimeID,
    OrganizationName,
    OperatingDateRange,
    ParticipatingDateRange,
    ProjectName,
    ProjectType
  )

validationEnrollment <- Enrollment %>% 
  select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    ProjectTimeID,
    RelationshipToHoH,
    EntryDate,
    RawEntryDate,
    MoveInDate,
    ExitDate,
    RawExitDate,
    MoveInDateAdjust,
    ExitAdjust,
    RawExitAdjust,
    LivingSituation,
    Destination,
    DestinationSubsidyType,
    DateCreated
  ) 

# to be used for more literal, data-quality-based analyses. contains enrollments
# that do not intersect any period of HMIS participation or project operation

validation <- validationProject %>%
  left_join(validationEnrollment, by = c("ProjectTimeID", "ProjectID")) %>%
  select(
    ProjectID,
    ProjectTimeID,
    OrganizationName,
    ProjectName,
    ProjectType,
    EnrollmentID,
    PersonalID,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    LivingSituation,
    Destination,
    DestinationSubsidyType,
    DateCreated
  ) %>%
  filter(!is.na(EntryDate))

# desk_time_providers <- validation %>%
#   dplyr::filter(
#     (entered_between(., today() - years(1), today()) |
#        exited_between(., today() - years(1), today())) &
#       ProjectType %in% lh_ph_hp_project_types) %>%
#   dplyr::select(ProjectName) %>% unique()

