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


# separate projectType = 1 into 1 and 0, based on TrackingMethod
# also add Organization info into project dataset to more easily pull this info

Project <- Project %>%
  left_join(Organization %>%
              select(OrganizationID, OrganizationName),
            by = "OrganizationID") %>%
  mutate(ProjectType = case_when(
    ProjectType == es_ee_project_type & TrackingMethod == 3 ~ 0,
    ProjectType == es_ee_project_type &
      (is.na(TrackingMethod) | TrackingMethod != 3) ~ 1, 
    TRUE ~ ProjectType
  ))

# This dataset is only used to populate the Client Counts header with the Project and Org names
Project0 <<- Project %>% 
  select(ProjectID, ProjectName, OrganizationID, OrganizationName, ProjectType)

small_project <- Project %>% select(ProjectID, ProjectType, ProjectName)

# Enrollment --------------------------------------------------------------

Enrollment <- Enrollment %>%
  left_join(Exit %>% 
              select(EnrollmentID, Destination, ExitDate, OtherDestination),
            by = "EnrollmentID") %>% 
  left_join(small_project, by = "ProjectID") %>%
  mutate(ExitAdjust = coalesce(ExitDate, as.Date(meta_HUDCSV_Export_Date)))

# Adding ProjectType to Enrollment bc we need EntryAdjust & MoveInAdjust

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
      # venders were instructed to do in the mapping documentation)
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
  mutate(
    MoveInDateAdjust = if_else(!is.na(HHMoveIn) &
                                 ymd(HHMoveIn) <= ExitAdjust,
                               if_else(EntryDate <= ymd(HHMoveIn),
                                       HHMoveIn, EntryDate),
                               NA_real_), 
    EntryAdjust = case_when(
      ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
      ProjectType %in% c(3, 9, 13) &
        !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
    )
  )

# Adding Age at Entry to Enrollment
small_client <- Client %>% select(PersonalID, DOB)
Enrollment <- Enrollment %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
  select(-DOB)

# Adding ClientLocation at Entry to Enrollment
small_location <- EnrollmentCoC %>%
  filter(DataCollectionStage == 1) %>%
  select(EnrollmentID, "ClientLocation" = CoCCode)  %>%
  mutate(EnrollmentID = as.character(EnrollmentID)) # in case a vendor's datatypes
# are incorrect

Enrollment <- Enrollment %>%
  left_join(small_location, by = "EnrollmentID")

rm(small_project, HHEntry, HHMoveIn, small_client, small_location)


# Only BedNight Services --------------------------------------------------

Services <- Services %>%
  filter(RecordType == 200 & !is.na(DateProvided))

# HUD CSV Specs -----------------------------------------------------------
# AS 3/21/23: Can we just store HUD_specs as a dataframe directly in our app, rather than importing it each time?
HUD_specs <- read_csv("public-resources/HUDSpecs.csv",
                      col_types = "ccnc") %>%
  as.data.frame()

# Build Validation df for app ---------------------------------------------

validationProject <- Project %>%
  select(ProjectID,
         OrganizationName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectCommonName,
         ProjectName,
         ProjectType,
         HMISParticipatingProject) %>%
  filter(HMISParticipatingProject == 1 &
           operating_between(., 
                             ymd(meta_HUDCSV_Export_Start), 
                             ymd(meta_HUDCSV_Export_End))) 

validationEnrollment <- Enrollment %>% 
  select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    RelationshipToHoH,
    EntryDate,
    MoveInDate,
    ExitDate,
    EntryAdjust,
    MoveInDateAdjust,
    ExitAdjust,
    LivingSituation,
    Destination,
    DateCreated
  ) 

validation <- validationProject %>%
  left_join(validationEnrollment, by = "ProjectID") %>%
  select(
    ProjectID,
    ProjectName,
    ProjectType,
    EnrollmentID,
    PersonalID,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    EntryAdjust,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    LivingSituation,
    Destination,
    DateCreated,
    OrganizationName
  ) %>%
  filter(!is.na(EntryDate))

desk_time_providers <- validation %>%
  dplyr::filter(
    (entered_between(., today() - years(1), today()) |
       exited_between(., today() - years(1), today())) &
      ProjectType %in% lh_ph_hp_project_types) %>%
  dplyr::select(ProjectName) %>% unique()
