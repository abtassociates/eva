Project <- Project %>%
  left_join(Organization %>%
              select(OrganizationID, OrganizationName),
            by = "OrganizationID") %>%
  mutate(ProjectType = case_when(
    ProjectType == 1 & TrackingMethod == 3 ~ 0,
    ProjectType == 1 ~ 1,
    TRUE ~ ProjectType
  ))

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
# only doing this for RRH and PSHs since Move In Date doesn't matter for ES, etc.

HHMoveIn <- Enrollment %>%
  filter(ProjectType %in% c(3, 9, 10, 13)) %>%
  mutate(
    AssumedMoveIn = if_else(
      EntryDate < hc_psh_started_collecting_move_in_date &
        ProjectType %in% c(3, 9, 10),
      1,
      0
    ),
    ValidMoveIn = case_when(
      AssumedMoveIn == 1 ~ EntryDate, # overwrites any MID where the Entry is 
      # prior to the date when PSH had to collect MID with the EntryDate (as
      # venders were instructed to do in the mapping documentation)
      AssumedMoveIn == 0 &
        ProjectType %in% c(psh_project_types) &
        EntryDate <= MoveInDate &
        ExitAdjust > MoveInDate ~ MoveInDate,
      # the Move-In Dates must fall between the Entry and ExitAdjust to be
      # considered valid and for PSH the hmid cannot = ExitDate
      MoveInDate <= ExitAdjust &
        MoveInDate >= EntryDate &
        ProjectType == 13 ~ MoveInDate
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

# HUD CSV Specs -----------------------------------------------------------

HUD_specs <- read_csv("public_data/HUDSpecs.csv",
                      col_types = "ccnc") %>%
  as.data.frame()
