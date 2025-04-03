# Purpose: ----------------------------------------------------------------

# This script creates the intermediate dataframes that are also used in various
# places, including:
# Enrollment, merged with other datasets and with added columns for faster access
# Project, merged with organization and with project type redefined to account
# for tracking method
# Services, limited to bed-nights only (record type = 200 and non-null date
# provided)
# validation, combining the Enrollment and Project, limited to non-null
# EntryDates, and limited to key fields
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

logToConsole("Running initial data prep")

# Project data ------------------------------------------------------------

# breaking out Projects into their participating times, adjusting ProjectIDs

project_prep <- Project %>%
  left_join(
    Organization %>%
      select(OrganizationID, OrganizationName, VictimServiceProvider),
    by = "OrganizationID"
  )

ProjectSegments <- project_prep %>%
  left_join(
    HMISParticipation %>%
      select(
        ProjectID,
        HMISParticipationType,
        HMISParticipationStatusStartDate,
        HMISParticipationStatusEndDate
      ) %>%
      unique(),
    by = "ProjectID"
  ) %>% # ^ changes granularity *if* there are any participation changers
  mutate(
    HMISParticipationStatusEndDate = coalesce(HMISParticipationStatusEndDate, no_end_date),
    OperatingDateRange =
      interval(
        OperatingStartDate,
        coalesce(OperatingEndDate, no_end_date)
      ),
    ParticipatingDateRange =
      interval(
        HMISParticipationStatusStartDate,
        coalesce(HMISParticipationStatusEndDate, no_end_date)
      )
  ) %>%
  group_by(ProjectID) %>%
  arrange(OperatingStartDate, .by_group = TRUE) %>%
  mutate(
    ProjectTimeID = case_when(
      n() > 1 ~ paste0(ProjectID, letters[row_number()]),
      TRUE ~ ProjectID)
    ) %>%
  ungroup() %>%
  relocate(ProjectTimeID, .after = ProjectID)

# * Use Project0 for most things.
# * Use ProjectSegments if your analysis uses specific participation data
# * Use Project if you need something from the original data as it came in that's
#     not in Project0 or ProjectSegments

Project0(project_prep %>%
  select(ProjectID,
         ProjectName,
         OrganizationID,
         OrganizationName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectType,
         RRHSubType,
         VictimServiceProvider) %>%
  unique()
)

rm(project_prep)

# Enrollment --------------------------------------------------------------
# Truncating Enrollments based on Operating/Participating -----------------

EnrollmentStaging <- Enrollment %>%
  left_join(Client %>% select(PersonalID, DOB),
            by = "PersonalID")%>%
  left_join(Exit %>%
              select(EnrollmentID, Destination, DestinationSubsidyType, ExitDate),
            by = "EnrollmentID") %>%
  mutate(ExitAdjust = coalesce(ExitDate, no_end_date),
         EnrollmentDateRange = interval(EntryDate, ExitAdjust),
         AgeAtEntry = age_years(DOB, EntryDate),
         DOB = NULL) %>%
  group_by(ProjectID, HouseholdID) %>%
  mutate(
    max_AgeAtEntry = max(AgeAtEntry),
    min_AgeAtEntry = min(AgeAtEntry)
  ) %>%
  mutate(
    HouseholdType = factor(
      ifelse(
        any(between(AgeAtEntry, 0, 17)) & max_AgeAtEntry >= 18,
        ifelse(
          between(max_AgeAtEntry, 0, 24),
          "PY",
          "ACminusPY"
        ),
        ifelse(
          min_AgeAtEntry >= 18,
          ifelse(
            between(max_AgeAtEntry, 0, 24),
            "UY", # UY = Unaccompanied Youth. YYA = PY + UY + CO
            "AOminusUY"
          ),
          ifelse(
            min_AgeAtEntry >= 0 & max_AgeAtEntry <= 17,
            "CO", 
            "UN"
          )
        )
      ),
      levels = c("AOminusUY", "ACminusPY", "CO", "UN", "PY", "UY")
    )
  ) %>%
  ungroup()

# Truncating Enrollments based on Operating/Participating -----------------
# Perform the join
EnrollmentOutside <- setDT(EnrollmentStaging %>%
  left_join(ProjectSegments %>%
              select(ProjectID,
                     ProjectTimeID,
                     ProjectType,
                     HMISParticipationStatusStartDate,
                     HMISParticipationStatusEndDate,
                     # ParticipatingDateRange,
                     OperatingStartDate,
                     OperatingEndDate,
                     # OperatingDateRange
                     ), by = "ProjectID",
            relationship = "many-to-many")) # %>%
# AS 5/5/24: commenting out for now because this merge doesn't work correctly with intervals
# Submitted GitHub issue for lubridate: https://github.com/tidyverse/lubridate/issues/1165
# reprex: 
# x <- data.table(id = c(1,2), interval = c(lubridate::interval(Sys.Date(), Sys.Date() + 1), NA))
# y <- data.table(id = c(1,1,2))
# z <- x[y, on = .(id)]
# print(z)

# Project_dt <- as.data.table(Project)[, .(ProjectID,
#                                          ProjectTimeID,
#                                          ProjectType,
#                                          ParticipatingDateRange,
#                                          OperatingDateRange)]
# 
# EnrollmentOutside <- Project_dt[as.data.table(EnrollmentStaging), on = .(ProjectID)]

# many-to-many bc there will be ees that match to 2 rows of the same ProjectID
# and this is expected at this point in the code bc we want to sus out which
# project period the enrollment should be attached to. these extra ees will
# be excluded later
# mutate(
  # EnrollmentvParticipating = case_when(
Enrollmentvs <- function(EntryDate, ExitAdjust, ComparisonStart, ComparisonEnd, comparisonWord) {
  fcase(
    (EntryDate >= ComparisonStart & ExitAdjust <= ComparisonEnd) |
    (EntryDate >= ComparisonStart & ComparisonEnd > Sys.Date()),
      "Inside",
    EntryDate > ComparisonEnd,
      paste0("Enrollment After ", comparisonWord," Period"),
    ExitAdjust < ComparisonStart,
      paste0("Enrollment Before ", comparisonWord, " Period"),
    EntryDate > ComparisonStart & ExitAdjust > ComparisonEnd,
      paste0("Enrollment Crosses ", comparisonWord, " End"),
    EntryDate < ComparisonStart & ExitAdjust > ComparisonEnd,
      paste0("Enrollment Crosses ", comparisonWord, " Period"),
    EntryDate < ComparisonStart & ExitAdjust > ComparisonStart,
      paste0("Enrollment Crosses ", comparisonWord, " Start")
  )
}

EnrollmentOutside[, `:=`(
  EnrollmentvParticipating = Enrollmentvs(
    EntryDate, ExitAdjust, 
    HMISParticipationStatusStartDate, HMISParticipationStatusEndDate, 
    "Participating"
    ),
  EnrollmentvOperating = Enrollmentvs(
    EntryDate, ExitAdjust, 
    OperatingStartDate, OperatingEndDate, 
    "Operating")
  )]

#   group_by(ProjectID, EnrollmentID) %>%
#   arrange(ProjectTimeID) %>%
#   slice(1L) %>%
# ungroup() %>%
EnrollmentOutside <- EnrollmentOutside[order(ProjectTimeID), head(.SD, 1), by = .(ProjectID, EnrollmentID)]

# select(EnrollmentID, ProjectID, ProjectTimeID, ProjectType, EnrollmentDateRange,
#        OperatingDateRange, ParticipatingDateRange, EnrollmentvParticipating,
#        EnrollmentvOperating)
EnrollmentOutside <- EnrollmentOutside[, .(EnrollmentID, ProjectID, ProjectTimeID, ProjectType, 
                                           EntryDate, ExitAdjust,
                                           OperatingStartDate, OperatingEndDate, 
                                           HMISParticipationStatusStartDate,
                                           HMISParticipationStatusEndDate,
                                           EnrollmentvParticipating,
                                           EnrollmentvOperating)]

Enrollment <- EnrollmentStaging %>%
  left_join(setDF(EnrollmentOutside),
            by = c("EnrollmentID", "ProjectID", "EntryDate", "ExitAdjust")) %>%
  mutate(
    ParticipatingDateRange = interval(HMISParticipationStatusStartDate, HMISParticipationStatusEndDate),
    OperatingDateRange = interval(OperatingStartDate, OperatingEndDate),
    EntryDateTruncated = if_else(
      EnrollmentvOperating %in% c("Enrollment Crosses Operating Start",
                                  "Enrollment Crosses Operating Period") |
        EnrollmentvParticipating %in% c("Enrollment Crosses Participating Start",
                                        "Enrollment Crosses Participating Period"),
      max(
        int_start(ParticipatingDateRange), int_start(OperatingDateRange), na.rm = TRUE
      ),
      EntryDate
    ), # truncates to the earliest Participating/Operating End Date
    ExitDateTruncated = if_else(
      EnrollmentvOperating %in% c("Enrollment Crosses Operating End",
                                  "Enrollment Crosses Operating Period") |
        EnrollmentvParticipating %in% c("Enrollment Crosses Participating End",
                                        "Enrollment Crosses Participating Period"),
      min(int_end(ParticipatingDateRange), int_end(OperatingDateRange), na.rm = TRUE),
      ExitDate
    ),
    InsideOrNot = NULL
  ) %>%
  relocate(Destination:ExitDateTruncated, .before = RelationshipToHoH)

# Move In Dates -----------------------------------------------------------

# granularity: HouseholdIDs with ValidMoveIns

# HHMoveIn <- Enrollment %>%
#   filter(ProjectType %in% ph_project_types) %>%
HHMoveIn <- as.data.table(Enrollment)[ProjectType %in% ph_project_types]
  
# Add the AssumedMoveIn and ValidMoveIn columns
  # mutate(
  #   AssumedMoveIn = if_else(
  #     EntryDate < hc_psh_started_collecting_move_in_date &
  #       ProjectType %in% psh_project_types,
  #     1,
  #     0
  #   ),
  #   ValidMoveIn = case_when(
  #     AssumedMoveIn == 1 ~ EntryDate, # overwrites any MID where the Entry is 
  #     # prior to the date when PSH had to collect MID with the EntryDate (as
  #     # vendors were instructed to do in the mapping documentation)
  #     AssumedMoveIn == 0 &
  #       ProjectType %in% psh_project_types &
  #       EntryDate <= MoveInDate &
  #       ExitAdjust > MoveInDate ~ MoveInDate,
  #     # the Move-In Dates must fall between the Entry and ExitAdjust to be
  #     # considered valid and for PSH the hmid cannot = ExitDate
  #     MoveInDate <= ExitAdjust &
  #       MoveInDate >= EntryDate &
  #       ProjectType == rrh_project_type ~ MoveInDate
  #   )
  # ) %>%
HHMoveIn <- HHMoveIn[, `:=`(
  AssumedMoveIn = ifelse(
    EntryDate < hc_psh_started_collecting_move_in_date & ProjectType %in% psh_project_types,
    1,
    0
  )
)]
HHMoveIn <- HHMoveIn[, `:=`(
  ValidMoveIn = fcase(
    AssumedMoveIn == 1,
    EntryDate,
    
    AssumedMoveIn == 0 & ProjectType %in% psh_project_types & EntryDate <= MoveInDate & ExitAdjust > MoveInDate,
    MoveInDate,
    
    MoveInDate <= ExitAdjust & MoveInDate >= EntryDate & ProjectType == rrh_project_type,
    MoveInDate
  )
)]

# filter(!is.na(ValidMoveIn)) %>%
HHMoveIn <- HHMoveIn[!is.na(ValidMoveIn)]
  
# Group by HouseholdID and calculate HHMoveIn
# group_by(HouseholdID) %>%
# summarise(HHMoveIn = min(ValidMoveIn, na.rm = TRUE)) %>%
# ungroup() %>%
HHMoveIn <- HHMoveIn[, .(HHMoveIn = min(ValidMoveIn, na.rm = TRUE)), by = HouseholdID]
  
# Select the columns and remove duplicates
# select(HouseholdID, HHMoveIn) %>%
# unique()
HHMoveIn <- unique(HHMoveIn, by = c("HouseholdID", "HHMoveIn"))

# Group by HouseholdID and calculate FirstEntry
# HHEntry <- Enrollment %>%
  # group_by(HouseholdID) %>%
  # mutate(FirstEntry = min(EntryDate)) %>%
  # ungroup() %>%
HHEntry <- as.data.table(Enrollment)[, .(HHEntry = min(EntryDate)), by = HouseholdID]

  # select(HouseholdID, "HHEntry" = FirstEntry) %>%
  # unique() %>%
HHEntry <- unique(HHEntry[, c(HouseholdID, HHEntry)])
  
  # left_join(HHMoveIn, by = "HouseholdID")
HHEntry <- HHMoveIn[HHEntry, on = .(HouseholdID)]

setDF(HHEntry)

Enrollment <- Enrollment %>%
  left_join(HHEntry, by = "HouseholdID") %>%
  mutate(
    MoveInDateAdjust = case_when(
      EntryDate < hc_psh_started_collecting_move_in_date &
        MoveInDate != EntryDate &
        ProjectType %in% psh_project_types ~ EntryDate,!is.na(HHMoveIn) &
        ymd(HHMoveIn) <= ExitAdjust ~ MoveInDate
    )
  )

# Only contains EEs within Operating and Participating Dates --------------
# to be used for system data analysis purposes. has been culled of enrollments
# that fall outside of participation/operation date ranges.

EnrollmentAdjust <- Enrollment %>%
  filter(
    !EnrollmentvParticipating %in% c(
      "Enrollment After Participating Period",
      "Enrollment Before Participating Period"
    ) &
      !EnrollmentvOperating %in% c(
        "Enrollment After Operating Period",
        "Enrollment Before Operating Period"
      )
  )

rm(HHMoveIn)

# Only BedNight Services --------------------------------------------------

Services <- Services %>%
  filter(RecordType == 200 & !is.na(DateProvided))

# Build validation() df for app ---------------------------------------------

validationProject <- ProjectSegments %>%
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
    EntryDateTruncated,
    MoveInDate,
    ExitDate,
    ExitDateTruncated,
    MoveInDateAdjust,
    ExitAdjust,
    LivingSituation,
    Destination,
    DestinationSubsidyType,
    DateCreated
  ) 

# to be used for more literal, data-quality-based analyses. contains enrollments
# that do not intersect any period of HMIS participation or project operation
validation(
  validationProject %>%
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
)

# Checking requirements by projectid --------------------------------------

projects_funders_types <- Funder %>%
  left_join(Project %>%
              select(ProjectID, ProjectType),
            join_by(ProjectID)) %>%
  filter(is.na(EndDate) | EndDate > meta_HUDCSV_Export_Start()) %>%
  select(ProjectID, ProjectType, Funder) %>%
  unique() %>%
  left_join(inc_ncb_hi_required, join_by(ProjectType, Funder)) %>%
  mutate(inc = replace_na(inc, FALSE),
         ncb = replace_na(ncb, FALSE),
         hi = replace_na(hi, FALSE),
         dv = replace_na(dv, FALSE)) %>%
  group_by(ProjectID) %>%
  summarise(inc = max(inc, na.rm = TRUE),
            ncb = max(ncb, na.rm = TRUE),
            hi = max(hi, na.rm = TRUE),
            dv = max(dv, na.rm = TRUE)) %>%
  ungroup()

# Active Inventory -------------------------------------------------------------
activeInventory <- Inventory %>%
  left_join(
    Project0() %>%
      select(
        ProjectID,
        OrganizationName,
        ProjectName,
        OperatingStartDate,
        OperatingEndDate
      ) %>%
      unique(),
    by = "ProjectID"
  ) %>%
  filter(
    coalesce(InventoryEndDate, no_end_date) >= meta_HUDCSV_Export_Start() &
      InventoryStartDate <= meta_HUDCSV_Export_End()
  )

# HMIS-participating projects with active Inventory during report period -------
# HMIS-Participating projects that have inventory are almost necessarilly residential.
# Store this "generous" span so we can check if any enrollments fall within it

set_collapse(na.rm = TRUE, verbose = FALSE)
HMIS_participating_projects_w_active_inv_no_overflow <- qDT(ProjectSegments) %>%
  # HMiS-participating projects
  fsubset(HMISParticipationType == 1, 
          ProjectID, 
          ProjectTimeID, 
          ProjectType,
          HMISParticipationStatusStartDate, 
          HMISParticipationStatusEndDate,
          OperatingStartDate,
          OperatingEndDate
  ) %>%
  join(
    activeInventory %>% 
      fselect(ProjectID, InventoryStartDate, InventoryEndDate, Availability, BedInventory) %>%
      funique(),
    on = "ProjectID",
    how = "inner",
    multiple = TRUE
  ) %>%
  # Get the Start+End dates for when each Project was Operating, HMIS Participating, and Active (Inventory)
  fmutate(
    ProjectHMISParticipationStart = pmax(
      HMISParticipationStatusStartDate, 
      OperatingStartDate
    ),
    ProjectHMISParticipationEnd = pmin(
      HMISParticipationStatusEndDate,
      OperatingEndDate,
      na.rm = TRUE
    ),
    ProjectHMISActiveParticipationStart = pmax(
      ProjectHMISParticipationStart,
      InventoryStartDate
    ),
    ProjectHMISActiveParticipationEnd = pmin(
      ProjectHMISParticipationEnd,
      InventoryEndDate,
      na.rm = TRUE
    )
  )

# desk_time_providers <- validation() %>%
#   dplyr::filter(
#     (entered_between(., today() - years(1), today()) |
#        exited_between(., today() - years(1), today())) &
#       ProjectType %in% lh_ph_hp_project_types) %>%
#   dplyr::select(ProjectName) %>% unique()

CurrentLivingSituation(CurrentLivingSituation)
Event(Event)
