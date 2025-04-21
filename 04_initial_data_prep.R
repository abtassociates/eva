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

logToConsole(session, "Running initial data prep")

# Project data ------------------------------------------------------------

# breaking out Projects into their participating times, adjusting ProjectIDs

project_prep <- Project %>%
  join(
    Organization %>%
      select(OrganizationID, OrganizationName, VictimServiceProvider),
    how = "left",
    on = "OrganizationID"
  )

ProjectSegments <- project_prep %>%
  join(
    HMISParticipation %>%
      select(
        ProjectID,
        HMISParticipationType,
        HMISParticipationStatusStartDate,
        HMISParticipationStatusEndDate
      ) %>%
      unique(),
    how = "left",
    multiple = T,
    on = "ProjectID"
  ) %>% # ^ changes granularity *if* there are any participation changers
  roworder(ProjectID, OperatingStartDate) %>%
  fgroup_by(ProjectID) %>%
  fmutate(
    ProjectTimeID = fifelse(
      GRPN() > 1,
      paste0(ProjectID, letters[rowid(ProjectID)]),
      ProjectID
    )
  ) %>%
  fungroup() %>%
  relocate(ProjectTimeID, .after = ProjectID)

# * Use Project0 for most things.
# * Use ProjectSegments if your analysis uses specific participation data
# * Use Project if you need something from the original data as it came in that's
#     not in Project0 or ProjectSegments

session$userData$Project0 <- project_prep %>%
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

rm(project_prep)

# Enrollment --------------------------------------------------------------
# Truncating Enrollments based on Operating/Participating -----------------
EnrollmentStaging <- Enrollment %>%
  join(Client %>% fselect(PersonalID, DOB), 
       on = "PersonalID") %>%
  join(Exit %>% fselect(EnrollmentID, Destination, DestinationSubsidyType, ExitDate),
       on = "EnrollmentID") %>%
  fmutate(ExitAdjust = fcoalesce(ExitDate, no_end_date),
         AgeAtEntry = age_years(DOB, EntryDate),
         DOB = NULL) %>%
  fgroup_by(ProjectID, HouseholdID) %>%
  fmutate(
    max_AgeAtEntry = fmax(AgeAtEntry),
    min_AgeAtEntry = fmin(AgeAtEntry),
    HouseholdType = factor(
      fifelse(
        any(between(AgeAtEntry, 0, 17)) & max_AgeAtEntry >= 18,
        fifelse(
          between(max_AgeAtEntry, 0, 24),
          "PY",
          "ACminusPY"
        ),
        fifelse(
          min_AgeAtEntry >= 18,
          fifelse(
            between(max_AgeAtEntry, 0, 24),
            "UY", # UY = Unaccompanied Youth. YYA = PY + UY + CO
            "AOminusUY"
          ),
          fifelse(
            min_AgeAtEntry >= 0 & max_AgeAtEntry <= 17,
            "CO", 
            "UN"
          )
        )
      ),
      levels = c("AOminusUY", "ACminusPY", "CO", "UN", "PY", "UY")
    )
  ) %>%
  fungroup()

# Truncating Enrollments based on Operating/Participating -----------------
# This also brings in Project-level info, e.g. ProjectType to the Enrollment dataset
EnrollmentOutside <- qDT(EnrollmentStaging) %>%
  fselect(EnrollmentID, ProjectID, EntryDate, ExitAdjust) %>%
  join(ProjectSegments %>%
              select(ProjectID,
                     ProjectTimeID,
                     ProjectType,
                     HMISParticipationStatusStartDate,
                     HMISParticipationStatusEndDate,
                     OperatingStartDate,
                     OperatingEndDate
                     ),
       on = "ProjectID",
       how = "left",
       multiple = TRUE) # many-to-many bc there will be ees that match to 2 rows of the same ProjectID
# and this is expected at this point in the code bc we want to sus out which
# project period the enrollment should be attached to. these extra ees will
# be excluded later

# AS 5/5/24: commenting out for now because this merge doesn't work correctly with intervals
# Submitted GitHub issue for lubridate: https://github.com/tidyverse/lubridate/issues/1165
# reprex: 
# x <- data.table(id = c(1,2), interval = c(lubridate::interval(Sys.Date(), Sys.Date() + 1), NA))
# y <- data.table(id = c(1,1,2))
# z <- x[y, on = .(id)]
# print(z)

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

# Get First HMIS span for each Project (technically, the enrollment record)
EnrollmentOutside <- EnrollmentOutside %>%
  roworder(ProjectTimeID) %>%
  collap( ~ ProjectID + EnrollmentID, ffirst) %>% 
  fselect(-EntryDate, -ExitAdjust)

Enrollment <- EnrollmentStaging %>%
  join(
    EnrollmentOutside,
    on = "EnrollmentID",
    how = "left"
  ) %>%
  fmutate(
    EntryDateTruncated = fifelse(
      EnrollmentvOperating %in% c("Enrollment Crosses Operating Start",
                                  "Enrollment Crosses Operating Period") |
        EnrollmentvParticipating %in% c("Enrollment Crosses Participating Start",
                                        "Enrollment Crosses Participating Period"),
      max(HMISParticipationStatusStartDate, OperatingStartDate, na.rm = TRUE),
      EntryDate
    ), # truncates to the earliest Participating/Operating End Date
    ExitDateTruncated = fifelse(
      EnrollmentvOperating %in% c("Enrollment Crosses Operating End",
                                  "Enrollment Crosses Operating Period") |
        EnrollmentvParticipating %in% c("Enrollment Crosses Participating End",
                                        "Enrollment Crosses Participating Period"),
      min(HMISParticipationStatusEndDate, OperatingEndDate, na.rm = TRUE),
      ExitDate
    )
  ) %>%
  relocate(Destination:ExitDateTruncated, .before = RelationshipToHoH)

# Move In Dates -----------------------------------------------------------

# granularity: HouseholdIDs with ValidMoveIns
browser()
HHMoveIn <- Enrollment %>% 
  fsubset(ProjectType %in% ph_project_types) %>%
  fmutate(
    # Add the AssumedMoveIn and ValidMoveIn columns
    AssumedMoveIn = EntryDate < hc_psh_started_collecting_move_in_date & 
      ProjectType %in% psh_project_types,
    ValidMoveIn = fcase(
      AssumedMoveIn == TRUE, 
        EntryDate,
      ProjectType %in% psh_project_types & EntryDate <= MoveInDate & MoveInDate < ExitAdjust, 
        MoveInDate,
      ProjectType == rrh_project_type & EntryDate <= MoveInDate & MoveInDate <= ExitAdjust,
        MoveInDate
    )
  ) %>%
  fsubset(!is.na(ValidMoveIn))
  
# Group by HouseholdID and calculate HHMoveIn
HHMoveIn <- HHMoveIn[, .(HHMoveIn = min(ValidMoveIn, na.rm = TRUE)), by = HouseholdID]
  
# Select the columns and remove duplicates
HHMoveIn <- unique(HHMoveIn, by = c("HouseholdID", "HHMoveIn"))

# Group by HouseholdID and calculate FirstEntry
HHEntry <- Enrollment %>%
  fgroup_by(HouseholdID) %>%
  fsummarise(HHEntry = fmin(EntryDate)) %>%
  join(
    HHMoveIn,
    on= "HouseholdID",
    how = "left"
  )

setDF(HHEntry)

Enrollment <- Enrollment %>%
  join(HHEntry, how = "left", on = "HouseholdID") %>%
  fmutate(
    MoveInDateAdjust = fcase(
      EntryDate < hc_psh_started_collecting_move_in_date &
        MoveInDate != EntryDate &
        ProjectType %in% psh_project_types, EntryDate,
      !is.na(HHMoveIn) & ymd(HHMoveIn) <= ExitAdjust, MoveInDate
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

# Build validation df for app ---------------------------------------------
# this contains Project and Org info together
validationProject <- ProjectSegments %>%
  select(
    ProjectID,
    ProjectTimeID,
    OrganizationName,
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
session$userData$validation <- validationProject %>%
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

# Checking requirements by projectid --------------------------------------

projects_funders_types <- Funder %>%
  left_join(Project %>%
              select(ProjectID, ProjectType),
            join_by(ProjectID)) %>%
  filter(is.na(EndDate) | EndDate > session$userData$meta_HUDCSV_Export_Start) %>%
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
    session$userData$Project0 %>%
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
    coalesce(InventoryEndDate, no_end_date) >= session$userData$meta_HUDCSV_Export_Start &
      InventoryStartDate <= session$userData$meta_HUDCSV_Export_End
  )

# HMIS-participating projects with active Inventory during report period -------
# HMIS-Participating projects that have inventory are almost necessarilly residential.
# Store this "generous" span so we can check if any enrollments fall within it
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

session$userData$CurrentLivingSituation <- CurrentLivingSituation
session$userData$Event <- Event
