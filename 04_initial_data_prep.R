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
         DOB = NULL)

# Truncating Enrollments based on Operating/Participating -----------------
# Perform the join
EnrollmentOutside <- as.data.table(EnrollmentStaging %>%
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
            relationship = "many-to-many")) 
# many-to-many bc there will be ees that match to 2 rows of the same ProjectID
# and this is expected at this point in the code bc we want to sus out which
# project period the enrollment should be attached to. these extra ees will
# be excluded later
Enrollmentvs <- function(EntryDate, ExitAdjust, ComparisonStart, ComparisonEnd, comparisonWord) {
  fcase(
    (EntryDate >= ComparisonStart & ExitAdjust <= ComparisonEnd) |
    (EntryDate >= ComparisonStart & ComparisonEnd > Sys.Date()),
      "Inside",
    EntryDate > ComparisonEnd,
      paste0("Enrollment After ", comparisonWord," Period"),
    EntryDate < ComparisonStart & ExitAdjust > ComparisonStart,
      paste0("Enrollment Crosses ", comparisonWord, " Start"),
    ExitAdjust < ComparisonStart,
      paste0("Enrollment Before ", comparisonWord, " Period"),
    EntryDate > ComparisonStart & ExitAdjust > ComparisonEnd,
      paste0("Enrollment Crosses ", comparisonWord, " End"),
    EntryDate < ComparisonStart & ExitAdjust > ComparisonEnd,
      paste0("Enrollment Crosses ", comparisonWord, " Period")
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
EnrollmentOutside <- EnrollmentOutside[order(ProjectTimeID), head(.SD, 1), by = .(ProjectID, EnrollmentID)]

EnrollmentOutside <- EnrollmentOutside[, .(EnrollmentID, ProjectID, ProjectTimeID, ProjectType, 
                                           EntryDate, ExitAdjust,
                                           OperatingStartDate, OperatingEndDate, 
                                           HMISParticipationStatusStartDate,
                                           HMISParticipationStatusEndDate,
                                           EnrollmentvParticipating,
                                           EnrollmentvOperating)]

Enrollment <- EnrollmentStaging %>%
  left_join(as.data.frame(EnrollmentOutside),
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
HHMoveIn <- as.data.table(Enrollment)[ProjectType %in% ph_project_types]

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
HHMoveIn <- HHMoveIn[, .(HHMoveIn = min(ValidMoveIn, na.rm = TRUE)), by = HouseholdID]
  
# Select the columns and remove duplicates
HHMoveIn <- unique(HHMoveIn, by = c("HouseholdID", "HHMoveIn"))

# Group by HouseholdID and calculate FirstEntry
HHEntry <- as.data.table(Enrollment)[, .(HHEntry = min(EntryDate)), by = HouseholdID]

  # select(HouseholdID, "HHEntry" = FirstEntry) %>%
  # unique() %>%
HHEntry <- unique(HHEntry[, c(HouseholdID, HHEntry)])
  
  # left_join(HHMoveIn, by = "HouseholdID")
HHEntry <- HHMoveIn[HHEntry, on = .(HouseholdID)]

HHEntry <- as.data.frame(HHEntry)

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

CurrentLivingSituation(CurrentLivingSituation)
Event(Event)