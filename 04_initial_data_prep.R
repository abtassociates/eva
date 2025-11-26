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
      fselect(OrganizationID, OrganizationName, VictimServiceProvider),
    how = "left",
    on = "OrganizationID"
  )

ProjectSegments <- project_prep %>%
  join(
    HMISParticipation %>%
      fselect(
        ProjectID,
        HMISParticipationType,
        HMISParticipationStatusStartDate,
        HMISParticipationStatusEndDate
      ) %>%
      funique(),
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
  fselect(ProjectID,
         ProjectName,
         OrganizationID,
         OrganizationName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectType,
         RRHSubType,
         HousingType,
         VictimServiceProvider,
         ContinuumProject) %>%
  funique()

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
    # DomesticViolenceCategory = fcase(
    #   DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1, "DVFleeing",
    #   DomesticViolenceSurvivor == 1, "DVNotFleeing",
    #   default = "NotDV"
    # ),
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
              fselect(ProjectID,
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

EnrollmentOutside <- EnrollmentOutside %>% 
  fmutate(
    EnrollmentvParticipating = Enrollmentvs(
      EntryDate, ExitAdjust, 
      HMISParticipationStatusStartDate, HMISParticipationStatusEndDate, 
      "Participating"
      ),
    EnrollmentvOperating = Enrollmentvs(
      EntryDate, ExitAdjust, 
      OperatingStartDate, OperatingEndDate, 
      "Operating")
  )

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

# Adjust Move-In Date -----------------------------------------------------------
# Move-In Date is only collected for HoH. So we need to compute an adjusted 
# household move-In Date (HMID) for all members, using the HoH's earliest move-in date
# Note households are tied to the Project. There may mistakenly be multiple HoH, so take earliest

# Here are the adjustments:
#   - For PSH projects, we account for HMID not being collected until 2017-10-01 
#     by using EntryDate, instead of MoveInDate
#   - For other PH projects, we don't trust HMIDs > ExitDate, so we set them to NA
#   - For all other project types, even though many have HMID, we set to NA
#     Because, throughout Eva, we only care about HMID for PH projects
HHMoveIn <- Enrollment %>%
  fsubset(RelationshipToHoH == 1 & ProjectType %in% ph_project_types) %>%
  fmutate(HoHMoveInDate = fcase(
    ProjectType %in% psh_oph_project_types & 
    EntryDate < hc_psh_started_collecting_move_in_date &
    (is.na(MoveInDate) | MoveInDate < EntryDate),
      EntryDate,
  
    !ProjectType %in% ph_project_types |
    (MoveInDate < EntryDate | (
      (ProjectType == rrh_project_type & MoveInDate > ExitAdjust) | 
        (ProjectType %in% psh_oph_project_types & MoveInDate >= ExitAdjust)
    )),
      NA,
    default = MoveInDate
  )) %>%
  fgroup_by(HouseholdID) %>%
  fsummarise(HMID = fmin(HoHMoveInDate))

Enrollment <- Enrollment %>%
  join(
    HHMoveIn,
    on = "HouseholdID",
    how = "left"
  ) %>%
  fmutate(
    MoveInDateAdjust = fifelse(
      RelationshipToHoH == 1, 
      HMID,
      # For non-HoH:
      #   - If they enter after HMID, set to EntryDate
      #   - If they enter on or before HMID, set to HMID
      #   - If they exited before HMID, set to NA
      fcase(
        !ProjectType %in% ph_project_types, NA_Date_,
        EntryDate > HMID, EntryDate,
        EntryDate <= HMID, HMID,
        ExitAdjust < HMID, NA_Date_
      )
    )
  )

rm(HHMoveIn)

# Only contains EEs within Operating and Participating Dates --------------
# to be used for system data analysis purposes. has been culled of enrollments
# that fall outside of participation/operation date ranges.

EnrollmentAdjust <- Enrollment %>%
  fsubset(
    !EnrollmentvParticipating %in% c(
      "Enrollment After Participating Period",
      "Enrollment Before Participating Period"
    ) &
      !EnrollmentvOperating %in% c(
        "Enrollment After Operating Period",
        "Enrollment Before Operating Period"
      )
  )

# Only BedNight Services --------------------------------------------------

Services <- Services %>%
  fsubset(RecordType == 200 & !is.na(DateProvided)) %>%
  fselect(EnrollmentID, DateCreated, DateProvided, PersonalID) %>%
  qDT()

# Build validation df for app ---------------------------------------------
# this contains Project and Org info together
validationProject <- ProjectSegments %>%
  fselect(
    ProjectID,
    ProjectTimeID,
    OrganizationName,
    ProjectName,
    ProjectType
  )

validationEnrollment <- Enrollment %>% 
  fselect(
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

#0.006122828 vs 0.0002565384  
session$userData$validation <- validationProject %>%
  join(validationEnrollment, on = c("ProjectTimeID", "ProjectID"), how='left', multiple=T) %>%
  fselect(
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
  fsubset(!is.na(EntryDate))

# Checking requirements by projectid --------------------------------------

projects_funders_types <- Funder %>%
  join(Project %>%
              fselect(ProjectID, ProjectType),
            on = 'ProjectID', how='left') %>%
  fsubset(is.na(EndDate) | EndDate > session$userData$meta_HUDCSV_Export_Start) %>%
  fselect(ProjectID, ProjectType, Funder) %>%
  funique() %>%
  join(inc_ncb_hi_required, on=c('ProjectType', 'Funder')) %>%
  fmutate(inc = replace_na(inc, FALSE),
         ncb = replace_na(ncb, FALSE),
         hi = replace_na(hi, FALSE),
         dv = replace_na(dv, FALSE)) %>%
  fgroup_by(ProjectID) %>%
  fsummarise(inc = fmax(inc, na.rm = TRUE),
            ncb = fmax(ncb, na.rm = TRUE),
            hi = fmax(hi, na.rm = TRUE),
            dv = fmax(dv, na.rm = TRUE)) %>%
  fungroup() %>%
  qDT()

# Active Inventory -------------------------------------------------------------
activeInventory <- Inventory %>%
  join(
    session$userData$Project0 %>%
      fselect(
        ProjectID,
        OrganizationName,
        ProjectName,
        OperatingStartDate,
        OperatingEndDate
      ) %>%
      funique(),
    on = "ProjectID",
    how = 'left'
  ) %>%
  fsubset(
    fcoalesce(InventoryEndDate, no_end_date) >= session$userData$meta_HUDCSV_Export_Start &
      InventoryStartDate <= session$userData$meta_HUDCSV_Export_End
  )

# Filter out overflow beds
activeInv_no_overflow <- activeInventory %>% 
  fsubset(
    (is.na(Availability) | Availability != 3) &
      BedInventory > 0 & !is.na(BedInventory)
  ) 

# Event (Used in DQ)
Event <- Event %>% 
  fselect(
    EnrollmentID,
    EventID,
    EventDate,
    Event,
    ProbSolDivRRResult,
    ReferralCaseManageAfter,
    LocationCrisisOrPHHousing,
    ReferralResult,
    ResultDate
  )

session$userData$Services <- Services
session$userData$Exit <- Exit
session$userData$Enrollment <- Enrollment
session$userData$CurrentLivingSituation <- CurrentLivingSituation
# desk_time_providers <- validation() %>%
#   dplyr::filter(
#     (entered_between(., today() - years(1), today()) |
#        exited_between(., today() - years(1), today())) &
#       ProjectType %in% lh_ph_hp_project_types) %>%
#   dplyr::select(ProjectName) %>% unique()

# HMIS Participation ------------------------------------------------------
# hmis_participating_projects <- session$userData$Project0 %>%
#   join(HMISParticipation %>% 
#          fsubset(HMISParticipationType %in% c(0,1,2)),
#        on = "ProjectID", how = 'inner') %>%
#   subset(ProjectType %in% project_types_w_beds) %>% 
#   pull(ProjectID) %>%
#   funique()
# 
# HMIS_participating_projects_w_active_inv_no_overflow <- base::intersect(
#   activeInv_no_overflow %>% pull(ProjectID) %>% funique(), 
#   hmis_participating_projects)

HMIS_project_active_inventories <- qDT(ProjectSegments) %>%
  fsubset(HMISParticipationType %in% c(0,1,2)) %>% # filter to projects with HMIS Participation
  join(activeInv_no_overflow %>% select(-DateCreated,-DateUpdated,-UserID,-DateDeleted), 
    ##on = "ProjectID",
    how = "inner",
    multiple = TRUE
  ) %>% 
  fsubset(ProjectType %in% project_types_w_beds) %>% # filter to ProjectType with Beds
  fsubset(ProjectType!=rrh_project_type | RRHSubType ==2) %>% # filter RRH projects to subtype 2
  # Get the Start+End dates for when each Project was Operating, HMIS Participating, and Active (Inventory)
  fmutate(
    InvHMISParticipationStart = pmax( # start of Operating & Participating 
      HMISParticipationStatusStartDate, 
      OperatingStartDate,
      na.rm = TRUE
    ),
    InvHMISParticipationEnd = pmin( # end of Operating & Participating
      HMISParticipationStatusEndDate,
      OperatingEndDate,
      na.rm = TRUE
    ),
    InvHMISActiveParticipationStart = pmax( # start of (Operating & Participating) & Active Inventory
      InvHMISParticipationStart,
      InventoryStartDate,
      na.rm = TRUE
    ),
    InvHMISActiveParticipationEnd = pmin( # end of (Operating & Participating) & Active Inventory
      InvHMISParticipationEnd,
      InventoryEndDate,
      na.rm = TRUE
    )
  )


HMIS_projects_w_active_inv <- HMIS_project_active_inventories %>%
  fgroup_by(ProjectID, HMISParticipationType, VictimServiceProvider, HousingType, TargetPopulation, HouseholdType, ESBedType, Availability) %>%
  fsummarise(ProjectHMISActiveParticipationStart = fmin(InvHMISActiveParticipationStart), # first active inv start with HMISPartiicpationType 
             ProjectHMISActiveParticipationEnd = fmax(InvHMISActiveParticipationEnd), # last active inv end with HMISPartiicpationType
             #TargetPopulation = list(sort(unique(TargetPopulation))), # sum?
             UnitInventory = fsum(UnitInventory),
             BedInventory = fsum(BedInventory),
             CHVetBedInventory = fsum(CHVetBedInventory),
             YouthVetBedInventory = fsum(YouthVetBedInventory),
             VetBedInventory = fsum(VetBedInventory),
             CHYouthBedInventory = fsum(CHYouthBedInventory),
             YouthBedInventory = fsum(YouthBedInventory),
             CHBedInventory = fsum(CHBedInventory)) %>% 
  fungroup()

HMIS_projects_w_active_inv <- HMIS_projects_w_active_inv %>%
  fmutate(HMISActiveParticipationDuration = fifelse(is.na(ProjectHMISActiveParticipationEnd),
                                                    Sys.Date() - as.Date(ProjectHMISActiveParticipationStart),
                                                    as.Date(ProjectHMISActiveParticipationEnd) - as.Date(ProjectHMISActiveParticipationStart)),
          VetUnitInventory = UnitInventory * (VetBedInventory + YouthVetBedInventory + CHVetBedInventory)/BedInventory,
          YouthUnitInventory = UnitInventory * (YouthBedInventory + YouthVetBedInventory + CHYouthBedInventory)/BedInventory,
          CHUnitInventory = UnitInventory * ( CHBedInventory + CHVetBedInventory + CHYouthBedInventory)/BedInventory
  )


browser()
