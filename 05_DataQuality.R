###############################
#   PURPOSE: This script conducts the data quality checks
#   It starts by creating the base_dq_data dataframe, which is 1 
#   enrollment per row and combines the necessary datasets to contain all
#   needed fields for all the checks
#
#   Checks include, but are not limited to:
#     - Missing UDEs (Missing/Incomplete/Refused Name Data quality, DOB, etc.)
#     - Missing Client Location
#     - Household Issues (children-only, too many HoHs, etc.)
#     - Missing Data at Entry (Living Situation,  Length of Stay, etc.)
#     - Overlaps
#     - Future Entry Exits
###############################
# run_data_quality_checks <- function() {
logToConsole(session, "Running Data Quality")

# The Variables That We Want ----------------------------------------------
# these are for the DQ export

vars_prep <- c(
  "EnrollmentID",
  "HouseholdID",
  "PersonalID",
  "OrganizationName",
  "ProjectID",
  "ProjectName",
  "ProjectType",
  "EntryDate",
  "MoveInDateAdjust",
  "ExitDate"
)

vars_we_want <- c(vars_prep,
                  "Issue",
                  "Type",
                  "Guidance")

# Clients to Check --------------------------------------------------------

# base_dq_data is meant to serve as a basic dataset with a granularity
# of 1 enrollment per row. if other data needs to be joined to this for specific
# data quality checks that will alter the granularity, please join to it when
# you're building that data frame.
# this will keep the base_dq_data more compact and of the same
# granularity for consistency
base_dq_data <- Enrollment %>%
  join(Client %>%
              fselect(-DateCreated), on = "PersonalID", how = 'left') %>%
  join(ProjectSegments %>% fselect(ProjectTimeID, ProjectName, OrganizationName),
            on = "ProjectTimeID", how = 'left') %>%
  fselect(
    funique(c(vars_prep,
    'FirstName',
    'NameDataQuality',
    'SSN',
    'SSNDataQuality',
    'DOB',
    'DOBDataQuality',
    'AgeAtEntry',
    'RaceNone',
    'AmIndAKNative',
    'Asian',
    'BlackAfAmerican',
    'NativeHIPacific',
    'White',
    'MidEastNAfrican',
    'HispanicLatinaeo',
    'VeteranStatus',
    'ProjectTimeID',
    'EnrollmentCoC',
    'RelationshipToHoH',
    'LivingSituation',
    'LengthOfStay',
    'LOSUnderThreshold',
    'PreviousStreetESSH',
    'DateToStreetESSH',
    'TimesHomelessPastThreeYears',
    'MonthsHomelessPastThreeYears',
    'DisablingCondition',
    'DateOfEngagement',
    'MoveInDate',
    'Destination',
    'DestinationSubsidyType',
    'ExitAdjust',
    'DateCreated',
    'HouseholdType'))
  )

DV <- HealthAndDV %>%
  fsubset(DataCollectionStage == 1) %>%
  fselect(EnrollmentID, DomesticViolenceSurvivor, WhenOccurred, CurrentlyFleeing)

base_dq_data <- base_dq_data %>%
  join(DV, on = "EnrollmentID", how = 'left')

rm(DV)

# Duplicate EEs -----------------------------------------------------------

duplicate_ees <- base_dq_data %>%
  fsubset(
    fduplicated(fselect(base_dq_data, PersonalID, ProjectID, EntryDate), all = TRUE)
  ) %>%
  merge_check_info_dt(checkIDs = 1) %>%
  fselect(vars_we_want)

# Missing UDEs ------------------------------------------------------------

# missing_name_dataquality <- base_dq_data %>%
#   filter(is.na(NameDataQuality)) %>%
#   merge_check_info_dt(checkIDs = 33) %>%
#   select(all_of(vars_we_want))

dkr_name <- base_dq_data %>%
  fsubset(NameDataQuality %in% c(dkr_dnc, 2)) %>%
  merge_check_info_dt(checkIDs = 78) %>%
  fselect(vars_we_want)

missing_dob <- base_dq_data %>%
  fsubset(is.na(DOB) & DOBDataQuality %in% c(1, 2)) %>%
  merge_check_info_dt(checkIDs = 34) %>%
  fselect(vars_we_want)

# missing_dob_dataquality <- base_dq_data %>%
#   filter(is.na(DOBDataQuality)) %>%
#   merge_check_info_dt(checkIDs = 35) %>%
#   select(all_of(vars_we_want))

dkr_dob <- base_dq_data %>%
  fsubset(DOBDataQuality %in% c(dkr_dnc)) %>%
  merge_check_info_dt(checkIDs = 60) %>%
  fselect(vars_we_want)

incorrect_dob <- base_dq_data %>%
  fsubset(AgeAtEntry < 0 | AgeAtEntry > 100) %>%
  merge_check_info_dt(checkIDs = 84) %>%
  fselect(vars_we_want)

# missing_ssn <- base_dq_data %>%
#   filter((is.na(SSN) & !SSNDataQuality %in% c(dkr_dnc))) %>%
#   merge_check_info_dt(checkIDs = 85) %>%
#   select(all_of(vars_we_want))

dkr_ssn <- base_dq_data %>%
  fsubset(SSNDataQuality %in% c(dkr_dnc)) %>%
  merge_check_info_dt(checkIDs = 67) %>%
  fselect(vars_we_want)

dkr_race <- base_dq_data %>%
  fsubset(RaceNone %in% c(dkr_dnc)) %>%
  merge_check_info_dt(checkIDs = 63) %>%
  fselect(vars_we_want)

# missing_veteran_status <- base_dq_data %>%
#   filter(
#     (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
#     (is.na(VeteranStatus))
#   ) %>%
#   merge_check_info_dt(checkIDs = 39) %>%
#   select(all_of(vars_we_want))

dkr_veteran <- base_dq_data %>%
  fsubset(
    (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
    VeteranStatus %in% c(dkr_dnc)
  ) %>%
  merge_check_info_dt(checkIDs = 66) %>%
  fselect(vars_we_want)

# Missing Client Location -------------------------------------------------

missing_enrollment_coc <- base_dq_data %>%
  fsubset(is.na(EnrollmentCoC) & RelationshipToHoH == 1) %>%
  merge_check_info_dt(checkIDs = 27) %>%
  fselect(vars_we_want)

# Household Issues --------------------------------------------------------

## OLD: demo: 0.038s, 0.008s, 0.439-0.449s, 0.869-0.883s
## NEW: demo: 0.007s, 0.007s, 0.021s, 0.045s

hh_children_only <- base_dq_data %>%
    fgroup_by(HouseholdID) %>%
    fsummarise(
      maxAge=fmax(AgeAtEntry, na.rm=FALSE)
    ) %>%
    fungroup() %>%
    fsubset(maxAge < 12) %>%
    join(base_dq_data, on = c("HouseholdID", "maxAge" = "AgeAtEntry"), how='left') %>%
    funique(cols = c("HouseholdID", "maxAge")) %>% 
    merge_check_info(checkIDs = 86) %>%
    fselect(vars_we_want)

hh_no_hoh <- base_dq_data %>%
  fgroup_by(HouseholdID) %>%
  fsummarise(
    hasHoH = fmin(RelationshipToHoH) == 1,
    PersonalID = min(PersonalID)
  ) %>%
  fungroup() %>%
  fsubset(!hasHoH) %>%
  join(
    base_dq_data, 
    on = c('PersonalID','HouseholdID'), 
    how = 'left'
  ) %>% 
  merge_check_info_dt(checkIDs = 2) %>%
  fselect(vars_we_want)

hh_too_many_hohs <- base_dq_data %>%
  fsubset(RelationshipToHoH == 1) %>% 
  fgroup_by(HouseholdID) %>%
  fsummarize(HoHsinHousehold = GRPN(),
             PersonalID = min(PersonalID)) %>%
  fungroup() %>%
  fsubset(HoHsinHousehold > 1) %>%
  join(base_dq_data, on=c('PersonalID','HouseholdID'), how='left') %>%
  merge_check_info_dt(checkIDs = 3) %>%
  fselect(vars_we_want)


hh_missing_rel_to_hoh <- base_dq_data %>%
  fsubset(RelationshipToHoH == 99) %>%
  join(hh_no_hoh, on = "HouseholdID", how = 'anti') %>%
  merge_check_info_dt(checkIDs = 4) %>%
  fselect(vars_we_want)

hh_issues <- 
  rowbind(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

rm(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

missing_approx_date_homeless <- base_dq_data %>%
  fselect(
    funique(c(vars_prep,
    'ProjectID',
    'AgeAtEntry',
    'RelationshipToHoH',
    'LOSUnderThreshold',
    'DateToStreetESSH',
    'PreviousStreetESSH'))
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           is.na(DateToStreetESSH) &
           LOSUnderThreshold == 1 &
           PreviousStreetESSH == 1
  ) %>%
  merge_check_info_dt(checkIDs = 28) %>%
  fselect(vars_we_want)

missing_previous_street_ESSH <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'PreviousStreetESSH',
    'LOSUnderThreshold'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           is.na(PreviousStreetESSH) &
           LOSUnderThreshold == 1
  ) %>%
  merge_check_info_dt(checkIDs = 29) %>%
  fselect(vars_we_want)

missing_residence_prior <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'LivingSituation'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LivingSituation))) %>%
  merge_check_info_dt(checkIDs = 30) %>%
  fselect(vars_we_want)

dkr_residence_prior <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'LivingSituation'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LivingSituation %in% c(dkr_dnc)) %>%
  merge_check_info_dt(checkIDs = 64) %>%
  fselect(vars_we_want)

missing_LoS <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'LengthOfStay'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LengthOfStay))) %>%
  merge_check_info_dt(checkIDs = 26) %>%
  fselect(vars_we_want)

dkr_LoS <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'LengthOfStay'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LengthOfStay %in% c(dkr_dnc)) %>%
  merge_check_info_dt(checkIDs = 73) %>%
  fselect(vars_we_want)

missing_months_times_homeless <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'MonthsHomelessPastThreeYears',
    'TimesHomelessPastThreeYears'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           ProjectType %in% c(
             es_nbn_project_type,
             es_ee_project_type,
             out_project_type,
             sh_project_type) &
           (
             is.na(MonthsHomelessPastThreeYears) |
               is.na(TimesHomelessPastThreeYears)
           )
  ) %>%
  merge_check_info_dt(checkIDs = 31) %>%
  fselect(vars_we_want)

dkr_months_times_homeless <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'MonthsHomelessPastThreeYears',
    'TimesHomelessPastThreeYears'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           (
             MonthsHomelessPastThreeYears %in% c(dkr_dnc) |
               TimesHomelessPastThreeYears %in% c(dkr_dnc)
           )
  ) %>%
  merge_check_info_dt(checkIDs = 61) %>%
  fselect(vars_we_want)

invalid_months_times_homeless <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'MonthsHomelessPastThreeYears',
    'TimesHomelessPastThreeYears',
    'DateToStreetESSH'
  ) %>%
  fsubset(ProjectType != 12 &
           (RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required)

approx_start_after_entry <- invalid_months_times_homeless %>%
  fsubset(!is.na(DateToStreetESSH) &
           EntryDate < DateToStreetESSH) %>%
  merge_check_info_dt(checkIDs = 69) %>%
  fselect(vars_we_want)

no_months_can_be_determined <- invalid_months_times_homeless %>%
  fsubset(MonthsHomelessPastThreeYears %in% c(dkr_dnc) &
           TimesHomelessPastThreeYears == 1) %>%
  merge_check_info_dt(checkIDs = 70) %>%

  fselect(vars_we_want)
no_months_v_living_situation_data <-
  invalid_months_times_homeless %>%
  fmutate(
    MonthHomelessnessBegan = floor_date(DateToStreetESSH, "month"),
    MonthEnteredProgram = floor_date(EntryDate, "month"),
    MonthDiff =
      interval(MonthHomelessnessBegan, MonthEnteredProgram) %/% months(1) + 1,
    MonthDiff = fifelse(MonthDiff >= 13, 13, MonthDiff),
    DateMonthsMismatch = fifelse(
      MonthsHomelessPastThreeYears - MonthDiff != 100 &
        TimesHomelessPastThreeYears == 1,
      1,
      0
    )
  ) %>%
  fsubset(TimesHomelessPastThreeYears == 1 &
           !is.na(DateToStreetESSH) &
           DateMonthsMismatch == 1) %>%
  merge_check_info_dt(checkIDs = 71) %>%
  fselect(vars_we_want)

approx_start_v_living_situation_data <-
  invalid_months_times_homeless %>%
  fmutate(
    HomelessOver3YearsAgo = !is.na(DateToStreetESSH) &
      ymd(DateToStreetESSH) <= ymd(EntryDate) %m-% months(36),
    SomethingsNotRight = TimesHomelessPastThreeYears != 1 |
      MonthsHomelessPastThreeYears < 112
  ) %>%
  fsubset(HomelessOver3YearsAgo == TRUE & SomethingsNotRight == TRUE) %>%
  merge_check_info_dt(checkIDs = 105) %>%
  fselect(vars_we_want)

rm(invalid_months_times_homeless)

missing_living_situation <- base_dq_data %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'RelationshipToHoH',
    'LivingSituation',
    'LengthOfStay',
    'LOSUnderThreshold',
    'PreviousStreetESSH'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           # not req'd prior to this
           ProjectType %in% c(
             th_project_type,
             psh_oph_project_types,
             sso_project_type,
             hp_project_type,
             rrh_project_type) &
           (
             (
               LivingSituation %in% institutional_livingsituation &
                 LengthOfStay %in% c(2, 3, 10, 11) & # <= 90 days
                 (is.na(LOSUnderThreshold) |
                    is.na(PreviousStreetESSH))
             ) |
               (
                 LivingSituation %in% c(perm_livingsituation, temp_livingsituation, other_livingsituation) &
                   LengthOfStay %in% c(10, 11) & # <= 7 days
                   (is.na(LOSUnderThreshold) |
                      is.na(PreviousStreetESSH))
               )
           )
  ) %>%
  merge_check_info_dt(checkIDs = 41) %>%
  fselect(vars_we_want)

dkr_living_situation <- base_dq_data %>%
  fselect(
    vars_prep,
    'RelationshipToHoH',
    'AgeAtEntry',
    'LivingSituation',
    'MonthsHomelessPastThreeYears',
    'TimesHomelessPastThreeYears'
  ) %>%
  fsubset((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate > hc_prior_living_situation_required &
           (
             MonthsHomelessPastThreeYears %in% c(dkr_dnc) |
               TimesHomelessPastThreeYears %in% c(dkr_dnc) |
               LivingSituation %in% c(dkr_dnc)
           )
  ) %>%
  merge_check_info_dt(checkIDs = 68) %>%
  fselect(vars_we_want)

# DisablingCondition at Entry
dkr_disabilities <- base_dq_data %>%
  fselect(vars_prep,
         'DisablingCondition') %>%
  fsubset(DisablingCondition %in% c(dkr_dnc)) %>%
  merge_check_info_dt(checkIDs = 32) %>%
  fselect(vars_we_want)

# smallDisabilities <- Disabilities %>%
#   filter(DataCollectionStage == 1 &
#            ((DisabilityType == 10 &
#                DisabilityResponse %in% c(1:3)) |
#               (DisabilityType != 10 & DisabilityResponse == 1)
#            )) %>%
#   mutate(
#     IndefiniteAndImpairs =
#       case_when(
#         DisabilityType %in% c(6, 8) ~ 1,
#         TRUE ~ IndefiniteAndImpairs)
#   ) %>%
#   select(
#     PersonalID,
#     DisabilitiesID,
#     EnrollmentID,
#     InformationDate,
#     DisabilityType,
#     IndefiniteAndImpairs
#   )
# 
# # Developmental & HIV/AIDS get automatically IndefiniteAndImpairs = 1 per FY2020
# conflicting_disabilities <- base_dq_data %>%
#   select(all_of(vars_prep),
#          EnrollmentID,
#          AgeAtEntry,
#          RelationshipToHoH,
#          DisablingCondition) %>%
#   left_join(
#     smallDisabilities %>%
#       filter(IndefiniteAndImpairs == 1),
#     by = c("PersonalID", "EnrollmentID")
#   ) %>% 
#   filter((DisablingCondition == 0 & !is.na(DisabilitiesID)) |
#            (DisablingCondition == 1 & is.na(DisabilitiesID))) %>% 
#   mutate(
#     Issue = "Conflicting Disability of Long Duration yes/no",
#     Type = "Error",
#     Guidance = "If the user answered \"Yes\" to the \"Does the client have a 
#     disabling condition?\", then there should be a disability subassessment that
#     indicates the disability determination is Yes *and* the \"If yes,... long
#     duration\" question is Yes. Similarly if the user answered \"No\", the 
#     client should not have any disability subassessments that indicate that they
#     do have a Disabling Condition."
#   ) %>%
#   select(all_of(vars_we_want))
# 
# rm(smallDisabilities)

# Long Stayers ------------------------------------------------------------

top_percents_long_stayers <- base_dq_data %>%
  fselect(vars_prep) %>%
  fsubset(
    ProjectType %in% c(long_stayer_percentile_project_types) &
      is.na(ExitDate) &
      (
        !ProjectType %in% c(ph_project_types) |
          (
            ProjectType %in% c(ph_project_types) &
              !is.na(MoveInDateAdjust)
          )
      )
  ) %>%
  fmutate(Days = as.numeric(difftime(
      session$userData$meta_HUDCSV_Export_Date, 
      fifelse(ProjectType %in% c(ph_project_types), MoveInDateAdjust, EntryDate)
  ))) %>%
  fgroup_by(ProjectType, sort = FALSE) %>%
  roworder(-Days) %>%
  fmutate(quantDays = quantile(Days, fifelse(
    ProjectType %in% long_stayer_98_percentile_project_types, .98, .99
  ))) %>%
  fungroup() %>%
  fsubset(Days > quantDays) %>%
  merge_check_info_dt(checkIDs = 104) %>%
  fselect(vars_we_want)

# long stayers flags that come from inputs come from calculate_long_stayers()

# Possible Missing HMID ---------------------------------------------------

missed_movein_stayers <- base_dq_data %>%
  fselect('RelationshipToHoH', vars_prep) %>%
  fsubset(is.na(ExitDate) &
           is.na(MoveInDateAdjust) &
           ProjectType %in% c(ph_project_types) & 
           RelationshipToHoH == 1
  ) %>%
  fmutate(Days = as.numeric(difftime(session$userData$meta_HUDCSV_Export_Date, EntryDate)))

Top2_movein <- fsubset(missed_movein_stayers,
                      Days > quantile(Days, prob = 1 - 2 / 100, na.rm = TRUE)) %>%
  fselect(vars_prep) %>%
  merge_check_info_dt(checkIDs = 72) %>%
  fselect(vars_we_want)

# Project Exit Before Start --------------
exit_before_start <- base_dq_data %>%
  fsubset(ExitDate < EntryDate & !is.null(ExitDate) & !is.null(EntryDate)) %>% 
  merge_check_info_dt(checkIDs = 99) %>%
  fselect(vars_we_want)
# Missing Destination -----------------------------------------------------

# missing_destination <- base_dq_data %>%
#   filter(!is.na(ExitDate) &
#            (is.na(Destination))) %>%
#   merge_check_info_dt(checkIDs = 74) %>%
#   select(all_of(vars_we_want))

dkr_destination <- base_dq_data %>%
  fsubset(Destination %in% c(dkr_dnc, 30)) %>%
  merge_check_info_dt(checkIDs = 59) %>%
  fselect(vars_we_want)

missing_destination_subsidy <- base_dq_data %>%
  fsubset(!is.na(ExitDate) &
           Destination == 435 &
           (is.na(DestinationSubsidyType) |
           !DestinationSubsidyType %in% c(subsidy_types))) %>%
  merge_check_info_dt(checkIDs = 121) %>%
  fselect(vars_we_want)

# Missing ResPrior Subsidy ------------------------------------------------

missing_res_prior_subsidy <- base_dq_data %>%
  join(Enrollment %>% fselect(EnrollmentID, RentalSubsidyType),
            on = 'EnrollmentID', how = 'left') %>%
  fsubset(LivingSituation == 435 &
           (is.na(RentalSubsidyType) |
           !RentalSubsidyType %in% c(subsidy_types))) %>%
  merge_check_info_dt(checkIDs = 130) %>%
  fselect(vars_we_want)


# Missing CLS Subsidy -----------------------------------------------------

missing_cls_subsidy <- base_dq_data %>%
  join(CurrentLivingSituation %>%
              fsubset(CurrentLivingSituation == 435 &
                       (is.na(CLSSubsidyType) |
                       !CLSSubsidyType %in% c(subsidy_types))) %>%
              fselect(CurrentLivingSitID, EnrollmentID, CLSSubsidyType),
            on = 'EnrollmentID', how = 'inner') %>%
  merge_check_info_dt(checkIDs = 129) %>%
  fselect(vars_we_want)

# Missing PATH Data -------------------------------------------------------

#* Length of Stay in Res Prior
### adult, PATH-enrolled, and:
### Length of Stay is null or DNC -> error -OR-
### Length of Stay is DKR -> warning

# smallProject <- Project %>% select(ProjectID, ProjectName)
# 

# path_missing_los_res_prior <- base_dq_data %>%
#   select(
#     all_of(vars_prep),
#     ProjectID,
#     AgeAtEntry,
#     ClientEnrolledInPATH,
#     LengthOfStay
#   ) %>%
#   left_join(smallProject, by = c("ProjectID", "ProjectName")) %>%
#   filter(ProjectID %in% c(path_funded) &
#            AgeAtEntry > 17 &
#            ClientEnrolledInPATH == 1 &
#            (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
#   mutate(Issue = "Missing Residence Prior Length of Stay (PATH)",
#          Type = "Error",
#          Guidance = guidance_missing_at_entry) %>%
#   select(all_of(vars_we_want))


#* Engagement at Exit
### adult, PATH-enrolled, Date of Engagement is null -> error

# path_no_status_at_exit <- base_dq_data %>%
#   select(
#     all_of(vars_prep),
#     AgeAtEntry,
#     ClientEnrolledInPATH,
#     DateOfPATHStatus,
#     ReasonNotEnrolled
#   ) %>%
#   left_join(smallProject, by = "ProjectName") %>%
#   filter(ProjectID %in% c(path_funded) &
#            !is.na(ExitDate) &
#            AgeAtEntry > 17 &
#            (
#              is.na(ClientEnrolledInPATH) |
#                is.na(DateOfPATHStatus) |
#                (ClientEnrolledInPATH == 0 &
#                   is.na(ReasonNotEnrolled))
#            )) %>%
#   mutate(Issue = "PATH Status at Exit Missing or Incomplete",
#          Type = "Error",
#          Guidance = guidance_missing_at_exit) %>%
#   select(all_of(vars_we_want))

#* Status Determination at Exit
### adult, PATH-Enrolled is not null
### Date of Status Determ is null -> error


# path_status_determination <- base_dq_data %>%
#   select(all_of(vars_prep),
#          AgeAtEntry,
#          ClientEnrolledInPATH,
#          DateOfPATHStatus) %>%
#   left_join(smallProject, by = "ProjectName") %>%
#   filter(
#     ProjectID %in% c(path_funded) &
#       AgeAtEntry > 17 &
#       !is.na(ClientEnrolledInPATH) &
#       is.na(DateOfPATHStatus)
#   ) %>%
#   mutate(Issue = "Missing Date of PATH Status",
#          Type = "Error",
#          Guidance = "Users must indicate the PATH Status Date for any adult 
#          enrolled in PATH.") %>%
#   select(all_of(vars_we_want))

#* PATH Enrolled at Exit
### adult and:
### PATH Enrolled null or DNC -> error -OR-
# path_enrolled_missing <- base_dq_data %>%
#   select(all_of(vars_prep), AgeAtEntry, ClientEnrolledInPATH) %>%
#   left_join(smallProject, by = "ProjectName") %>%
#   filter(
#     ProjectID %in% c(path_funded) &
#       !is.na(ExitDate) &
#       AgeAtEntry > 17 &
#       (ClientEnrolledInPATH == 99 |
#          is.na(ClientEnrolledInPATH))
#   ) %>%
#   mutate(
#     Issue = "Missing PATH Enrollment at Exit",
#     Type = "Error",
#     Guidance = "Please enter the data for this item by clicking into the 
#     Entry or Exit pencil and creating an Interim. In the assessment, enter 
#     the correct PATH Enrollment Date and Save."
#   ) %>%
#   select(all_of(vars_we_want))

#* Not Enrolled Reason
### adult
### PATH Enrolled = No
### Reason is null -> error
# path_reason_missing <- base_dq_data %>%
#   select(
#     all_of(vars_prep),
#     AgeAtEntry,
#     ClientEnrolledInPATH,
#     ReasonNotEnrolled,
#     ProjectType
#   ) %>%
#   left_join(smallProject, by = "ProjectName") %>%
#   filter(ProjectID %in% c(path_funded) &
#            AgeAtEntry > 17 &
#            ClientEnrolledInPATH == 0 &
#            is.na(ReasonNotEnrolled)) %>%
#   mutate(
#     Issue = "Missing Reason Not PATH Enrolled",
#     Type = "Error",
#     Guidance = "The user has indicated the household was not enrolled into 
#     PATH, but no reason was selected."
#   ) %>%
#   select(all_of(vars_we_want))

#* Connection with SOAR at Exit
### adult
### Connection w/ SOAR is null or DNC -> error -OR-
### Connection w/ SOAR DKR -> warning
# smallIncomeSOAR <- IncomeBenefits %>%
#   select(PersonalID,
#          EnrollmentID,
#          ConnectionWithSOAR,
#          DataCollectionStage) %>%
#   filter(DataCollectionStage == 3)
# 
# path_SOAR_missing_at_exit <- base_dq_data %>%
#   select(all_of(vars_prep),
#          EnrollmentID,
#          AgeAtEntry,
#          ClientEnrolledInPATH) %>%
#   left_join(smallProject, by = "ProjectName") %>%
#   left_join(smallIncomeSOAR, by = c("PersonalID", "EnrollmentID")) %>%
#   filter(ProjectID %in% c(path_funded) &
#            AgeAtEntry > 17 &
#            DataCollectionStage == 3 &
#            is.na(ConnectionWithSOAR)) %>%
#   mutate(Issue = "Missing Connection with SOAR at Exit",
#          Type = "Error",
#          Guidance = guidance_missing_at_exit) %>%
#   select(all_of(vars_we_want))
# 
# rm(smallIncomeSOAR)

# Missing PATH Contacts
## client is adult/hoh and has no contact record in the EE -> error
## this is a high priority data quality issue
## if the contact was an "Outreach" record after 10/1/2019, it is being
## filtered out because they should be using CLS subs past that date.
# small_contacts <- CurrentLivingSituation %>%
#   left_join(base_dq_data, by = "PersonalID") %>%
#   filter(
#     parseDate(InformationDate) >= EntryDate &
#       parseDate(InformationDate) <= ExitAdjust) %>% 
#   group_by(PersonalID, ProjectName, EntryDate, ExitDate) %>%
#   summarise(CurrentLivingSituationCount = n()) %>%
#   ungroup()
# 
# missing_path_contact <- base_dq_data %>%
#   filter(ProjectID %in% c(path_funded) &
#            (AgeAtEntry > 17 |
#               RelationshipToHoH == 1)) %>%
#   select(all_of(vars_prep)) %>%
#   left_join(small_contacts,
#             by = c("PersonalID",
#                    "ProjectName",
#                    "EntryDate",
#                    "ExitDate")) %>%
#   mutate_at(vars(CurrentLivingSituationCount), ~replace(., is.na(.), 0)) %>%
#   filter(CurrentLivingSituationCount == 0) %>%
#   mutate(Issue = "Missing PATH Contact",
#          Type = "High Priority",
#          Guidance = "Every adult or Head of Household must have a Living
#          Situation contact record. If you see a record there but there is
#          no Date of Contact, saving the Date of Contact will correct this
#          issue.") %>%
#   select(all_of(vars_we_want))

# Future Entry Exits ------------------------------------------------------

# PSHs in the old days before Move In Dates would definitely have been entering
# their clients prior to their Entry Date since back then the Entry Date was the
# day they moved in. So they're excused from this prior to Move In Date's existence.
future_ees <- base_dq_data %>%
  fsubset(EntryDate > DateCreated &
           (!ProjectType %in% psh_oph_project_types |
              (ProjectType %in% psh_oph_project_types & 
                  EntryDate >= hc_psh_started_collecting_move_in_date
              )))  %>%
  merge_check_info_dt(checkIDs = 75) %>%
  fselect(vars_we_want)

future_exits <- base_dq_data %>%
  fsubset(!is.na(ExitDate) &
           ExitDate > as.Date(session$userData$meta_HUDCSV_Export_Date)) %>%
  merge_check_info_dt(checkIDs = 14) %>%
  fselect(vars_we_want)
    
# Missing Income at Entry -------------------------------------------------

projects_require_income <- unique(projects_funders_types[inc == 1]$ProjectID)

base_dq_data_inc <- base_dq_data %>%
  join(IncomeBenefits, on = c("PersonalID", "EnrollmentID"), how = 'left', multiple=TRUE)

missing_income_entry <- base_dq_data_inc %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'DataCollectionStage',
    'TotalMonthlyIncome',
    'IncomeFromAnySource'
  ) %>%
  fsubset(DataCollectionStage == 1 &
           ProjectID %in% c(projects_require_income) &
           (AgeAtEntry > 17 |
              is.na(AgeAtEntry)) &
           (is.na(IncomeFromAnySource))) %>%
  merge_check_info_dt(checkIDs = 87) %>%
  fselect(vars_we_want)

# if IncomeFromAnySource is yes then one of these should be a yes, and if it's a 
# no, then all of them should be no
smallIncome <- IncomeBenefits %>%
  fselect(
    PersonalID,
    EnrollmentID,
    Earned,
    Unemployment,
    SSI,
    SSDI,
    VADisabilityService,
    VADisabilityNonService,
    PrivateDisability,
    WorkersComp,
    TANF,
    GA,
    SocSecRetirement,
    Pension,
    ChildSupport,
    Alimony,
    OtherIncomeSource,
    DataCollectionStage
  ) %>%
  fsubset(DataCollectionStage %in% c(1, 3))

smallIncome[is.na(smallIncome)] <- 0

smallIncome <- funique(smallIncome) %>% 
  join(
    funique(fselect(IncomeBenefits,"PersonalID",
                    "EnrollmentID",
                    "DataCollectionStage",
                    "TotalMonthlyIncome",
                    "IncomeFromAnySource")), 
    how = 'full',
    multiple = TRUE,
    on = c("PersonalID",
           "EnrollmentID",
           "DataCollectionStage"))

income_subs <- base_dq_data %>%
  fselect(c("AgeAtEntry", vars_prep)) %>%
  join(smallIncome, on = c("PersonalID", "EnrollmentID"), how='left', multiple = T) %>%
  fmutate(
    IncomeCount =
      Earned +
      Unemployment +
      SSI +
      SSDI +
      VADisabilityService +
      VADisabilityNonService +
      PrivateDisability +
      WorkersComp +
      TANF +
      GA +
      SocSecRetirement +
      Pension +
      ChildSupport +
      Alimony +
      OtherIncomeSource
  )

conflicting_income_entry <- income_subs %>%
  fsubset(DataCollectionStage == 1 &
           ProjectID %in% c(projects_require_income) &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) & # revisit
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>%
  merge_check_info_dt(checkIDs = 88) %>%
  fselect(vars_we_want)

# Missing Income at Exit --------------------------------------------------

## CANNOT GET THIS CHUNK TO GO *UNIVERSALLY *FASTER WITH COLLAPSE...works for COHHIO but not for anything smaller
missing_income_exit <- base_dq_data_inc %>%
  fselect(
    vars_prep,
    'AgeAtEntry',
    'DataCollectionStage',
    'TotalMonthlyIncome',
    'IncomeFromAnySource'
  ) %>%
  fsubset(DataCollectionStage == 3 &
           ProjectID %in% c(projects_require_income) &
           (AgeAtEntry > 17 |
              is.na(AgeAtEntry)) &
           (is.na(IncomeFromAnySource))) %>%
  merge_check_info_dt(checkIDs = 89) %>%
  fselect(vars_we_want)

conflicting_income_exit <- income_subs %>%
  fsubset(DataCollectionStage == 3 &
           ProjectID %in% c(projects_require_income) &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>%
  merge_check_info_dt(checkIDs = 90) %>%
  fselect(vars_we_want)

rm(income_subs)

# Enrollment Active Outside Participating Dates ---------------------------

enrollment_positions <- Enrollment %>%
  fselect(EnrollmentID, EnrollmentvOperating, EnrollmentvParticipating) %>%
  join(base_dq_data, on = "EnrollmentID", how = 'left')
enrollment_after_participating_period <- enrollment_positions %>%
  fsubset(EnrollmentvParticipating == "Enrollment After Participating Period") %>%
  merge_check_info_dt(checkIDs = 111) %>%
  fselect(vars_we_want)

enrollment_x_participating_start <- enrollment_positions %>%
  fsubset(EnrollmentvParticipating == "Enrollment Crosses Participating Start") %>%
  merge_check_info_dt(checkIDs = 112) %>%
  fselect(vars_we_want)

enrollment_before_participating_period <- enrollment_positions %>%
  fsubset(EnrollmentvParticipating == "Enrollment Before Participating Period") %>%
  merge_check_info_dt(checkIDs = 113) %>%
  fselect(vars_we_want)

enrollment_x_participating_end <- enrollment_positions %>%
  fsubset(EnrollmentvParticipating == "Enrollment Crosses Participating End") %>%
  merge_check_info_dt(checkIDs = 114) %>%
  fselect(vars_we_want)

enrollment_x_participating_period <- enrollment_positions %>%
  fsubset(EnrollmentvParticipating == "Enrollment Crosses Participation Period") %>%
  merge_check_info_dt(checkIDs = 115) %>%
  fselect(vars_we_want)

# Enrollment v Operating --------------------------------------------------

enrollment_after_operating_period <- enrollment_positions %>%
  fsubset(EnrollmentvOperating == "Enrollment After Operating Period") %>%
  merge_check_info_dt(checkIDs = 116) %>%
  fselect(vars_we_want)

enrollment_x_operating_start <- enrollment_positions %>%
  fsubset(EnrollmentvOperating == "Enrollment Crosses Operating Start") %>%
  merge_check_info_dt(checkIDs = 117) %>%
  fselect(vars_we_want)

enrollment_before_operating_period <- enrollment_positions %>%
  fsubset(EnrollmentvOperating == "Enrollment Before Operating Period") %>%
  merge_check_info_dt(checkIDs = 118) %>%
  fselect(vars_we_want)

enrollment_x_operating_end <- enrollment_positions %>%
  fsubset(EnrollmentvOperating == "Enrollment Crosses Operating End") %>%
  merge_check_info_dt(checkIDs = 119) %>%
  fselect(vars_we_want)

enrollment_x_operating_period <- enrollment_positions %>%
  fsubset(EnrollmentvOperating == "Enrollment Crosses Operating Period") %>%
  merge_check_info_dt(checkIDs = 120) %>%
  fselect(vars_we_want)

# Overlaps ----------------------------------------------------------------
# Create an initial dataset of possible overlaps
# and establish initial Enrollment intervals, based on project type
base_dq_data_dt <- qDT(base_dq_data)
overlap_staging <- base_dq_data_dt[
  EntryDate != ExitAdjust & 
  ((
    ProjectType %in% ph_project_types & 
      !is.na(MoveInDateAdjust)
    ) | 
     ProjectType %in% lh_residential_project_types
   ),
  .(
    PersonalID, 
    EnrollmentID, 
    ProjectType, 
    EnrollmentStart = fifelse(
      ProjectType %in% ph_project_types, 
      MoveInDateAdjust,
      EntryDate
    ),
    EnrollmentEnd = as.Date(ExitAdjust))
]

# For NbNs, modify EnrollmentStart/End to be the first/last DateProvided 
# for a given enrollment
if(nrow(Services) > 0) {
  services_summary <- Services[
    , .(
      FirstDateProvided = fmin(DateProvided, na.rm = TRUE),
      LastDateProvided = fmax(DateProvided, na.rm = TRUE)
    ), 
    by = EnrollmentID
  ]
  
  # now merge with staging and overwrite the original EnrollmentStart/End for 
  # nbns
  overlap_staging <- merge(
    overlap_staging, services_summary, 
    by = "EnrollmentID", 
    all.x = TRUE
  )
  #0.0147s vs 0.011s for main valid
  overlap_staging <- fmutate(overlap_staging,
       EnrollmentStart = fifelse(
         ProjectType == es_nbn_project_type,
         FirstDateProvided,
         EnrollmentStart
       ),
       EnrollmentEnd = fifelse(
         ProjectType == es_nbn_project_type,
         LastDateProvided,
         EnrollmentEnd
       )                    
    )
}

# get previous enrollment info using "lag"
overlap_dt <- overlap_staging[
  order(PersonalID, EnrollmentStart, EnrollmentEnd)
][, `:=`(
  PreviousEnrollmentID = shift(EnrollmentID, type = "lag"),
  PreviousEnrollmentStart = shift(EnrollmentStart, type = "lag"),
  PreviousEnrollmentEnd = shift(EnrollmentEnd, type = "lag"),
  PreviousProjectType = shift(ProjectType, type = "lag")
), by = PersonalID]

if(nrow(Services) > 0) {
# doing these now, to be used for overlap_details later
  overlap_dt$PreviousFirstDateProvided = shift(overlap_dt$FirstDateProvided, type = "lag")
  overlap_dt$PreviousLastDateProvided = shift(overlap_dt$LastDateProvided, type = "lag")
}

# Exclude first enrollment and do not compare RRH to PSH
overlap_dt[
  !is.na(PreviousEnrollmentID) &
  !(
    (ProjectType == rrh_project_type &
       PreviousProjectType %in% psh_oph_project_types) |
      (PreviousProjectType == rrh_project_type &
         ProjectType %in% psh_oph_project_types)
  )
]

# flag overlaps
# since the dataset is ordered by EnrollmentStart, there are 4 scenarios to consider:
# 1. 2nd enrl start < 1st enrl end, but 2nd enrl end > 1st enrl end - overlap by 1 day
# ----
#   ------
# 
# 2a. 2nd enrl fully contained within 1st - overlap 1 day
# ------
#   -
# 
# 2b. 2nd enrl fully contained within 1st - No overlap
# ------
#      -
#
# 2c. 2nd enrl fully contained within 1st - overlap 1 day
# ------
#     --
#
# 2d. 2nd enrl fully contained within 1st - overlap 1 days
# ------
# -
#
# 2e. 2nd enrl fully contained within 1st - overlap 2 days
# ------
# --
#
# 3. No overlap
# ------
#      ----
#
# 4. No overlap
# ------
#        ----
#
# The below method of calculating overlap days handles all 3 scenarios
# a non-overlap will have -OverlapDays, which will be handled correctly below
# when flagging if it's an overlap
overlap_dt[, OverlapDays := as.numeric(
  pmin(EnrollmentEnd, PreviousEnrollmentEnd) - 
  pmax(EnrollmentStart, PreviousEnrollmentStart))]

overlap_dt[, OverlapDays := fifelse(
  EnrollmentEnd < PreviousEnrollmentEnd,
  OverlapDays + 1,
  OverlapDays
)]

overlap_dt[, IsOverlap := fifelse(
  # NbN and EE, then overlap must be more than 2 days
  (
    (ProjectType == es_nbn_project_type & PreviousProjectType == es_ee_project_type) |
    (ProjectType == es_ee_project_type & PreviousProjectType == es_nbn_project_type)
  ),
  OverlapDays > 2,
  # otherwise, if not both NbN, any overlap counts (other than previous end == start)
  # if both NbN, we handle that differently later, looking only at Service records
  fifelse(
    !(ProjectType == es_nbn_project_type & PreviousProjectType == es_nbn_project_type),
    OverlapDays > 0 & EnrollmentStart != PreviousEnrollmentEnd,
    FALSE
  )
)]

overlap_dt <- overlap_dt[IsOverlap == TRUE]

# for NbN vs. NbN, if any DateProvided are the same, that's an overlap
# but because DatePRovided is m:1 with Enrollment, we need to process separately
# from the enrollment-level data above
if(nrow(Services) > 0) {
  overlap_nbns <- Services[, `:=`(
      PreviousEnrollmentID = shift(EnrollmentID, type = "lag"),
      IsOverlap = ifelse(duplicated(DateProvided) | duplicated(DateProvided, fromLast = TRUE), TRUE, FALSE), 
      PreviousProjectType = es_nbn_project_type
    ), 
    by = PersonalID
  ][
    IsOverlap == TRUE,
    .(PersonalID, EnrollmentID, PreviousEnrollmentID, DateProvided, IsOverlap, PreviousProjectType)
  ]

  # add the NbN overlaps back onto the main overlap_Dt
  overlap_dt <- rbindlist(
    list(overlap_dt, overlap_nbns),
    fill = TRUE
  )
}

# Bring in EvaChecks info, but overwrite Issue with overlap-specific text
# that indicates the project type being overlapped with
cols_to_keep <- c(
  "EnrollmentID",
  "PreviousEnrollmentID",
  "Issue",
  "Type",
  "Guidance"
)
if(nrow(Services) > 0) {
  cols_to_keep <- c(
    cols_to_keep,
    "DateProvided",
    "FirstDateProvided",
    "LastDateProvided",
    "PreviousFirstDateProvided",
    "PreviousLastDateProvided"
  )
}

overlap_dt <- merge_check_info_dt(overlap_dt, 77) %>% 
  fmutate(
    Issue = paste(
      "Overlap with",
      fifelse(str_sub(PreviousProjectType, 1, 1) %in% c("A", "E", "I", "O", "U"), "an", "a"),
      project_type(PreviousProjectType),
      "project"
    )
  ) %>% 
  fselect(cols_to_keep)

# Bring in additional enrollment details used to contextualize the flagged enrollment
# e.g. EntryDate, ExitAdjust, etc.
overlap_dt <- merge(
  overlap_dt,
  base_dq_data_dt[, c(vars_prep, "HouseholdType"), with = FALSE], 
  by = "EnrollmentID"
)

# For the Overlap Details tab of the export
# we want the same set of details for the overlapping enrollment (i.e. the "previous")

# this wide dataset is saved in the overlap_details() reactiveValue
# OverlappingDateProvided vs. FirstDateProvided vs. LastDateProvided:
# - OverlappingDateProvided is only relevant for NbN vs. NbN overlaps
# - FirstDateProvided and LastDateProvided are within a particular enrollment, 
#   constructing a range, used for NbN vs. any other type
get_overlap_col_order <- function() {
  main_enrl_cols <- vars_prep
  if(nrow(Services) > 0) {
    main_enrl_cols <- c(main_enrl_cols,
                        "FirstDateProvided",
                        "LastDateProvided"
    )
  }
  
  # add HouseholdType
  main_enrl_cols <- append(main_enrl_cols,
                           "HouseholdType",
                           after = which(main_enrl_cols == "HouseholdID"))
  
  previous_enrl_cols <- paste("Previous", main_enrl_cols, sep="")
  col_order <- c(main_enrl_cols, previous_enrl_cols)
  
  # add in OverlappingDateProvided
  if(nrow(Services) > 0) {
    col_order <- append(col_order,
                        c("OverlappingDateProvided" = "DateProvided"),
                        after = which(col_order == "MoveInDateAdjust"))
  }  
  
  return(col_order)
}
col_order <- get_overlap_col_order()

overlap_details <- merge(
  qDT(overlap_dt)[
    # Recode ProjectType to a more readable version
    , ProjectType := project_type(ProjectType)
  ],
  # Rename columns for previous enrollment
  base_dq_data_dt[
    , setNames(.SD, paste0("Previous", names(.SD)))
    , .SDcols = c(vars_prep, "HouseholdType")
  ],
  by = "PreviousEnrollmentID",
  all.x = TRUE
)[, `:=`(
  PreviousProjectType = project_type(PreviousProjectType),
  HouseholdType = fct_collapse(HouseholdType, !!!hh_types_in_exports),
  PreviousHouseholdType = fct_collapse(PreviousHouseholdType, !!!hh_types_in_exports)
)][
  # Drop Issue columns
  , !c("Issue", "Type", "Guidance"), with = FALSE
][
  # order and rename columns
  , ..col_order
]

# Remove unecessary columns
cols_to_remove <- "PreviousEnrollmentID"
if(nrow(Services) > 0) {
  cols_to_remove <- c(
    cols_to_remove,
    "DateProvided",
    "FirstDateProvided",
    "LastDateProvided",
    "PreviousFirstDateProvided",
    "PreviousLastDateProvided"
  )
}

get_vars(overlap_dt, cols_to_remove) <- NULL

overlap_dt[, HouseholdType := NULL]

# Invalid Move-in Date ----------------------------------------------------

invalid_movein_date <- base_dq_data %>%
  fsubset(ProjectType %in% ph_project_types & 
        ((!is.na(MoveInDate) & MoveInDate < EntryDate) | 
        (!is.na(MoveInDate) & !is.na(ExitDate) & MoveInDate > ExitDate))
  ) %>%
  merge_check_info_dt(checkIDs = 40) %>%
  fselect(vars_we_want)

# Missing Health Ins ------------------------------------------------------

projects_require_hi <- unique(projects_funders_types[hi == 1]$ProjectID)

missing_health_insurance <- base_dq_data_inc %>%
  fselect(vars_prep,
         'AgeAtEntry',
         'DataCollectionStage',
         'InsuranceFromAnySource') %>%
  fsubset((is.na(InsuranceFromAnySource)) &
           ProjectID %in% c(projects_require_hi))
  
missing_health_insurance_entry <- missing_health_insurance %>%
  fsubset(DataCollectionStage == 1) %>%
  merge_check_info_dt(checkIDs = 92) %>%
  fselect(vars_we_want)

missing_health_insurance_exit <- missing_health_insurance %>%
  fsubset(DataCollectionStage == 3) %>%
  merge_check_info_dt(checkIDs = 93) %>%
  fselect(vars_we_want)

health_insurance_subs <- base_dq_data_inc %>%
  fselect(
    vars_prep,
    'DataCollectionStage',
    'InsuranceFromAnySource',
    'Medicaid',
    'Medicare',
    'SCHIP',
    'VHAServices',
    'EmployerProvided',
    'COBRA',
    'PrivatePay',
    'StateHealthIns',
    'IndianHealthServices',
    'OtherInsurance'
  ) %>%
  fmutate(
    SourceCount = Medicaid + SCHIP + VHAServices + EmployerProvided +
      COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
      OtherInsurance + Medicare
  ) %>%
  fsubset((InsuranceFromAnySource == 1 &
            SourceCount == 0) |
           (InsuranceFromAnySource == 0 &
              SourceCount > 0))

conflicting_health_insurance_entry <- health_insurance_subs %>%
  fsubset(DataCollectionStage == 1 &
           ProjectID %in% c(projects_require_hi)) %>%
  merge_check_info_dt(checkIDs = 94) %>%
  fselect(vars_we_want)

conflicting_health_insurance_exit <- health_insurance_subs %>%
  fsubset(DataCollectionStage == 3 &
           ProjectID %in% c(projects_require_hi)) %>%
  merge_check_info_dt(checkIDs = 95) %>%
  fselect(vars_we_want)

rm(health_insurance_subs)

# Missing NCBs at Entry ---------------------------------------------------

projects_require_ncb <- unique(projects_funders_types[ncb == 1]$ProjectID)

#just the different kinds of non-cash benefits, many to an enrollment
ncb_subs <- IncomeBenefits %>%
  fselect(
    PersonalID,
    EnrollmentID,
    DataCollectionStage,
    SNAP,
    WIC,
    TANFChildCare,
    TANFTransportation,
    OtherTANF,
    OtherBenefitsSource
  ) %>%
  funique()

ncb_subs[is.na(ncb_subs)] <- 0

# basic ncb data but adding BenefitsFromAnySource, an ee-level data element
# BenefitsFromAnySource will repeat depending on its EEID & collection stage
ncbs <- ncb_subs %>%
  join(IncomeBenefits %>% fselect("PersonalID",
                             "EnrollmentID",
                             "DataCollectionStage",
                             "BenefitsFromAnySource") %>%
              funique(),
            on = c("PersonalID",
                   "EnrollmentID",
                   "DataCollectionStage"),
            how = 'full', 
            multiple = TRUE,
            validate = 'm:m')

# if there are conflicting yes/no records or conflicting subs, this will catch
# any that conflict with each other, which will prompt the user to correct the
# record(s) that's incorrect

ncb_staging <- base_dq_data %>%
    join(ncbs, on = c("PersonalID", "EnrollmentID"), how='left') %>%
    fsubset(
      DataCollectionStage == 1 &
        (AgeAtEntry > 17 |
           is.na(AgeAtEntry))
    ) %>%
    fmutate(
      BenefitCount = SNAP + WIC + TANFChildCare + TANFTransportation +
        OtherTANF + OtherBenefitsSource
    ) %>%
    fselect(vars_prep,
           "DataCollectionStage",
           "BenefitsFromAnySource",
           "BenefitCount") %>%
    funique()


missing_ncbs_entry <- ncb_staging %>%
  fsubset((is.na(BenefitsFromAnySource)) &
           ProjectID %in% c(projects_require_ncb)
  ) %>%
  merge_check_info_dt(checkIDs = 96) %>%
  fselect(vars_we_want)

conflicting_ncbs_entry <- base_dq_data %>%
  join(ncb_staging %>%
              fselect("PersonalID",
                     "EnrollmentID",
                     "DataCollectionStage",
                     "BenefitsFromAnySource",
                     "BenefitCount"),
            on = c("PersonalID",
                   "EnrollmentID"), how = 'left', multiple = TRUE) %>%
  fselect('AgeAtEntry',
         vars_prep,
         'DataCollectionStage',
         'BenefitsFromAnySource',
         'BenefitCount') %>%
  fsubset(DataCollectionStage == 1 &
           ProjectID %in% c(projects_require_ncb) &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((BenefitsFromAnySource == 1 &
               BenefitCount == 0) |
              (BenefitsFromAnySource == 0 &
                 BenefitCount > 0)
           )) %>%
  merge_check_info_dt(checkIDs = 97) %>%
  fselect(vars_we_want)
    
# Missing bed night for NBN Enrollment Entry ---------------------------------------
services_chk <- Services %>%
  fselect(EnrollmentID, DateProvided)  %>% 
  join(Enrollment %>% fselect(ProjectID, EnrollmentID), on = "EnrollmentID", how = 'left') %>% 
  unique

missing_bn0 <- base_dq_data %>% 
  fsubset(ProjectType == es_nbn_project_type) %>%
  join(HMISParticipation %>% fselect(ProjectID, HMISParticipationType), on = "ProjectID", how = 'left') %>%
  fsubset(HMISParticipationType == 1 ) %>%
  join(services_chk, on = c("EnrollmentID", "ProjectID"), how = 'left')

missing_bn1 <- missing_bn0 %>%
  fsubset(is.na(DateProvided)) %>% # EnrollmentID/ProjectID does NOT appear in services
  fselect(-DateProvided)

missing_bn2 <- missing_bn0 %>% 
  fsubset(!is.na(DateProvided)) %>% # EnrollmentID/ProjectID appears in services
  fselect(-DateProvided) %>% unique %>% # get unique rows after dropping DateProvided
  # rejoin with EntryDate = DateProvided, adding a flag that will be NA if the EntryDate does not appear
  join(services_chk %>% fmutate(services_flag=1), on = c("EnrollmentID", "ProjectID", "EntryDate" = "DateProvided"), how = 'left')
  
missing_bn2 <- missing_bn2 %>% 
  fsubset(is.na(services_flag)) %>% # but it does not appear on EntryDate
  fselect(-services_flag)

missing_bn_entry <- missing_bn1 %>% rbind(missing_bn2) %>% as.data.table() %>%
  merge_check_info_dt(checkIDs = 107) %>% 
  fselect(all_of(vars_we_want)) %>%
  unique()

# Bed night available for NBN Enrollment Exit ---------------------------------------
missing_bn2 <- missing_bn0 %>% 
  fsubset(!is.na(DateProvided)) %>% # EnrollmentID/ProjectID appears in services
  fselect(-DateProvided) %>% unique %>% # get unique rows after dropping DateProvided
  # rejoin with EntryDate = DateProvided, adding a flag that will be 1 if the ExitDate does appear
  join(services_chk %>% fmutate(services_flag=1), on = c("EnrollmentID", "ProjectID", "ExitDate" = "DateProvided"), how = 'left')

missing_bn2 <- missing_bn2 %>% 
  fsubset(services_flag==1) %>% # but it is appearing on ExitDate
  fselect(-DateProvided)

bn_on_exit <- missing_bn1 %>% rbind(missing_bn2) %>% as.data.table() %>%
  merge_check_info_dt(checkIDs = 108) %>% 
  fselect(all_of(vars_we_want)) %>%
  unique()

rm(missing_bn0, missing_bn1, missing_bn2, services_chk)


# SSVF --------------------------------------------------------------------
ssvf_base_dq_data <- base_dq_data %>%
  join(
    Funder[Funder %in% ssvf_fund_sources],
    on = "ProjectID",
    how = "inner"
  ) %>%
  fselect(vars_prep) %>%
  join(
    Enrollment %>%
      fselect(
        EnrollmentID,
        RelationshipToHoH,
        PercentAMI,
        VAMCStation,
        HPScreeningScore,
        ThresholdScore,
        TargetScreenReqd
      ),
    on = "EnrollmentID",
    how = "left"
  ) %>%
  join(
    Client %>%
      fselect(
        PersonalID,
        VeteranStatus,
        YearEnteredService,
        YearSeparated,
        WorldWarII,
        KoreanWar,
        VietnamWar,
        DesertStorm,
        AfghanistanOEF,
        IraqOIF,
        IraqOND,
        OtherTheater,
        MilitaryBranch,
        DischargeStatus
      ),
    on = "PersonalID",
    how = "left"
  )

veteran_missing_year_entered <- ssvf_base_dq_data %>%
  fsubset(VeteranStatus == 1 & is.na(YearEnteredService)) %>%
  merge_check_info_dt(checkIDs = 15) %>%
  fsubset(!is.na(Issue)) %>%
  fselect(vars_we_want)

veteran_incorrect_year_entered <- ssvf_base_dq_data %>%
  fsubset(VeteranStatus == 1 & YearEnteredService > year(today())) %>%
  merge_check_info_dt(checkIDs = 16) %>%
  fsubset(!is.na(Issue)) %>%
  fselect(vars_we_want)

veteran_missing_year_separated <- ssvf_base_dq_data %>%
  fsubset(VeteranStatus == 1 & is.na(YearSeparated)) %>%
  merge_check_info_dt(checkIDs = 17) %>%
  fsubset(!is.na(Issue)) %>%
  fselect(vars_we_want)

veteran_incorrect_year_separated <- ssvf_base_dq_data %>%
  fsubset(VeteranStatus == 1 & YearSeparated > year(today())) %>%
  merge_check_info_dt(checkIDs = 18) %>%
  fsubset(!is.na(Issue)) %>%
  fselect(vars_we_want)

veteran_missing_wars <- ssvf_base_dq_data %>%
  fsubset(
    VeteranStatus == 1 &
      (
        is.na(WorldWarII) |
          is.na(KoreanWar) |
          is.na(VietnamWar) |
          is.na(DesertStorm) |
          is.na(AfghanistanOEF) |
          is.na(IraqOIF) |
          is.na(IraqOND) |
          is.na(OtherTheater)
      )
  ) %>%
  merge_check_info_dt(checkIDs = 19) %>%
  fselect(vars_we_want)

veteran_missing_branch <- ssvf_base_dq_data %>%
  fsubset(VeteranStatus == 1 & is.na(MilitaryBranch)) %>%
  merge_check_info_dt(checkIDs = 20) %>%
  fselect(vars_we_want)

veteran_missing_discharge_status <- ssvf_base_dq_data %>%
  fsubset(VeteranStatus == 1 & is.na(DischargeStatus)) %>%
  merge_check_info_dt(checkIDs = 21) %>%
  fselect(vars_we_want)

ssvf_missing_percent_ami <- ssvf_base_dq_data %>%
  fsubset(RelationshipToHoH == 1 &
           is.na(PercentAMI)) %>%
  merge_check_info_dt(checkIDs = 22) %>%
  fselect(vars_we_want)

ssvf_missing_vamc <- ssvf_base_dq_data %>%
  fsubset(RelationshipToHoH == 1 &
           is.na(VAMCStation)) %>%
  merge_check_info_dt(checkIDs = 23) %>%
  fselect(vars_we_want)

ssvf_hp_screen <- ssvf_base_dq_data %>%
  fsubset(ProjectType == 12 &
           RelationshipToHoH == 1 &
           TargetScreenReqd == 1 &
           (is.na(HPScreeningScore) |
              is.na(ThresholdScore))) %>%
  merge_check_info_dt(checkIDs = 25) %>%
  fselect(vars_we_want)

dkr_client_veteran_info <- ssvf_base_dq_data %>%
  fsubset(VeteranStatus == 1)

dkr_client_veteran_discharge <- dkr_client_veteran_info %>%
  fsubset(DischargeStatus %in% c(dkr_dnc)) %>%
  merge_check_info_dt(checkIDs = 56) %>%
  fselect(vars_we_want)

dkr_client_veteran_wars <- dkr_client_veteran_info %>%
  fsubset(WorldWarII %in% c(dkr_dnc) |
        KoreanWar %in% c(dkr_dnc) |
        VietnamWar %in% c(dkr_dnc) |
        DesertStorm  %in% c(dkr_dnc) |
        AfghanistanOEF %in% c(dkr_dnc) |
        IraqOIF %in% c(dkr_dnc) |
        IraqOND %in% c(dkr_dnc) |
        OtherTheater  %in% c(dkr_dnc)
  ) %>%
  merge_check_info_dt(checkIDs = 57) %>%
  fselect(vars_we_want)

dkr_client_veteran_military_branch <- dkr_client_veteran_info %>%
  fsubset(MilitaryBranch %in% c(dkr_dnc)) %>%
  merge_check_info_dt(checkIDs = 58) %>%
  fselect(vars_we_want)
# Long Stayers -------------------------------------------------------------
# The goal is here to flag "stays" that go beyond the local setting 
# (that defines a "long" stay), and is set by the user
# A "stay" is the time between when we last "heard" from an enrollment and Export Date
# How we determine the last time we heard from an enrollment differs by Project Type

# Non-Residential Long Stayers --------------------------------------------
calculate_long_stayers_local_settings_dt <- function(projecttype){
  # get non-exited enrollments for projecttype
  logToConsole(session, glue::glue("In calculate long stayers: projecttype = {projecttype}"))
  non_exits <- session$userData$validation %>%
    fsubset(ProjectType == projecttype & 
              (ExitDate >= session$userData$meta_HUDCSV_Export_End | is.na(ExitDate))
    ) %>%
    fselect(vars_prep)
  
  # only proceed if there are any non-exited enrollments
  if(nrow(non_exits) == 0) return(NULL)
  logToConsole(session, "Has non-exits")
  
  # data with last-known dates
  # we're going to later compute the LAST Known Date to determine when we last heard from them
  # this starts the clock of how long their stay is.
  data_w_dates <- if(projecttype %in% c(out_project_type, sso_project_type, ce_project_type)) {
    # This will be merged back into non_exits
    CurrentLivingSituation %>% fselect(EnrollmentID, KnownDate = InformationDate)
  } else if(projecttype == es_nbn_project_type) {
    # This will be merged back into non_exits
    Services %>% fselect(EnrollmentID, KnownDate = DateProvided)
  } else {
    # If a different project type, we'll just use their EntryDate as the KnownDate
    non_exits
  }
  
  # calculate last-known date (differs by project type)
  non_exits_w_lastknown_date <- if(projecttype %in% c(other_project_project_type, day_project_type)) {
    # LastKnown = KnownDate (not fmax) because it's per enrollment, and EntryDate (now KnownDate) is at Enrollment level
    data_w_dates %>%
      fmutate(LastKnown = EntryDate)
  } else {
    join(non_exits, data_w_dates, on = "EnrollmentID", how="left", multiple=TRUE) %>%
      fgroup_by(EnrollmentID) %>%
      # Take EntryDate if there's no Information or DateProvided
      fmutate(LastKnown = fcoalesce(fmax(KnownDate), EntryDate)) %>%
      funique(cols = c("EnrollmentID", "LastKnown")) %>%
      fselect(-KnownDate)
  }
  
  # calculate days since last known
  return(
    qDT(non_exits_w_lastknown_date) %>%
      fmutate(
        DaysSinceLastKnown = as.numeric(difftime(
          as.Date(session$userData$meta_HUDCSV_Export_Date), LastKnown, units = "days"
        ))
      ) %>%
      merge_check_info_dt(
        fcase(
          projecttype %in% c(out_project_type, sso_project_type, ce_project_type), 103,
          projecttype == es_nbn_project_type, 142,
          projecttype %in% c(other_project_project_type, day_project_type), 102
        )
      )
  )
}

## ES NbN --------------------
ESNbN <- calculate_long_stayers_local_settings_dt(es_nbn_project_type) #1

## Non-Residential Projects (other than HP projects) --------
Outreach <- calculate_long_stayers_local_settings_dt(out_project_type) #4
ServicesOnly <- calculate_long_stayers_local_settings_dt(sso_project_type) #6
Other <- calculate_long_stayers_local_settings_dt(other_project_project_type) #7
DayShelter <- calculate_long_stayers_local_settings_dt(day_project_type) #11
CoordinatedEntry <- calculate_long_stayers_local_settings_dt(ce_project_type) #14

long_stayers <- rowbind(
  list(
    Outreach,
    ServicesOnly,
    Other,
    DayShelter,
    CoordinatedEntry
  )
)

# Outstanding Referrals --------------------------------------------
calculate_outstanding_referrals <- function(dq_data){
  if(is.null(dq_data)) return(NULL)
  logToConsole(session, paste0("in calculate_outstanding_referrals"))
  
  dq_data %>%
    join(Event,
              on = "EnrollmentID", how = 'left', multiple=TRUE) %>%
    fselect(funique(c(vars_prep, 'ProjectID', 'EventID', 'EventDate', 'ResultDate', 'Event'))) %>%
    fmutate(
      Days = 
        as.numeric(
          difftime(as.Date(session$userData$meta_HUDCSV_Export_Date), EventDate, units = "days")),
      EventType = fcase(
        Event == 10, "Referral to Emergency Shelter bed opening",
        Event == 11, "Referral to Transitional Housing bed/unit opening",
        Event == 12, "Referral to Joint TH-RRH project/unit/resource opening",
        Event == 13, "Referral to RRH project resource opening",
        Event == 14, "Referral to PSH project resource opening",
        Event == 15, "Referral to Other PH project/unit/resource opening",
        Event == 17, "Referral to Emergency Housing Voucher (EHV)",
        Event == 18, "Referral to a Housing Stability Voucher"
      )
    ) %>%
    fsubset(Event %in% c(10:15, 17:18) &
             is.na(ResultDate))
    # we don't select vars_we_want here because 
    # this gets used in DQ export, where we need all variables
}
## CE ------
outstanding_referrals <- calculate_outstanding_referrals(base_dq_data)

# All together now --------------------------------------------------------
dq_main <- as.data.table(rbind(
  approx_start_after_entry,
  approx_start_v_living_situation_data,
  conflicting_health_insurance_entry,
  conflicting_health_insurance_exit,
  conflicting_income_entry,
  conflicting_income_exit,
  conflicting_ncbs_entry,
  dkr_client_veteran_discharge,
  dkr_client_veteran_military_branch,
  dkr_client_veteran_wars,
  dkr_destination,
  dkr_dob,
  dkr_living_situation,
  dkr_LoS,
  dkr_months_times_homeless,
  dkr_name,
  dkr_race,
  dkr_residence_prior,
  dkr_ssn,
  dkr_veteran,
  overlap_dt,
  duplicate_ees,
  enrollment_after_operating_period,
  enrollment_after_participating_period,
  enrollment_before_operating_period,
  enrollment_before_participating_period,
  enrollment_x_operating_end,
  enrollment_x_operating_period,
  enrollment_x_operating_start,
  enrollment_x_participating_end,
  enrollment_x_participating_period,
  enrollment_x_participating_start,
  exit_before_start,
  future_ees,
  future_exits,
  hh_issues,
  incorrect_dob,
  invalid_movein_date,
  missing_approx_date_homeless,
  missing_cls_subsidy,
  # missing_destination,
  missing_destination_subsidy,
  dkr_disabilities,
  missing_bn_entry,
  bn_on_exit,
  missing_dob,
  # missing_dob_dataquality,
  missing_enrollment_coc,
  missing_health_insurance_entry,
  missing_health_insurance_exit,
  missing_income_entry,
  missing_income_exit,
  missing_living_situation,
  missing_LoS,
  missing_months_times_homeless,
  # missing_name_dataquality,
  missing_ncbs_entry,
  missing_previous_street_ESSH,
  missing_residence_prior,
  missing_res_prior_subsidy,
  # missing_ssn,
  # missing_veteran_status,
  no_months_can_be_determined,
  no_months_v_living_situation_data,
  ssvf_hp_screen,
  ssvf_missing_percent_ami,
  ssvf_missing_vamc,
  Top2_movein,
  top_percents_long_stayers,
  veteran_incorrect_year_entered,
  veteran_incorrect_year_separated,
  veteran_missing_branch,
  veteran_missing_discharge_status,
  veteran_missing_wars,
  veteran_missing_year_entered,
  veteran_missing_year_separated
))

dq_main <- dq_main %>% 
  fmutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning"))) %>% 
  funique() #%>% 
  #qDF()
