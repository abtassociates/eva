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

logToConsole("Running Data Quality")

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
  left_join(Client %>%
              select(-DateCreated), by = "PersonalID") %>%
  left_join(ProjectSegments %>% select(ProjectTimeID, ProjectName, OrganizationName),
            by = "ProjectTimeID") %>%
  select(
    all_of(vars_prep),
    FirstName,
    NameDataQuality,
    SSN,
    SSNDataQuality,
    DOB,
    DOBDataQuality,
    AgeAtEntry,
    RaceNone,
    AmIndAKNative,
    Asian,
    BlackAfAmerican,
    NativeHIPacific,
    White,
    MidEastNAfrican,
    HispanicLatinaeo,
    VeteranStatus,
    ProjectTimeID,
    EnrollmentCoC,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    AgeAtEntry,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    DateOfEngagement,
    MoveInDate,
    Destination,
    DestinationSubsidyType,
    ExitAdjust,
    DateCreated,
    HouseholdType
  )

DV <- HealthAndDV %>%
  filter(DataCollectionStage == 1) %>%
  select(EnrollmentID, DomesticViolenceSurvivor, WhenOccurred, CurrentlyFleeing)

base_dq_data <- base_dq_data %>%
  left_join(DV, by = "EnrollmentID")

rm(DV)

# Duplicate EEs -----------------------------------------------------------

duplicate_ees <-
  get_dupes(base_dq_data, PersonalID, ProjectID, EntryDate) %>%
  merge_check_info(checkIDs = 1) %>%
  select(all_of(vars_we_want))

# Missing UDEs ------------------------------------------------------------

# missing_name_dataquality <- base_dq_data %>%
#   filter(is.na(NameDataQuality)) %>%
#   merge_check_info(checkIDs = 33) %>%
#   select(all_of(vars_we_want))

dkr_name <- base_dq_data %>%
  filter(NameDataQuality %in% c(dkr_dnc, 2)) %>%
  merge_check_info(checkIDs = 78) %>%
  select(all_of(vars_we_want))

missing_dob <- base_dq_data %>%
  filter(is.na(DOB) & DOBDataQuality %in% c(1, 2)) %>%
  merge_check_info(checkIDs = 34) %>%
  select(all_of(vars_we_want))

# missing_dob_dataquality <- base_dq_data %>%
#   filter(is.na(DOBDataQuality)) %>%
#   merge_check_info(checkIDs = 35) %>%
#   select(all_of(vars_we_want))

dkr_dob <- base_dq_data %>%
  filter(DOBDataQuality %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 60) %>%
  select(all_of(vars_we_want))

incorrect_dob <- base_dq_data %>%
  filter(AgeAtEntry < 0 | AgeAtEntry > 100) %>%
  merge_check_info(checkIDs = 84) %>%
  select(all_of(vars_we_want))

# missing_ssn <- base_dq_data %>%
#   filter((is.na(SSN) & !SSNDataQuality %in% c(dkr_dnc))) %>%
#   merge_check_info(checkIDs = 85) %>%
#   select(all_of(vars_we_want))

dkr_ssn <- base_dq_data %>%
  filter(SSNDataQuality %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 67) %>%
  select(all_of(vars_we_want))

dkr_race <- base_dq_data %>%
  filter(RaceNone %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 63) %>%
  select(all_of(vars_we_want))

# missing_veteran_status <- base_dq_data %>%
#   filter(
#     (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
#     (is.na(VeteranStatus))
#   ) %>%
#   merge_check_info(checkIDs = 39) %>%
#   select(all_of(vars_we_want))

dkr_veteran <- base_dq_data %>%
  filter(
    (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
    VeteranStatus %in% c(dkr_dnc)
  ) %>%
  merge_check_info(checkIDs = 66) %>%
  select(all_of(vars_we_want))

# Missing Client Location -------------------------------------------------

missing_enrollment_coc <- base_dq_data %>%
  filter(is.na(EnrollmentCoC) & RelationshipToHoH == 1) %>%
  merge_check_info(checkIDs = 27) %>%
  select(all_of(vars_we_want))

# Household Issues --------------------------------------------------------

hh_children_only <- base_dq_data %>%
  group_by(HouseholdID) %>%
  summarise(
    hhMembers = n(),
    maxAge = max(AgeAtEntry),
  ) %>%
  filter(maxAge < 12) %>%
  ungroup() %>%
  left_join(base_dq_data, by = c("HouseholdID", "maxAge" = "AgeAtEntry")) %>%
  distinct(HouseholdID, maxAge, .keep_all = TRUE) %>%
  merge_check_info(checkIDs = 86) %>%
  select(all_of(vars_we_want))

# hh_no_hoh <- base_dq_data %>%
#   group_by(HouseholdID) %>%
#   summarise(hasHoH = if_else(min(RelationshipToHoH) != 1,
#                              FALSE,
#                              TRUE),
#             PersonalID = min(PersonalID)) %>%
#   filter(hasHoH == FALSE) %>%
#   ungroup() %>%
#   left_join(base_dq_data, by = c("PersonalID", "HouseholdID")) %>%
#   merge_check_info(checkIDs = 2) %>%
#   select(all_of(vars_we_want))

base_dq_data_dt <- as.data.table(base_dq_data)

hh_no_hoh_dt <- base_dq_data_dt[, .(hasHoH = ifelse(min(RelationshipToHoH) != 1, FALSE, TRUE),
                                    PersonalID = min(PersonalID)),
                                by = HouseholdID]
hh_no_hoh_dt <- hh_no_hoh_dt[hasHoH == FALSE]

hh_no_hoh <- as.data.frame(
  base_dq_data_dt[hh_no_hoh_dt, on = .(PersonalID, HouseholdID)]
) %>% 
  merge_check_info(checkIDs = 2) %>%
  select(all_of(vars_we_want))


hh_too_many_hohs <- base_dq_data %>%
  filter(RelationshipToHoH == 1) %>%
  group_by(HouseholdID) %>%
  summarise(HoHsinHousehold = n(),
            PersonalID = min(PersonalID)) %>%
  ungroup() %>%
  filter(HoHsinHousehold > 1) %>%
  left_join(base_dq_data, by = c("PersonalID", "HouseholdID")) %>%
  merge_check_info(checkIDs = 3) %>%
  select(all_of(vars_we_want))

hh_missing_rel_to_hoh <- base_dq_data %>%
  filter(RelationshipToHoH == 99) %>%
  anti_join(hh_no_hoh["HouseholdID"], by = "HouseholdID") %>%
  merge_check_info(checkIDs = 4) %>%
  select(all_of(vars_we_want))

hh_issues <- 
  rbind(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

rm(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

missing_approx_date_homeless <- base_dq_data %>%
  select(
    all_of(vars_prep),
    ProjectID,
    AgeAtEntry,
    RelationshipToHoH,
    LOSUnderThreshold,
    DateToStreetESSH,
    PreviousStreetESSH
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           is.na(DateToStreetESSH) &
           LOSUnderThreshold == 1 &
           PreviousStreetESSH == 1
  ) %>%
  merge_check_info(checkIDs = 28) %>%
  select(all_of(vars_we_want))

missing_previous_street_ESSH <- base_dq_data %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    DateToStreetESSH,
    PreviousStreetESSH,
    LOSUnderThreshold
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           is.na(PreviousStreetESSH) &
           LOSUnderThreshold == 1
  ) %>%
  merge_check_info(checkIDs = 29) %>%
  select(all_of(vars_we_want))

missing_residence_prior <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LivingSituation) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LivingSituation))) %>%
  merge_check_info(checkIDs = 30) %>%
  select(all_of(vars_we_want))

dkr_residence_prior <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LivingSituation) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LivingSituation %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 64) %>%
  select(all_of(vars_we_want))

missing_LoS <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LengthOfStay) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LengthOfStay))) %>%
  merge_check_info(checkIDs = 26) %>%
  select(all_of(vars_we_want))

dkr_LoS <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LengthOfStay) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LengthOfStay %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 73) %>%
  select(all_of(vars_we_want))

missing_months_times_homeless <- base_dq_data %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
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
  merge_check_info(checkIDs = 31) %>%
  select(all_of(vars_we_want))

dkr_months_times_homeless <- base_dq_data %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           (
             MonthsHomelessPastThreeYears %in% c(dkr_dnc) |
               TimesHomelessPastThreeYears %in% c(dkr_dnc)
           )
  ) %>%
  merge_check_info(checkIDs = 61) %>%
  select(all_of(vars_we_want))

invalid_months_times_homeless <- base_dq_data %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears,
    DateToStreetESSH
  ) %>%
  filter(ProjectType != 12 &
           (RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required)

approx_start_after_entry <- invalid_months_times_homeless %>%
  filter(!is.na(DateToStreetESSH) &
           EntryDate < DateToStreetESSH) %>%
  merge_check_info(checkIDs = 69) %>%
  select(all_of(vars_we_want))

no_months_can_be_determined <- invalid_months_times_homeless %>%
  filter(MonthsHomelessPastThreeYears %in% c(dkr_dnc) &
           TimesHomelessPastThreeYears == 1) %>%
  merge_check_info(checkIDs = 70) %>%
  select(all_of(vars_we_want))

no_months_v_living_situation_data <-
  invalid_months_times_homeless %>%
  mutate(
    MonthHomelessnessBegan = floor_date(DateToStreetESSH, "month"),
    MonthEnteredProgram = floor_date(EntryDate, "month"),
    MonthDiff =
      interval(MonthHomelessnessBegan, MonthEnteredProgram) %/% months(1) + 1,
    MonthDiff = if_else(MonthDiff >= 13, 13, MonthDiff),
    DateMonthsMismatch = if_else(
      MonthsHomelessPastThreeYears - MonthDiff != 100 &
        TimesHomelessPastThreeYears == 1,
      1,
      0
    )
  ) %>%
  filter(TimesHomelessPastThreeYears == 1 &
           !is.na(DateToStreetESSH) &
           DateMonthsMismatch == 1) %>%
  merge_check_info(checkIDs = 71) %>%
  select(all_of(vars_we_want))

approx_start_v_living_situation_data <-
  invalid_months_times_homeless %>%
  mutate(
    HomelessOver3YearsAgo = !is.na(DateToStreetESSH) &
      ymd(DateToStreetESSH) <= ymd(EntryDate) %m-% months(36),
    SomethingsNotRight = TimesHomelessPastThreeYears != 1 |
      MonthsHomelessPastThreeYears < 112
  ) %>%
  filter(HomelessOver3YearsAgo == TRUE & SomethingsNotRight == TRUE) %>%
  merge_check_info(checkIDs = 105) %>%
  select(all_of(vars_we_want))

rm(invalid_months_times_homeless)

missing_living_situation <- base_dq_data %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           # not req'd prior to this
           ProjectType %in% c(
             th_project_type,
             psh_project_types,
             sso_project_type,
             hp_project_type,
             rrh_project_type) &
           (
             (
               LivingSituation %in% institutional_livingsituation &
                 LengthOfStay %in% c(2, 3, 10, 11) & # <= 30 days
                 (is.na(LOSUnderThreshold) |
                    is.na(PreviousStreetESSH))
             ) |
               (
                 LivingSituation %in% perm_livingsituation &
                   LengthOfStay %in% c(10, 11) & # <= 7 days
                   (is.na(LOSUnderThreshold) |
                      is.na(PreviousStreetESSH))
               )
           )
  ) %>%
  merge_check_info(checkIDs = 41) %>%
  select(all_of(vars_we_want))

dkr_living_situation <- base_dq_data %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate > hc_prior_living_situation_required &
           (
             MonthsHomelessPastThreeYears %in% c(dkr_dnc) |
               TimesHomelessPastThreeYears %in% c(dkr_dnc) |
               LivingSituation %in% c(dkr_dnc)
           )
  ) %>%
  merge_check_info(checkIDs = 68) %>%
  select(all_of(vars_we_want))

# DisablingCondition at Entry
dkr_disabilities <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         DisablingCondition) %>%
  filter(DisablingCondition %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 32) %>%
  select(all_of(vars_we_want))

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
  select(all_of(vars_prep)) %>%
  filter(
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
  mutate(Days = as.numeric(difftime(
      meta_HUDCSV_Export_Date(), 
      if_else(ProjectType %in% c(ph_project_types), MoveInDateAdjust, EntryDate)
  ))) %>%
  group_by(ProjectType) %>%
  arrange(desc(Days)) %>%
  filter(Days > quantile(Days, if_else(
    ProjectType %in% c(long_stayer_98_percentile_project_types), .98, .99
  ))) %>%
  ungroup() %>% 
  merge_check_info(checkIDs = 104) %>%
  select(all_of(vars_we_want))

# long stayers flags that come from inputs come from calculate_long_stayers()

# Possible Missing HMID ---------------------------------------------------

missed_movein_stayers <- base_dq_data %>%
  select(RelationshipToHoH, all_of(vars_prep)) %>%
  filter(is.na(ExitDate) &
           is.na(MoveInDateAdjust) &
           ProjectType %in% c(ph_project_types) & 
           RelationshipToHoH == 1
  ) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_Date(), EntryDate)))

Top2_movein <- subset(missed_movein_stayers,
                      Days > quantile(Days, prob = 1 - 2 / 100, na.rm = TRUE)) %>%
  select(all_of(vars_prep)) %>%
  merge_check_info(checkIDs = 72) %>%
  select(all_of(vars_we_want))

# Project Exit Before Start --------------
exit_before_start <- base_dq_data %>%
  filter(ExitDate < EntryDate & !is.null(ExitDate) & !is.null(EntryDate)) %>% 
  merge_check_info(checkIDs = 99) %>%
  select(all_of(vars_we_want))

# Missing Destination -----------------------------------------------------

# missing_destination <- base_dq_data %>%
#   filter(!is.na(ExitDate) &
#            (is.na(Destination))) %>%
#   merge_check_info(checkIDs = 74) %>%
#   select(all_of(vars_we_want))

dkr_destination <- base_dq_data %>%
  filter(Destination %in% c(dkr_dnc, 30)) %>%
  merge_check_info(checkIDs = 59) %>%
  select(all_of(vars_we_want))

missing_destination_subsidy <- base_dq_data %>%
  filter(!is.na(ExitDate) &
           Destination == 435 &
           (is.na(DestinationSubsidyType) |
           !DestinationSubsidyType %in% c(subsidy_types))) %>%
  merge_check_info(checkIDs = 121) %>%
  select(all_of(vars_we_want))

# Missing ResPrior Subsidy ------------------------------------------------

missing_res_prior_subsidy <- base_dq_data %>%
  left_join(Enrollment %>% select(EnrollmentID, RentalSubsidyType),
            join_by(EnrollmentID)) %>%
  filter(LivingSituation == 435 &
           (is.na(RentalSubsidyType) |
           !RentalSubsidyType %in% c(subsidy_types))) %>%
  merge_check_info(checkIDs = 130) %>%
  select(all_of(vars_we_want))


# Missing CLS Subsidy -----------------------------------------------------

missing_cls_subsidy <- base_dq_data %>%
  inner_join(CurrentLivingSituation %>%
              filter(CurrentLivingSituation == 435 &
                       (is.na(CLSSubsidyType) |
                       !CLSSubsidyType %in% c(subsidy_types))) %>%
              select(CurrentLivingSitID, EnrollmentID, CLSSubsidyType),
            join_by(EnrollmentID)) %>%
  merge_check_info(checkIDs = 129) %>%
  select(all_of(vars_we_want))

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
  filter(EntryDate > DateCreated &
           (!ProjectType %in% psh_project_types |
              (ProjectType %in% psh_project_types & 
                  EntryDate >= hc_psh_started_collecting_move_in_date
              )))  %>%
  merge_check_info(checkIDs = 75) %>%
  select(all_of(vars_we_want))

future_exits <- base_dq_data %>%
  filter(!is.na(ExitDate) &
           ExitDate > as.Date(meta_HUDCSV_Export_Date())) %>%
  merge_check_info(checkIDs = 14) %>%
  select(all_of(vars_we_want))
    
# Missing Income at Entry -------------------------------------------------

projects_require_income <- projects_funders_types %>% filter(inc == 1) %>%
  pull(ProjectID)

missing_income_entry <- base_dq_data %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    DataCollectionStage,
    TotalMonthlyIncome,
    IncomeFromAnySource
  ) %>%
  filter(DataCollectionStage == 1 &
           ProjectID %in% c(projects_require_income) &
           (AgeAtEntry > 17 |
              is.na(AgeAtEntry)) &
           (is.na(IncomeFromAnySource))) %>%
  merge_check_info(checkIDs = 87) %>%
  select(all_of(vars_we_want))

# if IncomeFromAnySource is yes then one of these should be a yes, and if it's a 
# no, then all of them should be no
smallIncome <- IncomeBenefits %>%
  select(
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
  filter(DataCollectionStage %in% c(1, 3))

smallIncome[is.na(smallIncome)] <- 0

smallIncome <- smallIncome %>% 
  unique() %>%
  full_join(
    IncomeBenefits[c(
      "PersonalID",
      "EnrollmentID",
      "DataCollectionStage",
      "TotalMonthlyIncome",
      "IncomeFromAnySource"
    )] %>%
    unique(),
    by = c("PersonalID",
         "EnrollmentID",
         "DataCollectionStage"),
    relationship = "many-to-many")

income_subs <- base_dq_data[c("AgeAtEntry", vars_prep)] %>%
  left_join(smallIncome, by = c("PersonalID", "EnrollmentID")) %>%
  mutate(
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
  filter(DataCollectionStage == 1 &
           ProjectID %in% c(projects_require_income) &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) & # revisit
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>%
  merge_check_info(checkIDs = 88) %>%
  select(all_of(vars_we_want))

# Missing Income at Exit --------------------------------------------------
missing_income_exit <- base_dq_data %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    DataCollectionStage,
    TotalMonthlyIncome,
    IncomeFromAnySource
  ) %>%
  filter(DataCollectionStage == 3 &
           ProjectID %in% c(projects_require_income) &
           (AgeAtEntry > 17 |
              is.na(AgeAtEntry)) &
           (is.na(IncomeFromAnySource))) %>%
  merge_check_info(checkIDs = 89) %>%
  select(all_of(vars_we_want))

conflicting_income_exit <- income_subs %>%
  filter(DataCollectionStage == 3 &
           ProjectID %in% c(projects_require_income) &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>%
  merge_check_info(checkIDs = 90) %>%
  select(all_of(vars_we_want))

rm(income_subs)

# Enrollment Active Outside Participating Dates ---------------------------

enrollment_positions <- EnrollmentAdjust %>%
  select(EnrollmentID, EnrollmentvOperating, EnrollmentvParticipating) %>%
  left_join(base_dq_data, by = c("EnrollmentID"))

enrollment_after_participating_period <- enrollment_positions %>%
  filter(EnrollmentvParticipating == "Enrollment After Participating Period") %>%
  merge_check_info(checkIDs = 111) %>%
  select(all_of(vars_we_want))

enrollment_x_participating_start <- enrollment_positions %>%
  filter(EnrollmentvParticipating == "Enrollment Crosses Participating Start") %>%
  merge_check_info(checkIDs = 112) %>%
  select(all_of(vars_we_want))

enrollment_before_participating_period <- enrollment_positions %>%
  filter(EnrollmentvParticipating == "Enrollment Before Participating Period") %>%
  merge_check_info(checkIDs = 113) %>%
  select(all_of(vars_we_want))

enrollment_x_participating_end <- enrollment_positions %>%
  filter(EnrollmentvParticipating == "Enrollment Crosses Participating End") %>%
  merge_check_info(checkIDs = 114) %>%
  select(all_of(vars_we_want))

enrollment_x_participating_period <- enrollment_positions %>%
  filter(EnrollmentvParticipating == "Enrollment Crosses Participation Period") %>%
  merge_check_info(checkIDs = 115) %>%
  select(all_of(vars_we_want))

# Enrollment v Operating --------------------------------------------------

enrollment_after_operating_period <- enrollment_positions %>%
  filter(EnrollmentvOperating == "Enrollment After Operating Period") %>%
  merge_check_info(checkIDs = 116) %>%
  select(all_of(vars_we_want))

enrollment_x_operating_start <- enrollment_positions %>%
  filter(EnrollmentvOperating == "Enrollment Crosses Operating Start") %>%
  merge_check_info(checkIDs = 117) %>%
  select(all_of(vars_we_want))

enrollment_before_operating_period <- enrollment_positions %>%
  filter(EnrollmentvOperating == "Enrollment Before Operating Period") %>%
  merge_check_info(checkIDs = 118) %>%
  select(all_of(vars_we_want))

enrollment_x_operating_end <- enrollment_positions %>%
  filter(EnrollmentvOperating == "Enrollment Crosses Operating End") %>%
  merge_check_info(checkIDs = 119) %>%
  select(all_of(vars_we_want))

enrollment_x_operating_period <- enrollment_positions %>%
  filter(EnrollmentvOperating == "Enrollment Crosses Operating Period") %>%
  merge_check_info(checkIDs = 120) %>%
  select(all_of(vars_we_want))

# Overlaps ----------------------------------------------------------------
# Create an initial dataset of possible overlaps
# and establish initial Enrollment intervals, based on project type
base_dq_data_dt <- as.data.table(base_dq_data)
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
  services_dt <- as.data.table(Services)
  services_summary <- services_dt[
    , .(
      FirstDateProvided = min(DateProvided, na.rm = TRUE),
      LastDateProvided = max(DateProvided, na.rm = TRUE)
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
  
  overlap_staging[, `:=`(
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
  )]
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
       PreviousProjectType %in% psh_project_types) |
      (PreviousProjectType == rrh_project_type &
         ProjectType %in% psh_project_types)
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
  overlap_nbns <- services_dt[, `:=`(
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

overlap_dt <- merge_check_info_dt(overlap_dt, 77)[,
  Issue := paste(
    "Overlap with",
    ifelse(str_sub(PreviousProjectType, 1, 1) %in% c("A", "E", "I", "O", "U"), "an", "a"),
    project_type(PreviousProjectType),
    "project"
  )
][, ..cols_to_keep]

# Bring in additional enrollment details used to contextualize the flagged enrollment
# e.g. EntryDate, ExitAdjust, etc.
overlap_dt <- merge(
  overlap_dt,
  base_dq_data_dt[, c(vars_prep, "HouseholdType"), with = FALSE], 
  by = "EnrollmentID"
)[
  # Recode ProjectType to a more readable version
  , ProjectType := project_type(ProjectType)
]

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

overlap_details(
  # Rename columns for previous enrollment
  merge(
    overlap_dt,
    base_dq_data_dt[
      , setNames(.SD, paste0("Previous", names(.SD)))
      , .SDcols = c(vars_prep, "HouseholdType")
    ],
    by = "PreviousEnrollmentID",
    all.x = TRUE
  )[, `:=`(
      PreviousProjectType = project_type(PreviousProjectType),
      HouseholdType = factor(
        case_when(
          HouseholdType %in% c("PY", "ACminusPY") ~ "AC",
          HouseholdType %in% c("UY", "AOminusUY") ~ "AO",
          TRUE ~ HouseholdType
        ),
        levels = c("AO", "AC", "CO", "UN")
      ),
      PreviousHouseholdType = factor(
        case_when(
          PreviousHouseholdType %in% c("PY", "ACminusPY") ~ "AC",
          PreviousHouseholdType %in% c("UY", "AOminusUY") ~ "AO",
          TRUE ~ PreviousHouseholdType
        ),
        levels = c("AO", "AC", "CO", "UN")
      )
  )][
    # Drop Issue columns
    , !c("Issue", "Type", "Guidance"), with = FALSE
  ][
    # order and rename columns
    , ..col_order
  ]
)

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

overlap_dt[, (cols_to_remove) := NULL]

# convert to data.frame to play nice with other data.tables
overlaps_df <- setDF(overlap_dt[, HouseholdType := NULL])

# Invalid Move-in Date ----------------------------------------------------

invalid_movein_date <- base_dq_data %>%
  filter(ProjectType %in% ph_project_types & 
        ((!is.na(MoveInDate) & MoveInDate < EntryDate) | 
        (!is.na(MoveInDate) & !is.na(ExitDate) & MoveInDate > ExitDate))
  ) %>%
  merge_check_info(checkIDs = 40) %>%
  select(all_of(vars_we_want))

# Missing Health Ins ------------------------------------------------------

projects_require_hi <- projects_funders_types %>% filter(hi == 1) %>%
  pull(ProjectID)

missing_health_insurance <- base_dq_data %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         DataCollectionStage,
         InsuranceFromAnySource) %>%
  filter((is.na(InsuranceFromAnySource)) &
           ProjectID %in% c(projects_require_hi))
  
missing_health_insurance_entry <- missing_health_insurance %>%
  filter(DataCollectionStage == 1) %>%
  merge_check_info(checkIDs = 92) %>%
  select(all_of(vars_we_want))

missing_health_insurance_exit <- missing_health_insurance %>%
  filter(DataCollectionStage == 3) %>%
  merge_check_info(checkIDs = 93) %>%
  select(all_of(vars_we_want))

health_insurance_subs <- base_dq_data %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    all_of(vars_prep),
    DataCollectionStage,
    InsuranceFromAnySource,
    Medicaid,
    Medicare,
    SCHIP,
    VHAServices,
    EmployerProvided,
    COBRA,
    PrivatePay,
    StateHealthIns,
    IndianHealthServices,
    OtherInsurance
  ) %>%
  mutate(
    SourceCount = Medicaid + SCHIP + VHAServices + EmployerProvided +
      COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
      OtherInsurance + Medicare
  ) %>%
  filter((InsuranceFromAnySource == 1 &
            SourceCount == 0) |
           (InsuranceFromAnySource == 0 &
              SourceCount > 0))

conflicting_health_insurance_entry <- health_insurance_subs %>%
  filter(DataCollectionStage == 1 &
           ProjectID %in% c(projects_require_hi)) %>%
  merge_check_info(checkIDs = 94) %>%
  select(all_of(vars_we_want))

conflicting_health_insurance_exit <- health_insurance_subs %>%
  filter(DataCollectionStage == 3 &
           ProjectID %in% c(projects_require_hi)) %>%
  merge_check_info(checkIDs = 95) %>%
  select(all_of(vars_we_want))

rm(health_insurance_subs)

# Missing NCBs at Entry ---------------------------------------------------

projects_require_ncb <- projects_funders_types %>% filter(ncb == 1) %>%
  pull(ProjectID)

#just the different kinds of non-cash benefits, many to an enrollment
ncb_subs <- IncomeBenefits %>%
  select(
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
  unique()

ncb_subs[is.na(ncb_subs)] <- 0

# basic ncb data but adding BenefitsFromAnySource, an ee-level data element
# BenefitsFromAnySource will repeat depending on its EEID & collection stage
ncbs <- ncb_subs %>%
  full_join(IncomeBenefits[c("PersonalID",
                             "EnrollmentID",
                             "DataCollectionStage",
                             "BenefitsFromAnySource")] %>%
              unique(),
            by = c("PersonalID",
                   "EnrollmentID",
                   "DataCollectionStage"),
            relationship = "many-to-many")

# if there are conflicting yes/no records or conflicting subs, this will catch
# any that conflict with each other, which will prompt the user to correct the
# record(s) that's incorrect

ncb_staging <- base_dq_data %>%
  left_join(ncbs, by = c("PersonalID", "EnrollmentID")) %>%
  filter(
    DataCollectionStage == 1 &
      (AgeAtEntry > 17 |
         is.na(AgeAtEntry))
  ) %>%
  mutate(
    BenefitCount = SNAP + WIC + TANFChildCare + TANFTransportation +
      OtherTANF + OtherBenefitsSource
  ) %>%
  select(all_of(vars_prep),
         DataCollectionStage,
         BenefitsFromAnySource,
         BenefitCount) %>%
  unique()

missing_ncbs_entry <- ncb_staging %>%
  filter((is.na(BenefitsFromAnySource)) &
           ProjectID %in% c(projects_require_ncb)
  ) %>%
  merge_check_info(checkIDs = 96) %>%
  select(all_of(vars_we_want))

conflicting_ncbs_entry <- base_dq_data %>%
  left_join(ncb_staging %>%
              select("PersonalID",
                     "EnrollmentID",
                     "DataCollectionStage",
                     "BenefitsFromAnySource",
                     "BenefitCount"),
            by = c("PersonalID",
                   "EnrollmentID")) %>%
  select(AgeAtEntry,
         all_of(vars_prep),
         DataCollectionStage,
         BenefitsFromAnySource,
         BenefitCount) %>%
  filter(DataCollectionStage == 1 &
           ProjectID %in% c(projects_require_ncb) &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((BenefitsFromAnySource == 1 &
               BenefitCount == 0) |
              (BenefitsFromAnySource == 0 &
                 BenefitCount > 0)
           )) %>%
  merge_check_info(checkIDs = 97) %>%
  select(all_of(vars_we_want))
    
# SSVF --------------------------------------------------------------------

ssvf_funded <- Funder %>%
  filter(Funder %in% c(ssvf_fund_sources)) %>%
  pull(ProjectID)

ssvf_base_dq_data <- base_dq_data %>%
  filter(ProjectID %in% c(ssvf_funded)) %>%
  select(all_of(vars_prep)) %>%
  left_join(
    Enrollment %>%
      select(
        EnrollmentID,
        RelationshipToHoH,
        PercentAMI,
        VAMCStation,
        HPScreeningScore,
        ThresholdScore
      ),
    by = "EnrollmentID"
  ) %>%
  left_join(
    Client %>%
      select(
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
    by = "PersonalID"
  )

veteran_missing_year_entered <- ssvf_base_dq_data %>%
  filter(VeteranStatus == 1 & is.na(YearEnteredService)) %>%
  merge_check_info(checkIDs = 15) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

veteran_incorrect_year_entered <- ssvf_base_dq_data %>%
  filter(VeteranStatus == 1 & YearEnteredService > year(today())) %>%
  merge_check_info(checkIDs = 16) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

veteran_missing_year_separated <- ssvf_base_dq_data %>%
  filter(VeteranStatus == 1 & is.na(YearSeparated)) %>%
  merge_check_info(checkIDs = 17) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

veteran_incorrect_year_separated <- ssvf_base_dq_data %>%
  filter(VeteranStatus == 1 & YearSeparated > year(today())) %>%
  merge_check_info(checkIDs = 18) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

veteran_missing_wars <- ssvf_base_dq_data %>%
  filter(
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
  merge_check_info(checkIDs = 19) %>%
  select(all_of(vars_we_want))

veteran_missing_branch <- ssvf_base_dq_data %>%
  filter(VeteranStatus == 1 & is.na(MilitaryBranch)) %>%
  merge_check_info(checkIDs = 20) %>%
  select(all_of(vars_we_want))

veteran_missing_discharge_status <- ssvf_base_dq_data %>%
  filter(VeteranStatus == 1 & is.na(DischargeStatus)) %>%
  merge_check_info(checkIDs = 21) %>%
  select(all_of(vars_we_want))

ssvf_missing_percent_ami <- ssvf_base_dq_data %>%
  filter(RelationshipToHoH == 1 &
           is.na(PercentAMI)) %>%
  merge_check_info(checkIDs = 22) %>%
  select(all_of(vars_we_want))

ssvf_missing_vamc <- ssvf_base_dq_data %>%
  filter(RelationshipToHoH == 1 &
           is.na(VAMCStation)) %>%
  merge_check_info(checkIDs = 23) %>%
  select(all_of(vars_we_want))

ssvf_hp_screen <- ssvf_base_dq_data %>%
  filter(ProjectType == 12 &
           RelationshipToHoH == 1 &
           (is.na(HPScreeningScore) |
              is.na(ThresholdScore))) %>%
  merge_check_info(checkIDs = 25) %>%
  select(all_of(vars_we_want))

dkr_client_veteran_info <- ssvf_base_dq_data %>%
  filter(VeteranStatus == 1)

dkr_client_veteran_discharge <- dkr_client_veteran_info %>%
  filter(DischargeStatus %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 56) %>%
  select(all_of(vars_we_want))

dkr_client_veteran_wars <- dkr_client_veteran_info %>%
  filter(WorldWarII %in% c(dkr_dnc) |
        KoreanWar %in% c(dkr_dnc) |
        VietnamWar %in% c(dkr_dnc) |
        DesertStorm  %in% c(dkr_dnc) |
        AfghanistanOEF %in% c(dkr_dnc) |
        IraqOIF %in% c(dkr_dnc) |
        IraqOND %in% c(dkr_dnc) |
        OtherTheater  %in% c(dkr_dnc)
  ) %>%
  merge_check_info(checkIDs = 57) %>%
  select(all_of(vars_we_want))

dkr_client_veteran_military_branch <- dkr_client_veteran_info %>%
  filter(MilitaryBranch %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 58) %>%
  select(all_of(vars_we_want))

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
      overlaps_df,
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

    dq_main <- unique(dq_main)[, Type := factor(Type,
                                                levels = c("High Priority",
                                                           "Error",
                                                           "Warning"))]
    setDF(dq_main)
    
   dq_providers <- sort(Project0()$ProjectName) 
   
# Plots for System-Level DQ Tab -------------------------------------------
   dq_plot_df <- dq_main %>%
     left_join(Project0() %>%
                 select(ProjectID, OrganizationID), by = "ProjectID") %>%
     select(PersonalID,
            OrganizationID,
            OrganizationName,
            HouseholdID,
            Issue,
            Type) %>%
     unique()

# Prepping dataframes for plots for Organization-Level DQ Tab -----------------
   dq_org_plot_df <- dq_main %>%
     select(PersonalID,
            ProjectID,
            ProjectName,
            OrganizationName,
            HouseholdID,
            Issue,
            Type) %>%
     unique()
   
base_dq_data_func(base_dq_data)
dq_main_df(dq_main)
