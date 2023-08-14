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
  left_join(Project %>% select(ProjectTimeID, ProjectName, OrganizationName),
            by = "ProjectTimeID") %>%
  select(
    PersonalID,
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
    Woman,
    Man,
    NonBinary,
    Transgender,
    CulturallySpecific,
    DifferentIdentity,
    Questioning,
    GenderNone,
    VeteranStatus,
    EnrollmentID,
    ProjectID,
    ProjectTimeID,
    ProjectName,
    ProjectType,
    OrganizationName,
    EnrollmentCoC,
    EntryDate,
    HouseholdID,
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
    MoveInDateAdjust,
    ExitDate,
    Destination,
    DestinationSubsidyType,
    ExitAdjust,
    DateCreated
  )

DV <- HealthAndDV %>%
  filter(DataCollectionStage == 1) %>%
  select(EnrollmentID, DomesticViolenceSurvivor, WhenOccurred, CurrentlyFleeing)

base_dq_data <- base_dq_data %>%
  left_join(DV, by = "EnrollmentID")

rm(DV)

# The Variables That We Want ----------------------------------------------

vars_prep <- c(
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

# Duplicate EEs -----------------------------------------------------------

duplicate_ees <-
  get_dupes(base_dq_data, PersonalID, ProjectID, EntryDate) %>%
  merge_check_info(checkIDs = 1) %>%
  select(all_of(vars_we_want))

# Missing UDEs ------------------------------------------------------------
missing_name_dataquality <- base_dq_data %>%
  filter(NameDataQuality == 99 | is.na(NameDataQuality)) %>%
  merge_check_info(checkIDs = 33) %>%
  select(all_of(vars_we_want))

dkr_name <- base_dq_data %>%
  filter(NameDataQuality %in% c(dkr)) %>%
  merge_check_info(checkIDs = 77) %>%
  select(all_of(vars_we_want))

missing_dob <- base_dq_data %>%
  filter(is.na(DOB) & DOBDataQuality %in% c(1, 2)) %>%
  merge_check_info(checkIDs = 34) %>%
  select(all_of(vars_we_want))

missing_dob_dataquality <- base_dq_data %>%
  filter(is.na(DOBDataQuality)) %>%
  merge_check_info(checkIDs = 35) %>%
  select(all_of(vars_we_want))

dkr_dob <- base_dq_data %>%
  filter(DOBDataQuality %in% c(dkr_dnc)) %>%
  merge_check_info(checkIDs = 60) %>%
  select(all_of(vars_we_want))

incorrect_dob <- base_dq_data %>%
  filter(AgeAtEntry < 0 | AgeAtEntry > 100) %>%
  merge_check_info(checkIDs = 84) %>%
  select(all_of(vars_we_want))

missing_ssn <- base_dq_data %>%
  filter((is.na(SSN) & !SSNDataQuality %in% c(dkr)) |
           is.na(SSNDataQuality) | SSNDataQuality == 99) %>%
  merge_check_info(checkIDs = 85) %>%
  select(all_of(vars_we_want))

dkr_ssn <- base_dq_data %>%
  filter(SSNDataQuality %in% c(dkr)) %>%
  merge_check_info(checkIDs = 67) %>%
  select(all_of(vars_we_want))

dkr_race <- base_dq_data %>%
  filter(RaceNone %in% c(dkr)) %>%
  merge_check_info(checkIDs = 63) %>%
  select(all_of(vars_we_want))

missing_race <- base_dq_data %>%
  filter(RaceNone == 99 |
           AmIndAKNative + Asian + BlackAfAmerican + NativeHIPacific + White == 0) %>%
  merge_check_info(checkIDs = 36) %>%
  select(all_of(vars_we_want))

dkr_gender <- base_dq_data %>%
  filter(GenderNone %in% c(dkr)) %>%
  merge_check_info(checkIDs = 65) %>%
  select(all_of(vars_we_want))

missing_gender <- base_dq_data %>%
  filter(GenderNone == 99 |
           Woman + Man + NonBinary + Transgender + CulturallySpecific +
           DifferentIdentity + Questioning == 0) %>%
  merge_check_info(checkIDs = 38) %>%
  select(all_of(vars_we_want))

missing_veteran_status <- base_dq_data %>%
  filter(
    (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
    (VeteranStatus == 99 | is.na(VeteranStatus))
  ) %>%
  merge_check_info(checkIDs = 39) %>%
  select(all_of(vars_we_want))

dkr_veteran <- base_dq_data %>%
  filter(
    (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
    VeteranStatus %in% c(dkr)
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

hh_no_hoh <- base_dq_data %>%
  group_by(HouseholdID) %>%
  summarise(hasHoH = if_else(min(RelationshipToHoH) != 1,
                             FALSE,
                             TRUE),
            PersonalID = min(PersonalID)) %>%
  filter(hasHoH == FALSE) %>%
  ungroup() %>%
  left_join(base_dq_data, by = c("PersonalID", "HouseholdID")) %>%
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
  merge_check_info(checkIDs = 3)

hh_missing_rel_to_hoh <- base_dq_data %>%
  filter(RelationshipToHoH == 99) %>%
  anti_join(hh_no_hoh["HouseholdID"], by = "HouseholdID") %>%
  merge_check_info(checkIDs = 4) %>%
  select(all_of(vars_we_want))

hh_issues <- rbind(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

rm(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

missing_approx_date_homeless <- base_dq_data %>%
  select(
    all_of(vars_prep),
    EnrollmentID,
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
           (is.na(LivingSituation) | LivingSituation == 99)) %>%
  merge_check_info(checkIDs = 30) %>%
  select(all_of(vars_we_want))

dkr_residence_prior <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LivingSituation) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LivingSituation %in% c(dkr)) %>%
  merge_check_info(checkIDs = 64) %>%
  select(all_of(vars_we_want))

missing_LoS <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LengthOfStay) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
  merge_check_info(checkIDs = 26) %>%
  select(all_of(vars_we_want))

dkr_LoS <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LengthOfStay) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LengthOfStay %in% c(dkr)) %>%
  merge_check_info(checkIDs = 66) %>%
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
               is.na(TimesHomelessPastThreeYears) |
               MonthsHomelessPastThreeYears == 99 |
               TimesHomelessPastThreeYears == 99
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
             MonthsHomelessPastThreeYears %in% c(dkr) |
               TimesHomelessPastThreeYears %in% c(dkr)
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
  filter(
    ProjectType != 12 &
      (RelationshipToHoH == 1 | AgeAtEntry > 17) &
      EntryDate >= hc_prior_living_situation_required &
      TimesHomelessPastThreeYears == 1 &
      !is.na(DateToStreetESSH)
  ) %>% 
  mutate(
    MonthHomelessnessBegan = floor_date(DateToStreetESSH, "month"),
    MonthEnteredProgram = floor_date(EntryDate, "month"),
    MonthDiff = interval(MonthHomelessnessBegan, MonthEnteredProgram) %/% months(1) + 1,
    MonthDiff = if_else(MonthDiff >= 13, 13, MonthDiff),
    DateMonthsMismatch = if_else(MonthsHomelessPastThreeYears - MonthDiff != 100, 1, 0)
  )

invalid_months_times_homeless1 <- invalid_months_times_homeless %>%
  filter(MonthDiff <= 0) %>%
  merge_check_info(checkIDs = 69) %>%
  select(all_of(vars_we_want))

invalid_months_times_homeless2 <- invalid_months_times_homeless %>%
  filter(MonthsHomelessPastThreeYears < 100) %>%
  merge_check_info(checkIDs = 70) %>%
  select(all_of(vars_we_want))

invalid_months_times_homeless3 <- invalid_months_times_homeless %>%
  filter(DateMonthsMismatch == 1) %>%
  merge_check_info(checkIDs = 71) %>%
  select(all_of(vars_we_want))

incomplete_living_situation <- base_dq_data %>%
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
    PersonalID,
    HouseholdID,
    EnrollmentID,
    ProjectID,
    OrganizationName,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
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
             MonthsHomelessPastThreeYears %in% c(dkr) |
               TimesHomelessPastThreeYears %in% c(dkr) |
               LivingSituation %in% c(dkr)
           )
  ) %>%
  merge_check_info(checkIDs = 68) %>%
  select(all_of(vars_we_want))

# DisablingCondition at Entry
missing_disabilities <- base_dq_data %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         DisablingCondition) %>%
  filter(DisablingCondition == 99 |
           is.na(DisablingCondition)) %>%
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

th_stayers <- base_dq_data %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 2) %>%
  mutate(Days = as.numeric(difftime(as.Date(meta_HUDCSV_Export_Date), EntryDate)))
# using Export Date here to reflect the date the export was run on

Top2_TH <- subset(th_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

rrh_stayers <- base_dq_data %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 13) %>%
  mutate(Days = as.numeric(difftime(as.Date(meta_HUDCSV_Export_Date), EntryDate))) 

Top2_RRH <- subset(rrh_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

es_stayers <- base_dq_data %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 1) %>%
  mutate(Days = as.numeric(difftime(as.Date(meta_HUDCSV_Export_Date), EntryDate))) 

Top2_ES <- subset(es_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

psh_stayers <- base_dq_data %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) & ProjectType %in% c(psh_project_types)) %>%
  mutate(Days = as.numeric(difftime(as.Date(meta_HUDCSV_Export_Date), EntryDate))) 

Top1_PSH <- subset(psh_stayers, Days > quantile(Days, prob = 1 - 1 / 100))

hp_stayers <- base_dq_data %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 12) %>%
  mutate(Days = as.numeric(difftime(as.Date(meta_HUDCSV_Export_Date), EntryDate))) 

Top2_HP <- subset(hp_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

ce_stayers <- base_dq_data %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 14) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_End, EntryDate))) 

Top2_CE <- subset(ce_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

missed_movein_stayers <- base_dq_data %>%
  select(all_of(vars_prep)) %>%
  filter(is.na(ExitDate) &
           ProjectType %in% c(ph_project_types)
  ) %>%
  mutate(
    Days = as.numeric(difftime(MoveInDateAdjust, EntryDate))
  )

Top2_movein <- subset(missed_movein_stayers,
                      Days > quantile(Days, prob = 1 - 2 / 100, na.rm = TRUE)) %>%
  select(all_of(vars_prep)) %>%
  merge_check_info(checkIDs = 74) %>%
  select(all_of(vars_we_want))

long_stayers <- rbind(Top1_PSH,
                      Top2_ES,
                      Top2_RRH,
                      Top2_TH,
                      Top2_HP,
                      Top2_CE) %>%
  merge_check_info(checkIDs = 75) %>%
  select(all_of(vars_we_want))

long_stayers <-
  rbind(
    long_stayers,
    Top2_movein
  )

rm(list = ls(pattern = "Top*"),
   es_stayers,
   th_stayers,
   psh_stayers,
   rrh_stayers,
   hp_stayers)

# Project Exit Before Start --------------
exit_before_start <- base_dq_data %>%
  filter(ExitDate < EntryDate & !is.null(ExitDate) & !is.null(EntryDate)) %>% 
  merge_check_info(checkIDs = 99) %>%
  select(all_of(vars_we_want))

# Missing Destination -----------------------------------------------------

missing_destination <- base_dq_data %>%
  filter(!is.na(ExitDate) &
           (is.na(Destination) |
              Destination %in% c(99, 30))) %>%
  merge_check_info(checkIDs = 74) %>%
  select(all_of(vars_we_want))

dkr_destination <- base_dq_data %>%
  filter(Destination %in% c(dkr)) %>%
  merge_check_info(checkIDs = 59) %>%
  select(all_of(vars_we_want))

missing_destination_subsidy <- base_dq_data %>%
  filter(!is.na(ExitDate) &
           Destination == 435 &
           is.na(DestinationSubsidyType)) %>%
  mutate(
    Issue = "Missing Destination Subsidy Type",
    Type = "Error",
    Guidance = str_squish(
      "If a client exits to Rental by Client, the user must record the Subsidy
      Type."
    )
  )

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
  filter(ExitAdjust > as.Date(meta_HUDCSV_Export_Date)) %>%
  merge_check_info(checkIDs = 14) %>%
  select(all_of(vars_we_want))
    
# Missing Income at Entry -------------------------------------------------
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
           (AgeAtEntry > 17 |
              is.na(AgeAtEntry)) &
           (IncomeFromAnySource == 99 |
              is.na(IncomeFromAnySource))) %>%
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
  )

smallIncome[is.na(smallIncome)] <- 0

smallIncome <-
  smallIncome %>% unique() %>%
  full_join(IncomeBenefits[c(
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

income_subs <- base_dq_data[c("EnrollmentID",
                                      "AgeAtEntry",
                                      vars_prep)] %>%
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
           (AgeAtEntry > 17 |
              is.na(AgeAtEntry)) &
           (IncomeFromAnySource == 99 |
              is.na(IncomeFromAnySource))) %>%
  merge_check_info(checkIDs = 89) %>%
  select(all_of(vars_we_want))

conflicting_income_exit <- income_subs %>%
  filter(DataCollectionStage == 3 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>%
  merge_check_info(checkIDs = 90) %>%
  select(all_of(vars_we_want))

rm(income_subs)

# Enrollment Active Outside Operating Dates ------------------------

enrollment_v_operating <- EnrollmentOutside %>%
  filter(!EnrollmentvOperating %in% c("Inside")) %>%
  select(EnrollmentID, EnrollmentvOperating) %>%
  left_join(base_dq_data, by = c("EnrollmentID")) %>%
  mutate(Issue = EnrollmentvOperating,
         Type = "Error",
         Guidance = "replaceme") %>%
  select(all_of(vars_we_want))

# Enrollment Outside of Participating Dates -------------------------------

enrollment_v_participating <- EnrollmentOutside %>%
  filter(!EnrollmentvParticipating %in% c("Inside")) %>%
  select(EnrollmentID, EnrollmentvParticipating) %>%
  left_join(base_dq_data, by = c("EnrollmentID")) %>%
  mutate(Issue = EnrollmentvParticipating,
         Type = "Error",
         Guidance = "replaceme") %>%
  select(all_of(vars_we_want))

# Overlaps ----------------------------------------------------------------

overlap_staging <- base_dq_data %>% 
  select(!!vars_prep, ExitAdjust, EnrollmentID) %>%
  filter(EntryDate != ExitAdjust &
           ((
             ProjectType %in% ph_project_types &
               !is.na(MoveInDateAdjust)
           ) |
             ProjectType %in% lh_residential_project_types
           ))

if(nrow(Services) > 0){
  overlap_staging_nbn <- overlap_staging %>%  
  left_join(
    Services %>% 
      select(EnrollmentID, DateProvided) %>%
      group_by(EnrollmentID) %>%
      slice_min(DateProvided, n = 1L) %>% 
      ungroup() %>%
      unique()
    , by = "EnrollmentID"
  ) %>%
  mutate(
    EnrollmentStart = case_when(
      ProjectType == es_nbn_project_type ~ DateProvided, 
      ProjectType %in% lh_residential_project_types ~ EntryDate,
      ProjectType %in% ph_project_types ~ MoveInDateAdjust,
      TRUE ~ EntryDate
    ),
    EnrollmentEnd = if_else(ProjectType == es_nbn_project_type, 
                            DateProvided, 
                            as.Date(ExitAdjust))
  )
}

overlap_staging_no_nbn <- overlap_staging %>%  
  mutate(
    EnrollmentStart = case_when(
      ProjectType %in% lh_residential_project_types ~ EntryDate,
      ProjectType %in% ph_project_types ~ MoveInDateAdjust,
      TRUE ~ EntryDate
    ),
    EnrollmentEnd = as.Date(ExitAdjust)
  )

if(nrow(Services) > 0){
  overlap_staging <- overlap_staging_nbn %>% 
    select(
      PersonalID,
      EnrollmentID,
      ProjectType,
      EnrollmentStart,
      EnrollmentEnd,
      FirstDateProvided
    )
} else{
  overlap_staging <- overlap_staging_no_nbn %>% 
    mutate(FirstDateProvided = NA) %>%
    select(
      PersonalID,
      EnrollmentID,
      ProjectType,
      EnrollmentStart,
      EnrollmentEnd,
      FirstDateProvided
    )
}

overlaps <- overlap_staging %>%
  # sort enrollments for each person
  group_by(PersonalID) %>%
  arrange(EnrollmentStart, EnrollmentEnd) %>%
  mutate(
    # pull in previous enrollment into current enrollment record so we can 
    # compare intervals
    PreviousEnrollmentID = lag(EnrollmentID)) %>%
  ungroup() %>%
  filter(!is.na(PreviousEnrollmentID)) %>% # 48 secs
  left_join(overlap_staging %>%
              select("PreviousEnrollmentID" = EnrollmentID,
                     "PreviousProjectType" = ProjectType,
                     "PreviousEnrollmentStart" = EnrollmentStart,
                     "PreviousEnrollmentEnd" = EnrollmentEnd,
                     "PreviousFirstDateProvided" = FirstDateProvided
              ),
            by = c("PreviousEnrollmentID")) %>%
  filter(PreviousEnrollmentID != EnrollmentID &
           !(
             (ProjectType == rrh_project_type & 
                PreviousProjectType %in% psh_project_types) |
               (PreviousProjectType == rrh_project_type &
                  ProjectType %in% psh_project_types)
           )) %>% 
  # flag overlaps
  mutate(
    EnrollmentPeriod = interval(EnrollmentStart, EnrollmentEnd),
    PreviousEnrollmentPeriod = 
      interval(PreviousEnrollmentStart, PreviousEnrollmentEnd),
    IsOverlap = int_overlaps(EnrollmentPeriod, PreviousEnrollmentPeriod) & 
      EnrollmentStart != PreviousEnrollmentEnd
  ) %>%
  filter(IsOverlap == TRUE) %>%
  group_by(PersonalID) %>%
  mutate(NumOverlaps = sum(IsOverlap, na.rm = TRUE)) %>%
  ungroup() %>%
  # keep overlaps
  filter(((ProjectType == es_nbn_project_type |
             PreviousProjectType == es_nbn_project_type) & 
            NumOverlaps > 2
  ) |
    (NumOverlaps > 0 &
       !(ProjectType == es_nbn_project_type |
           PreviousProjectType == es_nbn_project_type))) %>% 
  # label issue types
  mutate(
    Type = "Warning",
    Guidance = str_squish(
      "This enrollment overlaps with another enrollment that would indicate a 
      household spent the same night in different inventory beds. Please review
      the HMIS Dual Enrollments and HIC Duplicate Inventory Training Resource for
      more information."
    ),
    # this is the issue that the Project folks will see, and it's the overlap with the Previous project
    Issue = paste("Overlap with", 
                  if_else(str_sub(project_type(PreviousProjectType), 1, 1) %in%
                            c("A", "E", "I", "O", "U"),
                          "an",
                          "a"),  
                  project_type(PreviousProjectType), 
                  "project"),
    # this is the issue that the Previous Project folks will see, and it's the overlap with the main project
    PreviousIssue = paste("Overlap with", 
                  if_else(str_sub(project_type(ProjectType), 1, 1) %in%
                            c("A", "E", "I", "O", "U"),
                          "an",
                          "a"),  
                  project_type(ProjectType), 
                  "project")
  ) %>%
  select(EnrollmentID, PreviousEnrollmentID, Issue, PreviousIssue, Type, Guidance, FirstDateProvided, PreviousFirstDateProvided) %>%
  unique() %>%
  #bring back in the fields they'll need to see (EntryDate, ExitDate, MoveInDate, ProjectName, OrganizationName)
  left_join(base_dq_data %>% 
            select(!!vars_prep, EnrollmentID),
            by = "EnrollmentID") %>%

  left_join(base_dq_data %>% 
            select(!!vars_prep, EnrollmentID) %>%
            setNames(paste0('Previous', names(.))),
            by = "PreviousEnrollmentID") %>%
  mutate(
    ProjectType = project_type(ProjectType),
    PreviousProjectType = project_type(PreviousProjectType)
  )

dq_overlaps1 <- overlaps %>%
  select(!!vars_we_want)

dq_overlaps2 <- overlaps %>%
  select(starts_with("Previous"), Type, Guidance) %>%
  rename_all(~str_replace(.,"^Previous","")) %>%
  select(!!vars_we_want)

# Invalid Move-in Date ----------------------------------------------------

invalid_movein_date <- base_dq_data %>%
  filter(ProjectType %in% ph_project_types & 
        (!is.na(MoveInDate) & MoveInDate < EntryDate) | 
        (!is.na(MoveInDate) & MoveInDate > ExitAdjust)
  ) %>%
  merge_check_info(checkIDs = 40) %>%
  select(all_of(vars_we_want))

# Missing Health Ins ------------------------------------------------------

missing_health_insurance <- base_dq_data %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         DataCollectionStage,
         InsuranceFromAnySource) %>%
  filter(InsuranceFromAnySource == 99 |
              is.na(InsuranceFromAnySource))
  
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
    VHAServicesHA,
    EmployerProvided,
    COBRA,
    PrivatePay,
    StateHealthIns,
    IndianHealthServices,
    OtherInsurance
  ) %>%
  mutate(
    SourceCount = Medicaid + SCHIP + VHAServicesHA + EmployerProvided +
      COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
      OtherInsurance + Medicare
  ) %>%
  filter((InsuranceFromAnySource == 1 &
            SourceCount == 0) |
           (InsuranceFromAnySource == 0 &
              SourceCount > 0))

conflicting_health_insurance_entry <- health_insurance_subs %>%
  filter(DataCollectionStage == 1) %>%
  merge_check_info(checkIDs = 94) %>%
  select(all_of(vars_we_want))

conflicting_health_insurance_exit <- health_insurance_subs %>%
  filter(DataCollectionStage == 3) %>%
  merge_check_info(checkIDs = 95) %>%
  select(all_of(vars_we_want))

rm(health_insurance_subs)

# Missing NCBs at Entry ---------------------------------------------------

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
         EnrollmentID,
         DataCollectionStage,
         BenefitsFromAnySource,
         BenefitCount) %>%
  unique()

missing_ncbs_entry <- ncb_staging %>%
  filter(BenefitsFromAnySource == 99 |
         is.na(BenefitsFromAnySource)
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
  select(
    EnrollmentID,
    HouseholdID,
    PersonalID,
    ProjectID,
    ProjectName,
    OrganizationName,
    ProjectType
  ) %>%
  left_join(
    Enrollment %>%
      select(
        EnrollmentID,
        HouseholdID,
        PersonalID,
        EntryDate,
        MoveInDateAdjust,
        ExitDate,
        RelationshipToHoH,
        PercentAMI,
        VAMCStation,
        HPScreeningScore,
        ThresholdScore
      ),
    by = c("PersonalID", "EnrollmentID", "HouseholdID")
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
        is.na(WorldWarII) | WorldWarII == 99 |
          is.na(KoreanWar) | KoreanWar == 99 |
          is.na(VietnamWar) | VietnamWar == 99 |
          is.na(DesertStorm) | DesertStorm == 99 |
          is.na(AfghanistanOEF) | AfghanistanOEF == 99 |
          is.na(IraqOIF) | IraqOIF == 99 |
          is.na(IraqOND) | IraqOND == 99 |
          is.na(OtherTheater) |
          OtherTheater == 99
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
  filter(DischargeStatus %in% c(dkr)) %>%
  merge_check_info(checkIDs = 56) %>%
  select(all_of(vars_we_want))

dkr_client_veteran_wars <- dkr_client_veteran_info %>%
  filter(WorldWarII %in% c(dkr) |
        KoreanWar %in% c(dkr) |
        VietnamWar %in% c(dkr) |
        DesertStorm  %in% c(dkr) |
        AfghanistanOEF %in% c(dkr) |
        IraqOIF %in% c(dkr) |
        IraqOND %in% c(dkr) |
        OtherTheater  %in% c(dkr)
  ) %>%
  merge_check_info(checkIDs = 57) %>%
  select(all_of(vars_we_want))

dkr_client_veteran_military_branch <- dkr_client_veteran_info %>%
  filter(MilitaryBranch %in% c(dkr)) %>%
  merge_check_info(checkIDs = 58) %>%
  select(all_of(vars_we_want))

    # All together now --------------------------------------------------------
    dq_main <- rbind(
      conflicting_health_insurance_entry,
      conflicting_health_insurance_exit,
      conflicting_income_entry,
      conflicting_income_exit,
      conflicting_ncbs_entry,
      dkr_client_veteran_wars,
      dkr_client_veteran_military_branch,
      dkr_client_veteran_discharge,
      dkr_destination,
      dkr_dob,
      dkr_gender,
      dkr_LoS,
      dkr_months_times_homeless,
      dkr_name,
      dkr_race,
      dkr_residence_prior,
      dkr_ssn,
      dkr_veteran,
      dq_overlaps1,
      dq_overlaps2,
      duplicate_ees,
      enrollment_v_operating,
      enrollment_v_participating,
      exit_before_start,
      future_ees,
      future_exits,
      hh_issues,
      incomplete_living_situation,
      incorrect_dob,
      invalid_months_times_homeless1,
      invalid_months_times_homeless2,
      invalid_months_times_homeless3,
      invalid_movein_date,
      long_stayers,
      missing_approx_date_homeless,
      missing_destination,
      missing_destination_subsidy,
      missing_disabilities,
      missing_dob,
      missing_dob_dataquality,
      missing_enrollment_coc,
      missing_gender,
      missing_health_insurance_entry,
      missing_health_insurance_exit,
      missing_income_entry,
      missing_income_exit,
      missing_LoS,
      missing_months_times_homeless,
      missing_name_dataquality,
      missing_ncbs_entry,
      missing_previous_street_ESSH,
      missing_race,
      missing_residence_prior,
      missing_ssn,
      missing_veteran_status,
      ssvf_hp_screen,
      ssvf_missing_percent_ami,
      ssvf_missing_vamc,
      veteran_missing_branch,
      veteran_missing_discharge_status,
      veteran_missing_wars,
      veteran_missing_year_entered,
      veteran_missing_year_separated,
      veteran_incorrect_year_entered,
      veteran_incorrect_year_separated
    ) %>%
  unique() %>%
  mutate(Type = factor(Type, levels = c("High Priority",
                                        "Error",
                                        "Warning")))
    
   dq_providers <- sort(Project0$ProjectName) 
   
# Plots for System-Level DQ Tab -------------------------------------------
   dq_plot_df <- dq_main %>%
     left_join(Project %>%
                 select(ProjectID, OrganizationID) %>%
                 unique(), by = "ProjectID") %>%
     select(PersonalID, OrganizationID, OrganizationName, HouseholdID, Issue, Type) %>%
     unique()

# Prepping dataframes for plots for Organization-Level DQ Tab -----------------
   dq_org_plot_df <- dq_main %>%
     select(PersonalID, ProjectID, ProjectName, OrganizationName, HouseholdID, Issue, Type) %>%
     unique()
   
