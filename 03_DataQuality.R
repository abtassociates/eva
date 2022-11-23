
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(HMIS)

source("03_guidance.R")

va_funded <- Funder %>%
  filter(Funder %in% va_fund_sources) %>%
  pull(ProjectID)

rhy_funded <- Funder %>%
  filter(Funder %in% rhy_fund_sources) %>%
  pull(ProjectID)

path_funded <- Funder %>%
  filter(Funder == path_fund_sources) %>%
  pull(ProjectID)

ssvf_funded <- Funder %>%
  filter(Funder == ssvf_fund_sources) %>%
  pull(ProjectID)

# Projects to Check -------------------------------------------------------
projects_current_hmis <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(HMISParticipatingProject == 1 &
           operating_between(., meta_HUDCSV_Export_Start, meta_HUDCSV_Export_End)) %>%
  select(
    ProjectID,
    OrganizationID,
    OperatingStartDate,
    OperatingEndDate,
    ProjectType,
    ProjectName,
    OrganizationName
  ) %>% unique()

# Clients to Check --------------------------------------------------------
served_in_date_range <- Enrollment %>%
  left_join(Client %>%
              select(-DateCreated), by = "PersonalID") %>%
  left_join(Project %>% select(ProjectID, TrackingMethod, OrganizationName), by = "ProjectID") %>%
  select(
    PersonalID,
    FirstName,
    NameDataQuality,
    SSN,
    SSNDataQuality,
    DOB,
    DOBDataQuality,
    AmIndAKNative,
    Asian,
    BlackAfAmerican,
    NativeHIPacific,
    White,
    RaceNone,
    Ethnicity,
    Female,
    Male,
    NoSingleGender,
    Transgender,
    Questioning,
    GenderNone,
    VeteranStatus,
    EnrollmentID,
    ProjectID,
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
    ExitAdjust,
    DateCreated,
    ClientEnrolledInPATH,
    LengthOfStay,
    DateOfPATHStatus,
    ReasonNotEnrolled,
    ClientLocation,
    TrackingMethod
  ) %>%
  inner_join(projects_current_hmis, by = "ProjectID")

DV <- HealthAndDV %>%
  filter(DataCollectionStage == 1) %>%
  select(EnrollmentID, DomesticViolenceVictim, WhenOccurred, CurrentlyFleeing)

served_in_date_range <- served_in_date_range %>%
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

# Missing UDEs ------------------------------------------------------------
dq_name <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      NameDataQuality == 99 | is.na(NameDataQuality) ~ 
        "Missing Name Data Quality",
      NameDataQuality %in% c(8, 9) ~ 
        "Incomplete or Don't Know/Refused Name"
    ),
    Type = case_when(
      Issue == "Missing Name Data Quality" ~ "Error",
      Issue == "Incomplete or Don't Know/Refused Name" ~ "Warning"
    ),
    Guidance = if_else(Type == "Warning", 
                       guidance_dkr_data, 
                       guidance_missing_pii)
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_dob <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      is.na(DOB) & DOBDataQuality %in% c(1, 2) ~ "Missing DOB",
      is.na(DOBDataQuality) ~ "Missing DOB Data Quality",
      DOBDataQuality %in% c(8, 9, 99) ~ 
        "Don't Know/Refused/Data Not Collected DOB",
      AgeAtEntry < 0 | AgeAtEntry > 100 ~ "Incorrect DOB or Entry Date"
    ),
    Type = case_when(
      Issue %in% c(
        "Missing DOB",
        "Incorrect DOB or Entry Date",
        "Missing DOB Data Quality"
      ) ~ "Error",
      Issue ==  "Don't Know/Refused/Data Not Collected DOB" ~ "Warning"
    ),
    Guidance = case_when(
      Issue == "Incorrect DOB or Entry Date" ~
        "The HMIS data is indicating the client entered the project PRIOR to
      being born. Correct either the Date of Birth or the Project Start Date, 
      whichever is incorrect.",
      Issue %in% c("Missing DOB", "Missing DOB Data Quality") ~
        guidance_missing_at_entry,
      Issue == "Don't Know/Refused/Data Not Collected DOB" ~
        guidance_dkr_data
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_ssn <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      SSN == "Missing" ~ "Missing SSN",
      SSN == "Invalid" ~ "Invalid SSN",
      SSN == "DKR" ~ "Don't Know/Refused SSN",
      SSN == "Incomplete" ~ "Invalid SSN"
    ),
    Type = case_when(
      Issue %in% c("Missing SSN", "Invalid SSN") ~ "Error",
      Issue == "Don't Know/Refused SSN" ~ "Warning"
    ),
    Guidance = case_when(
      Issue == "Don't Know/Refused SSN" ~ guidance_dkr_data,
      Issue == "Missing SSN" ~ guidance_missing_pii,
      Issue == "Invalid SSN" ~ "The Social Security Number does not conform with 
      standards set by the Social Security Administration. This includes rules 
      like every SSN is exactly 9 digits and cannot have certain number patterns. 
      Navigate to the client's record in HMIS to correct the data."
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_race <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      RaceNone == 99 ~ "Missing Race",
      RaceNone %in% c(8, 9) ~ "Don't Know/Refused Race"
    ),
    Type = case_when(
      Issue == "Missing Race" ~ "Error",
      Issue == "Don't Know/Refused Race" ~ "Warning"
    ),
    Guidance = if_else(Type == "Warning", 
                       guidance_dkr_data, 
                       guidance_missing_at_entry)
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_ethnicity <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      Ethnicity == 99 ~ "Missing Ethnicity",
      Ethnicity %in% c(8, 9) ~ "Don't Know/Refused Ethnicity"
    ),
    Type = case_when(
      Issue == "Missing Ethnicity" ~ "Error",
      Issue == "Don't Know/Refused Ethnicity" ~ "Warning"
    ),
    Guidance = if_else(Type == "Warning", 
                       guidance_dkr_data, 
                       guidance_missing_at_entry)
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_gender <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      GenderNone == 99 ~ "Missing Gender",
      GenderNone %in% c(8, 9) ~ "Don't Know/Refused Gender"
    ),
    Type = case_when(
      Issue == "Missing Gender" ~ "Error",
      Issue == "Don't Know/Refused Gender" ~ "Warning"
    ),
    Guidance = if_else(Type == "Warning", 
                       guidance_dkr_data, 
                       guidance_missing_at_entry)
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_veteran <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
        VeteranStatus == 99 ~ "Missing Veteran Status",
      (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
        VeteranStatus %in% c(8, 9) ~ "Don't Know/Refused Veteran Status"
    ),
    Type = case_when(
      Issue == "Missing Veteran Status" ~ "Error",
      Issue %in% c(
        "Don't Know/Refused Veteran Status"
      ) ~ "Warning"
    ),
    Guidance = case_when(
      Issue == "Missing Veteran Status" ~ guidance_missing_pii,
      Issue == "Don't Know/Refused Veteran Status" ~ guidance_dkr_data)
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

# Missing Client Location -------------------------------------------------

missing_client_location <- served_in_date_range %>%
  left_join(EnrollmentCoC %>% select(EnrollmentID, DataCollectionStage), by = "EnrollmentID") %>%
  filter(is.na(ClientLocation) & 
         RelationshipToHoH == 1 &
        DataCollectionStage == 1
  ) %>%
  mutate(Type = "High Priority",
         Issue = "Missing Client Location",
         Guidance = 
           "If Client Location is missing, this household will be excluded from
         all HUD reporting.") %>%
  select(all_of(vars_we_want))

# Household Issues --------------------------------------------------------
hh_children_only <- served_in_date_range %>%
  group_by(HouseholdID) %>%
  summarise(
    hhMembers = n(),
    maxAge = max(AgeAtEntry),
  ) %>%
  filter(maxAge < 12) %>%
  ungroup() %>%
  left_join(served_in_date_range, by = c("HouseholdID","maxAge" = "AgeAtEntry")) %>%
  distinct(HouseholdID, maxAge, .keep_all = TRUE) %>%
  mutate(Issue = "Oldest Household Member Under 12",
         Type = "High Priority",
         Guidance = "Unless your project serves youth younger than 12 
         exclusively, every household should have at least one adult in it. If 
         you are not sure how to correct this, please contact the HMIS team for 
         help.") %>%
  select(all_of(vars_we_want))

hh_no_hoh <- served_in_date_range %>%
  group_by(HouseholdID) %>%
  summarise(hasHoH = if_else(min(RelationshipToHoH) != 1,
                             FALSE,
                             TRUE),
            PersonalID = min(PersonalID)) %>%
  filter(hasHoH == FALSE) %>%
  ungroup() %>%
  left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
  mutate(
    Issue = "No Head of Household",
    Type = "High Priority",
    Guidance = "Please be sure all members of the household are included in the 
      program stay, and that each household member's birthdate is correct. 
      If those things are both true, or the client is a single, ensure that
      each household member has \"Relationship to Head of Household\" answered 
      at Project Start and that one of them says Self (head of household).
      Singles are always Self (head of household)."
  ) %>%
  select(all_of(vars_we_want))

hh_too_many_hohs <- served_in_date_range %>%
  filter(RelationshipToHoH == 1) %>%
  group_by(HouseholdID) %>%
  summarise(HoHsinHousehold = n(),
            PersonalID = min(PersonalID)) %>%
  filter(HoHsinHousehold > 1) %>%
  ungroup() %>%
  left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
  mutate(Issue = "Too Many Heads of Household",
         Type = "High Priority",
         Guidance = "Check the assessment at Project Start to be sure each 
         household member has \"Relationship to Head of Household\" answered 
         and that only one of them says \"Self (head of household)\".") %>%
  select(all_of(vars_we_want))

hh_missing_rel_to_hoh <- served_in_date_range %>%
  filter(RelationshipToHoH == 99) %>%
  anti_join(hh_no_hoh["HouseholdID"], by = "HouseholdID") %>%
  mutate(Issue = "Missing Relationship to Head of Household",
         Type = "High Priority",
         Guidance = "Check the assessment at Project Start to be sure each 
         household member has \"Relationship to Head of Household\" answered 
         and that only one of them says \"Self (head of household)\".") %>%
  select(all_of(vars_we_want))

hh_issues <- rbind(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

rm(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

missing_approx_date_homeless <- served_in_date_range %>%
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
  mutate(Issue = "Missing Approximate Date Homeless", 
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

missing_previous_street_ESSH <- served_in_date_range %>%
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
  mutate(Issue = "Missing Previously From Street, ES, or SH (Length of Time Homeless questions)",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

missing_residence_prior <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LivingSituation) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LivingSituation) | LivingSituation == 99)) %>%
  mutate(Issue = "Missing Residence Prior",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

dkr_residence_prior <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LivingSituation) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LivingSituation %in% c(8, 9)) %>%
  mutate(Issue = "Don't Know/Refused Residence Prior",
         Type = "Warning",
         Guidance = guidance_dkr_data) %>%
  select(all_of(vars_we_want))

missing_LoS <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LengthOfStay) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
  mutate(Issue = "Missing Length of Stay",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

dkr_LoS <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LengthOfStay) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LengthOfStay %in% c(8, 9)) %>%
  mutate(Issue = "Don't Know/Refused Residence Prior",
         Type = "Warning",
         Guidance = guidance_dkr_data) %>%
  select(all_of(vars_we_want))

missing_months_times_homeless <- served_in_date_range %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           EntryDate >= hc_prior_living_situation_required &
           ProjectType %in% c(1, 4, 8) &
           (
             is.na(MonthsHomelessPastThreeYears) |
               is.na(TimesHomelessPastThreeYears) |
               MonthsHomelessPastThreeYears == 99 |
               TimesHomelessPastThreeYears == 99
           )
  ) %>%
  mutate(Issue = "Missing Months or Times Homeless",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

dkr_months_times_homeless <- served_in_date_range %>%
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
             MonthsHomelessPastThreeYears %in% c(8, 9) |
               TimesHomelessPastThreeYears %in% c(8, 9)
           )
  ) %>%
  mutate(Issue = "Don't Know/Refused Months or Times Homeless",
         Type = "Warning",
         Guidance = guidance_dkr_data) %>%
  select(all_of(vars_we_want))


invalid_months_times_homeless <- served_in_date_range %>%
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
    DateMonthsMismatch = if_else(MonthsHomelessPastThreeYears - MonthDiff != 100, 1, 0),
    Issue = case_when(
      MonthDiff <= 0 ~
        "Homelessness Start Date Later Than Entry",
      MonthsHomelessPastThreeYears < 100 ~
        "Number of Months Homeless Can Be Determined",
      DateMonthsMismatch == 1 ~ 
        "Invalid Homelessness Start Date/Number of Months Homeless"),
    Type = "Warning",
    Guidance = case_when(
      MonthDiff <= 0 ~
        "This client has an Approximate Date Homelessness Started in their 
        enrollment that is after their Project Start Date. The information 
        at Project Start should reflect the client's situation at the point of 
        Project Start, so this date may have been incorrectly entered.",
      MonthsHomelessPastThreeYears < 100 ~
        "According to this client's assessment at Project Start, they 
        experienced a single episode of homelessness in the three years prior to 
        their Project Start and the approximate date homelessness started is known, 
        but there was no response entered for the total number of months they 
        experienced homelessness prior to this enrollment. It should be possible 
        to determine and enter the total number of months they experienced
        homelessness based on the Approximate Date Homelessness Started and the 
        Project Start Date.",
      DateMonthsMismatch == 1 ~ 
        "According to this client's assessment at Project Start, they experienced 
        a single episode of homelessness in the three years prior to their 
        enrollment and the approximate date homelessness started known, but the 
        total number of months they experienced homelessness prior to this 
        enrollment is inconsistent with the given dates. Please double-check this 
        information for consistency and accuracy.")) %>%
  filter(!is.na(Guidance)) %>%
  select(all_of(vars_we_want))

missing_living_situation <- served_in_date_range %>%
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
           ProjectType %in% c(2, 3, 6, 9, 10, 12, 13) &
           (
             (
               LivingSituation %in% c(15, 6, 7, 24, 4, 5) &
                 LengthOfStay %in% c(2, 3, 10, 11) &
                 (is.na(LOSUnderThreshold) |
                    is.na(PreviousStreetESSH))
             ) |
               (
                 LivingSituation %in% c(2, 3, 12, 13, 14, 15, 19,
                                        20, 21, 22, 23, 25, 26) &
                   LengthOfStay %in% c(10, 11) &
                   (is.na(LOSUnderThreshold) |
                      is.na(PreviousStreetESSH))
               )
           )
  ) %>%
  mutate(Issue = "Incomplete Living Situation Data", 
         Type = "Error",
         Guidance = "When responding to the Prior Living Situation questions in 
         your assessment at Project Start, users must answer questions about the 
         clients' situation prior to the \"Type of Residnce\" question that are 
         important to help determine that client's Chronicity. Please answer these 
         questions to the best of your knowledge.") %>%
  select(all_of(vars_we_want))

dkr_living_situation <- served_in_date_range %>%
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
             MonthsHomelessPastThreeYears %in% c(8, 9) |
               TimesHomelessPastThreeYears %in% c(8, 9) |
               LivingSituation %in% c(8, 9)
           )
  ) %>%
  mutate(Issue = "Don't Know/Refused Living Situation", 
         Type = "Warning",
         Guidance = guidance_dkr_data) %>%
  select(all_of(vars_we_want))

# DisablingCondition at Entry
detail_missing_disabilities <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         DisablingCondition) %>%
  filter(DisablingCondition == 99 |
           is.na(DisablingCondition)) %>%
  mutate(Issue = "Missing Disabling Condition", 
         Type = "Error",
         Guidance = guidance_missing_at_entry)

missing_disabilities <- detail_missing_disabilities %>%
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
# conflicting_disabilities <- served_in_date_range %>%
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

# Extremely Long Stayers --------------------------------------------------

th_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 2) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_Date, EntryDate)))
# using Export Date here to reflect the date the export was run on

Top2_TH <- subset(th_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

rrh_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 13) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_Date, EntryDate))) 

Top2_RRH <- subset(rrh_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

es_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 1) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_Date, EntryDate))) 

Top2_ES <- subset(es_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

psh_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) & ProjectType %in% c(3, 9, 10)) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_Date, EntryDate))) 

Top1_PSH <- subset(psh_stayers, Days > quantile(Days, prob = 1 - 1 / 100))

hp_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 12) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_Date, EntryDate))) 

Top2_HP <- subset(hp_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

ce_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 14) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_End, EntryDate))) 

Top2_CE <- subset(ce_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

outreach_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID, EnrollmentID) %>%
  left_join(
    CurrentLivingSituation %>% 
      group_by(EnrollmentID) %>%
      summarise(latestInfoDate = max(InformationDate)) %>%
      ungroup() %>%
      select(EnrollmentID, latestInfoDate)
    , by = "EnrollmentID"
  ) %>%
  filter(is.na(ExitDate) &
           ProjectType == 4) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_End, latestInfoDate))) 

Top2_Outreach <- subset(outreach_stayers %>% select(-c(EnrollmentID, latestInfoDate)), Days > input$OUTLongStayers)

missed_movein_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType %in% c(3,9,10,13)
  ) %>%
  mutate(
    Days = as.numeric(difftime(MoveInDateAdjust, EntryDate)),
    Issue = "Possible Missed Move-In Date",
    Type = "Warning",
    Guidance = "Fix Me"
  )

Top2_movein <- subset(missed_movein_stayers, Days > quantile(Days, prob = 1 - 2 / 100, na.rm = TRUE))

extremely_long_stayers <- rbind(Top1_PSH,
                                Top2_ES,
                                Top2_RRH,
                                Top2_TH,
                                Top2_HP,
                                Top2_CE,
                                Top2_Outreach) %>%
  mutate(
    Issue = "Possible Missed Exit Date",
    Type = "Warning",
    Guidance = paste("This enrollment is in the top",
                     case_when(ProjectType %in% c(3, 9, 10) ~ "1%",
                               TRUE ~ "2%"),
                     "of all other projects of its type in your HMIS system for
                     how many days it has been active. Please be sure this
                     household is still actively enrolled in this project and if
                     not, record the date they exited your project as the Exit
                     Date. If they are actively enrolled, do not change the data.")
  ) %>% 
  select(all_of(vars_we_want))

extremely_long_stayers <- rbind(extremely_long_stayers, Top2_movein %>% select(all_of(vars_we_want)))

rm(list = ls(pattern = "Top*"),
   es_stayers,
   th_stayers,
   psh_stayers,
   rrh_stayers,
   hp_stayers)


# Non-Residential Long Stayers --------------------------------------------

calculate_long_stayers <- function(input, projecttype){

  served_in_date_range %>%
    select(all_of(vars_prep), ProjectID) %>%
    mutate(
      Days = as.numeric(difftime(meta_HUDCSV_Export_Date, EntryDate)),
      Issue = "Days Enrollment Active Exceeds CoC-specific Settings",
      Type = "Warning",
      Guidance = "You have at least one active enrollment that has been
         active for longer than the days set for this Project Type in your
         CoC-specific Settings on the Upload CSV tab."
    ) %>%
    filter(is.na(ExitDate) &
             ProjectType == projecttype &
             input < Days) %>% 
    select(all_of(vars_we_want))
  
}

# can't do further logic with this because it needs to be reactive

# Incorrect Destination ---------------------------------------------------

# RRH mover inners only
moved_in_rrh <- served_in_date_range %>%
  filter(ProjectType == 13 & !is.na(MoveInDateAdjust)) %>%
  mutate(RRH_range = interval(EntryDate, ExitAdjust - days(1))) %>%
  select(PersonalID, 
         "RRHMoveIn" = MoveInDateAdjust, 
         RRH_range, 
         "RRHProjectName" = ProjectName)

enrolled_in_rrh <- served_in_date_range %>%
  filter(ProjectType == 13) %>%
  mutate(RRH_range = interval(EntryDate, ExitAdjust - days(1))) %>%
  select(PersonalID, 
         "RRHMoveIn" = MoveInDateAdjust, 
         RRH_range, 
         "RRHProjectName" = ProjectName)


# PSH mover inners only

enrolled_in_psh <- served_in_date_range %>%
  filter(ProjectType %in% c(3, 9) & !is.na(MoveInDateAdjust)) %>%
  mutate(PSH_range = interval(EntryDate, ExitAdjust - days(1))) %>%
  select(PersonalID, 
         PSH_range, 
         "PSHMoveIn" = MoveInDateAdjust,
         "PSHProjectName" = ProjectName)

# TH
enrolled_in_th <- served_in_date_range %>%
  filter(ProjectType == 2) %>%
  mutate(TH_range = interval(EntryDate, ExitAdjust - days(1))) %>%
  select(PersonalID, TH_range, "THProjectName" = ProjectName)

# SH

enrolled_in_sh <- served_in_date_range %>%
  filter(ProjectType == 8) %>%
  mutate(SH_range = interval(EntryDate, ExitAdjust - days(1))) %>%
  select(PersonalID, SH_range, "SHProjectName" = ProjectName)

# Project Exit Before Start --------------
exit_before_start <- served_in_date_range %>%
  filter(ExitDate < EntryDate & !is_null(ExitDate) & !is_null(EntryDate)) %>% 
  mutate(Issue = "Project Exit Before Start",
         Type = "Error",
         Guidance = guidance_exit_before_start) %>%
  select(all_of(vars_we_want))


# Missing Destination -----------------------------------------------------

missing_destination <- served_in_date_range %>%
  filter(!is.na(ExitDate) &
           (is.na(Destination) |
              Destination %in% c(99, 30))) %>%
  mutate(
    Issue = "Missing Destination",
    Type = "Warning",
    Guidance = paste(
      "It is widely understood that not every client will complete an exit
          interview, especially for high-volume emergency shelters. A few warnings
          for Missing Destination is no cause for concern, but if there is a
          large number, please contact your CoC to work out a way to improve
          client engagement."
    )
  ) %>%
  select(all_of(vars_we_want))

dkr_destination <- served_in_date_range %>%
  filter(Destination %in% c(8, 9)) %>%
  mutate(Issue = "Don't Know/Refused Destination",
         Type = "Warning",
         Guidance = guidance_dkr_data) %>%
  select(all_of(vars_we_want))

# Missing PATH Data -------------------------------------------------------

#* Length of Stay in Res Prior
### adult, PATH-enrolled, and:
### Length of Stay is null or DNC -> error -OR-
### Length of Stay is DKR -> warning

# smallProject <- Project %>% select(ProjectID, ProjectName)
# 

# path_missing_los_res_prior <- served_in_date_range %>%
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

# path_no_status_at_exit <- served_in_date_range %>%
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


# path_status_determination <- served_in_date_range %>%
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
# path_enrolled_missing <- served_in_date_range %>%
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
# path_reason_missing <- served_in_date_range %>%
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
# path_SOAR_missing_at_exit <- served_in_date_range %>%
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
#   left_join(served_in_date_range, by = "PersonalID") %>%
#   filter(
#     parseDate(InformationDate) >= EntryDate &
#       parseDate(InformationDate) <= ExitAdjust) %>% 
#   group_by(PersonalID, ProjectName, EntryDate, ExitDate) %>%
#   summarise(CurrentLivingSituationCount = n()) %>%
#   ungroup()
# 
# missing_path_contact <- served_in_date_range %>%
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

# Duplicate EEs -----------------------------------------------------------

duplicate_ees <-
  get_dupes(served_in_date_range, PersonalID, ProjectID, EntryDate) %>%
  mutate(
    Issue = "Duplicate Entries",
    Type = "High Priority",
    Guidance = "A client cannot have two enrollments with the same entry date
    into the same project. These are duplicate enrollment records. Please 
    consult your HMIS System Administrator on how to correct these duplicates."
  ) %>%
  select(all_of(vars_we_want))

# Future Entry Exits ------------------------------------------------------

# PSHs in the old days before Move In Dates would definitely have been entering
# their clients prior to their Entry Date since back then the Entry Date was the
# day they moved in. So they're excused from this prior to Move In Date's existence.
future_ees <- served_in_date_range %>%
  filter(EntryDate > DateCreated &
           (ProjectType %in% c(1, 2, 4, 8, 13) |
              (ProjectType %in% c(3, 9) & 
                  EntryDate >= hc_psh_started_collecting_move_in_date
              )))  %>%
  mutate(
    Issue = "Future Entry Date",
    Type = "Warning",
    Guidance = "Users should not be entering a client into a project on a 
    date in the future. If the Project Start Date is correct, there is no action 
    needed, but going forward, please be sure that your data entry workflow 
    is correct according to your project type."
  ) %>%
  select(all_of(vars_we_want))

future_exits <- served_in_date_range %>%
  filter(ExitAdjust > meta_HUDCSV_Export_Date) %>%
  mutate(
    Issue = "Future Exit Date",
    Type = "Error",
    Guidance = "This client's Exit Date is a date in the future. Please 
  enter the exact date the client left your program. If this client has not
  yet exited, delete the Exit and then enter the Exit Date once the client
  is no longer in your program."
  ) %>%
  select(all_of(vars_we_want))
    
# Missing Income at Entry -------------------------------------------------

missing_income_entry <- served_in_date_range %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    DataCollectionStage,
    TotalMonthlyIncome,
    IncomeFromAnySource
  ) %>%
  filter(DataCollectionStage == 1 &
           ProjectName != "Unsheltered Clients - OUTREACH" &
           (AgeAtEntry > 17 |
              is.na(AgeAtEntry)) &
           (IncomeFromAnySource == 99 |
              is.na(IncomeFromAnySource))) %>%
  mutate(Issue = "Income Missing at Entry",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

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
  smallIncome %>% full_join(IncomeBenefits[c(
    "PersonalID",
    "EnrollmentID",
    "DataCollectionStage",
    "TotalMonthlyIncome",
    "IncomeFromAnySource"
  )],
  by = c("PersonalID",
         "EnrollmentID",
         "DataCollectionStage"))

income_subs <- served_in_date_range[c("EnrollmentID",
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
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>%
  mutate(Issue = "Conflicting Income yes/no at Entry",
         Type = "Error",
         Guidance = guidance_conflicting_income) %>%
  select(all_of(vars_we_want))

# Missing Income at Exit --------------------------------------------------

missing_income_exit <- served_in_date_range %>%
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
  mutate(Issue = "Income Missing at Exit",
         Type = "Error",
         Guidance = guidance_missing_at_exit) %>%
  select(all_of(vars_we_want))

conflicting_income_exit <- income_subs %>%
  filter(DataCollectionStage == 3 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>%
  mutate(Issue = "Conflicting Income yes/no at Exit",
         Type = "Error",
         Guidance = guidance_conflicting_income) %>%
  select(all_of(vars_we_want))

rm(income_subs)

# Enrollment Active Outside Operating Dates ------------------------
active_outside_dates <- served_in_date_range %>%
  filter(RelationshipToHoH == 1 &
           EntryDate < OperatingStartDate |
           (ExitDate > OperatingEndDate & !is_null(ExitDate)) |
           (is_null(ExitDate) & !is_null(OperatingEndDate))
  ) %>%
  mutate(Issue = "Enrollment Active Outside Project Operating Dates",
         Type = "Error",
         Guidance = guidance_enrl_active_outside_op) %>%
  select(all_of(vars_we_want))

# Overlapping NEW (11/2) ------------------------------------
overlapNEWvars = c("EnrollmentID", 
                   "TrackingMethod", 
                   "EntryDate", 
                   # "ExitDate", 
                   "ExitAdjust",
                   "ProjectType", 
                   "PersonalID", 
                   "ProjectID",
                   "OrganizationName",
                   "ProjectName", 
                   "HouseholdID",
                   "DateProvided",
                   "MoveInDate"
                   )

overlapNEWcols_to_remove <- c("DateProvided.x", "DateProvided.y")
overlapNEW_services <- Services %>% 
  select(EnrollmentID, DateProvided)

# var dict: https://www.hudexchange.info/programs/hmis/hmis-data-standards/standards/Project_Descriptor_Data_Elements_(PDDE).htm

## DQ14a: Overlaps Between Residential Projects that Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date -----
# This gets the enrollment records for the relevant project types
mainRecords <- served_in_date_range %>%
  filter((ProjectType == 1 & TrackingMethod == 0) | ProjectType %in% c(2,8)) %>%
  left_join(overlapNEW_services, by = "EnrollmentID") %>%
  select(!!overlapNEWvars)

c1 <- mainRecords
c2 <- mainRecords

overlapNEW_entry_and_exit <- c1 %>% 
  inner_join(c2, by = "PersonalID") %>%
  filter(
    EntryDate.y < ExitAdjust.x & 
    EntryDate.x < ExitAdjust.y &
    (
      EntryDate.x < EntryDate.y | 
      (EntryDate.x == EntryDate.y & EnrollmentID.x > EnrollmentID.y)
    )
  ) %>%
  mutate(
    Issue = "Overlaps Between Residential Projects that Use Entry Date
    (Project Start Date) and Exit Date (Project Exit Date) to indicate the
    household is occupying that unit on that date",
    Type = "Warning",
    Guidance = overlapNEW_entry_and_exit_guidance,
    FirstDateProvided.x = NA,
    FirstDateProvided.y = NA
  ) %>%
  select(-!!overlapNEWcols_to_remove) %>%
  unique()
 
# DQ14b: Overlaps Between Residential Projects That Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date AND Projects That Use Bed Night Date to indicate the household is occupying that unit on that date ----
# Note that we "anchor" the primary enrollment as ES-nbn to capture overlaps with entry-exit projects, while staying distinct from the previous check
mainRecords <- served_in_date_range %>%
  filter(ProjectType %in% c(1,2,8)) %>%
  left_join(overlapNEW_services, by = "EnrollmentID") %>%
  mutate(ESnbn = (ProjectType == 1 & TrackingMethod == 3),
         EEnbn = (ProjectType == 1 & TrackingMethod == 0),
         THSH = (ProjectType %in% c(2,8))
  )

c1 <- mainRecords %>% filter(ESnbn)
c2 <- mainRecords

# This flags the ones that overlap
overlapNEW_entry_and_exit_bn <- c1 %>% 
  inner_join(c2, by = "PersonalID") %>%
  mutate(IsOverlap = EnrollmentID.x != EnrollmentID.y & 
    (
      ((EEnbn.y | THSH.y) & DateProvided.x >= EntryDate.y & DateProvided.x <= ExitAdjust.y) |
      (ESnbn.y & DateProvided.x == DateProvided.y)
    ) & (
      # the primary enrollment record should always be the ES nbn project
      !ESnbn.y | #if the other enrollment is not ES-nbn, then it's fine as long as they aren't the same (which they shouldn't be)
      (EnrollmentID.x > EnrollmentID.y & ESnbn.y) # if the other enrollment is ALSO ES-nbn, then take the larger enrollment, so that we don't show the same version twice (just flipped) (i.e. EnrollmentA-EnrollmentB AND EnrollmentB-EnrollmentA)
    )
  ) %>%
  group_by(EnrollmentID.x) %>%
  mutate(NumOverlaps = sum(IsOverlap)) %>%
  ungroup() %>%
  filter(NumOverlaps > 2 | ESnbn.y) %>%
  mutate(
    Issue = "Overlaps Between Residential Projects That Use Entry Date
    (Project Start Date) and Exit Date (Project Exit Date) to indicate the
    household is occupying that unit on that date AND Projects That Use Bed
    Night Date to indicate the household is occupying that unit on that date",
    Type = "Error",
    Guidance = overlapNEW_entry_and_exit_bn_guidance,
    FirstDateProvided.x = min(DateProvided.x),
    FirstDateProvided.y = NA
  ) %>%
  select(
    matches(paste(overlapNEWvars, collapse="|")), 
    c(Issue, Type, Guidance),
    -c(!!overlapNEWcols_to_remove, MoveInDateAdjust.x,MoveInDateAdjust.y)
  ) %>%
  unique()


# DQ14c in Excel: Overlaps Between Residential Projects That Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) OR Bed Night Date to indicate the household is occupying that unit on that date AND Residential Projects That Use Housing Move-In Date and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date
# This gets the enrollment records that are PH-RRH (13) and PH-PSH (3), as well as ES (1, includes NbN (when TrackingMethod = 3)), SH (8), and TH (2); 3 and 13 are compared to the others (not each other or themselves)
mainRecords <- served_in_date_range %>%
  filter(ProjectType %in% c(1,2,8,3,13)) %>%
  left_join(overlapNEW_services, by = "EnrollmentID") %>%
  mutate(PH = ProjectType %in% c(3,13),
         ESnbn = (ProjectType == 1 & TrackingMethod == 3),
         THSH = (ProjectType %in% c(2,8))
  )

c1 <- mainRecords %>% filter(PH)
c2 <- mainRecords

overlapNEW_entry_and_exit_bn2 <- c1 %>% 
  inner_join(c2, by = "PersonalID") %>%
  mutate(
    IsOverlap = EnrollmentID.x != EnrollmentID.y & PH.x & (
      (THSH.y & EntryDate.y < ExitAdjust.x & MoveInDate.x < ExitAdjust.y) |
      (ESnbn.y & MoveInDate.x <= DateProvided.y & DateProvided.y <= ExitAdjust.x) | 
      (ProjectType.y == 1 & EntryDate.y < ExitAdjust.x & MoveInDate.x < ExitAdjust.y)
    )
  ) %>%
  group_by(EnrollmentID.x) %>%
  mutate(NumOverlaps = sum(IsOverlap)) %>%
  ungroup() %>%
  filter(
    (THSH.y & NumOverlaps > 0) |
    (ProjectType.y == 1 & NumOverlaps > 2)
  ) %>%
  mutate(
    Issue = "Overlaps Between Residential Projects That Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) OR Bed Night Date to indicate the household is occupying that unit on that date AND Residential Projects That Use Housing Move-In Date and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date",
    Type = "Error",
    Guidance = overlapNEW_entry_and_exit_bn2_guidance,
    FirstDateProvided.x = NA,
    FirstDateProvided.y = min(DateProvided.y)
  ) %>%
  select(
    matches(paste(overlapNEWvars, collapse="|")),
    c(Issue, Type, Guidance),
    -c(!!overlapNEWcols_to_remove, MoveInDateAdjust.x,MoveInDateAdjust.y)
  ) %>%
  distinct()

# Overlaps Between Projects that Use Move-In Date to Exit Date to Indicate Occupancy ----
# This gets the enrollment records that are PH-RRH (13) and PH-PSH (3)
mainRecords <- served_in_date_range %>%
  filter(ProjectType %in% c(3,9,10,13)) %>%
  left_join(overlapNEW_services, by = "EnrollmentID") %>%
  select(!!overlapNEWvars)

c1 <- mainRecords
c2 <- mainRecords

overlapNEW_movein_and_exit <- c1 %>% 
  inner_join(c2, by = "PersonalID") %>%
  filter(
    ((ProjectType.x %in% c(3,9,10) & ProjectType.y %in% c(3,9,10)) | ProjectType.x == ProjectType.y) & 
    MoveInDate.y < ExitAdjust.x & 
    MoveInDate.x < ExitAdjust.y & 
    (MoveInDate.x < MoveInDate.y | (MoveInDate.x == MoveInDate.y & EnrollmentID.x > EnrollmentID.y))
  ) %>%
  mutate(
    Issue = "Overlaps Between Projects that Use Move-In Date to Exit Date to Indicate Occupancy",
    Type = "Error",
    Guidance = overlapNEW_movein_and_exit_guidance,
    FirstDateProvided.x = NA,
    FirstDateProvided.y = NA
  ) %>%
  select(-!!overlapNEWcols_to_remove) %>%
  distinct()

overlapNEW <- rbind(overlapNEW_entry_and_exit,
                    overlapNEW_entry_and_exit_bn,
                    overlapNEW_entry_and_exit_bn2,
                    overlapNEW_movein_and_exit)

rm(overlapNEW_entry_and_exit,
   overlapNEW_entry_and_exit_bn,
   overlapNEW_entry_and_exit_bn2,
   overlapNEW_movein_and_exit,
   overlapNEW_services)

# Invalid Move-in Date ----------------------------------------------------

invalid_movein_date <- served_in_date_range %>%
  filter(ProjectType %in% c(3, 9, 10, 13)) %>%
  mutate(
    Issue = case_when(
      (!is.na(MoveInDate) & MoveInDate < EntryDate) | 
      (!is.na(MoveInDate) & MoveInDate > ExitAdjust) ~ 
        "Invalid Move-In Date"
    ),
    Type = "Error",
    Guidance = "This move-in date does not fall between the Entry Date 
    and the Exit Date or this move-in date is after the date of the export.") %>%
  filter(Issue == "Invalid Move-In Date") %>%
  select(all_of(vars_we_want))

# Missing Health Ins ------------------------------------------------------

missing_health_insurance_entry <- served_in_date_range %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         DataCollectionStage,
         InsuranceFromAnySource) %>%
  filter(DataCollectionStage == 1 &
           (InsuranceFromAnySource == 99 |
              is.na(InsuranceFromAnySource))) %>%
  mutate(Issue = "Health Insurance Missing at Entry",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

missing_health_insurance_exit <- served_in_date_range %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(all_of(vars_prep),
         DataCollectionStage,
         InsuranceFromAnySource) %>%
  filter(DataCollectionStage == 3 &
           (InsuranceFromAnySource == 99 |
              is.na(InsuranceFromAnySource))) %>%
  mutate(Issue = "Health Insurance Missing at Exit",
         Type = "Error",
         Guidance = guidance_missing_at_exit) %>%
  select(all_of(vars_we_want))

health_insurance_subs <- served_in_date_range %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    all_of(vars_prep),
    DataCollectionStage,
    InsuranceFromAnySource,
    Medicaid,
    Medicare,
    SCHIP,
    VAMedicalServices,
    EmployerProvided,
    COBRA,
    PrivatePay,
    StateHealthIns,
    IndianHealthServices,
    OtherInsurance,
    HIVAIDSAssistance,
    ADAP
  ) %>%
  mutate(
    SourceCount = Medicaid + SCHIP + VAMedicalServices + EmployerProvided +
      COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
      OtherInsurance + Medicare
  )

conflicting_health_insurance_entry <- health_insurance_subs %>%
  filter(DataCollectionStage == 1 &
           ((InsuranceFromAnySource == 1 &
               SourceCount == 0) |
              (InsuranceFromAnySource == 0 &
                 SourceCount > 0)
           )) %>%
  mutate(Issue = "Conflicting Health Insurance yes/no at Entry",
         Type = "Error",
         Guidance = guidance_conflicting_hi) %>%
  select(all_of(vars_we_want))

conflicting_health_insurance_exit <- health_insurance_subs %>%
  filter(DataCollectionStage == 3 &
           ((InsuranceFromAnySource == 1 &
               SourceCount == 0) |
              (InsuranceFromAnySource == 0 &
                 SourceCount > 0)
           )) %>%
  mutate(
    Issue = "Conflicting Health Insurance yes/no at Exit",
    Type = "Error",
    Guidance = guidance_conflicting_hi
  ) %>%
  select(all_of(vars_we_want))

rm(health_insurance_subs)

# Missing NCBs at Entry ---------------------------------------------------

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
  )

ncb_subs[is.na(ncb_subs)] <- 0

ncb_subs <- ncb_subs %>%
  full_join(IncomeBenefits[c("PersonalID",
                             "EnrollmentID",
                             "DataCollectionStage",
                             "BenefitsFromAnySource")],
            by = c("PersonalID",
                   "EnrollmentID",
                   "DataCollectionStage"))

ncb_subs <- served_in_date_range %>%
  filter(ProjectName != "Unsheltered Clients - OUTREACH") %>%
  left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    DataCollectionStage,
    BenefitsFromAnySource,
    SNAP,
    WIC,
    TANFChildCare,
    TANFTransportation,
    OtherTANF,
    OtherBenefitsSource
  ) %>%
  mutate(
    BenefitCount = SNAP + WIC + TANFChildCare + TANFTransportation +
      OtherTANF + OtherBenefitsSource
  ) %>%
  select(PersonalID,
         EnrollmentID,
         DataCollectionStage,
         BenefitsFromAnySource,
         BenefitCount) %>%
  unique()

missing_ncbs_entry <- served_in_date_range %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(AgeAtEntry,
         all_of(vars_prep),
         DataCollectionStage,
         BenefitsFromAnySource) %>%
  filter(
    DataCollectionStage == 1 &
      (AgeAtEntry > 17 |
         is.na(AgeAtEntry)) &
      (BenefitsFromAnySource == 99 |
         is.na(BenefitsFromAnySource))
  ) %>%
  mutate(Issue = "Non-cash Benefits Missing at Entry",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

conflicting_ncbs_entry <- served_in_date_range %>%
  left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
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
  mutate(Issue = "Conflicting Non-cash Benefits yes/no at Entry",
         Type = "Error",
         Guidance = guidance_conflicting_ncbs) %>%
  select(all_of(vars_we_want))
    
# Missing Health Ins ------------------------------------------------------
    # 
    # missing_health_insurance_entry <- served_in_date_range %>%
    #   left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    #   select(all_of(vars_prep),
    #          AgeAtEntry,
    #          DataCollectionStage,
    #          InsuranceFromAnySource) %>%
    #   filter(DataCollectionStage == 1 &
    #            (InsuranceFromAnySource == 99 |
    #               is.na(InsuranceFromAnySource))) %>%
    #   mutate(Issue = "Health Insurance Missing at Entry",
    #          Type = "Error",
    #          Guidance = guidance_missing_at_entry) %>%
    #   select(all_of(vars_we_want))
    # 
    # missing_health_insurance_exit <- served_in_date_range %>%
    #   left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    #   select(all_of(vars_prep),
    #          DataCollectionStage,
    #          InsuranceFromAnySource) %>%
    #   filter(DataCollectionStage == 3 &
    #            (InsuranceFromAnySource == 99 |
    #               is.na(InsuranceFromAnySource))) %>%
    #   mutate(Issue = "Health Insurance Missing at Exit",
    #          Type = "Error",
    #          Guidance = guidance_missing_at_exit) %>%
    #   select(all_of(vars_we_want))
    # 
    # health_insurance_subs <- served_in_date_range %>%
    #   left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    #   select(
    #     all_of(vars_prep),
    #     DataCollectionStage,
    #     InsuranceFromAnySource,
    #     Medicaid,
    #     Medicare,
    #     SCHIP,
    #     VAMedicalServices,
    #     EmployerProvided,
    #     COBRA,
    #     PrivatePay,
    #     StateHealthIns,
    #     IndianHealthServices,
    #     OtherInsurance,
    #     HIVAIDSAssistance,
    #     ADAP
    #   ) %>%
    #   mutate(
    #     SourceCount = Medicaid + SCHIP + VAMedicalServices + EmployerProvided +
    #       COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
    #       OtherInsurance + Medicare
    #   )
    # 
    # conflicting_health_insurance_entry <- health_insurance_subs %>%
    #   filter(DataCollectionStage == 1 &
    #            ((InsuranceFromAnySource == 1 &
    #                SourceCount == 0) |
    #               (InsuranceFromAnySource == 0 &
    #                  SourceCount > 0)
    #            )) %>%
    #   mutate(Issue = "Conflicting Health Insurance yes/no at Entry",
    #          Type = "Error",
    #          Guidance = guidance_conflicting_hi) %>%
    #   select(all_of(vars_we_want))
    # 
    # conflicting_health_insurance_exit <- health_insurance_subs %>%
    #   filter(DataCollectionStage == 3 &
    #            ((InsuranceFromAnySource == 1 &
    #                SourceCount == 0) |
    #               (InsuranceFromAnySource == 0 &
    #                  SourceCount > 0)
    #            )) %>%
    #   mutate(
    #     Issue = "Conflicting Health Insurance yes/no at Exit",
    #     Type = "Error",
    #     Guidance = guidance_conflicting_hi
    #   ) %>%
    #   select(all_of(vars_we_want))
    # 
    # rm(health_insurance_subs)
    
# Missing NCBs at Entry ---------------------------------------------------
#     
#     ncb_subs <- IncomeBenefits %>%
#       select(
#         PersonalID,
#         EnrollmentID,
#         DataCollectionStage,
#         SNAP,
#         WIC,
#         TANFChildCare,
#         TANFTransportation,
#         OtherTANF,
#         OtherBenefitsSource
#       )
#     
#     ncb_subs[is.na(ncb_subs)] <- 0
#     
#     ncb_subs <- ncb_subs %>%
#       full_join(IncomeBenefits[c("PersonalID",
#                                  "EnrollmentID",
#                                  "DataCollectionStage",
#                                  "BenefitsFromAnySource")],
#                 by = c("PersonalID",
#                        "EnrollmentID",
#                        "DataCollectionStage"))
#     
#     ncb_subs <- served_in_date_range %>%
#       filter(ProjectName != "Unsheltered Clients - OUTREACH") %>%
#       left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
#       select(
#         PersonalID,
#         EnrollmentID,
#         HouseholdID,
#         AgeAtEntry,
#         ProjectName,
#         EntryDate,
#         MoveInDateAdjust,
#         ExitDate,
#         ProjectType,
#         DataCollectionStage,
#         BenefitsFromAnySource,
#         SNAP,
#         WIC,
#         TANFChildCare,
#         TANFTransportation,
#         OtherTANF,
#         OtherBenefitsSource
#       ) %>%
#       mutate(
#         BenefitCount = SNAP + WIC + TANFChildCare + TANFTransportation +
#           OtherTANF + OtherBenefitsSource
#       ) %>%
#       select(PersonalID,
#              EnrollmentID,
#              DataCollectionStage,
#              BenefitsFromAnySource,
#              BenefitCount) %>%
#       unique()
#     
#     missing_ncbs_entry <- served_in_date_range %>%
#       left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
#       select(AgeAtEntry,
#              all_of(vars_prep),
#              DataCollectionStage,
#              BenefitsFromAnySource) %>%
#       filter(
#         DataCollectionStage == 1 &
#           (AgeAtEntry > 17 |
#              is.na(AgeAtEntry)) &
#           (BenefitsFromAnySource == 99 |
#              is.na(BenefitsFromAnySource))
#       ) %>%
#       mutate(Issue = "Non-cash Benefits Missing at Entry",
#              Type = "Error",
#              Guidance = guidance_missing_at_entry) %>%
#       select(all_of(vars_we_want))
#     
#     conflicting_ncbs_entry <- served_in_date_range %>%
#       left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
#       select(AgeAtEntry,
#              all_of(vars_prep),
#              DataCollectionStage,
#              BenefitsFromAnySource,
#              BenefitCount) %>%
#       filter(DataCollectionStage == 1 &
#                (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
#                ((BenefitsFromAnySource == 1 &
#                    BenefitCount == 0) |
#                   (BenefitsFromAnySource == 0 &
#                      BenefitCount > 0)
#                )) %>%
#       mutate(Issue = "Conflicting Non-cash Benefits yes/no at Entry",
#              Type = "Error",
#              Guidance = guidance_conflicting_ncbs) %>%
#       select(all_of(vars_we_want))
#     
#     
# # Missing NCBs at Exit ----------------------------------------------------
#     missing_ncbs_exit <- served_in_date_range %>%
#       left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
#       select(AgeAtEntry,
#              all_of(vars_prep),
#              DataCollectionStage,
#              BenefitsFromAnySource) %>%
#       filter(
#         DataCollectionStage == 3 &
#           (AgeAtEntry > 17 |
#              is.na(AgeAtEntry)) &
#           (BenefitsFromAnySource == 99 |
#              is.na(BenefitsFromAnySource))
#       ) %>%
#       mutate(Issue = "Non-cash Benefits Missing at Exit",
#              Type = "Error",
#              Guidance = guidance_missing_at_exit) %>%
#       select(all_of(vars_we_want))
#     
#     conflicting_ncbs_exit <- served_in_date_range %>%
#       left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
#       select(
#         AgeAtEntry,
#         all_of(vars_prep),
#         DataCollectionStage,
#         BenefitsFromAnySource,
#         BenefitCount
#       ) %>%
#       filter(DataCollectionStage == 3 &
#                (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
#                ((BenefitsFromAnySource == 1 &
#                    BenefitCount == 0) |
#                   (BenefitsFromAnySource == 0 &
#                      BenefitCount > 0)
#                )) %>%
#       mutate(Issue = "Conflicting Non-cash Benefits yes/no at Exit",
#              Type = "Error",
#              Guidance = guidance_conflicting_ncbs) %>%
#       select(all_of(vars_we_want))
#     
#     rm(ncb_subs)
    
# Non HoHs w Svcs or Referrals --------------------------------------------
# SSVF projects should be showing this as an Error, whereas non-SSVF projects
# should be showing it as a warning, and only back to Feb of 2019
# services_on_hh_members <- served_in_date_range %>%
#   select(all_of(vars_prep),
#          ProjectID,
#          EnrollmentID,
#          RelationshipToHoH) %>%
#   filter(
#     RelationshipToHoH != 1 &
#       ProjectID %in% c(ssvf_funded)
#   ) %>%
#   semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
#   mutate(Issue = "Service Transaction on a Non Head of Household",
#          Type = "Warning",
#          Guidance = guidance_service_on_non_hoh) %>%
#   select(all_of(vars_we_want))
# 
# services_on_hh_members_ssvf <- served_in_date_range %>%
#   select(all_of(vars_prep),
#          ProjectID,
#          EnrollmentID,
#          RelationshipToHoH) %>%
#   filter(RelationshipToHoH != 1 &
#            ProjectID %in% c(ssvf_funded)) %>%
#   semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
#   mutate(Issue = "Service Transaction on a Non Head of Household (SSVF)",
#          Type = "Error",
#          Guidance = guidance_service_on_non_hoh) %>%
#   select(all_of(vars_we_want))

# SSVF --------------------------------------------------------------------

ssvf_served_in_date_range <- served_in_date_range %>%
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
        LastPermanentStreet,
        LastPermanentCity,
        LastPermanentState,
        LastPermanentZIP,
        AddressDataQuality,
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

veteran_missing_year_entered <- ssvf_served_in_date_range %>%
  filter(VeteranStatus == 1) %>%
  mutate(
    Issue = case_when(
      is.na(YearEnteredService) ~ "Missing Year Entered Service",
      YearEnteredService > year(today()) ~ "Incorrect Year Entered Service"),
    Type = "Error",
    Guidance = guidance_missing_at_entry
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

veteran_missing_year_separated <- ssvf_served_in_date_range %>%
  filter(VeteranStatus == 1) %>%
  mutate(
    Issue = case_when(
      is.na(YearSeparated) ~ "Missing Year Separated",
      YearSeparated > year(today()) ~ "Incorrect Year Separated"),
    Type = "Error",
    Guidance = guidance_missing_at_entry
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

veteran_missing_wars <- ssvf_served_in_date_range %>%
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
  mutate(Issue = "Missing War(s)",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

veteran_missing_branch <- ssvf_served_in_date_range %>%
  filter(VeteranStatus == 1 &
           is.na(MilitaryBranch)) %>%
  mutate(Issue = "Missing Military Branch",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

veteran_missing_discharge_status <- ssvf_served_in_date_range %>%
  filter(VeteranStatus == 1 & is.na(DischargeStatus)) %>%
  mutate(Issue = "Missing Discharge Status",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dkr_client_veteran_info <- ssvf_served_in_date_range %>%
  filter(VeteranStatus == 1) %>%
  mutate(
    Issue = case_when(
      WorldWarII %in% c(8, 9) |
        KoreanWar %in% c(8, 9) |
        VietnamWar %in% c(8, 9) |
        DesertStorm  %in% c(8, 9) |
        AfghanistanOEF %in% c(8, 9) |
        IraqOIF %in% c(8, 9) |
        IraqOND %in% c(8, 9) |
        OtherTheater  %in% c(8, 9)  ~ "Don't Know/Refused War(s)",
      MilitaryBranch %in% c(8, 9) ~ "Don't Know/Refused Military Branch",
      DischargeStatus %in% c(8, 9) ~ "Don't Know/Refused Discharge Status"
    ),
    Type = "Warning",
    Guidance = guidance_dkr_data
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

ssvf_missing_percent_ami <- ssvf_served_in_date_range %>%
  filter(RelationshipToHoH == 1 &
           is.na(PercentAMI)) %>%
  mutate(Issue = "Missing Percent AMI",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

ssvf_missing_vamc <- ssvf_served_in_date_range %>%
  filter(RelationshipToHoH == 1 &
           is.na(VAMCStation)) %>%
  mutate(Issue = "Missing VAMC Station Number",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

ssvf_missing_address <- ssvf_served_in_date_range %>%
  filter(RelationshipToHoH == 1 &
           (
             is.na(LastPermanentStreet) |
               is.na(LastPermanentCity) |
               is.na(LastPermanentState) |
               is.na(LastPermanentZIP)
           )) %>%
  mutate(Issue = "Missing Some or All of Last Permanent Address",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

ssvf_hp_screen <- ssvf_served_in_date_range %>%
  filter(ProjectType == 12 &
           RelationshipToHoH == 1 &
           (is.na(HPScreeningScore) |
              is.na(ThresholdScore))) %>%
  mutate(Issue = "Missing HP Screening or Threshold Score",
         Type = "Error",
         Guidance = guidance_missing_at_entry) %>%
  select(all_of(vars_we_want))

    # All together now --------------------------------------------------------
    dq_main <- rbind(
      conflicting_health_insurance_entry,
      conflicting_health_insurance_exit,
      conflicting_income_entry,
      conflicting_income_exit,
      conflicting_ncbs_entry,
      dkr_client_veteran_info,
      dkr_destination,
      dkr_living_situation,
      dkr_LoS,
      dkr_months_times_homeless,
      dkr_residence_prior,
      dq_dob,
      dq_ethnicity,
      dq_gender,
      dq_name,
      dq_race,
      dq_ssn,
      dq_veteran,
      duplicate_ees,
      extremely_long_stayers,
      future_ees,
      future_exits,
      hh_issues,
      invalid_months_times_homeless,
      invalid_movein_date,
      missing_approx_date_homeless,
      missing_client_location,
      missing_destination,
      missing_disabilities,
      missing_health_insurance_entry,
      missing_health_insurance_exit,
      missing_income_entry,
      missing_income_exit,
      missing_living_situation,
      missing_LoS,
      missing_months_times_homeless,
      missing_previous_street_ESSH,
      missing_ncbs_entry,
      missing_residence_prior,
      # overlapNEW, <- needs to only display the dq_main columns to be included
                  # in system and org-wide analyses
      ssvf_missing_address,
      ssvf_missing_vamc,
      ssvf_missing_percent_ami,      
      ssvf_hp_screen,
      veteran_missing_year_entered,
      veteran_missing_year_separated,
      veteran_missing_wars,
      veteran_missing_branch,
      veteran_missing_discharge_status,
      active_outside_dates,
      exit_before_start
    ) %>%
  unique() %>%
  mutate(Type = factor(Type, levels = c("High Priority",
                                        "Error",
                                        "Warning")))
    
   dq_providers <- sort(projects_current_hmis$ProjectName)
   
   # Controls what is shown in the System-Level DQ tab ------------------------
   
   dq_w_organization_names <- dq_main %>%
     left_join(Organization[c("OrganizationID", "OrganizationName")], by = "OrganizationName")
   
   # Controls what is shown in the Organization-Level DQ tab ------------------------
   
   dq_w_ids <- dq_main %>%
     left_join(Organization[c("OrganizationID", "OrganizationName")], by = "OrganizationName")
     # left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")
     

# Plots for System-Level DQ Tab -------------------------------------------
   
   # Top orgs with Errors - High Priority
   dq_data_high_priority_errors_org_level_plot <- dq_w_organization_names %>%
     filter(
       Type %in% c("High Priority") &
         !Issue %in% c(
           "No Head of Household",
           "Missing Relationship to Head of Household",
           "Too Many Heads of Household",
           "Children Only Household"
         )
     ) %>%
     select(PersonalID, OrganizationID, OrganizationName) %>%
     unique() %>%
     group_by(OrganizationName, OrganizationID) %>%
     summarise(clientsWithErrors = n()) %>%
     ungroup() %>%
     arrange(desc(clientsWithErrors))
   
   dq_data_high_priority_errors_org_level_plot$hover <-
     with(dq_data_high_priority_errors_org_level_plot,
          paste0(OrganizationName))
   
   dq_plot_organizations_high_priority_errors <-
     ggplot(
       head(dq_data_high_priority_errors_org_level_plot, 10L),
       aes(
         x = reorder(hover, clientsWithErrors),
         y = clientsWithErrors
       )
     ) +
     geom_col(show.legend = FALSE,
              color = "#063a89",
              fill = "#063a89") +
     coord_flip() +
     labs(x = "",
          y = "Number of Clients with High Priority Errors") +
     theme_classic() +
     theme(axis.line = element_line(linetype = "blank"),
           axis.text.x = element_blank(),
           axis.ticks = element_line(linetype = "blank"),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) +
     geom_text(aes(label = clientsWithErrors), hjust = -0.5, color = "black")
   
   # Most common high priority errors system-wide
   
   dq_data_high_priority_error_types_org_level <- dq_w_organization_names %>%
     filter(Type %in% c("High Priority")) %>%
     group_by(Issue) %>%
     summarise(Errors = n()) %>%
     ungroup() %>%
     arrange(desc(Errors))
   
   dq_plot_high_priority_errors_org_level <-
     ggplot(head(dq_data_high_priority_error_types_org_level, 10L),
            aes(
              x = reorder(Issue, Errors),
              y = Errors
            )) +
     geom_col(show.legend = FALSE,
              color = "#063A89",
              fill = "#063a89") +
     coord_flip() +
     labs(x = "",
          y = "Number of Clients with High Piority Errors") +
     theme_classic() +
     theme(axis.line = element_line(linetype = "blank"),
           axis.text.x = element_blank(),
           axis.ticks = element_line(linetype = "blank"),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) +
     geom_text(aes(label = Errors), hjust = -0.5, color = "black")
   
   # Top orgs with Errors - General
   
   dq_data_errors_org_level_plot <- dq_w_organization_names %>%
     filter(
       Type %in% c("Error") &
         !Issue %in% c(
           "No Head of Household",
           "Missing Relationship to Head of Household",
           "Too Many Heads of Household",
           "Children Only Household"
         )
     ) %>%
     select(PersonalID, OrganizationID, OrganizationName) %>%
     unique() %>%
     group_by(OrganizationName, OrganizationID) %>%
     summarise(clientsWithErrors = n()) %>%
     ungroup() %>%
     arrange(desc(clientsWithErrors))
   
   dq_data_errors_org_level_plot$hover <-
     with(dq_data_errors_org_level_plot,
          paste0(OrganizationName))
   
   dq_plot_organizations_errors <-
     ggplot(
       head(dq_data_errors_org_level_plot, 10L),
       aes(
         x = reorder(hover, clientsWithErrors),
         y = clientsWithErrors
       )
     ) +
     geom_col(show.legend = FALSE,
              color = "#063a89",
              fill = "#063a89") +
     coord_flip() +
     labs(x = "",
          y = "Number of Clients with General Errors") +
     theme_classic() +
     theme(axis.line = element_line(linetype = "blank"),
           axis.text.x = element_blank(),
           axis.ticks = element_line(linetype = "blank"),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) +
     geom_text(aes(label = clientsWithErrors), hjust = -0.5, color = "black")
   
   # Most common general errors system-wide
   
   dq_data_error_types_org_level <- dq_w_organization_names %>%
     filter(Type %in% c("Error")) %>%
     group_by(Issue) %>%
     summarise(Errors = n()) %>%
     ungroup() %>%
     arrange(desc(Errors))
   
   dq_plot_errors_org_level <-
     ggplot(head(dq_data_error_types_org_level, 10L),
            aes(
              x = reorder(Issue, Errors),
              y = Errors
            )) +
     geom_col(show.legend = FALSE,
              color = "#063A89",
              fill = "#063a89") +
     coord_flip() +
     labs(x = "",
          y = "Number of Clients with General Errors") +
     theme_classic() +
     theme(axis.line = element_line(linetype = "blank"),
           axis.text.x = element_blank(),
           axis.ticks = element_line(linetype = "blank"),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) +
     geom_text(aes(label = Errors), hjust = -0.5, color = "black")
   
   #Top orgs with warnings
   
   dq_data_warnings_org_level_plot <- dq_w_organization_names %>%
     filter(Type == "Warning") %>%
     group_by(OrganizationName, OrganizationID) %>%
     summarise(Warnings = n()) %>%
     ungroup() %>%
     arrange(desc(Warnings))
   
   dq_data_warnings_org_level_plot$hover <-
     with(dq_data_warnings_org_level_plot,
          paste0(OrganizationName))
   
   dq_plot_organizations_warnings <-
     ggplot(head(dq_data_warnings_org_level_plot, 10L),
            aes(
              x = reorder(hover, Warnings),
              y = Warnings
            )) +
     geom_col(show.legend = FALSE,
              color = "#063a89",
              fill = "#063A89") +
     coord_flip() +
     labs(x = "",
          y = "Number of Clients with Warnings") +
     theme_classic() +
     theme(axis.line = element_line(linetype = "blank"),
           axis.text.x = element_blank(),
           axis.ticks = element_line(linetype = "blank"),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) +
     geom_text(aes(label = Warnings), hjust = -0.5, color = "black")
   
   #Most common warnings system-wide
   
   dq_data_warning_types_org_level <- dq_w_organization_names %>%
     filter(Type == "Warning") %>%
     group_by(Issue) %>%
     summarise(Warnings = n()) %>%
     ungroup() %>%
     arrange(desc(Warnings))
   
   dq_plot_warnings_org_level <-
     ggplot(head(dq_data_warning_types_org_level, 10L),
            aes(
              x = reorder(Issue, Warnings),
              y = Warnings
            )) +
     geom_col(show.legend = FALSE,
              color = "#063A89",
              fill = "#063A89") +
     coord_flip() +
     labs(x = "",
          y = "Number of Clients with Warnings") +
     theme_classic() +
     theme(axis.line = element_line(linetype = "blank"),
           axis.text.x = element_blank(),
           axis.ticks = element_line(linetype = "blank"),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) +
     geom_text(aes(label = Warnings), hjust = -0.5, color = "black")
   
# Prepping dataframes for plots for Organization-Level DQ Tab -----------------
   
   # Top projects with Errors - High Priority
   dq_data_high_priority_errors_top_projects_df <- dq_main %>%
     filter(Type %in% c("High Priority")) %>%
     select(PersonalID, ProjectID, ProjectName, OrganizationName) %>%
     unique() %>%
     group_by(OrganizationName, ProjectName, ProjectID) %>%
     summarise(clientsWithErrors = n()) %>%
     ungroup() %>%
     arrange(desc(clientsWithErrors))
   
      # Most common high priority errors org-wide
   
   dq_data_high_priority_error_types_org_df <- dq_w_ids %>%
     filter(Type %in% c("High Priority")) %>%
     group_by(OrganizationName, Issue) %>%
     summarise(Errors = n()) %>%
     ungroup() %>%
     arrange(desc(Errors))
   
      # Top projects with Errors - General
   
   dq_data_errors_top_projects_df <- dq_w_ids %>%
     filter(Type %in% c("Error")) %>%
     select(PersonalID, ProjectID, ProjectName, OrganizationName) %>%
     unique() %>%
     group_by(OrganizationName, ProjectName, ProjectID) %>%
     summarise(clientsWithErrors = n()) %>%
     ungroup() %>%
     arrange(desc(clientsWithErrors))
   
      # Most common general errors org-wide
   
   dq_data_error_types_org_df <- dq_w_ids %>%
     filter(Type %in% c("Error")) %>%
     group_by(OrganizationName, Issue) %>%
     summarise(Errors = n()) %>%
     ungroup() %>%
     arrange(desc(Errors))
   
      #Top projects with warnings
   
   dq_data_warnings_top_projects_df <- dq_w_ids %>%
     filter(Type == "Warning") %>%
     group_by(OrganizationName, ProjectName, ProjectID) %>%
     summarise(Warnings = n()) %>%
     ungroup() %>%
     arrange(desc(Warnings))
   
      #Most common warnings org-wide
   
   dq_data_warning_types_org_df <- dq_w_ids %>%
     filter(Type == "Warning") %>%
     group_by(OrganizationName, Issue) %>%
     summarise(Warnings = n()) %>%
     ungroup() %>%
     arrange(desc(Warnings))
   
