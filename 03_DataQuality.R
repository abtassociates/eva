# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

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

# Providers to Check ------------------------------------------------------
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
  filter(served_between(., meta_HUDCSV_Export_Start, meta_HUDCSV_Export_End)) %>%
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
      FirstName == "Missing" ~ 
        "Missing Name Data Quality",
      FirstName %in% c("DKR", "Partial") ~
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
      DOBDataQuality == 99 ~ "Missing Date of Birth Data Quality",
      DOBDataQuality %in% c(2, 8, 9) ~ "Don't Know/Refused or Approx. Date of Birth",
      AgeAtEntry < 0 | AgeAtEntry > 100 ~ "Incorrect Date of Birth or Entry Date"
    ),
    Type = case_when(
      Issue %in% c(
        "Missing DOB",
        "Incorrect Date of Birth or Entry Date",
        "Missing Date of Birth Data Quality"
      ) ~ "Error",
      Issue ==  "Don't Know/Refused or Approx. Date of Birth" ~ "Warning"
    ),
    Guidance = case_when(
      Issue == "Incorrect Date of Birth or Entry Date" ~
        "The HMIS data is indicating the client entered the project PRIOR to
      being born. Correct either the Date of Birth or the Project Start Date, 
      whichever is incorrect.",
      Issue %in% c("Missing DOB", "Missing Date of Birth Data Quality") ~
        guidance_missing_at_entry,
      Issue == "Don't Know/Refused or Approx. Date of Birth" ~
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
  mutate(Issue = "Children Only Household",
         Type = "High Priority",
         Guidance = "Unless your project serves youth younger than 18 
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
  mutate(Days = as.numeric(difftime(today(), EntryDate))) %>%
  filter(is.na(ExitDate) &
           ProjectType == 2)

Top2_TH <- subset(th_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

rrh_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 13) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_End, EntryDate))) 

Top2_RRH <- subset(rrh_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

es_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 1) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_End, EntryDate))) 

Top2_ES <- subset(es_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

psh_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 3) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_End, EntryDate))) 

Top1_PSH <- subset(psh_stayers, Days > quantile(Days, prob = 1 - 1 / 100))

hp_stayers <- served_in_date_range %>%
  select(all_of(vars_prep), ProjectID) %>%
  filter(is.na(ExitDate) &
           ProjectType == 12) %>%
  mutate(Days = as.numeric(difftime(meta_HUDCSV_Export_End, EntryDate))) 

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

Top2_Outreach <- subset(outreach_stayers %>% select(-EnrollmentID, -latestInfoDate), Days > input$OutLongStayers)

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
    Guidance = "Fix Me"
  ) %>% 
  select(all_of(vars_we_want))

extremely_long_stayers <- rbind(extremely_long_stayers, Top2_movein %>% select(all_of(vars_we_want)))

rm(list = ls(pattern = "Top*"),
   es_stayers,
   th_stayers,
   psh_stayers,
   rrh_stayers,
   hp_stayers)

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

# Missing Project Stay or Incorrect Destination ---------------------------

# RRH

destination_rrh <- served_in_date_range %>%
  filter(Destination == 31)

# PSH

destination_psh <- served_in_date_range %>%
  filter(Destination == 3)

# TH
destination_th <- served_in_date_range %>%
  filter(Destination == 2)

# SH

destination_sh <- served_in_date_range %>%
  filter(Destination == 18)

# Check Eligibility, Project Type, Residence Prior ------------------------

# check_eligibility <- served_in_date_range %>%
#   select(
#     all_of(vars_prep),
#     ProjectID,
#     AgeAtEntry,
#     RelationshipToHoH,
#     LivingSituation,
#     LengthOfStay,
#     LOSUnderThreshold,
#     PreviousStreetESSH
#   ) %>%
#   filter(
#     RelationshipToHoH == 1 &
#       AgeAtEntry > 17 &
#       # EntryDate > hc_check_eligibility_back_to &
#       (ProjectType %in% c(3, 4, 8, 9, 10, 12, 13) |
#          (ProjectType == 2 & !ProjectID %in% c(rhy_funded))) &
#       (
#         (ProjectType %in% c(2, 3, 9, 10, 13) &
#            # PTCs that require LH status
#            (
#              is.na(LivingSituation) |
#                (
#                  LivingSituation %in% c(4:7, 15, 25:27, 29) & # institution
#                    (
#                      !LengthOfStay %in% c(2, 3, 10, 11) | # <90 days
#                        is.na(LengthOfStay) |
#                        PreviousStreetESSH == 0 | # LH prior
#                        is.na(PreviousStreetESSH)
#                    )
#                ) |
#                (
#                  LivingSituation %in% c(3, 10, 11, 14, 19:23, 28, 31, 35, 36) &
#                    # not homeless
#                    (
#                      !LengthOfStay %in% c(10, 11) |  # <1 week
#                        is.na(LengthOfStay) |
#                        PreviousStreetESSH == 0 | # LH prior
#                        is.na(PreviousStreetESSH)
#                    )
#                )
#            )) |
#           (
#             ProjectType == 12 &
#               (!LivingSituation %in% c(3, 10, 11, 14, 19:23, 28, 31, 35, 36) |
#               PreviousStreetESSH != 0 )
#           ) |
#           (ProjectType %in% c(8, 4) & # Safe Haven and Outreach
#              LivingSituation != 16) # unsheltered only
#       )
#   ) 
# 
#     detail_eligibility <- check_eligibility %>%
#       select(
#         OrganizationName,
#         PersonalID,
#         ProjectName,
#         ProjectType,
#         LivingSituation,
#         EntryDate,
#         ExitDate,
#         LengthOfStay,
#         LOSUnderThreshold,
#         PreviousStreetESSH
#       ) %>%
#       mutate(
#         ResidencePrior =
#           living_situation(LivingSituation),
#         LengthOfStay = case_when(
#           LengthOfStay == 2 ~ "One week or more but less than one month",
#           LengthOfStay == 3 ~ "One month or more but less than 90 days",
#           LengthOfStay == 4 ~ "90 days or more but less than one year",
#           LengthOfStay == 5 ~ "One year or longer",
#           LengthOfStay == 8 ~ "Client doesn't know",
#           LengthOfStay == 9 ~ "Client refused",
#           LengthOfStay == 10 ~ "One night or less",
#           LengthOfStay == 11 ~ "Two to six nights",
#           LengthOfStay == 99 ~ "Data not collected"
#         )
#       )
#     
#     check_eligibility <- check_eligibility %>%
#       mutate(
#         Issue = "Check Eligibility",
#         Type = "Warning",
#         Guidance = 
#           "Your Residence Prior data suggests that this project is either
#         serving ineligible households, the household was entered into the wrong
#         project, or the Residence Prior data at Entry is incorrect. Please check
#         the terms of your grant or speak with your CoC if you are unsure of 
#         eligibility criteria for your project type.") %>%
#       select(all_of(vars_we_want))
    
    # Missing Destination
    missing_destination <- served_in_date_range %>%
      filter(!is.na(ExitDate) &
               (is.na(Destination) | Destination %in% c(99, 30))) %>%
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
# this could be more nuanced but it's ok to leave it since we are also
# looking at overlaps
duplicate_ees <-
  get_dupes(served_in_date_range, PersonalID, ProjectID, EntryDate) %>%
  mutate(
    Issue = "Duplicate Entry Exits",
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

# future_exits <- Enrollment %>%
#   filter(ExitDate > meta_HUDCSV_Export_End) %>%
#   left_join(Project %>% select(ProjectID, OrganizationName), by = "ProjectID") %>%
#   mutate(
#     Issue = "Future Exit Date",
#     Type = "Error",
#     Guidance = "This client's Exit Date is a date in the future. Please 
#     enter the exact date the client left your program. If this client has not
#     yet exited, delete the Exit and then enter the Exit Date once the client
#     is no longer in your program."
#   ) %>%
#   select(all_of(vars_we_want))
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

# Not calculating Conflicting Income Amounts bc they're calculating the TMI from the
# subs instead of using the field itself. Understandable but that means I would
# have to pull the TMI data in through RMisc OR we kill TMI altogether. (We
# decided to kill TMI altogether.)

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
                   "ExitDate", 
                   "ExitAdjust",
                   "ProjectType", 
                   "PersonalID", 
                   "ProjectID")

overlapNEWc1Vars = c("PersonalID",
                     "EnrollmentID",
                     "EntryDate",
                     "ExitAdjust",
                     "ProjectType",
                     "TrackingMethod",
                     "ProjectID")

overlapNEWc2Vars = c("PersonalID",
                     "EnrollmentIDB" = "EnrollmentID",
                     "EntryDateB" = "EntryDate",
                     "ExitAdjustB" = "ExitAdjust",
                     "ProjectTypeB" = "ProjectType",
                     "TrackingMethodB" = "TrackingMethod",
                     "ProjectIDB" = "ProjectID")

overlapNEWFinalVars = c(
  "PersonalID",
  "EnrollmentID",
  # "EntryDate",
  # "ExitAdjust",
  "ProjectType",
  # "TrackingMethod",
  "ProjectID",
  # "DateProvided",
  # "MoveInDate",
  "EnrollmentIDB",
  # "EntryDateB",
  # "DateProvided_Or_ExitDateB",
  "ProjectTypeB",
  # "TrackingMethodB",
  "ProjectIDB"
  # "MoveInDateB"
)
## Overlaps Between Residential Projects that Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date -----
# This gets the enrollment records for the relevant project types
mainRecords <- served_in_date_range %>%
  filter((ProjectType == 1 & TrackingMethod == 0) | ProjectType %in% c(2,8)) %>%
  select(!!overlapNEWvars)


c1 <- mainRecords %>% select(!!overlapNEWc1Vars)
c2 <- mainRecords %>% select(!!overlapNEWc2Vars)
overlapNEW_entry_and_exit <- c1 %>% 
  inner_join(c2, by = "PersonalID") %>%
  filter(
    EntryDateB < ExitAdjust & 
    EntryDate < ExitAdjustB &
    (
      EntryDate < EntryDateB | 
      (EntryDate == EntryDateB & EnrollmentID > EnrollmentIDB)
    )
  ) %>%
  mutate(
    Issue = "Overlaps Between Residential Projects that Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date",
    Type = "Error",
    Guidance = overlapNEW_entry_and_exit_guidance
    # DateProvided_Or_ExitDateB = ExitAdjustB,
    # DateProvided=NA,
    # MoveInDate=NA, 
    # MoveInDateB=NA
  ) %>%
  select(!!overlapNEWFinalVars)
# colnames(overlapNEW_entry_and_exit)[colnames(overlapNEW_entry_and_exit) == "ExitAdjustB"] ="DateProvided_Or_ExitDateB"

# Overlaps Between Residential Projects That Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date AND Projects That Use Bed Night Date to indicate the household is occupying that unit on that date ----
# This gets the enrollment records that are night-by-night (i.e. have DateProvided in Services file)
# This gets the enrollment records that are ES (1, includes NbN), SH (8), and TH (2)
mainRecords <- served_in_date_range %>%
  filter(ProjectType %in% c(1,2,8)) %>%
  left_join(Services %>% select(EnrollmentID, DateProvided), by = "EnrollmentID") %>%
  select(!!overlapNEWvars, DateProvided)

c1 <- mainRecords %>% select(!!overlapNEWc1Vars, DateProvided)
c2 <- mainRecords %>% select(!!overlapNEWc2Vars, "DateProvidedB" = DateProvided)

# This flags the ones that overlap
initialFlags <- c1 %>% 
  inner_join(c2, by = "PersonalID") %>%
  mutate(IsOverlap = DateProvided >= EntryDateB & DateProvided <= ExitAdjustB,
         DateProvided_Or_ExitDateB = case_when(
           ProjectTypeB == 1 & TrackingMethodB == 3 ~ DateProvidedB,
           TRUE ~ ExitAdjustB
         )
  ) %>%
  filter(
    ProjectType == 1 & 
    TrackingMethod == 3 & (
      (EnrollmentID != EnrollmentIDB & (ProjectTypeB != 1 | TrackingMethodB != 3)) |
      (EnrollmentID > EnrollmentIDB & ProjectTypeB == 1 & TrackingMethodB == 3)
    ) & (
      (
        (
          (ProjectTypeB == 1 & TrackingMethodB == 0) | 
          ProjectTypeB %in% c(2,8)
        ) & 
        IsOverlap
      ) |
      ProjectTypeB == 1 & TrackingMethodB == 3 & DateProvided == DateProvidedB
    )
  )

numOverlaps <- initialFlags %>%
  select(EnrollmentID, IsOverlap) %>%
  group_by(EnrollmentID) %>%
  summarise(NumOverlaps = sum(IsOverlap)) %>%
  ungroup()


flaggedRecords <- initialFlags %>%
  left_join(numOverlaps, by = "EnrollmentID") %>%
  filter(NumOverlaps > 2 | (ProjectTypeB == 1 & TrackingMethodB == 3))


overlapNEW_entry_and_exit_bn <- flaggedRecords %>%
  # group_by(PersonalID, ProjectType, ProjectIDB, EnrollmentIDB, TrackingMethodB, EnrollmentID, ProjectTypeB, EntryDateB, DateProvided_Or_ExitDateB, NumOverlaps) %>%
  # summarise(FirstDateProvided = min(DateProvided)) %>%
  # ungroup() %>%
  mutate(
    Issue = "Overlaps Between Residential Projects That Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date AND Projects That Use Bed Night Date to indicate the household is occupying that unit on that date",
    Type = "Error",
    Guidance = overlapNEW_entry_and_exit_bn_guidance
    # DateProvided=NA, 
    # MoveInDate=NA, 
    # MoveInDateB=NA
  ) %>%
  select(!!overlapNEWFinalVars)

# Overlaps Between Residential Projects That Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) OR Bed Night Date to indicate the household is occupying that unit on that date AND Residential Projects That Use Housing Move-In Date and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date
# This gets the enrollment records that are PH-RRH (13) and PH-PSH (3), as well as ES (1, includes NbN (when TrackingMethod = 3)), SH (8), and TH (2); 3 and 13 are compared to the others (not each other or themselves)
mainRecords <- served_in_date_range %>%
  filter(ProjectType %in% c(1,2,8,3,13)) %>%
  left_join(Services %>% select(EnrollmentID, DateProvided), by = "EnrollmentID") %>%
  select(!!overlapNEWvars, DateProvided, MoveInDate)

c1 <- mainRecords %>% select(!!overlapNEWc1Vars, DateProvided, MoveInDate)
c2 <- mainRecords %>% select(!!overlapNEWc2Vars, "DateProvidedB" = DateProvided)

initialFlags <- c1 %>% 
  inner_join(c2, by = "PersonalID") %>%
  mutate(
    IsOverlap = ProjectType %in% c(3,13) & (
      (ProjectTypeB %in% c(2,8) & EntryDateB < ExitAdjust & MoveInDate < ExitAdjustB) |
      (ProjectTypeB == 1 & TrackingMethodB == 3 & DateProvidedB >= MoveInDate & DateProvidedB <= ExitAdjust) | 
      (EntryDateB < ExitAdjust & EntryDate < ExitAdjustB & EntryDate < EntryDateB)
    ),
    DateProvided_Or_ExitDateB = case_when(
       ProjectTypeB == 1 & TrackingMethodB == 3 ~ DateProvidedB,
       TRUE ~ ExitAdjustB
    )
  ) %>%
  filter(
    EnrollmentID != EnrollmentIDB & 
    ProjectType %in% c(3,13) & (
      (ProjectTypeB %in% c(2,8) & EntryDateB < ExitAdjust & MoveInDate < ExitAdjustB) |
      (ProjectTypeB == 1 & TrackingMethodB == 3 & DateProvidedB >= MoveInDate & DateProvidedB <= ExitAdjust) |
      (ProjectTypeB == 1 & EntryDateB < ExitAdjust & MoveInDate < ExitAdjustB)
    )
  )


numOverlaps <- initialFlags %>%
  select(EnrollmentID, IsOverlap) %>%
  group_by(EnrollmentID) %>%
  summarise(NumOverlaps = sum(IsOverlap)) %>%
  ungroup()


flaggedRecords <- initialFlags %>%
  left_join(numOverlaps, by = "EnrollmentID") %>%
  filter(
    (ProjectTypeB %in% c(2,8) & NumOverlaps > 0) |
    (ProjectTypeB %in% c(1) & NumOverlaps > 2)
  )

overlapNEW_entry_and_exit_bn2 <- flaggedRecords %>%
  # group_by(PersonalID, ProjectType, ProjectIDB, EnrollmentIDB, TrackingMethodB, EnrollmentID, ProjectTypeB, EntryDateB, DateProvided_Or_ExitDateB, NumOverlaps) %>%
  # summarise(DateProvided_Or_ExitDateB = min(DateProvided_Or_ExitDateB)) %>%
  # ungroup() %>%
  mutate(
    Issue = "Overlaps Between Residential Projects That Use Entry Date (Project Start Date) and Exit Date (Project Exit Date) OR Bed Night Date to indicate the household is occupying that unit on that date AND Residential Projects That Use Housing Move-In Date and Exit Date (Project Exit Date) to indicate the household is occupying that unit on that date",
    Type = "Error",
    Guidance = overlapNEW_entry_and_exit_bn2_guidance
    # DateProvided=NA,
    # MoveInDate=NA, 
    # MoveInDateB=NA
  ) %>%
  select(!!overlapNEWFinalVars)

# Overlaps Between Projects that Use Move-In Date to Exit Date to Indicate Occupancy ----
# This gets the enrollment records that are PH-RRH (13) and PH-PSH (3)
mainRecords <- served_in_date_range %>%
  filter(ProjectType %in% c(3,9,10,13)) %>%
  left_join(Services %>% select(EnrollmentID, DateProvided), by = "EnrollmentID") %>%
  select(!!overlapNEWvars, DateProvided, MoveInDate)

c1 <- mainRecords %>% select(!!overlapNEWc1Vars, MoveInDate)
c2 <- mainRecords %>% select(!!overlapNEWc2Vars, "MoveInDateB" = MoveInDate)

overlapNEW_movein_and_exit <- c1 %>% 
  inner_join(c2, by = "PersonalID") %>%
  filter(
    ((ProjectType %in% c(3,9,10) & ProjectTypeB %in% c(3,9,10)) | ProjectType == ProjectTypeB) & 
    MoveInDateB < ExitAdjust & 
    MoveInDate < ExitAdjustB & 
    (MoveInDate < MoveInDateB | (MoveInDate == MoveInDateB & EnrollmentID > EnrollmentIDB))
  ) %>%
  mutate(
    Issue = "Overlaps Between Projects that Use Move-In Date to Exit Date to Indicate Occupancy",
    Type = "Error",
    Guidance = overlapNEW_movein_and_exit_guidance
    # DateProvided_Or_ExitDateB = ExitAdjustB,
    # DateProvided=NA
  ) %>%
  select(!!overlapNEWFinalVars)

overlapNEW <- rbind(overlapNEW_entry_and_exit,
                    overlapNEW_entry_and_exit_bn,
                    overlapNEW_entry_and_exit_bn2,
                    overlapNEW_movein_and_exit)

rm(overlapNEW_entry_and_exit,
   overlapNEW_entry_and_exit_bn,
   overlapNEW_entry_and_exit_bn2,
   overlapNEW_movein_and_exit)

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


    # Overlapping Enrollment/Move In Dates ------------------------------------
    
    # this only pulls the most recent EE in the overlap and I think that's fine but
    # some users won't like being flagged for it if it's someone else's fault
    # but you can't tell whose fault it is from the data so...
    
    staging_overlaps <- served_in_date_range %>%
      select(all_of(vars_prep), ExitAdjust) %>%
      mutate(
        EntryAdjust = case_when(
          #for PSH and RRH, EntryAdjust = MoveInDate
          ProjectType %in% c(1, 2, 8, 12) |
            ProjectName == "Unsheltered Clients - OUTREACH" ~ EntryDate,
          ProjectType %in% c(3, 9, 13) &
            !is.na(MoveInDateAdjust) ~ MoveInDateAdjust,
          ProjectType %in% c(3, 9, 13) &
            is.na(MoveInDateAdjust) ~ EntryDate
        ),
        ExitAdjust = ExitAdjust - days(1),
        # bc a client can exit&enter same day
        LiterallyInProject = if_else(
          ProjectType %in% c(3, 9, 13),
          interval(MoveInDateAdjust, ExitAdjust),
          interval(EntryAdjust, ExitAdjust)
        ),
        Issue = "Overlapping Project Stays",
        Type = "High Priority",
        Guidance = "Fix Me"
      ) %>%
      filter(!is.na(LiterallyInProject) &
               int_length(LiterallyInProject) > 0) %>%
      get_dupes(., PersonalID) %>%
      group_by(PersonalID) %>%
      arrange(PersonalID, EntryAdjust) %>%
      mutate(
        PreviousEntryAdjust = lag(EntryAdjust),
        PreviousExitAdjust = lag(ExitAdjust),
        PreviousProject = lag(ProjectName)
      ) %>%
      filter(!is.na(PreviousEntryAdjust)) %>%
      ungroup()
    
    same_day_overlaps <- served_in_date_range %>%
      filter((ProjectType == 13 & MoveInDateAdjust == ExitDate) |
               ProjectType != 13) %>%
      select(all_of(vars_prep), ExitAdjust) %>%
      mutate(
        EntryAdjust = case_when(
          #for PSH and RRH, EntryAdjust = MoveInDate
          ProjectType %in% c(1, 2, 8, 12) |
            ProjectName == "Unsheltered Clients - OUTREACH" ~ EntryDate,
          ProjectType %in% c(3, 9, 13) &
            !is.na(MoveInDateAdjust) ~ MoveInDateAdjust,
          ProjectType %in% c(3, 9, 13) &
            is.na(MoveInDateAdjust) ~ EntryDate
        ),
        LiterallyInProject = case_when(
          ProjectType %in% c(3, 9) ~ interval(MoveInDateAdjust, ExitAdjust),
          ProjectType %in% c(1, 2, 4, 8, 12) ~ interval(EntryAdjust, ExitAdjust)
        ),
        Issue = "Overlapping Project Stays",
        Type = "Warning",
        Guidance = "Fix Me"
      ) %>%
      filter((!is.na(LiterallyInProject) & ProjectType != 13) |
               ProjectType == 13) %>%
      get_dupes(., PersonalID) %>%
      group_by(PersonalID) %>%
      arrange(PersonalID, EntryAdjust) %>%
      mutate(
        PreviousEntryAdjust = lag(EntryAdjust),
        PreviousExitAdjust = lag(ExitAdjust),
        PreviousProject = lag(ProjectName)
      ) %>%
      filter(ExitDate > PreviousEntryAdjust &
               ExitDate < PreviousExitAdjust) %>%
      ungroup() %>%
      select(all_of(vars_we_want), PreviousProject)
    
    rrh_overlaps <- served_in_date_range %>%
      select(all_of(vars_prep), ExitAdjust) %>%
      mutate(
        ExitAdjust = ExitAdjust - days(1),
        # bc a client can exit&enter same day
        InProject = interval(EntryDate, ExitAdjust),
        Issue = "Overlapping Project Stays",
        Type = "Warning",
        Guidance = "Fix Me"
      ) %>%
      filter(ProjectType == 13) %>%
      get_dupes(., PersonalID) %>%
      group_by(PersonalID) %>%
      arrange(PersonalID, EntryDate) %>%
      mutate(
        PreviousEntry = lag(EntryDate),
        PreviousExit = lag(ExitAdjust),
        PreviousProject = lag(ProjectName)
      ) %>%
      filter(!is.na(PreviousEntry)) %>%
      ungroup() %>%
      mutate(
        PreviousStay = interval(PreviousEntry, PreviousExit),
        Overlap = int_overlaps(InProject, PreviousStay)
      ) %>%
      filter(Overlap == TRUE) %>%
      select(all_of(vars_we_want), PreviousProject)
    
    psh_overlaps <- served_in_date_range %>%
      select(all_of(vars_prep), ExitAdjust) %>%
      mutate(
        ExitAdjust = ExitAdjust - days(1),
        # bc a client can exit&enter same day
        InProject = interval(EntryDate, ExitAdjust),
        Issue = "Overlapping Project Stays",
        Type = "Warning",
        Guidance = "Fix Me"
      ) %>%
      filter(ProjectType == 3) %>%
      get_dupes(., PersonalID) %>%
      group_by(PersonalID) %>%
      arrange(PersonalID, EntryDate) %>%
      mutate(
        PreviousEntry = lag(EntryDate),
        PreviousExit = lag(ExitAdjust),
        PreviousProject = lag(ProjectName)
      ) %>%
      filter(!is.na(PreviousEntry)) %>%
      ungroup() %>%
      mutate(
        PreviousStay = interval(PreviousEntry, PreviousExit),
        Overlap = int_overlaps(InProject, PreviousStay)
      ) %>%
      filter(Overlap == TRUE) %>%
      select(all_of(vars_we_want), PreviousProject)
    
    dq_overlaps <- staging_overlaps %>%
      mutate(
        PreviousStay = interval(PreviousEntryAdjust, PreviousExitAdjust),
        Overlap = int_overlaps(LiterallyInProject, PreviousStay)
      ) %>%
      filter(Overlap == TRUE) %>%
      select(all_of(vars_we_want), PreviousProject)
    
    dq_overlaps <-
      rbind(dq_overlaps, rrh_overlaps, psh_overlaps, same_day_overlaps) %>%
      unique() %>%
      mutate(
        Issue = "Overlapping Project Stays",
        Type = "Warning",
        Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor
        can they have a Move-In Date into a PSH or RRH project while they are
        still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's
        or any two PSH's simultaneously, housed or not.<br>
        Please look the client(s) up in HMIS and determine which project stay's
        Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the \"Previous 
        Provider's\" mistake, but if you are seeing clients here, it means your
        project stay was entered last. <br>
        If the overlap is not your project's mistake, please work with the project 
        that has the incorrect Entry/Move-In/or Exit Date to get this corrected 
        or send an email to hmis@cohhio.org if you cannot get it resolved. These 
        clients will NOT show on their Data Quality app. <br>
        If YOUR dates are definitely correct, it is fine to continue with other
        data corrections as needed."
      )
    
    rm(staging_overlaps,
       same_day_overlaps,
       rrh_overlaps,
       psh_overlaps)
    # 
    # # Missing Health Ins ------------------------------------------------------
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
    
#     # Missing NCBs at Entry ---------------------------------------------------
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

# # Old Outstanding Referrals -----------------------------------------------
# # CW says ProviderCreating should work instead of Referred-From Provider
# # Using ProviderCreating instead. Either way, I feel this should go in the
# # Provider Dashboard, not the Data Quality report.
# 
# internal_old_outstanding_referrals <- served_in_date_range %>%
#   semi_join(Referrals,
#             by = c("PersonalID")) %>%
#   left_join(Referrals,
#             by = c("PersonalID")) %>%
#   filter(ProviderCreating == ProjectName &
#            ProjectID != 1695) %>%
#   select(all_of(vars_prep),
#          ProviderCreating,
#          ReferralDate,
#          ReferralOutcome,
#          EnrollmentID) %>%
#   filter(is.na(ReferralOutcome) &
#            ReferralDate < today() - days(14)) %>%
#   mutate(
#     ProjectName = ProviderCreating,
#     Issue = "Old Outstanding Referral",
#     Type = "Warning",
#     Guidance = "Referrals should be closed in about 2 weeks. Please be sure you are
#   following up with any referrals and helping the client to find permanent
#   housing. Once a Referral is made, the receiving agency should be saving
#   the \"Referral Outcome\" once it is known. If you have Referrals that are
#   legitimately still open after 2 weeks because there is a lot of follow
#   up going on, no action is needed since the HMIS data is accurate."
#   ) %>%
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
      #check_disability_ssi,
      # check_eligibility,
      # conflicting_disabilities,
      conflicting_health_insurance_entry,
      conflicting_health_insurance_exit,
      conflicting_income_entry,
      conflicting_income_exit,
      conflicting_ncbs_entry,
      # conflicting_ncbs_exit,
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
      # incorrect_path_contact_date,
      invalid_months_times_homeless,
      # lh_without_spdat,
      #maybe_psh_destination,
      # maybe_rrh_destination,
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
      # missing_path_contact,
      missing_previous_street_ESSH,
      missing_ncbs_entry,
      # missing_ncbs_exit,
      missing_residence_prior,
      #no_bos_rrh,
      #no_bos_psh,
      #no_bos_th,
      #no_bos_sh,
      # path_enrolled_missing,
      # path_missing_los_res_prior,
      # path_no_status_at_exit,
      # path_reason_missing,
      # path_SOAR_missing_at_exit,
      # path_status_determination,
      # referrals_on_hh_members,
      # referrals_on_hh_members_ssvf,

      # rent_paid_no_move_in,
      # services_on_hh_members,
      # services_on_hh_members_ssvf,
      # should_be_psh_destination,
      # should_be_rrh_destination,
      # should_be_th_destination,
      # should_be_sh_destination,

      # spdat_on_non_hoh,
      ssvf_missing_address,
      ssvf_missing_vamc,
      ssvf_missing_percent_ami,      
      ssvf_hp_screen,
      # unlikely_ncbs_entry,
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

    # filtering out AP errors that are irrlevant to APs
    
    dq_main <- dq_main %>%
      filter(ProjectType != 14 |
               (
                 ProjectType == 14 &
                   Issue %in% c(
                     "60 Days in Mahoning Coordinated Entry",
                     "Access Point with Entry Exits",
                     "Missing Date of Birth Data Quality",
                     "Don't Know/Refused or Approx. Date of Birth",
                     "Missing DOB",
                     "Missing Name Data Quality",
                     "Incomplete or Don't Know/Refused Name",
                     "Rent Payment Made, No Move-In Date",
                     "Invalid SSN",
                     "Don't Know/Refused SSN",
                     "Missing SSN",
                     "Missing Veteran Status",
                     "Don't Know/Refused Veteran Status",
                     "Missing County Served",
                     "Children Only Household"
                   )
               ))
    
    # Controls what is shown in the CoC-wide DQ tab ---------------------------
    
    # for CoC-wide DQ tab

    dq_w_project_names <- dq_main %>%
      left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")
    
   dq_providers <- sort(projects_current_hmis$ProjectName)
   
   # Controls what is shown in the System-Level DQ tab ------------------------
   
   dq_w_organization_names <- dq_main %>%
     left_join(Organization[c("OrganizationID", "OrganizationName")], by = "OrganizationName")
   
   # Controls what is shown in the Organization-Level DQ tab ------------------------
   
   dq_w_ids <- dq_main %>%
     left_join(Organization[c("OrganizationID", "OrganizationName")], by = "OrganizationName") %>%
     left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")

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
           axis.ticks = element_line(linetype = "blank"),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) +
     geom_text(aes(label = Warnings), hjust = -0.5, color = "black")
   
# # Plots -------------------------------------------------------------------
#     
#     dq_data_errors_plot <- dq_w_project_names %>%
#       filter(
#         Type %in% c("Error", "High Priority") &
#           !Issue %in% c(
#             "No Head of Household",
#             "Missing Relationship to Head of Household",
#             "Too Many Heads of Household",
#             "Children Only Household"
#           )
#       ) %>%
#       select(PersonalID, ProjectID, ProjectName) %>%
#       unique() %>%
#       group_by(ProjectName, ProjectID) %>%
#       summarise(clientsWithErrors = n()) %>%
#       ungroup() %>%
#       arrange(desc(clientsWithErrors))
#     
#     dq_data_errors_plot$hover <-
#       with(dq_data_errors_plot,
#            paste0(ProjectName, ":", ProjectID))
#     
#     dq_plot_projects_errors <-
#       ggplot(
#         head(dq_data_errors_plot, 10L),
#         aes(
#           x = reorder(hover, clientsWithErrors),
#           y = clientsWithErrors,
#           fill = clientsWithErrors
#         )
#       ) +
#       geom_col(show.legend = FALSE) +
#       coord_flip() +
#       labs(x = "",
#            y = "Clients") +
#       scale_fill_viridis_c(direction = -1) +
#       theme_minimal(base_size = 18)
#     
#     dq_data_warnings_plot <- dq_w_project_names %>%
#       filter(Type == "Warning") %>%
#       group_by(ProjectName, ProjectID) %>%
#       summarise(Warnings = n()) %>%
#       ungroup() %>%
#       arrange(desc(Warnings))
#     
#     dq_data_warnings_plot$hover <-
#       with(dq_data_warnings_plot,
#            paste0(ProjectName, ":", ProjectID))
#     
#     dq_plot_projects_warnings <-
#       ggplot(head(dq_data_warnings_plot, 10L),
#              aes(
#                x = reorder(hover, Warnings),
#                y = Warnings,
#                fill = Warnings
#              )) +
#       geom_col(show.legend = FALSE) +
#       coord_flip() +
#       labs(x = "",
#            y = "Clients") +
#       scale_fill_viridis_c(direction = -1) +
#       theme_minimal(base_size = 18)
#     
#     dq_data_error_types <- dq_w_project_names %>%
#       filter(Type %in% c("Error", "High Priority")) %>%
#       group_by(Issue) %>%
#       summarise(Errors = n()) %>%
#       ungroup() %>%
#       arrange(desc(Errors))
#     
#     dq_plot_errors <-
#       ggplot(head(dq_data_error_types, 10L),
#              aes(
#                x = reorder(Issue, Errors),
#                y = Errors,
#                fill = Errors
#              )) +
#       geom_col(show.legend = FALSE) +
#       coord_flip() +
#       labs(x = "",
#            y = "Clients") +
#       scale_fill_viridis_c(direction = -1) +
#       theme_minimal(base_size = 18)
#     
#     dq_data_warning_types <- dq_w_project_names %>%
#       filter(Type == "Warning") %>%
#       group_by(Issue) %>%
#       summarise(Warnings = n()) %>%
#       ungroup() %>%
#       arrange(desc(Warnings))
#     
#     dq_plot_warnings <-
#       ggplot(head(dq_data_warning_types, 10L),
#              aes(
#                x = reorder(Issue, Warnings),
#                y = Warnings,
#                fill = Warnings
#              )) +
#       geom_col(show.legend = FALSE) +
#       coord_flip() +
#       labs(x = "",
#            y = "Clients") +
#       scale_fill_viridis_c(direction = -1) +
#       theme_minimal(base_size = 18)
#     
#     dq_data_hh_issues_plot <- dq_w_project_names %>%
#       filter(
#         Type %in% c("Error", "High Priority") &
#           Issue %in% c(
#             "Missing Relationship to Head of Household",
#             "No Head of Household",
#             "Too Many Heads of Household",
#             "Children Only Household"
#           )
#       ) %>%
#       select(PersonalID, ProjectID, ProjectName) %>%
#       unique() %>%
#       group_by(ProjectName, ProjectID) %>%
#       summarise(Households = n()) %>%
#       ungroup() %>%
#       arrange(desc(Households))
#     
#     dq_data_hh_issues_plot$hover <-
#       with(dq_data_hh_issues_plot,
#            paste0(ProjectName, ":", ProjectID))
#     
#     dq_plot_hh_errors <-
#       ggplot(head(dq_data_hh_issues_plot, 10L),
#              aes(
#                x = reorder(hover, Households),
#                y = Households,
#                fill = Households
#              )) +
#       geom_col(show.legend = FALSE) +
#       coord_flip() +
#       labs(x = "") +
#       scale_fill_viridis_c(direction = -1) +
#       theme_minimal(base_size = 18)
    
    # dq_data_eligibility_plot <- dq_w_project_names %>%
    #   filter(Type == "Warning" &
    #            Issue %in% c("Check Eligibility")) %>%
    #   select(PersonalID, ProjectID, ProjectName) %>%
    #   unique() %>%
    #   group_by(ProjectName, ProjectID) %>%
    #   summarise(Households = n()) %>%
    #   ungroup() %>%
    #   arrange(desc(Households))
    # 
    # dq_data_eligibility_plot$hover <-
    #   with(dq_data_eligibility_plot,
    #        paste0(ProjectName, ":", ProjectID))
    # 
    # dq_plot_eligibility <-
    #   ggplot(
    #     head(dq_data_eligibility_plot, 10L),
    #     aes(
    #       x = reorder(hover, Households),
    #       y = Households,
    #       fill = Households
    #     )
    #   ) +
    #   geom_col(show.legend = FALSE) +
    #   coord_flip() +
    #   labs(x = "") +
    #   scale_fill_viridis_c(direction = -1) +
    #   theme_minimal(base_size = 18)
       
# WARNING save.image does not save the environment properly, save must be used.
# save(list = ls(), file = "images/Data_Quality.RData", compress = FALSE)
    

