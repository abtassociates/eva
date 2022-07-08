# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
library(lubridate)
library(janitor)
# library(treemap)
library(plotly)
library(HMIS)

if (!exists("Enrollment")) load("images/COHHIOHMIS.RData")
if (!exists("tay")) {
  load("images/cohorts.RData")
  # rlang::env_binding_lock(environment(), ls())
}

# clients currently entered into a homeless project in our system

co_currently_homeless <- co_clients_served %>%
  filter((is.na(ExitDate) |
            ExitDate > today()) &
           (ProjectType %in% c(4, lh_project_types) |
              (
                ProjectType %in% c(ph_project_types) &
                  is.na(MoveInDateAdjust)
              ))) %>%
  select(
    PersonalID,
    ProjectName,
    ProjectType,
    HouseholdID,
    EnrollmentID,
    RelationshipToHoH,
    VeteranStatus,
    EntryDate,
    AgeAtEntry
  )

# Check Whether Each Client Has Income ---------------------------------

# getting income-related data and data collection stages. this will balloon
# out the number of rows per client, listing each yes/no update, then, using
# DateCreated, it picks out the most recent answer, keeping only that one

income_data <- co_currently_homeless %>%
  left_join(
    IncomeBenefits %>%
      select(
        PersonalID,
        EnrollmentID,
        IncomeFromAnySource,
        DateCreated,
        DataCollectionStage
      ),
    by = c("PersonalID", "EnrollmentID")
  ) %>%
  mutate(DateCreated = ymd_hms(DateCreated),
         IncomeFromAnySource = if_else(is.na(IncomeFromAnySource),
                                       if_else(AgeAtEntry >= 18 |
                                                 is.na(AgeAtEntry), 99, 0),
                                       IncomeFromAnySource)) %>%
  group_by(PersonalID, EnrollmentID) %>%
  arrange(desc(DateCreated)) %>%
  slice(1L) %>%
  ungroup() %>%
  select(PersonalID,
         EnrollmentID,
         IncomeFromAnySource)

# Check Whether Each Client Has Any Indication of Disability ------------

# this checks the enrollment's 1.3 and 4.02 records to catch potential 
# disabling conditions that may be used to determine PSH eligibility but 
# were not reported in 3.08. If any of these three data elements (1.3, 
# 4.02, 3.08) suggest the presence of a disabling condition, this section 
# flags that enrollment as belonging to a disabled client. Otherwise,
# the enrollment is marked not disabled.

extended_disability <- co_currently_homeless %>%
  left_join(Disabilities, by = c("EnrollmentID"))  %>%
  group_by(EnrollmentID) %>%
  mutate(D_Disability = if_else(DisabilityResponse == 1 &
                                  IndefiniteAndImpairs != 0, 1, 0),
         D_Disability = max(D_Disability)) %>%
  select(EnrollmentID, D_Disability) %>%
  left_join(IncomeBenefits, by = c("EnrollmentID")) %>%
  mutate(I_Disability = if_else(SSDI == 1 |
                                  VADisabilityService == 1 |
                                  VADisabilityNonService == 1 |
                                  PrivateDisability == 1, 
                                1, 0),
         I_Disability = max(I_Disability)) %>%
  select(EnrollmentID, D_Disability, I_Disability) %>%
  ungroup() %>%
  distinct() %>%
  left_join(Enrollment, by = c("EnrollmentID")) %>%
  mutate(any_disability = case_when(D_Disability == 1 |
                                    I_Disability == 1 |
                                    DisablingCondition == 1 ~ 1, 
                                    TRUE ~ 0)) %>%
  select(EnrollmentID, any_disability)

# adding household aggregations into the full client list
co_currently_homeless <- co_currently_homeless %>%
  left_join(
    income_data, 
    by = c("PersonalID", "EnrollmentID")) %>%
  left_join(extended_disability, by = "EnrollmentID") %>%
  left_join(
    Enrollment %>%
      select(EnrollmentID, PersonalID, HouseholdID, LivingSituation, 
             DateToStreetESSH, TimesHomelessPastThreeYears, ExitAdjust,
             MonthsHomelessPastThreeYears, DisablingCondition),
    by = c("PersonalID",
           "EnrollmentID",
           "HouseholdID")
  ) %>%
  mutate(SinglyChronic =
           if_else(((ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
                       !is.na(DateToStreetESSH)) |
                      (
                        MonthsHomelessPastThreeYears %in% c(112, 113) &
                          TimesHomelessPastThreeYears == 4 &
                          !is.na(MonthsHomelessPastThreeYears) &
                          !is.na(TimesHomelessPastThreeYears)
                      )
           ) &
             DisablingCondition == 1 &
             !is.na(DisablingCondition), 1, 0)) %>%
  group_by(PersonalID) %>%
  mutate(SinglyChronic = max(SinglyChronic)) %>%
  ungroup() %>%
  group_by(HouseholdID) %>%
  mutate(HouseholdSize = length(PersonalID),
         IncomeInHH = max(if_else(IncomeFromAnySource == 1, 100, IncomeFromAnySource)),
         IncomeInHH = if_else(IncomeInHH == 100, 1, IncomeInHH),
         DisabilityInHH = max(if_else(any_disability == 1, 1, 0)),
         ChronicStatus = if_else(max(SinglyChronic) == 1, "Chronic", "Not Chronic")
  ) %>%
  ungroup() %>%
  select("PersonalID", "ProjectName", "ProjectType", "HouseholdID", "EnrollmentID",
         "RelationshipToHoH", "VeteranStatus", "EntryDate", "AgeAtEntry",
         "DisablingCondition", "HouseholdSize", "IncomeInHH", "DisabilityInHH",
         "ChronicStatus")

# Account for Multiple EEs -------------------------------------------------

active_list <- co_currently_homeless %>%
  group_by(PersonalID) %>%
  
  # label all program as either literally homeless or a housing program
  mutate(PTCStatus = case_when(
    ProjectType %in% c(lh_project_types, 4) ~ "LH",
    ProjectType %in% c(ph_project_types) ~ "PH"
  ),
  PTCStatus = factor(
    PTCStatus,
    levels = c(
      "LH", "PH"
    )),
  
  # label all clients as literally homeless or in a housing program
  client_status = if_else(PTCStatus == "LH", 0, 1),
  client_status = max(client_status)
  ) %>%
  
  # if the client has at least one literally homeless entry, keep the most recent
  # otherwise, keep the most recent housing program entry
  arrange(PTCStatus, desc(EntryDate)) %>%
  slice(1L) %>%
  
  # apply human-readable status labels
  mutate(PTCStatus = if_else(
    client_status == 1,
    "Has Entry into RRH or PSH",
    "Currently Has No Entry into RRH or PSH"
  )) %>%
  ungroup() %>%
  select(-client_status)

# correcting for bad hh data (while also flagging it) ---------------------

# what household ids exist in the data?
ALL_HHIDs <- active_list %>% select(HouseholdID) %>% unique()

# marking who is a hoh (accounts for singles not marked as hohs in the data)
active_list <- active_list %>%
  mutate(
    RelationshipToHoH = if_else(is.na(RelationshipToHoH), 99, RelationshipToHoH),
    hoh = if_else(str_detect(HouseholdID, fixed("s_")) |
                    RelationshipToHoH == 1, 1, 0)) 

# what household ids exist if we only count those with a hoh?
HHIDs_in_current_logic <- active_list %>% 
  filter(hoh == 1) %>%
  select(HouseholdID) %>%
  unique()

# which hh ids did not have a hoh?
HHIDs_with_bad_dq <-
  anti_join(ALL_HHIDs, HHIDs_in_current_logic,
            by = "HouseholdID") 

# what household ids have multiple hohs?
mult_hohs <- active_list %>% 
  group_by(HouseholdID) %>% 
  summarise(hohs = sum(hoh)) %>%
  filter(hohs > 1) %>%
  select(HouseholdID)

# give me ALL household ids with some sort of problem
HHIDs_with_bad_dq <- rbind(HHIDs_with_bad_dq, mult_hohs)

# let's see those same household ids but with all the needed columns
HHIDs_with_bad_dq <-
  left_join(HHIDs_with_bad_dq, active_list, by = "HouseholdID")

rm(ALL_HHIDs, HHIDs_in_current_logic, mult_hohs)

# assigning hoh status to the oldest person in the hh
Adjusted_HoHs <- HHIDs_with_bad_dq %>%
  group_by(HouseholdID) %>%
  arrange(desc(AgeAtEntry)) %>% # picking oldest hh member
  slice(1L) %>% 
  mutate(correctedhoh = 1) %>%
  select(HouseholdID, PersonalID, EnrollmentID, correctedhoh) %>%
  ungroup()

# merging the "corrected" hohs back into the main dataset with a flag, then
# correcting the RelationshipToHoH
hohs <- active_list %>%
  left_join(Adjusted_HoHs,
            by = c("HouseholdID", "PersonalID", "EnrollmentID")) %>%
  mutate(RelationshipToHoH = if_else(correctedhoh == 1, 1, RelationshipToHoH)) %>%
  select(PersonalID, HouseholdID, correctedhoh)
  

active_list <- active_list %>%
  left_join(hohs, by = c("HouseholdID", "PersonalID")) %>%
  group_by(HouseholdID) %>%
  mutate(correctedhoh = if_else(is.na(correctedhoh), 0, 1),
         HH_DQ_Issue = max(correctedhoh)) %>%
  ungroup()

# COVID-19 ----------------------------------------------------------------

get_res_prior <- Enrollment %>%
  select(PersonalID, EntryDate, ExitDate, LivingSituation) %>%
  group_by(PersonalID) %>%
  arrange(desc(EntryDate)) %>%
  slice(1L)

covid_clients <- covid19 %>%
  mutate(
    COVID19AssessmentDate = ymd(COVID19AssessmentDate),
    ContactWithConfirmedDate = ymd(ContactWithConfirmedDate),
    ContactWithUnderInvestigationDate = ymd(ContactWithUnderInvestigationDate),
    TestDate = ymd(TestDate),
    DateUnderInvestigation = ymd(DateUnderInvestigation)
  ) %>%
  filter(ymd(COVID19AssessmentDate) >= ymd("20200401") &
           ymd(COVID19AssessmentDate) <= today()) %>%
  left_join(get_res_prior, by = "PersonalID") %>%
  mutate(LivingSituationDescr = living_situation(LivingSituation)) %>%
  as_tibble() %>%
  mutate(
    COVID19Priority = case_when(
      (
        Tested == 1 &
          TestResults == "Positive" &
          ymd(TestDate) > today() - days(14) &
          !is.na(TestDate)
      ) |
        # if tested positive in the past 14 days ^^
        (
          UnderInvestigation == 1 &
            ymd(DateUnderInvestigation) > today() - days(14)
        ) |
        (
          ContactWithConfirmedCOVID19Patient == 1 &
            (
              ymd(ContactWithConfirmedDate) >
                today() - days(14) |
                is.na(ContactWithConfirmedDate)
              # contact with definite COVID-19 in the past 14 days ^^
            )
        ) |
        (
          ContactWithUnderCOVID19Investigation == 1 &
            (
              ymd(ContactWithUnderInvestigationDate) >
                today() - days(14) |
                is.na(ContactWithUnderInvestigationDate)
            )
          # contact date with maybe COVID-19 was within the past 14 days ^^
        ) |
        (
          LivingSituation %in% c(7, 25) &
            EntryDate > today() - days(14) &
            EntryDate <= today()
        ) |
        # if the client came from jail or nursing home ^^
        (
          Symptom1BreathingDifficult +
            Symptom1Cough +
            Symptom2Chills +
            Symptom2SoreThroat +
            Symptom2Fever +
            Symptom2Headache +
            Symptom2LostTasteSmell +
            Symptom2MusclePain +
            Symptom2Congestion +
            Symptom2Nausea +
            Symptom2Diarrhea +
            Symptom2Weak) > 0 ~ 1, # "Needs Isolation/Quarantine"
      # if the client has any symptoms at all ^^
      (
        HealthRiskHistoryOfRespiratoryIllness +
          HealthRiskChronicIllness +
          HealthRiskOver65 +
          HealthRiskKidneyDisease +
          HealthRiskImmunocompromised +
          HealthRiskSmoke > 0
      )  ~ 2, # "Has Health Risk(s)",
      # if the client has any risks at all ^^
      TRUE ~ 4 # "No Known Risks or Exposure"
      # everyone else lands here ^
      # in the report, there will be another level: "Not Assessed Recently"
    )
  ) %>%
  select(PersonalID, COVID19Priority)

covid_hhs <- active_list %>%
  left_join(covid_clients, by = "PersonalID") %>%
  mutate(
    COVID19Priority = if_else(
      is.na(COVID19Priority),
      3, # "Not Assessed Recently"
      COVID19Priority
    )
  ) %>%
  group_by(HouseholdID) %>%
  mutate(COVID19Priority_hh = max(COVID19Priority)) %>%
  ungroup() %>%
  select(PersonalID, HouseholdID, COVID19Priority_hh) %>%
  mutate(
    COVID19Priority = case_when(
      COVID19Priority_hh == 1 ~ "Needs Isolation/Quarantine",
      COVID19Priority_hh == 2 ~ "Has Health Risk(s)",
      COVID19Priority_hh == 3 ~ "Not Assessed Recently",
      COVID19Priority_hh == 4 ~ "No Known Risks or Exposure"
    ),
    COVID19Priority = factor(
      COVID19Priority,
      levels = c(
        "Needs Isolation/Quarantine",
        "Has Health Risk(s)",
        "Not Assessed Recently",
        "No Known Risks or Exposure"
      )
    )
  ) %>%
  select(-COVID19Priority_hh)

# adding COVID19Priority to active list
active_list <- active_list %>%
  left_join(covid_hhs, by = c("PersonalID", "HouseholdID"))

# Adding in TAY, County, PHTrack ----------------------

# getting whatever data's needed from the Enrollment data frame, creating
# columns that tell us something about each household and some that are about
# each client
additional_data <- active_list %>%
  left_join(
    Enrollment %>%
      select(
        PersonalID,
        HouseholdID,
        CountyServed,
        PHTrack,
        ExpectedPHDate
      ),
    by = c("PersonalID", "HouseholdID")
  ) %>%
  group_by(HouseholdID) %>%
  mutate(
    CountyServed = if_else(is.na(CountyServed), "MISSING County", CountyServed),
    TAY = if_else(max(AgeAtEntry) < 25 & max(AgeAtEntry) >= 16, 1, 0),
    PHTrack = if_else(
      !is.na(PHTrack) &
        !is.na(ExpectedPHDate) &
        ymd(ExpectedPHDate) >= today(), PHTrack, NULL)
  ) %>%
  ungroup() %>%
  select(-AgeAtEntry)

# saving these new columns back to the active list
active_list <- additional_data



# County Guessing ---------------------------------------------------------

# replacing non-Unsheltered-Provider missings with County of the provider
county <- active_list %>%
  left_join(Project %>%
              select(ProjectName, ProjectCounty), by = "ProjectName") %>%
  mutate(
    CountyGuessed = if_else(CountyServed == "MISSING County", 1, 0),
    CountyServed = if_else(
      CountyServed == "MISSING County" &
        ProjectName != "Unsheltered Clients - OUTREACH",
      ProjectCounty,
      CountyServed
    ),
    ProjectCounty = NULL
  )

# replacing missings for the Unsheltered Provider with the County of the
# Default Provider of the person who entered the Enrollment (grrr!)
active_list <- county %>%
  left_join(Enrollment %>%
              select(EnrollmentID, UserCreating), by = "EnrollmentID") %>%
  mutate(
    UserID = as.numeric(gsub(pattern = '[^0-9\\.]', '', UserCreating, perl = TRUE))
    ) %>%
  left_join(Users %>%
              select(UserID, UserCounty), by = "UserID") %>%
  mutate(CountyServed = if_else(CountyServed == "MISSING County" &
                                  ProjectName == "Unsheltered Clients - OUTREACH",
                                UserCounty,
                                CountyServed)) %>%
  select(-starts_with("User"))

# Add in Score ------------------------------------------------------------

# taking the most recent score on the client, but this score cannot be over a
# year old.
scores_staging <- Scores %>%
  filter(ScoreDate > today() - years(1)) %>%
  group_by(PersonalID) %>%
  arrange(desc(ymd(ScoreDate))) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-ScoreDate)

active_list <- active_list %>%
  left_join(scores_staging, by = "PersonalID")

# Add Additional Chronic Statuses ---------------------------------------------

# adds current days in ES or SH projects to days homeless prior to entry and if
# it adds up to 365 or more, it marks the client as AgedIn
agedIntoChronicity <- active_list %>%
  left_join(Enrollment %>%
              select(EnrollmentID, PersonalID, HouseholdID, LivingSituation, 
                     DateToStreetESSH, TimesHomelessPastThreeYears, ExitAdjust,
                     MonthsHomelessPastThreeYears),
            by = c("PersonalID",
                   "EnrollmentID",
                   "HouseholdID")) %>%
  mutate(
    DaysHomelessInProject = difftime(ymd(ExitAdjust),
                                     ymd(EntryDate),
                                     units = "days"),
    DaysHomelessBeforeEntry = difftime(ymd(EntryDate),
                                       if_else(
                                         is.na(ymd(DateToStreetESSH)),
                                         ymd(EntryDate),
                                         ymd(DateToStreetESSH)
                                       ),
                                       units = "days"),
    ChronicStatus = if_else(
      ProjectType %in% c(1, 8) &
        ChronicStatus == "Not Chronic" &
        ymd(DateToStreetESSH) + days(365) > ymd(EntryDate) &
        !is.na(DateToStreetESSH) &
        DaysHomelessBeforeEntry + DaysHomelessInProject >= 365,
      "Aged In",
      ChronicStatus
    )
  ) %>%
  select(-DaysHomelessInProject,-DaysHomelessBeforeEntry)

# adds another ChronicStatus of "Nearly Chronic" which catches those hhs with
# almost enough times and months to qualify as Chronic
nearly_chronic <- agedIntoChronicity %>%
  mutate(
    ChronicStatus = if_else(
      ChronicStatus == "Not Chronic" &
        ((
          ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
            !is.na(DateToStreetESSH)
        ) |
          (
            MonthsHomelessPastThreeYears %in% c(110:113) &
              TimesHomelessPastThreeYears%in% c(3, 4) &
              !is.na(MonthsHomelessPastThreeYears) &
              !is.na(TimesHomelessPastThreeYears)
          )
        ) &
        DisablingCondition == 1 &
        !is.na(DisablingCondition),
      "Nearly Chronic",
      ChronicStatus
    )
  )
  
active_list <- active_list %>%
  select(-ChronicStatus) %>%
  left_join(
    nearly_chronic %>%
      select("PersonalID",
             "HouseholdID",
             "EnrollmentID",
             "ChronicStatus"),
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  ) %>%
  mutate(
    ChronicStatus = factor(
      ChronicStatus,
      levels = c(
        "Chronic",
        "Aged In",
        "Nearly Chronic",
        "Not Chronic"
      )
    )
  )

# THIS IS WHERE WE'RE SUMMARISING BY HOUSEHOLD (after all the group_bys)

active_list <- active_list %>%
  mutate(
    HoH_Adjust = case_when(HH_DQ_Issue == 1 ~ correctedhoh,
                           HH_DQ_Issue == 0 ~ hoh)
  ) %>%
  filter(HoH_Adjust == 1) %>%
  select(-correctedhoh, -RelationshipToHoH, -hoh, -HoH_Adjust)

# Add Referral Status -----------------------------------------------------

# thinking maybe it makes the most sense to only look at referrals that have 
# been accepted for the purposes of the Active List. Because who cares if
# there's an open referral on a client who needs housing? That doesn't mean
# anything because we haven't really assigned a meaning to that. But an
# accepted referral does supposedly mean something, and it would add context
# to know that a household on this list has been accepted into (if not entered 
# into) another project.

# also thinking the Refer-to provider should be an RRH or PSH? Maybe? Because
# referrals to a homeless project wouldn't mean anything on an Active List,
# right?

small_referrals <- Referrals %>%
  left_join(Project %>% 
              select(ProjectName, "ReferToPTC" = ProjectType),
            by = c("Referred-ToProvider" = "ProjectName"))

# isolates hhs with an Accepted Referral into a PSH or RRH project
who_has_referrals <- active_list %>%
  left_join(small_referrals %>%
              filter(ReferralDate >= today() - days(14) &
                       ReferralOutcome == "Accepted" &
                       ReferToPTC %in% c(3, 9, 13)) %>%
              group_by(PersonalID) %>%
              arrange(desc(ymd(ReferralDate))) %>%
              slice(1L) %>%
              ungroup(),
            by = c("PersonalID")) %>%
  select(PersonalID,
         HouseholdID,
         EnrollmentID,
         "ReferredToProvider" = "Referred-ToProvider",
         ReferralDate)

active_list <- active_list %>%
  left_join(
    who_has_referrals,
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  )

# Add Program if Not Shown

# this looks up the program for clients that are currently enrolled in a housing program
# AND are also in a literally homeless program, IF the LH program is the one shown on the 
# list. I'm thinking this is less repetitive--why show the program in the status column if 
# we already have it somewhere else in the row? But it could go either way

who_has_entries <- active_list %>%
  filter(PTCStatus == "Has Entry into RRH or PSH" &
           ProjectType %in% c(lh_project_types, 4)) %>%
  select("PersonalID") %>%
  left_join(co_currently_homeless %>%
              filter(ProjectType %in% c(ph_project_types)),
            by = "PersonalID") %>%
  group_by(PersonalID) %>%
  arrange(desc(EntryDate)) %>%
  slice(1L) %>%
  select(PersonalID, "EntryProvider" = ProjectName)

active_list <- active_list %>%
  left_join(
    who_has_entries,
    by = c("PersonalID")
  )

# Fleeing DV --------------------------------------------------------------

active_list <- active_list %>%
  left_join(
    HealthAndDV %>%
      # get DV information only for those on the active list
      inner_join(active_list %>%
                   select(PersonalID), 
                 by = "PersonalID")  %>%
      # get most recent DV information for those on the list
      group_by(PersonalID) %>%
      arrange(desc(InformationDate)) %>%
      slice(1L) %>%
      # pull variables we want
      select(EnrollmentID,
             PersonalID,
             CurrentlyFleeing,
             WhenOccurred),
    by = c("EnrollmentID", "PersonalID")
  ) %>%
  mutate(
    CurrentlyFleeing = if_else(is.na(CurrentlyFleeing), 99, CurrentlyFleeing),
    WhenOccurred = if_else(is.na(WhenOccurred), 99, WhenOccurred),
    CurrentlyFleeing = case_when(
      CurrentlyFleeing %in% c(0, 99) &
        WhenOccurred %in% c(4, 8, 9, 99) ~ "No",
      CurrentlyFleeing == 1 |
        WhenOccurred %in% c(1:3) ~ "Yes",
      CurrentlyFleeing %in% c(8, 9) ~ "Unknown"
    )
  ) %>%
  select(-WhenOccurred)

# Clean the House ---------------------------------------------------------

active_list <- active_list %>%
  mutate(
    VeteranStatus = translate_HUD_yes_no(VeteranStatus),
    DisabilityInHH = translate_HUD_yes_no(DisabilityInHH),
    IncomeFromAnySource = translate_HUD_yes_no(IncomeInHH),
    TAY = case_when(TAY == 1 ~ "Yes",
                    TAY == 0 ~ "No",
                    is.na(TAY) ~ "Unknown"), 
    ProjectName = if_else(
      ProjectName == "Unsheltered Clients - OUTREACH",
      paste("Unsheltered in",
            CountyServed,
            "County"),
      ProjectName
    ),
    PersonalID = as.character(PersonalID),
    Situation = case_when(
      PTCStatus == "Has Entry into RRH or PSH" ~ if_else(
        ProjectType %in% c(lh_project_types, 4), 
        paste(
          "Has Entry into",
          EntryProvider
        ),
        PTCStatus
      ),
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        !is.na(ReferredToProvider) ~
        paste(
          "No current Entry into RRH or PSH but",
          ReferredToProvider,
          "accepted this household's referral on",
          ReferralDate
        ),
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        is.na(ReferredToProvider) &
        !is.na(PHTrack) ~ paste("Permanent Housing Track:",
                                PHTrack,
                                "by",
                                ExpectedPHDate),
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        is.na(ReferredToProvider) &
        is.na(PHTrack) ~ 
        "No Entry or accepted Referral into PSH/RRH, and no current Permanent Housing Track"
    ),
    ShortSituation = factor(
      case_when(
        str_starts(PTCStatus, "Has Entry") ~ "Enrolled in RRH/PSH",
        str_starts(Situation, "No current") |
        str_starts(Situation, "Permanent") ~ "Has Referral or Plan",
        str_starts(Situation, "No Entry") ~ "No Housing Plan"
      ),
      levels = c("No Housing Plan", "Has Referral or Plan", "Enrolled in RRH/PSH")
    )
  ) %>% 
  select(-IncomeInHH)

# landing_data <- active_list %>%
#   select(PersonalID, CountyServed, COVID19Priority, ShortSituation) %>%
#   # filter(CountyServed == "Lorain") %>%
#   # mutate(COVID19Priority = as.character(COVID19Priority),
#   #        ShortSituation = as.character(ShortSituation)) %>%
#   group_by(COVID19Priority, ShortSituation) %>%
#   summarise(HHs = n()) %>%
#   ungroup() %>%
#   as.data.frame() 
# 
# landing <- treemap(
#   landing_data,
#   title = "Currently Literally Homeless Households",
#   index = c("ShortSituation", "COVID19Priority"),
#   border.lwds = c(4, .5),
#   border.col = c("#FFFFFF", "#D2B48C"),
#   palette = "RdBu",
#   vSize = "HHs",
#   vColor = "COVID19Priority",
#   type = "categorical",
#   position.legend = "bottom",
#   fontsize.labels = c(17, 12),
#   fontcolor.labels = c("white", "black"),
#   fontface.labels = c(2, 1),
#   bg.labels = "transparent",
#   # position.legend = "none",
#   align.labels = list(c("center", "center"),
#                       c("left", "top"))
# )

# rowsum(plotly_attempt$HHs, group = plotly_attempt$COVID19Priority)

# plot_ly(
#   b,
#   parents = ~ COVID19Priority,
#   labels = ~ ShortSituation,
#   values = ~ HHs,
#   type = 'treemap'
# )

rm(list = ls()[!(ls() %in% c("active_list"))])

# WARNING save.image does not save the environment properly, save must be used.
save(list = ls(), file = "images/Active_List.RData", compress = FALSE)

