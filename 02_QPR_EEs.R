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

# this script uses the COHHIOHMIS data to populate the QPR.

library(tidyverse)
library(lubridate)
library(janitor)
library(HMIS)

if (!exists("Enrollment")) load("images/COHHIOHMIS.RData")
if (!exists("tay")) {
  load("images/cohorts.RData")
  # rlang::env_binding_lock(environment(), ls())
}

# decided to continue to use a separate file for Goals (instead of building it
# in a tribble) because this way the CoC team can review it more easily.
goals <- read_csv("public_data/BoSGoals.csv", col_types = "cccdddddddd")

goals <- goals %>%
  gather(key = "ProjectType", 
         value = "Goal", 
         -SummaryMeasure, -Measure, -Operator) %>%
  mutate(ProjectType = as.numeric(ProjectType)) %>%
  filter(!is.na(Goal))

# Building qpr_leavers ----------------------------------------------------

smallProject <- Project %>%
  select(ProjectID,
         OrganizationName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectAKA,
         ProjectName,
         ProjectType,
         HMISParticipatingProject,
         GrantType,
         ProjectCounty,
         ProjectRegion) %>%
  filter(HMISParticipatingProject == 1 &
           operating_between(., ymd(calc_data_goes_back_to), ymd(meta_HUDCSV_Export_End)) &
           !is.na(ProjectRegion) &
           ProjectType %in% c(1:4, 8:9, 12:14)) %>%
  mutate(
    FriendlyProjectName = if_else(is.na(ProjectAKA), ProjectName, ProjectAKA))

smallEnrollment <- Enrollment %>% 
  select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    RelationshipToHoH,
    CountyServed,
    EntryDate,
    MoveInDate,
    ExitDate,
    EntryAdjust,
    MoveInDateAdjust,
    ExitAdjust,
    LivingSituation,
    Destination,
    DateCreated
  ) 

validation <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  select(
    ProjectID,
    ProjectName,
    ProjectType,
    CountyServed,
    EnrollmentID,
    PersonalID,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    EntryAdjust,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    LivingSituation,
    Destination,
    DateCreated
  ) %>%
  filter(!is.na(EntryDate))

smallEnrollment <- smallEnrollment %>%
  filter(str_detect(HouseholdID, fixed("s_")) |
           (str_detect(HouseholdID, fixed("h_")) &
              RelationshipToHoH == 1)) #<- only pulls in hohs and singles

# captures all leavers PLUS stayers in either HP or PSH because we include those
# stayers in Permanent Destinations. This is used for LoS and Exits to PH.

qpr_leavers <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter((!is.na(ExitDate) | ProjectType %in% c(3, 9, 12)) &
           served_between(., ymd(calc_data_goes_back_to), ymd(meta_HUDCSV_Export_End)) &
           RelationshipToHoH == 1) %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(temp_destinations) ~ "Temporary",
      Destination %in% c(perm_destinations) ~ "Permanent",
      Destination %in% c(institutional_destinations) ~ "Institutional",
      Destination %in% c(other_destinations) ~ "Other",
      is.na(Destination) ~ "Still in Program"
    ),
    DaysinProject = difftime(ExitAdjust, EntryDate, units = "days")
  ) %>% 
  filter(stayed_between(., ymd(calc_data_goes_back_to), ymd(meta_HUDCSV_Export_End))) %>% 
  arrange(ProjectName)

qpr_rrh_enterers <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(ProjectType == 13 &
           entered_between(., ymd(calc_data_goes_back_to), ymd(meta_HUDCSV_Export_End)) &
           RelationshipToHoH == 1) %>%
  mutate(
    DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days"),
    DaysinProject = difftime(ExitAdjust, EntryAdjust, units = "days")
  )

smallMainstreamBenefits <- IncomeBenefits %>%
  select(InsuranceFromAnySource, BenefitsFromAnySource,
         DataCollectionStage, EnrollmentID, InformationDate) %>%
  group_by(EnrollmentID) %>%
  slice(which.max(InformationDate)) %>% # most recent answer per Enrollment
  ungroup()


qpr_benefits <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(exited_between(., ymd(calc_data_goes_back_to), ymd(meta_HUDCSV_Export_End)) &
           RelationshipToHoH == 1) %>%
  left_join(smallMainstreamBenefits, by = "EnrollmentID") %>%
  select(ProjectName, FriendlyProjectName, PersonalID, HouseholdID, EntryDate,
         EntryAdjust, MoveInDate, MoveInDateAdjust, ExitDate, ExitAdjust,
         InsuranceFromAnySource, BenefitsFromAnySource, DataCollectionStage, 
         InformationDate, ProjectRegion, ProjectCounty, ProjectType) %>%
  mutate(ProjectType = case_when(
    ProjectType == 1 ~ "Emergency Shelters", 
    ProjectType == 2 ~ "Transitional Housing", 
    ProjectType == 3 ~ "Permanent Supportive Housing", 
    ProjectType == 4 ~ "Street Outreach", 
    ProjectType == 8 ~ "Safe Haven",
    ProjectType == 9 ~ "Permanent Supportive Housing", 
    ProjectType == 12 ~ "Prevention",  
    ProjectType == 13 ~ "Rapid Rehousing"
  )) %>%
  arrange(ProjectName, HouseholdID)

incomeMostRecent <- IncomeBenefits %>%
  select(IncomeFromAnySource, TotalMonthlyIncome, DataCollectionStage, 
         EnrollmentID, InformationDate) %>%
  group_by(EnrollmentID) %>%
  slice(which.max(InformationDate)) %>%
  ungroup() %>%
  mutate(RecentIncome = TotalMonthlyIncome) %>%
  select(EnrollmentID, RecentIncome)

incomeAtEntry <- IncomeBenefits %>%
  select(IncomeFromAnySource, TotalMonthlyIncome, DataCollectionStage, 
         EnrollmentID, InformationDate) %>%
  group_by(EnrollmentID) %>%
  slice(which.min(InformationDate)) %>%
  ungroup() %>%
  mutate(EntryIncome = TotalMonthlyIncome) %>%
  select(EnrollmentID, EntryIncome)

smallIncomeDiff <- 
  full_join(incomeAtEntry, incomeMostRecent, by = "EnrollmentID")

qpr_income <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(served_between(., ymd(calc_data_goes_back_to), ymd(meta_HUDCSV_Export_End)) &
           RelationshipToHoH == 1) %>%
  left_join(smallIncomeDiff, by = "EnrollmentID") %>%
  select(ProjectName, FriendlyProjectName, PersonalID, HouseholdID, EntryDate,
         EntryAdjust, MoveInDate, MoveInDateAdjust, ExitDate, ExitAdjust,
         EntryIncome, RecentIncome, ProjectRegion, ProjectCounty, ProjectType) %>%
  mutate(
    Difference = RecentIncome - EntryIncome,
    ProjectType = case_when(
      ProjectType == 1 ~ "Emergency Shelters",
      ProjectType == 2 ~ "Transitional Housing",
      ProjectType == 3 ~ "Permanent Supportive Housing",
      ProjectType == 4 ~ "Street Outreach",
      ProjectType == 8 ~ "Safe Haven",
      ProjectType == 9 ~ "Permanent Supportive Housing",
      ProjectType == 12 ~ "Prevention",
      ProjectType == 13 ~ "Rapid Rehousing"
    )
  ) %>% 
  arrange(ProjectName, HouseholdID)

qpr_spending <- Services %>%
  left_join(Enrollment,
            by = c("EnrollmentID", "PersonalID")) %>%
  left_join(smallProject, by = c("ProjectID", "ProjectType", "ProjectName")) %>%
  select(
    PersonalID,
    OrganizationName,
    ProjectName,
    ProjectRegion,
    ProjectType,
    Amount,
    Description,
    RelationshipToHoH,
    ServiceStartDate,
    EntryDate,
    MoveInDateAdjust,
    ExitDate
  ) %>% 
  filter(ProjectType %in% c(13, 12) &
           RelationshipToHoH == 1 &
           !is.na(Amount)) %>%
  select(-RelationshipToHoH)

rm(smallEnrollment,
   smallProject, 
   smallMainstreamBenefits,
   incomeMostRecent,
   incomeAtEntry,
   smallIncomeDiff)


# COVID-19 plots for Rm ---------------------------------------------------

get_res_prior <- validation %>%
  select(PersonalID, EntryDate, ExitDate, LivingSituation) %>%
  group_by(PersonalID) %>%
  arrange(desc(EntryDate)) %>%
  slice(1L)

covid19_plot <- covid19 %>%
  left_join(get_res_prior, by = "PersonalID") %>%
  filter(ymd(COVID19AssessmentDate) >= mdy("04012020") &
           ymd(COVID19AssessmentDate) <= today()) 
  
priority <- covid19_plot %>%
  mutate(
    Priority = case_when(
      # if tested positive
      (
        Tested == 1 &
          TestResults == "Positive" &
          ymd(TestDate) > ymd(COVID19AssessmentDate) - days(14) &
          !is.na(TestDate)
      ) |
        # if under investigation
        (
          UnderInvestigation == 1 &
            ymd(DateUnderInvestigation) > ymd(COVID19AssessmentDate) - days(14)
        ) |
        # contact with COVID-19
        (
          ContactWithConfirmedCOVID19Patient == 1 &
            (
              ymd(ContactWithConfirmedDate) >
                ymd(COVID19AssessmentDate) - days(14) |
                is.na(ContactWithConfirmedDate)
            )
          # compares contact date to the assessment date too since we want to
          # see severity at the time of assessment
        ) |
        (
          ContactWithUnderCOVID19Investigation == 1 &
            (
              ymd(ContactWithUnderInvestigationDate) >
                ymd(COVID19AssessmentDate) - days(14) |
                is.na(ContactWithUnderInvestigationDate)
            )
        ) |
        # if the client came from jail or nursing home
        (
          LivingSituation %in% c(7, 25) &
            EntryDate > ymd(COVID19AssessmentDate) - days(14) &
            EntryDate <= ymd(COVID19AssessmentDate)
        ) |
        # if the client has any symptoms at all
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
            Symptom2Weak
        ) > 0 ~ "Needs Isolation/Quarantine",
      # if the client has any risks at all
      (
        HealthRiskHistoryOfRespiratoryIllness +
          HealthRiskChronicIllness +
          HealthRiskOver65 +
          HealthRiskKidneyDisease +
          HealthRiskImmunocompromised +
          HealthRiskSmoke > 0
      )  ~ "Has Health Risk(s)",
      TRUE ~ "No Known Risks or Exposure"
      # everyone else lands here ^
      # in the report, there will be a third level: "Not Assessed Recently"
    ),
    Priority = factor(Priority, levels = c("Needs Isolation/Quarantine", 
                                           "Has Health Risk(s)", 
                                           "No Known Risks or Exposure")),
    Month = paste0(year(COVID19AssessmentDate),
                   str_pad(
                     month(COVID19AssessmentDate),
                     width = 2,
                     pad = "0"
                   )), 
    Month = as.numeric(factor(Month)),
    MonthOf = format.Date(COVID19AssessmentDate, "%b %Y")
  ) %>% 
  filter(month(COVID19AssessmentDate) != month(today()))

priority_plot <- priority %>%
  dplyr::select(PersonalID, MonthOf, Month, Priority) %>%
  group_by(MonthOf, Month, Priority) %>%
  summarise(Clients = n()) %>%
  arrange(Month)

covid19_priority_plot <- priority_plot %>%
  ggplot(aes(x = reorder(MonthOf, Month), y = Clients,
             fill = Priority, label = Clients)) +
  scale_fill_brewer(palette = "GnBu", direction = -1) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Month of", y = "Clients Assessed") +
  theme(legend.title=element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust=1, size = 11))

rm(priority, priority_plot)

# COVID Status plot -------------------------------------------------------

covid19_status <- covid19_plot %>%
  mutate(
    COVID19Status = case_when(
      Tested == 1 &
        TestResults == "Positive" &
        ymd(TestDate) > ymd(COVID19AssessmentDate) - days(14) &
        !is.na(TestDate) ~ "Positive",
      # testing positive in the 14 days prior to assessment is the only way to
      # land in this bucket
      (
        ContactWithConfirmedCOVID19Patient == 1 &
          (
            ymd(ContactWithConfirmedDate) >
              ymd(COVID19AssessmentDate) - days(14) |
              is.na(ContactWithConfirmedDate)
          )
        # compares contact date to date of the assessment
      ) |
        (
          ContactWithUnderCOVID19Investigation == 1 &
            (
              ymd(ContactWithUnderInvestigationDate) >
                ymd(COVID19AssessmentDate) - days(14) |
                is.na(ContactWithUnderInvestigationDate)
            )
        ) |
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
            Symptom2Weak
        ) > 0
      |
        (
          UnderInvestigation == 1 &
            ymd(DateUnderInvestigation) > ymd(COVID19AssessmentDate) - days(14)
        ) ~
        "May Have COVID-19",
      # being Under Investigation (past 14 days), any Symptom, or any Contact
      # in the 14 days prior to the assessment date will land you here ^
      TRUE ~ "No Current Indications"
      # everyone else lands here ^
    ),
    COVID19Status = factor(
      COVID19Status,
      levels = c("No Current Indications",
                 "May Have COVID-19",
                 "Positive")
    ), 
    Month = paste0(year(COVID19AssessmentDate),
                   str_pad(
                     month(COVID19AssessmentDate),
                     width = 2,
                     pad = "0"
                   )), 
    Month = as.numeric(factor(Month)),
    MonthOf = format.Date(COVID19AssessmentDate, "%b %Y")
  ) %>% 
  filter(month(COVID19AssessmentDate) != month(today()))

covid19_status_plot <- covid19_status %>%
  select(PersonalID, MonthOf, Month, COVID19Status) %>%
  group_by(MonthOf, Month, COVID19Status) %>%
  summarise(Clients = n()) %>%
  arrange(Month) %>%
  ggplot(aes(x = reorder(MonthOf, Month), y = Clients,
             fill = COVID19Status)) +
  geom_bar(stat = "identity", 
           position = position_stack(reverse = TRUE)) +  
  scale_fill_manual(values = c("#e0ecf4", "#9ebcda", "#8856a7")) +
  theme_minimal() +
  labs(x = "Month of", y = "Clients Assessed") +
  theme(legend.title=element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust=1, size = 11))

rm(covid19_status, covid19_plot)

# Save it out -------------------------------------------------------------
# WARNING save.image does not save the environment properly, save must be used.
save(list = ls(), file = "images/QPR_EEs.RData", compress = FALSE)


