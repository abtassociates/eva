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

# this script uses the COHHIOHMIS data to compare the avg SPDAT score of
# clients served in a county during the reporting period to the avg 
# SPDAt score of those who enrolled into a PSH or RRH project during the 
# reporting period.

library(tidyverse)
library(lubridate)
library(janitor)
library(HMIS)

# loading the COHHIOHMIS data, dropping unnecessary objects
if (!exists("Enrollment")) {
  load("images/COHHIOHMIS.RData")
  # rlang::env_binding_lock(environment(), ls())
}

# more paring down, only taking what variables I need from Enrollment
smallEnrollment <- Enrollment %>%
  left_join(Project, by = c("ProjectType", "ProjectID", "ProjectName")) %>%
  select(
    EnrollmentID,
    PersonalID,
    ProjectID,
    ProjectType,
    ProjectName,
    OperatingStartDate,
    OperatingEndDate,
    EntryDate,
    ExitDate,
    RelationshipToHoH,
    CountyServed
  )
# Entries will give us all the times a hh has an Entry into a PH project
Entries <- smallEnrollment %>%
  filter(ProjectType %in% c(3, 9, 13))

note_qpr_served_county <- "The horizontal lines represent the average scores of Heads 
of Household who were served in the County in a ES, TH, SH, or Outreach project 
during the reporting period and who were scored. If a Head of Household was 
served in a County outside the Balance of State or if that data was missing, 
they are not being counted. When there are multiple project entries for the same 
client, this only counts the most recent entry. When there are multiple scores, 
this only counts the most recent score. There should not be more than 1 score on 
the same day, but if there are it is counting the highest score."

# this object is used in the app to create the plot. it has date variables 
# included so the numbers can be filtered by date range in the app. it takes
# long to run.

qpr_spdats_county <-
  left_join(smallEnrollment, Scores, by = "PersonalID") %>%
  filter(
    ProjectType %in% c(1, 2, 4, 8) &
      RelationshipToHoH == 1 &
      ymd(ScoreDate) <= ymd(EntryDate) &
      !CountyServed %in% c(
        "Montgomery",
        "Cuyahoga",
        "Mahoning",
        "Lucas",
        "Stark",
        "Summit",
        "Hamilton",
        "Franklin",
        "--Outside of Ohio--"
      ) &
      !is.na(CountyServed)
  ) %>%
  select(
    EnrollmentID,
    PersonalID,
    ProjectName,
    EntryDate,
    ExitDate,
    CountyServed,
    ScoreDate,
    Score
  ) %>%
  group_by(PersonalID) %>%
  slice_max(ymd(EntryDate)) %>% # most recent EE
  slice_max(ymd(ScoreDate)) %>% # most recent score
  slice_max(Score) %>% # highest score
  ungroup() %>%
  select(PersonalID,
         ProjectName,
         CountyServed,
         Score,
         EntryDate,
         ExitDate)

note_qpr_housed_county <- "The triangle represents the average score of each 
household entering into a permanent housing project in a County during the 
reporting period. This will necessarily leave out households coming from 
Domestic Violence shelters since they are not scored. Any Heads of Household 
who entered a permanent housing project without a score will be counted as 
having a score of 0."

note_qpr_dq_community_need <- "It is very important that your Duplicate Entry Exits and your 
Household Data Quality tabs are totally clear for this report to be accurate. 
It is also important that your VI-SPDAT scores are ON THE HEAD OF HOUSEHOLD'S 
RECORD. Any scores recorded on non-HoHs will not be counted here.  Also if a 
HoH is missing their County data or they were served in a County outside the 
Ohio Balance of State, they will also not show here."

# this pulls all entries into PSH or RRH

qpr_spdats_project <- left_join(Entries, Scores, by = "PersonalID") %>%
  select(-ProjectType,
         -OperatingStartDate,
         -OperatingEndDate) %>%
  filter(
    RelationshipToHoH == 1 &
      (ymd(ScoreDate) <= ymd(EntryDate) | is.na(ScoreDate)) &
      !CountyServed %in% c(
        "Montgomery",
        "Cuyahoga",
        "Mahoning",
        "Lucas",
        "Stark",
        "Summit",
        "Hamilton",
        "Franklin",
        "--Outside of Ohio--"
      ) &
      !is.na(CountyServed)
  ) %>%
  mutate(
    ScoreAdjusted = if_else(is.na(Score), 0, Score),
    ScoreDateAdjusted = if_else(is.na(ScoreDate), today(), ScoreDate)
  ) %>%
  group_by(EnrollmentID) %>%
  slice_max(ymd(ScoreDateAdjusted)) %>%
  slice_max(ScoreAdjusted) %>%
  distinct() %>%
  ungroup() %>%
  select(-ScoreDateAdjusted)

# If you have clients here, you should either verify the scores saved here are 
# valid or the correct client is marked as the Head of Household.

SPDATsOnNonHoHs <- left_join(Entries, Scores, by = "PersonalID") %>%
  filter(RelationshipToHoH != 1 & 
           !is.na(Score) & 
           served_between(., ymd(calc_data_goes_back_to), ymd(meta_HUDCSV_Export_End))) %>%
  select(ProjectName, PersonalID, EntryDate, ExitDate, Score) %>%
  arrange(ProjectName)

rm(Entries, smallEnrollment, SPDATsOnNonHoHs)
# WARNING save.image does not save the environment properly, save must be used.
save(list = ls(), file = "images/QPR_SPDATs.RData", compress = FALSE)

