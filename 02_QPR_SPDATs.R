# this script uses the COHHIOHMIS data to compare the avg SPDAT score of
# clients served in a county during the reporting period to the avg 
# SPDAt score of those who enrolled into a PSH or RRH project during the 
# reporting period.

library(tidyverse)
library(lubridate)
library(janitor)
# loading the COHHIOHMIS data, dropping unnecessary objects
load("data/COHHIOHMIS.RData")
rm(Affiliation, Client, EnrollmentCoC, EmploymentEducation, Export, Exit, 
   Funder, HealthAndDV, Disabilities, IncomeBenefits, Geography, Inventory, 
   Offers, Organization, ProjectCoC, Services, VeteranCE)
# more paring down, only taking what variables I need from Enrollment
smallEnrollment <- Enrollment %>%
  left_join(Project, by = "ProjectID") %>%
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

rm(Enrollment, Project)

hhsServedInCounty <- "The horizontal lines represent the average scores of Heads 
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

CountyData <-
  left_join(smallEnrollment, Scores, by = "PersonalID") %>%
  filter(
    ProjectType %in% c(1, 2, 4, 8) &
      RelationshipToHoH == 1 & 
      ymd(StartDate) <= ymd(EntryDate) & 
      !CountyServed %in% c("Montgomery", 
                           "Cuyahoga", 
                           "Mahoning", 
                           "Lucas", 
                           "Stark", 
                           "Summit", 
                           "Hamilton", 
                           "Franklin",
                           "--Outside of Ohio--") &
      !is.na(CountyServed)) %>%
  select(EnrollmentID, PersonalID, ProjectID, EntryDate, ExitDate, CountyServed, StartDate, Score) %>%
  group_by(PersonalID) %>%
  mutate(MaxEntry = max(ymd(EntryDate))) %>% # most recent EE
  filter(ymd(MaxEntry) == ymd(EntryDate)) %>%  
  mutate(MaxScoreDate = max(ymd(StartDate))) %>% # most recent score
  filter(ymd(StartDate) == ymd(MaxScoreDate)) %>%
  mutate(MaxScore = max(Score)) %>% # highest score
  filter(Score == MaxScore) %>%
  ungroup() %>%
  select(PersonalID, CountyServed, Score, EntryDate, ExitDate) 

hhsHousedInCounty <- "The triangle represents the average score of each 
household entering into a permanent housing project in a County during the 
reporting period. This will necessarily leave out households coming from 
Domestic Violence shelters since they are not scored. Any Heads of Household 
who entered a permanent housing project without a score will be counted as 
having a score of 0."

noteToUsers <- "It is very important that your Duplicate Entry Exits and your 
Household Data Quality tabs are totally clear for this report to be accurate. 
It is also important that your VI-SPDAT scores are ON THE HEAD OF HOUSEHOLD'S 
RECORD. Any scores recorded on non-HoHs will not be counted here.  Also if a 
HoH is missing their County data or they were served in a County outside the 
Ohio Balance of State, they will also not show here."

# this pulls all entries into PSH or RRH

SPDATsByProject <- left_join(Entries, Scores, by = "PersonalID") %>%
  select(-ProjectType,
         -OperatingStartDate,
         -OperatingEndDate,
         -SPDATRecordID,
         -SPDATProvider) %>%
  filter(
    RelationshipToHoH == 1 &
      (ymd(StartDate) <= ymd(EntryDate) | is.na(StartDate)) &
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
  group_by(EnrollmentID) %>%
  mutate(MaxScoreDate = max(ymd(StartDate))) %>%
  filter(ymd(StartDate) == ymd(MaxScoreDate) | is.na(StartDate)) %>%
  mutate(MaxScore = max(Score)) %>%
  filter(Score == MaxScore | is.na(StartDate)) %>%
  distinct() %>%
  ungroup() %>%
  select(-MaxScoreDate, -MaxScore) %>%
  mutate(ScoreAdjusted = if_else(is.na(Score), 0, Score)) %>%
  filter(!is.na(ScoreAdjusted))

# If you have clients here, you should either verify the scores saved here are 
# valid or the correct client is marked as the Head of Household.

SPDATsOnNonHoHs <- left_join(Entries, Scores, by = "PersonalID") %>%
  filter(RelationshipToHoH != 1 & 
           !is.na(Score) & 
           served_between(., ReportStart, ReportEnd)) %>%
  select(ProjectName, PersonalID, EntryDate, ExitDate, Score) %>%
  arrange(ProjectName)

rm(Entries, Scores, smallEnrollment, EighthMonth, EleventhMonth, FifthMonth, 
   FirstMonth, FourthMonth, NinthMonth, ReportEnd, ReportingPeriod, ReportStart, 
   SecondMonth, SeventhMonth, SixthMonth, TenthMonth, ThirdMonth, TwelfthMonth, 
   Users)

save.image("data/QPR_SPDATs.RData")
