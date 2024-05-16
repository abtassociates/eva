# run 00-Start_here.R EXCEPT for the "Data Prep" section

# Destination Subsidy Type ------------------------------------------------

# pulling random records to change (that fit criteria)
Exit %>%
  filter(Destination == 435) %>%
  sample_n(5) %>%
  pull(ExitID)

# testing for null values in SubsidyType

Exit <- Exit %>%
  mutate(DestinationSubsidyType = case_when(
    !ExitID %in% c("806871", "795751") ~ DestinationSubsidyType,
    TRUE ~ NA
  ))

# testing for 99 (Data not collected, generally) values
  
Exit <- Exit %>%
  mutate(DestinationSubsidyType = case_when(
    ExitID %in% c("855533", "805377") ~ 99,
    TRUE ~ DestinationSubsidyType
  ))

# testing for nonsense values

Exit <- Exit %>%
  mutate(DestinationSubsidyType = case_when(
    ExitID == "845138" ~ 238,
    TRUE ~ DestinationSubsidyType
  ))

# CLS Subsidy Type --------------------------------------------------------

CurrentLivingSituation %>%
  filter(CurrentLivingSituation == 435) %>%
  sample_n(5) %>%
  pull(CurrentLivingSitID)

CurrentLivingSituation <- CurrentLivingSituation %>%
  mutate(CLSSubsidyType = case_when(
    !CurrentLivingSitID %in% c("2570", "3977") ~ CLSSubsidyType,
    TRUE ~ NA
  ))

CurrentLivingSituation <- CurrentLivingSituation %>%
  mutate(CLSSubsidyType = case_when(
    CurrentLivingSitID %in% c("1799", "101") ~ 99,
    TRUE ~ CLSSubsidyType
  ))

CurrentLivingSituation <- CurrentLivingSituation %>%
  mutate(CLSSubsidyType = case_when(
    CurrentLivingSitID == "4049" ~ 234,
    TRUE ~ CLSSubsidyType
  ))

# Prior Living Situation Subsidy ------------------------------------------

Enrollment %>%
  filter(LivingSituation == 435) %>%
  sample_n(5) %>%
  pull(EnrollmentID)

Enrollment <- Enrollment %>%
  mutate(RentalSubsidyType = case_when(
    !EnrollmentID %in% c("849994", "832945") ~ RentalSubsidyType,
    TRUE ~ NA
  ))

Enrollment <- Enrollment %>%
  mutate(RentalSubsidyType = case_when(
    EnrollmentID %in% c("790000", "777158") ~ 99,
    TRUE ~ RentalSubsidyType
  ))

Enrollment <- Enrollment %>%
  mutate(RentalSubsidyType = case_when(
    EnrollmentID == "788144" ~ 234,
    TRUE ~ RentalSubsidyType
  ))

# write edits to the csvs -------------------------------------------------

write.csv(
  Exit,
  here(paste0(directory, "data/Exit.csv")),
  na = "",
  row.names = FALSE
)

write.csv(
  CurrentLivingSituation,
  here(paste0(directory, "data/CurrentLivingSituation.csv")),
  na = "",
  row.names = FALSE
)

write.csv(
  Enrollment,
  here(paste0(directory, "data/Enrollment.csv")),
  na = "",
  row.names = FALSE
)

