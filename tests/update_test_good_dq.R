print("updating test good for DQ")

set.seed(12345)
select_random_rows <- function(cond) {
  return(
    split(
      sample(which(cond), 6), 
      rep(1:3, each = 2)
    )
  )
}
# Destination Subsidy Type -----------------------------------------------------

# pull 6 random rows to change (that fit criteria)
# we split up the 6 rows into 3 groups of 2, to test 3 issues
split_rows <- select_random_rows(Exit$Destination == 435)
Exit <- Exit %>%
  mutate(DestinationSubsidyType = 
           case_when(
             row_number() %in% split_rows[[1]] ~ NA, # null values
             row_number() %in% split_rows[[2]] ~ 99, # 99 (Data not collected, generally) values
             row_number() %in% split_rows[[3]] ~ 238, # nonsense values
             TRUE ~ DestinationSubsidyType
           )
  )

# CLS Subsidy Type -------------------------------------------------------------
split_rows <- select_random_rows(CurrentLivingSituation$CurrentLivingSituation == 435)
CurrentLivingSituation <- CurrentLivingSituation %>%
  mutate(CLSSubsidyType = 
           case_when(
             row_number() %in% split_rows[[1]] ~ NA, # null values
             row_number() %in% split_rows[[2]] ~ 99, # 99 (Data not collected, generally) values
             row_number() %in% split_rows[[3]] ~ 234, # nonsense values
             TRUE ~ CLSSubsidyType
           )
  )

# Prior Living Situation Subsidy -----------------------------------------------
split_rows <- select_random_rows(Enrollment$LivingSituation == 435)
Enrollment <- Enrollment %>%
  mutate(RentalSubsidyType = 
           case_when(
             row_number() %in% split_rows[[1]] ~ NA, # null values
             row_number() %in% split_rows[[2]] ~ 99, # 99 (Data not collected, generally) values
             row_number() %in% split_rows[[3]] ~ 234, # nonsense values
             TRUE ~ RentalSubsidyType
           )
  )

# Participation overlap (checkIDs = 131) ---------------------------------------
# this creates an "overlap" by duplicating a randomly selected existing record 
# and modifying the start date to be 2 days before the original record's end date
# to create a "gap", simply add/change the number to a positive number
HMISParticipation <- map_dfr(c(-2), ~ {
  HMISParticipation %>%
    filter(HMISParticipationType == 1 & !is.na(HMISParticipationStatusEndDate)) %>%
    sample_n(1) %>%
    mutate(HMISParticipationStatusStartDate = HMISParticipationStatusEndDate + days(.x),
           HMISParticipationStatusEndDate = NA,
           HMISParticipationID = paste0(HMISParticipationID,abs(.x)))
}) %>%
  bind_rows(HMISParticipation)

# RRH-SO projects with active inventory (checkID = 132) ------------------------
# this selects a random project with inventory and projecttype = 13
random_project <- Inventory %>% 
  filter(BedInventory > 0) %>%
  semi_join(
    Project %>% 
      filter(ProjectType == 13 & RRHSubType != 1), 
    by = "ProjectID") %>%
  sample_n(1) %>%
  pull(ProjectID)

# Update RRHSubType for that project to 1
Project <- Project %>%
  mutate(
    RRHSubType = if_else(ProjectID == random_project, 1, RRHSubType),
    RRHSOActivePeriod =
      interval(OperatingStartDate,
        coalesce(OperatingEndDate, meta_HUDCSV_Export_End))
    )

# finally, make sure inventory period overlaps project operating period
Inventory <- Inventory %>% 
  left_join(Project %>% 
              select(ProjectID, OperatingStartDate, OperatingEndDate), 
            by = "ProjectID") %>%
  mutate(InventoryStartDate = OperatingStartDate - days(1),
         InventoryEndDate = OperatingEndDate + days(1)) %>%
  select(-c(OperatingStartDate, OperatingEndDate))

# there are deleted records in the data -----------------------------------

Organization <- rbind(Organization[1,], Organization) %>%
  mutate(DateDeleted = if_else(row_number() == 1, ymd("20231101"), NA))

## add more checks here --------------------------------------------