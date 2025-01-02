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
split_rows <- select_random_rows(original_data_fixed_cols$Exit$Destination == 435)
original_data_fixed_cols$Exit <- original_data_fixed_cols$Exit %>%
  mutate(DestinationSubsidyType = 
           case_when(
             row_number() %in% split_rows[[1]] ~ NA, # null values
             row_number() %in% split_rows[[2]] ~ 99, # 99 (Data not collected, generally) values
             row_number() %in% split_rows[[3]] ~ 238, # nonsense values
             TRUE ~ DestinationSubsidyType
           )
  )

# CLS Subsidy Type -------------------------------------------------------------
split_rows <- select_random_rows(original_data_fixed_cols$CurrentLivingSituation$CurrentLivingSituation == 435)
original_data_fixed_cols$CurrentLivingSituation <- original_data_fixed_cols$CurrentLivingSituation %>%
  mutate(CLSSubsidyType = 
           case_when(
             row_number() %in% split_rows[[1]] ~ NA, # null values
             row_number() %in% split_rows[[2]] ~ 99, # 99 (Data not collected, generally) values
             row_number() %in% split_rows[[3]] ~ 234, # nonsense values
             TRUE ~ CLSSubsidyType
           )
  )

# Prior Living Situation Subsidy -----------------------------------------------
split_rows <- select_random_rows(original_data_fixed_cols$Enrollment$LivingSituation == 435)
original_data_fixed_cols$Enrollment <- original_data_fixed_cols$Enrollment %>%
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
date_shift <- -2
overlaps_temp <- original_data_fixed_cols$HMISParticipation %>%
  filter(HMISParticipationType == 1 &
           !is.na(HMISParticipationStatusEndDate)) %>%
  sample_n(1) %>%
  mutate(
    HMISParticipationStatusStartDate = HMISParticipationStatusEndDate + days(date_shift),
    HMISParticipationStatusEndDate = NA,
    HMISParticipationID = paste0(HMISParticipationID, abs(date_shift))
  )
original_data_fixed_cols$HMISParticipation <- bind_rows(overlaps_temp, original_data_fixed_cols$HMISParticipation)

# RRH-SO projects with active inventory (checkID = 132) ------------------------
# this selects a random project with inventory and projecttype = 13
random_project <- original_data_fixed_cols$Inventory %>% 
  filter(BedInventory > 0) %>%
  semi_join(
    original_data_fixed_cols$Project %>% 
      filter(ProjectType == 13 & RRHSubType != 1), 
    by = "ProjectID") %>%
  sample_n(1) %>%
  pull(ProjectID)

# Update RRHSubType for that project to 1
original_data_fixed_cols$Project <- original_data_fixed_cols$Project %>%
  mutate(
    RRHSubType = if_else(ProjectID == random_project, 1, RRHSubType) #,
    # RRHSOActivePeriod =
    #   interval(OperatingStartDate,
    #     coalesce(OperatingEndDate, original_data_fixed_cols$Export$ExportEndDate))
    )

# finally, make sure inventory period overlaps project operating period
original_data_fixed_cols$Inventory <- original_data_fixed_cols$Inventory %>% 
  left_join(original_data_fixed_cols$Project %>% 
              select(ProjectID, OperatingStartDate, OperatingEndDate), 
            by = "ProjectID") %>%
  mutate(InventoryStartDate = OperatingStartDate - days(1),
         InventoryEndDate = OperatingEndDate + days(1)) %>%
  select(-c(OperatingStartDate, OperatingEndDate))

# there are deleted records in the data -----------------------------------

original_data_fixed_cols$Organization <- rbind(original_data_fixed_cols$Organization[1,], original_data_fixed_cols$Organization) %>%
  mutate(DateDeleted = if_else(row_number() == 1, ymd("20231101"), NA))

# add some periods of zero utilization ------------------------------------
# This should make project 1376 flag for Zero Utilization^
tables_to_filter <- c("Enrollment",
                      "Exit",
                      "HealthAndDV",
                      "IncomeBenefits",
                      "YouthEducationStatus")
original_data_fixed_cols[tables_to_filter] <- lapply(
  original_data_fixed_cols[tables_to_filter],
  function(df) df %>% filter(EnrollmentID != "696923")
)

# impermissible characters
# brackets, Windows-1252 encoding with smart quotes
# add brackets and Windows smart quotes (which have crashed Eva in the past)
# Project will eventually be Windows-1252 encoded
original_data_fixed_cols$Project[5, "ProjectName"] <- "Organization G - [Brackets]"
original_data_fixed_cols$Project[7, "ProjectName"] <- "Organization S - ‘smart quote’"
original_data_fixed_cols$Project[8, "ProjectName"] <- "Organization M - “Double smart quote”"
# per mille sign but Organization will be left as utf-8 encoded
original_data_fixed_cols$Organization[2, "OrganizationName"] <- paste0(
  original_data_fixed_cols$Organization[2, "OrganizationName"],
  "‰"
)

# missing address, for VSP and CoC, as well as CoCCode and GeographyType
original_data_fixed_cols$Organization[11, "VictimServiceProvider"] <- 1 # org 55
# Non-VSP project (Org 49) and a project with Org 55
original_data_fixed_cols$ProjectCoC[c(1, 28), c("Address1", "CoCCode", "GeographyType", "ZIP")] <- NA

## add more checks here --------------------------------------------
