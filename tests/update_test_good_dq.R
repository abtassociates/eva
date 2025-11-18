library(collapse)
library(data.table)

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
  fmutate(
    rn = seq_row(.),
    DestinationSubsidyType = fcase(
      rn %in% split_rows[[1]], NA_real_, # null values
      rn %in% split_rows[[2]], 99, # 99 (Data not collected, generally) values
      rn %in% split_rows[[3]], 238, # nonsense values
      default = DestinationSubsidyType
    ),
    rn = NULL
  )

# CLS Subsidy Type -------------------------------------------------------------
split_rows <- select_random_rows(original_data_fixed_cols$CurrentLivingSituation$CurrentLivingSituation == 435)
original_data_fixed_cols$CurrentLivingSituation <- original_data_fixed_cols$CurrentLivingSituation %>%
  fmutate(
    rn = seq_row(.),
    CLSSubsidyType = fcase(
      rn %in% split_rows[[1]], NA_real_, # null values
      rn %in% split_rows[[2]], 99, # 99 (Data not collected, generally) values
      rn %in% split_rows[[3]], 234, # nonsense values
      default = CLSSubsidyType
    ),
    rn = NULL
  )

# Prior Living Situation Subsidy -----------------------------------------------
split_rows <- select_random_rows(original_data_fixed_cols$Enrollment$LivingSituation == 435)
original_data_fixed_cols$Enrollment <- original_data_fixed_cols$Enrollment %>%
  fmutate(
    rn = seq_row(.),
    RentalSubsidyType = fcase(
      rn %in% split_rows[[1]], NA_real_, # null values
      rn %in% split_rows[[2]], 99, # 99 (Data not collected, generally) values
      rn %in% split_rows[[3]], 234, # nonsense values
      default = RentalSubsidyType
    ),
    rn = NULL
  )

# Participation overlap (checkIDs = 131) ---------------------------------------
# this creates an "overlap" by duplicating a randomly selected existing record 
# and modifying the start date to be 2 days before the original record's end date
# to create a "gap", simply add/change the number to a positive number
date_shift <- -2
overlaps_temp <- original_data_fixed_cols$HMISParticipation %>%
  fsubset(HMISParticipationType == 1 &
           !is.na(HMISParticipationStatusEndDate)) %>%
  sample_n(1) %>%
  fmutate(
    HMISParticipationStatusStartDate = HMISParticipationStatusEndDate + days(date_shift),
    HMISParticipationStatusEndDate = NA,
    HMISParticipationID = paste0(HMISParticipationID, abs(date_shift))
  )
original_data_fixed_cols$HMISParticipation <- bind_rows(overlaps_temp, original_data_fixed_cols$HMISParticipation)

# RRH-SO projects with active inventory (checkID = 132) ------------------------
# this selects a random project with inventory and projecttype = 13
random_project <- original_data_fixed_cols$Inventory %>% 
  fsubset(BedInventory > 0) %>%
  join(
    original_data_fixed_cols$Project %>% 
      fsubset(ProjectType == rrh_project_type & RRHSubType != 1), 
    on = "ProjectID",
    how = "semi") %>%
  sample_n(1) %>%
  pull(ProjectID)

# Update RRHSubType for that project to 1
original_data_fixed_cols$Project <- original_data_fixed_cols$Project %>%
  fmutate(
    RRHSubType = fifelse(ProjectID == random_project, 1, RRHSubType) #,
    # RRHSOActivePeriod =
    #   interval(OperatingStartDate,
    #     coalesce(OperatingEndDate, original_data_fixed_cols$Export$ExportEndDate))
  )

# finally, make sure inventory period overlaps project operating period
original_data_fixed_cols$Inventory <- original_data_fixed_cols$Inventory %>% 
  join(
    original_data_fixed_cols$Project %>% 
      fselect(ProjectID, OperatingStartDate, OperatingEndDate), 
    on = "ProjectID"
  ) %>%
  fmutate(InventoryStartDate = OperatingStartDate - days(1),
         InventoryEndDate = OperatingEndDate + days(1)) %>%
  fselect(-c(OperatingStartDate, OperatingEndDate))

# there are deleted records in the data -----------------------------------

original_data_fixed_cols$Organization <- rbind(
  original_data_fixed_cols$Organization[1,], 
  original_data_fixed_cols$Organization
  ) %>%
  fmutate(DateDeleted = fifelse(seq_row(.) == 1, ymd("20231101"), NA))

# add some periods of zero utilization ------------------------------------
# This should make project 1376 flag for Zero Utilization^
tables_to_filter <- c("Enrollment",
                      "Exit",
                      "HealthAndDV",
                      "IncomeBenefits",
                      "YouthEducationStatus")
original_data_fixed_cols[tables_to_filter] <- lapply(
  original_data_fixed_cols[tables_to_filter],
  function(df) df %>% fsubset(EnrollmentID != "696923")
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

# Trigger the 'Project Missing in ProjectCoC file' High Priority Issue (checkID = 35) -------------------------
# Remove a project from ProjectsCOC, with ContinuumProject == 1
coc_project <- original_data_fixed_cols$Project %>% 
      fsubset(ContinuumProject == 1) 
coc_project <- coc_project$ProjectID[fnrow(coc_project)]

original_data_fixed_cols$ProjectCoC <- original_data_fixed_cols$ProjectCoC %>%
  fsubset(ProjectID != coc_project)
rm(coc_project)
# Trigger the 'No Enrollments within Active Inventory' Warning Issue (checkID = 141) -------------------------
# Get a project from the activeInventory with and EndDate
XInventory <- original_data_fixed_cols$Inventory %>%
  fsubset((is.na(Availability) | Availability != 3) &
     BedInventory > 0 & !is.na(BedInventory)) %>%
  fselect(ProjectID, InventoryID, InventoryStartDate, InventoryEndDate) %>%
  fsubset(!is.na(InventoryEndDate))

ainv_proj <- XInventory %>% fsubset(ProjectID == XInventory$ProjectID[fnrow(XInventory)])

# Update all the enrollments to have EntryDate 10 days after the Inventory End Date
original_data_fixed_cols$Enrollment <- original_data_fixed_cols$Enrollment %>% 
  fmutate(EntryDate = as.Date(fifelse(ProjectID == ainv_proj$ProjectID,
                           ymd(ainv_proj$InventoryEndDate + days(10)),
                           ymd(EntryDate))))

rm(XInventory, ainv_proj)

## add more checks here --------------------------------------------
