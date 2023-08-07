###############################
#   PURPOSE: This script conducts the PDDE checks, which are things admins fix 
#   (rather than End-users who fix DQ checks)
###############################

logToConsole("Running PDDE checker")

PDDEcols = c("OrganizationName",
             "ProjectID",
             "ProjectName",
             "Issue",
             "Type",
             "Guidance",
             "Detail")

# Subpop beds = TotalBeds
subpopNotTotal <- Inventory %>%
  left_join(Project, by = "ProjectID") %>%
  filter(ProjectType %in% project_types_w_beds &
           (CHVetBedInventory + 
              YouthVetBedInventory + 
              VetBedInventory + 
              CHYouthBedInventory + 
              YouthBedInventory + 
              CHBedInventory + 
              OtherBedInventory
           ) != BedInventory
  ) %>%
  merge_check_info(checkIDs = 46) %>%
  mutate(Detail = 
           paste0(
             str_squish("Inventory for CH Vets, Youth vets, Vets, CH Youth, Youth,
                        CH, and Other sum up to"), " ",
         CHVetBedInventory + 
           YouthVetBedInventory + 
           VetBedInventory + 
           CHYouthBedInventory + 
           YouthBedInventory + 
           CHBedInventory + 
           OtherBedInventory,
         " but your Total Beds is ",
         BedInventory,
         ". These totals must match.")
  ) %>%
  select(all_of(PDDEcols))

# Missing Operating End Date If a project has no open enrollments and the most
# recent Enrollment was 30+ days ago
operating_end_missing <- Enrollment %>%
  group_by(ProjectID) %>%
  mutate(NumOpenEnrollments = sum(is.na(ExitDate)),
         MostRecentEnrollment = max(ExitAdjust, na.rm = TRUE)

  ) %>%
  ungroup() %>%
  left_join(Project %>% 
              select(ProjectID, OrganizationName, OperatingEndDate), 
            by = "ProjectID") %>%
  filter(NumOpenEnrollments == 0 & 
           MostRecentEnrollment >= 
           coalesce(OperatingEndDate, Export$ExportDate) - 30 &
           is.null(OperatingEndDate)) %>%
  merge_check_info(checkIDs = 81) %>%
  mutate(Detail = paste(
           "This project has no open enrollments and the most recent Exit was",
           MostRecentEnrollment
         )
  ) %>%
  select(all_of(PDDEcols))

# Missing CoC Information Missing address field(s), Missing Geocode,
# Missing Geography Type, Invalid Zip Code if possible
missing_CoC_Info <- Project %>%
  left_join(ProjectCoC, by = "ProjectID") %>%
  filter(is.na(Address1) | 
           is.na(City) | 
           is.na(State) | 
           is.na(Geocode) | 
           is.na(GeographyType) | 
           nchar(ZIP) != 5 |
           is.na(ZIP) |
           is.na(CoCCode)
  )

missing_CoC_Geography <- missing_CoC_Info %>%
  filter(is.na(Geocode) | is.na(GeographyType) |
           is.na(CoCCode)) %>%
  merge_check_info(checkIDs = 5) %>%
  mutate(
    Detail = case_when(
      is.na(CoCCode) ~ "This project's CoC Code is missing.",
      is.na(Address1) ~ "This project's Address is missing.",
      is.na(City) ~ "This project's City is missing.",
      is.na(State) ~ "This project's State is missing.",
      is.na(Geocode) ~ "This project's Geocode is missing.",
      is.na(GeographyType) ~ "This project's Geography Type is missing.",
      nchar(ZIP) != 5 | is.na(ZIP) | ZIP == "00000" ~
        "ZIP is missing or not valid."
    )) %>%
  select(all_of(PDDEcols))

missing_CoC_Address <- missing_CoC_Info %>%
  filter(!(is.na(Geocode) | is.na(GeographyType) |
           is.na(CoCCode))) %>%
  merge_check_info(checkIDs = 42) %>%
  mutate(
    Detail = case_when(
      is.na(CoCCode) ~ "This project's CoC Code is missing.",
      is.na(Address1) ~ "This project's Address is missing.",
      is.na(City) ~ "This project's City is missing.",
      is.na(State) ~ "This project's State is missing.",
      is.na(Geocode) ~ "This project's Geocode is missing.",
      is.na(GeographyType) ~ "This project's Geography Type is missing.",
      nchar(ZIP) != 5 | is.na(ZIP) | ZIP == "00000" ~
        "ZIP is missing or not valid."
   )) %>%
  select(all_of(PDDEcols))

# Missing Inventory Record Is a residential project but has no active inventory for the duration of operating period OR for the reporting period
missing_inventory_record <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(ProjectType %in% project_types_w_beds &
           is.na(InventoryID)) %>% 
  merge_check_info(checkIDs = 43) %>%
  mutate(
    Detail = str_squish("This project has no Inventory records. Residential 
      project types should have inventory data.")
  ) %>% 
  select(all_of(PDDEcols))

# Inventory Start < Operating Start AND
# Inventory End > Operating End or Null
inventoryOutsideOperating <- Inventory %>%
  left_join(Project, by = "ProjectID")

inventoryStartPrecedesOp <- inventoryOutsideOperating %>%
  filter(InventoryStartDate < OperatingStartDate) %>%
  merge_check_info(checkIDs = 79) %>%
  mutate(
    Detail = str_squish(
      paste0(
        "This project may have been merged with another project which would explain
            why the Inventory Start Date of ",
        InventoryStartDate,
        " is prior to the project's Operating Start Date of ",
        OperatingStartDate,
        ". Please be sure this is the case and that it is not a typo."
      )
    )
  ) %>% 
  select(all_of(PDDEcols))


operating_end_precedes_inventory_end <- inventoryOutsideOperating %>%
  filter(coalesce(InventoryEndDate, as.Date(meta_HUDCSV_Export_Date)) >
           coalesce(OperatingEndDate, as.Date(meta_HUDCSV_Export_Date))
  ) %>%
  merge_check_info(checkIDs = 44)
  mutate(
    Detail = is.na(InventoryEndDate) &
      !is.na(OperatingEndDate) ~
      str_squish(
        paste0(
          "This project has an Inventory Record (",
          InventoryID,
          ") with an open Inventory End Date but the Project Operating End Date
            is ",
          OperatingEndDate,
          ". Please either end-date the Inventory, or if the project is still
            operating, clear the project's Operating End Date."
        )
      )
  ) %>%
  select(all_of(PDDEcols))

# HMIS Participating ------------------------------------------------------
# HMIS Participating != 1, OR VSP != 0 but client level data in file

hmis_not_participating_but_client <- Project %>%
  left_join(Organization %>% select(OrganizationID, VictimServiceProvider),
            by = "OrganizationID") %>%
  filter((HMISParticipatingProject != 1 |
            VictimServiceProvider != 0) &
           ProjectID %in% c(Enrollment$ProjectID %>% unique())
  ) %>%
  merge_check_info(checkIDs = 80) %>%
  mutate(
    Detail = str_squish(
      "There is client data in this project. Please check that this project is
      marked correctly as non-participating."
    )
  ) %>%
  select(all_of(PDDEcols)) 

es_no_tracking_method <- Project %>%
  filter(ProjectType %in% c(1, 0) & is.na(TrackingMethod)) %>%
  merge_check_info(checkIDs = 45) %>%
  mutate(
    Detail = paste("This project is an Emergency Shelter with no Tracking Method")
  ) %>%
  select(all_of(PDDEcols))


# Zero Utilization --------------------------------------------------------

projects_w_beds <- Inventory %>%
  filter(
    BedInventory > 0 &
      coalesce(InventoryEndDate, meta_HUDCSV_Export_End) >= meta_HUDCSV_Export_Start &
      InventoryStartDate <= meta_HUDCSV_Export_End
  ) %>%
  pull(ProjectID) %>%
  unique()

projects_w_clients <- Enrollment %>%
  pull(ProjectID) %>%
  unique()

res_projects_no_clients <- setdiff(projects_w_beds, projects_w_clients)

zero_utilization <- Project %>%
  filter(ProjectID %in% c(res_projects_no_clients)) %>%
  merge_check_info(checkIDs = 83) %>%
  mutate(
    Detail = str_squish("This project has active inventory beds in the report
                        period but did not serve any clients during that time.")
  ) %>%
  select(all_of(PDDEcols))

##### For later-------
# Incompatible Funding Source and Project Type Funding Source X can only be used with Project Type Y. Project Type A can only be used with Funding Source B. (this will need a lot more detail, hold on this one)

### Put it together ----
pdde_main <- rbind(
  subpopNotTotal,
  operating_end_missing,
  es_no_tracking_method,
  missing_CoC_Geography,
  missing_CoC_Address,
  missing_inventory_record,
  inventoryStartPrecedesOp,
  operating_end_precedes_inventory_end,
  hmis_not_participating_but_client,
  zero_utilization
)


