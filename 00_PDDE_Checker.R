PDDEcols = c("OrganizationName",
             "ProjectID",
             "ProjectName",
             "Issue",
             "Type",
             "Guidance")

# Subpop beds = TotalBeds
subpopNotTotal <- Inventory %>%
  left_join(Project, by = "ProjectID") %>%
  filter(ProjectType %in% c(project_types_w_beds) &
           (CHVetBedInventory + 
              YouthVetBedInventory + 
              VetBedInventory + 
              CHYouthBedInventory + 
              YouthBedInventory + 
              CHBedInventory + 
              OtherBedInventory
           ) != BedInventory
  ) %>%
  mutate(Issue = "Sum of the dedicated beds should equal the Total Beds",
         Type = "Error",
         Guidance = 
           paste0("Inventory for CH Vets, Youth vets, Vets, CH Youth, Youth, CH, and 
         Other sum up to ",
         CHVetBedInventory + 
           YouthVetBedInventory + 
           VetBedInventory + 
           CHYouthBedInventory + 
           YouthBedInventory + 
           CHBedInventory + 
           OtherBedInventory,
         " but your Total Beds is ",
         BedInventory,
         ". These totals should match.")
  ) %>%
  select(all_of(PDDEcols))

# Missing Operating End Date If a project has no open enrollments and the most recent Enrollment was 30+ days ago
operatingEndMissing <- Enrollment %>%
  group_by(ProjectID) %>%
  mutate(NumOpenEnrollments = sum(is_null(ExitDate)),
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
  mutate(Issue = "Potentially Missing Operating End Date",
         Type = "Warning",
         Guidance = paste(
           "This project has no open enrollments and the most recent Exit was",
           MostRecentEnrollment
         )
  ) %>%
  select(all_of(PDDEcols))

# Missing CoC Information Missing address field(s), Missing Geocode, Missing Geography Type, Invalid Zip Code if possible
missingCoCInfo <- Project %>%
  left_join(ProjectCoC, by = "ProjectID") %>%
  filter(is_null(Address1) | 
           is_null(City) | 
           is_null(State) | 
           is_null(Geocode) | 
           is_null(GeographyType) | 
           nchar(ZIP) != 5 |
           is_null(ZIP) |
           is_null(CoCCode)
  ) %>%
  mutate(Issue = "Missing Geography Information",
         Guidance = case_when(
           is_null(CoCCode) ~ "This project's CoC Code is missing",
           is_null(Address1) ~ "This project's Address is missing",
           is_null(City) ~ "This project's City is missing",
           is_null(State) ~ "This project's State is missing",
           is_null(Geocode) ~ "This project's Geocode is missing",
           is_null(GeographyType) ~ "This project's Geography Type is missing",
           nchar(ZIP) != 5 | is_null(ZIP) ~ "ZIP is missing or not valid"
         ),
         Type = if_else(is_null(Geocode) | is_null(GeographyType) |
                          is_null(CoCCode),
                        "High Priority",
                        "Error")) %>%
  select(all_of(PDDEcols))

# Missing Inventory Record Is a residential project but has no active inventory for the duration of operating period OR for the reporting period
missingInventoryRecord <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(ProjectType %in% c(project_types_w_beds) &
           (InventoryStartDate > OperatingEndDate | 
              InventoryEndDate < OperatingStartDate) & 
           (InventoryStartDate > meta_HUDCSV_Export_End | 
              InventoryEndDate < meta_HUDCSV_Export_Start) 
  ) %>%  
  group_by(ProjectID, ProjectName, OrganizationName) %>%
  summarise(InventoryRecordCount = n()) %>%
  ungroup() %>%
  mutate(
    Issue = "No Current Inventory Record",
    Type = "Warning", # revisit: should this be an Error? (No, sometimes a
    # project can be temporarily closed so it would be ok to have an inventory gap)
    Guidance = paste(
      "This residential project has",
      InventoryRecordCount,
      "current inventory records."
    )
  ) %>% 
  select(all_of(PDDEcols))

# Inventory Start < Operating Start AND
# Inventory End > Operating End or Null
inventoryOutsideOperating <- Inventory %>%
  left_join(Project, by = "ProjectID") %>%
  filter(InventoryStartDate < OperatingStartDate |
           InventoryEndDate > OperatingEndDate |
           (is_null(InventoryEndDate) & !is_null(OperatingEndDate))) %>%
  mutate(Issue = "Inventory outside operating dates",
         Type = "Warning",
         Guidance = case_when(
           is_null(InventoryEndDate) & !is_null(OperatingEndDate) ~
            paste("Inventory ID", InventoryID, 
                  "has no InventoryEndDate, but the project ended on",
                  OperatingEndDate
                  ),
           InventoryEndDate > OperatingEndDate ~
             paste("Inventory ID", InventoryID,
           "ended on", InventoryEndDate, "which is after the project ended on",
           OperatingEndDate),
           InventoryStartDate < OperatingStartDate ~ 
             paste("Inventory ID", InventoryID,
                   "starts on", InventoryStartDate,
                   "which precedes the project's Operating Start Date of",
                   OperatingStartDate)
         ) 
  ) %>%
  select(all_of(PDDEcols))

# HMIS Participating != 1, OR VSP != 0 but client level data in file
hmisNotParticipatingButClient <- Project %>%
  left_join(unique(Enrollment[c("PersonalID", "ProjectID")]), by = "ProjectID") %>%
  left_join(Organization %>% select(OrganizationID, VictimServiceProvider), 
            by = "OrganizationID") %>%
  filter((HMISParticipatingProject != 1 | VictimServiceProvider != 0) & 
           !is_null(PersonalID)) %>%
  mutate(Issue = "Non-HMIS-Participating project has client-level data",
         Type = "Warning",
         Guidance = "Please check that this project is marked correctly as
         non-participating. There is client data in this project."
  ) %>%
  select(all_of(PDDEcols)) 


##### For later-------
# Incompatible Funding Source and Project Type Funding Source X can only be used with Project Type Y. Project Type A can only be used with Funding Source B. (this will need a lot more detail, hold on this one)
# Utilization Hold - Push to next version

### Put it together ----
pdde_main <- rbind(
  subpopNotTotal,
  operatingEndMissing,
  missingCoCInfo,
  missingInventoryRecord,
  inventoryOutsideOperating,
  hmisNotParticipatingButClient
)
