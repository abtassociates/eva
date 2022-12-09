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
  filter(is.na(Address1) | 
           is.na(City) | 
           is.na(State) | 
           is.na(Geocode) | 
           is.na(GeographyType) | 
           nchar(ZIP) != 5 |
           is.na(ZIP) |
           is.na(CoCCode)
  ) %>%
  mutate(Issue = "Missing Geography Information",
         Guidance = case_when(
           is.na(CoCCode) ~ "This project's CoC Code is missing",
           is.na(Address1) ~ "This project's Address is missing",
           is.na(City) ~ "This project's City is missing",
           is.na(State) ~ "This project's State is missing",
           is.na(Geocode) ~ "This project's Geocode is missing",
           is.na(GeographyType) ~ "This project's Geography Type is missing",
           nchar(ZIP) != 5 | is.na(ZIP) ~ "ZIP is missing or not valid"
         ),
         Type = if_else(is.na(Geocode) | is.na(GeographyType) |
                          is.na(CoCCode),
                        "High Priority",
                        "Error")) %>%
  select(all_of(PDDEcols))

# Missing Inventory Record Is a residential project but has no active inventory for the duration of operating period OR for the reporting period
missingInventoryRecord <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(ProjectType %in% c(project_types_w_beds) &
           is.na(InventoryID)) %>% 
  mutate(
    Issue = "No Inventory Records",
    Type = "Error",
    Guidance = str_squish(
      paste("Project ID", 
            ProjectID,
            "has no Inventory records. Residential project types should have
            inventory data.")
    )
  )  %>% 
  select(all_of(PDDEcols))

# Inventory Start < Operating Start AND
# Inventory End > Operating End or Null
inventoryOutsideOperating <- Inventory %>%
  left_join(Project, by = "ProjectID") %>%
  mutate(
    Issue = case_when(
      InventoryStartDate < OperatingStartDate ~
        "Inventory Start Precedes Project Operating Start",
      coalesce(InventoryEndDate, as.Date(meta_HUDCSV_Export_Date)) >
        coalesce(OperatingEndDate, as.Date(meta_HUDCSV_Export_Date)) ~
        "Project Operating End precedes Inventory End",
      TRUE ~ "none"
    ),
    Type = if_else(
      Issue == "Inventory Start Precedes Project Operating Start",
      "Warning",
      "Error"
    ),
    Guidance = case_when(
      Issue == "Inventory Start Precedes Project Operating Start" ~
        str_squish(
          paste0(
            "Project ID ",
            ProjectID,
            " may have been merged with another project which would explain
            why the Inventory Start Date of ",
            InventoryStartDate,
            " is prior to the project's Operating Start Date of ",
            OperatingStartDate,
            ". Please be sure this is the case and that it is not a typo."
          )
        ),
      Issue == "Project Operating End precedes Inventory End" &
        is.na(InventoryEndDate) &
        !is.na(OperatingEndDate) ~
        str_squish(
          paste0(
            "Project ID ",
            ProjectID,
            " has an Inventory Record (",
            InventoryID,
            ") with an open Inventory End Date but the Project Operating End Date
            is ",
            OperatingEndDate,
            ". Please either end-date the Inventory, or if the project is still
            operating, clear the project's Operating End Date."
          )
        ),
      Issue == "Project Operating End precedes Inventory End" &
        !is.na(InventoryEndDate) &
        !is.na(OperatingEndDate) ~
        str_squish(
          paste0(
            "Project ID ", ProjectID, 
            " ended on ", OperatingEndDate,
            " but Inventory record ",
            InventoryID,
            " ended ",
            as.numeric(difftime(InventoryEndDate, OperatingEndDate, units = "days")),
            " days after that on ",
            InventoryEndDate,
            ". Please correct whichever date is incorrect."
          )
        )
    )
  ) %>% 
  filter(Issue != "none") %>%
  select(all_of(PDDEcols))

# HMIS Participating != 1, OR VSP != 0 but client level data in file
hmisNotParticipatingButClient <- Project %>%
  left_join(unique(Enrollment[c("PersonalID", "ProjectID")]), by = "ProjectID") %>%
  left_join(Organization %>% select(OrganizationID, VictimServiceProvider), 
            by = "OrganizationID") %>%
  filter((HMISParticipatingProject != 1 | VictimServiceProvider != 0) & 
           !is.na(PersonalID)) %>%
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
