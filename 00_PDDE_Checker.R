PDDEcols = c("ProjectID", "ProjectName", "Issue", "Guidance", "OrganizationName")

projectWithOrg <- Project %>% 
  left_join(Organization, by = "OrganizationID") %>%
  select(ProjectID, 
         ProjectName, 
         ProjectType, 
         HMISParticipatingProject,
         OrganizationID, 
         OrganizationName, 
         OperatingStartDate, 
         OperatingEndDate, 
         VictimServiceProvider
  )

# Subpop beds = TotalBeds
subpopNotTotal <- Inventory %>%
  left_join(projectWithOrg, by = "ProjectID") %>%
  filter(ProjectType %in% c(1,2,3,8,9,10,13) & 
           (CHVetBedInventory + 
              YouthVetBedInventory + 
              VetBedInventory + 
              CHYouthBedInventory + 
              YouthBedInventory + 
              CHBedInventory + 
              OtherBedInventory
           ) != BedInventory
  ) %>%
  mutate(Issue = "Sum of dedicated beds (including other beds) != total beds",
         Guidance = "Inventory for CH Vets, Youth vets, Vets, CH Youth, Youth, CH, and Other do not sum to the total number of beds"
  ) %>%
  select(all_of(PDDEcols))

# Missing Operating End Date If a project has no open enrollments and the most recent Enrollment was 30+ days ago
operatingEndMissing <- Enrollment %>%
  left_join(projectWithOrg, by = "ProjectID") %>%
  group_by(ProjectID) %>%
  mutate(NumOpenEnrollments = sum(is_null(ExitDate)),
         MostRecentEnrollment = max(ExitDate)
  ) %>%
  ungroup() %>%
  filter(NumOpenEnrollments == 0 & MostRecentEnrollment >= today() - 30) %>%
  mutate(Issue = "Missing Operating End Date",
         Guidance = "If a project has no open enrollments and the most recent Enrollment was 30+ days ago"
  ) %>%
  select(all_of(PDDEcols))

# Missing CoC Information Missing address field(s), Missing Geocode, Missing Geography Type, Invalid Zip Code if possible
missingCoCInfo <- ProjectCoC %>%
  left_join(projectWithOrg, by = "ProjectID") %>%
  filter(is_null(Address1) | 
           is_null(City) | 
           is_null(State) | 
           is_null(Geocode) | 
           is_null(GeographyType) | 
           (length(ZIP) != 5 & length(ZIP) != 10)
  ) %>%
  mutate(Issue = "Missing CoC Information",
         Guidance = "Missing address field(s), Missing Geocode, Missing Geography Type, Invalid Zip Code"
  ) %>%
  select(all_of(PDDEcols))

# Missing Inventory Record Is a residential project but has no active inventory for the duration of operating period OR for the reporting period
missingInventoryRecord <- Inventory %>%
  left_join(projectWithOrg, by = "ProjectID") %>%
  filter(ProjectType %in% c(1,2,3,8,9,10,13) & 
           (InventoryStartDate > OperatingEndDate | InventoryEndDate < OperatingStartDate) & 
           (InventoryStartDate > meta_HUDCSV_Export_End | InventoryEndDate < meta_HUDCSV_Export_Start)
  ) %>%
  mutate(Issue = "Missing Inventory Record",
         Guidance = "Is a residential project but has no active inventory for the duration of operating period OR for the reporting period"
  ) %>%
  select(all_of(PDDEcols))

# Funder.StartDate <= Project.OperatingStartDate
funderStartOnOrBeforeOperatingStart <- Funder %>%
  left_join(projectWithOrg, by = "ProjectID") %>%
  filter(StartDate <= OperatingStartDate) %>%
  mutate(Issue = "Funding period before operating start",
         Guidance = "Funding should begin after the Project begins"
  ) %>%
  select(all_of(PDDEcols))

# Inventory Start < Operating Start AND
# Inventory End > Operating End or Null
inventoryOutsideOperating <- Inventory %>%
  left_join(projectWithOrg, by = "ProjectID") %>%
  filter(InventoryStartDate < OperatingStartDate |
           InventoryEndDate > OperatingEndDate) %>%
  mutate(Issue = "Inventory outside operating dates",
         Guidance = "Inventory range is outside project operating range" 
  ) %>%
  select(all_of(PDDEcols))

# HMIS Participating != 1, OR VSP != 0 but client level data in file
hmisNotParticipatingButClient <- Project %>%
  left_join(unique(Enrollment[c("PersonalID", "ProjectID")]), by = "ProjectID") %>%
  filter((HMISParticipatingProject != 1 | VictimServiceProvider != 0) & !is_null(PersonalID)) %>%
  mutate(Issue = "Is HMIS but has client-level data",
         Guidance = "HMIS Projects should not have client-level data"
  ) %>%
  select(all_of(PDDEcols))


##### For later-------
# Incompatible Funding Source and Project Type Funding Source X can only be used with Project Type Y. Project Type A can only be used with Funding Source B. (this will need a lot more detail, hold on this one)
# Utilization Hold - Push to next version


pdde_main <- rbind(
  subpopNotTotal,
  operatingEndMissing,
  missingCoCInfo,
  missingInventoryRecord,
  funderStartOnOrBeforeOperatingStart,
  inventoryOutsideOperating,
  hmisNotParticipatingButClient
)