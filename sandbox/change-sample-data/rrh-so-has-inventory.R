# run 00-Start_here.R EXCEPT for the "Data Prep" section

# find an rrh-so

rrh_sso_project <- Project %>%
  filter(ProjectType == 13 & RRHSubType == 1) %>%
  pull(ProjectID) %>% head(1L)

colnames(Inventory)

test <- Inventory %>%
  add_row(
    InventoryID = "2000",
    ProjectID = rrh_sso_project,
    CoCCode = "XX-501",
    HouseholdType = 1,
    Availability = NA,
    UnitInventory = 8,
    BedInventory = 15,
    CHVetBedInventory = 0, 
    YouthVetBedInventory = 0,
    VetBedInventory = 0,
    CHYouthBedInventory = 0,
    YouthBedInventory = 0,
    CHBedInventory = 0,
    OtherBedInventory = 15,
    ESBedType = NA,
    InventoryStartDate = ymd("2020-01-01"), 
    InventoryEndDate = NA,
    DateCreated = ymd_hms("2021-10-07 14:30:16"),
    DateUpdated = ymd_hms("2021-10-07 14:30:16"),
    UserID = "54a",
    DateDeleted = NA, 
    ExportID = "1036"
  )

Inventory <- test

get_dupes(Inventory, InventoryID)

write.csv(
  Inventory,
  here(paste0(directory, "data/Inventory.csv")),
  na = "",
  row.names = FALSE
)
