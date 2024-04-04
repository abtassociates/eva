
# using FY24-ICF-inactive-inventory-oldOrgNames sample data set

colnames(Inventory)

Inventory %>% #filter(ProjectID == "1330") %>% # <- has a lot of inv records
  select(ProjectID, BedInventory, InventoryStartDate, InventoryEndDate) %>%
  left_join(Project %>% select(ProjectID, OrganizationName, OperatingStartDate, OperatingEndDate),
            by = "ProjectID") %>%
  mutate(
    ReportPeriod = interval(meta_HUDCSV_Export_Start, meta_HUDCSV_Export_End),
    OperatingPeriod = interval(
      OperatingStartDate,
      coalesce(OperatingEndDate, meta_HUDCSV_Export_End)
    ),
    Active = if_else(
      is.na(InventoryEndDate) |
        InventoryEndDate >= OperatingStartDate,
      1,
      0
    )
  ) %>%
  filter(InventoryStartDate < OperatingStartDate & Active == 0) %>%
  view()

# in the dataset, the inventoryOUtsideOpererating df catches 1 instance of this
# issue where the inventory record is active and its StartDate precedes the 
# OpStart. In the code above, if you change the "Active == 0" to "Active == 1"
# you'll see the record that *is* flagging in Eva, which we want to flag. If you
# run it Active == 0, you  can see that there are a lot of records where the
# InventoryStart precedes the OpStart, but it's inactive so it isn't flagging in
# Eva, which is what we want.


