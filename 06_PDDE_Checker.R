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

# Subpop beds should equal Total Beds -------------------------------------
subpopNotTotal <- Inventory %>%
  left_join(Project0(), by = "ProjectID") %>%
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
# recent exit was 30+ days ago
operating_end_missing <- Enrollment %>%
  group_by(ProjectID) %>%
  mutate(NumOpenEnrollments = sum(is.na(ExitDate)),
         MostRecentEnrollment = max(ExitAdjust, na.rm = TRUE) # keep* or change

  ) %>%
  ungroup() %>%
  left_join(Project0() %>% 
              select(ProjectID, ProjectName, OrganizationName) %>%
              unique(), 
            by = "ProjectID") %>%
  filter(NumOpenEnrollments == 0 & 
           MostRecentEnrollment < 
           meta_HUDCSV_Export_Date() - 30 &
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

HousingTypeDF <- Project %>% 
  select(ProjectID, HousingType)

missing_CoC_Info <- Project0() %>%
  left_join(ProjectCoC, by = "ProjectID") %>%
  left_join(HousingTypeDF, by = "ProjectID") %>% 
  filter(is.na(Address1) | 
           is.na(City) | 
           is.na(State) | 
           is.na(Geocode) | 
           is.na(GeographyType) | 
           !str_detect(ZIP, "^[0-9]{5}(-[0-9]{4})?$") |
           is.na(ZIP) |
           is.na(CoCCode)
  )

missing_CoC_Geography <- missing_CoC_Info %>%
  filter(is.na(Geocode) | is.na(GeographyType) |
           is.na(CoCCode)) %>%
  merge_check_info(checkIDs = 5) %>%
  mutate(
    Detail = paste0(
      "This project is missing a valid: ",
      if_else(is.na(Geocode), "Geocode, ", ""),
      if_else(is.na(GeographyType), "Geography Type, ", ""),
      if_else(is.na(CoCCode), "CoC Code, ", "")
    ) %>%
    str_remove(", $")
  ) %>%
  select(all_of(PDDEcols))

missing_CoC_Address <- missing_CoC_Info %>%
  filter(
    # no one should have an invalid zip
    (!str_detect(ZIP, "^[0-9]{5}(-[0-9]{4})?$") | is.na(ZIP)) |
    (
      # non-VSPs and non-tenant-scattered-sites should also not be missing address, city, and state
      !(VictimServiceProvider==1 | HousingType==tenant_scattered_site) &
      (is.na(Address1) | is.na(City) | is.na(State))
    )
  ) %>%
  merge_check_info(checkIDs = 42) %>%
  mutate(
    Detail = paste0(
      "This project is missing a valid: ",
      if_else(is.na(Address1), "Address", ""),
      if_else(is.na(City), ", City", ""),
      if_else(is.na(State), ", State", ""),
      if_else(!str_detect(ZIP, "^[0-9]{5}(-[0-9]{4})?$") | is.na(ZIP), ", ZIP", "")
    ) %>%
      str_remove(", $")
  ) %>%
  select(all_of(PDDEcols))

# Missing Inventory Record Is a residential project but has no active inventory
# for the duration of operating period OR for the reporting period

missing_inventory_record <- Project0() %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(ProjectType %in% project_types_w_beds &
           (RRHSubType == 2 | is.na(RRHSubType)) &
           is.na(InventoryID)) %>% 
  merge_check_info(checkIDs = 43) %>%
  mutate(Detail = "") %>%
  select(all_of(PDDEcols))

# Inventory Start < Operating Start AND
# Inventory End > Operating End or Null

inventory_start_precedes_operating_start <- activeInventory %>%
  filter(InventoryStartDate < OperatingStartDate) %>%
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
  merge_check_info(checkIDs = 79) %>%
  select(all_of(PDDEcols))


operating_end_precedes_inventory_end <- activeInventory %>%
  filter(coalesce(InventoryEndDate, no_end_date) >
           coalesce(OperatingEndDate, no_end_date)
  ) %>%
  mutate(
    Detail = case_when(
      is.na(InventoryEndDate) & !is.na(OperatingEndDate) ~
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
        ),
      !is.na(InventoryEndDate) & !is.na(OperatingEndDate) ~
        str_squish(
          paste0(
            "This project ended on ", OperatingEndDate,
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
  merge_check_info(checkIDs = 44) %>%
  select(all_of(PDDEcols))

# Active Inventory with No Enrollments ---------
# Active inventory records (with non-overflow beds) should have enrollments within their bounds
active_inventory_w_no_enrollments <- qDT(activeInventory) %>% 
  fsubset(
    (is.na(Availability) | Availability != 3) &
      BedInventory > 0 & !is.na(BedInventory)
  ) %>%
  fselect(ProjectID, InventoryID, InventoryStartDate, InventoryEndDate) %>%
  join(
    Enrollment %>% select(ProjectID, EntryDate, ExitAdjust),
    on = "ProjectID",
    multiple = TRUE,
    how="inner"
  ) %>%
  fgroup_by(ProjectID) %>%
  fmutate(
    # Check if inventory span overlaps with any enrollments
    any_inventory_overlap = anyv(
      (EntryDate <= InventoryEndDate | is.na(InventoryEndDate)) & 
        ExitAdjust >= InventoryStartDate,
      TRUE
    )
  ) %>%
  fungroup() %>%
  fsubset(!any_inventory_overlap) %>%
  funique(cols = c("ProjectID")) %>%
  # Bring in DQ cols
  join(
    Project0(),
    on = "ProjectID",
    how = "inner",
    multiple = TRUE
  ) %>%
  merge_check_info_dt(checkIDs = 141) %>%
  fmutate(Detail = "") %>%
  fselect(PDDEcols) %>%
  fsubset(!is.na(ProjectID))

# RRH project w no SubType ------------------------------------------------

rrh_no_subtype <- Project0() %>%
  filter(ProjectType == 13 & is.na(RRHSubType)) %>%
  merge_check_info(checkIDs = 110) %>%
  mutate(Detail = "") %>%
  select(all_of(PDDEcols))


# VSP with HMIS Participation ---------------------------------------------

vsp_projects <- Project0() %>%
  filter(VictimServiceProvider == 1) %>%
  pull(ProjectID) %>%
  unique()

participating_projects <- Project0() %>%
  inner_join(HMISParticipation %>%
               filter(HMISParticipationType == 1),
             by = "ProjectID") %>%
  pull(ProjectID) %>%
  unique()

vsps_that_are_hmis_participating <- 
  base::intersect(vsp_projects, participating_projects)

vsps_in_hmis <- Project0() %>%
  filter(ProjectID %in% c(vsps_that_are_hmis_participating)) %>%
  merge_check_info(checkIDs = 133) %>%
  mutate(Detail = "") %>%
  select(all_of(PDDEcols))
  
 # Zero Utilization --------------------------------------------------------
# HMIS participating projects that have ANY active inventory (with available beds) 
# should not have 0 enrollments
zero_utilization <- HMIS_participating_projects_w_active_inv_no_overflow %>%
  # Only keep (inventory) with non-overflow beds
  # Overflow beds are meant to be available on an ad hoc or temporary basis. And 
  # since this dataset is used for flagging inventory-related problems, we don't
  # want to flag cases with only overflow since those beds could reasonably not be filled
  fsubset(
    (is.na(Availability) | Availability != 3) &
      BedInventory > 0 & !is.na(BedInventory)
  ) %>%
  funique(cols = "ProjectTimeID") %>%
  join(
    Enrollment,
    on = "ProjectTimeID",
    multiple = TRUE,
    how="anti"
  ) %>%
  join(Project0(), on = "ProjectID", how = "inner") %>%
  merge_check_info_dt(checkIDs = 83) %>%
  fmutate(Detail = "") %>%
  fselect(PDDEcols) %>%
  fsubset(!is.na(ProjectID))

# if a comparable db uses Eva, this will not flag for them^

# OLD:
# projects_w_beds <- activeInventory %>%
#   filter(BedInventory > 0) %>%
#   pull(ProjectID) %>%
#   unique()
# 
# projects_w_clients <- Enrollment %>%
#   pull(ProjectID) %>%
#   unique()
# 
# res_projects_no_clients <- setdiff(projects_w_beds, projects_w_clients)
# 
# zero_utilization <- Project0() %>%
#   inner_join(HMISParticipation %>%
#               filter(HMISParticipationType == 1) %>%
#               distinct(ProjectID), by = "ProjectID") %>%
#   filter(ProjectID %in% c(res_projects_no_clients)) %>%
#   merge_check_info(checkIDs = 83) %>%
#   mutate(Detail = "") %>%
#   select(all_of(PDDEcols))

# if a comparable db uses Eva, this will not flag for them^

# RRH-SO projects with active inventory -----------------------------------

rrh_so_w_inventory <- activeInventory %>%
  mutate(
    InventoryActivePeriod = 
      interval(InventoryStartDate,
               coalesce(InventoryEndDate, no_end_date))
  ) %>%
  select(InventoryID, ProjectID, InventoryActivePeriod, BedInventory) %>%
  left_join(Project0(), join_by(ProjectID)) %>%
  mutate(RRHSOyn = ProjectType == 13 & RRHSubType == 1,
         RRHSOActivePeriod =
           interval(OperatingStartDate,
                    coalesce(OperatingEndDate, no_end_date)),
         Detail = "") %>%
  filter(RRHSOyn == TRUE & 
           !is.na(BedInventory) & BedInventory > 0 &
           int_overlaps(InventoryActivePeriod, RRHSOActivePeriod)) %>%
  merge_check_info(checkIDs = 132) %>%
  select(all_of(PDDEcols))

# For later.. -------------------------------------------------------------

# Incompatible Funding Source and Project Type Funding Source X can only be used
# with Project Type Y. Project Type A can only be used with Funding Source B.
# (this will need a lot more detail, hold on this one)


# Overlapping participations ----------------------------------------------
overlapping_ce_participation <- CEParticipation %>%
  left_join(Project0() %>% select(ProjectID, OrganizationName, ProjectName),
            by = "ProjectID") %>%
  group_by(ProjectID) %>%
  arrange(CEParticipationStatusStartDate) %>%
  mutate(PreviousCEParticipationID = lag(CEParticipationID),
         PreviousCEStart = lag(CEParticipationStatusStartDate),
         PreviousCEEnd = lag(CEParticipationStatusEndDate)) %>%
  ungroup() %>%
  filter(!is.na(PreviousCEParticipationID)) %>%
  mutate(ParticipationPeriod =
           interval(
             CEParticipationStatusStartDate,
             coalesce(CEParticipationStatusEndDate, no_end_date)),
         PreviousParticipationPeriod = 
           interval(
             PreviousCEStart,
             coalesce(PreviousCEEnd, no_end_date)
           ),
         OverlapYN = int_overlaps(ParticipationPeriod, PreviousParticipationPeriod)
  ) %>%
  filter(OverlapYN == TRUE) %>%
  mutate(Detail = paste(
           "This project's first participation period goes from",
           CEParticipationStatusStartDate,
           "to",
           if_else(is.na(CEParticipationStatusEndDate),
                   "current,",
                   paste0(CEParticipationStatusEndDate, ",")),
           "and the other participation period goes from",
           PreviousCEStart,
           "to",
           if_else(is.na(PreviousCEEnd),
                   "current.",
                   paste0(PreviousCEEnd, "."))
         )) %>%
  merge_check_info(checkIDs = 128) %>%
  select(all_of(PDDEcols))

overlapping_hmis_participation <- HMISParticipation %>%
  left_join(Project0() %>% select(ProjectID, OrganizationName, ProjectName),
            by = "ProjectID") %>%
  group_by(ProjectID) %>%
  arrange(HMISParticipationStatusStartDate) %>%
  mutate(
    PreviousHMISParticipationID = lag(HMISParticipationID),
    PreviousHMISStart = lag(HMISParticipationStatusStartDate),
    PreviousHMISEnd = lag(HMISParticipationStatusEndDate)) %>%
  ungroup() %>%
  filter(!is.na(PreviousHMISParticipationID)) %>%
  mutate(ParticipationPeriod =
           interval(
             HMISParticipationStatusStartDate,
             coalesce(HMISParticipationStatusEndDate, no_end_date)),
         PreviousParticipationPeriod = 
           interval(
             PreviousHMISStart,
             coalesce(PreviousHMISEnd, no_end_date)
           ),
         OverlapYN = int_overlaps(ParticipationPeriod, PreviousParticipationPeriod)
         ) %>% 
         filter(OverlapYN) %>%
         mutate(Detail = paste(
           "This project's first HMIS participation period goes from",
           HMISParticipationStatusStartDate,
           "to",
           if_else(is.na(HMISParticipationStatusEndDate),
                   "today,",
                   paste0(HMISParticipationStatusEndDate, ",")),
           "and the other participation period goes from",
           PreviousHMISStart,
           "to",
           if_else(is.na(PreviousHMISEnd),
                   "today.",
                   paste0(PreviousHMISEnd, "."))
         )) %>%
  merge_check_info(checkIDs = 131) %>%
  select(all_of(PDDEcols))


# Bed Type incompatible with Housing Type -----------------------------------
# For ES projects, if HousingType is 1 or 2 (site-based), then BedType should be 1 (facility based beds) or 3 (Other bed type). If HousingType is 3 (tenant-based), then BedType should be 2 (voucher beds).

ES_BedType_HousingType <- activeInventory %>%
  left_join(Project0() %>% select(ProjectID, ProjectType), by = "ProjectID") %>%
  left_join(HousingTypeDF, by = "ProjectID") %>% 
  filter(ProjectType %in% c(es_ee_project_type, es_nbn_project_type) &
           ((HousingType %in% c(client_single_site, client_multiple_sites) & !(ESBedType %in% c(1, 3))) | (HousingType==tenant_scattered_site & ESBedType!=2)) 
  ) %>%
  merge_check_info(checkIDs = 135) %>% 
  mutate(Detail = "Bed Type incompatible with Housing Type:  Facility-based beds should align to the Housing Type of site-based and voucher-based beds should align to the Housing Type of tenant-based."
  ) %>%
  select(all_of(PDDEcols))


# Project CoC Missing Bed Inventory & Incorrect CoC in bed inventory -----------------------------------

activeInventory_COC_merged <-  join(
    activeInventory,
    ProjectCoC, 
    on = c("ProjectID", "CoCCode"), 
    how="full",
    multiple = TRUE,
    column="source"
  ) %>%
  join(Project0(), on="ProjectID", drop.dup.cols = "x")

# Throw a warning if there is no inventory record for a ProjectID and COCCode combo in the ProjectCoC data

Active_Inventory_per_COC <- activeInventory_COC_merged %>%
  fsubset(source == "ProjectCoC") %>%
  join(missing_inventory_record, on = "ProjectID", how="anti") %>%
  join(Project %>% select(ProjectID, ProjectType, RRHSubType), on="ProjectID", how="left") %>%
  fsubset(ProjectType %in% project_types_w_beds &
           (RRHSubType == 2 | is.na(RRHSubType))) %>% 
  merge_check_info(checkIDs = 136) %>% 
  mutate(Detail = "Residential projects must have a bed inventory for each CoC they serve."
  ) %>%
  select(all_of(PDDEcols)) %>%
  unique()
  
# Throw an error if there is no COC record for a ProjectID and COCCode combo in the inventory data

COC_Records_per_Inventory <- activeInventory_COC_merged %>%
  fsubset(source == "activeInventory") %>%
  merge_check_info(checkIDs = 137) %>%
  mutate(Detail = str_squish("Any CoC represented in a project's active bed 
                             inventory records must also be listed as a CoC 
                             associated with the Project.")) %>%
  select(all_of(PDDEcols)) %>%
  unique()

# More units than beds in inventory record. -----------------------------------
more_units_than_beds_inventory <- activeInventory %>%
  filter(UnitInventory > BedInventory) %>% 
merge_check_info(checkIDs = 138) %>%
  mutate(Detail = "An inventory record cannot have more units than total number of beds. Please update this inventory record in HMIS to ensure that units are less than or equal to the number of beds."
  ) %>%
  select(all_of(PDDEcols))


# Client-level data in VSP organization. -----------------------------------
# Projects under Organizations marked as Victim Service Providers should not have client data in HMIS
# If VictimServiceProvider==1, then flag as high priority error if client data is present (i.e. any enrollments).

vsp_clients <- Project0() %>%
  filter(VictimServiceProvider==1) %>%
  inner_join(Enrollment, by = "ProjectID") %>%
  merge_check_info(checkIDs = 139) %>%
  mutate(Detail = "Projects under Organizations marked as Victim Service Providers should not have client data in HMIS."
  ) %>%
  select(all_of(PDDEcols)) %>% 
  unique()


# Put it all together -----------------------------------------------------

pdde_main(bind_rows(
  subpopNotTotal,
  operating_end_missing,
  rrh_no_subtype,
  missing_CoC_Geography,
  missing_CoC_Address,
  missing_inventory_record,
  operating_end_precedes_inventory_end,
  overlapping_ce_participation,
  overlapping_hmis_participation,
  inventory_start_precedes_operating_start,
  active_inventory_w_no_enrollments,
  rrh_so_w_inventory,
  vsps_in_hmis,
  zero_utilization,
  ES_BedType_HousingType,
  Active_Inventory_per_COC,
  COC_Records_per_Inventory,
  more_units_than_beds_inventory,
  vsp_clients
) %>%
  unique() %>%
  mutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning")))
)


