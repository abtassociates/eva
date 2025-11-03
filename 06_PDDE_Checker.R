###############################
#   PURPOSE: This script conducts the PDDE checks, which are things admins fix 
#   (rather than End-users who fix DQ checks)
###############################
logToConsole(session, "Running PDDE checker")

PDDEcols = c("OrganizationName",
             "ProjectID",
             "ProjectName",
             "Issue",
             "Type",
             "Guidance",
             "Detail")

# Subpop beds should equal Total Beds -------------------------------------
subpopNotTotal <- Inventory %>%
  join(session$userData$Project0, on = "ProjectID", how = 'left') %>%
  fsubset(ProjectType %in% project_types_w_beds &
           (CHVetBedInventory + 
              YouthVetBedInventory + 
              VetBedInventory + 
              CHYouthBedInventory + 
              YouthBedInventory + 
              CHBedInventory + 
              OtherBedInventory
           ) != BedInventory
  ) %>%
  merge_check_info_dt(checkIDs = 46) %>%
  fmutate(Detail = 
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
  fselect(PDDEcols)

# Missing Operating End Date If a project has no open enrollments and the most
# recent exit was 30+ days ago
operating_end_missing <- Enrollment %>%
  fgroup_by(ProjectID) %>%
  fmutate(NumOpenEnrollments = fsum(is.na(ExitDate)),
         MostRecentEnrollment = fmax(ExitAdjust, na.rm = TRUE) # keep* or change

  ) %>%
  fungroup() %>%
  join(session$userData$Project0 %>% 
              fselect(ProjectID, ProjectName, OrganizationName) %>%
              funique(), 
            on = "ProjectID", how = 'left') %>%
  fsubset(NumOpenEnrollments == 0 & 
           MostRecentEnrollment < 
           session$userData$meta_HUDCSV_Export_Date - 30 &
           is.null(OperatingEndDate)) %>%
  merge_check_info_dt(checkIDs = 81) %>%
  fmutate(Detail = paste(
           "This project has no open enrollments and the most recent Exit was",
           MostRecentEnrollment
         )
  ) %>%
  fselect(PDDEcols)

# Missing CoC Information Missing address field(s), Missing Geocode,
# Missing Geography Type, Invalid Zip Code if possible

missing_CoC_Info <- session$userData$Project0 %>%
  join(ProjectCoC, on = "ProjectID", how = 'left') %>% 
  fsubset(is.na(Address1) | 
           is.na(City) | 
           is.na(State) | 
           is.na(Geocode) | 
           is.na(GeographyType) | 
           !str_detect(ZIP, "^[0-9]{5}(-[0-9]{4})?$") |
           is.na(ZIP) |
           is.na(CoCCode)
  )

missing_CoC_Geography <- missing_CoC_Info %>%
  fsubset(is.na(Geocode) | is.na(GeographyType) |
           is.na(CoCCode)) %>%
  merge_check_info_dt(checkIDs = 5) %>%
  fmutate(
    Detail = paste0(
      "This project is missing a valid: ",
      fifelse(is.na(Geocode), "Geocode, ", ""),
      fifelse(is.na(GeographyType), "Geography Type, ", ""),
      fifelse(is.na(CoCCode), "CoC Code, ", "")
    ) %>%
    str_remove(", $")
  ) %>%
  fselect(PDDEcols)

missing_CoC_Address <- missing_CoC_Info %>%
  fsubset(
    # no one should have an invalid zip
    (!str_detect(ZIP, "^[0-9]{5}(-[0-9]{4})?$") | is.na(ZIP)) |
    (
      # non-VSPs and non-tenant-scattered-sites should also not be missing address, city, and state
      !(VictimServiceProvider==1 | HousingType==tenant_scattered_site) &
      (is.na(Address1) | is.na(City) | is.na(State))
    )
  ) %>%
  merge_check_info_dt(checkIDs = 42) %>%
  fmutate(
    Detail = paste0(
      "This project is missing a valid: ",
      fifelse(is.na(Address1), "Address", ""),
      fifelse(is.na(City), ", City", ""),
      fifelse(is.na(State), ", State", ""),
      fifelse(!str_detect(ZIP, "^[0-9]{5}(-[0-9]{4})?$") | is.na(ZIP), ", ZIP", "")
    ) %>%
      str_remove(", $")
  ) %>%
  fselect(PDDEcols)

# Missing Inventory Record Is a residential project but has no active inventory
# for the duration of operating period OR for the reporting period

missing_inventory_record <- session$userData$Project0 %>%
  join(Inventory, on = "ProjectID", how = 'left') %>%
  fsubset(ProjectType %in% project_types_w_beds &
           (RRHSubType == 2 | is.na(RRHSubType)) &
           is.na(InventoryID)) %>% 
  merge_check_info_dt(checkIDs = 43) %>%
  fmutate(Detail = "") %>%
  fselect(PDDEcols)

# Inventory Start < Operating Start AND
# Inventory End > Operating End or Null

inventory_start_precedes_operating_start <- activeInventory %>%
  fsubset(InventoryStartDate < OperatingStartDate) %>%
  fmutate(
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
  merge_check_info_dt(checkIDs = 79) %>%
  fselect(PDDEcols)


operating_end_precedes_inventory_end <- activeInventory %>%
  fsubset(fcoalesce(InventoryEndDate, no_end_date) >
           fcoalesce(OperatingEndDate, no_end_date)
  ) %>%
  fmutate(
    Detail = fcase(
      is.na(InventoryEndDate) & !is.na(OperatingEndDate),
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
      !is.na(InventoryEndDate) & !is.na(OperatingEndDate),
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
  merge_check_info_dt(checkIDs = 44) %>%
  fselect(PDDEcols)

# Active Inventory with No Enrollments ---------
# Active inventory records (with non-overflow beds) should have enrollments within their bounds
get_active_inventory_no_enrollments <- function() {
  if(fnrow(activeInventory) == 0) return(NULL)
  
  active_inventory_w_no_enrollments <- qDT(activeInventory) %>% 
    fsubset(
      (is.na(Availability) | Availability != 3) &
        BedInventory > 0 & !is.na(BedInventory)
    ) 
  
  if(fnrow(active_inventory_w_no_enrollments) == 0) return(NULL)
  
  active_inventory_w_no_enrollments %>%
    fselect(ProjectID, InventoryID, InventoryStartDate, InventoryEndDate) %>%
    join(
      Enrollment %>% fselect(ProjectID, EntryDate, ExitAdjust),
      on = "ProjectID",
      multiple = TRUE,
      how="inner"
    ) %>%
    fmutate(
      # Check if inventory span overlaps with any enrollments
      inventory_overlap = (EntryDate <= InventoryEndDate | is.na(InventoryEndDate)) & 
        ExitAdjust >= InventoryStartDate
    ) %>%
    fgroup_by(ProjectID) %>%
    fmutate(any_inventory_overlap = any(inventory_overlap, na.rm=TRUE)) %>%
    fungroup() %>%
    fsubset(!any_inventory_overlap) %>%
    funique(cols = c("ProjectID")) %>%
    # Bring in DQ cols
    join(
      session$userData$Project0,
      on = "ProjectID",
      how = "inner",
      multiple = TRUE
    ) %>%
    merge_check_info_dt(checkIDs = 141) %>%
    fmutate(Detail = "") %>%
    fselect(PDDEcols) %>%
    fsubset(!is.na(ProjectID))
}
active_inventory_w_no_enrollments <- get_active_inventory_no_enrollments()

# RRH project w no SubType ------------------------------------------------

rrh_no_subtype <- session$userData$Project0 %>%
  fsubset(ProjectType == 13 & is.na(RRHSubType)) %>%
  merge_check_info_dt(checkIDs = 110) %>%
  fmutate(Detail = "") %>%
  fselect(PDDEcols)


# VSP with HMIS Participation ---------------------------------------------

vsp_projects <- session$userData$Project0 %>%
  fsubset(VictimServiceProvider == 1) %>%
  pull(ProjectID) %>%
  funique()

participating_projects <- session$userData$Project0 %>%
  join(HMISParticipation %>%
               fsubset(HMISParticipationType == 1),
             on = "ProjectID", how = 'inner') %>%
  pull(ProjectID) %>%
  funique()

vsps_that_are_hmis_participating <- 
  base::intersect(vsp_projects, participating_projects)

vsps_in_hmis <- session$userData$Project0 %>%
  fsubset(ProjectID %in% c(vsps_that_are_hmis_participating)) %>%
  merge_check_info_dt(checkIDs = 133) %>%
  fmutate(Detail = "") %>%
  fselect(PDDEcols)
  
# Zero Utilization --------------------------------------------------------
# HMIS participating projects that have ANY active inventory (with available beds) 
# should not have 0 enrollments
zero_utilization <- qDT(ProjectSegments) %>%
  # HMiS-participating projects
  fsubset(HMISParticipationType == 1, 
          ProjectID, 
          ProjectTimeID, 
          ProjectType,
          HMISParticipationStatusStartDate, 
          HMISParticipationStatusEndDate,
          OperatingStartDate,
          OperatingEndDate
  ) %>%
  join(
    activeInventory %>% 
      fselect(ProjectID, InventoryStartDate, InventoryEndDate, Availability, BedInventory) %>%
      funique(),
    on = "ProjectID",
    how = "inner",
    multiple = TRUE
  ) %>%
  # Get the Start+End dates for when each Project was Operating, HMIS Participating, and Active (Inventory)
  fmutate(
    ProjectHMISParticipationStart = pmax(
      HMISParticipationStatusStartDate, 
      OperatingStartDate
    ),
    ProjectHMISParticipationEnd = pmin(
      HMISParticipationStatusEndDate,
      OperatingEndDate,
      na.rm = TRUE
    ),
    ProjectHMISActiveParticipationStart = pmax(
      ProjectHMISParticipationStart,
      InventoryStartDate
    ),
    ProjectHMISActiveParticipationEnd = pmin(
      ProjectHMISParticipationEnd,
      InventoryEndDate,
      na.rm = TRUE
    )
  ) %>%
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
  join(
    session$userData$Project0, 
    on = "ProjectID", 
    how = "inner"
  ) %>%
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
# zero_utilization <- session$userData$Project0 %>%
#   inner_join(HMISParticipation %>%
#               filter(HMISParticipationType == 1) %>%
#               distinct(ProjectID), by = "ProjectID") %>%
#   filter(ProjectID %in% c(res_projects_no_clients)) %>%
#   merge_check_info_dt(checkIDs = 83) %>%
#   mutate(Detail = "") %>%
#   select(all_of(PDDEcols))

# if a comparable db uses Eva, this will not flag for them^

# RRH-SO projects with active inventory -----------------------------------

rrh_so_w_inventory <- activeInventory %>%
  fmutate(
    InventoryActivePeriod = 
      interval(InventoryStartDate,
               fcoalesce(InventoryEndDate, no_end_date))
  ) %>%
  fselect(InventoryID, ProjectID, InventoryActivePeriod, BedInventory) %>%
  join(session$userData$Project0, on = 'ProjectID', how = 'left') %>%
  fmutate(RRHSOyn = ProjectType == 13 & RRHSubType == 1,
         RRHSOActivePeriod =
           interval(OperatingStartDate,
                    fcoalesce(OperatingEndDate, no_end_date)),
         Detail = "") %>%
  fsubset(RRHSOyn == TRUE & 
           !is.na(BedInventory) & BedInventory > 0 &
           int_overlaps(InventoryActivePeriod, RRHSOActivePeriod)) %>%
  merge_check_info_dt(checkIDs = 132) %>%
  fselect(PDDEcols)

# For later.. -------------------------------------------------------------

# Incompatible Funding Source and Project Type Funding Source X can only be used
# with Project Type Y. Project Type A can only be used with Funding Source B.
# (this will need a lot more detail, hold on this one)

# Overlapping participations ----------------------------------------------
overlapping_ce_participation <- CEParticipation %>%
  join(session$userData$Project0 %>% fselect(ProjectID, OrganizationName, ProjectName),
            on = "ProjectID", how = 'left') %>%
  fgroup_by(ProjectID) %>%
  roworder(ProjectID, CEParticipationStatusStartDate) %>%
  fmutate(PreviousCEParticipationID = lag(CEParticipationID),
         PreviousCEStart = lag(CEParticipationStatusStartDate),
         PreviousCEEnd = lag(CEParticipationStatusEndDate)) %>%
  fungroup() %>%
  fsubset(!is.na(PreviousCEParticipationID)) %>%
  fmutate(ParticipationPeriod =
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
  fsubset(OverlapYN == TRUE) %>%
  fmutate(Detail = paste(
           "This project's first participation period goes from",
           CEParticipationStatusStartDate,
           "to",
           fifelse(is.na(CEParticipationStatusEndDate),
                   "current,",
                   paste0(CEParticipationStatusEndDate, ",")),
           "and the other participation period goes from",
           PreviousCEStart,
           "to",
           fifelse(is.na(PreviousCEEnd),
                   "current.",
                   paste0(PreviousCEEnd, "."))
         )) %>%
  merge_check_info_dt(checkIDs = 128) %>%
  fselect(PDDEcols)

overlapping_hmis_participation <- HMISParticipation %>%
  join(session$userData$Project0 %>% fselect(ProjectID, OrganizationName, ProjectName),
            on = "ProjectID", how = 'left') %>%
  fgroup_by(ProjectID) %>%
  roworder(ProjectID, HMISParticipationStatusStartDate) %>%
  fmutate(
    PreviousHMISParticipationID = flag(HMISParticipationID),
    PreviousHMISStart = flag(HMISParticipationStatusStartDate),
    PreviousHMISEnd = flag(HMISParticipationStatusEndDate)) %>%
  fungroup() %>%
  fsubset(!is.na(PreviousHMISParticipationID)) %>%
  fmutate(ParticipationPeriod =
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
   fsubset(OverlapYN) %>%
   fmutate(Detail = paste(
     "This project's first HMIS participation period goes from",
     HMISParticipationStatusStartDate,
     "to",
     fifelse(is.na(HMISParticipationStatusEndDate),
             "today,",
             paste0(HMISParticipationStatusEndDate, ",")),
     "and the other participation period goes from",
     PreviousHMISStart,
     "to",
     fifelse(is.na(PreviousHMISEnd),
             "today.",
             paste0(PreviousHMISEnd, "."))
   )) %>%
  merge_check_info_dt(checkIDs = 131) %>%
  fselect(PDDEcols)


# Bed Type incompatible with Housing Type -----------------------------------
# For ES projects, if HousingType is 1 or 2 (site-based), then BedType should be 1 (facility based beds) or 3 (Other bed type). If HousingType is 3 (tenant-based), then BedType should be 2 (voucher beds).

ES_BedType_HousingType <- activeInventory %>%
  join(session$userData$Project0 %>% fselect(ProjectID, ProjectType, HousingType), on = "ProjectID", how = 'left') %>%
  fsubset(ProjectType %in% c(es_ee_project_type, es_nbn_project_type) &
           ((HousingType %in% c(client_single_site, client_multiple_sites) & !(ESBedType %in% c(1, 3))) | (HousingType==tenant_scattered_site & ESBedType!=2)) 
  ) %>%
  merge_check_info_dt(checkIDs = 135) %>% 
  fmutate(Detail = "Bed Type incompatible with Housing Type:  Facility-based beds should align to the Housing Type of site-based and voucher-based beds should align to the Housing Type of tenant-based."
  ) %>%
  fselect(PDDEcols)

# No Enrollments in Services for NbN Project ------------------------------------

nbn_nobns <- nbn_w_hmis_participation %>% # Get enrollments whose projects were NBN and had HMIS Participation
  fselect(EnrollmentID, ProjectID, ProjectName, OrganizationName) %>%
  funique() %>%
  join(services_chk, on = "EnrollmentID", how = "left") %>%
  fgroup_by(ProjectID) %>%
  fmutate(
    miss_all_enroll = fifelse(GRPN() > 0, all(is.na(has_bn_eq_entry)), FALSE) # not having this value implies EnrollmentID NOT in services_check
  ) %>% 
  fungroup()

rm(nbn_w_hmis_participation, services_chk)
nbn_nobns <- nbn_nobns %>% filter(miss_all_enroll) # filter to projects with all enrollmentID missing

nbn_nobns <- nbn_nobns %>% 
  merge_check_info(checkIDs = 106) %>% 
  fmutate(Detail = "") %>%
  fselect(PDDEcols) %>%
  unique()

# Project CoC Missing Bed Inventory & Incorrect CoC in bed inventory -----------------------------------

activeInventory_COC_merged <-  join(
    activeInventory,
    ProjectCoC, 
    on = c("ProjectID", "CoCCode"), 
    how="full",
    multiple = TRUE,
    column="source"
  ) %>%
  join(
    session$userData$Project0, 
    on="ProjectID", 
    drop.dup.cols = "x"
  )

# Throw a warning if there is no inventory record for a ProjectID and COCCode combo in the ProjectCoC data

Active_Inventory_per_COC <- activeInventory_COC_merged %>%
  fsubset(source == "ProjectCoC") %>%
  join(missing_inventory_record, on = "ProjectID", how="anti") %>%
  join(session$userData$Project0 %>% select(ProjectID, ProjectType, RRHSubType), on="ProjectID", how="left") %>%
  fsubset(ProjectType %in% project_types_w_beds &
           (RRHSubType == 2 | is.na(RRHSubType))) %>% 
  merge_check_info_dt(checkIDs = 136) %>% 
  mutate(Detail = "Residential projects must have a bed inventory for each CoC they serve."
  ) %>%
  fselect(PDDEcols) %>%
  unique()
  
# Throw an error if there is no COC record for a ProjectID and COCCode combo in the inventory data

COC_Records_per_Inventory <- activeInventory_COC_merged %>%
  fsubset(source == "activeInventory") %>%
  merge_check_info_dt(checkIDs = 137) %>%
  fmutate(Detail = str_squish("Any CoC represented in a project's active bed 
                             inventory records must also be listed as a CoC 
                             associated with the Project.")) %>%
  fselect(PDDEcols) %>%
  funique()

# More units than beds in inventory record. -----------------------------------
more_units_than_beds_inventory <- activeInventory %>%
  fsubset(UnitInventory > BedInventory) %>% 
merge_check_info_dt(checkIDs = 138) %>%
  fmutate(Detail = "An inventory record cannot have more units than total number of beds. Please update this inventory record in HMIS to ensure that units are less than or equal to the number of beds."
  ) %>%
  fselect(PDDEcols)


# Client-level data in VSP organization. -----------------------------------
# Projects under Organizations marked as Victim Service Providers should not have client data in HMIS
# If VictimServiceProvider==1, then flag as high priority error if client data is present (i.e. any enrollments).

vsp_clients <- session$userData$Project0 %>%
  fsubset(VictimServiceProvider==1) %>%
  join(Enrollment, on = "ProjectID", how = 'inner') %>%
  merge_check_info_dt(checkIDs = 139) %>%
  fmutate(Detail = "Projects under Organizations marked as Victim Service Providers should not have client data in HMIS."
  ) %>%
  fselect(PDDEcols) %>% 
  funique()



# Project Missing in ProjectCoC file --------------------------------------
project_no_coc <- session$userData$Project0 %>%
  fsubset(ContinuumProject==1) %>%
  join(ProjectCoC, on = "ProjectID", how = 'anti') %>%
  merge_check_info_dt(checkIDs = 35) %>%
  fmutate(Detail = "" ) %>%
  fselect(PDDEcols) %>% 
  funique()

# Residential Project Missing Housing Type --------------------------------
res_no_house_type <- session$userData$Project0 %>% # filter to residential projects
  fsubset(ProjectType %in% project_types_w_beds) %>% 
  fsubset(ProjectType != rrh_project_type | # take all that aren't rrh_project_type or
            RRHSubType == 2) %>% # if type == rrh_project_type, take only subset 2
  fsubset(is.na(HousingType) | is.null(HousingType)) %>%  # but HousingType is missing / null
  merge_check_info_dt(checkIDs = 36) %>%
  fmutate(Detail = "" ) %>%
  fselect(PDDEcols) %>% 
  funique()

# Long-Term Seasonal Inventory --------------------------------------------

lt_seas_inv <- session$userData$Project0 %>% 
  join(Inventory, on = "ProjectID", how = 'inner') %>% # inner join gets only ProjectID in Inventory
  fsubset(!is.na(InventoryID)) # filter to those with InventoryID not missing

lt_seas_inv_1 <- lt_seas_inv %>% 
  fsubset(is.na(Availability)) # filter to missing Availability

lt_seas_inv_2 <- lt_seas_inv %>% 
  fsubset(Availability == 2) %>% # filter to seasonal Availabilty
  fmutate(avail_days = InventoryEndDate - InventoryStartDate) %>% # calculate available days
  fsubset(is.na(InventoryEndDate) | avail_days > 365) %>% # filter to missing end date or avail_days over 365
  fselect(-avail_days) 

lt_seas_inv <- lt_seas_inv_1 %>% rbind(lt_seas_inv_2) %>% # rbind these together
  fgroup_by(ProjectID) %>% 
  fmutate(Detail = paste("Seasonal inventory record(s) that may need updated inventory dates:",
                          paste0(unique(InventoryID), collapse = ", ") ))  %>%
  fungroup %>%
  merge_check_info_dt(checkIDs = 37) %>%
  fselect(PDDEcols) %>% 
  funique()

rm(lt_seas_inv_1, lt_seas_inv_2)

# Put it all together -----------------------------------------------------

pdde_main <- rowbind(
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
  nbn_nobns,
  Active_Inventory_per_COC,
  COC_Records_per_Inventory,
  more_units_than_beds_inventory,
  vsp_clients,
  project_no_coc,
  res_no_house_type,
  lt_seas_inv
) %>%
  funique() %>%
  fmutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning")))
