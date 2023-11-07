# run 00-Start_here.R EXCEPT for the "Data Prep" section

# sample data has no projects that are just comparable db particip --------

Project <- Project %>%
  add_row(
    "ProjectID" = "1800",
    "OrganizationID" = "80",
    "ProjectName" = "A Safe Place - ES DV",
    "ProjectCommonName" = NA,
    "OperatingStartDate" = ymd("20001001"),
    "OperatingEndDate" = NA,
    "ContinuumProject" = 1,
    "ProjectType" = 0,
    "HousingType" = 1,
    "RRHSubType" = NA,
    "ResidentialAffiliation" = NA,
    "TargetPopulation" = 1,
    "HOPWAMedAssistedLivingFac" = NA,
    "PITCount" = NA,
    "DateCreated" = ymd_hms("20001001 06:00:00"),
    "DateUpdated" = ymd_hms("20001001 06:00:00"),
    "UserID" = "4566",
    "DateDeleted" = NA,
    "ExportID" = Export$ExportID
  )

HMISParticipation <- HMISParticipation %>%
  add_row(
   "HMISParticipationID" = "207",
   "ProjectID" = "1800",
   "HMISParticipationType" = 2,
   "HMISParticipationStatusStartDate" = ymd("20001001"),
   "HMISParticipationStatusEndDate" = NA,
   "DateCreated" = ymd_hms("20001001 06:00:00"),
   "DateUpdated" = ymd_hms("20001001 06:00:00"),
   "UserID" = "4566",
   "DateDeleted" = NA
  )

Inventory <- Inventory %>%
  add_row(
    "InventoryID" = "4630", 
    "ProjectID" = "1800",            
    "CoCCode" = "XX-501",              
    "HouseholdType" = 3,       
    "Availability" = 1,  
    "UnitInventory" = 6,        
    "BedInventory" = 15,         
    "CHVetBedInventory" = 0,   
    "YouthVetBedInventory" = 0, 
    "VetBedInventory" = 0,      
    "CHYouthBedInventory" = 0,  
    "YouthBedInventory" = 0,   
    "CHBedInventory" = 0,  
    "OtherBedInventory" = 15,    
    "ESBedType" = 1,            
    "InventoryStartDate" = ymd("20001001"),  
    "InventoryEndDate" = NA,  
    "DateCreated" = ymd_hms("20001001 06:00:00"),          
    "DateUpdated" = ymd_hms("20001001 06:00:00"),          
    "UserID" = "4566",              
    "DateDeleted" = NA, 
    "ExportID" = Export$ExportID
  )

write.csv(
  Project,
  here(paste0(directory, "data/Enrollment.csv")),
  na = "",
  row.names = FALSE
)

write.csv(
  HMISParticipation,
  here(paste0(directory, "data/Enrollment.csv")),
  na = "",
  row.names = FALSE
)

write.csv(
  Inventory,
  here(paste0(directory, "data/Enrollment.csv")),
  na = "",
  row.names = FALSE
)

# remove enrollments from one of the dv projects --------------------------

# 2 projects in the sample data are represented twice in the HMISParticipation
# file because they switched from comparable db (ParticipationType = 2) to
# HMIS participating (ParticiationType = 1) These are projects 488 and 773.
# Both of them have enrollments in the enrollment file. I want to add nuance
# so that one of them does not have any enrollments.

Enrollment <- Enrollment %>%
  filter(ProjectID != 488)

write.csv(
  Enrollment,
  here(paste0(directory, "data/Enrollment.csv")),
  na = "",
  row.names = FALSE
)

# once you've run all this, you should be able to test two things:

# 1. if 488 is not flagged, that means the logic is not recognizing that
# that project was participating during the reporting period and thus should
# have some enrollments (even if they were previously or subsequently 
# participating in a comparable database.) so we want 488 to flag.

# 2. if 1800 flags (like it would in Eva live) that means Eva is incorrectly
# ignoring the fact that that project is not HMIS Participating and thus
# shouldn't have any enrollments. We do NOT want 1800 to flag.

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

dv_projects <- HMISParticipation %>%
  group_by(ProjectID) %>%
  slice_min(HMISParticipationType) %>%
  ungroup() %>%
  filter(HMISParticipationType == 2) %>%
  pull(ProjectID)

dv_projects %in% c(projects_w_beds)

dv_projects %in% c(projects_w_clients)

res_projects_no_clients <- setdiff(projects_w_beds, projects_w_clients)
