
logToConsole(session, "building HMIS Participation Datasets")
## Create Data for HMIS Participation ------------------------------------------
# Inventory Level --------------------------------------------------------------
HMIS_project_active_inventories <- qDT(ProjectSegments) %>%
  fsubset(HMISParticipationType %in% c(0,1,2)) %>% # filter to projects with HMIS Participation
  join(activeInv_no_overflow %>% select(-DateCreated,-DateUpdated,-UserID,-DateDeleted), 
       ##on = "ProjectID",
       how = "inner",
       multiple = TRUE
  ) %>% 
  fsubset(ProjectType %in% project_types_w_beds) %>% # filter to ProjectType with Beds
  fsubset(ProjectType!=rrh_project_type | RRHSubType ==2) %>% # filter RRH projects to subtype 2
  # Get the Start+End dates for when each Project was Operating, HMIS Participating, and Active (Inventory)
  fmutate(
    InvHMISParticipationStart = pmax( # start of Operating & Participating 
      HMISParticipationStatusStartDate, 
      OperatingStartDate,
      na.rm = TRUE
    ),
    InvHMISParticipationEnd = pmin( # end of Operating & Participating
      HMISParticipationStatusEndDate,
      OperatingEndDate,
      na.rm = TRUE
    ),
    InvHMISActiveParticipationStart = pmax( # start of (Operating & Participating) & Active Inventory
      InvHMISParticipationStart,
      InventoryStartDate,
      na.rm = TRUE
    ),
    InvHMISActiveParticipationEnd = pmin( # end of (Operating & Participating) & Active Inventory
      InvHMISParticipationEnd,
      InventoryEndDate,
      na.rm = TRUE
    )
  ) %>% select(-InvHMISParticipationStart, - InvHMISParticipationEnd) # drop the interim step vars 

# Project-Level ----------------------------------------------------------------
HMIS_projects_w_active_inv <- HMIS_project_active_inventories %>%
  fgroup_by(ProjectID, HMISParticipationType, VictimServiceProvider, HousingType, TargetPopulation, HouseholdType, ESBedType, Availability) %>%
  fsummarise(ProjectHMISActiveParticipationStart = fmin(InvHMISActiveParticipationStart), # first active inv start with HMISPartiicpationType 
             ProjectHMISActiveParticipationEnd = fmax(InvHMISActiveParticipationEnd), # last active inv end with HMISPartiicpationType
             #TargetPopulation = list(sort(unique(TargetPopulation))), # sum?
             UnitInventory = fsum(UnitInventory),
             BedInventory = fsum(BedInventory),
             CHVetBedInventory = fsum(CHVetBedInventory),
             YouthVetBedInventory = fsum(YouthVetBedInventory),
             VetBedInventory = fsum(VetBedInventory),
             CHYouthBedInventory = fsum(CHYouthBedInventory),
             YouthBedInventory = fsum(YouthBedInventory),
             CHBedInventory = fsum(CHBedInventory)) %>% 
  fungroup()

# Estimate Dedicated Vet, Youth, and CH (Child) units based on ratio of beds----
HMIS_projects_w_active_inv <- HMIS_projects_w_active_inv %>%
  fmutate(VetUnitInventory = UnitInventory * (VetBedInventory + YouthVetBedInventory + CHVetBedInventory)/BedInventory,
          YouthUnitInventory = UnitInventory * (YouthBedInventory + YouthVetBedInventory + CHYouthBedInventory)/BedInventory,
          CHUnitInventory = UnitInventory * ( CHBedInventory + CHVetBedInventory + CHYouthBedInventory)/BedInventory
  ) %>% join(Project, how="left") # join project to get full details (name for inputpicker)


# update input project picker on Inventory & Utilization dropdown - Project LeveL tab ---- 
updatePickerInput(session = session,
                  inputId = "HMISprojects",
                  choices = sort(unique(HMIS_projects_w_active_inv$ProjectName)))

## Functions -------------------------------------------------------------------
# make function get_quarters() to get quarterly PIT dates ----------------------
# this always returns the last 4 quarterly end dates, even when they come before session$userDate$ReportStart
# for example, if the report start was january 1st 2024 and the report end is december 31st 2024
# this returns the last wednesday in january, april, july, and october of 2024
# In our calculations,
# q1 would track end of january 2024 - end of april 2024
# q2 would track end of april 2024 - end of july 2024
# q3 would track end of july 2024 - end of october 2024
# q4 would track end of october 2023 - end of january 2024
# since the current 4th quarter is not complete until end of january 2025, it displays the last Q4
# annual totals will therefore be looking at end of october 2023 - end of october 2024
# we don't consider ReportStart at all. We do sort the quarters when they are used.
# however, annual totals will look at dates a year prior to 
get_quarters <- function(){
  # get the last date in activeInventory
  lastday <- as.Date(session$userData$ReportEnd)
  y_last <- year(lastday)
  # the quarters end on the last wednedsay of january, april, july & october
  # create a function to get the exact date given a month and year
  last_wednesday <- function(year, month) {
    # Get the last day of the month
    last_day <- ceiling_date(ymd(paste(year, month, "01", sep = "-")), "month") - days(1)
    # Find the weekday of the last day (1 = Sunday, 7 = Saturday)
    weekday <- wday(last_day)
    # Calculate the difference to the last Wednesday (4 = Wednesday)
    diff <- ifelse(weekday >= 4, weekday - 4, weekday + 3)
    # Subtract the difference to get the last Wednesday
    last_day - days(diff)
  }
  
  q1_PIT <- as.Date(fifelse( last_wednesday(y_last, 1) <= lastday, # if lastday is after the current year's 1st quarter,
                             last_wednesday(y_last,1), # use last wedensday of this january
                             last_wednesday(y_last-1,1))) # else use last wednesday of last january
  q2_PIT <- as.Date(fifelse( last_wednesday(y_last, 4) <= lastday, # if lastday is after the current year's 2nd quarter,
                             last_wednesday(y_last,4), # use last wedensday of this april
                             last_wednesday(y_last-1,4))) # else use last wednesday of last april
  q3_PIT <- as.Date(fifelse( last_wednesday(y_last, 7) <= lastday, # if lastday is after the current year's 3rd quarter,
                             last_wednesday(y_last,7), # use last wedensday of this july
                             last_wednesday(y_last-1,7))) # else use last wednesday of last july
  q4_PIT <- as.Date(fifelse( last_wednesday(y_last, 10) <= lastday, # if lastday is after the current year's 4th quarter,
                             last_wednesday(y_last,10), # use last wedensday of this october
                             last_wednesday(y_last-1,10))) # else use last wednesday of last october
  
  quarters <- c(q1_PIT, q2_PIT, q3_PIT, q4_PIT) # create vector of quarterly dates
  names(quarters) <- c("Q1", "Q2", "Q3", "Q4")
  return(quarters)
}

# make function get_months() to get monthly PIT dates --------------------------
get_months <- function(){
  lastday <- as.Date(session$userData$ReportEnd)
  y_last <- year(lastday)
  m_last <- month(lastday)
  end_month = ymd(paste(y_last,m_last,"01", sep="-")) # first day of ending month
  # if last_day is the last day of the month (first day of next month minus a day)
  end_month <- as.Date(ifelse(lastday == end_month + months(1) - days(1), 
                              end_month, # the month is complete
                              end_month - months(1) # otherwise, use the previous month
  ))
  months <- seq(end_month - months(11), end_month, by = "months")
  names(months) <- month.abb[month(months)]
  return(months)
}
# counting functions count_Beds_Units() & count_Enrollments() ------------------
# create functions to count Beds & Units and Served (Enrollments) & HH_Served (HOH Enrollments)
count_Beds_Units <- function(pit_dates, extra_groups = NULL){ # use NULL so length == 0
  if(length(extra_groups)==0){
    grouping_vars <- c("PIT", "ProjectID")
  }else{
    grouping_vars <- c("PIT", "ProjectID", extra_groups) %>% unique
  }
  pit_dates <- data.frame("PIT" = pit_dates) %>% fmutate(temp=1)
  
  Bed_Unit_Count <- HMIS_project_active_inventories %>% 
    #fselect(ProjectID, BedInventory, UnitInventory, InventoryStartDate, InventoryEndDate) %>%
    fmutate(temp = 1) %>%
    join( # expand rows for each PIT date
      pit_dates, 
      on="temp", 
      multiple=T
    ) %>%
    fmutate(
      activeInv = InventoryStartDate <= as.Date(PIT) & (is.na(InventoryEndDate) | InventoryEndDate > as.Date(PIT))
    ) %>%
    fgroup_by(grouping_vars) %>%
    fsummarize(
      PIT_Beds = fsum(fifelse(activeInv,BedInventory,0)),
      PIT_Units = fsum(fifelse(activeInv,UnitInventory,0))
    ) %>% fungroup()
  #For each relevant project, count the number of beds and units for the project available for occupancy on each of the 4 PIT Dates.
  #For inventory to be considered "active" on a PIT Date it must meet the following logic: InventoryStartDate <= [PIT Date] and InventoryEndDate > [PIT Date] or NULL
  return(Bed_Unit_Count)
}
count_Enrollments <-function(pit_dates, extra_groups = NULL){
  if(length(extra_groups)==0){
    grouping_vars <- c("PIT", "ProjectID", "ProjectType")
  }else{
    grouping_vars <- c("PIT", "ProjectID", "ProjectType", extra_groups) %>% unique
  }
  pit_dates <- data.frame("PIT" = pit_dates) %>% fmutate(temp=1)
  #For each relevant project and using the EnrollmentAdjust data frame, count the number of people "served in a bed" on each of the 4 PIT Dates.
  #For an enrollment to be considered "active" on a PIT Date it must meet the following logic: EntryDate <= [PIT Date] and ExitAdjust > [PIT Date] or is NULL
  #Exclude any ES - NbN enrollments where there is no Bed Night record on [PIT Date]
  #Exclude any permanent housing enrollments where MoveInDateAdjust < [PIT Date]
  services_qPIT <- Services %>%
    fselect(EnrollmentID, DateProvided)  %>% 
    join(EnrollmentAdjust %>% fselect(EnrollmentID, ProjectID), on = "EnrollmentID", how = 'full')  %>% 
    fmutate(temp = 1) %>%
    join( # expand rows for each PIT date
      pit_dates %>% fmutate(temp=1), 
      on="temp", 
      multiple=T
    ) %>%
    fmutate(bn_PIT = as.Date(DateProvided) == as.Date(PIT)) %>%  
    fmutate(bn_PIT = fifelse(is.na(bn_PIT),FALSE,bn_PIT)) %>%  
    fgroup_by(EnrollmentID, PIT) %>% # For each enrollment & PIT Date,
    fsummarise( # flag if Enrollment has any Service records where DateProvided == PIT
      has_bn_PIT = any(bn_PIT, na.rm=TRUE)
    )  %>% fungroup()
  Bed_Unit_Util <- EnrollmentAdjust %>%
    join(services_qPIT, on = "EnrollmentID", how = "left", multiple = T) %>%
    fmutate(# Enrollment Active
      activeEnroll = EntryDate <= as.Date(PIT) & (is.na(ExitAdjust) | ExitAdjust > as.Date(PIT)),
      eligProjPerm = !(ProjectType %in% c(3,9,10,13)) | fifelse(is.na(MoveInDateAdjust), 
                                                                FALSE, # if MoveInDateAdjust is missing, use FALSE to count zero days
                                                                MoveInDateAdjust >= as.Date(PIT))
    ) %>% 
    fmutate(Served = fifelse(activeEnroll & eligProjPerm, 1, 0), # flag active & eligible enrollments
            HHServed = fifelse(activeEnroll & eligProjPerm & RelationshipToHoH==1, # count households by just flagging active/elig enrollments that are head of household
                               1, 0)) %>%
    fgroup_by(grouping_vars) %>% 
    fsummarise(
      eligProjNBN = any(has_bn_PIT),
      PIT_Served = fsum(Served),
      PIT_HHServed = fsum(HHServed))%>%
    fungroup()  %>% fsubset(!is.na(PIT_Served) & (ProjectType != es_nbn_project_type |eligProjNBN))
  
  return(Bed_Unit_Util)
} 

## Build Project Level data ----------------------------
logToConsole(session, "building project-level utilization")
# sort PIT dates
quarters <- get_quarters() %>% sort
mons <- get_months() %>% sort
# full join the results of passing through counting functions
project_level_util_q <- count_Beds_Units(quarters) %>%
  join(count_Enrollments(quarters), how = "left") %>%
  fmutate(PIT_Bed_Utilization = paste(round(PIT_Served / PIT_Beds *100, digits = 1), "%"),
          PIT_Unit_Utilization = paste(round(PIT_HHServed / PIT_Units*100, digits = 1), "%"))

project_level_util_m <- count_Beds_Units(mons) %>%
  join(count_Enrollments(mons), how = "left") %>%
  fmutate(PIT_Bed_Utilization = paste(round(PIT_Served / PIT_Beds *100, digits = 1), "%"),
          PIT_Unit_Utilization = paste(round(PIT_HHServed / PIT_Units*100, digits = 1), "%"))

# pass results to session for server_09_inv_util.R to finish 
session$userData$project_level_util_q <- project_level_util_q 
session$userData$project_level_util_m <- project_level_util_m 
session$userData$HMIS_project_active_inventories <- HMIS_project_active_inventories 
session$userData$EnrollmentAdjust <- EnrollmentAdjust


