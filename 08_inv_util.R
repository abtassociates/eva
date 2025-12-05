
logToConsole(session, "quarterly bed unit inventory")

# make function to get quarterly PIT dates
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
get_months <- function(){
  lastday <- as.Date(session$userData$ReportEnd)
  y_last <- year(lastday)
  m_last <- month(lastday)
  end_month = ymd(paste(y_last,m_last,"01", sep="-")) # first day of ending month
  # if last_day is the last day of the month (first day of next month minus a day)
  end_month <- as.Date(ifelse(last_day == end_month + months(1) - days(1), 
                              end_month, # the month is complete
                              end_month - months(1) # otherwise, use the previous month
  ))
  months <- seq(end_month - months(11), end_month, by = "months")
  
  names(months) <- month.abb[month(months)]
  return(months)
}  

# counting functions -----------------------------------------------
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

# create versions that sum counts over a range of dates
# use difference in dates to calculate the 'report length' and calculate nightly averages
count_Beds_Units_rng <- function(range_start,range_end, extra_groups = NULL){
  if(length(extra_groups)==0){
    grouping_vars <- c( "ProjectID")
  }else{
    grouping_vars <- c("ProjectID", extra_groups) %>% unique
  }
  
  activeDays <- function(inv_start, inv_end){
    overlap_start <- max(as.Date(inv_start), range_start, na.rm=TRUE)
    overlap_end <- min(as.Date(inv_end), range_end, na.rm=TRUE)
    if(overlap_start>overlap_end){
      return(0)
    }else{
      return(as.numeric(overlap_end-overlap_start+1))
    }
  }
  
  Bed_Unit_Count <- HMIS_project_active_inventories %>% 
    #fselect(ProjectID, BedInventory, UnitInventory, InventoryStartDate, InventoryEndDate) %>%
    fmutate(
      active_days = activeDays(InventoryStartDate, InventoryEndDate)
    ) %>%
    fgroup_by(grouping_vars) %>%
    fsummarize(
      Total_Beds = fsum(active_days*BedInventory),
      Total_Units = fsum(active_days*UnitInventory)
    ) %>% fungroup()
  
  # calculate length of range 
  report_length <- as.numeric(range_end - range_start)
  Bed_Unit_Count <- Bed_Unit_Count %>% # use it to calculate average nightly beds/units
    fmutate(Avg_Nightly_Beds = Total_Beds / report_length ,
            Avg_Nightly_Units = Total_Units / report_length )
  
  return(Bed_Unit_Count)
}
count_Enrollments_rng <-function(range_start,range_end, extra_groups = NULL){
  if(length(extra_groups)==0){
    grouping_vars <- c( "ProjectID", "ProjectType")
  }else{
    grouping_vars <- c("ProjectID", "ProjectType", extra_groups) %>% unique
  }
  browser()
  activeDays <- function(inv_start, inv_end){
    overlap_start <- max(as.Date(inv_start), range_start, na.rm=TRUE)
    overlap_end <- min(as.Date(inv_end), range_end, na.rm=TRUE)
    if(overlap_start>overlap_end){
      return(0)
    }else{
      return(as.numeric(overlap_end-overlap_start+1))
    }
  }
  #For each relevant project and using the EnrollmentAdjust data frame, count the number of people "served in a bed" on each of the 4 PIT Dates.
  #For an enrollment to be considered "active" on a PIT Date it must meet the following logic: EntryDate <= [PIT Date] and ExitAdjust > [PIT Date] or is NULL
  #Exclude any ES - NbN enrollments where there is no Bed Night record on [PIT Date]
  #Exclude any permanent housing enrollments where MoveInDateAdjust < [PIT Date]
  services_qPIT <- Services %>%
    fselect(EnrollmentID, DateProvided)  %>% 
    fmutate(bn_rng = fifelse(as.Date(DateProvided) > range_start & as.Date(DateProvided) <= range_end, 1, 0)) # 1 or 0 day in range
  
  Bed_Unit_Util <- EnrollmentAdjust %>%
    join(services_qPIT, on = "EnrollmentID", how = "left") %>%
    fsubset(!(ProjectType %in% c(3,9,10,13)) | !is.na(MoveInDateAdjust)) %>% # drop perm housing project enrollments if move in date missing
    fsubset(!(ProjectType %in% c(3,9,10,13)) | MoveInDateAdjust >= DateProvided) # drop perm housing project enrollments if move in date after DateProvided
  
  Bed_Unit_Util$active_days <- mapply(activeDays, inv_start = Bed_Unit_Util$EntryDate, inv_end = Bed_Unit_Util$ExitAdjust) # count days of enrollment overlapping with days of range
  
  Bed_Unit_Util <- Bed_Unit_Util %>% fmutate(# Enrollment Active
      enrollDays = fifelse(!is.na(bn_rng), bn_rng, active_days)) %>% # if row is in services, use bn_rng to count days, otherwise, use active_days
    fmutate(HHServed = fifelse(RelationshipToHoH==1, # count households by just flagging active/elig enrollments that are head of household
                               enrollDays, 0)) %>%
    fgroup_by(grouping_vars) %>% 
    fsummarise(
      eligProjNBN = any(enrollDays>0),
      Total_Served = fsum(enrollDays),
      Total_HHServed = fsum(HHServed))%>%
    fungroup()  %>% fsubset(!is.na(Total_Served) & (ProjectType != es_nbn_project_type |eligProjNBN))
  
  # calculate length of range 
  report_length <- as.numeric(range_end - range_start )
  Bed_Unit_Util <- Bed_Unit_Util %>% # use it to calculate average served beds/units
    fmutate(Avg_Nightly_Served = Total_Served / report_length ,
            Avg_Nightly_HHServed = Total_HHServed / report_length ) %>%
    select(-eligProjNBN, -ProjectType)
  
  return(Bed_Unit_Util)
} 

nightly_avg <- function(period, labels ){
  
  for (q in 1:length(period)){
    
    if(q!=length(period)){ # IF NOT LAST
      nightly_avg_q <- count_Beds_Units_rng(period[q], period[q+1]) %>%
        join(count_Enrollments_rng(period[q], period[q+1]), how = "left") %>%
        fmutate(PIT = period[q],
                label = labels[q])
      
    }else{ # IF LAST, use a year from first quarter minus a day (so full range is 365)
      nightly_avg_q <- count_Beds_Units_rng(period[q], period[1] + years(1) - days(1)) %>%
        join(count_Enrollments_rng(period[q], period[1] + years(1) - days(1)), how = "left") %>%
        fmutate(PIT = period[q],
                label = labels[q])
    }
    if(q==1){
      nightly_avg <- nightly_avg_q
    }else{
      nightly_avg <- nightly_avg %>% rowbind(nightly_avg_q)
    }
    rm(nightly_avg_q) # delete quarter
  }
  
  nightly_avg_ann <- nightly_avg %>% fgroup_by(ProjectID) %>%
    fsummarise(Total_Beds = fsum(Total_Beds),
               Total_Units = fsum(Total_Units),
               Total_Served = fsum(Total_Served),
               Total_HHServed = fsum(Total_HHServed)) %>% 
    fungroup %>%
    fmutate(
      PIT = as.Date(NA),
      label = "Annual",
      Avg_Nightly_Beds = Total_Beds / 365,
      Avg_Nightly_Units = Total_Units / 365,
      Avg_Nightly_Served = Total_Served / 365,
      Avg_Nightly_HHServed = Total_HHServed / 365
    )
  
  nightly_avg <- nightly_avg %>% rowbind(nightly_avg_ann) %>%
    fmutate(Avg_Nightly_Bed_Util = Avg_Nightly_Served / Avg_Nightly_Beds,
            Avg_Nightly_Unit_Util = Avg_Nightly_HHServed / Avg_Nightly_Units)
  return(nightly_avg)
}
quarters <- get_quarters() %>% sort

# full join the results of passing quarters through counting functions
# Counts on PIT Dates
project_level_util_q <- count_Beds_Units(quarters) %>%
  join(count_Enrollments(quarters), how = "left")

# Avg over Quarters 
nightly_avg <- nightly_avg(period = quarters, labels = names(quarters))

project_level_util_q <- project_level_util_q %>% join(nightly_avg, how = "full") 
rm(nightly_avg)

# calculate project level quarterly utilization
project_level_util_q <- project_level_util_q %>%
  fmutate(Bed_Utilization = PIT_Served / PIT_Beds,
          Unit_Utilization = PIT_HHServed / PIT_Units)

# pass results to session for server_09_inv_util.R
session$userData$project_level_util_q <- project_level_util_q 
