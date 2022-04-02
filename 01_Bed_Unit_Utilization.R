# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)
library(scales)
library(HMIS)

if (!exists("Enrollment")) load("images/COHHIOHMIS.RData")
if (!exists("tay")) {
  load("images/cohorts.RData")
  # rlang::env_binding_lock(environment(), ls())
}


# despite the fact we're pulling in usually more than 2 years of data, the 
# utilization reporting will only go back 2 years. (decision based on lack of
# a need to go back further and time to code all that.)
FileEnd <- format.Date(calc_2_yrs_prior_end, "%m-%d-%Y")
FileStart <- format.Date(calc_2_yrs_prior_start, "%m-%d-%Y")
FilePeriod <- calc_2_yrs_prior_range

# Creating Beds table -----------------------------------------------------

small_project <- Project %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         HMISParticipatingProject) %>%
  filter(ProjectType %in% c(project_types_w_beds) &
           operating_between(Project, FileStart, FileEnd) &
           is.na(Project$GrantType) &
           HMISParticipatingProject == 1)

small_inventory <- Inventory %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate
    )  %>%
  filter((
    ymd(InventoryStartDate) <= mdy(FileEnd) &
      (
        ymd(InventoryEndDate) >= mdy(FileStart) |
          is.na(InventoryEndDate)
      )
  ) &
    Inventory$CoCCode %in% c("OH-507", "OH-504"))

Beds <- inner_join(small_project, small_inventory, by = "ProjectID")

# Creating Utilizers table ------------------------------------------------

small_enrollment <- Enrollment %>% 
  select(PersonalID,
         EnrollmentID,
         ProjectID,
         EntryDate,
         EntryAdjust,
         MoveInDateAdjust,
         ExitDate,
         ExitAdjust,
         HouseholdID,
         RelationshipToHoH,
         MoveInDate) %>%
  filter(served_between(., FileStart, FileEnd))

Utilizers <- semi_join(small_enrollment, Beds, by = "ProjectID") 

Utilizers <- left_join(Utilizers, small_project, by = "ProjectID") %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectName,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    EntryAdjust,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust
  )

# Client Utilization of Beds ----------------------------------------------

# filtering out any PSH or RRH records without a proper Move-In Date plus the 
# fake training providers
utilizers_clients <- Utilizers %>%
  mutate(StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))) %>%
  filter(
    int_overlaps(StayWindow, FilePeriod) &
      (
    (
      ProjectType %in% c(3, 9) &
        !is.na(EntryAdjust) &
        ymd(MoveInDateAdjust) >= ymd(EntryDate) &
        ymd(MoveInDateAdjust) < ymd(ExitAdjust)
    ) |
      ProjectType %in% c(1, 2, 8)
  ) &
    !ProjectID %in% c(1775, 1695, fake_projects))

# filtering Beds object to exclude any providers that served 0 hhs in date range

Beds <- Beds %>%
  right_join(utilizers_clients %>%
               select(ProjectID) %>%
               unique(), by = "ProjectID")

# function for adding bed nights per ee

bed_nights_per_ee <- function(table, interval) {
  # if the ee date range and a given interval (in my reporting, a month) overlap,
  if_else(int_overlaps(table$StayWindow, interval),
          # then return the difference between
          as.numeric(difftime(
            # if the exit date precedes the end of the interval, then the exit 
            # date, otherwise the end of the interval 
            if_else(
              ymd(table$ExitAdjust) <=  int_end(interval),
              as.POSIXct(table$ExitAdjust),
              int_end(interval) + days(1)
            ),
            # if the entry date is after the start of the interval, then the 
            # entry date, otherwise the beginning of the interval
            if_else(
              ymd(table$EntryAdjust) >= int_start(interval),
              as.POSIXct(table$EntryAdjust),
              int_start(interval)
            ),
            # give it to me in days
            units = "days"
          )), NULL
  )
}

nth_Month <- function(n) {
  interval(floor_date(mdy(FileStart) %m+% months(n - 1), unit = "months"),
           seq(as.Date(floor_date(mdy(FileStart) %m+% months(n), unit = "months")),
               length = 1, by = "1 month") - 1)
}

FirstMonth <- nth_Month(1)
SecondMonth <- nth_Month(2)
ThirdMonth <- nth_Month(3)
FourthMonth <- nth_Month(4)
FifthMonth <- nth_Month(5)
SixthMonth <- nth_Month(6)
SeventhMonth <- nth_Month(7)
EighthMonth <- nth_Month(8)
NinthMonth <- nth_Month(9)
TenthMonth <- nth_Month(10)
EleventhMonth <- nth_Month(11)
TwelfthMonth <- nth_Month(12)
ThirteenthMonth <- nth_Month(13)
FourteenthMonth <- nth_Month(14)
FifteenthMonth <- nth_Month(15)
SixteenthMonth <- nth_Month(16)
SeventeenthMonth <- nth_Month(17)
EighteenthMonth <- nth_Month(18)
NineteenthMonth <- nth_Month(19)
TwentiethMonth <- nth_Month(20)
TwentyfirstMonth <- nth_Month(21)
TwentysecondMonth <- nth_Month(22)
TwentythirdMonth <- nth_Month(23)
TwentyfourthMonth <- nth_Month(24)
# adding in month columns with utilization numbers

utilizers_clients <- utilizers_clients %>%
  mutate(
    # FilePeriod = bed_nights_per_ee(utilizers_clients, FilePeriod),
    Month1 = bed_nights_per_ee(utilizers_clients, FirstMonth),
    Month2 = bed_nights_per_ee(utilizers_clients, SecondMonth),
    Month3 = bed_nights_per_ee(utilizers_clients, ThirdMonth),
    Month4 = bed_nights_per_ee(utilizers_clients, FourthMonth),
    Month5 = bed_nights_per_ee(utilizers_clients, FifthMonth),
    Month6 = bed_nights_per_ee(utilizers_clients, SixthMonth),
    Month7 = bed_nights_per_ee(utilizers_clients, SeventhMonth),
    Month8 = bed_nights_per_ee(utilizers_clients, EighthMonth),
    Month9 = bed_nights_per_ee(utilizers_clients, NinthMonth),
    Month10 = bed_nights_per_ee(utilizers_clients, TenthMonth),
    Month11 = bed_nights_per_ee(utilizers_clients, EleventhMonth),
    Month12 = bed_nights_per_ee(utilizers_clients, TwelfthMonth),
    Month13 = bed_nights_per_ee(utilizers_clients, ThirteenthMonth),
    Month14 = bed_nights_per_ee(utilizers_clients, FourteenthMonth),
    Month15 = bed_nights_per_ee(utilizers_clients, FifteenthMonth),
    Month16 = bed_nights_per_ee(utilizers_clients, SixteenthMonth),
    Month17 = bed_nights_per_ee(utilizers_clients, SeventeenthMonth),
    Month18 = bed_nights_per_ee(utilizers_clients, EighteenthMonth),
    Month19 = bed_nights_per_ee(utilizers_clients, NineteenthMonth),
    Month20 = bed_nights_per_ee(utilizers_clients, TwentiethMonth),
    Month21 = bed_nights_per_ee(utilizers_clients, TwentyfirstMonth),
    Month22 = bed_nights_per_ee(utilizers_clients, TwentysecondMonth),
    Month23 = bed_nights_per_ee(utilizers_clients, TwentythirdMonth),
    Month24 = bed_nights_per_ee(utilizers_clients, TwentyfourthMonth)
  ) %>%
  mutate(
    across(starts_with("Month"), ~if_else(is.na(.x), 0, .x))
  ) %>%
  select(ProjectName, ProjectID, ProjectType, PersonalID, EnrollmentID, 
         EntryDate, MoveInDateAdjust, ExitDate, starts_with("Month"))

utilizers_clients <- as.data.frame(utilizers_clients)

# making granularity by provider instead of by enrollment id
BedNights <- utilizers_clients %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(
    # BNY = sum(FilePeriod, na.rm = TRUE),
    BN1 = sum(Month1, na.rm = TRUE),
    BN2 = sum(Month2, na.rm = TRUE),
    BN3 = sum(Month3, na.rm = TRUE),
    BN4 = sum(Month4, na.rm = TRUE),
    BN5 = sum(Month5, na.rm = TRUE),
    BN6 = sum(Month6, na.rm = TRUE),
    BN7 = sum(Month7, na.rm = TRUE),
    BN8 = sum(Month8, na.rm = TRUE),
    BN9 = sum(Month9, na.rm = TRUE),
    BN10 = sum(Month10, na.rm = TRUE),
    BN11 = sum(Month11, na.rm = TRUE),
    BN12 = sum(Month12, na.rm = TRUE),
    BN13 = sum(Month13, na.rm = TRUE),
    BN14 = sum(Month14, na.rm = TRUE),
    BN15 = sum(Month15, na.rm = TRUE),
    BN16 = sum(Month16, na.rm = TRUE),
    BN17 = sum(Month17, na.rm = TRUE),
    BN18 = sum(Month18, na.rm = TRUE),
    BN19 = sum(Month19, na.rm = TRUE),
    BN20 = sum(Month20, na.rm = TRUE),
    BN21 = sum(Month21, na.rm = TRUE),
    BN22 = sum(Month22, na.rm = TRUE),
    BN23 = sum(Month23, na.rm = TRUE),
    BN24 = sum(Month24, na.rm = TRUE)
  ) %>%
  ungroup()

# Bed Capacity ------------------------------------------------------------

BedCapacity <- Beds %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(InventoryEndAdjust = if_else(is.na(InventoryEndDate),
                                      mdy(FileEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(FileStart),
                                        ymd(InventoryStartDate),
                                        mdy(FileStart)),
         AvailableWindow = interval(ymd(InventoryStartAdjust),
                                    ymd(InventoryEndAdjust))) 

# function for bed capacity at the bed record level

bed_capacity <- function(interval) {
  if_else(int_overlaps(BedCapacity$AvailableWindow, interval),
          (as.numeric(difftime(
            if_else(
              ymd(BedCapacity$InventoryEndAdjust) <=  int_end(interval),
              as.POSIXct(BedCapacity$InventoryEndAdjust),
              int_end(interval)
            ),
            if_else(
              ymd(BedCapacity$InventoryStartAdjust) >= int_start(interval),
              as.POSIXct(BedCapacity$InventoryStartAdjust),
              int_start(interval)
            ),
            units = "days"
          ))+1) * BedCapacity$BedInventory, NULL
  )
}

BedCapacity <- BedCapacity %>%
  mutate(
    # PE_DateRange = bed_capacity(PE_FilePeriod),
    # FilePeriod = bed_capacity(FilePeriod),
    Month1 = bed_capacity(FirstMonth),
    Month2 = bed_capacity(SecondMonth),
    Month3 = bed_capacity(ThirdMonth),
    Month4 = bed_capacity(FourthMonth),
    Month5 = bed_capacity(FifthMonth),
    Month6 = bed_capacity(SixthMonth),
    Month7 = bed_capacity(SeventhMonth),
    Month8 = bed_capacity(EighthMonth),
    Month9 = bed_capacity(NinthMonth),
    Month10 = bed_capacity(TenthMonth),
    Month11 = bed_capacity(EleventhMonth),
    Month12 = bed_capacity(TwelfthMonth),
    Month13 = bed_capacity(ThirteenthMonth),
    Month14 = bed_capacity(FourteenthMonth),
    Month15 = bed_capacity(FifteenthMonth),
    Month16 = bed_capacity(SixteenthMonth),
    Month17 = bed_capacity(SeventeenthMonth),
    Month18 = bed_capacity(EighteenthMonth),
    Month19 = bed_capacity(NineteenthMonth),
    Month20 = bed_capacity(TwentiethMonth),
    Month21 = bed_capacity(TwentyfirstMonth),
    Month22 = bed_capacity(TwentysecondMonth),
    Month23 = bed_capacity(TwentythirdMonth),
    Month24 = bed_capacity(TwentyfourthMonth)
  ) %>%
  select(
    -InventoryStartDate,
    -InventoryEndDate,
    -InventoryEndAdjust,-BedInventory,
    -InventoryStartAdjust,
    -AvailableWindow
  )

BedCapacity <- BedCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(
    # BCPE = sum(PE_DateRange, na.rm = TRUE),
    # BCY = sum(FilePeriod, na.rm = TRUE),
    BC1 = sum(Month1, na.rm = TRUE),
    BC2 = sum(Month2, na.rm = TRUE),
    BC3 = sum(Month3, na.rm = TRUE),
    BC4 = sum(Month4, na.rm = TRUE),
    BC5 = sum(Month5, na.rm = TRUE),
    BC6 = sum(Month6, na.rm = TRUE),
    BC7 = sum(Month7, na.rm = TRUE),
    BC8 = sum(Month8, na.rm = TRUE),
    BC9 = sum(Month9, na.rm = TRUE),
    BC10 = sum(Month10, na.rm = TRUE),
    BC11 = sum(Month11, na.rm = TRUE),
    BC12 = sum(Month12, na.rm = TRUE),
    BC13 = sum(Month13, na.rm = TRUE),
    BC14 = sum(Month14, na.rm = TRUE),
    BC15 = sum(Month15, na.rm = TRUE),
    BC16 = sum(Month16, na.rm = TRUE),
    BC17 = sum(Month17, na.rm = TRUE),
    BC18 = sum(Month18, na.rm = TRUE),
    BC19 = sum(Month19, na.rm = TRUE),
    BC20 = sum(Month20, na.rm = TRUE),
    BC21 = sum(Month21, na.rm = TRUE),
    BC22 = sum(Month22, na.rm = TRUE),
    BC23 = sum(Month23, na.rm = TRUE),
    BC24 = sum(Month24, na.rm = TRUE)
  ) %>%
  ungroup()

# Bed Utilization ---------------------------------------------------------

utilization_bed <- 
  left_join(BedCapacity,
            BedNights,
            by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(
    # FilePeriod = BNY / BCY, accuracy = .1,
    Month1 = BN1 / BC1, accuracy = .1,
    Month2 = BN2 / BC2, accuracy = .1,
    Month3 = BN3 / BC3, accuracy = .1,
    Month4 = BN4 / BC4, accuracy = .1,
    Month5 = BN5 / BC5, accuracy = .1,
    Month6 = BN6 / BC6, accuracy = .1,
    Month7 = BN7 / BC7, accuracy = .1,
    Month8 = BN8 / BC8, accuracy = .1,
    Month9 = BN9 / BC9, accuracy = .1,
    Month10 = BN10 / BC10, accuracy = .1,
    Month11 = BN11 / BC11, accuracy = .1,
    Month12 = BN12 / BC12, accuracy = .1,
    Month13 = BN13 / BC13, accuracy = .1,
    Month14 = BN14 / BC14, accuracy = .1,
    Month15 = BN15 / BC15, accuracy = .1,
    Month16 = BN16 / BC16, accuracy = .1,
    Month17 = BN17 / BC17, accuracy = .1,
    Month18 = BN18 / BC18, accuracy = .1,
    Month19 = BN19 / BC19, accuracy = .1,
    Month20 = BN20 / BC20, accuracy = .1,
    Month21 = BN21 / BC21, accuracy = .1,
    Month22 = BN22 / BC22, accuracy = .1,
    Month23 = BN23 / BC23, accuracy = .1,
    Month24 = BN24 / BC24, accuracy = .1
  ) %>% 
  select(ProjectID, ProjectName, ProjectType, starts_with("Month")) %>%
  ungroup()

rm(BedCapacity, BedNights) 

names(utilization_bed) <-
  c(
    "ProjectID",
    "ProjectName",
    "ProjectType",
    format.Date(int_start(FirstMonth), "%m%d%Y"),
    format.Date(int_start(SecondMonth), "%m%d%Y"),
    format.Date(int_start(ThirdMonth), "%m%d%Y"),
    format.Date(int_start(FourthMonth), "%m%d%Y"),
    format.Date(int_start(FifthMonth), "%m%d%Y"),
    format.Date(int_start(SixthMonth), "%m%d%Y"),
    format.Date(int_start(SeventhMonth), "%m%d%Y"),
    format.Date(int_start(EighthMonth), "%m%d%Y"),
    format.Date(int_start(NinthMonth), "%m%d%Y"),
    format.Date(int_start(TenthMonth), "%m%d%Y"),
    format.Date(int_start(EleventhMonth), "%m%d%Y"),
    format.Date(int_start(TwelfthMonth), "%m%d%Y"),
    format.Date(int_start(ThirteenthMonth), "%m%d%Y"),
    format.Date(int_start(FourteenthMonth), "%m%d%Y"),
    format.Date(int_start(FifteenthMonth), "%m%d%Y"),
    format.Date(int_start(SixteenthMonth), "%m%d%Y"),
    format.Date(int_start(SeventeenthMonth), "%m%d%Y"),
    format.Date(int_start(EighteenthMonth), "%m%d%Y"),
    format.Date(int_start(NineteenthMonth), "%m%d%Y"),
    format.Date(int_start(TwentiethMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfirstMonth), "%m%d%Y"),
    format.Date(int_start(TwentysecondMonth), "%m%d%Y"),
    format.Date(int_start(TwentythirdMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfourthMonth), "%m%d%Y")
  )

# Inf means there were no beds but there were clients served.
# %NaN means there were no beds and no clients served that month.

# HH Utilization of Units -------------------------------------------------

HHUtilizers <- Utilizers %>%
  mutate(
    EntryAdjust = case_when(
      ProjectType %in% c(lh_project_types) ~ EntryDate,
      ProjectType %in% c(3, 9) ~ MoveInDateAdjust
    ),
    ExitAdjust = if_else(
      is.na(ExitDate) & ymd(EntryAdjust) <= mdy(FileEnd),
      mdy(FileEnd),
      ymd(ExitDate)
    ),
    StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
  ) %>%
  filter(
    str_detect(HouseholdID, fixed("s_")) |
      (str_detect(HouseholdID, fixed("h_")) &
         RelationshipToHoH == 1) &
      int_overlaps(StayWindow, FilePeriod) &
      (
        (
          ProjectType %in% c(3, 9) &
            !is.na(EntryAdjust) &
            ymd(MoveInDateAdjust) >= ymd(EntryDate) &
            ymd(MoveInDateAdjust) <= ymd(ExitAdjust)
        ) |
          ProjectType %in% c(lh_project_types)
      ) &
      !ProjectID %in% c(1775, 1695, fake_projects)
  ) %>%
  select(-EntryDate,-MoveInDateAdjust,-HouseholdID,-RelationshipToHoH)

HHUtilizers <- HHUtilizers %>%
  mutate(
    # FilePeriod = bed_nights_per_ee(HHUtilizers, FilePeriod),
    Month1 = bed_nights_per_ee(HHUtilizers, FirstMonth),
    Month2 = bed_nights_per_ee(HHUtilizers, SecondMonth),
    Month3 = bed_nights_per_ee(HHUtilizers, ThirdMonth),
    Month4 = bed_nights_per_ee(HHUtilizers, FourthMonth),
    Month5 = bed_nights_per_ee(HHUtilizers, FifthMonth),
    Month6 = bed_nights_per_ee(HHUtilizers, SixthMonth),
    Month7 = bed_nights_per_ee(HHUtilizers, SeventhMonth),
    Month8 = bed_nights_per_ee(HHUtilizers, EighthMonth),
    Month9 = bed_nights_per_ee(HHUtilizers, NinthMonth),
    Month10 = bed_nights_per_ee(HHUtilizers, TenthMonth),
    Month11 = bed_nights_per_ee(HHUtilizers, EleventhMonth),
    Month12 = bed_nights_per_ee(HHUtilizers, TwelfthMonth),
    Month13 = bed_nights_per_ee(HHUtilizers, ThirteenthMonth),
    Month14 = bed_nights_per_ee(HHUtilizers, FourteenthMonth),
    Month15 = bed_nights_per_ee(HHUtilizers, FifteenthMonth),
    Month16 = bed_nights_per_ee(HHUtilizers, SixteenthMonth),
    Month17 = bed_nights_per_ee(HHUtilizers, SeventeenthMonth),
    Month18 = bed_nights_per_ee(HHUtilizers, EighteenthMonth),
    Month19 = bed_nights_per_ee(HHUtilizers, NineteenthMonth),
    Month20 = bed_nights_per_ee(HHUtilizers, TwentiethMonth),
    Month21 = bed_nights_per_ee(HHUtilizers, TwentyfirstMonth),
    Month22 = bed_nights_per_ee(HHUtilizers, TwentysecondMonth),
    Month23 = bed_nights_per_ee(HHUtilizers, TwentythirdMonth),
    Month24 = bed_nights_per_ee(HHUtilizers, TwentyfourthMonth)
    ) %>%
  mutate(
    across(starts_with("Month"), ~if_else(is.na(.x), 0, .x))
  )
  
HHUtilizers <- as.data.frame(HHUtilizers)

# making granularity by provider instead of by enrollment id
HHNights <- HHUtilizers %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(
    # HNY = sum(FilePeriod, na.rm = TRUE),
    HN1 = sum(Month1, na.rm = TRUE),
    HN2 = sum(Month2, na.rm = TRUE),
    HN3 = sum(Month3, na.rm = TRUE),
    HN4 = sum(Month4, na.rm = TRUE),
    HN5 = sum(Month5, na.rm = TRUE),
    HN6 = sum(Month6, na.rm = TRUE),
    HN7 = sum(Month7, na.rm = TRUE),
    HN8 = sum(Month8, na.rm = TRUE),
    HN9 = sum(Month9, na.rm = TRUE),
    HN10 = sum(Month10, na.rm = TRUE),
    HN11 = sum(Month11, na.rm = TRUE),
    HN12 = sum(Month12, na.rm = TRUE),
    HN13 = sum(Month13, na.rm = TRUE),
    HN14 = sum(Month14, na.rm = TRUE),
    HN15 = sum(Month15, na.rm = TRUE),
    HN16 = sum(Month16, na.rm = TRUE),
    HN17 = sum(Month17, na.rm = TRUE),
    HN18 = sum(Month18, na.rm = TRUE),
    HN19 = sum(Month19, na.rm = TRUE),
    HN20 = sum(Month20, na.rm = TRUE),
    HN21 = sum(Month21, na.rm = TRUE),
    HN22 = sum(Month22, na.rm = TRUE),
    HN23 = sum(Month23, na.rm = TRUE),
    HN24 = sum(Month24, na.rm = TRUE)
  ) %>%
  ungroup()
    
# leaving this one ^^ because the client-level 
# detail should be good enough for R minor elevated

rm(HHUtilizers) 


# Unit Capacity -----------------------------------------------------------

UnitCapacity <- Beds %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(InventoryEndAdjust = if_else(is.na(InventoryEndDate),
                                      mdy(FileEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(FileStart),
                                        ymd(InventoryStartDate),
                                        mdy(FileStart)),
         AvailableWindow = interval(ymd(InventoryStartAdjust),
                                    ymd(InventoryEndAdjust)),
         UnitCount = if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) 

# function to calculate unit capacity at the bed record level

unit_capacity <- function(interval) {
  if_else(
    int_overlaps(UnitCapacity$AvailableWindow, interval),
    (as.numeric(
      difftime(
        if_else(
          ymd(UnitCapacity$InventoryEndAdjust) <=  int_end(interval),
          as.POSIXct(UnitCapacity$InventoryEndAdjust),
          int_end(interval)
        ),
        if_else(
          ymd(UnitCapacity$InventoryStartAdjust) >= int_start(interval),
          as.POSIXct(UnitCapacity$InventoryStartAdjust),
          int_start(interval)
        ),
        units = "days"
      )
    ) + 1) * UnitCapacity$UnitCount,
    NULL
  )
}

UnitCapacity <- UnitCapacity %>%
  mutate(
    # PE_Date_Range = unit_capacity(PE_FilePeriod),
    # FilePeriod = unit_capacity(FilePeriod),
    Month1 = unit_capacity(FirstMonth),
    Month2 = unit_capacity(SecondMonth),
    Month3 = unit_capacity(ThirdMonth),
    Month4 = unit_capacity(FourthMonth),
    Month5 = unit_capacity(FifthMonth),
    Month6 = unit_capacity(SixthMonth),
    Month7 = unit_capacity(SeventhMonth),
    Month8 = unit_capacity(EighthMonth),
    Month9 = unit_capacity(NinthMonth),
    Month10 = unit_capacity(TenthMonth),
    Month11 = unit_capacity(EleventhMonth),
    Month12 = unit_capacity(TwelfthMonth),
    Month13 = unit_capacity(ThirteenthMonth),
    Month14 = unit_capacity(FourteenthMonth),
    Month15 = unit_capacity(FifteenthMonth),
    Month16 = unit_capacity(SixteenthMonth),
    Month17 = unit_capacity(SeventeenthMonth),
    Month18 = unit_capacity(EighteenthMonth),
    Month19 = unit_capacity(NineteenthMonth),
    Month20 = unit_capacity(TwentiethMonth),
    Month21 = unit_capacity(TwentyfirstMonth),
    Month22 = unit_capacity(TwentysecondMonth),
    Month23 = unit_capacity(TwentythirdMonth),
    Month24 = unit_capacity(TwentyfourthMonth))

UnitCapacity <- UnitCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(
    # UCPE = sum(PE_Date_Range, na.rm = TRUE),
    # UCY = sum(FilePeriod, na.rm = TRUE),
    UC1 = sum(Month1, na.rm = TRUE),
    UC2 = sum(Month2, na.rm = TRUE),
    UC3 = sum(Month3, na.rm = TRUE),
    UC4 = sum(Month4, na.rm = TRUE),
    UC5 = sum(Month5, na.rm = TRUE),
    UC6 = sum(Month6, na.rm = TRUE),
    UC7 = sum(Month7, na.rm = TRUE),
    UC8 = sum(Month8, na.rm = TRUE),
    UC9 = sum(Month9, na.rm = TRUE),
    UC10 = sum(Month10, na.rm = TRUE),
    UC11 = sum(Month11, na.rm = TRUE),
    UC12 = sum(Month12, na.rm = TRUE),
    UC13 = sum(Month13, na.rm = TRUE),
    UC14 = sum(Month14, na.rm = TRUE),
    UC15 = sum(Month15, na.rm = TRUE),
    UC16 = sum(Month16, na.rm = TRUE),
    UC17 = sum(Month17, na.rm = TRUE),
    UC18 = sum(Month18, na.rm = TRUE),
    UC19 = sum(Month19, na.rm = TRUE),
    UC20 = sum(Month20, na.rm = TRUE),
    UC21 = sum(Month21, na.rm = TRUE),
    UC22 = sum(Month22, na.rm = TRUE),
    UC23 = sum(Month23, na.rm = TRUE),
    UC24 = sum(Month24, na.rm = TRUE)
  ) %>%
  ungroup()

# Unit Utilization --------------------------------------------------------

utilization_unit <- left_join(UnitCapacity,
                              HHNights,
                              by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(
    # FilePeriod = HNY / UCY,
    # accuracy = .1,
    Month1 = HN1 / UC1,
    accuracy = .1,
    Month2 = HN2 / UC2,
    accuracy = .1,
    Month3 = HN3 / UC3,
    accuracy = .1,
    Month4 = HN4 / UC4,
    accuracy = .1,
    Month5 = HN5 / UC5,
    accuracy = .1,
    Month6 = HN6 / UC6,
    accuracy = .1,
    Month7 = HN7 / UC7,
    accuracy = .1,
    Month8 = HN8 / UC8,
    accuracy = .1,
    Month9 = HN9 / UC9,
    accuracy = .1,
    Month10 = HN10 / UC10,
    accuracy = .1,
    Month11 = HN11 / UC11,
    accuracy = .1,
    Month12 = HN12 / UC12,
    accuracy = .1,
    Month13 = HN13 / UC13,
    accuracy = .1,
    Month14 = HN14 / UC14,
    accuracy = .1,
    Month15 = HN15 / UC15,
    accuracy = .1,
    Month16 = HN16 / UC16,
    accuracy = .1,
    Month17 = HN17 / UC17,
    accuracy = .1,
    Month18 = HN18 / UC18,
    accuracy = .1,
    Month19 = HN19 / UC19,
    accuracy = .1,
    Month20 = HN20 / UC20,
    accuracy = .1,
    Month21 = HN21 / UC21,
    accuracy = .1,
    Month22 = HN22 / UC22,
    accuracy = .1,
    Month23 = HN23 / UC23,
    accuracy = .1,
    Month24 = HN24 / UC24,
    accuracy = .1
  ) %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         # FilePeriod,
         starts_with("Month"))

rm(UnitCapacity, HHNights, Utilizers)

names(utilization_unit) <- 
  c("ProjectID", "ProjectName", "ProjectType", 
    # "FilePeriod",
    format.Date(int_start(FirstMonth), "%m%d%Y"),
    format.Date(int_start(SecondMonth), "%m%d%Y"),
    format.Date(int_start(ThirdMonth), "%m%d%Y"),
    format.Date(int_start(FourthMonth), "%m%d%Y"),
    format.Date(int_start(FifthMonth), "%m%d%Y"),
    format.Date(int_start(SixthMonth), "%m%d%Y"),
    format.Date(int_start(SeventhMonth), "%m%d%Y"),
    format.Date(int_start(EighthMonth), "%m%d%Y"),
    format.Date(int_start(NinthMonth), "%m%d%Y"),
    format.Date(int_start(TenthMonth), "%m%d%Y"),
    format.Date(int_start(EleventhMonth), "%m%d%Y"),
    format.Date(int_start(TwelfthMonth), "%m%d%Y"),
    format.Date(int_start(ThirteenthMonth), "%m%d%Y"),
    format.Date(int_start(FourteenthMonth), "%m%d%Y"),
    format.Date(int_start(FifteenthMonth), "%m%d%Y"),
    format.Date(int_start(SixteenthMonth), "%m%d%Y"),
    format.Date(int_start(SeventeenthMonth), "%m%d%Y"),
    format.Date(int_start(EighteenthMonth), "%m%d%Y"),
    format.Date(int_start(NineteenthMonth), "%m%d%Y"),
    format.Date(int_start(TwentiethMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfirstMonth), "%m%d%Y"),
    format.Date(int_start(TwentysecondMonth), "%m%d%Y"),
    format.Date(int_start(TwentythirdMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfourthMonth), "%m%d%Y"))

rm(bed_capacity, bed_nights_per_ee, unit_capacity)

small_project <- Project %>%
  filter(ProjectType %in% c(project_types_w_beds) &
           ymd(OperatingStartDate) <= today() &
           (is.na(OperatingEndDate) | OperatingEndDate >= today()) &
           is.na(Project$GrantType)) %>%
  select(ProjectID,
         ProjectName,
         ProjectType, 
         OrganizationName,
         HMISParticipatingProject)

# Current Bed Utilization -------------------------------------------------

small_inventory <- Inventory %>%
  filter((ymd(InventoryStartDate) <= today() &
            (
              ymd(InventoryEndDate) >= today() |
                is.na(InventoryEndDate)
            )) &
           Inventory$CoCCode %in% c("OH-507", "OH-504")) %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate
  )

small_inventory <- inner_join(small_project, small_inventory, by = "ProjectID")

Capacity <- small_inventory %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         OrganizationName,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(UnitCount = if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) %>%
  group_by(ProjectID, ProjectName, ProjectType, OrganizationName) %>%
  summarise(UnitCount = sum(UnitCount),
            BedCount = sum(BedInventory)) %>%
  ungroup()

providerids <- Capacity %>% 
  select(ProjectID, ProjectName, OrganizationName, ProjectType) %>%
  arrange(ProjectName) 
#here is where you could add a left join to the Regions object and add in Region

Clients <- Enrollment %>%
  left_join(., providerids, by = c("ProjectID", "ProjectName")) %>%
  filter(is.na(ExitDate)) %>%
  group_by(ProjectID, ProjectName) %>%
  summarise(Clients = n_distinct(PersonalID)) %>%
  ungroup()

Households <- Enrollment %>%
  left_join(., providerids, by = c("ProjectID", "ProjectName")) %>%
  filter(is.na(ExitDate)) %>%
  group_by(ProjectID, ProjectName) %>%
  summarise(Households = n_distinct(HouseholdID)) %>%
  ungroup()

utilization <-
  left_join(Capacity, Clients,
            by = c("ProjectID", "ProjectName")) %>%
  left_join(., Households, 
            by = c("ProjectID", "ProjectName")) %>%
  filter(ProjectType %in% c(project_types_w_beds)) %>%
  mutate(BedUtilization = percent(Clients/BedCount, accuracy = 1),
         UnitUtilization = percent(Households/UnitCount, accuracy = 1))

names(utilizers_clients) <-
  c(
    "ProjectName",
    "ProjectID",
    "ProjectType",
    "PersonalID",
    "EnrollmentID",
    "EntryDate",
    "MoveInDate",
    "ExitDate",
    format.Date(int_start(FirstMonth), "%m%d%Y"),
    format.Date(int_start(SecondMonth), "%m%d%Y"),
    format.Date(int_start(ThirdMonth), "%m%d%Y"),
    format.Date(int_start(FourthMonth), "%m%d%Y"),
    format.Date(int_start(FifthMonth), "%m%d%Y"),
    format.Date(int_start(SixthMonth), "%m%d%Y"),
    format.Date(int_start(SeventhMonth), "%m%d%Y"),
    format.Date(int_start(EighthMonth), "%m%d%Y"),
    format.Date(int_start(NinthMonth), "%m%d%Y"),
    format.Date(int_start(TenthMonth), "%m%d%Y"),
    format.Date(int_start(EleventhMonth), "%m%d%Y"),
    format.Date(int_start(TwelfthMonth), "%m%d%Y"),
    format.Date(int_start(ThirteenthMonth), "%m%d%Y"),
    format.Date(int_start(FourteenthMonth), "%m%d%Y"),
    format.Date(int_start(FifteenthMonth), "%m%d%Y"),
    format.Date(int_start(SixteenthMonth), "%m%d%Y"),
    format.Date(int_start(SeventeenthMonth), "%m%d%Y"),
    format.Date(int_start(EighteenthMonth), "%m%d%Y"),
    format.Date(int_start(NineteenthMonth), "%m%d%Y"),
    format.Date(int_start(TwentiethMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfirstMonth), "%m%d%Y"),
    format.Date(int_start(TwentysecondMonth), "%m%d%Y"),
    format.Date(int_start(TwentythirdMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfourthMonth), "%m%d%Y")
  )

rm(Households, Clients, Capacity, small_inventory, small_project, providerids)

note_bed_utilization <- "Bed Utilization is the percentage of a project's 
available beds being populated by individual clients."

note_unit_utilization <- "Unit Utilization is the percentage of a project's 
available units being populated by households. A household can be a single 
individual or multiple clients presenting together for housing."

note_calculation_utilization <- "Bed Utilization = bed nights* served / total 
possible bed nights** in a month.
<p> Unit Utilization = unit nights served / total possible unit nights in a 
month.

<p>* A bed night is a single night in a bed.
<p>* A unit night is a single night in a unit.
<p>** Total possible bed/unit nights = number of beds/units a project has × how 
many days are in the given month.
<p>Example A: Client A enters a shelter on May 1 and exits on May 5. They spent 
four nights in the shelter, so that was 4 bed nights from that client alone in 
the month of May for that shelter.

<p>Example B: PSH Project A served 10 people every single night in the month of 
June. Each client was served 30 bed nights during that month, and since there 
were 10 clients, that PSH project served a total of 300 bed nights for the 
month of June.

<p>Example C: PSH Project B has 5 beds. That project's total possible bed 
nights for the month of April (which has 30 days in it) is 30 x 5, which is 150.

<p>Example D: Using what we know from Example B of PSH Project A's total bed 
nights for the month of June, let's calculate what their bed utilization was 
for that month. They have 11 beds and June has 30 days so since 11 × 30 = 330 
possible bed nights. Their bed utilization is bed nights (300) divided by 
possible bed nights (330), which is: 91%!"

# removing all the Value objects we created as those are not used in the apps
rm(list = ls(all.names = TRUE, pattern = "Month$"))
rm(list = ls(all.names = TRUE, pattern = "co_"))


# Find Outliers for HIC Purposes ------------------------------------------

# utilization_unit_overall <- utilization_unit %>%
#   select(ProjectID, ProjectName, ProjectType, FilePeriod)
# 
# outliers_hi <- subset(utilization_unit_overall,
#                       FilePeriod > quantile(FilePeriod, prob = 0.90))
# 
# outliers_lo <- subset(utilization_unit_overall,
#                       FilePeriod < quantile(FilePeriod, prob = 0.03))
# 
# outliers <- rbind(outliers_hi, outliers_lo)

# WARNING save.image does not save the environment properly, save must be used.
save(list = ls(), file = "images/Utilization.RData", compress = FALSE)



