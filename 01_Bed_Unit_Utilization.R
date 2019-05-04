library(tidyverse)
library(lubridate)
library(scales)
load("data/COHHIOHMIS.RData")

#if you want to change the reporting dates, you have to do it in the 
#get_the_csv_things script

# Creating Beds table -----------------------------------------------------

SmallProject <- Project %>%
  select(ProjectID,
         ProjectName,
         ProjectType) %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9, 13) &
           operating_between(Project, ReportStart, ReportEnd) &
           is.na(Project$GrantType))

SmallInventory <- Inventory %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate,
    HMISParticipatingBeds
  )  %>%
  filter((
    ymd(InventoryStartDate) <= mdy(ReportEnd) &
      (
        ymd(InventoryEndDate) >= mdy(ReportStart) |
          is.na(InventoryEndDate)
      )
  ) &
    !is.na(HMISParticipatingBeds) &
    Inventory$CoCCode == "OH-507")

Beds <- inner_join(SmallProject, SmallInventory, by = "ProjectID")
# Creating Utilizers table ------------------------------------------------

SmallEnrollment <- Enrollment %>% 
  select(PersonalID,
         EnrollmentID,
         ProjectID,
         EntryDate,
         ExitDate,
         HouseholdID,
         RelationshipToHoH,
         MoveInDate)

Utilizers <- semi_join(SmallEnrollment, Beds, by = "ProjectID") 

Utilizers <- left_join(Utilizers, SmallProject, by = "ProjectID") %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectName,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    MoveInDate,
    ExitDate
  )
# Cleaning up the house ---------------------------------------------------

rm(Affiliation, Client, Disabilities, EmploymentEducation, EnrollmentCoC, Exit, 
   Export, Funder, Geography, HealthAndDV, IncomeBenefits, Organization, 
   ProjectCoC, Scores, Services, SmallEnrollment, SmallInventory, SmallProject, 
   Users, Offers, VeteranCE)
# Client Utilization of Beds ----------------------------------------------

# filtering out any PSH or RRH records without a proper Move-In Date plus the 
# fake training providers
ClientUtilizers <- Utilizers %>%
  mutate(EntryAdjust = case_when(
           ProjectType %in% c(1, 2, 8) ~ EntryDate,
           ProjectType %in% c(3, 9, 13) ~ MoveInDate),
         ExitAdjust = if_else(is.na(ExitDate), today(), ymd(ExitDate)),
         StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
           ) %>%
  filter(
    int_overlaps(StayWindow, ReportingPeriod) &
      (
    (
      ProjectType %in% c(3, 9, 13) &
        !is.na(EntryAdjust) &
        ymd(MoveInDate) >= ymd(EntryDate) &
        ymd(MoveInDate) < ymd(ExitAdjust)
    ) |
      ProjectType %in% c(1, 2, 8)
  ) &
    !ProjectID %in% c(1775, 1695, 1849, 1032, 1030, 1031, 1317)) %>%
  select(-EntryDate, -MoveInDate, -ExitDate)

# function for adding bed nights per ee

bed_nights_per_ee <- function(table, interval) {
  if_else(int_overlaps(table$StayWindow, interval),
          as.numeric(difftime(
            if_else(
              ymd(table$ExitAdjust) <=  int_end(interval),
              as.POSIXct(table$ExitAdjust) + days(1),
              int_end(interval) + days(1)
            ),
            if_else(
              ymd(table$EntryAdjust) >= int_start(interval),
              as.POSIXct(table$EntryAdjust),
              int_start(interval)
            ),
            units = "days"
          )), NULL
  )
}


# adding in month columns with utilization numbers

ClientUtilizers <- ClientUtilizers %>%
  mutate(
    ReportPeriod = bed_nights_per_ee(ClientUtilizers, ReportingPeriod),
    Month1 = bed_nights_per_ee(ClientUtilizers, FirstMonth),
    Month2 = bed_nights_per_ee(ClientUtilizers, SecondMonth),
    Month3 = bed_nights_per_ee(ClientUtilizers, ThirdMonth),
    Month4 = bed_nights_per_ee(ClientUtilizers, FourthMonth),
    Month5 = bed_nights_per_ee(ClientUtilizers, FifthMonth),
    Month6 = bed_nights_per_ee(ClientUtilizers, SixthMonth),
    Month7 = bed_nights_per_ee(ClientUtilizers, SeventhMonth),
    Month8 = bed_nights_per_ee(ClientUtilizers, EighthMonth),
    Month9 = bed_nights_per_ee(ClientUtilizers, NinthMonth),
    Month10 = bed_nights_per_ee(ClientUtilizers, TenthMonth),
    Month11 = bed_nights_per_ee(ClientUtilizers, EleventhMonth),
    Month12 = bed_nights_per_ee(ClientUtilizers, TwelfthMonth)
  ) %>%
  select(ProjectName, ProjectID, ProjectType, PersonalID, EnrollmentID, 
         ReportPeriod, starts_with("Month"))
ClientUtilizers <- as.data.frame(ClientUtilizers)
# making granularity by provider instead of by enrollment id
BedNights <- ClientUtilizers %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(
    BNY = sum(ReportPeriod, na.rm = TRUE),
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
    BN12 = sum(Month12, na.rm = TRUE)
  )
rm(ClientUtilizers)
# Bed Capacity ------------------------------------------------------------

BedCapacity <- Beds %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(InventoryEndAdjust = if_else(is.na(InventoryEndDate),
                                      mdy(ReportEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(ReportStart),
                                        ymd(InventoryStartDate),
                                        mdy(ReportStart)),
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
    ReportPeriod = bed_capacity(ReportingPeriod),
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
    Month12 = bed_capacity(TwelfthMonth)) %>%
  select(-InventoryStartDate, -InventoryEndDate, -InventoryEndAdjust,
         -BedInventory, -InventoryStartAdjust, -AvailableWindow)

BedCapacity <- BedCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(
    BCY = sum(ReportPeriod, na.rm = TRUE),
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
    BC12 = sum(Month12, na.rm = TRUE)
  )
# Bed Utilization ---------------------------------------------------------

BedUtilization <- 
  left_join(BedCapacity,
            BedNights,
            by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(
    ReportPeriod = percent(BNY / BCY, accuracy = .1),
    Month1 = percent(BN1 / BC1, accuracy = .1),
    Month2 = percent(BN2 / BC2, accuracy = .1),
    Month3 = percent(BN3 / BC3, accuracy = .1),
    Month4 = percent(BN4 / BC4, accuracy = .1),
    Month5 = percent(BN5 / BC5, accuracy = .1),
    Month6 = percent(BN6 / BC6, accuracy = .1),
    Month7 = percent(BN7 / BC7, accuracy = .1),
    Month8 = percent(BN8 / BC8, accuracy = .1),
    Month9 = percent(BN9 / BC9, accuracy = .1),
    Month10 = percent(BN10 / BC10, accuracy = .1),
    Month11 = percent(BN11 / BC11, accuracy = .1),
    Month12 = percent(BN12 / BC12, accuracy = .1)
  ) %>% 
  select(ProjectID, ProjectName, ProjectType, ReportPeriod, Month1, Month2, 
         Month3, Month4, Month5, Month6, Month7, Month8, Month9, Month10, 
         Month11, Month12)

rm(BedCapacity, BedNights)

names(BedUtilization) <- 
  c("ProjectID", "ProjectName", "ProjectType", "ReportingPeriod",
    month.name[(month(ymd(int_start(FirstMonth))))],
    month.name[(month(ymd(int_start(SecondMonth))))],
    month.name[(month(ymd(int_start(ThirdMonth))))],
    month.name[(month(ymd(int_start(FourthMonth))))],
    month.name[(month(ymd(int_start(FifthMonth))))],
    month.name[(month(ymd(int_start(SixthMonth))))],
    month.name[(month(ymd(int_start(SeventhMonth))))],
    month.name[(month(ymd(int_start(EighthMonth))))],
    month.name[(month(ymd(int_start(NinthMonth))))],
    month.name[(month(ymd(int_start(TenthMonth))))],
    month.name[(month(ymd(int_start(EleventhMonth))))],
    month.name[(month(ymd(int_start(TwelfthMonth))))])

#Inf means there were no beds but there were clients served.
#%NaN means there were no beds and no clients served that month.

# HH Utilization of Units -------------------------------------------------

HHUtilizers <- Utilizers %>%
  mutate(EntryAdjust = case_when(
    ProjectType %in% c(1, 2, 8) ~ EntryDate,
    ProjectType %in% c(3, 9, 13) ~ MoveInDate),
    ExitAdjust = if_else(is.na(ExitDate), mdy(ReportEnd), ymd(ExitDate)),
    ExitDate = NULL,
    StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
  ) %>%
  filter(
    (str_detect(HouseholdID, fixed("s_")) |
       str_detect(HouseholdID, fixed("h_")) &
       RelationshipToHoH == 1) &
    int_overlaps(StayWindow, ReportingPeriod) &
      (
        (
          ProjectType %in% c(3, 9, 13) &
            !is.na(EntryAdjust) &
            ymd(MoveInDate) >= ymd(EntryDate) &
            ymd(MoveInDate) < ymd(ExitAdjust)
        ) |
          ProjectType %in% c(1, 2, 8)
      ) &
      !ProjectID %in% c(1775, 1695, 1849, 1032, 1030, 1031, 1317)) %>%
  select(-EntryDate, -MoveInDate, -HouseholdID, -RelationshipToHoH)

HHUtilizers <- HHUtilizers %>%
  mutate(
    ReportPeriod = bed_nights_per_ee(HHUtilizers, ReportingPeriod),
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
    Month12 = bed_nights_per_ee(HHUtilizers, TwelfthMonth)
    )
HHUtilizers <- as.data.frame(HHUtilizers)
# making granularity by provider instead of by enrollment id
HHNights <- HHUtilizers %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(
    HNY = sum(ReportPeriod, na.rm = TRUE),
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
    HN12 = sum(Month12, na.rm = TRUE)
  )
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
                                      mdy(ReportEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(ReportStart),
                                        ymd(InventoryStartDate),
                                        mdy(ReportStart)),
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
    ReportPeriod = unit_capacity(ReportingPeriod),
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
    Month12 = unit_capacity(TwelfthMonth))

UnitCapacity <- UnitCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(
    UCY = sum(ReportPeriod, na.rm = TRUE),
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
    UC12 = sum(Month12, na.rm = TRUE)
  )

# Unit Utilization --------------------------------------------------------

UnitUtilization <- left_join(UnitCapacity,
                            HHNights,
                            by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(
    ReportPeriod = percent(HNY / UCY, accuracy = .1),
    Month1 = percent(HN1 / UC1, accuracy = .1),
    Month2 = percent(HN2 / UC2, accuracy = .1),
    Month3 = percent(HN3 / UC3, accuracy = .1),
    Month4 = percent(HN4 / UC4, accuracy = .1),
    Month5 = percent(HN5 / UC5, accuracy = .1),
    Month6 = percent(HN6 / UC6, accuracy = .1),
    Month7 = percent(HN7 / UC7, accuracy = .1),
    Month8 = percent(HN8 / UC8, accuracy = .1),
    Month9 = percent(HN9 / UC9, accuracy = .1),
    Month10 = percent(HN10 / UC10, accuracy = .1),
    Month11 = percent(HN11 / UC11, accuracy = .1),
    Month12 = percent(HN12 / UC12, accuracy = .1)
  ) %>%
  select(ProjectID, ProjectName, ProjectType, ReportPeriod, Month1, Month2, 
         Month3, Month4, Month5, Month6, Month7, Month8, Month9, Month10, 
         Month11, Month12)
rm(UnitCapacity, HHNights, Beds, Utilizers)

names(UnitUtilization) <- 
  c("ProjectID", "ProjectName", "ProjectType", "ReportingPeriod",
    month.name[(month(ymd(int_start(FirstMonth))))],
    month.name[(month(ymd(int_start(SecondMonth))))],
    month.name[(month(ymd(int_start(ThirdMonth))))],
    month.name[(month(ymd(int_start(FourthMonth))))],
    month.name[(month(ymd(int_start(FifthMonth))))],
    month.name[(month(ymd(int_start(SixthMonth))))],
    month.name[(month(ymd(int_start(SeventhMonth))))],
    month.name[(month(ymd(int_start(EighthMonth))))],
    month.name[(month(ymd(int_start(NinthMonth))))],
    month.name[(month(ymd(int_start(TenthMonth))))],
    month.name[(month(ymd(int_start(EleventhMonth))))],
    month.name[(month(ymd(int_start(TwelfthMonth))))])

rm(bed_capacity, bed_nights_per_ee, unit_capacity)

SmallProject <- Project %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9, 13) &
           ymd(OperatingStartDate) <= today() &
           (is.na(OperatingEndDate) | OperatingEndDate >= today()) &
           is.na(Project$GrantType)) %>%
  select(ProjectID,
         ProjectName,
         ProjectType, 
         OrganizationName)

SmallInventory <- Inventory %>%
  filter((ymd(InventoryStartDate) <= today() &
            (
              ymd(InventoryEndDate) >= today() |
                is.na(InventoryEndDate)
            )) &
           !is.na(HMISParticipatingBeds) &
           Inventory$CoCCode == "OH-507") %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate,
    HMISParticipatingBeds
  )

SmallInventory <- inner_join(SmallProject, SmallInventory, by = "ProjectID")

Capacity <- SmallInventory %>%
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
  left_join(., providerids, by = "ProjectID") %>%
  filter(is.na(ExitDate)) %>%
  group_by(ProjectID, ProjectName) %>%
  summarise(Clients = n_distinct(PersonalID)) %>%
  ungroup()

Households <- Enrollment %>%
  left_join(., providerids, by = "ProjectID") %>%
  filter(is.na(ExitDate)) %>%
  group_by(ProjectID, ProjectName) %>%
  summarise(Households = n_distinct(HouseholdID)) %>%
  ungroup()

Utilization <-
  left_join(Capacity, Clients,
            by = c("ProjectID", "ProjectName")) %>%
  left_join(., Households, 
            by = c("ProjectID", "ProjectName")) %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9)) %>%
  mutate(BedUtilization = percent(Clients/BedCount),
         UnitUtilization = percent(Households/UnitCount))

rm(Households, Clients, Capacity, Enrollment, Project, Inventory, 
   SmallInventory, SmallProject)

save.image("data/Utilization.RData")