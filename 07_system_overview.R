logToConsole("Running system overview")

# Age ---------------------------------------------------------------------
system_person_ages <- EnrollmentAdjust %>%
  group_by(PersonalID) %>%
  slice_max(AgeAtEntry, na_rm = TRUE, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(AgeCategory = factor(
    case_when(
      is.na(AgeAtEntry) | AgeAtEntry < 0 ~ "Unknown",
      AgeAtEntry >= 0 & AgeAtEntry <= 12 ~ "0 to 12",
      AgeAtEntry >= 13 & AgeAtEntry <= 17 ~ "13 to 17",
      AgeAtEntry >= 18 & AgeAtEntry <= 21 ~ "18 to 21",
      AgeAtEntry >= 22 & AgeAtEntry <= 24 ~ "22 to 24",
      AgeAtEntry >= 25 & AgeAtEntry <= 34 ~ "25 to 34",
      AgeAtEntry >= 35 & AgeAtEntry <= 44 ~ "35 to 44",
      AgeAtEntry >= 45 & AgeAtEntry <= 54 ~ "45 to 54",
      AgeAtEntry >= 55 & AgeAtEntry <= 64 ~ "55 to 64",
      AgeAtEntry >= 65 & AgeAtEntry <= 74 ~ "65 to 74",
      AgeAtEntry >= 75 ~ "75 and older",
      TRUE ~ "something's wrong"
    ),
    levels = c(
      "0 to 12",
      "13 to 17",
      "18 to 21",
      "22 to 24",
      "25 to 34",
      "35 to 44",
      "45 to 54",
      "55 to 64",
      "65 to 74",
      "75 and older",
      "Unknown"
    )
  )) %>%
  select(PersonalID, "MostRecentAgeAtEntry" = AgeAtEntry, AgeCategory)

# Build report dates ------------------------------------------------------

# if the start date's day of the month = 1, then that's the start date
# otherwise go forward a month and use the 1st of that month.
ExportStartAdjusted <- if_else(
  day(meta_HUDCSV_Export_Start()) == 1,
  meta_HUDCSV_Export_Start(),
  floor_date(meta_HUDCSV_Export_Start() %m+% months(1), unit = "month"))

# if you go forward to the first day of the next month and then subtract a day,
# and that equals the raw ExportEndDate, that means it is already a last day of
# the month so we just return the raw ExportEndDate. If the date is something
# other than that, then we want to get the first day of the month and go back 
# a day so that it cuts off on the last day of the month previous to the raw
# ExportEndDate
ExportEndAdjusted <- if_else(
  floor_date(meta_HUDCSV_Export_End() %m+% months(1), unit = "month") - days(1) ==
    meta_HUDCSV_Export_End(),
  meta_HUDCSV_Export_End(),
  floor_date(meta_HUDCSV_Export_End(), unit = "month") - days(1))

ReportEnd <- ExportEndAdjusted
ReportStart <- ReportEnd - years(1) + days(1)

# Data prep ---------------------------------------------------------------

# using EnrollmentAdjust because that df doesn't contain enrollments that fall
# outside periods of operation/participation
enrollment_prep <- EnrollmentAdjust %>%
  select(EnrollmentID,
         PersonalID,
         ProjectID,
         ProjectType,
         HouseholdID,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ExitAdjust,
         Destination,
         AgeAtEntry,
         RelationshipToHoH,
         LivingSituation,
         RentalSubsidyType,
         LengthOfStay,
         LOSUnderThreshold,
         PreviousStreetESSH,
         DateToStreetESSH,
         TimesHomelessPastThreeYears,
         MonthsHomelessPastThreeYears,
         DisablingCondition,
         Destination
         ) %>%
  left_join(Project %>% 
              select(ProjectID,
                     ProjectName,
                     OrganizationID,
                     RRHSubType,
                     ContinuumProject),
            join_by(ProjectID)) %>%
  left_join(Organization %>%
              select(OrganizationID, OrganizationName) %>%
              unique(),
            by = "OrganizationID") %>%
  left_join(Client %>%
              select(PersonalID, VeteranStatus), by = "PersonalID") %>%
  left_join(HealthAndDV %>%
              filter(DataCollectionStage == 1) %>%
              select(EnrollmentID, DomesticViolenceSurvivor, CurrentlyFleeing),
            by = "EnrollmentID") %>%
  left_join(system_person_ages, join_by(PersonalID)) %>%
  filter(ContinuumProject == 1) 
# IMPORTANT: ^ same granularity as EnrollmentAdjust!

# corrected hohs ----------------------------------------------------------

hh_adjustments <- enrollment_prep %>%
  mutate(VeteranStatus = if_else(VeteranStatus == 1 &
                                   !is.na(VeteranStatus), 1, 0),
         HoHAlready = if_else(RelationshipToHoH == 1 &
                                AgeAtEntry > 17, 1, 0)) %>%
  group_by(HouseholdID, ProjectID) %>%
  arrange(desc(HoHAlready),
          desc(VeteranStatus),
          desc(AgeAtEntry),
          PersonalID,
          .by_group = TRUE) %>%
  mutate(Sequence = seq(n()),
         CorrectedHoH = if_else(Sequence == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(HouseholdID) %>%
  mutate(
    HouseholdType = factor(case_when(
      all(AgeAtEntry < 25 & AgeAtEntry >= 18, na.rm = TRUE) &
        !any(is.na(AgeAtEntry)) ~ "Youth and Young Adult",
      all(AgeAtEntry >= 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~
        "Adult-Only",
      any(AgeAtEntry < 18, na.rm = TRUE) & any(AgeAtEntry >= 18, na.rm = TRUE) ~
        "Adult-Child",
      all(AgeAtEntry < 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~
        "Child-Only",
      TRUE ~ "Unknown Household"
    ),
    levels = c("Youth and Young Adult",
               "Adult-Only",
               "Adult-Child",
               "Child-Only",
               "Unknown Household"))
  ) %>%
  ungroup() %>%
  select(EnrollmentID, CorrectedHoH, HouseholdType)

# keeps original HoH unless the HoH is younger than 18 or if there are mult hohs
# if they are younger than 18, or if there are mult hohs, it will take the
# veteran in the hh. if there are none or multiple veterans, it will take the
# oldest. if they're the same veteran status and age, it will name the person
# with the first PersonalID.
# if the hh looks like: 
# age 8 (hoh), age 40 (non-veteran), age 38 (veteran) ->  age 38 (veteran)
# age 16 (hoh), age 8, age 3 -> hoh stays age 16 (bc they're the oldest)
# age 42 (hoh), age 52 (veteran), age 5 -> hoh stays age 42 (hoh) (trusting the
# decisions of the case manager, maybe the 42 year old was the eligible person)
# age 53 (hoh), age 46 (hoh), age 17 -> only the age 53 and not the age 46

# adding corrected hoh ----------------------------------------------------
enrollment_prep_hohs <- enrollment_prep %>%
  left_join(hh_adjustments, join_by(EnrollmentID)) %>%
  relocate(CorrectedHoH, .after = RelationshipToHoH)

# (^ also same granularity as EnrollmentAdjust)

rm(hh_adjustments)


# Enrollment-level flags --------------------------------------------------
# as much wrangling as possible without needing hhtype, project type, and level
# of detail inputs
enrollment_categories <- enrollment_prep_hohs %>%
  filter(ReportStart - years(2) <= ExitAdjust &
           ProjectType != 12) %>% 
  mutate(
    lh_prior_livingsituation = !is.na(LivingSituation) &
      (
        LivingSituation %in% homeless_livingsituation |
          (
            LivingSituation %in% institutional_livingsituation &
              LOSUnderThreshold == 1 &
              PreviousStreetESSH == 1 &
              !is.na(LOSUnderThreshold) &
              !is.na(PreviousStreetESSH)
          )
      ),
    lh_at_entry =
      lh_prior_livingsituation == TRUE |
      ProjectType %in% lh_project_types,
    EnrolledHomeless =
      ProjectType %in% project_types_enrolled_homeless |
      lh_prior_livingsituation == TRUE,
    straddles_start =
      EntryDate <= ReportStart &
      ExitAdjust >= ReportStart,
    in_date_range =
      ExitAdjust >= ReportStart &
      EntryDate <= ReportEnd,
    # Domestic Violence - this is needed for the System Composition chart
    DomesticViolenceCategory = case_when(
      DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1 ~
        syso_spec_pops_people[2], # DV Currently Fleeing
      DomesticViolenceSurvivor == 1 &
        (is.na(CurrentlyFleeing) | CurrentlyFleeing != 1) ~
        syso_spec_pops_people[3], # DV Not Currently Fleeing
      DomesticViolenceSurvivor == 1 ~
        syso_spec_pops_people[4], # DV Total
      TRUE ~
        syso_spec_pops_people[1] # No Special Population Selected
      )
  ) %>%
  select(
    EnrollmentID, 
    PersonalID, 
    HouseholdID,
    EntryDate, 
    MoveInDateAdjust,
    ExitDate, 
    ExitAdjust,
    ProjectType, 
    lh_prior_livingsituation,
    lh_at_entry,
    straddles_start,
    in_date_range,
    EnrolledHomeless,
    LivingSituation,
    LOSUnderThreshold,
    PreviousStreetESSH,
    Destination,
    AgeAtEntry,
    CorrectedHoH,
    DomesticViolenceCategory,
    HouseholdType
  ) %>% 
  group_by(PersonalID) %>%
  arrange(EntryDate, .by_group = TRUE) %>%
  mutate(
    ordinal = row_number(),
    days_to_next_entry =
      difftime(lead(EntryDate, order_by = EntryDate),
               ExitAdjust,
               units = "days"),
    days_since_previous_exit =
      difftime(EntryDate,
               lag(ExitAdjust, order_by = ExitAdjust),
               units = "days")
  ) %>%
  group_by(PersonalID, in_date_range) %>%
  mutate(
    lecr = in_date_range == TRUE & max(ordinal) == ordinal,
    eecr = in_date_range == TRUE & min(ordinal) == ordinal,
    lookback = if_else(in_date_range == TRUE, 0, rev(row_number()))
  ) %>%
  ungroup() %>%
  select(-AgeAtEntry)

nbn_enrollments_services <- Services %>%
  filter(RecordType == 200) %>%
  inner_join(EnrollmentAdjust %>%
               filter(ProjectType == 1) %>%
               select(EnrollmentID),
             join_by(EnrollmentID)) %>%
  mutate(
    nbn_service_15_before_start =
      between(DateProvided,
              ReportStart - days(15),
              DateProvided),
    nbn_service_15_after_end =
      between(DateProvided,
              DateProvided,
              ReportEnd + days(15))
  ) %>%
  filter(
    nbn_service_15_before_start == TRUE |
      nbn_service_15_after_end == TRUE) %>%
  select(EnrollmentID,
         nbn_service_15_before_start,
         nbn_service_15_after_end)

# using data.table --------------------------------------------------------
# before_dt <- now()
# Left join enrollment_categories on nbn_enrollments_services
enrollment_categories_dt <-
  as.data.table(nbn_enrollments_services)[as.data.table(enrollment_categories),
                                          on = .(EnrollmentID)]
# after_dt <- now()


# using table.express -----------------------------------------------------

# before_te <- now()
# Left join enrollment_categories on nbn_enrollments_services
enrollment_categories_exp <- as.data.table(enrollment_categories) %>%
  table.express::left_join(as.data.table(nbn_enrollments_services), EnrollmentID)
# after_te <- now()

# compare -----------------------------------------------------------------

# after_dt - before_dt
# 
# after_te - before_te

outreach_w_proper_cls_vector <- 
  CurrentLivingSituation %>%
  filter(CurrentLivingSituation %in% homeless_livingsituation &
           between(InformationDate,
                   ReportEnd - days(60),
                   ReportEnd + days(60))) %>%
  pull(EnrollmentID) %>%
  unique()

# Client-level flags ------------------------------------------------------
# will help us categorize people

client_categories <- Client %>%
  select(PersonalID,
         all_of(race_cols),
         all_of(gender_cols),
         VeteranStatus
  ) %>%
  left_join(system_person_ages, join_by(PersonalID)) %>%
  mutate(
    VeteranStatus = if_else(VeteranStatus == 1 &
                              !is.na(VeteranStatus), 1, 0),
    # flattening data and eliminating nulls in case they're present
    Woman = if_else(Woman == 1 & !is.na(Woman), 1, 0),
    Man = if_else(Man == 1 & !is.na(Man), 1, 0),
    NonBinary = if_else(NonBinary == 1 & !is.na(NonBinary), 1, 0),
    Transgender = if_else(Transgender == 1 & !is.na(Transgender), 1, 0),
    CulturallySpecific = if_else(CulturallySpecific == 1 & !is.na(CulturallySpecific), 1, 0),
    DifferentIdentity = if_else(DifferentIdentity == 1 & !is.na(DifferentIdentity), 1, 0),
    Questioning = if_else(Questioning == 1 & !is.na(Questioning), 1, 0),
    # exclusive logic
    TransgenderExclusive = if_else(Transgender == 1, 1, 0),
    GenderExpansiveExclusive = if_else(
      Transgender == 0 &
        (CulturallySpecific + NonBinary + DifferentIdentity + Questioning > 0 | 
        (Man == 1 & Woman == 1)),
      1,
      0
    ),
    ManExclusive = if_else(
      Man == 1 &
        CulturallySpecific +
        NonBinary +
        DifferentIdentity +
        Questioning +
        Woman +
        Transgender == 0, 1, 0),
    WomanExclusive = if_else(
      Woman == 1 &
        CulturallySpecific +
        NonBinary +
        DifferentIdentity +
        Questioning +
        Man +
        Transgender == 0, 1, 0),
    UnknownExclusive = if_else(
      Woman +
        CulturallySpecific +
        NonBinary +
        DifferentIdentity +
        Questioning +
        Man +
        Transgender == 0, 1, 0),
    DQExclusive = TransgenderExclusive + GenderExpansiveExclusive + ManExclusive +
        WomanExclusive + UnknownExclusive, # all values should = 1
    # inclusive logic
  TransgenderInclusive = if_else(
    Transgender == 1 |
      (Woman == 1 & Man == 1) |
      CulturallySpecific + NonBinary + DifferentIdentity + Questioning > 0, 1, 0),
  WomanInclusive = if_else(Woman == 1, 1, 0),
  ManInclusive = if_else(Man == 1, 1, 0),
  CisInclusive = if_else ((
    Woman == 1 &
      Man + NonBinary + Transgender + CulturallySpecific +
      DifferentIdentity + Questioning == 0
  ) |
    (
      Man == 1 &
        Woman + NonBinary + Transgender + CulturallySpecific +
        DifferentIdentity + Questioning == 0
    ),
  1,
  0
  ),
  NonBinaryInclusive = if_else(NonBinary == 1, 1, 0),
  ## Race/Ethnicity
  # flattening the values, eliminating nulls
  AmIndAKNative = if_else(AmIndAKNative == 1 & !is.na(AmIndAKNative), 1, 0),
  Asian = if_else(Asian == 1 & !is.na(Asian), 1, 0),
  BlackAfAmerican = if_else(BlackAfAmerican == 1 & !is.na(BlackAfAmerican), 1, 0),
  NativeHIPacific = if_else(NativeHIPacific == 1 & !is.na(NativeHIPacific), 1, 0),
  White = if_else(White == 1 & !is.na(White), 1, 0),
  MidEastNAfrican = if_else(MidEastNAfrican == 1 & !is.na(MidEastNAfrican), 1, 0),
  HispanicLatinaeo = if_else(HispanicLatinaeo == 1 & !is.na(HispanicLatinaeo), 1, 0),
  # exclusive logic group 1
  AmIndAKNativeAloneExclusive1 = 
    if_else(AmIndAKNative == 1 &
              Asian +
              BlackAfAmerican +
              NativeHIPacific +
              White +
              MidEastNAfrican +
              HispanicLatinaeo == 0, 1, 0),
  AmIndAKNativeLatineExclusive1 = 
    if_else(AmIndAKNative == 1 & HispanicLatinaeo == 1 &
              Asian +
              BlackAfAmerican +
              NativeHIPacific +
              White +
              MidEastNAfrican == 0, 1, 0),
  AsianAloneExclusive1 =
    if_else(Asian == 1 &
              AmIndAKNative +
              BlackAfAmerican +
              NativeHIPacific +
              White +
              MidEastNAfrican +
              HispanicLatinaeo == 0, 1, 0),
  AsianLatineExclusive1 =
    if_else(Asian == 1 & HispanicLatinaeo == 1 &
              AmIndAKNative +
              BlackAfAmerican +
              NativeHIPacific +
              White +
              MidEastNAfrican == 0, 1, 0),
  BlackAfAmericanAloneExclusive1 =
    if_else(BlackAfAmerican == 1 &
              AmIndAKNative +
              Asian +
              NativeHIPacific +
              White +
              MidEastNAfrican +
              HispanicLatinaeo == 0, 1, 0),
  BlackAfAmericanLatineExclusive1 =
    if_else(BlackAfAmerican == 1 & HispanicLatinaeo == 1 &
              AmIndAKNative +
              Asian +
              NativeHIPacific +
              White +
              MidEastNAfrican == 0, 1, 0),
  LatineAloneExclusive1 =
    if_else(HispanicLatinaeo == 1 &
              AmIndAKNative +
              Asian +
              NativeHIPacific +
              White +
              MidEastNAfrican +
              BlackAfAmerican == 0, 1, 0),
  MENAAloneExclusive1 =
    if_else(MidEastNAfrican == 1 &
              AmIndAKNative +
              Asian +
              NativeHIPacific +
              White +
              HispanicLatinaeo +
              BlackAfAmerican == 0, 1, 0),
  MENALatineExclusive1 =
    if_else(MidEastNAfrican == 1 & HispanicLatinaeo == 1 &
              AmIndAKNative +
              Asian +
              NativeHIPacific +
              White +
              BlackAfAmerican == 0, 1, 0),
  NativeHIPacificAloneExclusive1 =
    if_else(NativeHIPacific == 1 &
              AmIndAKNative +
              Asian +
              MidEastNAfrican +
              White +
              HispanicLatinaeo +
              BlackAfAmerican == 0, 1, 0),
  NativeHIPacificLatineExclusive1 =
    if_else(NativeHIPacific == 1 & HispanicLatinaeo == 1 &
              AmIndAKNative +
              Asian +
              MidEastNAfrican +
              White +
              BlackAfAmerican == 0, 1, 0),
  WhiteAloneExclusive1 =
    if_else(White == 1 &
              AmIndAKNative +
              Asian +
              MidEastNAfrican +
              NativeHIPacific +
              HispanicLatinaeo +
              BlackAfAmerican == 0, 1, 0),
  WhiteLatineExclusive1 =
    if_else(White == 1 & HispanicLatinaeo == 1 &
              AmIndAKNative +
              Asian +
              MidEastNAfrican +
              NativeHIPacific +
              BlackAfAmerican == 0, 1, 0),
  MultipleNotLatineExclusive1 =
    if_else(HispanicLatinaeo == 0 &
              AmIndAKNative +
              Asian +
              MidEastNAfrican +
              NativeHIPacific +
              White +
              BlackAfAmerican > 1, 1, 0),
  MultipleLatineExclusive1 =
    if_else(HispanicLatinaeo == 1 &
              AmIndAKNative +
              Asian +
              MidEastNAfrican +
              NativeHIPacific +
              White +
              BlackAfAmerican > 1, 1, 0),
  RaceEthnicityNoneExclusive =
    if_else(
      HispanicLatinaeo +
        AmIndAKNative +
        Asian +
        MidEastNAfrican +
        NativeHIPacific +
        White +
        BlackAfAmerican == 0,
      1,
      0
    ),
  # Data quality column to check for mutual exclusivity
  DQExclusive1RaceEth =
    AmIndAKNativeAloneExclusive1 +
    AmIndAKNativeLatineExclusive1 +
    AsianAloneExclusive1 +
    AsianLatineExclusive1 +
    BlackAfAmericanAloneExclusive1 +
    BlackAfAmericanLatineExclusive1 +
    LatineAloneExclusive1 +
    MENAAloneExclusive1 +
    MENALatineExclusive1 +
    NativeHIPacificAloneExclusive1 +
    NativeHIPacificLatineExclusive1 +
    WhiteAloneExclusive1 +
    WhiteLatineExclusive1 +
    MultipleNotLatineExclusive1 +
    MultipleLatineExclusive1 +
    RaceEthnicityNoneExclusive, # all should equal 1
  # exclusive logic group 2
  BILPOCExclusive2 = if_else(
    AmIndAKNative +
      Asian +
      MidEastNAfrican +
      NativeHIPacific +
      HispanicLatinaeo +
      BlackAfAmerican > 0, 1, 0
  ),
  WhiteExclusive2 = if_else(
    White == 1 &
      AmIndAKNative +
      Asian +
      MidEastNAfrican +
      NativeHIPacific +
      HispanicLatinaeo +
      BlackAfAmerican == 0, 1, 0
  ),
  # Data quality check for exclusive group 2
  DQRaceEthExclusive2 =
    BILPOCExclusive2 +
    WhiteExclusive2 +
    RaceEthnicityNoneExclusive, # all rows should equal 1
  # inclusive logic group 1
  AmIndAKNativeInclusive1 = if_else(AmIndAKNative == 1, 1, 0),
  AsianInclusive1 = if_else(Asian == 1, 1, 0),
  BlackAfAmericanInclusive1 = if_else(BlackAfAmerican == 1, 1, 0),
  LatineInclusive1 = if_else(HispanicLatinaeo == 1, 1, 0),
  MENAInclusive1 = if_else(MidEastNAfrican == 1, 1, 0),
  NativeHIPacificInclusive1 = if_else(NativeHIPacific == 1, 1, 0),
  WhiteInclusive1 = if_else(White == 1, 1, 0),
  # catches missings, any methodology any group
  RaceEthnicityNone = if_else(
    AmIndAKNative +
      Asian +
      BlackAfAmerican +
      NativeHIPacific +
      White +
      MidEastNAfrican +
      HispanicLatinaeo == 0, 1, 0),
  # inclusive logic group 2
  BlackAfAmericanLatineInclusive2 =
    if_else(BlackAfAmerican == 1 & HispanicLatinaeo == 1, 1, 0),
  LatineInclusive2 = if_else(HispanicLatinaeo == 1, 1, 0),
  LatineAloneInclusive2 = if_else(
    HispanicLatinaeo == 1 &
      AmIndAKNative +
      Asian +
      NativeHIPacific +
      White +
      MidEastNAfrican +
      BlackAfAmerican == 0, 1, 0
  )) %>%
  select(-all_of(gender_cols), -all_of(race_cols))

browser()

client_categories_reactive <- reactive({
  
  exclusive_methodology <- client_categories %>%
    select(PersonalID,
           VeteranStatus,
           ends_with("Exclusive"),
           ends_with("Exclusive1"),
           ends_with("Exclusive2")
           ) %>%
    left_join(system_person_ages, join_by(PersonalID)) %>%
    mutate(AgeCategory = if_else(is.na(AgeCategory), "Unknown", AgeCategory)) %>%
    filter(AgeCategory %in% input$syso_age &
             input$methodology_type == 1 &
             if_any(.cols = c(input$syso_gender), ~isTruthy(.)) &
             if_any(.cols = c(input$syso_race_ethnicity), ~isTruthy(.)) &
             ((input$syso_spec_pops == "Veteran" &
                VeteranStatus == 1) |
             (input$syso_spec_pops == "NonVeteran" &
                VeteranStatus == 0) |
               input$syso_spec_pops == "None"))
  
  inclusive_methodology <- client_categories %>%
    select(PersonalID,
           VeteranStatus,
           ends_with("Inclusive"),
           ends_with("Inclusive1"),
           ends_with("Inclusive2")
    ) %>%
    left_join(system_person_ages, join_by(PersonalID)) %>%
    mutate(AgeCategory = if_else(is.na(AgeCategory), "Unknown", AgeCategory)) %>%
    filter(AgeCategory %in% input$syso_age &
             input$methodology_type == 2 &
             if_any(.cols = c(input$syso_gender), ~isTruthy(.)) &
             if_any(.cols = c(input$syso_race_ethnicity), ~isTruthy(.)) &
             ((input$syso_spec_pops == "Veteran" &
                 VeteranStatus == 1) |
                (input$syso_spec_pops == "NonVeteran" &
                   VeteranStatus == 0) |
                input$syso_spec_pops == "None"))
  
  if_else(input$methodology_type == 1,
          exclusive_methodology,
          inclusive_methodology)
})

# Enrollment-level reactive -----------------------------------------------

enrollment_categories_reactive <- reactive({
  
  # Filter enrollments by hhtype, project type, and level-of-detail inputs
  x <- enrollment_categories_dt[
      (input$syso_hh_types == "All Households" |
         HouseholdType == input$syso_hh_type) &
      (input$syso_level_of_detail == "All People" |
         (input$syso_level_of_detail == "All Adults and Heads of Households" &
            (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
         (input$syso_level_of_detail == "All Heads of Households" &
            CorrectedHoH == 1)) &
      (input$syso_project_type == "All Project Types" |
         (input$syso_project_type == "Residential Project Types" &
            ProjectType %in% project_types_w_beds) |
         (input$syso_project_type == "Non-Residential Project Types" &
            ProjectType %in% non_res_project_types))
  ]
  
  x
  
})

# Client-level reactive ---------------------------------------------------
# get filtered people-level system dataframe
clients_enrollments_reactive <- reactive({
  enrollment_categories_reactive() %>%
    filter(in_date_range == TRUE) %>%
    inner_join(client_categories_reactive(), join_by(PersonalID))
})

system_df_people_syso_filtered <- reactive({
  clients_enrollments_reactive() %>%
    filter(
      # Age
      (
        setequal(syso_age_cats, input$syso_age) |
          is.null(input$syso_age) |
          AgeCategory%in% input$syso_age
      ) &
      # Special Populations
      (
        input$syso_spec_pops == 1 | # no special populations (all)
        
        # # People
        # input$syso_level_of_detail %in% c(1,2) & (
        #   input$syso_spec_pops == 2 & TRUE
        # ) |
        # # Households
        # !(input$syso_level_of_detail %in% c(1,2)) & (
        #   input$syso_spec_pops == 2 & TRUE
        # )
        (input$syso_spec_pops == 2 &
           DomesticViolenceSurvivor == 1 &
           CurrentlyFleeing == 0) |
        (input$syso_spec_pops == 3 &
           DomesticViolenceSurvivor == 1 &
           CurrentlyFleeing == 1) |
        (input$syso_spec_pops == 4 &
           DomesticViolenceSurvivor == 1)
      ) &
      # Gender
      (
        setequal(syso_gender_cats(), input$syso_gender) |
          is.null(input$syso_gender) |
          GenderCategory %in% input$syso_gender
      ) &
      # Race/Ethnicity
      (
        input$syso_race_ethnicity == 0 |
        AllRaceEthnicity %in% input$syso_race_ethnicity | 
        GroupedRaceEthnicity %in% input$syso_race_ethnicity 
      )
    ) %>%
    select(PersonalID) %>% 
    unique()
})

# Client-level enrollment summary data reactive ---------------------------
# get final people-level, inflow/outflow dataframe by joining the filtered 
# enrollment and people dfs, as well as flagging their inflow and outflow types
sys_inflow_outflow_plot_df <- reactive({
  # add inflow type and active enrollment typed used for system overview plots
  universe <- system_df_enrl_filtered() %>%
    inner_join(system_df_people_syso_filtered(), join_by(PersonalID)) %>%
    # get rid of rows where the enrollment is neither a lookback enrollment,
    # an eecr, or an lecr. So, keeping all lookback records plus the eecr and lecr 
    filter(!(lookback == 0 & eecr == FALSE & lecr == FALSE)) %>%
    mutate(
      # INFLOW CALCULATOR COLUMNS
      #LOGIC: active homeless at start
        # basically it has to straddle report start
          # the entry date of the EECR needs to be on or before the report date range
          # the exitadjust has to be after report start
      # OR the eecr & lookback1 have to end and start within 14 days of each
      # other and of the report start
        # EnrolledHomeless status of the EECR needs to be true
          # JUST FOR FULL DISCLOSURE, this means: 
            # ProjectType %in% project_types_enrolled_homeless |
            # lh_prior_livingsituation == TRUE

      active_at_start_homeless =
        eecr == TRUE &
        (
          # PH project types have move-in after start (or no move-in)
          (
            ProjectType %in% ph_project_types &
            (
              is.na(MoveInDateAdjust) |
              MoveInDateAdjust > ReportStart
            )
          ) |
          # Otherwise, EnrolledHomeless = TRUE (includes 0,1,2,4,8,14 and 6,11)
          (
            ProjectType == ce_project_type &
            lh_prior_livingsituation == TRUE &
              between(EntryDate,
                      ReportStart - days(90),
                      ReportStart + days(90))
          ) |
          # Otherwise, EnrolledHomeless = TRUE (includes 0,1,2,4,8,14 and 6,11)
          (
            !(ProjectType %in% ph_project_types) &
            EnrolledHomeless == TRUE
          )
        ) &
        # Enrollment straddles start or the enrollment is within 2 weeks from start
        # and within 2 weeks of prev enrollment
        (straddles_start == TRUE |
           (EntryDate >= ReportStart &
              between(difftime(EntryDate, ReportStart,
                               units = "days"),
                      0,
                      14) &
              !is.na(days_since_previous_exit) &
              between(as.numeric(days_since_previous_exit), 0, 14))),
      
      #LOGIC: enrolled housed at start
      # Exit.ExitDate is null or > ReportStartDate AND
    
      # Project.ProjectType IN (3, 9, 10, 13) AND
      # Enrollment.MoveInDate is !NULL OR <= ReportStartDate AND
      # Enrollment.LivingSituation is LiterallyHomeless*"
      active_at_start_housed = eecr == TRUE & 
        ProjectType %in% ph_project_types & 
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust <= ReportStart &
        lh_prior_livingsituation == TRUE,
      
      # LOGIC helper columns for outflow
      
      lookback1_perm_dest = lookback == 1 & 
        Destination %in% perm_destinations,
      
      eecr_lh_at_entry = eecr == TRUE &
        lh_at_entry == TRUE,
      
      at_least_14_days_to_eecr_enrl = lookback == 1 & 
        days_to_next_entry >= 14,
      
      lookback1_temp_dest = lookback == 1 & 
        !(Destination %in% perm_destinations),
      
      # outflow columns
      perm_dest_lecr = lecr == TRUE &
        Destination %in% perm_destinations &
        ExitAdjust < ReportEnd, # 
      
      temp_dest_lecr = lecr == TRUE &
        !(Destination %in% perm_destinations) &
        ExitAdjust < ReportEnd,
      
      homeless_at_end = lecr == TRUE & # REVISIT GD, CHECK LOGIC
        EntryDate <= ReportEnd &
        ExitAdjust > ReportEnd & 
        ( # 1
          ProjectType %in% lh_project_types_nc |
            
          # 2
          (ProjectType == es_nbn_project_type & nbn_service_within15_end == TRUE) |
         
          # 3
          (ProjectType == out_project_type & 
            EnrollmentID %in% outreach_w_proper_cls()) |
          
          # 4
          (ProjectType %in% ph_project_types &
          (is.na(MoveInDateAdjust) | MoveInDateAdjust >= ReportEnd) &
          lh_prior_livingsituation == TRUE) 
        ),

      housed_at_end = lecr == TRUE & 
        EntryDate <= ReportEnd &
        ExitAdjust > ReportEnd &
        ProjectType %in% ph_project_types & 
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust <= ReportEnd &
        lh_prior_livingsituation == TRUE
    )
  
  universe_ppl <- universe %>%
    group_by(PersonalID) %>%
    # filter(max(lecr) == 1 & max(eecr) == 1) %>%
    summarise(
      # INFLOW
      active_at_start_homeless_client = max(active_at_start_homeless),
      
      active_at_start_housed_client = max(active_at_start_housed),
      
      return_from_perm_client = max(lookback1_perm_dest) == 1 & 
        max(eecr_lh_at_entry) == 1 & 
        max(at_least_14_days_to_eecr_enrl) == 1,
      
      reengaged_from_temp_client = max(lookback1_temp_dest) == 1 & 
        max(eecr_lh_at_entry) == 1 & 
        max(at_least_14_days_to_eecr_enrl) == 1,
      
      newly_homeless_client = max(lookback) == 0,
      
      InflowTypeSummary = case_when(
        active_at_start_homeless_client == TRUE |
          active_at_start_housed_client == TRUE ~
          "Active at Start",
        newly_homeless_client == TRUE |
          return_from_perm_client == TRUE |
          reengaged_from_temp_client == TRUE ~
          "Inflow",
        TRUE ~ "something's wrong"
      ),
      
      InflowTypeDetail = case_when(
        active_at_start_homeless_client == TRUE ~ "Homeless",
        active_at_start_housed_client == TRUE ~ "Housed",
        newly_homeless_client == TRUE ~ "Newly Homeless",
        return_from_perm_client == TRUE ~ "Returned from \nPermanent",
        reengaged_from_temp_client == TRUE ~ "Re-engaged from \nNon-Permanent",
        TRUE ~ "something's wrong"
      ),
      
      # OUTFLOW
      perm_dest_client = max(perm_dest_lecr),
      
      temp_dest_client = max(temp_dest_lecr),
      
      homeless_at_end_client = max(homeless_at_end),
      
      housed_at_end_client  = max(housed_at_end),
      
      OutflowTypeSummary = case_when(
        perm_dest_client == TRUE |
          temp_dest_client == TRUE ~
          "Outflow",
        homeless_at_end_client == TRUE |
          housed_at_end_client == TRUE ~
          "Active at End",
        TRUE ~ "something's wrong"
      ),

      OutflowTypeDetail = case_when(
        perm_dest_client == TRUE ~ "Exited to \nPermanent Destination",
        temp_dest_client == TRUE ~ "Exited to \nNon-Permanent Destination",
        homeless_at_end_client == TRUE ~ "Homeless",
        housed_at_end_client == TRUE ~ "Housed",
        TRUE ~ "something's wrong"
      )
    ) %>%
    ungroup() 
  
  plot_data <- universe_ppl %>%
    select(PersonalID, 
           active_at_start_homeless_client, 
           active_at_start_housed_client,
           return_from_perm_client,
           reengaged_from_temp_client,
           InflowTypeSummary,
           InflowTypeDetail,
           perm_dest_client,
           temp_dest_client,
           homeless_at_end_client,
           housed_at_end_client,
           OutflowTypeSummary,
           OutflowTypeDetail
    ) %>%
    unique()
  
  # AS QC check:
  missing_types <- universe %>% 
    inner_join(
      universe_ppl %>% 
        filter(
          OutflowTypeDetail == "something's wrong" | 
            InflowTypeDetail == "something's wrong"), 
      by="PersonalID") %>% 
    mutate(
      missing_inflow = eecr & InflowTypeDetail == "something's wrong",
      missing_outflow = lecr & OutflowTypeDetail == "something's wrong",
    )
  
  category_counts <- plot_data %>% 
    pivot_longer(
      cols = c(InflowTypeDetail, OutflowTypeDetail), 
      names_to = "x.axis.var", 
      values_to = "cat.var") %>%
    group_by(x.axis.var, cat.var) %>%
    summarise(values = n()) %>%
    # filter(!is.na(cat.var)) %>%
    mutate(
      values = ifelse(x.axis.var == "OutflowTypeDetail", values * -1, values),
      inflow_outflow = x.axis.var,
      x.axis.var = case_when(
        x.axis.var == "InflowTypeDetail" &
          cat.var %in% active_at_vals
        ~ active_as_of_start(),
        
        x.axis.var == "OutflowTypeDetail" &
          cat.var %in% active_at_vals
        ~ active_as_of_end(),
        
        x.axis.var == "InflowTypeDetail"
        ~ "Inflow",
        
        x.axis.var == "OutflowTypeDetail"
        ~ "Outflow"
      )
    )
  plot_data
})
# 
# sys_df_people_universe_filtered_r(system_df_people_universe_filtered)
# sys_inflow_outflow_plot_data(sys_inflow_outflow_plot_df)
