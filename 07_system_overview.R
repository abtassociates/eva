logToConsole("Running system overview")

# Age ---------------------------------------------------------------------
EnrollmentAdjustAge <- setDT(EnrollmentAdjust)[
  , AgeAtEntry := fifelse(is.na(AgeAtEntry), -1, AgeAtEntry)
]

system_person_ages <- EnrollmentAdjustAge[
    , .(AgeAtEntry = max(AgeAtEntry, na.rm = TRUE)), by = PersonalID
  ][, AgeCategory := factor(fcase(
      AgeAtEntry < 0, "Unknown",
      between(AgeAtEntry, 0, 12), "0 to 12",
      between(AgeAtEntry, 13, 17), "13 to 17",
      between(AgeAtEntry, 18, 21), "18 to 21",
      between(AgeAtEntry, 22, 24), "22 to 24",
      between(AgeAtEntry, 25, 34), "25 to 34",
      between(AgeAtEntry, 35, 44), "35 to 44",
      between(AgeAtEntry, 45, 54), "45 to 54",
      between(AgeAtEntry, 55, 64), "55 to 64",
      between(AgeAtEntry, 65, 74), "65 to 74",
      AgeAtEntry >= 75, "75 and older",
      default = "Unknown"),
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
    ))
  ][, .(PersonalID, MostRecentAgeAtEntry = AgeAtEntry, AgeCategory)]


# Client-level flags ------------------------------------------------------
# will help us categorize people for filtering

session$userData$client_categories <- as.data.table(Client %>%
  left_join(system_person_ages, join_by(PersonalID)) %>%
  select(PersonalID,
         all_of(race_cols),
         all_of(gender_cols),
         VeteranStatus,
         AgeCategory
  ) %>%
  mutate(
    VeteranStatus = if_else(VeteranStatus == 1 &
                              !is.na(VeteranStatus), 1, 0),
    # flattening data and eliminating nulls in case they're present
    Woman = if_else(Woman == 1 & !is.na(Woman), 1, 0),
    Man = if_else(Man == 1 & !is.na(Man), 1, 0),
    NonBinary = if_else(NonBinary == 1 & !is.na(NonBinary), 1, 0),
    Transgender = if_else(Transgender == 1 & !is.na(Transgender), 1, 0),
    CulturallySpecific =
      if_else(CulturallySpecific == 1 & !is.na(CulturallySpecific), 1, 0),
    DifferentIdentity =
      if_else(DifferentIdentity == 1 & !is.na(DifferentIdentity), 1, 0),
    Questioning = if_else(Questioning == 1 & !is.na(Questioning), 1, 0),
    # exclusive logic
    TransgenderMethod1 = if_else(Transgender == 1, 1, 0),
    GenderExpansiveMethod1 = if_else(
      Transgender == 0 &
        (CulturallySpecific + NonBinary + DifferentIdentity + Questioning > 0 | 
           (Man == 1 & Woman == 1)),
      1,
      0
    ),
    ManMethod1 = if_else(
      Man == 1 &
        CulturallySpecific +
        NonBinary +
        DifferentIdentity +
        Questioning +
        Woman +
        Transgender == 0, 1, 0),
    WomanMethod1 = if_else(
      Woman == 1 &
        CulturallySpecific +
        NonBinary +
        DifferentIdentity +
        Questioning +
        Man +
        Transgender == 0, 1, 0),
    GenderUnknown = if_else(
      Woman +
        CulturallySpecific +
        NonBinary +
        DifferentIdentity +
        Questioning +
        Man +
        Transgender == 0, 1, 0),
    DQMethod1 = TransgenderMethod1 + GenderExpansiveMethod1 + ManMethod1 +
      WomanMethod1 + GenderUnknown, # all values should = 1
    # Method2 logic
    TransgenderMethod2 = if_else(
      Transgender == 1 |
        (Woman == 1 & Man == 1) |
        CulturallySpecific + NonBinary + DifferentIdentity + Questioning > 0, 1, 0),
    WomanMethod2 = if_else(Woman == 1, 1, 0),
    ManMethod2 = if_else(Man == 1, 1, 0),
    WomanOrManOnlyMethod2 = if_else ((
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
    NonBinaryMethod2 = if_else(NonBinary == 1, 1, 0),
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
    AmIndAKNativeAloneMethod1Detailed = 
      if_else(AmIndAKNative == 1 &
                Asian +
                BlackAfAmerican +
                NativeHIPacific +
                White +
                MidEastNAfrican +
                HispanicLatinaeo == 0, 1, 0),
    AmIndAKNativeLatineMethod1Detailed = 
      if_else(AmIndAKNative == 1 & HispanicLatinaeo == 1 &
                Asian +
                BlackAfAmerican +
                NativeHIPacific +
                White +
                MidEastNAfrican == 0, 1, 0),
    AsianAloneMethod1Detailed =
      if_else(Asian == 1 &
                AmIndAKNative +
                BlackAfAmerican +
                NativeHIPacific +
                White +
                MidEastNAfrican +
                HispanicLatinaeo == 0, 1, 0),
    AsianLatineMethod1Detailed =
      if_else(Asian == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                BlackAfAmerican +
                NativeHIPacific +
                White +
                MidEastNAfrican == 0, 1, 0),
    BlackAfAmericanAloneMethod1Detailed =
      if_else(BlackAfAmerican == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                MidEastNAfrican +
                HispanicLatinaeo == 0, 1, 0),
    BlackAfAmericanLatineMethod1Detailed =
      if_else(BlackAfAmerican == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                MidEastNAfrican == 0, 1, 0),
    LatineAloneMethod1Detailed =
      if_else(HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                MidEastNAfrican +
                BlackAfAmerican == 0, 1, 0),
    MidEastNAfricanAloneMethod1Detailed =
      if_else(MidEastNAfrican == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                HispanicLatinaeo +
                BlackAfAmerican == 0, 1, 0),
    MidEastNAfricanLatineMethod1Detailed =
      if_else(MidEastNAfrican == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                BlackAfAmerican == 0, 1, 0),
    NativeHIPacificAloneMethod1Detailed =
      if_else(NativeHIPacific == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                White +
                HispanicLatinaeo +
                BlackAfAmerican == 0, 1, 0),
    NativeHIPacificLatineMethod1Detailed =
      if_else(NativeHIPacific == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                White +
                BlackAfAmerican == 0, 1, 0),
    WhiteAloneMethod1Detailed =
      if_else(White == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                NativeHIPacific +
                HispanicLatinaeo +
                BlackAfAmerican == 0, 1, 0),
    WhiteLatineMethod1Detailed =
      if_else(White == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                NativeHIPacific +
                BlackAfAmerican == 0, 1, 0),
    MultipleNotLatineMethod1Detailed =
      if_else(HispanicLatinaeo == 0 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                NativeHIPacific +
                White +
                BlackAfAmerican > 1, 1, 0),
    MultipleLatineMethod1Detailed =
      if_else(HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                NativeHIPacific +
                White +
                BlackAfAmerican > 1, 1, 0),
    RaceEthnicityUnknown =
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
    DQMethod1DetailedRaceEth =
      AmIndAKNativeAloneMethod1Detailed +
      AmIndAKNativeLatineMethod1Detailed +
      AsianAloneMethod1Detailed +
      AsianLatineMethod1Detailed +
      BlackAfAmericanAloneMethod1Detailed +
      BlackAfAmericanLatineMethod1Detailed +
      LatineAloneMethod1Detailed +
      MidEastNAfricanAloneMethod1Detailed +
      MidEastNAfricanLatineMethod1Detailed +
      NativeHIPacificAloneMethod1Detailed +
      NativeHIPacificLatineMethod1Detailed +
      WhiteAloneMethod1Detailed +
      WhiteLatineMethod1Detailed +
      MultipleNotLatineMethod1Detailed +
      MultipleLatineMethod1Detailed +
      RaceEthnicityUnknown, # all should equal 1
    # exclusive logic group 2
    BILPOCMethod1Summarized = if_else(
      AmIndAKNative +
        Asian +
        MidEastNAfrican +
        NativeHIPacific +
        HispanicLatinaeo +
        BlackAfAmerican > 0, 1, 0
    ),
    WhiteMethod1Summarized = if_else(
      White == 1 &
        AmIndAKNative +
        Asian +
        MidEastNAfrican +
        NativeHIPacific +
        HispanicLatinaeo +
        BlackAfAmerican == 0, 1, 0
    ),
    # Data quality check for exclusive group 2
    DQRaceEthMethod1Summarized =
      BILPOCMethod1Summarized +
      WhiteMethod1Summarized +
      RaceEthnicityUnknown, # all rows should equal 1
    # Method2 logic group 1
    AmIndAKNativeMethod2Detailed = if_else(AmIndAKNative == 1, 1, 0),
    AsianMethod2Detailed = if_else(Asian == 1, 1, 0),
    BlackAfAmericanMethod2Detailed = if_else(BlackAfAmerican == 1, 1, 0),
    LatineMethod2Detailed = if_else(HispanicLatinaeo == 1, 1, 0),
    MidEastNAfricanMethod2Detailed = if_else(MidEastNAfrican == 1, 1, 0),
    NativeHIPacificMethod2Detailed = if_else(NativeHIPacific == 1, 1, 0),
    WhiteMethod2Detailed = if_else(White == 1, 1, 0),
    # catches missings, any methodology any group
    # RaceEthnicityNone = if_else(
    #   AmIndAKNative +
    #     Asian +
    #     BlackAfAmerican +
    #     NativeHIPacific +
    #     White +
    #     MidEastNAfrican +
    #     HispanicLatinaeo == 0, 1, 0),
    # Method2 logic group 2
    BlackAfAmericanLatineMethod2Summarized =
      if_else(BlackAfAmerican == 1 & HispanicLatinaeo == 1, 1, 0),
    LatineMethod2Summarized = if_else(HispanicLatinaeo == 1, 1, 0),
    LatineAloneMethod2Summarized = if_else(
      HispanicLatinaeo == 1 &
        AmIndAKNative +
        Asian +
        NativeHIPacific +
        White +
        MidEastNAfrican +
        BlackAfAmerican == 0, 1, 0
    )) %>%
  select(-all_of(gender_cols), -all_of(race_cols))

# Client-level flags, filtered ----------------------------------------------------
client_categories_filtered <- reactive({
  req(nrow(client_categories) > 0)
  setDT(client_categories)[
    , All := 1
  ][AgeCategory %in% input$syso_age &
    get(input$syso_gender) == 1 &
    get(input$syso_race_ethnicity) == 1 &
    (
      input$syso_spec_pops == "None" |
        (input$syso_spec_pops == "Veteran" &
           VeteranStatus == 1 & !(AgeCategory %in% c("0 to 12", "13 to 17"))) |
        (input$syso_spec_pops == "NonVeteran" &
           VeteranStatus == 0 & !(AgeCategory %in% c("0 to 12", "13 to 17")))
    )
  ]
})

# Data prep ---------------------------------------------------------------

# using EnrollmentAdjust because that df doesn't contain enrollments that fall
# outside periods of operation/participation
enrollment_prep <- EnrollmentAdjustAge %>%
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
         DisablingCondition
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
  # left_join(HealthAndDV %>%
  #             filter(DataCollectionStage == 1) %>%
  #             select(EnrollmentID, DomesticViolenceSurvivor, CurrentlyFleeing),
  #           by = "EnrollmentID") %>%
  left_join(system_person_ages, join_by(PersonalID)) %>%
  filter(ContinuumProject == 1 & EntryDate < coalesce(ExitDate, no_end_date)) %>%
  select(-ContinuumProject)
# IMPORTANT: ^ same granularity as EnrollmentAdjust! A @TEST here might be to
# check that
# enrollment_prep %>%
#   nrow() == EnrollmentAdjust %>% filter(ContinuumProject == 1) %>% nrow()
# This aims to add demographic data that lives in various other tables added
# to the enrollment data *without changing the granularity*

# corrected hohs ----------------------------------------------------------

# preps household data to match the way we need the app to 

hh_adjustments <- as.data.table(enrollment_prep)[, `:=`(
  HoHAlready = fifelse(RelationshipToHoH == 1 & AgeAtEntry > 17, 1, 0)
)][order(-HoHAlready, -AgeAtEntry, PersonalID), 
   `:=`
   (
     CorrectedHoH = ifelse(seq_len(.N) == 1, 1, 0),
     max_AgeAtEntry = max(AgeAtEntry),
     min_AgeAtEntry = min(AgeAtEntry)
   ),
   by = .(HouseholdID, ProjectID)
][, HouseholdType := factor(
  fifelse(
    any(between(AgeAtEntry, 0, 17)) & max_AgeAtEntry >= 18,
    fifelse(
      between(max_AgeAtEntry, 0, 24),
      "PY",
      "AC"
    ),
    fifelse(
      min_AgeAtEntry >= 18,
      fifelse(
        between(max_AgeAtEntry, 0, 24),
        "UY", # UY = Unaccompanied Youth. YYA = PY + UY + CO
        "AO"
      ),
      fifelse(
        min_AgeAtEntry >= 0 & max_AgeAtEntry <= 17,
        "CO", 
        "UN"
      )
    )
  ),
  levels = c("AO", "AC", "CO", "UN", "PY", "UY")
), by = HouseholdID][
  , .(EnrollmentID, CorrectedHoH, HouseholdType)
]

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
enrollment_categories <- as.data.table(enrollment_prep_hohs)[, `:=`(
  ProjectTypeWeight = fcase(
    ProjectType %in% ph_project_types & !is.na(MoveInDateAdjust), 100,
    ProjectType %in% ph_project_types & is.na(MoveInDateAdjust), 80,
    ProjectType %in% lh_residential_project_types, 60,
    ProjectType %in% non_res_project_types, 40,
    default = 20
  ),
  lh_prior_livingsituation = !is.na(LivingSituation) &
    (LivingSituation %in% homeless_livingsituation_incl_TH |
       (LivingSituation %in% institutional_livingsituation &
          LOSUnderThreshold == 1 & PreviousStreetESSH == 1 &
          !is.na(LOSUnderThreshold) & !is.na(PreviousStreetESSH)))
  )][, `:=`(
    lh_at_entry = lh_prior_livingsituation | ProjectType %in% lh_project_types,
    EnrolledHomeless = ProjectType %in% project_types_enrolled_homeless |
      lh_prior_livingsituation
  )][
    ProjectType != hp_project_type & 
    EntryDate <= session$userData$ReportEnd & ExitAdjust >= (session$userData$ReportStart - years(2))
  ][, .(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust,
    ProjectType,
    MostRecentAgeAtEntry,
    lh_prior_livingsituation,
    lh_at_entry,
    EnrolledHomeless,
    LivingSituation,
    LOSUnderThreshold,
    PreviousStreetESSH,
    Destination,
    AgeAtEntry,
    CorrectedHoH,
    # DomesticViolenceCategory,
    HouseholdType,
    ProjectTypeWeight
  )
]
setkey(enrollment_categories, EnrollmentID)
setindex(enrollment_categories, PersonalID, ProjectType)

# Prepare a dataset of literally homeless CLS records, along with EntryDate and ProjectType. 
# This partially replaces the old homeless_cls_finder function
# (which required filtering the same way for every record, so now we're doing more work once)
# by casting a wide net for (Non-Res) Project Types that rely on CurrentLivingSituation 
# this dataset will be used to categorize people as active_at_start, homeless_at_end, and unknown_at_end
# it's also used in determining EECR and LECR (see 07_system_overview_period_specific_prep.R)
lh_cls <- as.data.table(CurrentLivingSituation)[
  CurrentLivingSituation %in% homeless_livingsituation_incl_TH
][
  enrollment_categories[ProjectType %in% non_res_project_types], 
  on = .(EnrollmentID),
  .(
    EnrollmentID, PersonalID, EntryDate, ProjectType, lh_prior_livingsituation, ExitAdjust, InformationDate, CurrentLivingSituation,
    info_equal_entry = InformationDate == EntryDate,
    info_equal_exit = InformationDate == ExitAdjust
  ),
  nomatch = NULL
]


# Remove "problematic" enrollments ----------------------------------
# These are enrollments that entered the system as not LH
# but then have a CLS later in the enrollment
# two ways to identify "not LH" for their entry enrollment (eecr): 
# 1. !lh_prior_livingsituation and 
# 2. no lh cls where InformationDate == EntryDate
entered_not_lh_but_lh_cls_later <- enrollment_categories[
  EntryDate <= session$userData$ReportEnd & ExitAdjust >= session$userData$ReportStart &
    ProjectType %in% non_res_project_types
][, 
  first_enrollment := min(EntryDate), 
  by = PersonalID
][
  !(EntryDate == first_enrollment & lh_prior_livingsituation)
][
  lh_cls[,
         has_lh_at_entry := any(InformationDate == EntryDate),
         by = .(EnrollmentID)
  ][has_lh_at_entry == FALSE],
  on = .(EnrollmentID),
  nomatch = NULL
]

enrollment_categories <- enrollment_categories[
  !entered_not_lh_but_lh_cls_later, on=.(PersonalID)
]

source("07_system_overview_period_specific_prep.R", local=TRUE)
session$userData$report_dates <- get_report_dates()

source("07_system_overview_plot_data_prep.R", local=TRUE)