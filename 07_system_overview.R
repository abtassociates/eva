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

client_categories <- Client %>%
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
    GenderUnknown = if_else(
      Woman +
        CulturallySpecific +
        NonBinary +
        DifferentIdentity +
        Questioning +
        Man +
        Transgender == 0, 1, 0),
    DQExclusive = TransgenderExclusive + GenderExpansiveExclusive + ManExclusive +
      WomanExclusive + GenderUnknown, # all values should = 1
    # inclusive logic
    TransgenderInclusive = if_else(
      Transgender == 1 |
        (Woman == 1 & Man == 1) |
        CulturallySpecific + NonBinary + DifferentIdentity + Questioning > 0, 1, 0),
    WomanInclusive = if_else(Woman == 1, 1, 0),
    ManInclusive = if_else(Man == 1, 1, 0),
    WomanOrManOnlyInclusive = if_else ((
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
      RaceEthnicityUnknown, # all should equal 1
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
      RaceEthnicityUnknown, # all rows should equal 1
    # inclusive logic group 1
    AmIndAKNativeInclusive1 = if_else(AmIndAKNative == 1, 1, 0),
    AsianInclusive1 = if_else(Asian == 1, 1, 0),
    BlackAfAmericanInclusive1 = if_else(BlackAfAmerican == 1, 1, 0),
    LatineInclusive1 = if_else(HispanicLatinaeo == 1, 1, 0),
    MENAInclusive1 = if_else(MidEastNAfrican == 1, 1, 0),
    NativeHIPacificInclusive1 = if_else(NativeHIPacific == 1, 1, 0),
    WhiteInclusive1 = if_else(White == 1, 1, 0),
    # catches missings, any methodology any group
    # RaceEthnicityNone = if_else(
    #   AmIndAKNative +
    #     Asian +
    #     BlackAfAmerican +
    #     NativeHIPacific +
    #     White +
    #     MidEastNAfrican +
    #     HispanicLatinaeo == 0, 1, 0),
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

# Client-level flags, filtered ----------------------------------------------------
client_categories_filtered <- reactive({
  client_categories %>%
    mutate(All = 1) %>%
    filter(
      AgeCategory %in% input$syso_age &
        !!sym(input$syso_gender) == 1 &
        !!sym(input$syso_race_ethnicity) == 1 &
        (
          input$syso_spec_pops == "None" |
            (input$syso_spec_pops == "Veteran" &
               VeteranStatus == 1 & !(AgeCategory %in% c("0 to 12", "13 to 17"))) |
            (input$syso_spec_pops == "NonVeteran" &
               VeteranStatus == 0 & !(AgeCategory %in% c("0 to 12", "13 to 17")))
        )
    )  
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
  inner_join(client_categories_filtered() %>%
               select(PersonalID, VeteranStatus), by = "PersonalID") %>%
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
)][order(-HoHAlready, -VeteranStatus, -AgeAtEntry, PersonalID), 
   `:=`
   (
     Sequence = seq_len(.N),
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
)]

source("07_system_overview_period_specific_prep.R", local=TRUE)
source("07_system_overview_plot_data_prep.R", local=TRUE)