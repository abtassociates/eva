logToConsole("Running system overview")

# Age ---------------------------------------------------------------------
EnrollmentAdjustAge <- as.data.table(EnrollmentAdjust)[
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
  VeteranStatus = fifelse(VeteranStatus == 1 & !is.na(VeteranStatus), 1, 0),
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


# NbN prep ----------------------------------------------------------------

nbn_enrollments_services <- Services %>%
  filter(RecordType == 200) %>%
  inner_join(
    EnrollmentAdjust %>%
      filter(ProjectType == es_nbn_project_type) %>%
      select(EnrollmentID),
    join_by(EnrollmentID)
  ) %>%
  # ^ limits shelter night services to enrollments associated to NbN shelters
  mutate(
    NbN15DaysBeforeReportStart =
      between(DateProvided,
              ReportStart() - days(15),
              ReportStart()),
    NbN15DaysAfterReportEnd =
      between(DateProvided,
              ReportEnd(),
              ReportEnd() + days(15)),
    NbN15DaysBeforeReportEnd =
      between(DateProvided,
              ReportEnd() - days(15),
              ReportEnd())
  )

if(nbn_enrollments_services %>% nrow() > 0) nbn_enrollments_services <-
  nbn_enrollments_services %>%
  group_by(EnrollmentID) %>%
  summarise(
    NbN15DaysBeforeReportStart = max(NbN15DaysBeforeReportStart, na.rm = TRUE),
    NbN15DaysAfterReportEnd = max(NbN15DaysAfterReportEnd, na.rm = TRUE),
    NbN15DaysBeforeReportEnd = max(NbN15DaysBeforeReportEnd, na.rm = TRUE)) %>%
  mutate(
    NbN15DaysBeforeReportStart = replace_na(NbN15DaysBeforeReportStart, 0),
    NbN15DaysAfterReportEnd = replace_na(NbN15DaysAfterReportEnd, 0),
    NbN15DaysBeforeReportEnd = replace_na(NbN15DaysBeforeReportEnd, 0)
  ) %>%
  ungroup()

nbn_enrollments_services <- nbn_enrollments_services %>%
  select(EnrollmentID,
         NbN15DaysBeforeReportStart,
         NbN15DaysAfterReportEnd,
         NbN15DaysBeforeReportEnd) %>%
  filter(NbN15DaysBeforeReportStart == 1 |
           NbN15DaysAfterReportEnd == 1 |
           NbN15DaysBeforeReportEnd == 1)

# homeless cls finder function --------------------------------------------

homeless_cls_finder <- function(date, window = "before", days = 60) {
  
  plus_days <- if_else(window == "before", 0, days)
  minus_days <- if_else(window == "after", 0, days)
  
  CurrentLivingSituation %>%
    filter(
      CurrentLivingSituation %in% homeless_livingsituation_incl_TH &
        between(InformationDate,
                date - days(minus_days),
                date + days(plus_days))
    ) %>%
    pull(EnrollmentID) %>%
    unique()
}
# Enrollment-level flags --------------------------------------------------
# as much wrangling as possible without needing hhtype, project type, and level
# of detail inputs

# using data.table --------------------------------------------------------
get_enrollment_categories <- function(startDate, endDate) {
  enrollment_flags <- as.data.table(enrollment_prep_hohs)[, `:=`(
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
      lh_prior_livingsituation,
    straddles_start = EntryDate <= startDate & ExitAdjust >= startDate,
    straddles_end = EntryDate <= endDate & ExitAdjust >= endDate,
    in_date_range = ExitAdjust >= startDate & EntryDate <= endDate #,
    # DomesticViolenceCategory = fcase(
    #   DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1, "DVFleeing",
    #   DomesticViolenceSurvivor == 1, "DVNotFleeing",
    #   default = "NotDV"
    # )
  )][
    # Apply filtering with efficient conditions
    if(startDate == ReportStart() & endDate == ReportEnd())
    (ReportStart() - years(2)) <= ExitAdjust &
      ProjectType != hp_project_type &
      (ProjectType != ce_project_type |
         (ProjectType == ce_project_type &
          (EnrollmentID %in% homeless_cls_finder(ReportStart(), "before", 90) |
              EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 90) |
            (between(EntryDate, ReportStart() - days(90), ReportStart()) &
              lh_prior_livingsituation) |
            (between(EntryDate, ReportEnd() - days(90), ReportEnd()) &
              lh_prior_livingsituation)))) &
      (!ProjectType %in% c(out_project_type,
          sso_project_type, other_project_project_type, day_project_type) |
        (ProjectType %in% c(out_project_type, sso_project_type,
          other_project_project_type, day_project_type) &
          (EnrollmentID %in% homeless_cls_finder(ReportStart(), "before", 60) |
            EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 60) |
            (between(EntryDate, ReportStart() - days(60), ReportStart()) &
              lh_prior_livingsituation) |
            (between(EntryDate, ReportEnd() - days(60), ReportEnd()) &
              lh_prior_livingsituation)))) else TRUE
  ][, c(
    "EnrollmentID",
    "PersonalID",
    "HouseholdID",
    "EntryDate",
    "MoveInDateAdjust",
    "ExitDate",
    "ExitAdjust",
    "ProjectType",
    "MostRecentAgeAtEntry",
    "lh_prior_livingsituation",
    "lh_at_entry",
    "straddles_start",
    "straddles_end",
    "in_date_range",
    "EnrolledHomeless",
    "LivingSituation",
    "LOSUnderThreshold",
    "PreviousStreetESSH",
    "Destination",
    "AgeAtEntry",
    "CorrectedHoH",
    # "DomesticViolenceCategory",
    "HouseholdType",
    "ProjectTypeWeight"
    ), with = FALSE
  ][
    # Add grouping and ordering steps
    order(EntryDate), `:=`(
      StraddlesStart = .N, MaxProjectTypeStart = max(ProjectTypeWeight)
    ), by = .(PersonalID, straddles_start)
  ][order(EntryDate), `:=`(
      StraddlesEnd = .N, MaxProjectTypeEnd = max(ProjectTypeWeight)
    ), by = .(PersonalID, straddles_end)
  ][order(PersonalID, EntryDate),
    # Add mutations related to overlaps and rank ordering
    `:=`(
      InvolvedInOverlapStart = straddles_start & StraddlesStart > 1,
      InvolvedInOverlapEnd = straddles_end & StraddlesEnd > 1,
      ordinal = seq_len(.N),
      days_to_next_entry = difftime(shift(EntryDate, type = "lead"),
          ExitAdjust, units = "days"),
      days_since_previous_exit = difftime(EntryDate, shift(ExitAdjust), units = "days"),
      next_enrollment_project_type = shift(ProjectType, type = "lead"),
      previous_enrollment_project_type = shift(ProjectType)
    ), by = .(PersonalID)
  ][# AS 9/23/24: We're creating the RankOrder variables and then filtering the 
    # start and ends  all at once, rather than starts first then ends. 
    # The reason is that this correctly handles cases where enrollment B overlaps 
    # with A at the start and C at the end, by dropping B as the later of the
    # "start" overlaps and C as the later of the end ones. As a result the person 
    # does not get an LECR and is therefore dropped from the analysis. 
    # If we handle starts and ends separately, B would be dropped, and then C 
    # would be the LECR.
    order(-ProjectTypeWeight, EntryDate, ExitAdjust), `:=`(
      RankOrderStartOverlaps = rowid(PersonalID, InvolvedInOverlapStart),
      RankOrderEndOverlaps = rowid(PersonalID, InvolvedInOverlapEnd)
  )][
    # Filter out non-overlapping enrollments
    (InvolvedInOverlapStart == FALSE | RankOrderStartOverlaps == 1) &
      (InvolvedInOverlapEnd == FALSE | RankOrderEndOverlaps == 1) &
      (days_to_next_entry < 730 | is.na(days_to_next_entry))
  ][, `:=`(
      lecr = in_date_range & max(ordinal) == ordinal,
      eecr = in_date_range & min(ordinal) == ordinal
    ), by = .(PersonalID, in_date_range)
  ][
    order(PersonalID, in_date_range, -EntryDate, -ExitAdjust),
    lookback := ifelse(in_date_range, 0, seq_len(.N) - fifelse(in_date_range, 1, 0)), 
    by = .(PersonalID)
  ][
    ,AgeAtEntry := NULL
  ]

  merge(
    enrollment_flags, 
    nbn_enrollments_services, 
    by = "EnrollmentID",
    all.x = T
  )[, `:=`(
    NbN15DaysBeforeReportStart = replace_na(NbN15DaysBeforeReportStart, 0),
    NbN15DaysAfterReportEnd = replace_na(NbN15DaysAfterReportEnd, 0),
    NbN15DaysBeforeReportEnd = replace_na(NbN15DaysBeforeReportEnd, 0)
  )][
    order(EnrollmentID)
  ]
}

# for testing dt vs. dplyr
# all.equal(enrollment_categories, enrollment_categories2, check.attributes = FALSE)

# using table.express -----------------------------------------------------

# before_te <- now()
# Left join enrollment_categories on nbn_enrollments_services
# enrollment_categories_exp <- as.data.table(enrollment_categories) %>%
#   table.express::left_join(as.data.table(nbn_enrollments_services), EnrollmentID)
# after_te <- now()

# compare -----------------------------------------------------------------

# after_dt - before_dt
# 
# after_te - before_te

# Client-level flags ------------------------------------------------------
# will help us categorize people for filtering
dv_flag <- function(startDate, endDate) {
  as.data.table(Enrollment)[, .(EnrollmentID, EntryDate, ExitAdjust)][
    as.data.table(HealthAndDV)[DataCollectionStage == 1, .(
      EnrollmentID,
      PersonalID,
      DomesticViolenceSurvivor = fifelse(
        is.na(DomesticViolenceSurvivor),
        0,
        fifelse(DomesticViolenceSurvivor== 1, 1, 0)),
      CurrentlyFleeing = fifelse(
        is.na(CurrentlyFleeing),
        0,
        fifelse(CurrentlyFleeing == 1, 1, 0))
    )],
    on = .(EnrollmentID)
  ][ExitAdjust >= startDate & EntryDate <= endDate, 
    .(DomesticViolenceCategory = 
        fifelse(
          max(DomesticViolenceSurvivor, na.rm = TRUE) == 1 & 
            max(CurrentlyFleeing, na.rm = TRUE) == 1, "DVFleeing",
          fifelse(
            max(DomesticViolenceSurvivor, na.rm = TRUE) == 1, "DVNotFleeing", "NotDV"))
    ), by = PersonalID]
}

client_categories <- function(startDate, endDate) {
  Client %>%
    left_join(system_person_ages, join_by(PersonalID)) %>%
    left_join(as.data.frame(dv_flag(startDate, endDate)), join_by(PersonalID)) %>%
    select(PersonalID,
          all_of(race_cols),
          all_of(gender_cols),
          VeteranStatus,
          AgeCategory,
          DomesticViolenceCategory
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
    select(-all_of(gender_cols), -all_of(race_cols)) %>%
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
            VeteranStatus == 0 & !(AgeCategory %in% c("0 to 12", "13 to 17"))) |
          (DomesticViolenceCategory == input$syso_spec_pops | 
             input$syso_spec_pops == "DVTotal" & DomesticViolenceCategory != "NotDV")
        )
    ) %>%
    select(-All)
}

# Enrollment-level reactive -----------------------------------------------
enrollment_categories <- function(startDate, endDate) {
  # Filter enrollments by hhtype, project type, and level-of-detail inputs
  get_enrollment_categories(startDate, endDate) %>%
    left_join(Client %>% select(PersonalID, VeteranStatus), join_by(PersonalID)) %>%
    filter((input$syso_hh_type == "All" |
            (input$syso_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
            (input$syso_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
            (input$syso_hh_type == "AO" & HouseholdType %in% c("AO","UY")) | 
            (input$syso_hh_type == "AC" & HouseholdType %in% c("AC","PY")) | 
            input$syso_hh_type == HouseholdType
              ) &
      (input$syso_level_of_detail == "All" |
         (input$syso_level_of_detail == "HoHsAndAdults" &
            (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
         (input$syso_level_of_detail == "HoHsOnly" &
            CorrectedHoH == 1)) &
        ((input$syso_project_type == "All" |
           (input$syso_project_type == "Residential" &
              ProjectType %in% project_types_w_beds &
              eecr == TRUE) | eecr == FALSE) |
           ((input$syso_project_type == "NonResidential" &
              ProjectType %in% non_res_project_types &
               eecr == TRUE) | eecr == FALSE)) # &
        # (input$syso_spec_pops %in% c("None", "Veteran", "NonVeteran") |
        #    (input$syso_spec_pops == "DVTotal" & DomesticViolenceCategory != "NotDV") |
        #    (input$syso_spec_pops == "NotDV" & DomesticViolenceCategory == "NotDV") |
        #    (input$syso_spec_pops == DomesticViolenceCategory & (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1))
        #    )
           ) %>%
    select(
      EnrollmentID,
      PersonalID,
      ProjectType,
      EntryDate,
      MoveInDateAdjust,
      ExitAdjust,
      Destination,
      CorrectedHoH,
      MostRecentAgeAtEntry,
      HouseholdType,
      lh_prior_livingsituation,
      lh_at_entry,
      EnrolledHomeless,
      straddles_start,
      in_date_range,
      # DomesticViolenceCategory,
      days_to_next_entry,
      days_since_previous_exit,
      lecr,
      eecr,
      lookback,
      NbN15DaysAfterReportEnd,
      NbN15DaysBeforeReportEnd
    )
}

# Client-level reactive ---------------------------------------------------

# get filtered people-level system dataframe
clients_enrollments <- function(startDate, endDate) {
  merge(
    enrollment_categories(startDate, endDate) %>% select(-MostRecentAgeAtEntry),
    client_categories(startDate, endDate), 
    by = "PersonalID",
    all = TRUE
  )
}


# Enrollment-level universe -----------------------
# only includes people and their lookback thru LECR enrollments

# hello weary traveler amongst these date ranges. you may find it helpful to
# find example clients and their Entry and Exit Dates and enter them into
# https://onlinetools.com/time/visualize-date-intervals <- here.
# add inflow type and active enrollment typed used for system overview plots
universe <- function(startDate, endDate) {
  client_enrollments_reactive_dt <- as.data.table(clients_enrollments(startDate, endDate))
    client_enrollments_reactive_dt[
      !(lookback == 0 & eecr == FALSE & lecr == FALSE),
      order_ees := fifelse(lecr == TRUE, 0, 
                           fifelse(eecr == TRUE, 1, lookback + 1))
    ][
      order(-order_ees), # Equivalent of arrange(desc(order_ees))
      days_to_next_entry := as.numeric(difftime(shift(EntryDate, type = "lead"), 
                                                ExitAdjust, units = "days")),
      by = PersonalID # Equivalent of group_by(PersonalID)
    ][, `:=`(
    # mutate(
      # INFLOW CALCULATOR COLUMNS
      # LOGIC: active homeless at start
        # basically it has to straddle report start
          # the entry date of the EECR needs to be on or before the reporting period
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
              MoveInDateAdjust >= ReportStart()
            )
          ) |
            
          ( # take only ce enrollments where the PLS or the CLS is <= 90 days
            # prior to ReportStart
            if(startDate == ReportStart() & endDate == ReportEnd())  
            ProjectType == ce_project_type &
              (EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 90) |
                 (between(EntryDate, ReportStart() - days(90), ReportStart()) &
                    lh_prior_livingsituation == TRUE)) else TRUE
          ) |
          # take any other enrollments if their PLS was literally homeless
          (
            !(ProjectType %in% ph_project_types) &
            EnrolledHomeless == TRUE
          )
        ) &
        # Enrollment straddles start or the enrollment is within 2 weeks from start
        # and within 2 weeks of prev enrollment
        (straddles_start == TRUE |
           (straddles_start == FALSE &
              EntryDate >= ReportStart() &
              between(difftime(EntryDate, ReportStart(),
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
        MoveInDateAdjust < ReportStart(),
      
      # LOGIC helper columns
      
      lookback1_perm_dest = lookback == 1 & 
        Destination %in% perm_livingsituation,
      
      eecr_lh_at_entry = eecr == TRUE &
        lh_at_entry == TRUE,
      
      at_least_14_days_to_eecr_enrl = lookback == 1 & 
        !is.na(days_to_next_entry) &
        days_to_next_entry >= 14,
      
      lookback1_temp_dest = lookback == 1 & 
        !(Destination %in% perm_livingsituation),
      
      # outflow columns
      perm_dest_lecr = lecr == TRUE &
        Destination %in% perm_livingsituation &
        ExitAdjust <= endDate, 
      
      temp_dest_lecr = lecr == TRUE &
        !(Destination %in% perm_livingsituation) &
        ExitAdjust <= endDate,
      
      homeless_at_end = lecr == TRUE & 
        EntryDate <= ReportEnd() &
        ExitAdjust >= ReportEnd() &
        ( # e/e shelter, th, sh
          ProjectType %in% lh_project_types_nc |
            
            # nbn shelter
            (ProjectType == es_nbn_project_type &
               (in_date_range == TRUE | NbN15DaysAfterReportEnd == TRUE)) |
            
            # outreach, sso, other, day shelter
            (ProjectType %in% c(out_project_type,
                                sso_project_type,
                                other_project_project_type,
                                day_project_type) &
               (EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 60) |
                  (between(EntryDate, ReportEnd() - days(60), ReportEnd()) &
                     lh_prior_livingsituation == TRUE))) |
            
            # CE
            (ProjectType %in% ce_project_type &
               (EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 90) |
                  (between(EntryDate, ReportEnd() - days(90), ReportEnd()) &
                     lh_prior_livingsituation == TRUE)
                )
             ) |
            
            # PSH, OPH, RRH
            (ProjectType %in% ph_project_types &
               (is.na(MoveInDateAdjust) | MoveInDateAdjust >= ReportEnd()))
          ),

      housed_at_end = lecr == TRUE & 
        EntryDate <= ReportEnd() &
        ExitAdjust >= ReportEnd() &
        ProjectType %in% ph_project_types & 
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust < ReportEnd(),
      
      unknown_at_end = lecr == TRUE &
        EntryDate <= ReportEnd() &
        ExitAdjust >= ReportEnd() &
        # outreach, sso, other, day shelter
        ((ProjectType %in% c(out_project_type,
                            sso_project_type,
                            other_project_project_type,
                            day_project_type) &
            !EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 60) &
            (!between(EntryDate, ReportEnd() - days(60), ReportEnd()) |
               lh_prior_livingsituation == FALSE)) |

        # nbn shelter
        (ProjectType == es_nbn_project_type &
          (in_date_range == TRUE | NbN15DaysBeforeReportEnd == FALSE)) |
           
        
        # CE
        (ProjectType %in% ce_project_type &
           !EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 90) &
           (!between(EntryDate, ReportEnd() - days(90), ReportEnd()) |
              lh_prior_livingsituation == FALSE)
           
        ))
    )]
}

# Enrollment-level universe with client-level flags -----------------------
# Need to keep it enrollment-level so other scripts can reference the enrollments
universe_ppl_flags <- function(startDate, endDate) {  
  universe(startDate, endDate)[, {
    lecr_max <- if(all(is.na(lecr))) 0 else max(lecr, na.rm = TRUE)
    eecr_max <- if(all(is.na(eecr))) 0 else max(eecr, na.rm = TRUE)
    
    .SD[lecr_max == 1 & eecr_max == 1]
  }, by = PersonalID][, `:=`(
      # INFLOW
      active_at_start_homeless_client = max(active_at_start_homeless),
      
      active_at_start_housed_client = max(active_at_start_housed),
      
      return_from_perm_client = max(lookback1_perm_dest) == 1 & 
        # max(eecr_lh_at_entry) == 1 & 
        max(at_least_14_days_to_eecr_enrl) == 1,
      
      reengaged_from_temp_client = max(lookback1_temp_dest) == 1 & 
        # max(eecr_lh_at_entry) == 1 & 
        max(at_least_14_days_to_eecr_enrl) == 1,
      
      newly_homeless_client = max(lookback) == 0 |
        max(eecr_lh_at_entry) == 0 | 
        max(at_least_14_days_to_eecr_enrl) == 0
    ), by = PersonalID
  ][, `:=`(
      InflowTypeSummary = fifelse(
        active_at_start_homeless_client == TRUE |
          active_at_start_housed_client == TRUE,
          "Active at Start",
        fifelse(newly_homeless_client == TRUE |
          return_from_perm_client == TRUE |
          reengaged_from_temp_client == TRUE,
          "Inflow",
          "something's wrong"
        )
      ),
      
      InflowTypeDetail = fifelse(
        active_at_start_homeless_client == TRUE, "Homeless",
        fifelse(active_at_start_housed_client == TRUE, "Housed",
        fifelse(return_from_perm_client == TRUE, "Returned from \nPermanent",
        fifelse(reengaged_from_temp_client == TRUE, "Re-engaged from \nNon-Permanent",
        fifelse(newly_homeless_client == TRUE & days_of_data() >= 1094, "First-Time \nHomeless",
        fifelse(newly_homeless_client == TRUE & days_of_data() < 1094, "Inflow\nUnspecified",
        "something's wrong")))))
      ),
      
      # OUTFLOW
      perm_dest_client = max(perm_dest_lecr),
      
      temp_dest_client = max(temp_dest_lecr),
      
      homeless_at_end_client = max(homeless_at_end),
      
      housed_at_end_client = max(housed_at_end),
      
      unknown_at_end_client = max(unknown_at_end)
      
  ), by = PersonalID
  ][, `:=`(
      OutflowTypeSummary = fifelse(
        perm_dest_client == TRUE |
          temp_dest_client == TRUE |
          unknown_at_end_client == TRUE,
          "Outflow",
        fifelse(homeless_at_end_client == TRUE |
          housed_at_end_client == TRUE,
          "Active at End",
          "something's wrong")
      ),

      OutflowTypeDetail = fifelse(
        perm_dest_client == TRUE, "Exited,\nPermanent",
        fifelse(temp_dest_client == TRUE, "Exited,\nNon-Permanent",
        fifelse(unknown_at_end_client == TRUE, "Inactive",
        fifelse(homeless_at_end_client == TRUE, "Homeless",
        fifelse(housed_at_end_client == TRUE, "Housed",
        "something's wrong"))))
      )
    )
  ]
}

# Client-level enrollment summary data reactive ---------------------------
# get final people-level, inflow/outflow dataframe by joining the filtered 
# enrollment and people dfs, as well as flagging their inflow and outflow types
inflow_outflow_df <- reactive({
  
  exportTestValues(universe_ppl_flags = universe_ppl_flags() %>% nice_names())
  
  plot_data <- universe_ppl_flags(ReportStart(), ReportEnd()) %>%
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
           unknown_at_end_client,
           OutflowTypeSummary,
           OutflowTypeDetail
    ) %>%
    unique()
  # AS QC check:
#   missing_types <- universe() %>%
#     inner_join(
#       plot_data %>%
#         filter(
#           OutflowTypeDetail == "something's wrong" |
#             InflowTypeDetail == "something's wrong"),
#       by = "PersonalID") %>%
#     mutate(
#       missing_inflow = eecr == TRUE & InflowTypeDetail == "something's wrong",
#       missing_outflow = lecr == TRUE & OutflowTypeDetail == "something's wrong",
#     ) %>%
#     filter(missing_inflow == TRUE | missing_outflow == TRUE)
#   
# # browser()
#   
#   category_counts <- plot_data %>%
#     select(PersonalID, InflowTypeDetail, OutflowTypeDetail) %>%
#     pivot_longer(
#       cols = c(InflowTypeDetail, OutflowTypeDetail), 
#       names_to = "Time", 
#       values_to = "Status") %>%
#     group_by(Time, Status) %>%
#     summarise(values = n()) %>%
#     ungroup() %>%
#     filter(!is.na(Status)) %>%
#     mutate(
#       values = ifelse(Time == "OutflowTypeDetail", values * -1, values),
#       inflow_outflow = Time,
#       Time = case_when(
#         Time == "InflowTypeDetail" &
#           Status %in% c("Homeless", "Housed")
#         ~ paste0("Active as of \n", ReportStart()),
#         
#         Time == "OutflowTypeDetail" &
#           Status %in% c("Homeless", "Housed")
#         ~ paste0("Active as of \n", ReportEnd()),
#         
#         Time == "InflowTypeDetail"
#         ~ "Inflow",
#         
#         Time == "OutflowTypeDetail"
#         ~ "Outflow"
#       )
#     )
  plot_data
})

# newly_homeless_clients <- plot_data %>%
#   filter(InflowTypeDetail == "Newly Homeless") %>%
#   pull(PersonalID) %>%
#   unique()
# 
# enrollment_categories  %>%
#   group_by(PersonalID) %>%
#   mutate(Count = n()) %>%
#   ungroup() %>%
#   filter(PersonalID %in% c(newly_homeless_clients) & Count > 1) %>%
#   mutate(DestinationDescription = living_situation(Destination),
#          ReportStart = ReportStart(),
#          ReportEnd = ReportEnd(),
#          ExportStart = ExportStartAdjusted,
#          ExportEnd = ExportEndAdjusted,
#          LookbackBegins = ReportStart() - years(2),
#          ProjectType = project_type_abb(ProjectType),
#          LivingSituation = living_situation(LivingSituation)) %>%
#   select(
#     PersonalID,
#     EnrollmentID,
#     ExportStart,
#     LookbackBegins,
#     ReportStart,
#     EntryDate,
#     ExitAdjust,
#     ReportEnd,
#     ExportEnd,
#     ProjectType,
#     LivingSituation,
#     DestinationDescription,
#     days_to_next_entry,
#     days_since_previous_exit,
#     lecr,
#     eecr,
#     lookback
#   ) -> for_review
# 
# write_csv(for_review, here("newly_homeless_20240912a.csv"))

# Monthyl Inflow-Outflows -----------------------------------------------------
sys_act_monthly_enrollment_categories <- reactive({
  start_month <- floor_date(ReportStart(), "month")  # Start 11 months before end
  end_month <- floor_date(ReportEnd(), "month")
  
  # Check if we have at least 12 months of data
  # if (difftime(end_month, start_month, units = "days") < 365) {
  #   return(NULL)
  # }
  
  monthly_dates <- data.frame(
    month_start = seq(start_month, end_month, by = "month"),
    month_end = ceiling_date(seq(start_month, end_month, by = "month"), "month") - days(1)
  )
  
  monthly_results <- lapply(1:nrow(monthly_dates), function(i) {
    month_start <- monthly_dates$month_start[i]
    month_end <- monthly_dates$month_end[i]
    result <- universe_ppl_flags(month_start, month_end)
    result[, month := month_start]
    return(result)
  })
  
  rbindlist(monthly_results)
}) %>% bindCache(ReportStart(), ReportEnd())

# Calculate monthly summaries
sys_act_monthly_data <- reactive({
  req(sys_act_monthly_enrollment_categories())
  sys_act_monthly_enrollment_categories()[, {
    # Count unique PersonalIDs for each category using system flow logic
    Inflow <- uniqueN(PersonalID[InflowTypeSummary == "Inflow"])
    
    Outflow <- uniqueN(PersonalID[OutflowTypeSummary == "Outflow"])
    
    .(Inflow = Inflow,
      Outflow = Outflow,
      `Monthly Change` = Inflow - Outflow)
  }, by = month
  ][, month := factor(format(month, "%b"), levels = month.abb)]
})

# Prepare plot data
# Prepare plot data
sys_act_monthly_plot_data <- reactive({
  data <- sys_act_monthly_data()
  req(data)

  # Find max inflow and min outflow for highlighting
  max_inflow <- data[which.max(`Monthly Change`), month]
  min_outflow <- data[which.min(`Monthly Change`), month]
  
  plot_data <- rbindlist(list(
    data[, .(month = month, 
              Count = Inflow, 
              Flow_Type = "Inflow",
              highlight = month == max_inflow)],
    data[, .(month = month, 
              Count = Outflow, 
              Flow_Type = "Outflow",
              highlight = month == min_outflow)]
  ))
  
  setorder(plot_data, month)
  return(plot_data)
})

# Create plot
output$sys_act_monthly_ui_chart <- renderPlot({
  req(sys_act_monthly_plot_data())

  ggplot(sys_act_monthly_plot_data(), 
          aes(x = month, y = Count, fill = Flow_Type, alpha = highlight, group=Flow_Type)) +
    geom_col(position = position_dodge(preserve="single"), width = 0.5) +
    geom_col(data = sys_act_monthly_plot_data()[highlight == TRUE],
          aes(x = month, y = Count, fill = Flow_Type, group=Flow_Type),
          position = position_dodge(preserve = "single"),
          width = 0.5,
          color = "black",
          alpha = 0.6) +
    scale_fill_manual(values = c("Inflow" = "#BDB6D7", "Outflow" = "#7F5D9D")) +
    scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1), guide = "none") +
    theme_minimal() +
    labs(
      title = "Monthly Homeless Population Flow",
      x = "Month",
      y = "Count",
      fill = "Flow Type"
    ) +
    theme(
      axis.text = element_blank(),
      axis.title.x = element_blank(), 
      axis.title.y = paste0("Count of ", case_when(
          input$syso_level_of_detail == "All" ~ "People",
          input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
          TRUE ~
            getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
        )),   
      legend.position = "none",
      panel.grid = element_blank(),        # Remove gridlines
      axis.line.y = element_blank(), 
      axis.line.x = element_line(),          # Remove axis lines
      plot.margin = margin(l = 48),        # Increase left margin
      axis.ticks = element_blank()
      
    )
})

# Create summary table
output$sys_act_monthly_table <- renderDT({
  req(sys_act_monthly_data())

    summary_data <- dcast(
      melt(sys_act_monthly_data(), id.vars = "month", variable.name = "Type"),
      Type ~ month,
      value.var = "value"
    )

    datatable(summary_data,
              options = list(
                dom = 't',
                ordering = FALSE,
                pageLength = 3,
                columnDefs = list(
                  list(
                    width = "48px",    # Set to specific width
                    targets = 0
                  )  
                )
              ),
              rownames = FALSE) # %>%
      # formatStyle(
      #   names(summary_data)[-1],
      #   backgroundColor = styleEqual(
      #     c(max(summary_data[Type == "Monthly Change", -1]), 
      #       min(summary_data[Type == "Monthly Change", -1])),
      #     c("#BDB6D7", "#7F5D9D")
      #   ),
      #   target = "cell",
      #   subset = summary_data$Type == "Monthly Change"
      # )
})