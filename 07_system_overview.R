logToConsole(session, "Running system overview")

# Age ---------------------------------------------------------------------
EnrollmentAdjustAge <- qDT(EnrollmentAdjust) %>% 
  fmutate(AgeAtEntry = fifelse(is.na(AgeAtEntry), -1, AgeAtEntry))

system_person_ages <- EnrollmentAdjustAge %>%
  fgroup_by(PersonalID) %>%
  fmutate(AgeAtEntry = fmax(AgeAtEntry, na.rm = TRUE)) %>%
  fungroup() %>%
  fmutate(
    AgeCategory = factor(fcase(
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
  ) %>%
  fselect(PersonalID, MostRecentAgeAtEntry = AgeAtEntry, AgeCategory)


# Client-level flags ------------------------------------------------------
# will help us categorize people for filtering
session$userData$client_categories <- qDT(Client) %>%
  join(system_person_ages, on = "PersonalID") %>%
  fselect(c(
    "PersonalID",
    race_cols,
    "VeteranStatus",
    "AgeCategory"
  )) %>%
  fmutate(
    VeteranStatus = fifelse(VeteranStatus == 1 &
                              !is.na(VeteranStatus), 1, 0),
    ## Race/Ethnicity
    # flattening the values, eliminating nulls
    AmIndAKNative = fifelse(AmIndAKNative == 1 & !is.na(AmIndAKNative), 1, 0),
    Asian = fifelse(Asian == 1 & !is.na(Asian), 1, 0),
    BlackAfAmerican = fifelse(BlackAfAmerican == 1 & !is.na(BlackAfAmerican), 1, 0),
    NativeHIPacific = fifelse(NativeHIPacific == 1 & !is.na(NativeHIPacific), 1, 0),
    White = fifelse(White == 1 & !is.na(White), 1, 0),
    MidEastNAfrican = fifelse(MidEastNAfrican == 1 & !is.na(MidEastNAfrican), 1, 0),
    HispanicLatinaeo = fifelse(HispanicLatinaeo == 1 & !is.na(HispanicLatinaeo), 1, 0),
    # exclusive logic group 1
    AmIndAKNativeAloneMethod1Detailed = 
      fifelse(AmIndAKNative == 1 &
                Asian +
                BlackAfAmerican +
                NativeHIPacific +
                White +
                MidEastNAfrican +
                HispanicLatinaeo == 0, 1, 0),
    AmIndAKNativeLatineMethod1Detailed = 
      fifelse(AmIndAKNative == 1 & HispanicLatinaeo == 1 &
                Asian +
                BlackAfAmerican +
                NativeHIPacific +
                White +
                MidEastNAfrican == 0, 1, 0),
    AsianAloneMethod1Detailed =
      fifelse(Asian == 1 &
                AmIndAKNative +
                BlackAfAmerican +
                NativeHIPacific +
                White +
                MidEastNAfrican +
                HispanicLatinaeo == 0, 1, 0),
    AsianLatineMethod1Detailed =
      fifelse(Asian == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                BlackAfAmerican +
                NativeHIPacific +
                White +
                MidEastNAfrican == 0, 1, 0),
    BlackAfAmericanAloneMethod1Detailed =
      fifelse(BlackAfAmerican == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                MidEastNAfrican +
                HispanicLatinaeo == 0, 1, 0),
    BlackAfAmericanLatineMethod1Detailed =
      fifelse(BlackAfAmerican == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                MidEastNAfrican == 0, 1, 0),
    LatineAloneMethod1Detailed =
      fifelse(HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                MidEastNAfrican +
                BlackAfAmerican == 0, 1, 0),
    MidEastNAfricanAloneMethod1Detailed =
      fifelse(MidEastNAfrican == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                HispanicLatinaeo +
                BlackAfAmerican == 0, 1, 0),
    MidEastNAfricanLatineMethod1Detailed =
      fifelse(MidEastNAfrican == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                NativeHIPacific +
                White +
                BlackAfAmerican == 0, 1, 0),
    NativeHIPacificAloneMethod1Detailed =
      fifelse(NativeHIPacific == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                White +
                HispanicLatinaeo +
                BlackAfAmerican == 0, 1, 0),
    NativeHIPacificLatineMethod1Detailed =
      fifelse(NativeHIPacific == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                White +
                BlackAfAmerican == 0, 1, 0),
    WhiteAloneMethod1Detailed =
      fifelse(White == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                NativeHIPacific +
                HispanicLatinaeo +
                BlackAfAmerican == 0, 1, 0),
    WhiteLatineMethod1Detailed =
      fifelse(White == 1 & HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                NativeHIPacific +
                BlackAfAmerican == 0, 1, 0),
    MultipleNotLatineMethod1Detailed =
      fifelse(HispanicLatinaeo == 0 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                NativeHIPacific +
                White +
                BlackAfAmerican > 1, 1, 0),
    MultipleLatineMethod1Detailed =
      fifelse(HispanicLatinaeo == 1 &
                AmIndAKNative +
                Asian +
                MidEastNAfrican +
                NativeHIPacific +
                White +
                BlackAfAmerican > 1, 1, 0),
    RaceEthnicityUnknown =
      fifelse(
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
    BILPOCMethod1Summarized = fifelse(
      AmIndAKNative +
        Asian +
        MidEastNAfrican +
        NativeHIPacific +
        HispanicLatinaeo +
        BlackAfAmerican > 0, 1, 0
    ),
    WhiteMethod1Summarized = fifelse(
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
    AmIndAKNativeMethod2Detailed = fifelse(AmIndAKNative == 1, 1, 0),
    AsianMethod2Detailed = fifelse(Asian == 1, 1, 0),
    BlackAfAmericanMethod2Detailed = fifelse(BlackAfAmerican == 1, 1, 0),
    LatineMethod2Detailed = fifelse(HispanicLatinaeo == 1, 1, 0),
    MidEastNAfricanMethod2Detailed = fifelse(MidEastNAfrican == 1, 1, 0),
    NativeHIPacificMethod2Detailed = fifelse(NativeHIPacific == 1, 1, 0),
    WhiteMethod2Detailed = fifelse(White == 1, 1, 0),
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
      fifelse(BlackAfAmerican == 1 & HispanicLatinaeo == 1, 1, 0),
    LatineMethod2Summarized = fifelse(HispanicLatinaeo == 1, 1, 0),
    LatineAloneMethod2Summarized = fifelse(
      HispanicLatinaeo == 1 &
        AmIndAKNative +
        Asian +
        NativeHIPacific +
        White +
        MidEastNAfrican +
        BlackAfAmerican == 0, 1, 0
    )
  )
session$userData$client_categories[, (race_cols) := NULL]

# Data prep ---------------------------------------------------------------

# using EnrollmentAdjust because that df doesn't contain enrollments that fall
# outside periods of operation/participation
enrollment_prep <- EnrollmentAdjustAge %>%
  fselect(EnrollmentID,
         PersonalID,
         ProjectID,
         ProjectType,
         HouseholdID,
         HouseholdType,
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
  join(Project %>% 
              fselect(ProjectID,
                     ProjectName,
                     OrganizationID,
                     RRHSubType,
                     ContinuumProject),
            on = 'ProjectID', how = 'left') %>%
  join(Organization %>%
              fselect(OrganizationID, OrganizationName) %>%
              funique(),
            on = "OrganizationID", how = 'left') %>%

  # left_join(HealthAndDV %>%
  #             filter(DataCollectionStage == 1) %>%
  #             select(EnrollmentID, DomesticViolenceSurvivor, CurrentlyFleeing),
  #           by = "EnrollmentID") %>%
  join(system_person_ages, on = "PersonalID") %>%
  fsubset(ContinuumProject == 1 & EntryDate < coalesce(ExitDate, no_end_date)) %>% # exclude impossible enrollments
  fselect(-ContinuumProject)

# IMPORTANT: ^ same granularity as EnrollmentAdjust! A @TEST here might be to
# check that
# enrollment_prep %>%
#   nrow() == EnrollmentAdjust %>% filter(ContinuumProject == 1) %>% nrow()
# This aims to add demographic data that lives in various other tables added
# to the enrollment data *without changing the granularity*

# corrected hohs ----------------------------------------------------------

# preps household data to match the way we need the app to 
hh_adjustments <- enrollment_prep[, `:=`(
  HoHAlready = fifelse(RelationshipToHoH == 1 & AgeAtEntry > 17, 1, 0)
)][order(-HoHAlready, -AgeAtEntry, PersonalID), 
   CorrectedHoH := fifelse(seq_len(.N) == 1, 1, 0),
   by = .(HouseholdID, ProjectID)
][
  , .(EnrollmentID, CorrectedHoH)
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
  join(hh_adjustments, on = 'EnrollmentID', how='left') %>%
  colorder(RelationshipToHoH, CorrectedHoH, pos = 'after')

# (^ also same granularity as EnrollmentAdjust)
rm(hh_adjustments)

# Full Enrollment-level Data Prep ----------------------------------------------
# **ProjectTypeWeight** helps determine eecr/lecr
# **lh_prior_livingsituation** is used to define was_lh_at_start and was_lh_at_end
# which are then used to select the EECR/LECR
# throws out HP and enrollments outside Report window and 2 years prior
# limits to only necessary columns

enrollment_categories <- enrollment_prep_hohs %>% 
  fsubset(
    ProjectType != hp_project_type & 
    EntryDate <= session$userData$ReportEnd & ExitAdjust >= (session$userData$ReportStart %m-% years(2))
  ) %>%
  fmutate(
    during_period = EntryDate <= session$userData$ReportEnd & ExitAdjust >= session$userData$ReportStart
  ) %>%
  fgroup_by(PersonalID) %>%
  fmutate(has_enrl_in_date_range = anyv(during_period, TRUE)) %>%
  fungroup() %>%
  fsubset(has_enrl_in_date_range) %>%
  fmutate(
    ProjectTypeWeight = fcase(
      ProjectType %in% ph_project_types & !is.na(MoveInDateAdjust), 100,
      ProjectType %in% ph_project_types & is.na(MoveInDateAdjust), 80,
      ProjectType %in% lh_residential_project_types, 60,
      ProjectType %in% setdiff(non_res_project_types, ce_project_type), 40,
      ProjectType == ce_project_type, 30,
      default = 20
    ),
    lh_prior_livingsituation = !is.na(LivingSituation) &
      (LivingSituation %in% homeless_livingsituation_incl_TH |
         (LivingSituation %in% institutional_livingsituation &
            LOSUnderThreshold == 1 & PreviousStreetESSH == 1 &
            !is.na(LOSUnderThreshold) & !is.na(PreviousStreetESSH)
         )
      ),
    # The purpose of this variable is to capture the idea that an LH date (LH PLS, LH CLS, or Bed Night)
    # Can tell us something about a person's LH status for some time beyond that particular date
    # In this way, it will help us construct an enrollment's last LH date
    # It also helps us determine if an enrollment is LH at period start/end, 
    # by looking back from the period start/end this number of days to see if there's an LH date
    days_lh_valid = fcase(
      ProjectType == ce_project_type, 90,
      ProjectType %in% non_res_project_types, 60,
      ProjectType == es_nbn_project_type, 15,
      default = 0
    ),
    lh_at_entry = ProjectType %in% c(lh_project_types, ph_project_types) | 
      (ProjectType %in% non_res_project_types & lh_prior_livingsituation)
  ) %>% 
  fselect(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust,
    ProjectType,
    MostRecentAgeAtEntry,
    LivingSituation,
    lh_prior_livingsituation,
    LOSUnderThreshold,
    PreviousStreetESSH,
    Destination,
    AgeAtEntry,
    CorrectedHoH,
    # DomesticViolenceCategory,
    HouseholdType,
    ProjectTypeWeight,
    days_lh_valid,
    lh_at_entry
  ) %>% 
  setkeyv(cols = c("EnrollmentID", "PersonalID", "ProjectType"))

# Get dataset of literally homeless CLS records. This will be used to:
# 1. remove problematic enrollments
# 2. categorize non-res enrollments/people as active_at_start, homeless_at_end, 
# and unknown_at_end
lh_cls <- CurrentLivingSituation %>%
  fsubset(
    CurrentLivingSituation %in% homeless_livingsituation_incl_TH, 
    EnrollmentID, InformationDate
  ) %>%
  funique()

# Remove "problematic" enrollments ----------------------------------
# These are non-residential (other than SO) enrollments for which we have no LH evidence: 
# So any enrollment that is not lh_prior_livingsituation and has no LH CLS
problematic_nonres_enrollmentIDs <- base::setdiff(
  (enrollment_categories %>% 
     fsubset(ProjectType %in% non_res_nonlh_project_types & !lh_prior_livingsituation)
  )$EnrollmentID,
  unique(lh_cls$EnrollmentID)
)

enrollment_categories <- enrollment_categories %>%
  fsubset(
    !EnrollmentID %in% problematic_nonres_enrollmentIDs &
    EntryDate < ExitAdjust #exclude impossible enrollments. EntryDate == ExitAdjust is possible but not useful
  )

# Set MoveInDateAdjust to no_end_date if NA. 
# This will allow us to just use MoveInDateAdjust without also checking for NA
# MoveInDateAdjust is used to determine if/when a person was Housed.
enrollment_categories <- enrollment_categories %>%
  ftransform(MoveInDateAdjust = MoveInDateAdjust)

# This step does 2 things:
#  1. Compute lh_date, first_lh_date. and last_lh_date
#  2. Modify EntryDate and ExitAdjust to be these first and last LH dates. 
#     (Only applicable to non-res projects with no LH PLS or no ExitDate, but with an LH CLS)
# lh_date is the InformationDate (Non-Res) or DateProvided (ES-NbN) 
# first_lh_date and last_lh_date are also used to determine days_since_last_lh. And first_lh_date is used for the FTH Inflow status
session$userData$lh_info <- enrollment_categories %>%
  join(lh_cls %>% frename(InformationDate = lh_date) %>% funique(), on="EnrollmentID", multiple =TRUE) %>%
  join(Services %>% fselect(EnrollmentID, lh_date_s = DateProvided) %>% funique(), on="EnrollmentID", multiple =TRUE) %>%
  fmutate(
    lh_date = fcoalesce(lh_date, lh_date_s),
    non_exit_lh_in_report = 
      ((EntryDate + days_lh_valid) %between% list(session$userData$ReportStart, session$userData$ReportEnd) & lh_prior_livingsituation) |
      (lh_date + days_lh_valid) %between% list(session$userData$ReportStart, session$userData$ReportEnd)
  ) %>%
  fgroup_by(EnrollmentID) %>%
  fmutate(exit_is_only_lh = !any(non_exit_lh_in_report, na.rm=TRUE)) %>%
  fungroup() %>%
  fmutate(
    first_lh_compare_date = pmin(
      fifelse(MoveInDateAdjust <= session$userData$ReportStart, NA, ExitAdjust),
      fifelse(
        lh_at_entry,
        EntryDate,
        fifelse(lh_date < EntryDate, ExitAdjust, lh_date)
      ),
      na.rm=TRUE
    ),
    last_lh_compare_date = pmax(
      lh_date,
      fifelse(!exit_is_only_lh, ExitDate, NA),
      fifelse(
        lh_at_entry,
        pmin(EntryDate + days_lh_valid, ExitAdjust, na.rm = TRUE),
        NA
      ),
      na.rm=TRUE
    )
  ) %>%
  fgroup_by(EnrollmentID) %>%
  fmutate(
    first_lh_date = fmin(first_lh_compare_date),
    last_lh_date = fmax(last_lh_compare_date)
  ) %>%
  ftransform(
    last_lh_date = fifelse(
      ProjectType %in% c(lh_project_types_nonbn, ph_project_types),
      fcoalesce(MoveInDateAdjust, ExitAdjust),
      last_lh_date
    )
  ) %>%
  fungroup() %>%
  fselect(
    PersonalID, 
    EnrollmentID,
    ProjectType, 
    MoveInDateAdjust,
    lh_prior_livingsituation, 
    days_lh_valid, 
    lh_at_entry,
    lh_date,
    first_lh_date,
    last_lh_date
  )

session$userData$report_dates <- get_report_dates()

session$userData$enrollment_categories <- enrollment_categories %>%
  join(
    session$userData$lh_info %>% fselect(EnrollmentID, first_lh_date, last_lh_date) %>% funique(),
    on = "EnrollmentID"
  ) %>%
  fmutate(
    # "Trimming" EntryDate and ExitAdjust for non-res and NbN projects
    # This is because such projects SHOULD have an LH CLS when they enter, but don't always do this.
    # So we set Entry and Exit to the first/last LH dates in these cases.
    EntryDate = fifelse(
      ProjectType %in% c(lh_project_types_nonbn, ph_project_types, out_project_type, es_nbn_project_type),
      EntryDate,
      first_lh_date
    ),
    ExitAdjust = fifelse(
      ProjectType %in% nbn_non_res & ExitAdjust == no_end_date,
      last_lh_date + days_lh_valid,
      ExitAdjust
    )
  ) %>%
  fsubset(EntryDate < ExitAdjust) # After trimming, want to ensure that the new EntryDate < new ExitAdjust

# Force run/calculate period_specific_data reactive
# Better to do it up-front than while charts are loading
period_specific_data()