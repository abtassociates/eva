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
      )
  ) %>% 
  fselect(EnrollmentID,
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
          ProjectTypeWeight) %>% 
  setkeyv(cols = c("EnrollmentID", "PersonalID", "ProjectType"))

# Get dataset of literally homeless CLS records. This will be used to:
# 1. remove problematic enrollments
# 2. categorize non-res enrollments/people as active_at_start, homeless_at_end, 
# and unknown_at_end

lh_cls <- CurrentLivingSituation %>%
  fselect(EnrollmentID, InformationDate, CurrentLivingSituation) %>%
  fsubset(CurrentLivingSituation %in% homeless_livingsituation_incl_TH)

# Remove "problematic" enrollments ----------------------------------
# These are non-residential (other than SO) enrollments for which we have no LH evidence: 
# So any enrollment that is not lh_prior_livingsituation and has no LH CLS
problematic_nonres_enrollmentIDs2 <- base::setdiff(
  (enrollment_categories %>% 
     fsubset(ProjectType %in% non_res_nonlh_project_types & !lh_prior_livingsituation)
  )$EnrollmentID,
  unique(lh_cls$EnrollmentID))

enrollment_categories <- enrollment_categories %>%
  fsubset(!EnrollmentID %in% problematic_nonres_enrollmentIDs)

# Calculate days_since_lookback, days_to_lookahead, and other lookback info
# First, determine days_to_lookahead
dt <- enrollment_categories %>%
  setkey(PersonalID, EntryDate, ExitAdjust) %>%
  fmutate(
    days_to_lookahead = L(EntryDate, -1, g = PersonalID) - ExitAdjust
  )
  
# I don't understand why/how this non-equi self-join works.
# In theory, since this is a right-join, for each EntryDate, we're pulling in the 
# latest record whose ExitAdjust is still before the EntryDate, i.e. the (valid) lookback,
# then we pull in that lookback's info.
# It's just surprising which columns are the relevant ones. If you were to look at
# the dataset before the fmutate, EntryDate and ExitAdjust don't make sense.
# Ideally we'd create the new variables all in the same right-join, rather than
# joining back to the full dt later, but this just doesn't yield the desired results.
lookback_info <- dt[ 
  dt[, .(PersonalID, EnrollmentID, EntryDate, EntryTemp = EntryDate)], # This is the i. prefix dataset
  on = .(PersonalID, ExitAdjust <= EntryDate),
  mult = "last"
] %>% 
fsummarize(
  PersonalID = PersonalID,
  EnrollmentID = i.EnrollmentID,
  days_since_lookback = EntryTemp - ExitDate, # ExitDate is the lookup's ExitDate
  lookback_enrollment_id = EnrollmentID,
  lookback_dest_perm = Destination %in% perm_livingsituation,
  lookback_movein = MoveInDateAdjust,
  lookback_is_nonres_or_nbn = ProjectType %in% nbn_non_res
)

session$userData$enrollment_categories <- join(
  dt,
  lookback_info,
  on = c("PersonalID", "EnrollmentID")
)
rm(dt)

# Prepare a dataset of non_res enrollments and corresponding LH info
# will be used to categorize non-res enrollments/people as active_at_start, 
# homeless_at_end, and unknown_at_end
non_res_enrollments <- session$userData$enrollment_categories %>% 
  fsubset(ProjectType %in% non_res_project_types) %>% 
  fselect(EnrollmentID, EntryDate, ProjectType, ExitAdjust, lh_prior_livingsituation,
          days_since_lookback,
          days_to_lookahead)


session$userData$lh_non_res <- join(
  non_res_enrollments,
  lh_cls,
  on = "EnrollmentID",
  how = "left",
  column = TRUE
) %>% 
  fgroup_by(EnrollmentID) %>%
  fmutate(
    last_lh_info_date = fmax(
      pmax(
        InformationDate, 
        fifelse(
          ProjectType == out_project_type | lh_prior_livingsituation, 
          EntryDate, 
          NA
        )
      ) 
    )
  ) %>%
  fungroup()

# Do something similar for ES NbNs and Services
es_nbn_enrollments <- fsubset(session$userData$enrollment_categories, ProjectType == es_nbn_project_type) %>% 
  fselect(EnrollmentID,EntryDate, ProjectType, ExitAdjust, lh_prior_livingsituation,
          days_since_lookback, 
          days_to_lookahead)


session$userData$lh_nbn <- join(
  es_nbn_enrollments,
  Services %>% fselect(EnrollmentID, DateProvided),
  on="EnrollmentID",
  how = "left"
) %>% 
  fgroup_by(EnrollmentID) %>%
  fmutate(
    last_lh_info_date = fmax(
      pmax(DateProvided, EntryDate)
    )
  ) %>%
  fungroup()

rm(es_nbn_enrollments, non_res_enrollments)

session$userData$report_dates <- get_report_dates()

# Force run/calculate period_specific_data reactive
# Better to do it up-front than while charts are loading
period_specific_data()