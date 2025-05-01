logToConsole(session, "Running system overview")

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
         VeteranStatus,
         AgeCategory
  ) %>%
  mutate(
    VeteranStatus = if_else(VeteranStatus == 1 &
                              !is.na(VeteranStatus), 1, 0),
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
  select(-all_of(race_cols))
)

# Data prep ---------------------------------------------------------------

# using EnrollmentAdjust because that df doesn't contain enrollments that fall
# outside periods of operation/participation
enrollment_prep <- EnrollmentAdjustAge %>%
  select(EnrollmentID,
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
  left_join(hh_adjustments, join_by(EnrollmentID)) %>%
  relocate(CorrectedHoH, .after = RelationshipToHoH)
# (^ also same granularity as EnrollmentAdjust)
rm(hh_adjustments)

# Full Enrollment-level Data Prep ----------------------------------------------
# Includes ProjectTypeWeight for helping determine eecr/lecr
# throws out HP and enrollments outside Report window and 2 years prior
# limits to only necessary columns
enrollment_categories <- as.data.table(enrollment_prep_hohs)[, `:=`(
  ProjectTypeWeight = fcase(
    ProjectType %in% ph_project_types & !is.na(MoveInDateAdjust), 100,
    ProjectType %in% ph_project_types & is.na(MoveInDateAdjust), 80,
    ProjectType %in% lh_residential_project_types, 60,
    ProjectType %in% non_res_project_types & ProjectType != ce_project_type, 40,
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
  )][
    ProjectType != hp_project_type & 
    EntryDate <= session$userData$ReportEnd & ExitAdjust >= (session$userData$ReportStart %m-% years(2))
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
    LivingSituation,
    lh_prior_livingsituation,
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
lh_non_res <- qDT(CurrentLivingSituation) %>%
  fsubset(CurrentLivingSituation %in% homeless_livingsituation_incl_TH) %>%
  join(
    enrollment_categories[ProjectType %in% non_res_project_types], 
    on = "EnrollmentID",
    how = "left"
  ) %>%
  fselect(
    EnrollmentID, EntryDate, ProjectType, ExitAdjust, InformationDate, lh_prior_livingsituation
  )

setkey(lh_non_res, EnrollmentID)

# Do something similar for ES NbNs and Services
lh_nbn <- qDT(Services) %>%
  fselect(EnrollmentID, DateProvided) %>%
  join(
    EnrollmentAdjust[
      ProjectType == es_nbn_project_type, 
      .(EnrollmentID, PersonalID, EntryDate, ExitAdjust)
    ],
    on="EnrollmentID",
    how = "inner"
  )
setkey(lh_nbn, EnrollmentID)

# Remove "problematic" enrollments ----------------------------------
# These are enrollments that entered the system as not LH
# but then have a CLS later in the enrollment
# two ways to identify "not LH" for their entry enrollment (eecr): 
# 1. !lh_prior_livingsituation and 
# 2. no lh cls where InformationDate == EntryDate
lh_cls_info_eq_entry <- lh_non_res %>%
  fgroup_by(EnrollmentID) %>%
  fsummarise(has_lh_cls_eq_entry = anyv(InformationDate == EntryDate, TRUE))

entered_not_lh_but_lh_cls_later <- enrollment_categories %>%
  fsubset(
    EntryDate <= session$userData$ReportEnd & 
    ExitAdjust >= session$userData$ReportStart &
    ProjectType %in% non_res_project_types
  ) %>%
  fgroup_by(PersonalID) %>%
  fmutate(first_enrollment = fmin(EntryDate)) %>%
  fungroup()  %>%
  fsubset(!(EntryDate == first_enrollment & lh_prior_livingsituation)) %>%
  join(
    lh_cls_info_eq_entry %>% fsubset(has_lh_cls_eq_entry == TRUE),
    on = "EnrollmentID",
    how = "inner"
  )

# Define lh_prior_livingsituation, lh_at_entry, and EnrolledHomeless
# **lh_prior_livingsituation** is used to define was_lh_at_start and was_lh_at_end
# which are then used to select the EECR/LECR
# **lh_at_entry** is used to categorize someone as first-time homeless
# **EnrolledHomeless** is used to categorize someone as Active at Start: Homeless
enrollment_categories <- enrollment_categories %>%
  join(
    entered_not_lh_but_lh_cls_later, 
    on = "PersonalID",
    how = "anti"
  ) %>%
  join(
    lh_cls_info_eq_entry,
    on = "EnrollmentID",
    how = "left"
  ) %>%
  fmutate(
    lh_at_entry = ProjectType %in% lh_residential_project_types |
      (ProjectType %in% psh_oph_project_types & (
        MoveInDateAdjust >= EntryDate | is.na(MoveInDateAdjust)
      )) |
      (ProjectType %in% non_res_project_types & (
        lh_prior_livingsituation | has_lh_cls_eq_entry
      )),
    EnrolledHomeless = ProjectType %in% project_types_enrolled_homeless |
      lh_prior_livingsituation
  )

session$userData$report_dates <- get_report_dates()

# Period-Specific Prep Functions -----------------------------------------------
# These steps and functions process data for a given period, which can be the full
# reporting period, or each month in between the start and end of the full period

## LH info for non-res projects -----------
# custom_rprof({
# continuing the work of the base lh_non_res dataset from 07_system_overview.R 
# we now make it period-specific, and collapse it down to the enrollment-level
# so this contains enrollments with LH CLS and an indicator as to 
# whether InformationDate is within to 60 or 90 days 
# (depending on project type, but only limited to Non-Res Project Types) 
# from the period start/end
# we then merge this with enrollment_categories to fully replace the homeless_cls_finder function
# this avoids having to re-filter and do the check for each enrollment
lh_non_res_period <- function(startDate, endDate) {
  lh_non_res %>%
    # Initial filtering
    fsubset(EntryDate <= endDate & ExitAdjust >= (startDate %m-% years(2))) %>%
    
    # Calculate time windows
    ftransform(
      start_window = startDate - fifelse(ProjectType == ce_project_type, 90, 60),
      end_window = endDate - fifelse(ProjectType == ce_project_type, 90, 60)
    ) %>%
    ftransform(
      lh_cls_in_start_window = between(InformationDate, start_window, startDate),
      lh_cls_in_end_window = between(InformationDate, end_window, endDate),
      entry_in_start_window = between(EntryDate, start_window, startDate),
      entry_in_end_window = between(EntryDate, end_window, endDate),
      lh_cls_in_range = between(InformationDate, startDate, endDate)
    ) %>%
    
    # Group by EnrollmentID and calculate window flags
    fgroup_by(EnrollmentID) %>%
    fmutate(
      has_lh_cls_in_start_window = anyv(lh_cls_in_start_window, TRUE),
      has_lh_cls_in_end_window = anyv(lh_cls_in_end_window, TRUE),
      has_lh_cls_in_range = anyv(lh_cls_in_range, TRUE)
    ) %>%
    fungroup()
}

lh_nbn_period <- function(startDate, endDate) {
  lh_nbn %>%
    # Initial filtering
    fsubset(EntryDate <= endDate & ExitAdjust >= (startDate %m-% years(2))) %>%
    ftransform(
      NbN15DaysBeforeReportStart = between(DateProvided, startDate - 15, startDate),
      NbN15DaysBeforeReportEnd = between(DateProvided, endDate - 15, endDate),
      entry_in_start_window = between(EntryDate, startDate - 15, startDate),
      entry_in_end_window = between(EntryDate, endDate - 15, endDate)
    ) %>%
    fgroup_by(EnrollmentID) %>%
    fsummarise(
      NbN15DaysBeforeReportStart = anyv(NbN15DaysBeforeReportStart, TRUE),
      NbN15DaysBeforeReportEnd = anyv(NbN15DaysBeforeReportEnd, TRUE),
      entry_in_start_window = entry_in_start_window,
      entry_in_end_window = entry_in_end_window
    ) %>%
    fsubset(
      NbN15DaysBeforeReportStart | 
      NbN15DaysBeforeReportEnd |
      entry_in_start_window |
      entry_in_end_window
    )
}

## LH info for NbN projects --------

# Narrow down to period-relevant enrollments and create eecr, lecr, 
# and other variables used for Inflow/Outflow categorization
session$userData$get_period_specific_enrollment_categories <- memoise::memoise(
  function(report_period, upload_name) {
    # custom_rprof({
    startDate <- report_period[1]
    endDate <- report_period[2]
    
    lh_info <- unique(
      # Capture LH info for both non-residential projects AND ES NbN projects
      rbind(
        lh_non_res_period(startDate, endDate),
        lh_nbn_period(startDate, endDate),
        fill = TRUE
      )[, 
      .(
        EnrollmentID,
        
        was_lh_at_start = (
          # Any LH project type (except ES-NbN, which is handled separately)
          ProjectType %in% lh_project_types_nonbn |
          (ProjectType %in% c(es_nbn_project_type, non_res_project_types) & (
            # Entry in window (15/60/90 days, depending on project type) & lh prior living situation
            (entry_in_start_window & lh_prior_livingsituation) |
            
            # ES NbN and Bed Night in 15-day window
            (ProjectType == es_nbn_project_type & NbN15DaysBeforeReportStart) |
            
            # Non-Res and LH CLS in 60/90-day window OR 
            # Entry after start but LH prior or LH CLS
            (ProjectType %in% non_res_project_types & (
              has_lh_cls_in_start_window | 
              (EntryDate > startDate & (lh_prior_livingsituation | has_lh_cls_in_range))
            ))
          ))
        ),
        
        was_lh_at_end = (
          # Any LH project type (except ES-NbN, which is handled separately)
          ProjectType %in% lh_project_types_nonbn |
            (ProjectType %in% c(es_nbn_project_type, non_res_project_types) & (
              # Entry in window (15/60/90 days, depending on project type) & lh prior living situation
              (entry_in_end_window & lh_prior_livingsituation) |
                
                # ES NbN and Bed Night in 15-day window
                (ProjectType == es_nbn_project_type & NbN15DaysBeforeReportEnd) |
                
                # Non-Res and LH CLS in 60/90-day window OR 
                # Entry after start but LH prior or LH CLS
                (ProjectType %in% non_res_project_types & (
                  has_lh_cls_in_end_window | 
                    (ExitAdjust < endDate & (lh_prior_livingsituation | has_lh_cls_in_range))
                ))
            ))
        ),
        
        has_lh_cls_in_range,
        NbN15DaysBeforeReportStart,
        NbN15DaysBeforeReportEnd
      )
    ])
    enrollment_categories_period <- join(
      enrollment_categories %>% fsubset(
        # keep enrollments in date range and exits within the 2 yrs prior to start
        EntryDate <= endDate & ExitAdjust >= (startDate %m-% years(2))
      ),
      lh_info,
      on = "EnrollmentID",
      how = "left"
    )

    # browser()
    enrollment_categories_period <- enrollment_categories_period %>%
      ftransform(
        straddles_start = EntryDate <= startDate & ExitAdjust >= startDate,
        straddles_end = EntryDate <= endDate & ExitAdjust >= endDate,
        in_date_range = EntryDate <= endDate & ExitAdjust >= startDate #,
        # DomesticViolenceCategory = fcase(
        #   DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1, "DVFleeing",
        #   DomesticViolenceSurvivor == 1, "DVNotFleeing",
        #   default = "NotDV"
        # )
      ) %>% 
      # drop in-range, non-res enrollments that are not LH at start AND have no CLS later
      fsubset(!(
        in_date_range & 
          ProjectType %in% non_res_project_types & 
          ((!was_lh_at_start | is.na(was_lh_at_start)) & !has_cls_after_entry)
      )) %>%
      # Flag if person had any straddling enrollments
      # to be used when calculating eecr/lecr in no-straddle cases
      fgroup_by(PersonalID) %>%
      fmutate(
        any_straddle_start = anyv(straddles_start, TRUE),
        any_straddle_end = anyv(straddles_end, TRUE)
      ) %>%
      # flag the first and last straddling enrollments, 
      # by (desc) ProjectTypeWeight and EntryDate
      roworder(-ProjectTypeWeight, EntryDate) %>%
      fmutate(
        eecr_straddle = ffirst(
          fifelse(straddles_start, EnrollmentID, NA)
        ) == EnrollmentID,
        lecr_straddle = flast(
          fifelse(straddles_end, EnrollmentID, NA)
        ) == EnrollmentID
      ) %>%
      # flag the first and last enrollments in the report period,
      # for people that have no straddles,
      # by EntryDate and (desc) ProjectTypeWeight
      roworder(EntryDate, -ProjectTypeWeight) %>%
      fmutate(
        eecr_no_straddle = ffirst(
          fifelse(!any_straddle_start & in_date_range, EnrollmentID, NA)
        ) == EnrollmentID,
        lecr_no_straddle = flast(
          fifelse(!any_straddle_end & in_date_range, EnrollmentID, NA)
        ) == EnrollmentID
      ) %>%
      fungroup() %>%
      # Create eecr and lecr flags
      fmutate(
        eecr = (eecr_straddle | eecr_no_straddle) & (
          !ProjectType %in% non_res_nonlh_project_types | was_lh_at_start
        ),
        lecr = (lecr_straddle | lecr_no_straddle) & (
          !ProjectType %in% non_res_nonlh_project_types | was_lh_at_end
        ),
        eecr_is_res = eecr & ProjectType %in% project_types_w_beds
      ) %>%
      fgroup_by(PersonalID) %>%
      fmutate(
        has_lecr = anyv(lecr, TRUE),
        has_eecr = anyv(eecr, TRUE),
        # this is to match with the project type filter
        eecr_project_type = fifelse(
          anyv(eecr_is_res, TRUE), "Residential", "NonResidential"
        )
      ) %>%
      fungroup() %>%
      fsubset(has_eecr == TRUE) %>%
      # "fill in" lecr as TRUE where eecr is the only enrollment
      ftransform(lecr = lecr | (eecr & !has_lecr)) %>%
      roworder(PersonalID, EntryDate) %>%
      fmutate(
        first_lookback = L(eecr, n = -1, g = PersonalID) == TRUE, #first_lookback is TRUE if the next/lead eecr value is TRUE
        lookback = EntryDate < startDate & (!eecr | is.na(eecr)) & (!lecr | is.na(lecr)),
        days_since_lookback = fifelse(eecr, EntryDate - L(ExitAdjust, g = PersonalID), NA),
        has_enrollment_after_lecr = L(EntryDate, n = -1, g = PersonalID) <= ExitAdjust & lecr #note if they have another enrollment after their LECR. If so, they won't eventually be counted as outflow
      ) %>%
      fsubset(eecr | lecr | lookback) %>%
      fselect(-c(any_straddle_start, any_straddle_end, eecr_no_straddle, eecr_straddle, lecr_straddle, lecr_no_straddle))

    # }, "07_system_overview.R")
  },
  cache = cachem::cache_mem(max_size = 100 * 1024^2) 
)

# Force run/calculate period_specific_data reactive
# Better to do it up-front than while charts are loading
period_specific_data()