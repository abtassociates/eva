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
     CorrectedHoH = ifelse(seq_len(.N) == 1, 1, 0)
   ),
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

# enrollment_categories <- enrollment_prep_hohs %>%
#   mutate(
#     ProjectTypeWeight = case_when(
#       # speaks to presumed trustworthiness of data, not urgency
#       ProjectType %in% ph_project_types &
#         !is.na(MoveInDateAdjust) ~ 100,
#       ProjectType %in% ph_project_types &
#         is.na(MoveInDateAdjust) ~ 80,
#       ProjectType %in% lh_residential_project_types ~ 60,
#       ProjectType %in% non_res_project_types ~ 40,
#       TRUE ~ 20
#     ),
#     lh_prior_livingsituation = !is.na(LivingSituation) &
#       (
#         LivingSituation %in% homeless_livingsituation_incl_TH |
#           (
#             LivingSituation %in% institutional_livingsituation &
#               LOSUnderThreshold == 1 &
#               PreviousStreetESSH == 1 &
#               !is.na(LOSUnderThreshold) &
#               !is.na(PreviousStreetESSH)
#           )
#       ),
#     lh_at_entry =
#       lh_prior_livingsituation == TRUE |
#       ProjectType %in% lh_project_types,
#     EnrolledHomeless =
#       ProjectType %in% project_types_enrolled_homeless |
#       lh_prior_livingsituation == TRUE,
#     straddles_start =
#       EntryDate <= ReportStart() &
#       ExitAdjust >= ReportStart(),
#     straddles_end =
#       EntryDate <= ReportEnd() &
#       ExitAdjust >= ReportEnd(),
#     in_date_range =
#       ExitAdjust >= ReportStart() &
#       EntryDate <= ReportEnd(),
#     # Domestic Violence - this is needed for the System Composition chart
#     DomesticViolenceCategory = case_when(
#       DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1 ~
#         "DVFleeing",
#       DomesticViolenceSurvivor == 1 &
#         (is.na(CurrentlyFleeing) | CurrentlyFleeing != 1) ~
#         "DVNotFleeing",
#       TRUE ~
#         "NotDV"
#       )
#   ) %>%
#   filter(
#     ReportStart() - years(2) <= ExitAdjust &
#       ProjectType != hp_project_type &
#       (ProjectType != ce_project_type |
#          (ProjectType == ce_project_type &
#           (EnrollmentID %in% homeless_cls_finder(ReportStart(), "before", 90) |
#              EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 90) |
#               (
#                 between(EntryDate, ReportStart() - days(90), ReportStart()) == TRUE &
#                   lh_prior_livingsituation == TRUE
#               ) |
#               (
#                 between(EntryDate, ReportEnd() - days(90), ReportEnd()) &
#                   lh_prior_livingsituation == TRUE
#               )
#           )
#       )) &
#       (!ProjectType %in% c(out_project_type,
#                           sso_project_type,
#                           other_project_project_type,
#                           day_project_type) |
#          (ProjectType %in% c(out_project_type,
#                              sso_project_type,
#                              other_project_project_type,
#                              day_project_type) &
#             (EnrollmentID %in% homeless_cls_finder(ReportStart(), "before", 60) |
#                EnrollmentID %in% homeless_cls_finder(ReportEnd(), "before", 60) |
#                (
#                  between(EntryDate, ReportStart() - days(60), ReportStart()) == TRUE &
#                    lh_prior_livingsituation == TRUE
#                ) |
#                (
#                  between(EntryDate, ReportEnd() - days(60), ReportEnd()) == TRUE &
#                    lh_prior_livingsituation == TRUE
#                )
#             )))
#   ) %>%
#   select(
#     EnrollmentID,
#     PersonalID,
#     HouseholdID,
#     EntryDate,
#     MoveInDateAdjust,
#     ExitDate,
#     ExitAdjust,
#     ProjectType,
#     MostRecentAgeAtEntry,
#     lh_prior_livingsituation,
#     lh_at_entry,
#     straddles_start,
#     straddles_end,
#     in_date_range,
#     EnrolledHomeless,
#     LivingSituation,
#     LOSUnderThreshold,
#     PreviousStreetESSH,
#     Destination,
#     AgeAtEntry,
#     CorrectedHoH,
#     DomesticViolenceCategory,
#     HouseholdType,
#     ProjectTypeWeight
#   ) %>%
#   group_by(PersonalID, straddles_start) %>%
#   mutate(StraddlesStart = n(),
#          MaxProjectTypeStart = max(ProjectTypeWeight)) %>%
#   group_by(PersonalID, straddles_end) %>%
#   mutate(StraddlesEnd = n(),
#          MaxProjectTypeEnd = max(ProjectTypeWeight)) %>%
#   group_by(PersonalID) %>%
#   arrange(EntryDate, .by_group = TRUE) %>%
#   mutate(
#     InvolvedInOverlapStart = straddles_start == TRUE &
#       StraddlesStart > 1,
#     InvolvedInOverlapEnd = straddles_end == TRUE &
#       StraddlesEnd > 1,
#     ordinal = row_number(),
#     days_to_next_entry =
#       difftime(lead(EntryDate, order_by = EntryDate),
#                ExitAdjust,
#                units = "days"),
#     days_since_previous_exit =
#       difftime(EntryDate,
#                lag(ExitAdjust, order_by = ExitAdjust),
#                units = "days"),
#     next_enrollment_project_type = lead(ProjectType),
#     previous_enrollment_project_type = lag(ProjectType)
#     ) %>%
#   group_by(PersonalID, InvolvedInOverlapStart) %>%
#   arrange(desc(ProjectTypeWeight), EntryDate, ExitAdjust,
#           .by_group = TRUE) %>%
#   mutate(RankOrderStartOverlaps = row_number()) %>%
#   # getting rid of enrollments involved in an overlap across ReportStart that
#   # didn't get picked as the eecr
#   filter((InvolvedInOverlapStart == TRUE & RankOrderStartOverlaps == 1) |
#            InvolvedInOverlapStart == FALSE) %>%
#   group_by(PersonalID, InvolvedInOverlapEnd) %>%
#   arrange(desc(ProjectTypeWeight), EntryDate, ExitAdjust,
#           .by_group = TRUE) %>%
#   mutate(RankOrderEndOverlaps = row_number()) %>%
#   # getting rid of enrollments involved in an overlap across ReportEnd that
#   # didn't get picked as the lecr
#   filter((InvolvedInOverlapEnd == TRUE & RankOrderEndOverlaps == 1) |
#            InvolvedInOverlapEnd == FALSE) %>%
#   group_by(PersonalID, in_date_range) %>%
#   arrange(EntryDate, ExitAdjust, .by_group = TRUE) %>%
#   mutate(
#     lecr = in_date_range == TRUE & max(ordinal) == ordinal,
#     eecr = in_date_range == TRUE & min(ordinal) == ordinal,
#     lookback = if_else(in_date_range == TRUE, 0, rev(row_number()))
#   ) %>%
#   ungroup() %>%
#   select(-AgeAtEntry) %>%
#   left_join(nbn_enrollments_services, join_by(EnrollmentID)) %>%
#   mutate(NbN15DaysBeforeReportStart = replace_na(NbN15DaysBeforeReportStart, 0),
#          NbN15DaysAfterReportEnd = replace_na(NbN15DaysAfterReportEnd, 0),
#          NbN15DaysBeforeReportEnd = replace_na(NbN15DaysBeforeReportEnd, 0)) %>%
#   arrange(EnrollmentID)

# using data.table --------------------------------------------------------

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
      lh_prior_livingsituation,
    straddles_start = EntryDate <= ReportStart() & ExitAdjust >= ReportStart(),
    straddles_end = EntryDate <= ReportEnd() & ExitAdjust >= ReportEnd(),
    in_date_range = ExitAdjust >= ReportStart() & EntryDate <= ReportEnd()
  )][
    # Apply filtering with efficient conditions
    (ReportStart() %m-% years(2)) <= ExitAdjust &
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
              lh_prior_livingsituation))))
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

enrollment_categories <- merge(
    enrollment_categories, 
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
# AS 2/3/2025: commenting out dv_flag. First, we aren't using DomesticViolence 
# right now in the System Overview tab because we weren't ready for it
# Second, this should be defined at the enrollment level, not client level 
# dv_flag <- as.data.table(Enrollment)[, .(EnrollmentID, EntryDate, ExitAdjust)][
#   as.data.table(HealthAndDV)[DataCollectionStage == 1, .(
#     EnrollmentID,
#     PersonalID,
#     DomesticViolenceSurvivor = fifelse(
#       is.na(DomesticViolenceSurvivor),
#       0,
#       fifelse(DomesticViolenceSurvivor== 1, 1, 0)),
#     CurrentlyFleeing = fifelse(
#       is.na(CurrentlyFleeing),
#       0,
#       fifelse(CurrentlyFleeing == 1, 1, 0))
#   )],
#   on = .(EnrollmentID)
# ][ExitAdjust >= ReportStart() & EntryDate <= ReportEnd(), 
#   .(DomesticViolenceCategory = 
#       fifelse(
#         max(DomesticViolenceSurvivor, na.rm = TRUE) == 1 & 
#           max(CurrentlyFleeing, na.rm = TRUE) == 1, "DVFleeing",
#         fifelse(
#           max(DomesticViolenceSurvivor, na.rm = TRUE) == 1, "DVNotFleeing", "NotDV"))
#   ), by = PersonalID]

client_categories <- Client %>%
  left_join(system_person_ages, join_by(PersonalID)) %>%
  # left_join(setDF(dv_flag), join_by(PersonalID)) %>%
  select(PersonalID,
         all_of(race_cols),
         VeteranStatus,
         AgeCategory #,
         # DomesticViolenceCategory
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
  # Method 1 logic, detailed
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
  # Method 1 logic, summarized
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
  # Data quality check for Method 1, summarized
  DQRaceEthMethod1Summarized =
    BILPOCMethod1Summarized +
    WhiteMethod1Summarized +
    RaceEthnicityUnknown, # all rows should equal 1
  # Method 2 logic, detailed
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
  # Method 2 logic group 2
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

client_categories_reactive <- reactive({
  client_categories %>%
    mutate(All = 1) %>%
    filter(
      AgeCategory %in% input$syso_age &
        !!sym(input$syso_race_ethnicity) == 1 &
        (
          input$syso_spec_pops == "None" |
          (input$syso_spec_pops == "Veteran" &
            VeteranStatus == 1 & !(AgeCategory %in% c("0 to 12", "13 to 17"))) |
          (input$syso_spec_pops == "NonVeteran" &
            VeteranStatus == 0 & !(AgeCategory %in% c("0 to 12", "13 to 17"))) # |
          # (DomesticViolenceCategory == input$syso_spec_pops | 
          #    input$syso_spec_pops == "DVTotal" & DomesticViolenceCategory != "NotDV")
        )
    ) %>%
    select(-All)
  
})

# Enrollment-level reactive -----------------------------------------------

enrollment_categories_reactive <- reactive({
  
  # Filter enrollments by hhtype, project type, and level-of-detail inputs
  enrollment_categories %>%
    left_join(Client %>% select(PersonalID, VeteranStatus), join_by(PersonalID)) %>%
    filter((input$syso_hh_type == "All" |
            (input$syso_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
            (input$syso_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
            (input$syso_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
            (input$syso_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
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
      LivingSituation,
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
})

# Client-level reactive ---------------------------------------------------

# get filtered people-level system dataframe
clients_enrollments_reactive <- reactive({
  enrollment_categories_reactive() %>%
    select(-MostRecentAgeAtEntry) %>%
    inner_join(client_categories_reactive(), join_by(PersonalID))
})


# Enrollment-level universe -----------------------
# only includes people and their lookback thru LECR enrollments

# hello weary traveler amongst these date ranges. you may find it helpful to
# find example clients and their Entry and Exit Dates and enter them into
# https://onlinetools.com/time/visualize-date-intervals <- here.
# add inflow type and active enrollment typed used for system overview plots
universe <- reactive({
  client_enrollments_reactive_dt <- setDT(clients_enrollments_reactive())
  # clients_enrollments_reactive() %>%
    # get rid of rows where the enrollment is neither a lookback enrollment,
    # an eecr, or an lecr. So, keeping all lookback records plus the eecr and lecr 
    # filter(!(lookback == 0 & eecr == FALSE & lecr == FALSE)) %>%
    # mutate(
    #   order_ees = case_when(
    #     lecr == TRUE ~ 0,
    #     eecr == TRUE ~ 1,
    #     TRUE ~ lookback + 1)) %>%
    client_enrollments_reactive_dt[
      !(lookback == 0 & eecr == FALSE & lecr == FALSE),
      order_ees := fifelse(lecr == TRUE, 0, 
                           fifelse(eecr == TRUE, 1, lookback + 1))
    ][
    # group_by(PersonalID) %>%
    # arrange(desc(order_ees), .by_group = TRUE) %>%
    # mutate(
    #   days_to_next_entry =
    #     difftime(lead(EntryDate, order_by = EntryDate),
    #              ExitAdjust, units = "days")) %>%
    # ungroup() %>%
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
            ProjectType == ce_project_type &
              (EnrollmentID %in% homeless_cls_finder(ReportStart(), "before", 90) |
                 (between(EntryDate, ReportStart() - days(90), ReportStart()) &
                    lh_prior_livingsituation == TRUE))
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
        ExitAdjust <= ReportEnd(), 
      
      temp_dest_lecr = lecr == TRUE &
        !(Destination %in% perm_livingsituation) &
        ExitAdjust <= ReportEnd(),
      
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
})

# Enrollment-level universe with client-level flags -----------------------
# Need to keep it enrollment-level so other scripts can reference the enrollments
universe_ppl_flags <- reactive({
  # universe() %>%
  #   group_by(PersonalID) %>%
  #   filter(max(lecr, na.rm = TRUE) == 1 & max(eecr, na.rm = TRUE) == 1) %>%
  #   # drops ppl w/o an eecr or lecr
  #   mutate(
  universe()[, .SD[ # Subset by group (PersonalID) and filter rows based on conditions
    max(lecr, na.rm = TRUE) == 1 & max(eecr, na.rm = TRUE) == 1], 
    by = PersonalID][, `:=`(
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
})

# Client-level enrollment summary data reactive ---------------------------
# get final people-level, inflow/outflow dataframe by joining the filtered 
# enrollment and people dfs, as well as flagging their inflow and outflow types
inflow_outflow_df <- reactive({
  
  exportTestValues(universe_ppl_flags = universe_ppl_flags() %>% nice_names())
  
  plot_data <- universe_ppl_flags() %>%
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
