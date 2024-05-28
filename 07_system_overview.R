logToConsole("Running system overview")

# Age ---------------------------------------------------------------------
browser()
system_person_ages <- EnrollmentAdjust %>%
  group_by(PersonalID) %>%
  slice_max(AgeAtEntry, na_rm = TRUE, with_ties = FALSE) %>%
  ungroup() %>%
  select(PersonalID, "MostRecentAgeAtEntry" = AgeAtEntry)

# Build report dates ------------------------------------------------------

# if the start date's day of the month = 1, then that's the start date
# otherwise go forward a month and use the 1st of that month.
ReportStart <- if_else(
  day(meta_HUDCSV_Export_Start()) == 1,
  meta_HUDCSV_Export_Start(),
  floor_date(meta_HUDCSV_Export_Start() %m+% months(1), unit = "month"))

# if you go forward to the first day of the next month and then subtract a day,
# and that equals the raw ExportEndDate, that means it is already a last day of
# the month so we just return the raw ExportEndDate. If the date is something
# other than that, then we want to get the first day of the month and go back 
# a day so that it cuts off on the last day of the month previous to the raw
# ExportEndDate
ReportEnd <- if_else(
  floor_date(meta_HUDCSV_Export_End() %m+% months(1), unit = "month") - days(1) ==
    meta_HUDCSV_Export_End(),
  meta_HUDCSV_Export_End(),
  floor_date(meta_HUDCSV_Export_End(), unit = "month") - days(1))

# Data prep ---------------------------------------------------------------

# using EnrollmentAdjust because that df doesn't contain enrollments that fall
# outside periods of operation/participation
system_df_prep <- EnrollmentAdjust %>%
  select(EnrollmentID,
         PersonalID,
         ProjectID,
         ProjectType,
         HouseholdID,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ExitAdjust,
         EnrollmentDateRange,
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

# corrected hohs ----------------------------------------------------------

hh_adjustments <- system_df_prep %>%
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
  select(EnrollmentID, CorrectedHoH)

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
system_df_prep <- system_df_prep %>%
  left_join(hh_adjustments, join_by(EnrollmentID)) %>%
  relocate(CorrectedHoH, .after = RelationshipToHoH)

rm(hh_adjustments)


# Enrollment-level flags --------------------------------------------------
# will help us categorize enrollments
system_df_enrl_flags <- system_df_prep %>%
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
      )
    ,
    lh_at_entry = lh_prior_livingsituation == TRUE |
      ProjectType %in% lh_project_types,
    EnrolledHomeless = ProjectType %in% project_types_enrolled_homeless |
      lh_prior_livingsituation == TRUE,
    
    # Domestic Violence - this is needed for the System Composition chart
    DomesticViolence = case_when(
      DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 0 ~ syso_spec_pops_people[2],
      DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1 ~ syso_spec_pops_people[3],
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
    EnrolledHomeless,
    LivingSituation,
    LOSUnderThreshold,
    PreviousStreetESSH,
    Destination,
    EnrollmentDateRange,
    AgeAtEntry,
    MostRecentAgeAtEntry,
    CorrectedHoH,
    DomesticViolence,
    CurrentlyFleeing,
    DomesticViolenceSurvivor
  ) %>%
  group_by(HouseholdID) %>%
  mutate(
    HouseholdType = case_when(
      all(AgeAtEntry < 25 & AgeAtEntry >= 18, na.rm = TRUE) &
        !any(is.na(AgeAtEntry)) ~ "Youth and Young Adult",
      all(AgeAtEntry >= 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~
        "Adult-Only",
      any(AgeAtEntry < 18, na.rm = TRUE) & any(AgeAtEntry >= 18, na.rm = TRUE) ~
        "Adult-Child",
      all(AgeAtEntry < 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~
        "Child-Only",
      TRUE ~ "Unknown Household"
    )
  ) %>% 
  ungroup()

# Client-level flags ------------------------------------------------------
# will help us categorize people

system_df_client_flags <- reactive({
  Client %>%
    select(PersonalID,
           all_of(race_cols),
           all_of(gender_cols),
           VeteranStatus
           ) %>%
    left_join(system_person_ages, join_by(PersonalID)) %>%
    mutate(
      Age = case_when(
        MostRecentAgeAtEntry >= 0 & MostRecentAgeAtEntry <= 12 ~
          syso_age_cats["0 to 12"],
        MostRecentAgeAtEntry >= 13 & MostRecentAgeAtEntry <= 17 ~
          syso_age_cats["13 to 17"],
        MostRecentAgeAtEntry >= 18 & MostRecentAgeAtEntry <= 21 ~
          syso_age_cats["18 to 21"],
        MostRecentAgeAtEntry >= 22 & MostRecentAgeAtEntry <= 24 ~
          syso_age_cats["22 to 24"],
        MostRecentAgeAtEntry >= 25 & MostRecentAgeAtEntry <= 34 ~
          syso_age_cats["25 to 34"],
        MostRecentAgeAtEntry >= 35 & MostRecentAgeAtEntry <= 44 ~
          syso_age_cats["35 to 44"],
        MostRecentAgeAtEntry >= 45 & MostRecentAgeAtEntry <= 54 ~
          syso_age_cats["45 to 54"],
        MostRecentAgeAtEntry >= 55 & MostRecentAgeAtEntry <= 64 ~
          syso_age_cats["55 to 64"],
        MostRecentAgeAtEntry >= 65 & MostRecentAgeAtEntry <= 74 ~
          syso_age_cats["65 to 74"],
        MostRecentAgeAtEntry >= 75 ~
          syso_age_cats["75 and older"]
      ),
      
      Gender = case_when(
        # Exclusive ---
        input$methodology_type == 1 &
          any_cols_selected_except(., 
                                   c(
                                     CulturallySpecific,
                                     NonBinary,
                                     Questioning,
                                     DifferentIdentity
                                   ), 
                                   "Transgender"
          ) &
          any_cols_selected_except(., gender_cols, c("GenderNone","Transgender")) 
        ~ syso_gender_excl["Gender Expansive, not including transgender"],
        
        input$methodology_type == 1 & 
          no_cols_selected_except(., gender_cols, "Man") 
        ~ syso_gender_excl["Man (Boy, if child) alone"],
        
        input$methodology_type == 1 & Transgender == 1
        ~ syso_gender_excl["Transgender, alone or in combination"],
        
        input$methodology_type == 1 & 
          no_cols_selected_except(., gender_cols, "Woman")
        ~ syso_gender_excl["Woman (Girl, if child) alone"],  
        
        input$methodology_type == 1 & 
          no_cols_selected_except(., gender_cols, "GenderNone")
        ~ syso_gender_excl["Unknown"],
        
        # Inclusive ----
        input$methodology_type == 2 & (
          (Woman == 1 & Man == 1) |
            min_cols_selected_except(., gender_cols, c("Man","Woman"), 1)
        ) ~ syso_gender_incl["Gender Expansive, including transgender"],
        
        input$methodology_type == 2 & Man == 1
        ~ syso_gender_incl["Man (Boy, if child) alone or in combination"],
        
        input$methodology_type == 2 & NonBinary == 1
        ~ syso_gender_incl["Non-Binary alone or in combination"],      
        
        input$methodology_type == 2 & (
          (Man == 1 & Woman != 1) |
            (Woman == 1 & Man != 1)
        ) ~ syso_gender_incl["Only Woman (Girl, if child) OR Only Man (Boy, if child)"],
        
        input$methodology_type == 2 & Woman == 1
        ~ syso_gender_incl["Woman (Girl, if child) alone or in combination"]
      ),
      
      AllRaceEthnicity = case_when(
        # Exclusive
        # American Indian, Alaska Native, or Indigenous Alone" = 1,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, "AmIndAKNative")
        ~ syso_race_ethnicity_excl[["Group 1"]][1],
        
        # American Indian, Alaska Native, or Indigenous & Hispanic/Latina/e/o" = 2,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, c("AmIndAKNative", "HispanicLatinaeo"))
        ~ syso_race_ethnicity_excl[["Group 1"]][2],
        
        # Asian or Asian American Alone" = 3,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, "Asian")
        ~ syso_race_ethnicity_excl[["Group 1"]][3],
        
        # Asian or Asian American & Hispanic/Latina/e/o" = 4,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, c("Asian", "HispanicLatinaeo"))
        ~ syso_race_ethnicity_excl[["Group 1"]][4],
        
        # Black, African American, or African Alone" = 5,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, "BlackAfAmerican")
        ~ syso_race_ethnicity_excl[["Group 1"]][5],  
        
        # Black, African American, or African & Hispanic/Latina/e/o" = 6,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, c("BlackAfAmerican",
                                                  "HispanicLatinaeo"))
        ~ syso_race_ethnicity_excl[["Group 1"]][6],
        
        # Hispanic/Latina/e/o Alone" = 7,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, "HispanicLatinaeo")
        ~ syso_race_ethnicity_excl[["Group 1"]][7],
        
        # Middle Eastern or North African Alone" = 8,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, "MidEastNAfrican")
        ~ syso_race_ethnicity_excl[["Group 1"]][8],
        
        # Middle Eastern or North African & Hispanic/Latina/e/o" = 9,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, c("MidEastNAfrican",
                                                  "HispanicLatinaeo"))
        ~ syso_race_ethnicity_excl[["Group 1"]][9],
        
        # Native Hawaiin or Pacific Islander Alone" = 10,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, "NativeHIPacific")
        ~ syso_race_ethnicity_excl[["Group 1"]][10],
        
        # Native Hawaiin or Pacific Islander & Hispanic/Latina/e/o" = 11,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, c("NativeHIPacific",
                                                  "HispanicLatinaeo"))
        ~ syso_race_ethnicity_excl[["Group 1"]][11],
        
        # White Alone" = 12,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, "White")
        ~ syso_race_ethnicity_excl[["Group 1"]][12],
        
        # White & Hispanic/Latina/e/o" = 13,
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, c("White", "HispanicLatinaeo"))
        ~ syso_race_ethnicity_excl[["Group 1"]][13],
        
        # Multi-Racial (not Hispanic/Latina/e/o)" = 14,
        input$methodology_type == 1 & 
          min_cols_selected_except(., race_cols, c("RaceNone",
                                                   "HispanicLatinaeo"), 2)
        ~ syso_race_ethnicity_excl[["Group 1"]][14],
        
        # Multi-Racial & Hispanic/Latina/e/o" = 15),
        input$methodology_type == 1 & 
          min_cols_selected_except(., race_cols, "RaceNone", 2)
        ~ syso_race_ethnicity_excl[["Group 1"]][15],
        
        # Inclusive
        #American Indian, Alaska Native, or Indigenous Inclusive" = 1,
        input$methodology_type == 2 & AmIndAKNative == 1
        ~ syso_race_ethnicity_incl[["Group 1"]][1],
        
        # Asian or Asian American Inclusive" = 2,
        input$methodology_type == 2 & Asian == 1
        ~ syso_race_ethnicity_incl[["Group 1"]][2],
        
        # Black, African American, or African Inclusive" = 3,
        input$methodology_type == 2 & BlackAfAmerican == 1
        ~ syso_race_ethnicity_incl[["Group 1"]][3],
        
        # Hispanic/Latina/e/o = 4
        input$methodology_type == 2 & HispanicLatinaeo == 1
        ~ syso_race_ethnicity_incl[["Group 1"]][4],
        
        # Middle Eastern Inclusive = 5
        input$methodology_type == 2 & MidEastNAfrican == 1
        ~ syso_race_ethnicity_incl[["Group 1"]][5],
        
        # Native Hawaiin or Pacific Islander Inclusive" = 6,
        input$methodology_type == 2 & NativeHIPacific == 1
        ~ syso_race_ethnicity_incl[["Group 1"]][6],
        
        # White Inclusive" = 7),
        input$methodology_type == 2 & White == 1
        ~ syso_race_ethnicity_incl[["Group 1"]][7]
        
      ),
      GroupedRaceEthnicity = case_when(
        # All People of Color" = 16 or Group 2, item 1
        input$methodology_type == 1 & 
          !no_cols_selected_except(., race_cols, c("White","RaceNone")) > 0
        ~ syso_race_ethnicity_excl[["Group 2"]][1],
        
        # White Only" = 17, or Group 2, item 2
        input$methodology_type == 1 & 
          no_cols_selected_except(., race_cols, "White")
        ~ syso_race_ethnicity_excl[["Group 2"]][2],
        
        # Black, African American or African and Hispanic/Latina/e/o Inclusive" = 8,
        #  or Group 2, item 1
        input$methodology_type == 2 & (BlackAfAmerican == 1 | HispanicLatinaeo == 1)
        ~ syso_race_ethnicity_incl[["Group 2"]][1],
        
        # Hispanic/Latina/e/o Inclusive" = 9 or Group 2, item 2
        input$methodology_type == 2 & HispanicLatinaeo == 1
        ~ syso_race_ethnicity_incl[["Group 2"]][2],
        
        # Hispanic/Latina/e/o Alone" = 10 or Group 2, item 3
        input$methodology_type == 2 &
          no_cols_selected_except(., race_cols, "HispanicLatinaeo")
        ~ syso_race_ethnicity_incl[["Group 2"]][3],
      ),
    )
})

# Enrollment-level reactive -----------------------------------------------

outreach_w_proper_cls <- reactive({
  CurrentLivingSituation %>%
    filter(CurrentLivingSituation %in% homeless_livingsituation &
             between(InformationDate,
                     input$syso_date_range[2] - days(60),
                     input$syso_date_range[2] + days(60))) %>%
    pull(EnrollmentID) %>%
    unique()
})

system_df_enrl_filtered <- reactive({
  nbn_enrollments_services <- Services %>%
    filter(RecordType == 200) %>%
    inner_join(EnrollmentAdjust %>%
                filter(ProjectType == 1) %>%
                select(EnrollmentID),
              join_by(EnrollmentID)) %>%
    mutate(
      nbn_service_within15_start =
        between(DateProvided,
                input$syso_date_range[1] - days(15),
                input$syso_date_range[1] + days(15)),
      nbn_service_within15_end =
        between(DateProvided,
                input$syso_date_range[2] - days(15),
                input$syso_date_range[2] + days(15))
    ) %>%
    filter(
      nbn_service_within15_start == TRUE |
        nbn_service_within15_end == TRUE) %>%
    select(EnrollmentID,
           nbn_service_within15_start,
           nbn_service_within15_end)
  
  nbn_enrollments_w_proper_services <-  nbn_enrollments_services %>%
    pull(EnrollmentID) %>%
    unique()
  
  # Perform left join
  system_df_enrl_flags_dt <- as.data.table(nbn_enrollments_services)[
    as.data.table(
      system_df_enrl_flags %>% 
        mutate(
          EnrollmentDateRangeStart = int_start(EnrollmentDateRange),
          EnrollmentDateRangeEnd = int_end(EnrollmentDateRange),
        ) %>%
        select(-EnrollmentDateRange)
    ), on=.(EnrollmentID)]
  
  # Filter data
  system_df_enrl_flags_dt <- system_df_enrl_flags_dt[
    as.numeric(difftime(ExitAdjust, input$syso_date_range[1], unit = "days")) / 365 <= 2 &
      ProjectType != 12 &
      (input$syso_hh_type == 1 | HouseholdType == getNameByValue(syso_hh_types, input$syso_hh_type)) &
      (input$syso_level_of_detail == 1 |
         (input$syso_level_of_detail == 2 & (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
         (input$syso_level_of_detail == 3 & CorrectedHoH == 1)) &
      (input$syso_project_type == 1 |
         (input$syso_project_type == 2 & ProjectType %in% project_types_w_beds) |
         (input$syso_project_type == 3 & ProjectType %in% non_res_project_types))
  ]
  
  # Group by PersonalID and arrange by EntryDate
  system_df_enrl_flags_dt[, `:=`(
    ordinal = seq_len(.N),
    next_entry_days = difftime(c(EntryDate[-1], NA), ExitAdjust, units = "days"),
    previous_exit_days = difftime(EntryDate, c(ExitAdjust[-1], NA), units = "days")
  ), by = PersonalID]
  
  # Mutate new columns
  system_df_enrl_flags_dt[, `:=`(
    straddles_start = EntryDate <= input$syso_date_range[1] & ExitAdjust >= input$syso_date_range[1],
    in_date_range = 
      (
        EnrollmentDateRangeStart <= input$syso_date_range[1] & 
          EnrollmentDateRangeEnd > input$syso_date_range[1]
      ) |
      (
        EnrollmentDateRangeStart <= input$syso_date_range[2] & 
          EnrollmentDateRangeEnd > input$syso_date_range[2]
      ) |
      (
        EnrollmentDateRangeStart >= input$syso_date_range[1] & 
          EnrollmentDateRangeEnd <= input$syso_date_range[2]
      )
  )]
  
  # Group by PersonalID and in_date_range and mutate new columns
  system_df_enrl_flags_dt[, `:=`(
    lecr = in_date_range == TRUE & max(ordinal) == ordinal,
    eecr = in_date_range == TRUE & min(ordinal) == ordinal,
    lookback = ifelse(in_date_range == TRUE, 0, rev(seq_len(.N)))
  ), by = .(PersonalID, in_date_range)]
  
  
  # system_df_enrl_flags %>%
  #   left_join(nbn_enrollments_services, join_by(EnrollmentID)) %>%
  #   filter(
  #   # remove enrollments where the exit is over 2 years prior to report start
  #     as.numeric(difftime(ExitAdjust, input$syso_date_range[1],
  #                         unit = "days")) / 365 <= 2 &
  #   
  #   # excluding these because Project Type 12 is Homelessness Prevention. 
  #   # Households in that project are technically already housed
  #   ProjectType != 12 &
  #     
  #   # Active At Start logic
  #   # ((ProjectType %in% c(es_ee_project_type, th_project_type, sh_project_type)) |
  #   #   (ProjectType == es_nbn_project_type &
  #   #      EnrollmentID %in% nbn_enrollments_w_proper_services) |
  #   #   (ProjectType == out_project_type &
  #   #      EnrollmentID %in% outreach_w_proper_cls() &
  #   #      lh_prior_livingsituation == TRUE) |
  #   #   (ProjectType %in% c(ph_project_types) &
  #   #      (is.na(MoveInDateAdjust) |
  #   #         MoveInDateAdjust > input$syso_date_range[1]) &
  #   #      lh_prior_livingsituation == TRUE) |
  #     # (ProjectType == ce_project_type &
  #     #    lh_prior_livingsituation == TRUE &
  #     #    between(EntryDate,
  #     #            input$syso_date_range[1] - days(90),
  #     #            input$syso_date_range[1] + days(90)))) &
  # 
  #   # Household Type
  #   (
  #     # "All Households" = 1, 
  #     input$syso_hh_type == 1 |
  #       HouseholdType == getNameByValue(syso_hh_types, input$syso_hh_type)
  #   ) & 
  #     # Level of Detail
  #     (
  #       input$syso_level_of_detail == 1 |
  #       (input$syso_level_of_detail == 2 & 
  #          (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
  #       (input$syso_level_of_detail == 3 & CorrectedHoH == 1)
  #     ) & 
  #     # Project Type
  #     (
  #       input$syso_project_type == 1 |
  #         (input$syso_project_type == 2 &
  #            ProjectType %in% project_types_w_beds) |
  #         (input$syso_project_type == 3 &
  #            ProjectType %in% non_res_project_types)
  #     )
  #   ) %>%
  #   group_by(PersonalID) %>%
  #   arrange(EntryDate, .by_group = TRUE) %>%
  #   mutate(ordinal = row_number(),
  #          next_entry_days = 
  #            difftime(lead(EntryDate, order_by = EntryDate), # next date
  #                     ExitAdjust, # enrollment date
  #                     unit = "days"),
  #          previous_exit_days =
  #            difftime(EntryDate, # enrollment date
  #                     lag(ExitAdjust, order_by = ExitAdjust), # previous date
  #                     unit = "days")) %>%
  #   ungroup() %>%
  #   mutate(
  #     straddles_start =
  #       EntryDate <= input$syso_date_range[1] &
  #       ExitAdjust >= input$syso_date_range[1],
  #     in_date_range =
  #       int_overlaps(EnrollmentDateRange,
  #                    interval(input$syso_date_range[1], input$syso_date_range[2])
  #       )) %>%
  #   group_by(PersonalID, in_date_range) %>%
  #   mutate(
  #     lecr = in_date_range == TRUE & max(ordinal) == ordinal,
  #     eecr = in_date_range == TRUE & min(ordinal) == ordinal,
  #     lookback = if_else(in_date_range == TRUE, 0, rev(row_number()))
  #   ) %>%
  #   ungroup()
})

# Client-level reactive ---------------------------------------------------
# get filtered people-level system dataframe
system_df_people_universe_filtered <- reactive({
  system_df_enrl_filtered() %>%
    filter(in_date_range == TRUE) %>%
    select(-MostRecentAgeAtEntry) %>%
    left_join(system_df_client_flags(), join_by(PersonalID))
})

system_df_people_syso_filtered <- reactive({
  system_df_people_universe_filtered() %>%
    filter(
      # Age
      (
        setequal(syso_age_cats, input$syso_age) |
          is.null(input$syso_age) |
          Age %in% input$syso_age
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
        (input$syso_spec_pops == 2 & DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 0) |
        (input$syso_spec_pops == 3 & DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1) |
        (input$syso_spec_pops == 4 & DomesticViolenceSurvivor == 1)
      ) &
      # Gender
      (
        setequal(syso_gender_cats(), input$syso_gender) |
          is.null(input$syso_gender) |
          Gender %in% input$syso_gender
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
# get final people-level, inflow/outflow dataframe by joining the filtered----- 
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
              MoveInDateAdjust > input$syso_date_range[1]
            )
          ) |
          # Otherwise, EnrolledHomeless = TRUE (includes 0,1,2,4,8,14 and 6,11)
          (
            ProjectType == ce_project_type &
            lh_prior_livingsituation == TRUE &
              between(EntryDate,
                      input$syso_date_range[1] - days(90),
                      input$syso_date_range[1] + days(90))
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
           (EntryDate >= input$syso_date_range[1] &
              between(difftime(EntryDate, input$syso_date_range[1],
                               units = "days"),
                      0,
                      14) &
              !is.na(previous_exit_days) &
              between(as.numeric(previous_exit_days), 0, 14))),
      
      #LOGIC: enrolled housed at start
      # Exit.ExitDate is null or > ReportStartDate AND
    
      # Project.ProjectType IN (3, 9, 10, 13) AND
      # Enrollment.MoveInDate is !NULL OR <= ReportStartDate AND
      # Enrollment.LivingSituation is LiterallyHomeless*"
      active_at_start_housed = eecr == TRUE & 
        ProjectType %in% ph_project_types & 
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust <= input$syso_date_range[1] &
        lh_prior_livingsituation == TRUE,
      
      # LOGIC helper columns for outflow
      
      lookback1_perm_dest = lookback == 1 & 
        Destination %in% perm_destinations,
      
      eecr_lh_at_entry = eecr == TRUE &
        lh_at_entry == TRUE,
      
      at_least_14_days_to_eecr_enrl = lookback == 1 & 
        next_entry_days >= 14,
      
      lookback1_temp_dest = lookback == 1 & 
        !(Destination %in% perm_destinations),
      
      # outflow columns
      perm_dest_lecr = lecr == TRUE &
        Destination %in% perm_destinations &
        ExitAdjust < input$syso_date_range[2], # 
      
      temp_dest_lecr = lecr == TRUE &
        !(Destination %in% perm_destinations) &
        ExitAdjust < input$syso_date_range[2],
      
      homeless_at_end = lecr == TRUE & # REVISIT GD, CHECK LOGIC
        EntryDate <= input$syso_date_range[2] &
        ExitAdjust > input$syso_date_range[2] & 
        ( # 1
          ProjectType %in% lh_project_types_nc |
            
          # 2
          (ProjectType == es_nbn_project_type & nbn_service_within15_end == TRUE) |
         
          # 3
          (ProjectType == out_project_type & 
            EnrollmentID %in% outreach_w_proper_cls()) |
          
          # 4
          (ProjectType %in% ph_project_types &
          (is.na(MoveInDateAdjust) | MoveInDateAdjust >= input$syso_date_range[2]) &
          lh_prior_livingsituation == TRUE) 
        ),

      housed_at_end = lecr == TRUE & 
        EntryDate <= input$syso_date_range[2] &
        ExitAdjust > input$syso_date_range[2] &
        ProjectType %in% ph_project_types & 
        !is.na(MoveInDateAdjust) &
        MoveInDateAdjust <= input$syso_date_range[2] &
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

sys_df_people_universe_filtered_r(system_df_people_universe_filtered)
sys_inflow_outflow_plot_data(sys_inflow_outflow_plot_df)
