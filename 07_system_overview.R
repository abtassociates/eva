logToConsole("Running system overview")


# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
# Define the hardcoded values for x.axis.var and cat.var
# we need all combinations for the 0s
active_as_of_start <<- reactive({
  paste0("Active as of \n", input$syso_date_range[1])
})

active_as_of_end <<- reactive({
  paste0("Active as of \n", input$syso_date_range[2])
})

active_at_vals <<- c("Homeless", "Housed")

x.axis.var_summary_values <<- reactive({
  
  c(
    active_as_of_start(),
    "Inflow",
    "Outflow",
    active_as_of_end()
  )
})

x.axis.var_detail_values <<- reactive({
  c(
    active_as_of_start(),
    "Newly Homeless", 
    "Returned from \nPermanent", 
    "Re-engaged from \nNon-Permanent",
    "Continued system \nengagement",
    "Permanent Destination",
    "Non-Permanent \nDestination",
    active_as_of_end()
  )
})

cat.var_summary_values <<- c(
  "Homeless", 
  "Housed", 
  "Inflow", 
  "Outflow"
)

cat.var_detail_values <<- c(
  "Homeless", 
  "Housed", 
  "Newly Homeless", 
  "Returned from \nPermanent", 
  "Re-engaged from \nNon-Permanent",
  "Continued system \nengagement",
  "Permanent Destination",
  "Non-Permanent \nDestination"
)

# Data prep ---------------------------------------------------------------

system_person_ages <<- EnrollmentAdjust %>%
  group_by(PersonalID) %>%
  slice_max(AgeAtEntry, na_rm = TRUE, with_ties = FALSE) %>%
  ungroup() %>%
  select(PersonalID, "MostRecentAgeAtEntry" = AgeAtEntry)

# using EnrollmentAdjust because that df doesn't contain enrollments that fall
# outside periods of operation/participation
system_df_prep <<- EnrollmentAdjust %>%
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

hh_adjustments <<- system_df_prep %>%
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
system_df_prep <<- system_df_prep %>%
  left_join(hh_adjustments, join_by(EnrollmentID)) %>%
  relocate(CorrectedHoH, .after = RelationshipToHoH)

rm(hh_adjustments)


# Enrollment-level flags --------------------------------------------------
# will help us categorize enrollments
system_df_enrl_flags <<- system_df_prep %>%
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
      lh_prior_livingsituation == TRUE
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
    CorrectedHoH
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

system_df_client_flags <<- Client %>%
  select(PersonalID,
         all_of(race_cols),
         all_of(gender_cols),
         VeteranStatus
         ) %>%
  left_join(system_person_ages, join_by(PersonalID))

# Enrollment-level reactive -----------------------------------------------

outreach_w_proper_cls <<- reactive({
  CurrentLivingSituation %>%
    filter(CurrentLivingSituation %in% homeless_livingsituation &
             between(InformationDate,
                     input$syso_date_range[2] - days(60),
                     input$syso_date_range[2] + days(60))) %>%
    pull(EnrollmentID) %>%
    unique()
})

system_df_enrl_filtered <<- reactive({
  # browser()
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
  
  system_df_enrl_flags %>%
    left_join(nbn_enrollments_services, join_by(EnrollmentID)) %>%
    filter(
    # remove enrollments where the exit is over 2 years prior to report start
      as.numeric(difftime(ExitAdjust, input$syso_date_range[1],
                          unit = "days")) / 365 <= 2 &
    
    # excluding these because Project Type 12 is Homelessness Prevention. 
    # Households in that project are technically already housed
    ProjectType != 12 &
      
    # Active At Start logic
    # ((ProjectType %in% c(es_ee_project_type, th_project_type, sh_project_type)) |
    #   (ProjectType == es_nbn_project_type &
    #      EnrollmentID %in% nbn_enrollments_w_proper_services) |
    #   (ProjectType == out_project_type &
    #      EnrollmentID %in% outreach_w_proper_cls() &
    #      lh_prior_livingsituation == TRUE) |
    #   (ProjectType %in% c(ph_project_types) &
    #      (is.na(MoveInDateAdjust) |
    #         MoveInDateAdjust > input$syso_date_range[1]) &
    #      lh_prior_livingsituation == TRUE) |
      # (ProjectType == ce_project_type &
      #    lh_prior_livingsituation == TRUE &
      #    between(EntryDate,
      #            input$syso_date_range[1] - days(90),
      #            input$syso_date_range[1] + days(90)))) &

    # Household Type
    (
      # "All Households" = 1, 
      input$syso_hh_type == 1 |
        HouseholdType == getNameByValue(syso_hh_types, input$syso_hh_type)
    ) & 
      # Level of Detail
      (
        input$syso_level_of_detail == 1 |
        (input$syso_level_of_detail == 2 & 
           (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
        (input$syso_level_of_detail == 3 & CorrectedHoH == 1)
      ) & 
      # Project Type
      (
        input$syso_project_type == 1 |
          (input$syso_project_type == 2 &
             ProjectType %in% project_types_w_beds) |
          (input$syso_project_type == 3 &
             ProjectType %in% non_res_project_types)
      )
    ) %>%
    group_by(PersonalID) %>%
    arrange(EntryDate, .by_group = TRUE) %>%
    mutate(ordinal = row_number(),
           next_entry_days = 
             difftime(lead(EntryDate, order_by = EntryDate), # next date
                      ExitAdjust, # enrollment date
                      unit = "days"),
           previous_exit_days =
             difftime(EntryDate, # enrollment date
                      lag(ExitAdjust, order_by = ExitAdjust), # previous date
                      unit = "days")) %>%
    ungroup() %>%
    mutate(
      straddles_start =
        EntryDate <= input$syso_date_range[1] &
        ExitAdjust >= input$syso_date_range[1],
      in_date_range =
        int_overlaps(EnrollmentDateRange,
                     interval(input$syso_date_range[1], input$syso_date_range[2])
        )) %>%
    group_by(PersonalID, in_date_range) %>%
    mutate(
      lecr = in_date_range == TRUE & max(ordinal) == ordinal,
      eecr = in_date_range == TRUE & min(ordinal) == ordinal,
      lookback = if_else(in_date_range == TRUE, 0, rev(row_number()))
    ) %>%
    ungroup()
})

# Population reactives ----------------------------------------------------

# Set race/ethnicity + gender filter options based on methodology type selection
# Set special populations options based on level of detail selection
syso_race_ethnicity_cats <<- reactive({
  ifelse(
    input$methodology_type == 1,
    list(syso_race_ethnicity_excl),
    list(syso_race_ethnicity_incl)
  )[[1]]
})

syso_gender_cats <<- reactive({
  ifelse(
    input$methodology_type == 1,
    list(syso_gender_excl),
    list(syso_gender_incl)
  )[[1]]
})

syso_spec_pops_cats <<- reactive({
  ifelse(
    input$syso_level_of_detail %in% c(1,2),
    list(syso_spec_pops_people),
    list(syso_spec_pops_hoh)
  )[[1]]
})

# Client-level reactive ---------------------------------------------------
# get filtered people-level system dataframe
system_df_people_filtered <<- reactive({
  
  clients_in_report_date_range <- system_df_enrl_filtered() %>%
    filter(in_date_range == TRUE) %>%
    pull(PersonalID) %>% unique()
  
  system_df_client_flags %>%
    filter(
      PersonalID %in% c(clients_in_report_date_range) &
      # Age
      (
        setequal(syso_age_cats, input$syso_age) |
          (MostRecentAgeAtEntry >= 0 & MostRecentAgeAtEntry <= 12 &
             syso_age_cats["0 to 12"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 13 & MostRecentAgeAtEntry <= 17 &
             syso_age_cats["13 to 17"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 18 & MostRecentAgeAtEntry <= 20 &
             syso_age_cats["18 to 21"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 21 & MostRecentAgeAtEntry <= 24 &
             syso_age_cats["21 to 24"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 25 & MostRecentAgeAtEntry <= 34 &
             syso_age_cats["25 to 34"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 35 & MostRecentAgeAtEntry <= 44 &
             syso_age_cats["35 to 44"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 45 & MostRecentAgeAtEntry <= 54 &
             syso_age_cats["45 to 54"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 55 & MostRecentAgeAtEntry <= 64 &
             syso_age_cats["55 to 64"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 65 & MostRecentAgeAtEntry <= 74 &
             syso_age_cats["65 to 74"] %in% input$syso_age) |
          (MostRecentAgeAtEntry >= 75 &
             syso_age_cats["75 and older"] %in% input$syso_age)
      ) &
      # Special Populations
      (
        input$syso_spec_pops == 1 # no special populations (all)
        
        # # People
        # input$syso_level_of_detail %in% c(1,2) & (
        #   input$syso_spec_pops == 2 & TRUE
        # ) |
        # # Households
        # !(input$syso_level_of_detail %in% c(1,2)) & (
        #   input$syso_spec_pops == 2 & TRUE
        # )
      ) &
      # # Gender
      # (
      #   # Exclusive
      #   (input$methodology_type == 1 & (
      #     # All genders
      #     input$syso_gender == 1 |
      # 
      #     # Gender diverse/expansive, not including transgender
      #     (input$syso_gender == 2 &
      #        any_cols_selected_except(., c(
      #          CulturallySpecific,
      #          NonBinary,
      #          Questioning,
      #          DifferentIdentity), "Transgender") &
      #        any_cols_selected_except(., gender_cols, c("GenderNone","Transgender"))
      #     ) |
      # 
      # 
      #     # Man alone
      #     (input$syso_gender == 3 &
      #        no_cols_selected_except(., gender_cols, "Man")) |
      # 
      #     # Transgender, alone or in combo
      #     (input$syso_gender == 4 & Transgender == 1) |
      # 
      #     # Woman, alone
      #     (input$syso_gender == 5 &
      #        no_cols_selected_except(., gender_cols, "Woman")) |
      # 
      #     # Unknown
      #     (input$syso_gender == 6 &
      #        no_cols_selected_except(., gender_cols, "GenderNone"))
      #   )) |
      #   # inclusive
      #   (input$methodology_type == 2 & (
      #     # All Genders
      #     input$syso_gender == 1 |
      # 
      #     # Gender Diverse/Expansive, including transgender
      #     (input$syso_gender == 2 & (
      #       (Woman == 1 & Man == 1) |
      #       min_cols_selected_except(., gender_cols, c("Man","Woman"), 1)
      #     )) |
      # 
      #     # Man (Boy, if child) alone or in combination" = 3,
      #     (input$syso_gender == 3 & Man == 1) |
      # 
      #     # Non-Binary alone or in combination
      #     (input$syso_gender == 4 & NonBinary == 1) |
      # 
      #     # Only Woman (Girl, if child) OR Only Man (Boy, if child)
      #     (input$syso_gender == 5 & (
      #       (Man == 1 & Woman != 1) |
      #       (Woman == 1 & Man != 1)
      #     )) |
      # 
      #     # Woman (Girl, if child) alone or in combination
      #     (input$syso_gender == 6 & Woman == 1)
      #   ))
      # ) &
      # Race/Ethnicity
      (
        # Exclusive
        (input$methodology_type == 1 & (
          # All Races/Ethnicities" = 0,
          input$syso_race_ethnicity == 0 |
            
          # American Indian, Alaska Native, or Indigenous Alone" = 1,
          (input$syso_race_ethnicity == 1 & 
             no_cols_selected_except(., race_cols, "AmIndAKNative")) |
          
          # American Indian, Alaska Native, or Indigenous & Hispanic/Latina/e/o" = 2,
          (input$syso_race_ethnicity == 2 & 
             no_cols_selected_except(., race_cols, c("AmIndAKNative", "HispanicLatinaeo"))) |
          
          # Asian or Asian American Alone" = 3,
          (input$syso_race_ethnicity == 3 & 
             no_cols_selected_except(., race_cols, "Asian")) |
          
          # Asian or Asian American & Hispanic/Latina/e/o" = 4,
          (input$syso_race_ethnicity == 4 & 
             no_cols_selected_except(., race_cols, c("Asian", "HispanicLatinaeo"))) |
          
          # Black, African American, or African Alone" = 5,
          (input$syso_race_ethnicity == 5 & 
             no_cols_selected_except(., race_cols, "BlackAfAmerican")) |
          
          # Black, African American, or African & Hispanic/Latina/e/o" = 6,
          (input$syso_race_ethnicity == 6 & 
             no_cols_selected_except(., race_cols, c("BlackAfAmerican",
                                                     "HispanicLatinaeo"))) |
          
          # Hispanic/Latina/e/o Alone" = 7,
          (input$syso_race_ethnicity == 7 & 
             no_cols_selected_except(., race_cols, "HispanicLatinaeo")) |
          
          # Middle Eastern or North African Alone" = 8,
          (input$syso_race_ethnicity == 8 & 
             no_cols_selected_except(., race_cols, "MidEastNAfrican")) |
          
          # Middle Eastern or North African & Hispanic/Latina/e/o" = 9,
          (input$syso_race_ethnicity == 9 & 
             no_cols_selected_except(., race_cols, c("MidEastNAfrican",
                                                     "HispanicLatinaeo"))) |
          
          # Native Hawaiin or Pacific Islander Alone" = 10,
          (input$syso_race_ethnicity == 10 & 
             no_cols_selected_except(., race_cols, "NativeHIPacific")) |
          
          # Native Hawaiin or Pacific Islander & Hispanic/Latina/e/o" = 11,
          (input$syso_race_ethnicity == 11 & 
             no_cols_selected_except(., race_cols, c("NativeHIPacific",
                                                     "HispanicLatinaeo"))) |
          
          # White Alone" = 12,
          (input$syso_race_ethnicity == 12 & 
             no_cols_selected_except(., race_cols, "White")) |
          
          # White & Hispanic/Latina/e/o" = 13,
          (input$syso_race_ethnicity == 13 & 
             no_cols_selected_except(., race_cols, c("White",
                                                     "HispanicLatinaeo"))) |
          
          # Multi-Racial (not Hispanic/Latina/e/o)" = 14,
          (input$syso_race_ethnicity == 14 & 
             min_cols_selected_except(., race_cols, c("RaceNone",
                                                      "HispanicLatinaeo"), 2)) |
          
          # Multi-Racial & Hispanic/Latina/e/o" = 15),
          (input$syso_race_ethnicity == 15 & 
             min_cols_selected_except(., race_cols, "RaceNone", 2)) |
          
          # All People of Color" = 16,
          (input$syso_race_ethnicity == 16 & 
             no_cols_selected_except(., race_cols, c("AmIndAKNative",
                                                     "HispanicLatinaeo"))) |
          
          # White Only" = 17
          (input$syso_race_ethnicity == 17 & 
             no_cols_selected_except(., race_cols, "White"))
        )) |
        
        # Inclusive
        (input$methodology_type == 2 & (
          # All Races/Ethnicities" = 0,
          input$syso_race_ethnicity == 0 |
            
          #American Indian, Alaska Native, or Indigenous Inclusive" = 1,
          (input$syso_race_ethnicity == 1 & AmIndAKNative == 1) |
          
          # Asian or Asian American Inclusive" = 2,
          (input$syso_race_ethnicity == 2 & Asian == 1) |
          
          # Black, African American, or African Inclusive" = 3,
          (input$syso_race_ethnicity == 3 & BlackAfAmerican == 1) |
          
          # Hispanic/Latina/e/o = 4
          (input$syso_race_ethnicity == 4 & HispanicLatinaeo == 1) |
            
          # Middle Eastern Inclusive = 5
          (input$syso_race_ethnicity == 5 & MidEastNAfrican == 1) |
          
          # Native Hawaiin or Pacific Islander Inclusive" = 6,
          (input$syso_race_ethnicity == 6 & NativeHIPacific == 1) |
          
          # White Inclusive" = 7),
          (input$syso_race_ethnicity == 7 & White == 1) |
            
          # Black, African American or African and Hispanic/Latina/e/o Inclusive" = 8,
          (input$syso_race_ethnicity == 8 & 
            (BlackAfAmerican == 1 | HispanicLatinaeo == 1)) |
            
          # Hispanic/Latina/e/o Inclusive" = 9,
          (input$syso_race_ethnicity == 9 & HispanicLatinaeo == 1) |
            
          # Hispanic/Latina/e/o Alone" = 10)
          (input$syso_race_ethnicity == 10 & 
             no_cols_selected_except(., race_cols, "HispanicLatinaeo"))
        ))
      )
    ) %>%
    select(PersonalID) %>% 
    unique()
})


# Plot prompts for plot subtitle ------------------------------------------

syso_detailBox <<- reactive({
  # remove group names from race/ethnicity filter
  # so we can use getNameByValue() to grab the selected option label
  syso_race_ethnicities <- unlist(syso_race_ethnicity_cats())
  names(syso_race_ethnicities) <- gsub("Group [0-9]+\\.", "",
                                      names(syso_race_ethnicities))
  
  list(
    strong("Date Range: "),
    input$syso_date_range[1],
    " to ",
    input$syso_date_range[2], 
    br(),
    strong("Household Type: "),
    getNameByValue(syso_hh_types, input$syso_hh_type),
    " | ",
    strong("Level of Detail: "),
    getNameByValue(syso_level_of_detail, input$syso_level_of_detail),
    " | ",
    strong("Project Type: "),
    getNameByValue(syso_project_types, input$syso_project_type), 
    br(),
    strong("Age: "),
    if_else(
      setequal(syso_age_cats, input$syso_age),
      "All Ages",
      getNameByValue(syso_age_cats, input$syso_age)
    ),
    " | ",
    strong("Gender: "),
    getNameByValue(syso_gender_cats(), input$syso_gender),
    " | ",
    strong("Race/Ethnicity: "),
    getNameByValue(syso_race_ethnicities, input$syso_race_ethnicity),
    " | ",
    strong("Special Populations: "),
    getNameByValue(syso_spec_pops_cats(), input$syso_spec_pops), 
    br(),
    strong("Methodology Type: "),
    getNameByValue(syso_methodology_types, input$methodology_type) 
  )
})

syso_chartSubheader <<- reactive({
  list(
    strong("Total Served: "), 
    formatC(
      nrow(system_df_people_filtered()),
      format = "d",
      big.mark = ","
    ),
    br()
  )
})


# Client-level enrollment summary data reactive ---------------------------
# get final people-level, inflow/outflow dataframe by joining the filtered----- 
# enrollment and people dfs, as well as flagging their inflow and outflow types
system_plot_data <<- reactive({
  system_universe_ppl() %>%
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
})
  
system_universe_enrl <<- reactive({
  # add inflow type and active enrollment typed used for system overview plots
  system_df_enrl_filtered() %>%
    inner_join(system_df_people_filtered(), by = "PersonalID") %>%
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
              between(difftime(EntryDate, input$syso_date_range[1], units = "days"), 0, 14) &
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
        MoveInDateAdjust <= input$syso_date_range[1] &
        lh_prior_livingsituation == TRUE
    )
})

system_universe_ppl <<- reactive({
  system_universe_enrl() %>%
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
})

# AS QC check:
missing_types <<- system_universe_enrl() %>% 
  inner_join(
    system_universe_ppl() %>% 
      filter(
        OutflowTypeDetail == "something's wrong" | 
          InflowTypeDetail == "something's wrong"), 
      by="PersonalID") %>% 
  mutate(
    missing_inflow = eecr & InflowTypeDetail == "something's wrong",
    missing_outflow = lecr & OutflowTypeDetail == "something's wrong",
  )

category_counts <<- system_plot_data() %>% 
  pivot_longer(
    cols = c(InflowTypeDetail, OutflowTypeDetail), 
    names_to = "x.axis.var", 
    values_to = "cat.var"
  ) %>%
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

system_activity_prep <<- reactive({
  system_plot_data() %>% # this is a people-level df
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
})

# Combine the inflow types (newly homeless, returned from permanent, re-engaged
# into one group)
system_activity_summary_prep <<- reactive({
  system_activity_prep() %>%
    mutate(
      cat.var = case_when(
        x.axis.var == "Inflow" &
          !(cat.var %in% active_at_vals)
        ~ "Inflow",
        
        x.axis.var == "Outflow" &
          !(cat.var %in% active_at_vals)
        ~ "Outflow",
        
        TRUE ~ cat.var
      )
    ) %>%
    group_by(x.axis.var, cat.var) %>%
    mutate(values = sum(values)) %>%
    ungroup() %>%
    unique()
})

# Rename the x-axis.var values to be the inflow and outflow types, 
# which are in cat.var
system_activity_detail_prep <<- reactive({
  system_activity_prep() %>%
    mutate(
      x.axis.var = ifelse(
        !(cat.var %in% active_at_vals),
        cat.var,
        x.axis.var
      )
    )
})

prep_for_chart <<- function(df, catvar_values, xvar_values) {
  # expand to make sure all combinations are included so the spacing is right
  # also factor, sort, and group for the chart
  df %>%
    right_join(
      expand.grid(x.axis.var = xvar_values,
                  cat.var = catvar_values),
      by = c("x.axis.var", "cat.var")) %>%
    replace_na(list(values = 0)) %>%
    mutate(
      x.axis.var = factor(x.axis.var, levels = xvar_values),
      cat.var = factor(cat.var, levels = catvar_values)
    ) %>%
    arrange(x.axis.var, desc(cat.var)) %>%
    group_by(x.axis.var) %>%
    mutate(group.id = cur_group_id()) %>%
    ungroup() %>%
    mutate(end.Bar = cumsum(values),
           start.Bar = c(0, head(end.Bar, -1))) %>%
    select(inflow_outflow, x.axis.var, cat.var, values, group.id, end.Bar, start.Bar)
}

renderSystemPlot <<- function(id) {
  # req(valid_file() == 1)
    if(id == "sys_act_summary_ui_chart") {
      colors <- c('#73655E','#C6BDB9','#C34931', '#16697A')
      df <- prep_for_chart(
        system_activity_summary_prep(),
        cat.var_summary_values,
        x.axis.var_summary_values()
      )
    } else {
      colors <-
        c('#73655E',
          '#C6BDB9',
          '#C34931',
          '#C34931',
          '#C34931',
          '#C34931',
          '#16697A',
          '#16697A')
      df <- prep_for_chart(
        system_activity_detail_prep(),
        cat.var_detail_values,
        x.axis.var_detail_values()
      )
    }
    
    s = max(df$end.Bar) + 20
    num_segments <- 20
    segment_size <- get_segment_size(s/num_segments)
    
    ggplot(df, aes(x = group.id, fill = cat.var)) + 
      # \_Simple Waterfall Chart ----
    geom_rect(aes(xmin = group.id - 0.25, # control bar gap width
                  xmax = group.id + 0.25, 
                  ymin = end.Bar,
                  ymax = start.Bar),
              color = "black",
              alpha = 0.8
    ) + 
      # \_Lines Between Bars ----
    geom_segment(aes(
      x = ifelse(group.id == last(group.id),
                 last(group.id),
                 group.id + 0.25),
      xend = ifelse(group.id == last(group.id),
                    last(group.id),
                    group.id + 0.75),
      y = ifelse(cat.var == last(cat.var),
                 end.Bar,
                 # these will be removed once we set the y limits
                 s + segment_size),
      yend = ifelse(cat.var == last(cat.var),
                    end.Bar,
                    # these will be removed once we set the y limits
                    s + segment_size),
      colour = "black"
    ), show.legend = FALSE) +
      # \_Numbers inside bars (each category) ----
    geom_text(
      mapping = aes(
        label = ifelse(
          !is.na(inflow_outflow) |
            as.character(x.axis.var) == as.character(cat.var),
          scales::comma(values),
          ""
        ),
        y = ifelse(abs(values) < segment_size / 4,
                   end.Bar + 10,
                   rowSums(cbind(start.Bar, values / 2)))
      ),
      color = "black",
      fontface = "bold",
      size = 4
    ) +
      # \_Change colors ----
    scale_fill_manual(values = colors) +
      # \_Change y axis to same scale as original ----
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, s + segment_size / 2)
    ) +
      # \_Add tick marks on x axis to look like the original plot ----
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(min(df$group.id) - 0.4, max(df$group.id) + 0.4),
      breaks = c(min(df$group.id) - 0.4,
                 unique(df$group.id),
                 unique(df$group.id) + 0.4),
      labels =
        c("",
          as.character(unique(df$x.axis.var)),
          rep(c(""), length(unique(df$x.axis.var))))
    ) +
      # \_Theme options to make it look like the original plot ----
    theme(
      text = element_text(size = 14, color = "#4e4d47"),
      axis.text = element_text(
        size = 10,
        color = "#4e4d47",
        face = "bold"
      ),
      axis.text.x = element_text(vjust = 1),
      axis.ticks.x = element_line(color =
                                    c(
                                      "black",
                                      rep(NA, length(unique(df$x.axis.var))),
                                      rep("black", length(unique(df$x.axis.var)) - 1)
                                    )),
      axis.line = element_line(colour = "#4e4d47", linewidth = 0.5),
      axis.ticks.length = unit(.15, "cm"),
      axis.title.x =      element_blank(),
      # hide y axis
      axis.title.y =      element_blank(),
      axis.ticks.y =      element_blank(),
      axis.line.y =       element_blank(),
      axis.text.y =       element_blank(),
      panel.background =  element_blank(),
      panel.border    =   element_blank(),
      panel.grid.major =  element_blank(),
      panel.grid.minor =  element_blank(),
      plot.background =   element_blank(),
      plot.margin =       unit(c(1, 1, 1, 1), "lines"),
      legend.text =       element_text(
        size = 10,
        color = "#4e4d47",
        face = "bold",
        margin = margin(l = 0.25, unit = "cm"
        )
      ),
      legend.title =       element_blank()
    )
    # browser()
    # system_universe_enrl <- system_universe_enrl
    # system_universe_ppl <- system_universe_ppl
    # system_df_enrl_filtered <- system_df_enrl_filtered
    # system_df_people_filtered <- system_df_people_filtered
    # syso_chartSubheader <- syso_chartSubheader
    # syso_detailBox <- syso_detailBox
    # system_df_people_filtered <- system_df_people_filtered
    # syso_spec_pops_cats <- syso_spec_pops_cats
    # syso_gender_cats <- syso_gender_cats
    # syso_race_ethnicity_cats <- syso_race_ethnicity_cats
    # outreach_w_proper_cls <- outreach_w_proper_cls
    # system_df_client_flags <- system_df_client_flags
    # system_df_enrl_flags <- system_df_enrl_flags
    # system_df_prep <- system_df_prep
    # active_at_vals <- active_at_vals
    # active_as_of_end <- active_as_of_end
    # active_as_of_start <- active_as_of_start
    # renderSystemPlot <- renderSystemPlot
    # missing_types <- missing_types
    # category_counts <- category_counts
    
    # save(list = c(ls(envir = .GlobalEnv, all.names = TRUE), ls(all.names = TRUE)), file = "waterfall.RData", compress="xz")
    
  # })
  # return(plotOutput(id, height = 400))
}

