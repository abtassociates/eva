logToConsole("Running System Overview")

system_df_prep <- EnrollmentAdjust %>%
  left_join(Project0 %>%
              select(ProjectID, ProjectName, OrganizationID, RRHSubType),
            by = "ProjectID") %>%
  left_join(Organization %>% select(OrganizationID, OrganizationName),
            by = "OrganizationID") %>%
  left_join(Client %>%
              select(PersonalID, VeteranStatus, all_of(gender_cols)), by = "PersonalID") %>%
  left_join(HealthAndDV %>%
              filter(DataCollectionStage == 1) %>%
              select(EnrollmentID, DomesticViolenceSurvivor, CurrentlyFleeing),
            by = "EnrollmentID") %>%
  select(
    EnrollmentID,
    PersonalID,
    VeteranStatus,
    OrganizationName,
    ProjectID,
    ProjectName,
    ProjectType,
    RRHSubType,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    DomesticViolenceSurvivor,
    CurrentlyFleeing,
    LivingSituation,
    RentalSubsidyType,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    AgeAtEntry,
    Destination,
    DestinationSubsidyType
  )

# corrected hohs ----------------------------------------------------------

hh_adjustments <- system_df_prep %>%
  mutate(VeteranStatus = if_else(VeteranStatus == 1 &
                                   !is.na(VeteranStatus), 1, 0),
         HoHAlready = if_else(RelationshipToHoH == 1 &
                                AgeAtEntry > 17, 1, 0)) %>%
  group_by(HouseholdID, ProjectID) %>%
  arrange(desc(HoHAlready), desc(VeteranStatus), desc(AgeAtEntry), PersonalID) %>%
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

system_df <- system_df_prep %>%
  left_join(hh_adjustments, join_by(EnrollmentID)) %>%
  relocate(CorrectedHoH, .after = RelationshipToHoH)


# Set race/ethnicity + gender filter options based on methodology type selection
# Set special populations options based on level of detail selection
syso_race_ethnicity_cats <- reactive(
  ifelse(
    input$methodology_type == "1",
    list(syso_race_ethnicity_incl),
    list(syso_race_ethnicity_excl)
  )[[1]]
)

syso_gender_cats <- reactive(
  ifelse(
    input$methodology_type == "1",
    list(syso_gender_incl),
    list(syso_gender_excl)
  )[[1]]
)

syso_spec_pops_cats <- reactive(
  ifelse(
    input$syso_level_of_detail %in% c(1,2),
    list(syso_spec_pops_people),
    list(syso_spec_pops_hoh)
  )[[1]]
)

system_activity_filtered <- reactive({
  system_df %>%
  group_by(HouseholdID) %>%
  mutate(
    Household_Type = case_when(
      all(is.na(AgeAtEntry)) ~ "Unknown Households",
      all(AgeAtEntry >= 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~ "Adult-Only",
      any(AgeAtEntry < 18, na.rm = TRUE) & any(AgeAtEntry >= 18, na.rm = TRUE) ~ "Adult-Child",
      all(AgeAtEntry < 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~ "Child-Only",
      all(AgeAtEntry < 25 & AgeAtEntry >= 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~ "Youth and Young Adult",
      any(is.na(AgeAtEntry)) ~ "Unknown Households"
    )
  ) %>% 
  ungroup() %>%
  filter(
    # Household Type
    Household_Type == input$syso_hh_types &
      
      # Level of Detail
      (
        (input$syso_level_of_detail == 1) |
          (input$syso_level_of_detail == 2 & (AgeAtEntry >= 18 | RelationshipToHoH == 1)) |
          (input$syso_level_of_detail == 3 & RelationshipToHoH == 1)
      ) & 
      
      # Project Type
      (
        input$syso_project_type == 1 & ProjectType %in% all_project_types2 |
          input$syso_project_type == 2 & ProjectType %in% project_types_w_beds |
          input$syso_project_type == 3 & ProjectType %in% non_res_project_types
      ) & 
      
      # Age
      (
        (AgeAtEntry >= 0 & AgeAtEntry <= 12 & syso_age_cats[1] %in% input$syso_age) |
          (AgeAtEntry >= 13 & AgeAtEntry <= 17 & syso_age_cats[2] %in% input$syso_age) |
          (AgeAtEntry >= 18 & AgeAtEntry <= 20 & syso_age_cats[3] %in% input$syso_age) |
          (AgeAtEntry >= 21 & AgeAtEntry <= 24 & syso_age_cats[4] %in% input$syso_age) |
          (AgeAtEntry >= 25 & AgeAtEntry <= 34 & syso_age_cats[5] %in% input$syso_age) |
          (AgeAtEntry >= 35 & AgeAtEntry <= 44 & syso_age_cats[6] %in% input$syso_age) |
          (AgeAtEntry >= 45 & AgeAtEntry <= 54 & syso_age_cats[7] %in% input$syso_age) |
          (AgeAtEntry >= 55 & AgeAtEntry <= 64 & syso_age_cats[8] %in% input$syso_age) |
          (AgeAtEntry >= 65 & AgeAtEntry <= 74 & syso_age_cats[9] %in% input$syso_age) |
          (AgeAtEntry >= 75 ~ syso_age_cats[10] %in% input$syso_age)
      ) &
      
      # Gender
      (
        # inclusive
        (input$methodology_type == "1" & (
          input$syso_gender == 1 |
            (input$syso_gender == 2 & (
              (Woman == 1 & Man == 1) | 
                !(Woman == 1 & Man == 1)
            )) |
            (input$syso_gender == 3 & Man == 1) | 
            (input$syso_gender == 4 & NonBinary == 1) | 
            (input$syso_gender == 5 & (Man == 1 | Woman == 1)) | 
            (input$syso_gender == 6 & Woman == 1) 
        )) |
          # Exclusive
          (input$methodology_type != "1" & (
            # All genders
            input$syso_gender == 1 |
              
              # Gender diverse
              (input$syso_gender == 2 & 
                 min_cols_selected_except(., gender_cols, "Transgender", 1)) |
              
              # Man alone
              (input$syso_gender == 3 & 
                 no_cols_selected_except(., gender_cols, "Man")) | 
              
              # Transgender, alone or in combo
              (input$syso_gender == 4 & Transgender == 1) | 
              
              # Woman, alone
              (input$syso_gender == 5 & 
                 no_cols_selected_except(., gender_cols, "Woman")) |
              
              # Unknown
              (input$syso_gender == 5 & 
                 no_cols_selected_except(., gender_cols, "GenderNone"))
          ))
      ) & 
      
      # Race/Ethnicity
      (
        # Inclusive
        (input$methodology_type == "1" & (
          #American Indian, Alaska Native, or Indigenous Inclusive" = 1,
          (input$syso_race_ethnicity == 1 & AmIndAKNative == 1) |
            
            # Asian or Asian American Inclusive" = 2,
            (input$syso_race_ethnicity == 2 & Asian == 1) |
            
            # Black, African American, or African Inclusive" = 3,
            (input$syso_race_ethnicity == 3 & BlackAfAmerican == 1) |
            
            # Middle Eastern Inclusive" = 4,
            (input$syso_race_ethnicity == 4 & MidEastNAfrican == 1) |
            
            # Native Hawaiin or Pacific Islander Inclusive" = 5,
            (input$syso_race_ethnicity == 5 & NativeHIPacific == 1) |
            
            # White Inclusive" = 6),
            (input$syso_race_ethnicity == 6 & White == 1) |
            
            # All People of Color" = 7,
            (input$syso_race_ethnicity == 7 & 
               any(setdiff(race_cols, c(RaceNone, White)))) |
            
            # White Only" = 8),
            (input$syso_race_ethnicity == 8 & 
               no_cols_selected_except(., race_cols, White)) |
            
            # Black, African American or African and Hispanic/Latina/e/o Inclusive" = 9,
            (input$syso_race_ethnicity == 9 & 
               (BlackAfAmerican == 1 | HispanicLatinaeo == 1)) |
            
            # Hispanic/Latina/e/o Inclusive" = 10,
            (input$syso_race_ethnicity == 10 & HispanicLatinaeo == 1) |
            
            # Hispanic/Latina/e/o Alone" = 11)
            (input$syso_race_ethnicity == 11 & 
               no_cols_selected_except(., race_cols, HispanicLatinaeo))
        )) |
          # Exclusive
          (input$methodology_type != "1" & (
            # American Indian, Alaska Native, or Indigenous Alone" = 1,
            (input$syso_race_ethnicity == 1 & 
               no_cols_selected_except(., race_cols, AmIndAKNative)) |
              
              # American Indian, Alaska Native, or Indigenous & Hispanic/Latina/e/o" = 2,
              (input$syso_race_ethnicity == 2 & 
                 no_cols_selected_except(., race_cols, c(AmIndAKNative, HispanicLatinaeo))) |
              
              # Asian or Asian American Alone" = 3,
              (input$syso_race_ethnicity == 3 & 
                 no_cols_selected_except(., race_cols, Asian)) |
              
              # Asian or Asian American & Hispanic/Latina/e/o" = 4,
              (input$syso_race_ethnicity == 4 & 
                 no_cols_selected_except(., race_cols, c(Asian, HispanicLatinaeo))) |
              
              # Black, African American, or African Alone" = 5,
              (input$syso_race_ethnicity == 5 & 
                 no_cols_selected_except(., race_cols, BlackAfAmerican)) |
              
              # Black, African American, or African & Hispanic/Latina/e/o" = 6,
              (input$syso_race_ethnicity == 6 & 
                 no_cols_selected_except(., race_cols, c(BlackAfAmerican, HispanicLatinaeo))) |
              
              # Hispanic/Latina/e/o Alone" = 7,
              (input$syso_race_ethnicity == 7 & 
                 no_cols_selected_except(., race_cols, HispanicLatinaeo)) |
              
              # Middle Eastern or North African Alone" = 8,
              (input$syso_race_ethnicity == 8 & 
                 no_cols_selected_except(., race_cols, MidEastNAfrican)) |
              
              # Middle Eastern or North African & Hispanic/Latina/e/o" = 9,
              (input$syso_race_ethnicity == 9 & 
                 no_cols_selected_except(., race_cols, c(MidEastNAfrican, HispanicLatinaeo))) |
              
              # Native Hawaiin or Pacific Islander Alone" = 10,
              (input$syso_race_ethnicity == 10 & 
                 no_cols_selected_except(., race_cols, NativeHIPacific)) |
              
              # Native Hawaiin or Pacific Islander & Hispanic/Latina/e/o" = 11,
              (input$syso_race_ethnicity == 11 & 
                 no_cols_selected_except(., race_cols, c(NativeHIPacific, HispanicLatinaeo))) |
              
              # White Alone" = 12,
              (input$syso_race_ethnicity == 12 & 
                 no_cols_selected_except(., race_cols, White)) |
              
              race_cols <- c("RaceNone", "AmIndAKNative", "Asian", "BlackAfAmerican", 
                             "NativeHIPacific", "White", "MidEastNAfrican", "HispanicLatinaeo")
            
            # White & Hispanic/Latina/e/o" = 13,
            (input$syso_race_ethnicity == 13 & 
                no_cols_selected_except(., race_cols, c(White, HispanicLatinaeo))) |
              
              # Multi-Racial (not Hispanic/Latina/e/o)" = 14,
              (input$syso_race_ethnicity == 14 & 
                 min_cols_selected_except(., race_cols, c(RaceNone, HispanicLatinaeo), 2)) |
              
              # Multi-Racial & Hispanic/Latina/e/o" = 15),
              (input$syso_race_ethnicity == 15 & 
                 min_cols_selected_except(., race_cols, RaceNone, 2)) |
              
              # All People of Color" = 16,
              (input$syso_race_ethnicity == 16 & 
                 no_cols_selected_except(., race_cols, c(AmIndAKNative, HispanicLatinaeo))) |
              
              # White Only" = 17
              (input$syso_race_ethnicity == 17 & 
                 no_cols_selected_except(., race_cols, c(AmIndAKNative, HispanicLatinaeo)))
          ))
      )
  ) %>%
  group_by(PersonalID) %>%
  mutate(
    EarliestEntry = min(EntryDate),
    LatestExit = coalesce(max(ExitDate), no_end_date) 
  ) %>%
  ungroup()
})

syso_detailBox <- reactive({
  # remove group names from race/ethnicity filter
  # so we can use getNameByValue() to grab the selected option label
  syso_race_ethnicitys <- unlist(syso_race_ethnicity_cats())
  names(syso_race_ethnicitys) <- gsub("Group [0-9]+\\.", "", names(syso_race_ethnicitys))
  
  list(
    strong("Date Range: "), input$syso_date_range[1], " to ", input$syso_date_range[2], 
    br(),
    strong("Household Type: "), getNameByValue(syso_hh_types, input$syso_hh_type), " | ",
    strong("Level of Detail: "), getNameByValue(syso_level_of_detail, input$syso_level_of_detail), " | ",
    strong("Project Type: "), getNameByValue(syso_project_types, input$syso_project_type), 
    br(),
    strong("Age: "), if_else(
      setequal(syso_age_cats, input$syso_age),
      "All Ages",
      getNameByValue(syso_age_cats, input$syso_age)
    ), " | ",
    strong("Gender: "), getNameByValue(syso_gender_cats(), input$syso_gender), " | ",
    strong("Race/Ethnicity: "), getNameByValue(syso_race_ethnicitys, input$syso_race_ethnicity), " | ",
    strong("Special Populations: "), getNameByValue(syso_spec_pops_cats(), input$syso_spec_pops), 
    br(),
    strong("Methodology Type: "), getNameByValue(syso_methodology_types, input$methodology_type) 
  )
})


syso_chartSubheader <- reactive({
  list(
    strong("Total Served: "), nrow(system_activity_filtered), 
    br()
  )
})
