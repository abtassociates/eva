logToConsole("Running system overview")

system_person_ages <- EnrollmentAdjust %>%
  group_by(PersonalID) %>%
  slice_max(AgeAtEntry, na_rm = TRUE, with_ties = FALSE) %>%
  ungroup() %>%
  select(PersonalID, "MostRecentAgeAtEntry" = AgeAtEntry)

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

# Enrollment-level flags. will help us categorize enrollments
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
    EnrolledHoused = EnrolledHomeless == FALSE
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
    EnrolledHoused,
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

# Client-level flags. will help us categorize people

system_df_client_flags <- Client %>%
  select(PersonalID,
         all_of(race_cols),
         all_of(gender_cols),
         VeteranStatus
         ) %>%
  left_join(system_person_ages, join_by(PersonalID))

# universe filters/enrollment-level filters -----------------------------------
system_df_enrl_filtered <- reactive({
  browser()
  nbn_enrollments_w_proper_services <- EnrollmentAdjust %>%
    select(EnrollmentID, ProjectType) %>%
    filter(ProjectType == 1) %>%
    inner_join(Services %>%
                filter(RecordType == 200 &
                         between(DateProvided,
                                 input$syso_date_range[1] - days(15),
                                 input$syso_date_range[1] + days(15))) %>%
                select(EnrollmentID, DateProvided),
              join_by(EnrollmentID)) %>%
    pull(EnrollmentID) %>%
    unique()
  
  outreach_w_proper_cls <- CurrentLivingSituation %>%
    filter(CurrentLivingSituation %in% homeless_livingsituation &
             between(InformationDate,
                     input$syso_date_range[1] - days(90),
                     input$syso_date_range[1] + days(90))) %>%
    pull(EnrollmentID) %>%
    unique()
  
  system_df_enrl_flags %>%
    filter(
    # remove enrollments where the exit is over 2 years prior to report start
      as.numeric(difftime(ExitDate, input$syso_date_range[1],
                          unit = "days")) / 365 <= 2 &
    # Active At Start logic
    ((ProjectType %in% c(es_ee_project_type, th_project_type, sh_project_type)) |
      (ProjectType == es_nbn_project_type &
         EnrollmentID %in% nbn_enrollments_w_proper_services) |
      (ProjectType == out_project_type &
         EnrollmentID %in% outreach_w_proper_cls &
         lh_prior_livingsituation == TRUE) |
      (ProjectType %in% c(ph_project_types) &
         (is.na(MoveInDateAdjust) |
            MoveInDateAdjust > input$syso_date_range[1]) &
         lh_prior_livingsituation == TRUE) |
      (ProjectType == ce_project_type &
         lh_prior_livingsituation == TRUE &
         between(EntryDate,
                 input$syso_date_range[1] - days(90),
                 input$syso_date_range[1] + days(90)))) &

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
    mutate(enrollment_before = lag(EnrollmentID, order_by = EntryDate),
           enrollment_after = lead(EnrollmentID, order_by = ExitAdjust),
           ordinal = row_number()) %>%
    ungroup() %>%
    mutate(
      in_date_range =
        int_overlaps(EnrollmentDateRange,
                     interval(input$syso_date_range[1], input$syso_date_range[2])
        )) %>%
    group_by(PersonalID, in_date_range) %>%
    mutate(
      lecr = in_date_range == TRUE & max(ordinal) == ordinal,
      eecr = in_date_range == TRUE & min(ordinal) == ordinal,
      lookback = if_else(in_date_range == FALSE, rev(row_number()), 0)) %>%
    ungroup()
})

# system inflow_outflow filters/people-level filters---------------------------
# Set race/ethnicity + gender filter options based on methodology type selection
# Set special populations options based on level of detail selection
syso_race_ethnicity_cats <- reactive({
  ifelse(
    input$methodology_type == 1,
    list(syso_race_ethnicity_excl),
    list(syso_race_ethnicity_incl)
  )[[1]]
})

syso_gender_cats <- reactive({
  ifelse(
    input$methodology_type == 1,
    list(syso_gender_excl),
    list(syso_gender_incl)
  )[[1]]
})

syso_spec_pops_cats <- reactive({
  ifelse(
    input$syso_level_of_detail %in% c(1,2),
    list(syso_spec_pops_people),
    list(syso_spec_pops_hoh)
  )[[1]]
})

# get filtered people-level system dataframe
system_df_people_filtered <- reactive({
  
  clients_in_report_date_range <- system_df_prep %>%
    filter(int_overlaps(
      EnrollmentDateRange,
      interval(input$syso_date_range[1], input$syso_date_range[2])
    )) %>%
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
      # Gender
      (
        # Exclusive
        (input$methodology_type == 1 & (
          # All genders
          input$syso_gender == 1 |
            
          # Gender diverse/expansive, not including transgender
          (input$syso_gender == 2 & 
             any_cols_selected_except(., c(
               CulturallySpecific, 
               NonBinary, 
               Questioning, 
               DifferentIdentity), "Transgender") & 
             any_cols_selected_except(., gender_cols, c("GenderNone","Transgender"))
          ) |
             
          
          # Man alone
          (input$syso_gender == 3 & 
             no_cols_selected_except(., gender_cols, "Man")) | 
          
          # Transgender, alone or in combo
          (input$syso_gender == 4 & Transgender == 1) | 
          
          # Woman, alone
          (input$syso_gender == 5 & 
             no_cols_selected_except(., gender_cols, "Woman")) |
          
          # Unknown
          (input$syso_gender == 6 & 
             no_cols_selected_except(., gender_cols, "GenderNone"))
        )) |
        # inclusive
        (input$methodology_type == 2 & (
          # All Genders
          input$syso_gender == 1 |
            
          # Gender Diverse/Expansive, including transgender
          (input$syso_gender == 2 & (
            (Woman == 1 & Man == 1) | 
            min_cols_selected_except(., gender_cols, c("Man","Woman"), 1)
          )) |
          
          # Man (Boy, if child) alone or in combination" = 3,
          (input$syso_gender == 3 & Man == 1) | 
        
          # Non-Binary alone or in combination
          (input$syso_gender == 4 & NonBinary == 1) | 
            
          # Only Woman (Girl, if child) OR Only Man (Boy, if child)
          (input$syso_gender == 5 & (
            (Man == 1 & Woman != 1) | 
            (Woman == 1 & Man != 1)
          )) | 
          
          # Woman (Girl, if child) alone or in combination
          (input$syso_gender == 6 & Woman == 1) 
        ))
      ) &
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

# detail stuff above chart---------------------------
syso_detailBox <- reactive({
  # remove group names from race/ethnicity filter
  # so we can use getNameByValue() to grab the selected option label
  syso_race_ethnicitys <- unlist(syso_race_ethnicity_cats())
  names(syso_race_ethnicitys) <- gsub("Group [0-9]+\\.", "",
                                      names(syso_race_ethnicitys))
  
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
    getNameByValue(syso_race_ethnicitys, input$syso_race_ethnicity),
    " | ",
    strong("Special Populations: "),
    getNameByValue(syso_spec_pops_cats(), input$syso_spec_pops), 
    br(),
    strong("Methodology Type: "),
    getNameByValue(syso_methodology_types, input$methodology_type) 
  )
})

syso_chartSubheader <- reactive({
  list(
    strong("Total Served: "), 
    formatC(
      nrow(system_df_people()),
      format = "d",
      big.mark = ","
    ),
    br()
  )
})

# Early & Late Enrollments Crossing the Date Range ------------------------

# determine the earliest and latest enrollments crossing the report
# based on the enrollment-level filtered dataset
# get the earliest enrollment cross the report range
# this is needed for inflow types Returned from Permanent and Re-Engaged from Temporary/Unknown
# in case there are overlapping enrollments (caught by DQ), take first

# Function to calculate min and max enrollments crossing 
# start and end of the report period, respectively

calc_ecr <- function(crosstype, ecr_DT) {
  # Calculate the minimum EnrollmentDateRange for each PersonalID
  crosstype_ecr <- ecr_DT[, 
                      .(crosstype_enrollmentstart = get(crosstype)(EntryDate)), 
                      by = PersonalID]
  
  # Merge the minimum EnrollmentDateRange back to the original data table
  ecr <- ecr_DT[
    crosstype_ecr, 
    on = .(PersonalID, EntryDate = crosstype_enrollmentstart)]
  
  # Use unique() to remove duplicates based on PersonalID
  ecr <- unique(ecr, by = c("PersonalID"))
  
  # Rename columns except household ID and PersonalID
  suffix <- ifelse(crosstype == "min", "_eecr", "_lecr")
  names(ecr) <- paste0(names(ecr), suffix)
  names(ecr)[names(ecr) == paste0("PersonalID", suffix)] <- "PersonalID"
  
  return(ecr)
}
# AS: maybe move to enrollment_filtered()
enrollments_crossing_report <- reactive({
  ecr <- system_df_enrl_filtered() %>%
    filter(
      int_overlaps(
        EnrollmentDateRange,
        interval(input$syso_date_range[1], input$syso_date_range[2])
      ) 
    ) %>%
    select(
      PersonalID, 
      HouseholdID,
      Destination, 
      EntryDate,
      ExitDate,
      lh_prior_livingsituation,
      EnrolledHomeless,
      EnrolledHoused,
      lh_at_entry,
      MoveInDateAdjust,
      ProjectType
    )
  
  ecr_DT <- as.data.table(ecr)
  
  return(list(
    eecr = calc_ecr("min", ecr_DT),
    lecr = calc_ecr("max", ecr_DT)
  ))
})

# AS TO GD: Please review this closely.
# get final people-level, inflow/outflow dataframe by joining the filtered----- 
# enrollment and people dfs, as well as flagging their inflow and outflow types
system_df_people <- reactive({
  # add inflow type and active enrollment typed used for system overview plots
  # browser()
  
  eecr_lecr <- full_join(
    enrollments_crossing_report()$eecr,
    enrollments_crossing_report()$lecr,
    join_by(PersonalID))
  
  universe <- system_df_enrl_filtered() %>%
    inner_join(system_df_people_filtered(), by = "PersonalID") %>%
    left_join(eecr_lecr,
              join_by(PersonalID)) %>%
    mutate(
      is_before_eecr = EntryDate < EntryDate_eecr,
      enrollment_ordinal = case_when(
        is_before_eecr == TRUE ~ 0,
        HouseholdID == HouseholdID_eecr &
          HouseholdID_eecr == HouseholdID_lecr ~ 2,
        HouseholdID == HouseholdID_eecr &
          HouseholdID != HouseholdID_lecr ~ 1,
        HouseholdID == HouseholdID_lecr &
          HouseholdID != HouseholdID_eecr ~ 3,
        TRUE ~ 99
      )
    ) %>%
    # create enrollment-level variables/flags that will be used to 
    # label people to be counted in the system activity charts
    group_by(PersonalID) %>%
    mutate(
      lookback_stay_in_lh = 
        any(ProjectType %in% lh_project_types &
              is_before_eecr == TRUE),
      lookback_entered_as_homeless = 
        any(lh_prior_livingsituation &
              is_before_eecr == TRUE),
      NoEnrollmentsToLHFor14DaysFromLECR = !any(
        as.numeric(difftime(ExitDate_lecr, EntryDate, "days")) <= -14 & 
          ProjectType %in% lh_project_types
      ), # is this what the logic means (VL)
      return_from_permanent = 
        lh_at_entry == TRUE &
        is_before_eecr == FALSE &
        any(lh_at_entry == TRUE & 
              as.numeric(difftime(EntryDate_eecr, ExitDate, unit = "days")) >= 14 &
              Destination %in% perm_destinations),
      reengaged_from_temporary =
        lh_at_entry == TRUE &
        is_before_eecr == FALSE &
        any(
          is_before_eecr == TRUE &
            as.numeric(difftime(EntryDate_eecr, ExitDate, unit = "days")) >= 14 &
            !(Destination %in% perm_destinations)
        ),
      pathway_skipped_report_start =
        lh_at_entry == TRUE &
        is_before_eecr == FALSE &
        any(
          is_before_eecr == TRUE &
            as.numeric(difftime(EntryDate_eecr, ExitDate, unit = "days")) < 14 &
            !(Destination %in% perm_destinations)
        ),
      enrolled_homeless_at_start = EntryDate_eecr <= input$syso_date_range[1] &
        coalesce(ExitDate_eecr, no_end_date) > input$syso_date_range[1] &
        EnrolledHomeless_eecr == TRUE,
      enrolled_housed_at_start = enrolled_homeless_at_start == FALSE,
      enrolled_homeless_at_end = EntryDate_lecr <= input$syso_date_range[2] &
        coalesce(ExitDate_lecr, no_end_date) > input$syso_date_range[2] &
        EnrolledHomeless_lecr == TRUE,
      enrolled_housed_at_end = enrolled_homeless_at_end == FALSE
    ) %>%
    ungroup()
  
  inflow <- universe %>%
    filter(!is.na(HouseholdID_eecr)) %>%
    select(PersonalID, lookback_stay_in_lh, lookback_entered_as_homeless,
           NoEnrollmentsToLHFor14DaysFromLECR, return_from_permanent,
           reengaged_from_temporary, enrolled_homeless_at_start,
           enrolled_housed_at_start, pathway_skipped_report_start) %>%
    unique() %>%
    mutate(InflowType = case_when(
      #1) If project type is in (lh_project_types), then client is not newly homeless (0)
      #2) If LivingSituation is in (hs_living_situation), then client is not newly homeless (0)
      #3) If LivingSituation is in (non_hs_living_sit) and both LOSUnderThreshold and PreviousStreetESSH == 1, then client is not newly homeless (0)
      enrolled_homeless_at_start == TRUE ~
        "Enrolled: Homeless",
      enrolled_housed_at_start == TRUE ~
        "Enrolled: Housed",
      lookback_stay_in_lh == FALSE &
        lookback_entered_as_homeless == FALSE ~ "Newly Homeless",
      return_from_permanent == TRUE ~ "Returned from \nPermanent",
      reengaged_from_temporary == TRUE ~ "Re-engaged from \nNon-Permanent",
      pathway_skipped_report_start == TRUE ~ "Continued system \nengagement",
      TRUE ~ "something's wrong"
    )) %>%
    select(PersonalID, InflowType)
  
  outflow <- universe %>%
    filter(!is.na(HouseholdID_lecr)) %>%
    select(PersonalID, Destination_lecr, ExitDate_lecr, enrolled_homeless_at_end,
           enrolled_housed_at_end) %>%
    unique() %>%
    mutate(OutflowType = case_when(
      # The client has exited from an enrollment with a permanent destination
      # and does not have any other enrollments (aside from RRH/PSH with a move-in date?)
      # in emergency shelter, transitional housing, safe haven, or street outreach for at least 14 days following.
      Destination_lecr %in% perm_destinations &
        !is.na(ExitDate_lecr)# &
      # NoEnrollmentsToLHFor14DaysFromLECR == TRUE
      ~ "Permanent Destination",
      
      # The client has exited from an enrollment with a temporary/unknown destination
      # and does not have any other enrollments (aside from RRH/PSH with a move-in date?)
      # in emergency shelter, transitional housing, safe haven, or street outreach for at least 14 days following.!(Destination_lecr %in% perm_destinations) &
      !is.na(ExitDate_lecr) &
        !Destination_lecr %in% perm_destinations
      ~ "Non-Permanent \nDestination",
      
      enrolled_homeless_at_end == TRUE ~ "Enrolled: Homeless",
      
      enrolled_housed_at_end == TRUE ~ "Enrolled: Housed",
      TRUE ~ "something's wrong"
    )) %>%
    select(PersonalID, OutflowType)
  
  full_join(inflow, outflow, join_by(PersonalID))
})

