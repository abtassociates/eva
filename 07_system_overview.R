logToConsole("Running system overview")

# using EnrollmentAdjust because that df doesn't contain enrollments that fall
# outside periods of operation/participation
system_df_prep <- EnrollmentAdjust %>%
  left_join(Project %>% 
              select(ProjectID,
                     ProjectName,
                     OrganizationID,
                     RRHSubType,
                     ContinuumProject) %>%
              unique(),
            by = "ProjectID") %>%
  left_join(Organization %>% select(OrganizationID, OrganizationName),
            by = "OrganizationID") %>%
  left_join(Client %>%
              select(
                PersonalID,
                VeteranStatus#,
                # all_of(gender_cols),
                # all_of(race_cols)
              ), by = "PersonalID") %>%
  left_join(HealthAndDV %>%
              filter(DataCollectionStage == 1) %>%
              select(EnrollmentID, DomesticViolenceSurvivor, CurrentlyFleeing),
            by = "EnrollmentID") %>%
  select(
    AgeAtEntry,
    ContinuumProject,
    CurrentlyFleeing,
    DateToStreetESSH,
    Destination,
    DestinationSubsidyType,
    DisablingCondition,
    DomesticViolenceSurvivor,
    EnrollmentID,
    EntryDate,
    ExitAdjust,
    ExitDate,
    HouseholdID,
    LengthOfStay,
    LOSUnderThreshold,
    LivingSituation,
    MoveInDateAdjust,
    MonthsHomelessPastThreeYears,
    OrganizationName,
    PersonalID,
    PreviousStreetESSH,
    ProjectID,
    ProjectName,
    ProjectType,
    RRHSubType,
    RelationshipToHoH,
    RentalSubsidyType,
    TimesHomelessPastThreeYears,
    VeteranStatus#,
    # all_of(gender_cols),
    # all_of(race_cols)
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
system_df_prep <- system_df_prep %>%
  left_join(hh_adjustments, join_by(EnrollmentID)) %>%
  relocate(CorrectedHoH, .after = RelationshipToHoH)

# Enrollment-level flags. will help us categorize enrollments
system_df_enrl_flags <- system_df_prep %>%
  mutate(
    EnrollmentDateRange = interval(EntryDate, coalesce(ExitDate, no_end_date)),
    EnteredAsHomeless = !is.na(LivingSituation) &
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
    EntryStatusHomeless = EnteredAsHomeless |
      ProjectType %in% lh_project_types,
    EnrolledHomeless = ContinuumProject == 1 &
      ProjectType %in% project_types_enrolled_homeless &
      LivingSituation %in% lh_livingsituation,
    EnrolledHoused = ContinuumProject == 1 &
      ProjectType %in% ph_project_types & 
      LivingSituation %in% homeless_livingsituation
  ) %>%
  select(
    EnrollmentID, 
    PersonalID, 
    HouseholdID,
    EntryDate, 
    ExitDate, 
    ProjectType, 
    EnteredAsHomeless,
    EntryStatusHomeless,
    EnrolledHomeless,
    EnrolledHoused,
    MoveInDateAdjust,
    LivingSituation,
    LOSUnderThreshold,
    PreviousStreetESSH,
    Destination,
    EnrollmentDateRange,
    AgeAtEntry,
    RelationshipToHoH#,
    # gender_cols,
    # race_cols
  ) %>%
  group_by(HouseholdID) %>%
  mutate(
    Household_Type = case_when(
      all(is.na(AgeAtEntry)) ~ "Unknown Households",
      all(AgeAtEntry >= 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~
        "Adult-Only",
      any(AgeAtEntry < 18, na.rm = TRUE) & any(AgeAtEntry >= 18, na.rm = TRUE) ~
        "Adult-Child",
      all(AgeAtEntry < 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~
        "Child-Only",
      all(AgeAtEntry < 25 & AgeAtEntry >= 18, na.rm = TRUE) &
        !any(is.na(AgeAtEntry)) ~ "Youth and Young Adult",
      TRUE ~ "All Households"
    )
  ) %>% 
  ungroup()

# Client-level flags. will help us categorize people
system_df_client_flags <- Client %>%
  mutate(AgeAtReportEnd = age_years(DOB, meta_HUDCSV_Export_End)) %>%
  select(PersonalID,
         all_of(race_cols),
         all_of(gender_cols),
         AgeAtReportEnd,
         VeteranStatus
         )

# universe filters/enrollment-level filters -----------------------------------
system_df_enrl_filtered <- reactive({
  system_df_enrl_flags %>%
  filter(
    # Household Type
    (
      # "All Households" = 1, 
      input$syso_hh_type == 1 |
        Household_Type == getNameByValue(syso_hh_types, input$syso_hh_type)
    ) & 
      # Level of Detail
      (
        (input$syso_level_of_detail == 1) |
          (input$syso_level_of_detail == 2 & 
             (AgeAtEntry >= 18 | RelationshipToHoH == 1)) |
          (input$syso_level_of_detail == 3 & RelationshipToHoH == 1)
      ) & 
      # Project Type
      (
        input$syso_project_type == 1 |
          (input$syso_project_type == 2 & ProjectType %in% project_types_w_beds) |
          (input$syso_project_type == 3 & ProjectType %in% non_res_project_types)
      )
  )
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
  system_df_people <- 
    system_df_enrl_flags[!duplicated(system_df_enrl_flags$PersonalID), ]
  system_df_people %>%
    filter(
      # Age
      (
        setequal(syso_age_cats, input$syso_age) |
          (AgeAtEntry >= 0 & AgeAtEntry <= 12 &
             syso_age_cats["0 to 12"] %in% input$syso_age) |
          (AgeAtEntry >= 13 & AgeAtEntry <= 17 &
             syso_age_cats["13 to 17"] %in% input$syso_age) |
          (AgeAtEntry >= 18 & AgeAtEntry <= 20 &
             syso_age_cats["18 to 21"] %in% input$syso_age) |
          (AgeAtEntry >= 21 & AgeAtEntry <= 24 &
             syso_age_cats["21 to 24"] %in% input$syso_age) |
          (AgeAtEntry >= 25 & AgeAtEntry <= 34 &
             syso_age_cats["25 to 34"] %in% input$syso_age) |
          (AgeAtEntry >= 35 & AgeAtEntry <= 44 &
             syso_age_cats["35 to 44"] %in% input$syso_age) |
          (AgeAtEntry >= 45 & AgeAtEntry <= 54 &
             syso_age_cats["45 to 54"] %in% input$syso_age) |
          (AgeAtEntry >= 55 & AgeAtEntry <= 64 &
             syso_age_cats["55 to 64"] %in% input$syso_age) |
          (AgeAtEntry >= 65 & AgeAtEntry <= 74 &
             syso_age_cats["65 to 74"] %in% input$syso_age) |
          (AgeAtEntry >= 75 & syso_age_cats["75 and older"] %in% input$syso_age)
      ) &
      # Special Populations
      (
        input$syso_spec_pops == 1
        
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
          (input$syso_gender == 6 & 
             no_cols_selected_except(., gender_cols, "GenderNone"))
        )) |
        # inclusive
        (input$methodology_type == 2 & (
          input$syso_gender == 1 |
          (input$syso_gender == 2 & (
            (Woman == 1 & Man == 1) | 
              !(Woman == 1 & Man == 1)
          )) |
          (input$syso_gender == 3 & Man == 1) | 
          (input$syso_gender == 4 & NonBinary == 1) | 
          (input$syso_gender == 5 & (Man == 1 | Woman == 1)) | 
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
             no_cols_selected_except(., race_cols, c("BlackAfAmerican", "HispanicLatinaeo"))) |
          
          # Hispanic/Latina/e/o Alone" = 7,
          (input$syso_race_ethnicity == 7 & 
             no_cols_selected_except(., race_cols, "HispanicLatinaeo")) |
          
          # Middle Eastern or North African Alone" = 8,
          (input$syso_race_ethnicity == 8 & 
             no_cols_selected_except(., race_cols, "MidEastNAfrican")) |
          
          # Middle Eastern or North African & Hispanic/Latina/e/o" = 9,
          (input$syso_race_ethnicity == 9 & 
             no_cols_selected_except(., race_cols, c("MidEastNAfrican", "HispanicLatinaeo"))) |
          
          # Native Hawaiin or Pacific Islander Alone" = 10,
          (input$syso_race_ethnicity == 10 & 
             no_cols_selected_except(., race_cols, "NativeHIPacific")) |
          
          # Native Hawaiin or Pacific Islander & Hispanic/Latina/e/o" = 11,
          (input$syso_race_ethnicity == 11 & 
             no_cols_selected_except(., race_cols, c("NativeHIPacific","HispanicLatinaeo"))) |
          
          # White Alone" = 12,
          (input$syso_race_ethnicity == 12 & 
             no_cols_selected_except(., race_cols, "White")) |
          
          # White & Hispanic/Latina/e/o" = 13,
          (input$syso_race_ethnicity == 13 & 
             no_cols_selected_except(., race_cols, c("White", "HispanicLatinaeo"))) |
          
          # Multi-Racial (not Hispanic/Latina/e/o)" = 14,
          (input$syso_race_ethnicity == 14 & 
             min_cols_selected_except(., race_cols, c("RaceNone", "HispanicLatinaeo"), 2)) |
          
          # Multi-Racial & Hispanic/Latina/e/o" = 15),
          (input$syso_race_ethnicity == 15 & 
             min_cols_selected_except(., race_cols, "RaceNone", 2)) |
          
          # All People of Color" = 16,
          (input$syso_race_ethnicity == 16 & 
             no_cols_selected_except(., race_cols, c("AmIndAKNative", "HispanicLatinaeo"))) |
          
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
          
          # Middle Eastern Inclusive" = 4,
          (input$syso_race_ethnicity == 4 & MidEastNAfrican == 1) |
          
          # Native Hawaiin or Pacific Islander Inclusive" = 5,
          (input$syso_race_ethnicity == 5 & NativeHIPacific == 1) |
          
          # White Inclusive" = 6),
          (input$syso_race_ethnicity == 6 & White == 1) |
          
          # All People of Color" = 7,
          (input$syso_race_ethnicity == 7 & 
             any_cols_selected_except(., race_cols, c("RaceNone", "White"))) |
          
          # White Only" = 8),
          (input$syso_race_ethnicity == 8 & 
             no_cols_selected_except(., race_cols, "White")) |
          
          # Black, African American or African and Hispanic/Latina/e/o Inclusive" = 9,
          (input$syso_race_ethnicity == 9 & 
             (BlackAfAmerican == 1 | HispanicLatinaeo == 1)) |
          
          # Hispanic/Latina/e/o Inclusive" = 10,
          (input$syso_race_ethnicity == 10 & HispanicLatinaeo == 1) |
          
          # Hispanic/Latina/e/o Alone" = 11)
          (input$syso_race_ethnicity == 11 & 
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
  names(syso_race_ethnicitys) <- gsub("Group [0-9]+\\.", "", names(syso_race_ethnicitys))
  
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


# determine the earliest and latest enrollments crossing the report
# based on the enrollment-level filtered dataset
# get the earliest enrollment cross the report range
# this is needed for inflow types Returned from Permanent and Re-Engaged from Temporary/Unknown
# in case there are overlapping enrollments (caught by DQ), take first
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
      ExitDate,
      EntryDate,
      EnteredAsHomeless,
      EnrolledHomeless,
      EnrolledHoused,
      EntryStatusHomeless,
      MoveInDateAdjust,
      ProjectType
    )
  
  ecr_DT <- as.data.table(ecr)
  
  # Function to calculate min and max enrollments crossing 
  # start and end of the report period, respectively
  calc_ecr <- function(ctype) {
    # Calculate the minimum EnrollmentDateRange for each PersonalID
    ctype_ecr <- ecr_DT[, 
                        .(ctype_enrollmentstart = get(ctype)(EntryDate)), 
                        by = PersonalID]
    
    # Merge the minimum EnrollmentDateRange back to the original data table
    ecr <- ecr_DT[
      ctype_ecr, 
      on = .(PersonalID, EntryDate = ctype_enrollmentstart)]
    
    # Use unique() to remove duplicates based on PersonalID
    ecr <- unique(ecr, by = c("PersonalID"))
    
    # Rename columns except household ID and PersonalID
    suffix <- ifelse(ctype == "min", "_eecr", "_lecr")
    names(ecr) <- paste0(names(ecr), suffix)
    names(ecr)[names(ecr) == paste0("PersonalID", suffix)] <- "PersonalID"
    
    return(ecr)
  }
  return(list(
    eecr = calc_ecr("min"),
    lecr = calc_ecr("max")
  ))
})



# AS TO GD: Please review this closely.
# get final people-level, inflow/outflow dataframe by joining the filtered----- 
# enrollment and people dfs, as well as flagging their inflow and outflow types
system_df_people <- reactive({
browser()
  # add inflow type and active enrollment typed used for system overview plots
  system_df_enrl_filtered() %>%
    inner_join(system_df_people_filtered(), by = "PersonalID") %>%
    inner_join(enrollments_crossing_report()$eecr, by = "PersonalID") %>%
    left_join(enrollments_crossing_report()$lecr, by = "PersonalID") %>%
    # remove enrollments where the exit is over 2 years prior to report start
    filter(as.numeric(difftime(ExitDate, input$syso_date_range[1],
                               unit = "days")) / 365 <= 2) %>%
    mutate(is_before_eecr = EntryDate < EntryDate_eecr) %>%
    # create enrollment-level variables/flags that will be used to 
    # label people to be counted in the system activity charts
    group_by(PersonalID) %>%
    mutate(
      stay_in_lh = any(ProjectType %in% lh_project_types & is_before_eecr == TRUE),
      entered_as_homeless = any(EnteredAsHomeless & is_before_eecr),
      NoEnrollmentsToLHFor14DaysFromLECR = !any(
        as.numeric(difftime(ExitDate_lecr, EntryDate, "days")) <= -14 & 
          ProjectType %in% lh_project_types
      ),
      return_from_permanent = any(EntryStatusHomeless & 
        as.numeric(difftime(EntryDate_eecr, ExitDate, unit="days")) >= 14 &
        Destination_eecr %in% perm_destinations),
      
      reengaged_from_temporary = any(EntryStatusHomeless &
        as.numeric(difftime(EntryDate_eecr, ExitDate, unit = "days")) >= 14 &
        !(Destination %in% perm_destinations))
    ) %>%
    ungroup() %>%
    unique() %>%
    mutate(
      InflowType = case_when(
        #1) If project type is in (lh_project_types), then client is not newly homeless (0)
        #2) If LivingSituation is in (hs_living_situation), then client is not newly homeless (0)
        #3) If LivingSituation is in (non_hs_living_sit) and both LOSUnderThreshold and PreviousStreetESSH == 1, then client is not newly homeless (0)
        EnrolledHomeless_eecr ~ "Enrolled: Homeless",
        EnrolledHoused_eecr ~ "Enrolled: Housed",
        !stay_in_lh & !entered_as_homeless ~ "Newly Homeless",
        return_from_permanent ~ "Returned from \nPermanent",
        reengaged_from_temporary ~ "Re-engaged from \nTemporary/Unknown"
      ),
      
      OutflowType = case_when(
        # The client has exited from an enrollment with a permanent destination 
        # and does not have any other enrollments (aside from RRH/PSH with a move-in date?) 
        # in emergency shelter, transitional housing, safe haven, or street outreach for at least 14 days following.
        Destination_lecr %in% perm_destinations & 
          NoEnrollmentsToLHFor14DaysFromLECR
        ~ "Permanent Destination",
        
        # The client has exited from an enrollment with a temporary/unknown destination 
        # and does not have any other enrollments (aside from RRH/PSH with a move-in date?) 
        # in emergency shelter, transitional housing, safe haven, or street outreach for at least 14 days following.
        !(Destination_lecr %in% perm_destinations) & 
          NoEnrollmentsToLHFor14DaysFromLECR
        ~ "Temporary/Unknown \nDestination",
        
        EnrolledHomeless_lecr
        ~ "Enrolled: Homeless",
        
        EnrolledHoused_lecr
        ~ "Enrolled: Housed"
      )
    ) %>%
    filter(!is.na(InflowType) | !is.na(OutflowType)) %>%
    select(
      PersonalID,
      InflowType,
      OutflowType
    ) %>%
    unique()
})

