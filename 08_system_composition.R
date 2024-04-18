syso_race_ethnicities <- unlist(syso_race_ethnicity_cats())
names(syso_race_ethnicities) <- gsub("Group [0-9]+\\.", "",
                                     names(syso_race_ethnicities))


client_df <- system_df_client_flags %>%
  mutate(
    Age = case_when(
      MostRecentAgeAtEntry >= 0 & MostRecentAgeAtEntry <= 12 ~ syso_age_cats["0 to 12"],
      MostRecentAgeAtEntry >= 13 & MostRecentAgeAtEntry <= 17 ~ syso_age_cats["13 to 17"],
      MostRecentAgeAtEntry >= 18 & MostRecentAgeAtEntry <= 20 ~ syso_age_cats["18 to 20"],
      MostRecentAgeAtEntry >= 21 & MostRecentAgeAtEntry <= 24 ~ syso_age_cats["21 to 24"],
      MostRecentAgeAtEntry >= 25 & MostRecentAgeAtEntry <= 34 ~ syso_age_cats["25 to 34"],
      MostRecentAgeAtEntry >= 35 & MostRecentAgeAtEntry <= 44 ~ syso_age_cats["35 to 44"],
      MostRecentAgeAtEntry >= 45 & MostRecentAgeAtEntry <= 54 ~ syso_age_cats["45 to 54"],
      MostRecentAgeAtEntry >= 55 & MostRecentAgeAtEntry <= 64 ~ syso_age_cats["55 to 64"],
      MostRecentAgeAtEntry >= 65 & MostRecentAgeAtEntry <= 74 ~ syso_age_cats["65 to 74"],
      MostRecentAgeAtEntry >= 75 ~ syso_age_cats["75 and older"]
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
      ) ~ syso_gender_incl["Only Woman (Girl, if child) OR Only Man (Boy, if child)"]  
      
      input$methodology_type == 2 & Woman == 1
      ~ syso_gender_incl["Woman (Girl, if child) alone or in combination"]
    ),
  
    race_eth = case_when(
      # Exclusive
      # American Indian, Alaska Native, or Indigenous Alone" = 1,
      input$methodology_type == 1 & 
      no_cols_selected_except(., race_cols, "AmIndAKNative")
      ~ syso_race_ethnicity_excl[1],
        
      # American Indian, Alaska Native, or Indigenous & Hispanic/Latina/e/o" = 2,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, c("AmIndAKNative", "HispanicLatinaeo"))
      ~ syso_race_ethnicity_excl[2],
        
      # Asian or Asian American Alone" = 3,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, "Asian")
      ~ syso_race_ethnicity_excl[3],
        
      # Asian or Asian American & Hispanic/Latina/e/o" = 4,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, c("Asian", "HispanicLatinaeo"))
      ~ syso_race_ethnicity_excl[4],
      
      # Black, African American, or African Alone" = 5,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, "BlackAfAmerican")
      ~ syso_race_ethnicity_excl[5],  
      
      # Black, African American, or African & Hispanic/Latina/e/o" = 6,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, c("BlackAfAmerican",
                                                   "HispanicLatinaeo"))
      ~ syso_race_ethnicity_excl[6],
      
      # Hispanic/Latina/e/o Alone" = 7,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, "HispanicLatinaeo")
      ~ syso_race_ethnicity_excl[7],
      
      # Middle Eastern or North African Alone" = 8,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, "MidEastNAfrican")
      ~ syso_race_ethnicity_excl[8],
      
      # Middle Eastern or North African & Hispanic/Latina/e/o" = 9,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, c("MidEastNAfrican",
                                                   "HispanicLatinaeo"))
      ~ syso_race_ethnicity_excl[9],
      
      # Native Hawaiin or Pacific Islander Alone" = 10,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, "NativeHIPacific")
      ~ syso_race_ethnicity_excl[10],
      
      # Native Hawaiin or Pacific Islander & Hispanic/Latina/e/o" = 11,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, c("NativeHIPacific",
                                                   "HispanicLatinaeo"))
      ~ syso_race_ethnicity_excl[11],
      
      # White Alone" = 12,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, "White")
      ~ syso_race_ethnicity_excl[12],
      
      # White & Hispanic/Latina/e/o" = 13,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, c("White", "HispanicLatinaeo"))
      ~ syso_race_ethnicity_excl[13],
      
      # Multi-Racial (not Hispanic/Latina/e/o)" = 14,
      input$methodology_type == 1 & 
        min_cols_selected_except(., race_cols, c("RaceNone",
                                                    "HispanicLatinaeo"), 2)
      ~ syso_race_ethnicity_excl[14],
      
      # Multi-Racial & Hispanic/Latina/e/o" = 15),
      input$methodology_type == 1 & 
        min_cols_selected_except(., race_cols, "RaceNone", 2)
      ~ syso_race_ethnicity_excl[15],
      
      # All People of Color" = 16,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, c("AmIndAKNative",
                                                   "HispanicLatinaeo"))
      ~ syso_race_ethnicity_excl[16],
      
      # White Only" = 17
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, "White")
      ~ syso_race_ethnicity_excl[17],
      
      # Inclusive
      #American Indian, Alaska Native, or Indigenous Inclusive" = 1,
      input$methodology_type == 2 & AmIndAKNative == 1
      ~ syso_race_ethnicity_incl[1],
      
      # Asian or Asian American Inclusive" = 2,
      input$methodology_type == 2 & Asian == 1
      ~ syso_race_ethnicity_incl[2],
    
      # Black, African American, or African Inclusive" = 3,
      input$methodology_type == 2 & BlackAfAmerican == 1
      ~ syso_race_ethnicity_incl[3],

      # Hispanic/Latina/e/o = 4
      input$methodology_type == 2 & HispanicLatinaeo == 1
      ~ syso_race_ethnicity_incl[4],

      # Middle Eastern Inclusive = 5
      input$methodology_type == 2 & MidEastNAfrican == 1
      ~ syso_race_ethnicity_incl[5],
      
      # Native Hawaiin or Pacific Islander Inclusive" = 6,
      input$methodology_type == 2 & NativeHIPacific == 1
      ~ syso_race_ethnicity_incl[6],
      
      # White Inclusive" = 7),
      input$methodology_type == 2 & White == 1
      ~ syso_race_ethnicity_incl[7],
      
      # Black, African American or African and Hispanic/Latina/e/o Inclusive" = 8,
      input$methodology_type == 2 & (BlackAfAmerican == 1 | HispanicLatinaeo == 1)
      ~ syso_race_ethnicity_incl[8],
      
      # Hispanic/Latina/e/o Inclusive" = 9,
      input$methodology_type == 2 & ispanicLatinaeo == 1
      ~ syso_race_ethnicity_incl[9],
      
      # Hispanic/Latina/e/o Alone" = 10)
      input$methodology_type == 2 &
             no_cols_selected_except(., race_cols, "HispanicLatinaeo")
      ~ syso_race_ethnicity_incl[10],
  )
)

genderByRace <- system_df_client_flags %>%
  select(Gender,RaceEthnicity) %>%
  count(Gender = factor(Gender), RaceEthnicity = factor(RaceEthnicity)) %>%
  filter(n > 10) %>%
  mutate(pct = prop.table(n))

ggplot(
  genderByRace, 
  aes(Gender, RaceEthnicity)
  ) +                           
  geom_tile(color = "white",
            lwd = 0.5,
            linetype = 1,
            aes(fill = n)) +
  scale_fill_gradient(low = "#D2E3D9", 
                      high = "#084954") +
  geom_text(aes(label = paste0(n, "\n", "(",scales::percent(pct, accuracy = 0.1),")"), size = 4))
