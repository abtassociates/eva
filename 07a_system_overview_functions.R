
# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
system_activity_raw <- system_df %>%
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
            (input$syso_level_of_detail == "All Adults and Heads of Households" & (AgeAtEntry >= 18 | RelationshipToHoH == 1)) |
            (input$syso_level_of_detail == "All Heads of Households" & RelationshipToHoH == 1) |
            (input$syso_level_of_detail == "All People")
        ) & 

        # Project Type
        (
            (ProjectType %in% input$syso_project_types) | input$syso_project_types == "All Project Types"
        ) & 

         # Age
        (
            (AgeAtEntry >= 0 & AgeAtEntry <= 12 & syso_age[1] %in% input$syso_age) |
            (AgeAtEntry >= 13 & AgeAtEntry <= 17 & syso_age[2] %in% input$syso_age) |
            (AgeAtEntry >= 18 & AgeAtEntry <= 20 & syso_age[3] %in% input$syso_age) |
            (AgeAtEntry >= 21 & AgeAtEntry <= 24 & syso_age[4] %in% input$syso_age) |
            (AgeAtEntry >= 25 & AgeAtEntry <= 34 & syso_age[5] %in% input$syso_age) |
            (AgeAtEntry >= 35 & AgeAtEntry <= 44 & syso_age[6] %in% input$syso_age) |
            (AgeAtEntry >= 45 & AgeAtEntry <= 54 & syso_age[7] %in% input$syso_age) |
            (AgeAtEntry >= 55 & AgeAtEntry <= 64 & syso_age[8] %in% input$syso_age) |
            (AgeAtEntry >= 65 & AgeAtEntry <= 74 & syso_age[9] %in% input$syso_age) |
            (AgeAtEntry >= 75 ~ syso_age[10] %in% input$syso_age)
        ) &

        # Gender
        (
            input$syso_gender == "All Genders"
        ) & 

        # Race/Ethnicity
        (
            input$syso_race_ethnicity == "All Races/Ethnicities"
        )
    )
