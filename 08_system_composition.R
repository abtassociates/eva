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
      ) ~ syso_gender_incl["Only Woman (Girl, if child) OR Only Man (Boy, if child)"],
      
      input$methodology_type == 2 & Woman == 1
      ~ syso_gender_incl["Woman (Girl, if child) alone or in combination"]
    ),
  
    RaceEthnicity = case_when(
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
      
      # All People of Color" = 16,
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, c("AmIndAKNative",
                                                   "HispanicLatinaeo"))
      ~ syso_race_ethnicity_excl[["Group 2"]][16],
      
      # White Only" = 17
      input$methodology_type == 1 & 
        no_cols_selected_except(., race_cols, "White")
      ~ syso_race_ethnicity_excl[["Group 2"]][17],
      
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
      ~ syso_race_ethnicity_incl[["Group 1"]][7],
      
      # Black, African American or African and Hispanic/Latina/e/o Inclusive" = 8,
      input$methodology_type == 2 & (BlackAfAmerican == 1 | HispanicLatinaeo == 1)
      ~ syso_race_ethnicity_incl[["Group 2"]][8],
      
      # Hispanic/Latina/e/o Inclusive" = 9,
      input$methodology_type == 2 & HispanicLatinaeo == 1
      ~ syso_race_ethnicity_incl[["Group 2"]][9],
      
      # Hispanic/Latina/e/o Alone" = 10)
      input$methodology_type == 2 &
             no_cols_selected_except(., race_cols, "HispanicLatinaeo")
      ~ syso_race_ethnicity_incl[["Group 2"]][10],
    )
  ) %>%
  select(PersonalID, Gender,RaceEthnicity, Age)

get_v_cats <- function(v) {
  return(
    switch(v,
      "Gender" = syso_gender_cats(), 
      "Age" = syso_age_cats, 
      "RaceEthnicity" = syso_race_ethnicities, 
    )
  )
}

get_sys_comp_plot_df <- function(vars) {
  cats <- lapply(vars, get_v_cats)
  names(cats) <- vars
  
  return(
    client_df %>%
      count(!!!syms(vars)) %>%
      filter(n > 10) %>%
      mutate(pct = prop.table(n)) %>%
      right_join(
        do.call(expand.grid, cats),
        by = vars
      ) # %>%
      # mutate_all(~replace(., is.na(.), 0))
  )
}

sys_comp_plot <- function(vars) {
  req(length(vars) == 2)
  
  # remove the / from Race/Ethnicity, since the varname is RaceEthnicity
  vars <- str_remove(vars, "/")
  v1 <- vars[1]
  v2 <- vars[2]
  
  cats <- lapply(vars, get_v_cats)
  
  plot_df <- get_sys_comp_plot_df(vars)
  plot_df[[v1]] <- factor(plot_df[[v1]], levels = cats[[1]])
  plot_df[[v2]] <- factor(plot_df[[v2]], levels = cats[[2]])
  
  h_total <- plot_df %>% 
    group_by(!!!syms(v2)) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!v1 := 'Total')
  
  v_total <- plot_df %>% 
    group_by(!!!syms(v1)) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!v2 := 'Total')

  font_size <- 10/.pt
  
  return(
    ggplot(plot_df, aes(.data[[v1]], .data[[v2]])) +   
    # main data into cells for each cross-combination
    geom_tile(
      color = 'white',
      lwd = 0.5,
      linetype = 1,
      aes(fill = n)) +
    scale_fill_gradient(low = "#D2E3D9", 
                        high = "#084954",
                        na.value = 'white') + # na.value makes 0s invisible
    # display like 14/(0.8%)
    # set text color to be 508 compliant contrasting
    geom_text(
      aes(label = paste0(n, "\n", "(",scales::percent(pct, accuracy = 0.1),")")), 
      size = font_size,
      color = ifelse(
        plot_df$n > mean(plot_df$n,na.rm=TRUE),
        'white', 
        'black'
      )
    ) +
    
    # Row totals
    ggnewscale::new_scale("fill") +
    geom_tile(data = h_total,
              color = "white",
              lwd = 0.5,
              linetype = 1,
              aes(fill = N)) +

    scale_fill_gradient(low = "#ede7e3", high = "#73655e", na.value = 'white') +

    geom_text(
      aes(label= ifelse(
        is.na(N),
        "",
        paste0(N, "\n", "(",scales::percent(N/sum(N, na.rm=TRUE), accuracy = 0.1),")")
      )), 
      size = font_size,
      color = ifelse(
        h_total$N > mean(h_total$N,na.rm=TRUE),
        'white', 
        'black'
      ),
      data= h_total
    ) +

    # column totals
    ggnewscale::new_scale("fill") +
    geom_tile(data = v_total,
              color = "white",
              lwd = 0.5,
              linetype = 1,
              aes(fill = N)) +
    scale_fill_gradient(low = "#ede7e3", high = "#73655e", na.value = 'white') +
      
    geom_text(
      aes(label= ifelse(
        is.na(N),
        "",
        paste0(N, "\n", "(",scales::percent(N/sum(N, na.rm=TRUE), accuracy = 0.1),")")
      )),
      size = font_size,
      color = ifelse(
        v_total$N > mean(v_total$N,na.rm=TRUE),
        'white', 
        'black'
      ),
      data= v_total
    ) +
      
    # axis labels
    scale_x_discrete(
      labels = str_wrap(c(names(cats[[1]]), "Total"), width=20),
      limits = c(levels(plot_df[[v1]]), "Total"),
      position = "top"
    ) +
    scale_y_discrete(
      labels = str_wrap(c("Total", names(cats[[2]])), width=30),
      limits = c("Total", rev(levels(plot_df[[v2]])))
    ) +
      
    # other stuff
    theme_bw() +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 12))
  )
}
