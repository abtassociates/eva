get_race_ethnicity_vars <- function(v) {
  if(v == "All") {
    syso_race_ethnicities_all <- unlist(syso_race_ethnicity_cats(input$methodology_type)["Detailed"])
    names(syso_race_ethnicities_all) <- gsub("Detailed.", "",
                                             names(syso_race_ethnicities_all))
    return(syso_race_ethnicities_all)
  } else if(v %in% c("Grouped")) {
    syso_race_ethnicities_grouped <- unlist(syso_race_ethnicity_cats(input$methodology_type)["Summarized"])
    names(syso_race_ethnicities_grouped) <- gsub("Summarized.", "",
                                                 names(syso_race_ethnicities_grouped))
    return(syso_race_ethnicities_grouped)
  }
}

syscomp_detailBox <- function(session) {
  return(
    list(
      strong("Date Range: "),
      
      ReportStart(), " to ", ReportEnd(), br(),
      
      if (getNameByValue(syso_hh_types, input$syso_hh_type) != "All People")
        chart_selection_detail_line("Household Type", syso_hh_types, input$syso_hh_type),
      
      chart_selection_detail_line("Level of Detail", syso_level_of_detail, input$syso_level_of_detail),
      
      if (getNameByValue(syso_project_types, input$syso_project_type) != "All")
        chart_selection_detail_line("Project Type", syso_project_types, input$syso_project_type),
      
      chart_selection_detail_line("Methodology Type", syso_methodology_types, input$methodology_type),
      
      HTML(glue(
        "<strong>Filter Selections</strong>: {paste(input$system_composition_filter, collapse=' and ')} <br>"
      )),
      
      chart_selection_detail_line("Methodology Type", syso_methodology_types, input$methodology_type)
    )
  )
}


get_sys_comp_plot_df <- function(varnames) {
  # named list of all selected options and 
  # the corresponding variables in the underlying data
  var_cols <- list(
    "Age" = "AgeCategory",
    "All Races/Ethnicities" = get_race_ethnicity_vars("All"),
    "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
    "Hispanic-Focused Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
    "Domestic Violence" = "DomesticViolenceCategory",
    "Gender" = unlist(syso_gender_cats(input$methodology_type)),
    "Homelessness Type" =  "HomelessnessType",
    "Veteran Status" =  "VeteranStatus"
  )
  
  # get dataset underlying the freqs we will produce below
  comp_df <- sys_df_people_universe_filtered_r()() %>%
    select(PersonalID, unname(var_cols[[varnames[1]]]), unname(var_cols[[varnames[2]]]))
  
  # Function to process each combination of the variables underlying the all-served
  # selections. E.g. if Age and Gender (and Exclusive methopdology type), 
  # then we'd combine 0 to 12 with ManExclusive, 0 to 12 with WomanExclusive, 
  # 13 to 24 with ManExclusive, etc.
  process_combination <- function(v1, v2, comp_df) {
    freq_df <- as.data.frame(
      table(comp_df[[v1]], comp_df[[v2]])
    )
    names(freq_df) <- c(v1, v2, "n")
    
    # make adjustments to the data frame based on the types of the user's selections
    #   - for binary/dummy vars (e.g. Gender or Race), filter to the 1s 
    #   and change to the variable label, which will make reporting will be easier
    #   - for other vars, rename so column names are closer to the original selections
    #   which will make code easier to read later
    for (i in seq_along(varnames)) {
      if(length(var_cols[[varnames[i]]]) > 1) {
        # multi-select/binary
        v <- get(paste0("v", i))
        freq_df <- freq_df %>%
          filter(!!sym(v) == 1) %>%
          mutate(!!names(var_cols[varnames[i]]) := names(var_cols[[varnames[i]]])[var_cols[[varnames[i]]] == v]) %>%
          select(-!!sym(v))
      } else {
        # otherwise
        freq_df <- rename(freq_df, !!varnames[i] := get(paste0("v", i)))
      }
    }
    return(freq_df)
  }
  
  # Get a dataframe of the freqs of all combinations
  # along with percents
  freqs <- expand_grid(v1 = var_cols[[varnames[1]]], v2 = var_cols[[varnames[2]]]) %>%
    pmap_dfr( ~ process_combination(..1, ..2, comp_df)) %>%
    mutate(
      pct = (n / sum(n, na.rm = TRUE)),
      n = ifelse(n <= 10, NA, n) # redcat counts under 10
    )
  
  return(freqs)
}

# this gets all the categories of the selected variable
# this is used to make sure even empty categories are included in the chart
get_v_cats <- function(v) {
  return(
    switch(v,
           "Gender" = syso_gender_cats(input$methodology_type),
           "Age" = syso_age_cats, 
           "All Races/Ethnicities" = get_race_ethnicity_vars("All"), 
           "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped"), 
           "Hispanic-Focused Races/Ethnicities" = get_race_ethnicity_vars("Grouped"), 
           "Domestic Violence" = syso_dv_pops,
           "Veteran Status" = syso_veteran_pops,
           "Homelessness Type" = c("Homelessness Type1", "Homelessness Type2")
    )
  )
}

sys_comp_plot <- function(sys_comp_filter_selections) {
  # must have selected 2 variables to cross-tab
  req(length(sys_comp_filter_selections) == 2)
  
  # race/ethnicity, if selected, should always be on the row
  if(sys_comp_filter_selections[1] == "All Races/Ethnicities" |  
     sys_comp_filter_selections[1] == "Grouped Races/Ethnicities" | 
     sys_comp_filter_selections[1] == "Hispanic-Focused Races/Ethnicities") {
    x <- sys_comp_filter_selections[1]
    sys_comp_filter_selections[2] <- sys_comp_filter_selections[1]
    sys_comp_filter_selections[1] <- x
  }
  
  plot_df <- get_sys_comp_plot_df(sys_comp_filter_selections)
  if(all(is.na(plot_df$n))) return()
  
  # plot_df <- as.data.frame(plot_df)
  plot_df[sys_comp_filter_selections[1]] <- factor(plot_df[[sys_comp_filter_selections[1]]])
  plot_df[[sys_comp_filter_selections[2]]] <- factor(plot_df[[sys_comp_filter_selections[2]]], levels = rev(names(get_v_cats(sys_comp_filter_selections[2]))))
  
  h_total <- plot_df %>% 
    group_by(!!!syms(sys_comp_filter_selections[[2]])) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!sys_comp_filter_selections[[1]] := 'Total')
  
  v_total <- plot_df %>% 
    group_by(!!!syms(sys_comp_filter_selections[[1]])) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!sys_comp_filter_selections[[2]] := 'Total')
  
  
  font_size <- 14/.pt

  
  return(
    ggplot(plot_df, aes(.data[[sys_comp_filter_selections[1]]], .data[[sys_comp_filter_selections[2]]])) +
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
        labels = str_wrap(c(names(get_v_cats(sys_comp_filter_selections[1])), "Total"), width=20),
        limits = c(levels(plot_df[[sys_comp_filter_selections[1]]]), "Total"),
        position = "top"
      ) +
      scale_y_discrete(
        labels = str_wrap(c("Total", rev(names(get_v_cats(sys_comp_filter_selections[2])))), width=30),
        limits = c("Total", levels(plot_df[[sys_comp_filter_selections[2]]]))
      ) +
      
      # other stuff
      theme_bw() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.title.x = element_text(sys_comp_filter_selections[1], size = 13),
            axis.title.y = element_text(sys_comp_filter_selections[2], size = 13),
            axis.text = element_text(size = 14))
  )
}
