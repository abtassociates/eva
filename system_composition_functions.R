get_race_ethnicity_vars <- function(v) {
  if(v == "All Races/Ethnicities") {
    syso_race_ethnicities_all <- unlist(syso_race_ethnicity_cats(input$methodology_type)["Detailed"])
    names(syso_race_ethnicities_all) <- gsub("Detailed.", "",
                                             names(syso_race_ethnicities_all))
    return(syso_race_ethnicities_all)
  } else if(v == "Grouped Races/Ethnicities") {
    syso_race_ethnicities_grouped <- unlist(syso_race_ethnicity_cats(input$methodology_type)["Summarized"])
    names(syso_race_ethnicities_grouped) <- gsub("Summarized.", "",
                                                 names(syso_race_ethnicities_grouped))
    return(syso_race_ethnicities_grouped)
  }
}

# this gets all the categories of the selected variable
get_v_cats <- function(v) {
syscomp_detailBox <- function(session) {
  return(
    switch(v,
           "Gender" = syso_gender_cats(input$methodology_type),
           "Age" = syso_age_cats, 
           "All Races/Ethnicities" = get_race_ethnicity_vars("All Races/Ethnicities"), 
           "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped Races/Ethnicities"), 
           "Domestic Violence" = syso_dv_pops
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
  cats <- lapply(varnames, get_v_cats)
  names(cats) <- varnames
  
  var_cols <- sapply(varnames, get_sys_comp_var)
  
  comp_df <- sys_df_people_universe_filtered_r()() %>%
    select(PersonalID, unname(var_cols[[1]]), unname(var_cols[[2]]))
  
  # if("Gender" %in% varnames) {
  #   comp_df <- comp_df %>% 
  #     mutate(
  #       Gender = if_any(.cols = names(var_cols[["Gender"]]), ~.x==1)
  #     )
  # }
  
  is_multiselect <- function(v) {length(v) > 1}
  
  # for binary/dummy vars, filter to the 1s 
  # and change the 1s to the label of the variable so when we combine later, 
  # reporting will be easier
  handle_binary_var <- function(freq_df, vnum, v) {
    return(
      freq_df <- freq_df %>%
        filter(!!sym(v) == 1) %>%
        mutate(!!names(var_cols[vnum]) := names(var_cols[[vnum]])[var_cols[[vnum]] == v]) %>%
        select(-!!sym(v))
    )
  }
  
  # Function to process each combination of the variables underlying the all-served
  # selections. E.g. if Age and Gender (and Exclusive methopdology type), 
  # then we'd combine 0 to 12 with ManExclusive, 0 to 12 with WomanExclusive, 
  # 13 to 24 with ManExclusive, etc.
  process_combination <- function(v1, v2, comp_df) {
    freq_df <- as.data.frame(
      table(comp_df[[v1]], comp_df[[v2]])
    )
    names(freq_df) <- c(v1, v2, "n")
    
    if (is_multiselect(var_cols[[1]])) {
      freq_df <- handle_binary_var(freq_df, 1, v1)
    }
    
    if (is_multiselect(var_cols[[2]])) {
      freq_df <- handle_binary_var(freq_df, 2, v2)
    }
    
    freq_df %>% 
      mutate(n = ifelse(n <= 10, NA, n))
  }
  
  # Get a dataframe of the freqs of all combinations
  # along with percents
  freqs <- expand_grid(
      v1 = var_cols[[1]], 
      v2 = var_cols[[2]]
    ) %>%
    pmap_dfr(~ process_combination(..1, ..2, comp_df)) %>%
    mutate(
      pct = (n / sum(n, na.rm = TRUE))
    )
  
  return(freqs)
}

syso_gender_cats <- function(methodology = 1){
  ifelse(
    methodology == 1,
    list(syso_gender_excl),
    list(syso_gender_incl)
  )[[1]]
}

get_sys_comp_var <- function(v) {
  return(
    switch(v,
           "All Races/Ethnicities" = "AllRaceEthnicity", 
           "Grouped Races/Ethnicities" = "GroupedRaceEthnicity",
           "Domestic Violence" = "DomesticViolence",
           "Gender" = unlist(syso_gender_cats(input$methodology_type)),
           v
    )
  )
}

sys_comp_plot <- function(vars) {
  # must have selected 2 variables to cross-tab
  req(length(vars) == 2)
  
  # race/ethnicity, if selected, should always be on the row
  if(vars[1] == "All Races/Ethnicities" |  
     vars[1] == "Grouped Races/Ethnicities") {
    x <- vars[1]
    vars[2] <- vars[1]
    vars[1] <- x
  }
  
  vars <- sapply(vars, function(v) {
    if_else(v == "Age", "AgeCategory", v)
  })
  
  # these are the categories/values of the selected variables
  cats1 <- get_v_cats(vars[1])
  cats2 <- get_v_cats(vars[2])
  
  plot_df <- get_sys_comp_plot_df(vars)
  
  if(all(is.na(plot_df$n))) return()
  
  # plot_df <- as.data.frame(plot_df)
  plot_df[vars[1]] <- factor(plot_df[[vars[1]]])
  plot_df[vars[2]] <- factor(plot_df[[vars[2]]])
  
  h_total <- plot_df %>% 
    group_by(!!!syms(vars[[2]])) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!vars[[1]] := 'Total')
  
  v_total <- plot_df %>% 
    group_by(!!!syms(vars[[1]])) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!vars[[2]] := 'Total')
  
  
  font_size <- 14/.pt

  return(
    ggplot(plot_df, aes(.data[[vars[1]]], .data[[vars[2]]])) +
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
        labels = str_wrap(c(names(cats1), "Total"), width=20),
        limits = c(levels(plot_df[[vars[1]]]), "Total"),
        position = "top"
      ) +
      scale_y_discrete(
        labels = str_wrap(c("Total", rev(names(cats2))), width=30),
        limits = c("Total", levels(plot_df[[vars[2]]]))
      ) +
      
      # other stuff
      theme_bw() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.title.x = element_text(vars[1], size = 13),
            axis.title.y = element_text(vars[2], size = 13),
            axis.text = element_text(size = 14))
  )
}
