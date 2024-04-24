syso_race_ethnicities_all <- unlist(syso_race_ethnicity_cats()["Group 1"])
names(syso_race_ethnicities_all) <- gsub("Group [0-9]+\\.", "",
                                     names(syso_race_ethnicities_all))

syso_race_ethnicities_grouped <- unlist(syso_race_ethnicity_cats()["Group 2"])
names(syso_race_ethnicities_grouped) <- gsub("Group [0-9]+\\.", "",
                                         names(syso_race_ethnicities_grouped))


sys_comp_filter_choices <- reactive({
  ifelse(
    input$methodology_type == 1,
    list(sys_comp_filter_choices1),
    list(sys_comp_filter_choices2)
  )[[1]]
})

get_v_cats <- function(v) {
  return(
    switch(v,
      "Gender" = syso_gender_cats(), 
      "Age" = syso_age_cats, 
      "All Races/Ethnicities" = syso_race_ethnicities_all, 
      "Grouped Races/Ethnicities" = syso_race_ethnicities_grouped, 
      "Domestic Violence" = syso_spec_pops_people[2:3]
    )
  )
}

get_sys_comp_plot_df <- function(varnames) {
  cats <- lapply(varnames, get_v_cats)
  names(cats) <- varnames

  var_cols <- sapply(varnames, get_sys_comp_var)
  
  return(
    system_df_people_universe_filtered() %>%
      select(PersonalID, var_cols) %>%
      count(!!!syms(varnames)) %>%
      filter(n > 10) %>%
      mutate(pct = prop.table(n)) %>%
      right_join(
        do.call(expand.grid, cats),
        by = varnames
      )
  )
}

get_sys_comp_var <- function(v) {
  return(
    switch(v,
           "All Races/Ethnicities" = "AllRaceEthnicity", 
           "Grouped Races/Ethnicities" = "GroupedRaceEthnicity",
           "Domestic Violence" = "DomesticViolence",
           v
    )
  )
}

sys_comp_filters <- function(session) {
  return(
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
      strong("Project Types: "),
      getNameByValue(syso_project_types, input$syso_project_types),
      br(),
      strong("Filter Selections: "),
      paste(input$system_composition_filter, collapse=" and "),
      br(),
      strong("Methodology Type: "),
      getNameByValue(syso_methodology_types, input$methodology_type) 
    )
  )
}

sys_comp_plot <- function(vars) {
  req(length(vars) == 2)
  
  # race/ethnicity should always be on the row
  if(vars[1] == "All Races/Ethnicities" |  
     vars[1] == "Grouped Races/Ethnicities") {
    x <- vars[1]
    vars[2] <- vars[1]
    vars[1] <- x
  }
  
  cats1 <- get_v_cats(vars[1])
  cats2 <- get_v_cats(vars[2])
  
  plot_df <- get_sys_comp_plot_df(vars)
  
  if(all(is.na(plot_df$n))) return()
  
  plot_df[vars[1]] <- factor(plot_df[[vars[1]]])
  plot_df[vars[2]] <- factor(plot_df[[vars[2]]])
  
  h_total <- plot_df %>% 
    group_by(!!!syms(vars[2])) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!vars[1] := 'Total')
  
  v_total <- plot_df %>% 
    group_by(!!!syms(vars[1])) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!vars[2] := 'Total')

  
  font_size <- 10/.pt
  
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
      labels = str_wrap(c("Total", names(cats2)), width=30),
      limits = c("Total", rev(levels(plot_df[[vars[2]]])))
    ) +
      
    # other stuff
    theme_bw() +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_text(vars[1]),
          axis.title.y = element_text(vars[2]),
          axis.text = element_text(size = 13))
  )
}
