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
      
      if (input$syso_project_type != "All")
        chart_selection_detail_line("Project Type", syso_project_types, input$syso_project_type),
      
      chart_selection_detail_line("Methodology Type", syso_methodology_types, input$methodology_type),
      
      HTML(glue(
        "<strong>Filter Selections</strong>: {paste(input$system_composition_filter, collapse=' and ')} <br>"
      ))
    )
  )
}

get_var_cols <- function() {
  return(list(
    "Age" = "AgeCategory",
    "All Races/Ethnicities" = get_race_ethnicity_vars("All"),
    "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
    "Hispanic-Focused Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
    "Domestic Violence" = "DomesticViolenceCategory",
    "Gender" = unlist(syso_gender_cats(input$methodology_type)),
    # "Homelessness Type" =  "HomelessnessType",# Victoria, 8/15/24: Not including this for Launch
    "Veteran Status" =  "VeteranStatus"
  ))
}
get_sys_comp_plot_df <- function(selections) {
  # named list of all selected options and 
  # the corresponding variables in the underlying data
  var_cols <- get_var_cols()
  
  # get dataset underlying the freqs we will produce below
  comp_df <- sys_df_people_universe_filtered_r() %>%
    select(PersonalID, unname(var_cols[[selections[1]]]), unname(var_cols[[selections[2]]]))
  
  # Function to process each combination of the variables underlying the all-served
  # selections. E.g. if Age and Gender (and Exclusive methopdology type), 
  # then we'd combine 0 to 12 with ManExclusive, 0 to 12 with WomanExclusive, 
  # 13 to 24 with ManExclusive, etc.
  process_combination <- function(v1, v2, comp_df) {
    freq_df <- as.data.frame(
      table(comp_df[[v1]], comp_df[[v2]])
    )
    names(freq_df) <- c(selections[1], selections[2], "n")
    
    # for selections comprised of multiple (binary/dummy) vars (e.g. Gender or Race), 
    # filter to the 1s and change the 1 to the variable name
    for (i in seq_along(selections)) {
      v <- get(paste0("v", i))
      vname <- sym(selections[i])
      var_cats <- var_cols[[vname]]
      if(length(var_cats) > 1) {
        freq_df <- freq_df %>%
          filter(!!vname == 1) %>%
          mutate(!!vname := v)
      }
    }
    return(freq_df)
  }

  # Get a dataframe of the freqs of all combinations
  # along with percents
  freqs <- expand_grid(v1 = var_cols[[selections[1]]], v2 = var_cols[[selections[2]]]) %>%
    pmap_dfr( ~ process_combination(..1, ..2, comp_df)) # %>%
    # mutate(pct = (n / sum(n, na.rm = TRUE)))
  
  # Handle DV, since the "Total" is not an actual value of DomesticViolenceCategory.
  if("Domestic Violence" %in% selections) {
    dv_totals <- freqs %>%
      filter(`Domestic Violence` %in% c("DVFleeing", "DVNotFleeing")) %>%
      group_by(!!sym(ifelse(selections[1] == "Domestic Violence",
                            selections[2], selections[1]))) %>%
      summarize(
        `Domestic Violence` = "DVTotal",
        n = sum(n, na.rm = TRUE) #,
        # pct = sum(pct, na.rm = TRUE)
      )
    freqs <- bind_rows(freqs, dv_totals)
  }
  
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
           # "Homelessness Type" = c("Homelessness Type1", "Homelessness Type2") # Victoria, 8/15/24: Not including this for Launch
    )
  )
}

# Suppression Rule 2: If only one cell in a group (i.e. row and/or column) is suppressed, 
# then suppress the next lowest value in that group
suppress_next_lowest_val <- function(df, group_v, n_v) {
  return(
    df %>%
      group_by(!!sym(group_v)) %>%
      mutate(
        count_redacted = sum(wasRedacted, na.rm = TRUE),
        next_lowest = min(!!sym(n_v), na.rm = TRUE),
        wasRedacted = ifelse(
          count_redacted == 1 & (
            (wasRedacted & is.na(!!sym(n_v))) |
              (!wasRedacted & !!sym(n_v) == next_lowest)
          ),
          TRUE,
          wasRedacted
        )
      ) %>%
      ungroup() %>%
      select(-c(count_redacted, next_lowest))
  )
}

sys_comp_plot_1var <- function(selection) {
  var_cols <- get_var_cols()
  v <- var_cols[[selection]]
  comp_df <- sys_df_people_universe_filtered_r() %>%
    select(PersonalID, unname(v))
  
  v_cats1 <- get_v_cats(selection)
  v_cats1_labels <- if(is.null(names(v_cats1))) {v_cats1} else {names(v_cats1)}
  
  if(length(v) > 1) {
    plot_df <- comp_df %>%
      pivot_longer(cols = -PersonalID,
                   names_to = selection,
                   values_to = "value") %>%
      group_by(!!sym(selection)) %>%
      summarize(n = sum(value, na.rm = TRUE), .groups = 'drop')

    plot_df[selection] <- factor(plot_df[[selection]], 
                                     levels = v_cats1,
                                     labels = v_cats1_labels)
  } else {
    plot_df <- as.data.frame(
      table(comp_df[[v]])
    )
    names(plot_df) <- c(selection, "n")
  }
  
  plot_df <- plot_df %>%
    mutate(
      wasRedacted = between(n, 1, 10),
      n = ifelse(n <= 10, NA, n)
    )
  
  plot_df <- suppress_next_lowest_val(plot_df, selection, "n")
  
  font_size <- 14/.pt
  return(
    ggplot(plot_df, aes("", .data[[selection]])) +
      # main data into cells for each cross-combination
      geom_tile(
        color = '#f0f0f0',
        lwd = 0.5,
        linetype = 1,
        aes(fill = n)) +
      scale_fill_gradient(low = "#D2E3D9", 
                          high = "#084954",
                          na.value = ifelse(
                            is.na(plot_df$wasRedacted) | !plot_df$wasRedacted,
                            "white",
                            "#D2E3D9"
                          )) +
      # set text color to be 508 compliant contrasting
      geom_text(
        aes(label = ifelse(
          wasRedacted,
          "***",
          scales::comma(n)
        )),
        size = font_size,
        color = ifelse(
          plot_df$n > mean(plot_df$n,na.rm=TRUE) & !plot_df$wasRedacted,
          'white', 
          'black'
        )
      ) +
      scale_y_discrete(
        labels = str_wrap(c("Total", v_cats1_labels), width=30),
        limits = c("Total", levels(plot_df[[selection]])),
      ) +
      # other stuff
      theme_bw() +
      ggtitle(
        sys_total_count_line(nrow(sys_df_people_universe_filtered_r()))
      ) +
      theme(legend.position = "none", 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(size=17, hjust=0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(selection, size = 13),
            axis.text = element_text(size = 14))
  )
}

sys_comp_plot_2vars <- function(selections) {
  # race/ethnicity, if selected, should always be on the row
  if(selections[1] == "All Races/Ethnicities" |  
     selections[1] == "Grouped Races/Ethnicities" | 
     selections[1] == "Hispanic-Focused Races/Ethnicities") {
    x <- selections[2]
    selections[2] <- selections[1]
    selections[1] <- x
  }
  
  plot_df <- get_sys_comp_plot_df(selections)
  validate(
    need(
      sum(plot_df$n > 0, na.rm=TRUE) > 0,
      message = "No data to show"
    ),
    need(
      sum(plot_df$n > 10, na.rm=TRUE) > 0,
      message = "Not enough data to show"
    )
  )
  
  if(all(is.na(plot_df$n))) return()
  v_cats1 <- get_v_cats(selections[1])
  v_cats1_labels <- if(is.null(names(v_cats1))) {v_cats1} else {names(v_cats1)}

  v_cats2 <- get_v_cats(selections[2])
  v_cats2_labels <- if(is.null(names(v_cats2))) {rev(v_cats2)} else {rev(names(v_cats2))}

  plot_df[selections[1]] <- factor(plot_df[[selections[1]]], 
                                   levels = v_cats1,
                                   labels = v_cats1_labels)
  
  plot_df[selections[2]] <- factor(plot_df[[selections[2]]],
                                   levels = rev(v_cats2),
                                   labels = v_cats2_labels)
  
  plot_df <- plot_df %>% 
    complete(!!sym(selections[1]), !!sym(selections[2])) %>%
    replace(is.na(.), 0)
    
  h_total <- plot_df %>% 
    group_by(!!!syms(selections[[2]])) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(
      !!selections[[1]] := 'Total',
      wasRedacted = between(N, 1, 10),
      N = ifelse(N <= 10, NA, N)
    )
  h_total <- suppress_next_lowest_val(h_total, selections[1], "N")
  
  v_total <- plot_df %>% 
    group_by(!!!syms(selections[[1]])) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(
      !!selections[[2]] := 'Total',
      wasRedacted = between(N, 1, 10),
      N = ifelse(N <= 10, NA, N)
    )
  v_total <- suppress_next_lowest_val(v_total, selections[2], "N")
  
  font_size <- 14/.pt
  
  # Suppress the next lowest value in a group 
  # if there's only one suppressed cell in that group
  plot_df <- plot_df %>%
    mutate(
      wasRedacted = between(n, 1, 10),
      n = ifelse(n <= 10, NA, n)
    )
  plot_df <- suppress_next_lowest_val(plot_df, selections[1], "n")
  plot_df <- suppress_next_lowest_val(plot_df, selections[2], "n")
  
  return(
    ggplot(plot_df, aes(.data[[selections[1]]], .data[[selections[2]]])) +
      # main data into cells for each cross-combination
      geom_tile(
        color = '#f0f0f0',
        lwd = 0.5,
        linetype = 1,
        aes(fill = n)) +
      scale_fill_gradient(low = "#D2E3D9", 
                          high = "#084954",
                          na.value = ifelse(
                            is.na(plot_df$wasRedacted) | !plot_df$wasRedacted,
                            "white",
                            "#D2E3D9"
                          )) + # na.value makes 0s invisible
      # display like 14/(0.8%)
      # set text color to be 508 compliant contrasting
      geom_text(
        # aes(label = paste0(scales::comma(n), "\n", "(",scales::percent(pct, accuracy = 0.1),")")),
        aes(label = ifelse(
          wasRedacted,
          "***",
          scales::comma(n)
        )),
        size = font_size,
        color = ifelse(
          plot_df$n > mean(plot_df$n,na.rm=TRUE) & !plot_df$wasRedacted,
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
      
      scale_fill_gradient(
        low = "#ede7e3",
        high = "#73655e",
        na.value = ifelse(h_total$wasRedacted,"#ede7e3",'white')
      ) +
      
      geom_text(
        aes(label= ifelse(
          wasRedacted,
          "***",
          # paste0(scales::comma(N), "\n", "(",scales::percent(N/sum(N, na.rm=TRUE), accuracy = 0.1),")")
          scales::comma(N)
        )), 
        size = font_size,
        color = ifelse(
          h_total$N > mean(h_total$N,na.rm=TRUE) & !h_total$wasRedacted,
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
      scale_fill_gradient(
        low = "#ede7e3",
        high = "#73655e",
        na.value = ifelse(v_total$wasRedacted,"#ede7e3",'white')
      ) +
      
      geom_text(
        aes(label= ifelse(
          wasRedacted,
          "***",
          # paste0(N, "\n", "(",scales::percent(N/sum(N, na.rm=TRUE), accuracy = 0.1),")")
          scales::comma(N)
        )),
        size = font_size,
        color = ifelse(
          v_total$N > mean(v_total$N,na.rm=TRUE) & !v_total$wasRedacted,
          'white', 
          'black'
        ),
        data= v_total
      ) +
      
      # axis labels
      scale_x_discrete(
        labels = str_wrap(c(v_cats1_labels, "Total"), width=20),
        limits = c(levels(plot_df[[selections[1]]]), "Total"),
        position = "top"
      ) +
      scale_y_discrete(
        labels = str_wrap(c("Total", v_cats2_labels), width=30),
        limits = c("Total", levels(plot_df[[selections[2]]])),
      ) +
      
      # other stuff
      theme_bw() +

      ggtitle(
        sys_total_count_line(
          nrow(sys_df_people_universe_filtered_r())
        )
      ) +
      
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(size=17, hjust=0.5),
            axis.title.x = element_text(selections[1], size = 13),
            axis.title.y = element_text(selections[2], size = 13),
            axis.title.x.top = element_text(margin = margin(0, 0, 15, 0)),
            axis.text = element_text(size = 14))
  )
}
