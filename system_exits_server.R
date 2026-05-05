
compare_export_bar_width <- 0.4
compare_bar_width <- 0.4

output$syse_compare_subpop_filter_selections <-renderUI({ 
  req(session$userData$valid_file() == 1)
  
  sys_detailBox(
    detail_type = 'subpop',
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
  )
})

output$syse_compare_subpop2_filter_selections <-renderUI({ 

  req(!is.null(input$syse_compare_subpop2_selections) & session$userData$valid_file() == 1)
  
  sys_detailBox(selection = input$syse_compare_subpop2_selections,
                detail_type = 'phd',
                methodology_type = ifelse('All Races/Ethnicities' %in% input$syse_compare_subpop2_selections, '1',
                                          ifelse('Grouped Races/Ethnicities' %in% input$syse_compare_subpop2_selections, '2', NA)),
                cur_project_types = input$syse_project_type,
                startDate = session$userData$ReportStart,
                endDate = session$userData$ReportEnd,
                age = input$syse_age,
                spec_pops = input$syse_spec_pops,
                race_eth = input$syse_race_ethnicity)
})

# output$syse_subpop2_race_ethnicity <- renderUI({
#   
#   choices <- switch(input$syse_methodology_type, "1" = sys_race_ethnicity_method1, "2" = sys_race_ethnicity_method2)
#   #names(choices[1]) <- 'None Selected'
#   choices <- setNames(choices, nm=c("None Selected", names(choices[-1])))
#   pickerInput(
#   #selectInput(
#     label = "",#label = "Race/Ethnicity",
#     inputId = "syse_subpop2_race_ethnicity",
#     choices = choices,
#     #choiceValues = choices,
#     #choiceNames = c("None Selected", names(choices[-1])),
#     selected = "None Selected",
#     options = list(
#       `dropdown-align-right` = TRUE,
#       `dropup-auto` = FALSE,
#       container = "body"#,
#       #noneSelectedText = "-"
#     )
#   )
#   
# })

# output$syse_subpop2_post_selections <-renderUI({ 
#   req(session$userData$valid_file() == 1)
#   req(isTruthy(input$syse_subpop2_selections)) 
#   
#   out <- tagList()
#   
#   if('Age' %in% input$syse_subpop2_selections){
#     out <- tagList(out, 
#                    pickerInput(
#                      inputId = "syse_subpop2_age",
#                      label = "Age",
#                      selected = sys_age_cats,
#                      choices = sys_age_cats,
#                      multiple = TRUE,
#                      options = pickerOptions(
#                        actionsBox = TRUE,
#                        selectedTextFormat = paste("count >", length(sys_age_cats)-1),
#                        countSelectedText = "All Ages",
#                        noneSelectedText = "All Ages",
#                        container = "body"
#                      )
#                    ))
#   }
#   
#   if('All Races/Ethnicities' %in% input$syse_subpop2_selections){
#     out <- tagList(out,
#                    pickerInput(
#                      label = "Race/Ethnicity",
#                      inputId = "syse_subpop2_race_ethnicity1",
#                      choices = sys_race_ethnicity_method1,
#                      selected = sys_race_ethnicity_method1,
#                      options = list(
#                        `dropdown-align-right` = TRUE,
#                        `dropup-auto` = FALSE,
#                        container = "body"
#                      )
#                    ))
#   }
#   if("Grouped Races/Ethnicities" %in% input$syse_subpop2_selections){
#     out <- tagList(out,
#                    pickerInput(
#                      label = "Race/Ethnicity",
#                      inputId = "syse_subpop2_race_ethnicity2",
#                      choices = sys_race_ethnicity_method2,
#                      selected = sys_race_ethnicity_method2,
#                      options = list(
#                        `dropdown-align-right` = TRUE,
#                        `dropup-auto` = FALSE,
#                        container = "body"
#                      )
#                    ))
#   }
#   
#   if("Veteran Status (Adult Only)" %in% input$syse_subpop2_selections){
#     out <- tagList(out,pickerInput(
#       label = "Veteran Status",
#       inputId = "syse_subpop2_spec_pops",
#       choices = sys_spec_pops_people,
#       selected = sys_spec_pops_people[1],
#       options = pickerOptions(container = "body")
#     )
#     )
#   }
#   #out
#   layout_columns(!!!out)#, col_widths = rep(4, length(out)))
#   
# })

#### DISPLAY FILTER SELECTIONS ###

output$syse_types_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  
  sys_detailBox(
    detail_type = 'types',
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
  )
})

## separate info for time chart tab since report period covers 2 years before ReportEnd
output$syse_compare_time_filter_selections <- renderUI({
  req(session$userData$valid_file() == 1)
 
  sys_detailBox(
    detail_type = 'time',
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
  )
})

sys_phd_selections_info <- reactive({
  sys_demographics_selection_info(type = 'exits', selection = input$syse_phd_selections)
})

output$syse_phd_summary_selections <- renderUI({
  req(!is.null(input$syse_phd_selections) & session$userData$valid_file() == 1)
  
  sys_detailBox(selection = input$syse_phd_selections,
                detail_type = 'phd',
                methodology_type = ifelse('All Races/Ethnicities' %in% input$syse_phd_selections, '1',
                                          ifelse('Grouped Races/Ethnicities' %in% input$syse_phd_selections, '2', NA)),
                cur_project_types = input$syse_project_type,
                startDate = session$userData$ReportStart,
                endDate = session$userData$ReportEnd)
})
  

sys_phd_selections_summary <- function() {
  return(
    sys_export_summary_initial_df(type = 'exits') %>%
      rowbind(sys_demographics_selection_info(type = 'exits', selection = input$syse_phd_selections)) %>%
      frename("System Exit Demographics" = Value)
  )
}

syse_level_of_detail_text <- reactive({
  case_when(
    input$syse_level_of_detail == "All" ~ "People",
    input$syse_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(sys_level_of_detail, input$syse_level_of_detail)
  )
})

output$syse_types_ui_chart <- renderPlot({
  
  syse_types_chart("Destination Type", input$syse_dest_type_filter)
})


sys_phd_plot_1var <- function(subtab = 'phd', methodology_type, selection, isExport = FALSE) {
  var_cols <- get_var_cols(methodology_type)
  
  comp_df <- all_filtered_syse_demog() %>% 
    remove_non_applicables(selection = selection) %>% 
    select(PersonalID, Destination, unname(var_cols[[selection]]))
  comp_df_phd <- comp_df %>% 
    fsubset(Destination %in% perm_livingsituation) %>% 
    fselect(-Destination)
  
  comp_df <- comp_df %>% fselect(-Destination)
  
  validate(
    need(
      nrow(comp_df) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      nrow(comp_df) > 10,
      message = suppression_msg
    )
  )
  
  plot_df <- get_sys_plot_df_1var(comp_df, var_cols[[selection]], selection = selection)
  plot_df_phd <- get_sys_plot_df_1var(comp_df_phd, var_cols[[selection]], selection = selection)
  
  # hide download buttons if not enough data
  toggle_download_buttons(subtab,plot_df)
  
  type <- 'exits'
  selection_cats1 <- get_selection_cats(selection, type = type)
  
  if(is.null(names(selection_cats1))){
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  plot_df[[selection]] <- factor(
    plot_df[[selection]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels,
    ordered = TRUE)
  
  plot_df_phd[[selection]] <- factor(
    plot_df_phd[[selection]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels,
    ordered = TRUE)
  
  if(subtab == 'comp'){
    sys_comp_plot_df(plot_df)
  } else if(subtab == 'phd'){
    sys_phd_plot_df(plot_df)
  }
  
  
  plot_df_supp <- plot_df %>%
    suppress_values("n",keep_orig_var = TRUE) %>%
    suppress_next_val_if_one_suppressed_in_group(selection, "n")
  
  plot_df_joined <- join(plot_df_supp,
                         plot_df_phd %>% 
                           frename(num = n), how = 'left', on=c(selection)) %>% 
    fmutate(frac = ifelse(n == 0 | is.na(n), NA, num / n),
            frac_export = ifelse(n_orig == 0 | is.na(n_orig), 0, num / n_orig))
  
  export_label1 <- paste0(selection, ' (Demographic Section 1)')
  
  export_df <- plot_df_joined %>% 
    join(plot_df_supp %>% 
           fmutate(`Suppression Flag` = ifelse(!is.na(wasRedacted) & wasRedacted, "Yes","No")) %>% 
           fselect(-n, -wasRedacted),
         how = 'left') %>% 
    fmutate(frac_export = scales::percent(frac_export, accuracy=0.1)) %>% 
    fselect(get(selection), 'Total Count' = n_orig, 'Permanent Count' = num, 'Percent in Permanent' = frac_export, `Suppression Flag`) %>% 
    arrange(!!sym(selection))
  names(export_df)[1] <- export_label1
  
  sys_phd_export(export_df)
  
  plot_df_joined <- plot_df_joined %>% fselect(-n_orig, -frac_export)
  plot_df <- plot_df_supp %>% fselect(-n_orig)
  
  
  return(
    ggplot(plot_df_joined, aes("", .data[[selection]])) +
      # main data into cells for each cross-combination
      geom_tile(
        color = '#f0f0f0',
        lwd = 0.5,
        linetype = 1,
        aes(fill = frac)
      ) +
      scale_fill_gradient(
        low = get_brand_color('light_green'),
        high = get_brand_color('dark_green'),
        breaks = seq(0,1,by=0.05),
        na.value = ifelse(
          is.na(plot_df_joined$wasRedacted) | !plot_df_joined$wasRedacted,
          "white",
          get_brand_color('light_green')
        )
      ) + # na.value makes 0s invisible
      # set text color to be 508 compliant contrasting
      geom_text(
        aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n', '(',num,' of ',n,')'))),
        size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction * 0.6, 1),
        color = ifelse(
          plot_df_joined$frac > mean(plot_df_joined$frac, na.rm = TRUE) & !plot_df_joined$wasRedacted,
          'white',
          'black'
        )
      ) +
      scale_y_discrete(
        labels = label_wrap(30),
        limits = rev(levels(plot_df[[selection]])),
      ) +
      # other stuff
      theme_bw() +
      ggtitle(syse_total_count_display(
        nrow(comp_df),
        nrow(comp_df_phd)
      )) +
      labs(caption = "*** indicates the value is suppressed") +
      theme(
        text = element_text(size = sys_chart_text_font_pts),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
      )
  )
}

sys_phd_export <- reactiveVal()

sys_phd_plot_2vars <- function(subtab = 'phd', methodology_type, selections, isExport = FALSE) {
  # race/ethnicity, if selected, should always be on the row
  var_cols <- get_var_cols(methodology_type)
  
  if (selections[1] %in% c("All Races/Ethnicities", "Grouped Races/Ethnicities")) {
    selections <- c(selections[2], selections[1])
  }
  
  # get dataset underlying the freqs we will produce below
  
  comp_df <- all_filtered_syse_demog() %>% 
    remove_non_applicables(selection = selections) %>% 
    select(
      PersonalID, 
      Destination,
      unname(var_cols[[selections[1]]]), 
      unname(var_cols[[selections[2]]])
    ) %>%
    funique()
  
  comp_df_phd <- comp_df %>% 
    fsubset(Destination %in% perm_livingsituation) %>% 
    fselect(-Destination)
  
  comp_df <- comp_df %>% fselect(-Destination)
  
  
  
  validate(
    need(
      nrow(comp_df) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      nrow(comp_df) > 10,
      message = suppression_msg
    )
  )
  
  plot_df <- get_sys_plot_df_2vars(comp_df, var_cols, selections = selections)
  plot_df_phd <- get_sys_plot_df_2vars(comp_df_phd, var_cols, selections = selections)
  
  
  
  toggle_download_buttons(subtab, plot_df)
  
  type <- 'exits'
  
  selection_cats1 <- get_selection_cats(selections[1], type = type)
  
  if (is.null(names(selection_cats1))) {
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  selection_cats2 <- get_selection_cats(selections[2], type = type)
  if (is.null(names(selection_cats2))) {
    selection_cats2_labels <- selection_cats2
  } else {
    selection_cats2_labels <- names(selection_cats2)
  }
  
  plot_df[selections[1]] <- factor(
    plot_df[[selections[1]]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels)
  
  plot_df[selections[2]] <- factor(
    plot_df[[selections[2]]], 
    levels = selection_cats2, 
    labels = selection_cats2_labels)
  
  plot_df <- plot_df %>%
    complete(
      !!sym(selections[1]),
      !!sym(selections[2])
    ) %>%
    replace(is.na(.), 0)
  
  plot_df_phd[selections[1]] <- factor(
    plot_df_phd[[selections[1]]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels)
  
  plot_df_phd[selections[2]] <- factor(
    plot_df_phd[[selections[2]]], 
    levels = selection_cats2, 
    labels = selection_cats2_labels)
  
  plot_df_phd <- plot_df_phd %>%
    complete(
      !!sym(selections[1]),
      !!sym(selections[2])
    ) %>%
    replace(is.na(.), 0)
  
  if(methodology_type == 1) {
    
    h_total <- plot_df %>%
      group_by(!!!syms(selections[[2]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[1]] := 'Total') %>%
      suppress_values("N") %>%
      suppress_next_val_if_one_suppressed_in_group(selections[1], "N")
    
    v_total <- plot_df %>%
      group_by(!!!syms(selections[[1]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[2]] := 'Total') %>%
      suppress_values("N") %>%
      suppress_next_val_if_one_suppressed_in_group(selections[2], "N")
    
    h_total_phd <- plot_df_phd %>%
      group_by(!!!syms(selections[[2]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[1]] := 'Total') #%>%
    #suppress_values("N") %>%
    #suppress_next_val_if_one_suppressed_in_group(selections[1], "N")
    
    v_total_phd <- plot_df_phd %>%
      group_by(!!!syms(selections[[1]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[2]] := 'Total') #%>%
    #suppress_values("N") %>%
    #suppress_next_val_if_one_suppressed_in_group(selections[2], "N")
  }
  
  # save before supressing the values
  # this will be used for the download/export
  sys_phd_plot_df(plot_df)
  
  # Suppress values <= 10
  plot_df_supp <- plot_df %>%
    suppress_values("n", keep_orig_var = TRUE) %>%
    suppress_next_val_if_one_suppressed_in_group(selections[1], "n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[2], "n")
  
  # plot_df_phd <- plot_df_phd %>%
  #   suppress_values("n") %>%
  #   suppress_next_val_if_one_suppressed_in_group(selections[1], "n") %>%
  #   suppress_next_val_if_one_suppressed_in_group(selections[2], "n")
  
  plot_df_joined <- join(plot_df_supp,plot_df_phd %>% 
                           frename(num = n), 
                         how = 'left',
                         on = c(selections[1],selections[2])) %>% 
    fmutate(frac = ifelse(n == 0 | is.na(n), NA, num / n),
            frac_export = ifelse(n_orig == 0 | is.na(n_orig), 0, num / n_orig))
  
  export_label1 <- paste0(selections[2], ' (Demographic Section 1)')
  export_label2 <- paste0(selections[1], ' (Demographic Section 2)')
  
  export_df <- plot_df_joined %>% 
    join(plot_df_supp %>% 
           fmutate(`Suppression Flag` = ifelse(!is.na(wasRedacted) & wasRedacted, "Yes","No")) %>% 
           fselect(-n, -wasRedacted, -n_orig),
         how = 'left') %>% 
    fmutate(frac_export = scales::percent(frac_export, accuracy=0.1)) %>% 
    fselect(get(selections[2]), get(selections[1]), 'Total Count' = n_orig, 'Permanent Count' = num, 'Percent in Permanent' = frac_export, `Suppression Flag`) %>% 
    arrange(!!sym(selections[2]))
  names(export_df)[1:2] <- c(export_label1, export_label2)
  
  sys_phd_export(export_df)
  
  if(methodology_type == 1){
    h_total_joined <- join(h_total,h_total_phd %>% frename(num = N), how = 'left', on=c(selections[1],selections[2])) %>% 
      fmutate(frac = ifelse(N == 0 | is.na(N), NA, num / N))
    
    v_total_joined <- join(v_total,v_total_phd %>% frename(num = N), how = 'left', on=c(selections[1],selections[2])) %>% 
      fmutate(frac = ifelse(N == 0 | is.na(N), NA, num / N))
    
  }
  
  plot_df_joined <- plot_df_joined %>% fselect(-n_orig, -frac_export)
  plot_df <- plot_df_supp %>% fselect(-n_orig)
  
  g <- ggplot(plot_df_joined, aes(.data[[selections[1]]], .data[[selections[2]]])) +
    # main data into cells for each cross-combination
    geom_tile(
      color = '#f0f0f0',
      lwd = 0.5,
      linetype = 1,
      aes(fill = frac)
    ) +
    scale_fill_gradient(
      low = get_brand_color('light_green'),
      high = get_brand_color('dark_green'),
      breaks = seq(0,1,by=0.05),
      na.value = ifelse(
        is.na(plot_df_joined$wasRedacted) | !plot_df_joined$wasRedacted,
        "white",
        get_brand_color('light_green')
      )
    ) + # na.value makes 0s invisible
    # set text color to be 508 compliant contrasting
    geom_text(
      # aes(label = paste0(scales::comma(n), "\n", "(",scales::percent(pct, accuracy = 0.1),")")),
      aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n', '(',num,' of ',n,')'))),#scales::comma(n))),
      size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction * 0.6, 1),
      color = ifelse(
        plot_df_joined$frac > mean(plot_df_joined$frac, na.rm = TRUE) & !plot_df_joined$wasRedacted,
        'white',
        'black'
      )
    )
  
  
  x_labels <- selection_cats1_labels
  x_limits <- levels(plot_df[[selections[1]]])
  y_labels <- rev(selection_cats2_labels)
  y_limits <- rev(levels(plot_df[[selections[2]]]))
  
  if(methodology_type == 1) {
    x_labels <- c(x_labels, "Total")
    x_limits <- c(x_limits, "Total")
    y_labels <- c("Total", y_labels)
    y_limits <- c("Total", y_limits)
    g <- g + 
      ggnewscale::new_scale("fill") +
      # Row totals
      geom_tile(
        data = h_total_joined,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = frac)
      ) +
      
      scale_fill_gradient(
        low = get_brand_color('light_grey'),
        high = get_brand_color('dark_grey'),
        breaks = seq(0,1,by=0.05),
        na.value = ifelse(h_total_joined$wasRedacted, get_brand_color('light_grey'), 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n', '(',num,' of ',N,')'))),
        size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction* 0.6, 1),
        color = ifelse(
          h_total_joined$frac > mean(h_total_joined$frac, na.rm = TRUE) & !h_total_joined$wasRedacted,
          'white',
          'black'
        ),
        data = h_total_joined
      ) +
      
      # column totals
      ggnewscale::new_scale("fill") +
      geom_tile(
        data = v_total_joined,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = frac)
      ) +
      scale_fill_gradient(
        low = get_brand_color('light_grey'),
        high = get_brand_color('dark_grey'),
        breaks = seq(0,1,by=0.05),
        na.value = ifelse(v_total_joined$wasRedacted, get_brand_color('light_grey'), 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n', '(',num,' of ',N,')'))),
        size = sys_chart_text_font * ifelse(isExport, 0.45, 1),
        color = ifelse(
          v_total_joined$frac > mean(v_total_joined$frac, na.rm = TRUE) & !v_total_joined$wasRedacted,
          'white',
          'black'
        ),
        data = v_total_joined
      )
  }
  g + 
    # axis labels
    scale_x_discrete(
      labels = str_wrap(x_labels, width = 20),
      limits = x_limits,
      position = "top"
    ) +
    scale_y_discrete(
      labels = str_wrap(y_labels, width = 30),
      limits = y_limits
    ) +
    
    # other stuff
    theme_bw() +
    
    ggtitle(syse_total_count_display(
      nrow(comp_df),
      nrow(comp_df_phd)
    )) +
    labs(caption = "*** indicates the value is suppressed") +
    
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # axis.title.x.top = element_text(margin = margin(0, 0, 15, 0)),
      axis.text = element_text(size = sys_comp_axis_text_font * ifelse(windowSize()[1] < 1300, 0.8, 1) * ifelse(isExport, 0.45, 1))
    )
}


tree_exits_data <- reactive({
  all_filtered_syse()  %>% 
    fselect( Destination, PersonalID, EnrollmentID) %>% 
    fmutate(`Destination Type` = fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    )) 
})

syse_types_chart <- function(varname, status, show_legend = FALSE){
  
  tree_colors <- c(
    "Permanent" = get_brand_color('dark_blue'),
    "Homeless" = get_brand_color('coral'),
    "Institutional" = get_brand_color('med_grey'),
    "Temporary" = get_brand_color('med_grey'),
    "Other/Unknown" = get_brand_color('med_grey')
  )
  nr <- nrow(tree_exits_data())
  
  validate(need(nr > 0, no_data_msg))
  validate(need(nr > 10, suppression_msg))
  
  tree_exits_summ <- tree_exits_data() %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(Count = GRPN(), 
               Percent = GRPN() / nr) %>% 
    fungroup() %>% 
    fmutate(Percent = Count/fsum(Count),
            text_color = fifelse(`Destination Type` %in% c('Temporary','Institutional','Other/Unknown'), 'black', 'white'),
            label = str_c(`Destination Type`, ': ', scales::label_comma()(Count),
                          ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
            )) %>% 
    fmutate(border_color = "black") %>% 
    fmutate(subgroup2 = factor(
      fifelse(`Destination Type` %in% c('Permanent','Homeless'), 'group1', 'group2'),
      levels = c('group1','group2'))
    ) %>%
    roworder('subgroup2', 'Destination Type') 
  
  if(show_legend == FALSE){
    ggplot(tree_exits_summ, aes(area = Count, fill = `Destination Type`,
                                label = label, subgroup = border_color, subgroup2 = subgroup2 ) )+
      labs(title = paste0(scales::label_comma()(nr), " System Exits for ",
                          syse_level_of_detail_text(), " in ",
                          str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
                          if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"),"\n")
      ) +
      geom_treemap(layout='squarified', start='bottomright',color = "black", size = 2, show.legend = FALSE) +
      geom_treemap_text(layout='squarified', start='bottomright',aes(color = text_color),  place = "center", grow = FALSE, reflow = TRUE) +
      geom_treemap_subgroup_border(layout='squarified',start='bottomright',color = "black", size = 4, show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_manual(values = tree_colors) +
      theme_minimal() +
      coord_fixed(ratio =0.8) +
      theme(
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
      )
    
  } else if (show_legend == TRUE){
    ggplot(tree_exits_summ, aes(area = Count, fill = `Destination Type`,
                                label = label, subgroup = border_color, subgroup2 = subgroup2 ) )+
      labs(title = paste0(scales::label_comma()(nr), " System Exits for ",
                          syse_level_of_detail_text(), " in ",
                          str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
                          if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"),"\n")
      ) +
      geom_treemap(layout='squarified', start='bottomright',color = "black", size = 2, show.legend = FALSE,) +
      geom_treemap_text(layout='squarified', start='bottomright',aes(color = text_color),  place = "center", grow = FALSE, reflow = TRUE) +
      geom_treemap_subgroup_border(layout='squarified',start='bottomright',color = "black", size = 4, show.legend = FALSE)
    scale_color_identity() +
      scale_fill_manual("",breaks = tree_exits_summ$label, values = setNames(tree_colors, tree_exits_summ$label)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
        legend.text = element_text(size = sys_chart_text_font),
        legend.position = 'bottom'
      )
  }
  
}


output$syse_types_download_btn <- downloadHandler( filename = date_stamped_filename("System Exits by Type Report - "),
                                                   content = function(file) 
                                                   {
                                                     logToConsole(session, "System Exits by Type data download")
                                                     
                                                     write_xlsx(
                                                       list(
                                                         "SystemExitsByType Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
                                                           rowbind(
                                                             sys_export_filter_selections(type = 'exits'),
                                                             data.table(Chart = 'Total System Exits', Value = scales::label_comma()(nrow(tree_exits_data())))              
                                                           ) %>% 
                                                           frename('System Exits by Type' = Value),
                                                         
                                                         "SystemExitTypesData" = tree_exits_data() %>% 
                                                           fmutate(`Destination Type Detail` = living_situation(Destination)) %>% 
                                                           fgroup_by(`Destination Type`,`Destination Type Detail`, sort = TRUE) %>% 
                                                           fsummarize(Count = GRPN()) %>% 
                                                           fungroup() %>% 
                                                           list_all_destinations(fill_zero = TRUE) %>% 
                                                           fmutate(Percent = scales::label_percent(accuracy = 0.1,scale=100)(Count / fsum(Count)))
                                                       ),
                                                       path = file,
                                                       format_headers = FALSE,
                                                       col_names = TRUE
                                                     )  
                                                     
                                                     logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
                                                                                 if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
                                                     
                                                   })

output$syse_types_download_btn_ppt <- downloadHandler(filename = function() {
  paste("System Exits by Type_", Sys.Date(), ".pptx", sep = "")
},
content = function(file) {
  logToConsole(session, "In syse_types_download_btn_ppt")
  
  sys_perf_ppt_export(file = file, 
                      type = 'exits',
                      title_slide_title = "System Exits by Type",
                      summary_items = sys_export_summary_initial_df(type = 'exits') %>%
                        fsubset(Chart != "Start Date" & Chart != "End Date") %>% 
                        rowbind(sys_export_filter_selections(type = 'exits'),
                                data.table(Chart="Total System Exits", Value = scales::label_comma()(nrow(tree_exits_data())))),
                      plots = list("System Exits by Type" = syse_types_chart("Destination Type", input$syse_dest_type_filter)),
                      summary_font_size = 19,
                      startDate = session$userData$ReportStart, 
                      endDate = session$userData$ReportEnd, 
                      sourceID = session$userData$Export$SourceID,
                      in_demo_mode = input$in_demo_mode
  )
})


# System Exit Comparisons  ------------------------------------------------

subpop_chart_validation <- function(hh_type, level_of_detail, project_type, raceeth, vetstatus, age, show = TRUE, req = FALSE) {
  logToConsole(session, "In subpop_chart_validation")
 
  cond1 <- hh_type != 'All' | level_of_detail != 'All' | project_type != 'All'
  cond2 <-  raceeth != "All" | vetstatus != "None" | length(age) != length(sys_age_cats)
  
  filter_type <- input$subpop_comparison_type_filter
  if(filter_type == 'Client-Level'){
    cond <- cond1
  } else if(filter_type == 'Demographics'){
    cond <- cond2
  } else if(filter_type == 'Both'){
    cond <- cond1 | cond2
  }
  
  ## whether to show validate message or not
  if(show){
    validate(
      need(
        cond,#"All Ages",
        message = "Please select one or more demographic filters to generate the subpopulation chart and table."
      )
    )
  } else if (req){
    ##  just hide but do not show a duplicate validate message
    req(cond)
  } else {
    ## otherwise, just return TRUE/VALSE of condition
    return(cond)
  }
}

subpop2_chart_validation <- function(show = TRUE, req = FALSE) {
  logToConsole(session, "In subpop2_chart_validation")
  
  
  cond <- any(did_factors_change()) 
  
  ## whether to show validate message or not
  if(show){
    validate(
      need(
        cond,#"All Ages",
        message = "Please select a household type or one or more demographic filters to generate the subpopulation chart."
      )
    )
  } else if (req){
    ##  just hide but do not show a duplicate validate message
    req(cond)
  } else {
    ## otherwise, just return TRUE/VALSE of condition
    return(cond)
  }
}

time_chart_validation <- function(startDate, endDate, raceeth, vetstatus, age, show = TRUE) {
  logToConsole(session, "In time_chart_validation")
  
  cond <- interval(startDate, endDate) >= days(729)

  ## whether to show validate message or not
  if(show){
    validate(
      need(
        cond,
        message = "Data will not be shown for reporting periods of less than 2 years."
      )
    )
  } else {
    ## otherwise, just hide but do not show a duplicate validate message
    req(cond)
  }
  
}

syse_subpop2_export_summary <- reactive({

  subpop2_table_df <- get_syse_compare_subpop2_data(output_type = 'table') 
  
  which_factors_changed <- names(which(did_factors_change() == 1))
  
  labels_factors_changed <- c(
    meets_hh_type = ifelse('meets_hh_type' %in% which_factors_changed && input$syse_hh_type != 'All', getNameByValue(sys_hh_types,input$syse_hh_type), NA_character_),
    meets_age_filter = ifelse('meets_age_filter' %in% which_factors_changed && length(input$syse_subpop2_age) < length(sys_age_cats), paste0(input$syse_subpop2_age, collapse=', '), NA_character_),
    meets_race_eth_filter = ifelse('meets_race_eth_filter' %in% which_factors_changed && input$syse_subpop2_race_ethnicity1 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(1), input$syse_subpop2_race_ethnicity1), collapse=','), 
                                   ifelse(input$syse_subpop2_race_ethnicity2 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(2), input$syse_subpop2_race_ethnicity2)) , NA_character_)),
    meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed && input$syse_subpop2_spec_pops != 'None', input$syse_subpop2_spec_pops, NA_character_)
  )
  labels_all_other <- c(
    meets_hh_type = 'All Other Household Types',
    meets_age_filter = 'All Other Ages',
    meets_race_eth_filter = 'All Other Races/Ethnicities',
    meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed, paste0(setdiff(c('Veteran','Non-Veteran'), input$syse_subpop2_spec_pops), 's'), NA_character_)
  )
  
  if('meets_hh_type' %in% which_factors_changed){
    levels(subpop2_table_df[['meets_hh_type']]) <- c(labels_factors_changed['meets_hh_type'],labels_all_other['meets_hh_type'])
  } else {
    subpop2_table_df <- subpop2_table_df %>% 
      fmutate(meets_hh_type = getNameByValue(sys_hh_types, input$syse_hh_type))
  }
  
  if('meets_age_filter' %in% which_factors_changed){
    levels(subpop2_table_df[['meets_age_filter']]) <- c(labels_factors_changed['meets_age_filter'],labels_all_other['meets_age_filter'])
  } else {
    subpop2_table_df <- subpop2_table_df %>% 
      fmutate(meets_age_filter = "All Ages")
  }
  
  if('meets_race_eth_filter' %in% which_factors_changed){
    levels(subpop2_table_df[['meets_race_eth_filter']]) <- c(labels_factors_changed['meets_race_eth_filter'],labels_all_other['meets_race_eth_filter'])
  } else {
    subpop2_table_df <- subpop2_table_df %>% 
      fmutate(meets_race_eth_filter = getNameByValue(sys_race_ethnicity_cats(input$syse_methodology_type), 
                                                     switch(input$syse_methodology_type, '1'=input$syse_subpop2_race_ethnicity1,
                                                            '2' = input$syse_subpop2_race_ethnicity2))
      )
  }
  
  if('meets_vet_filter' %in% which_factors_changed){
    levels(subpop2_table_df[['meets_vet_filter']]) <- c(labels_factors_changed['meets_vet_filter'],labels_all_other['meets_vet_filter'])
  } else {
    subpop2_table_df <- subpop2_table_df %>% 
      fmutate(meets_vet_filter = getNameByValue(sys_spec_pops_people, input$syse_subpop2_spec_pops))
  }
  
  export_names <- c('Household Type' = 'meets_hh_type', 'Race/Ethnicity' = 'meets_race_eth_filter',
                   'Age' = 'meets_age_filter', 'Veteran Status' = 'meets_vet_filter',
                   'Suppression Flag' = 'wasRedacted','Count' = 'N','Total System Exits' = 'total','Percent of Total System Exits' = 'pct')
  
  subpop2_table_df %>% 
    fmutate(pct = ifelse(is.nan(pct), 0, pct),
            pct = scales::percent(pct, accuracy=0.1)) %>% 
    get_vars( vars=c(which_factors_changed, 'Destination Type','N','total','pct','wasRedacted')) %>% 
    arrange(pick(which_factors_changed), 'Destination Type') %>% 
    rename(any_of(export_names))

})

syse_subpop2_export_detail <- reactive({
  
  
  ## compute subcategories of destination types for data export - not shown in chart or table
  pct_subpop_sub <- subpop2() %>% 
    fselect( Destination, PersonalID, EnrollmentID) %>% 
    fmutate(`Destination Type` = fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    ))  %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_subpop = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_subpop = count_subpop /sum(count_subpop, na.rm=T))
  
  pct_comparison_sub <- everyone_else2() %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_comparison = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_comparison = count_comparison/sum(count_comparison, na.rm=T))
 
  pct_subpop_totals <- pct_subpop_sub %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
               count_subpop = sum(count_subpop), 
               pct_subpop = sum(pct_subpop)) 
  
  pct_comparison_totals <- pct_comparison_sub %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
               count_comparison = sum(count_comparison), 
               pct_comparison = sum(pct_comparison)) 
  
  #full_join(pct_prev_year_sub, pct_current_year_sub) %>% 
  full_join(pct_subpop_sub %>% 
              rowbind(pct_subpop_totals), 
            pct_comparison_sub %>% 
              rowbind(pct_comparison_totals), 
            by=c('Destination Type','Destination Type Detail')) %>%    
    list_all_destinations(fill_zero = TRUE, add_totals = TRUE) %>% 
    fmutate(#pct_diff = map2_chr(count_subpop, count_comparison, .f = calc_pct_diff),
            total_count = count_subpop + count_comparison,
            pct_comparison = scales::percent(pct_comparison, accuracy = 0.1,scale=100),
            pct_subpop = scales::percent(pct_subpop, accuracy = 0.1,scale=100)) %>% 
    fselect(`Destination Type`, `Destination Type Detail`, 'Subpopulation %' = pct_subpop, 'Subpopulation Count' = count_subpop, 
            #'Percent Difference' = pct_diff, 
            'Everyone Else %' = pct_comparison, 'Everyone Else Count' = count_comparison)#, 'Total Count' = total_count)
})

everyone_else <- reactive({
  
  filter_type <- input$subpop_comparison_type_filter
  
  if(filter_type == "Client-Level"){
    enrl <- enrollments_filtered_syse()
    client <- session$userData$client_categories
  } else if(filter_type == "Demographics"){
    enrl <- session$userData$enrollment_categories
    client <- syse_client_categories_filtered()
  } else if(filter_type == "Both"){
    enrl <- session$userData$enrollment_categories
    client <- session$userData$client_categories
  }
  
  ## first apply enrollment filters, if any
  enrolled_filt <- join(
    enrl,
    client %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  )
  
  ## get system exits - expand_by_period + get_active...
  enrolled_w_exits <- enrolled_filt %>% 
    expand_by_periods(chart_type = 'exits_types') %>% 
    get_active_info(enrolled_filt) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive'))  %>% 
    ## drop rows that are in the filtered version - (everyone minus subpop)
    fsubset(!(EnrollmentID %in% all_filtered_syse_subpop()$EnrollmentID))
  
  ## special case for VeteranStatus: exclude children from Everyone Else group in both Veteran and Non-Veteran cases
  if(input$syse_spec_pops != sys_spec_pops_people[1]){
    enrolled_w_exits <- enrolled_w_exits %>% 
      join(session$userData$client_categories %>% fselect(PersonalID, AgeCategory), how='left') %>% 
      fsubset(!(AgeCategory %in% c("0 to 12", "13 to 17")))
  }
  
  ## add destination type detail
  enrolled_w_exits %>% 
    fmutate(`Destination Type` = fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    )) %>% 
    fmutate(
      `Destination Type` = factor(`Destination Type`, levels = c('Permanent','Homeless','Institutional','Temporary','Other/Unknown'))
    )
  
})

everyone_else2 <- reactive({
  comps()$everyone_else %>% 
    fmutate(`Destination Type` = fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    ))
})

## add counts in parens for table formatting
format_compare_value <- function(count, total){
  pct <- scales::percent(count/total, accuracy = 1, scale = 100)
  sprintf('%s (%s)', pct, count)
}

## % difference: used for subpop charts
calc_pct_diff <- function(val1, val2, format = 'char'){
  if(val1 == 0 | val2 == 0){
    ifelse(format == 'char', '-', NA)
  } else {
    pct_diff <- abs(val1 - val2)/((val1 + val2)/2)
    ifelse(format == 'char', 
           scales::percent(pct_diff, accuracy = 0.1, scale = 100),
           pct_diff)
  }
}

# % change: used for time charts
calc_pct_change <- function(count_prev, count_current, accuracy = 1, format='char'){
  if(count_prev == 0){
    ifelse(format=='char', '-', NA)
  } else {
    pct_change <- (count_current - count_prev) / count_prev
    
    ifelse(format == 'char', 
           scales::percent(pct_change, accuracy = accuracy, scale = 100),
           pct_change)
  }
}

# get_syse_compare_subpop_data <- function(output_type = 'table'){
#   
#   validate(need(nrow(all_filtered_syse_subpop()) > 0, no_data_msg))
#   validate(need(nrow(all_filtered_syse_subpop()) > 10, suppression_msg))
#   
#   .total_s <- fnrow(all_filtered_syse_subpop())
#   
#   count_subpop <- all_filtered_syse_subpop() %>% 
#     fselect( Destination, PersonalID, EnrollmentID) %>% 
#     fmutate(`Destination Type` = fcase(
#       Destination %in% perm_livingsituation, 'Permanent',
#       Destination %in% 100:199, 'Homeless',
#       Destination %in% temp_livingsituation, 'Temporary',
#       Destination %in% institutional_livingsituation, 'Institutional',
#       Destination %in% other_livingsituation, 'Other/Unknown',
#       default = 'Other/Unknown'
#     )) %>% fsummarize(
#       'Permanent' = fsum(`Destination Type` == 'Permanent'),
#       'Homeless'= fsum(`Destination Type` == 'Homeless'),
#       'Institutional' = fsum(`Destination Type` == 'Institutional'),
#       'Temporary' = fsum(`Destination Type` == 'Temporary'),
#       'Other/Unknown' = fsum(`Destination Type` == 'Other/Unknown')
#     )
#   
#   .total_e <- fnrow(everyone_else())
#   
#   count_everyone_else <- everyone_else() %>% fsummarize(
#     'Permanent' = fsum(`Destination Type` == 'Permanent'),
#     'Homeless'= fsum(`Destination Type` == 'Homeless'),
#     'Institutional' = fsum(`Destination Type` == 'Institutional'),
#     'Temporary' = fsum(`Destination Type` == 'Temporary'),
#     'Other/Unknown' = fsum(`Destination Type` == 'Other/Unknown')
#   )
#   
#   if(output_type == 'chart'){
#     
#     pct_subpop <- count_subpop / .total_s
#     pct_everyone_else <- count_everyone_else / .total_e
#     
#     pct_diff <- purrr::map2_dfr(count_subpop, count_everyone_else, .f = calc_pct_diff, format='num')
#     
#     data.table(
#       subpop_summ = c("Subpopulation","Everyone Else","Percent Difference"),
#       round(
#         rowbind(
#           pct_subpop,
#           pct_everyone_else,
#           pct_diff
#         ),
#         2)
#     )
#     
#   } else if (output_type == 'table'){
#     
#     pct_diff <- purrr::map2_dfr(count_subpop, count_everyone_else, .f = calc_pct_diff)
#     
#     pct_subpop <- count_subpop %>% 
#       fmutate(
#         'Permanent' = format_compare_value(Permanent, .total_s),
#         'Homeless'= format_compare_value(Homeless, .total_s),
#         'Institutional' = format_compare_value(Institutional, .total_s),
#         'Temporary' = format_compare_value(Temporary, .total_s),
#         'Other/Unknown' = format_compare_value(`Other/Unknown`, .total_s)
#       )
#     
#     pct_everyone_else <- count_everyone_else %>% 
#       fmutate(
#         'Permanent' = format_compare_value(Permanent, .total_e),
#         'Homeless'= format_compare_value(Homeless, .total_e),
#         'Institutional' = format_compare_value(Institutional, .total_e),
#         'Temporary' = format_compare_value(Temporary, .total_e),
#         'Other/Unknown' = format_compare_value(`Other/Unknown`, .total_e)
#       )
#     
#     data.table(
#       subpop_summ = c("Subpopulation","Everyone Else","Percent Difference"),
#       rowbind(
#         pct_subpop,
#         pct_everyone_else,
#         pct_diff
#       )
#     )
#   }
#   
#   
# }

get_syse_compare_subpop2_data <- function(output_type = 'table'){
  
  validate(need(nrow(subpop2()) > 0, no_data_msg))
  validate(need(nrow(subpop2()) > 10, suppression_msg))
  
  validate(need(nrow(everyone_else2()) > 0, no_data_msg))
  validate(need(nrow(everyone_else2()) > 10, suppression_msg))
  
  
  df_subpop <- subpop2() %>% 
    fmutate(`Destination Type` = factor(fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    ), levels = c('Permanent','Homeless','Institutional','Temporary','Other/Unknown')))
  
  .total_s <- fnrow(df_subpop)
  
  count_subpop <- df_subpop %>%
    count(`Destination Type`, .drop=FALSE, name='N') %>% 
    fmutate(total = fsum(N), wasRedacted = total < 10, pct = ifelse(wasRedacted * (output_type == "chart"), NA, N / total))
  
  .total_e <- fnrow(everyone_else2())
  
  count_df <- expand.grid(
    meets_hh_type = c(TRUE, FALSE),
    meets_age_filter = c(TRUE, FALSE),
    meets_race_eth_filter = c(TRUE, FALSE),
    meets_vet_filter = c(TRUE, FALSE)
  )
  
  filt_vars <- c('meets_hh_type','meets_age_filter','meets_race_eth_filter','meets_vet_filter')
  which_factors_changed <- names(which(did_factors_change() == 1))
  
  count_everyone_else <- everyone_else2() %>%
    count(`Destination Type`, meets_hh_type, meets_age_filter, meets_race_eth_filter, meets_vet_filter,.drop=F,name='N') %>%
    fgroup_by(meets_hh_type, meets_age_filter, meets_race_eth_filter, meets_vet_filter) %>% 
    #fmutate(pct = N / fsum(N)) %>% 
    fmutate(total = fsum(N), wasRedacted = total < 10, pct = ifelse(wasRedacted * (output_type == "chart"), NA, N / total)) %>% 
    fungroup() %>%
    dplyr::filter(if_all(setdiff(filt_vars, which_factors_changed), ~ .x == TRUE)) %>% 
    fsubset(((meets_hh_type==T) + (meets_age_filter==T) + (meets_race_eth_filter==T) + (meets_vet_filter==T)) < 4)
  
  # which_factors_changed <- names(which(did_factors_change() == 1))
  # labels_factors_changed <- c(
  #   meets_hh_type = ifelse('meets_hh_type' %in% which_factors_changed && input$syse_hh_type != 'All', getNameByValue(sys_hh_types,input$syse_hh_type), NA_character_),
  #   meets_age_filter = ifelse('meets_age_filter' %in% which_factors_changed && length(input$syse_subpop2_age) < length(sys_age_cats), paste0(input$syse_subpop2_age, collapse=', '), NA_character_),
  #   meets_race_eth_filter = ifelse('meets_race_eth_filter' %in% which_factors_changed && input$syse_subpop2_race_ethnicity1 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(1), input$syse_subpop2_race_ethnicity1), collapse=','), 
  #                                  ifelse(input$syse_subpop2_race_ethnicity2 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(2), input$syse_subpop2_race_ethnicity2)) , NA_character_)),
  #   meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed && input$syse_subpop2_spec_pops != 'None', input$syse_subpop2_spec_pops, NA_character_)
  # )
  # labels_all_other <- c(
  #   meets_hh_type = 'All Other Household Types',
  #   meets_age_filter = 'All Other Ages',
  #   meets_race_eth_filter = 'All Other Races/Ethnicities',
  #   meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed, paste0(setdiff(c('Veteran','Non-Veteran'), input$syse_subpop2_spec_pops), 's'), NA_character_)
  # )
  
  if(output_type == 'chart'){
    
    rowbind(
      count_subpop %>% fmutate(meets_hh_type = TRUE, meets_age_filter = TRUE, 
                               meets_race_eth_filter = TRUE, meets_vet_filter = TRUE, group = 'subpop'),
      count_everyone_else %>% fmutate(group = 'everyone_else')
    )
    
    
  } else if (output_type == 'table'){
    
    rowbind(
      count_subpop %>% fmutate(meets_hh_type = TRUE, meets_age_filter = TRUE, 
                               meets_race_eth_filter = TRUE, meets_vet_filter = TRUE, group = 'subpop'),
      count_everyone_else %>% fmutate(group = 'everyone_else')
    )
  }
}

syse_time_export <- reactive({
  
  prev_year <- everyone() %>% 
    fsubset(period == 'Previous Year')
  
  current_year <- everyone() %>% 
    fsubset(period == 'Current Year')
  
  ## compute subcategories of destination types for data export - not shown in chart or table
  pct_prev_year_sub <- prev_year %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_prev_year = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_prev_year = count_prev_year/fsum(count_prev_year, na.rm=T))
  
  pct_current_year_sub <- current_year %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_cur_year = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_cur_year = count_cur_year/fsum(count_cur_year, na.rm=T))
  
  
  pct_prev_year_totals <- pct_prev_year_sub %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
               count_prev_year = fsum(count_prev_year), 
               pct_prev_year = fsum(pct_prev_year)) 
  
  pct_current_year_totals <- pct_current_year_sub %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
               count_cur_year = fsum(count_cur_year), 
               pct_cur_year = fsum(pct_cur_year)) 
  
  #full_join(pct_prev_year_sub, pct_current_year_sub) %>% 
  full_join(pct_prev_year_sub %>% 
              rowbind(pct_prev_year_totals), 
            pct_current_year_sub %>% 
              rowbind(pct_current_year_totals), 
            by=c('Destination Type','Destination Type Detail')) %>%    
    list_all_destinations(fill_zero = TRUE, add_totals = TRUE) %>% 
    fmutate(pct_change = map2_chr(count_prev_year, count_cur_year, .f = calc_pct_change, accuracy = 0.1),
            pct_cur_year = scales::percent(pct_cur_year, accuracy=0.1, scale=100), 
            pct_prev_year = scales::percent(pct_prev_year, accuracy=0.1, scale=100)) %>% 
    fselect(`Destination Type`, `Destination Type Detail`, 'Previous Year %' = pct_prev_year, 'Current Year %' = pct_cur_year, 
            'Percent Change' = pct_change, 'Previous Year Count' = count_prev_year, 'Current Year Count' = count_cur_year)
  
})

everyone <- reactive({
  all_filtered_syse_time() %>% 
    fmutate(`Destination Type` = fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    )) %>% 
    fmutate(
      `Destination Type` = factor(`Destination Type`, levels = c('Permanent','Homeless','Institutional','Temporary','Other/Unknown'))
    )
})

get_syse_compare_time_data <- function(output_type = 'table'){
  
  validate(need(nrow(all_filtered_syse_time()) > 0, no_data_msg))
  validate(need(nrow(all_filtered_syse_time()) > 10, suppression_msg))
  
  prev_year <- everyone() %>% 
    fsubset(period == 'Previous Year')

  current_year <- everyone() %>% 
    fsubset(period == 'Current Year')
  
  count_prev_year <- prev_year %>% fsummarize(
    'Permanent' = fsum(`Destination Type` == 'Permanent'),
    'Homeless'= fsum(`Destination Type` == 'Homeless'),
    'Institutional' = fsum(`Destination Type` == 'Institutional'),
    'Temporary' = fsum(`Destination Type` == 'Temporary'),
    'Other/Unknown' = fsum(`Destination Type` == 'Other/Unknown')
  )
  
  count_current_year <- current_year %>% fsummarize(
    'Permanent' = fsum(`Destination Type` == 'Permanent'),
    'Homeless'= fsum(`Destination Type` == 'Homeless'),
    'Institutional' = fsum(`Destination Type` == 'Institutional'),
    'Temporary' = fsum(`Destination Type` == 'Temporary'),
    'Other/Unknown' = fsum(`Destination Type` == 'Other/Unknown')
  )
  
  .total_p <- fnrow(prev_year)
  
  .total_c <- fnrow(current_year)
  
  if(output_type == 'chart'){
    pct_prev_year <- count_prev_year / .total_p

    pct_current_year <- count_current_year / .total_c
    
    pct_change <- map2_dfr(count_prev_year, count_current_year, .f = calc_pct_change, format='num')
    
    data.table(
      time_summ = c("Current Year","Previous Year","Percent Change"),
      round(rowbind(
        pct_current_year,
        pct_prev_year,
        pct_change
      ), 2)
    )
    
  } else if(output_type == 'table'){
    
    .total_p <- fnrow(prev_year)
    
    pct_prev_year <- count_prev_year %>% fsummarize(
      'Permanent' = format_compare_value(Permanent, .total_p),
      'Homeless'= format_compare_value(Homeless, .total_p),
      'Institutional' = format_compare_value(Institutional, .total_p),
      'Temporary' = format_compare_value(Temporary, .total_p),
      'Other/Unknown' = format_compare_value(`Other/Unknown`, .total_p)
    )
    
    .total_c <- fnrow(current_year)
    
    pct_current_year <- count_current_year %>% fsummarize(
      'Permanent' = format_compare_value(Permanent, .total_c),
      'Homeless'= format_compare_value(Homeless, .total_c),
      'Institutional' = format_compare_value(Institutional, .total_c),
      'Temporary' = format_compare_value(Temporary, .total_c),
      'Other/Unknown' = format_compare_value(`Other/Unknown`, .total_c)
    )
    
    pct_change <- map2_dfr(count_prev_year, count_current_year, .f = calc_pct_change)
    
    data.table(
      time_summ = c("Current Year","Previous Year","Percent Change"),
      rowbind(
        pct_current_year,
        pct_prev_year,
        pct_change
      )
    )
  }
  
}

syse_subpop2_selections <- reactive({
  possible <- c("Age","Race/Ethnicity","Veteran Status (Adult Only)")
  selected <- which(c(input$syse_subpop2_age_selection, input$syse_subpop2_race_eth_selection, input$syse_subpop2_vet_selection))
  
  vals <- possible[selected]
  if("Race/Ethnicity" %in% vals){
    vals[vals == "Race/Ethnicity"] <- c("All Races/Ethnicities","Grouped Races/Ethnicities")[as.numeric(input$syse_methodology_type)]
  }
  
  vals
})

observeEvent(input$syse_subpop2_age_selection,
               {
                if(isTruthy(input$syse_subpop2_age_selection)){
                  shinyjs::enable(id = 'age_picker')
                } else {
                  shinyjs::disable(id = 'age_picker')
                }                 
               })

observeEvent(input$syse_subpop2_race_eth_selection,
             {
               if(isTruthy(input$syse_subpop2_race_eth_selection)){
                 shinyjs::enable(id = 'race_eth_picker')
               } else {
                 shinyjs::disable(id = 'race_eth_picker')
               }                 
             }, ignoreInit=F)

observeEvent(input$syse_subpop2_vet_selection,
             {
               if(isTruthy(input$syse_subpop2_vet_selection)){
                 shinyjs::enable(id = 'vet_picker')
               } else {
                 shinyjs::disable(id = 'vet_picker')
               }                 
             })


observeEvent(syse_subpop2_selections(),{
  
  str_vec <- c('Age','Races/Ethnicities','Veteran Status')
  excl_vec <- c('age','race_eth','vet')
  if(length(syse_subpop2_selections()) == 2){
    
    excl <- which(!sapply(str_vec, \(x) any(str_detect(syse_subpop2_selections(), x)) ,USE.NAMES = F))
    shinyjs::disable(id = paste0('syse_subpop2_',excl_vec[excl],'_selection'))
  } else {
    shinyjs::enable(id='syse_subpop2_age_selection')
    shinyjs::enable(id='syse_subpop2_race_eth_selection')
    shinyjs::enable(id='syse_subpop2_vet_selection')
  }
})

did_factors_change <- reactive({
  c(
    meets_hh_type = (input$syse_hh_type != 'All'),
    meets_age_filter = ('Age' %in% syse_subpop2_selections() && length(input$syse_subpop2_age) < length(sys_age_cats)),
    meets_race_eth_filter = ('All Races/Ethnicities' %in% syse_subpop2_selections() && input$syse_subpop2_race_ethnicity1 != 'All') +
      ('Grouped Races/Ethnicities' %in% syse_subpop2_selections() && input$syse_subpop2_race_ethnicity2 != 'All'),
    meets_vet_filter = ('Veteran Status (Adult Only)' %in% syse_subpop2_selections() && input$syse_subpop2_spec_pops != 'None')
  )
})

syse_compare_subpop2_chart <- function(subpop_data = get_syse_compare_subpop2_data(output_type = 'chart'),
                                       dest_type = input$subpop2_dest_type, isExport = FALSE){
  req(all_filtered_syse_subpop())
  
  subgroup_colors <- c(
    'subpop' = get_brand_color('med_purple'),
    'everyone_else' = get_brand_color('med_grey2')
  )
  ## long format needed for plotting points
  subpop2_chart_df <- subpop_data %>% 
    fsubset(`Destination Type` == dest_type)
  
  ## add x-axis labels for PPT download only
  # if(isExport){
  #   #dest_type_labels <- subpop_segment_df$dest_type
  #   #bar_width <- compare_export_bar_width
  #   
  # } else {
  #   #dest_type_labels <- rep(NA,5)    
  #   #bar_width <- compare_bar_width
  #   
  # }
  
  title_start <- paste0("Total System Exits for ",
                        syse_level_of_detail_text(), " in ",
                        str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
                        if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"))
  
  title <- paste0(title_start, 
                  c(paste0(' (Subpopulation): ', scales::label_comma()(nrow(subpop2()))),
                    paste0(' (Everyone Else): ', scales::label_comma()(nrow(everyone_else2())))),
                  collapse='\n'
  )
  
  num_factors_changed <- sum(did_factors_change())
  which_factors_changed <- names(which(did_factors_change() == 1))
  
  labels_factors_changed <- c(
    meets_hh_type = ifelse('meets_hh_type' %in% which_factors_changed && input$syse_hh_type != 'All', getNameByValue(sys_hh_types,input$syse_hh_type), NA_character_),
    meets_age_filter = ifelse('meets_age_filter' %in% which_factors_changed && length(input$syse_subpop2_age) < length(sys_age_cats), paste0(input$syse_subpop2_age, collapse=', '), NA_character_),
    meets_race_eth_filter = ifelse('meets_race_eth_filter' %in% which_factors_changed && input$syse_subpop2_race_ethnicity1 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(1), input$syse_subpop2_race_ethnicity1), collapse=','), 
                                   ifelse(input$syse_subpop2_race_ethnicity2 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(2), input$syse_subpop2_race_ethnicity2)) , NA_character_)),
    meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed && input$syse_subpop2_spec_pops != 'None', input$syse_subpop2_spec_pops, NA_character_)
  )
  labels_all_other <- c(
    meets_hh_type = 'All Other Household Types',
    meets_age_filter = 'All Other Ages',
    meets_race_eth_filter = 'All Other Races/Ethnicities',
    meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed, paste0(setdiff(c('Veteran','Non-Veteran'), input$syse_subpop2_spec_pops), 's'), NA_character_)
  )
  
  
  
  if(num_factors_changed == 1){
    
    levels(subpop2_chart_df[[which_factors_changed]]) <- c(labels_factors_changed[which_factors_changed],labels_all_other[which_factors_changed])
   
    g <- ggplot(subpop2_chart_df, aes(x=!!sym(which_factors_changed), y=1))
    
  } else if(num_factors_changed == 2){
    
    if('meets_hh_type' %in% which_factors_changed){
      
      other_factor <- setdiff(which_factors_changed, 'meets_hh_type')
      
      ## horizontal variable
      levels(subpop2_chart_df[['meets_hh_type']]) <- c(labels_factors_changed['meets_hh_type'],labels_all_other['meets_hh_type'])

      ## horizontal variable
      levels(subpop2_chart_df[[other_factor]]) <- c(labels_factors_changed[other_factor],labels_all_other[other_factor])
      
      g <- ggplot(subpop2_chart_df, aes(x=meets_hh_type, y=fct_rev(!!sym(other_factor))))
      
    } else if ('meets_race_eth_filter' %in% which_factors_changed) {
      other_factor <- setdiff(which_factors_changed, 'meets_race_eth_filter')
      
      ## horizontal variable
      levels(subpop2_chart_df[[other_factor]]) <- c(labels_factors_changed[other_factor],labels_all_other[other_factor])
      
      ## vertical variable
      levels(subpop2_chart_df[['meets_race_eth_filter']]) <- c(labels_factors_changed['meets_race_eth_filter'],labels_all_other['meets_race_eth_filter'])
      
      g <- ggplot(subpop2_chart_df, aes(x=!!sym(other_factor), y=fct_rev(meets_race_eth_filter))
      )        
    } else {
      
      ## horizontal variable
      levels(subpop2_chart_df[[which_factors_changed[1]]]) <- c(labels_factors_changed[which_factors_changed[1]],labels_all_other[which_factors_changed[1]])

      ## vertical variable
      levels(subpop2_chart_df[[which_factors_changed[2]]]) <- c(labels_factors_changed[which_factors_changed[2]],labels_all_other[which_factors_changed[2]])
      
      g <- ggplot(subpop2_chart_df, aes(x=!!sym(which_factors_changed[1]), y=fct_rev(!!sym(which_factors_changed[2])))) 
    
    }
    
  } else if(num_factors_changed == 3){
    
    if('meets_race_eth_filter' %in% which_factors_changed){
      vert_var <- 'meets_race_eth_filter'
      if('meets_hh_type' %in% which_factors_changed){
        horiz_var_outer <- 'meets_hh_type'
        horiz_var_inner <- setdiff(which_factors_changed, c('meets_race_eth_filter','meets_hh_type'))
      } else {
        other_factors <- setdiff(which_factors_changed, vert_var)
        horiz_var_inner <- other_factors[1]#setdiff(which_factors_changed, c('meets_race_eth_filter','meets_hh_type'))
        horiz_var_outer <- other_factors[2]
      }
    } else {
      horiz_var_inner <- which_factors_changed[1]
      horiz_var_outer <- which_factors_changed[2]
      vert_var <- which_factors_changed[3]
      #other_factors <- which_factors_changed[1:2]
    }
    
    ## horizontal variable
    levels(subpop2_chart_df[[horiz_var_inner]]) <- c(labels_factors_changed[horiz_var_inner],labels_all_other[horiz_var_inner])

    ## vertical variable
    levels(subpop2_chart_df[[vert_var]]) <- c(labels_factors_changed[vert_var],labels_all_other[vert_var])
   
    ## facet variable
    levels(subpop2_chart_df[[horiz_var_outer]]) <- c(labels_factors_changed[horiz_var_outer],labels_all_other[horiz_var_outer])
    
    g <- ggplot(subpop2_chart_df, aes(x=!!sym(horiz_var_inner), y=fct_rev(!!sym(vert_var))))
    
    g <- g + facet_wrap(as.formula(paste0('~ ', horiz_var_outer)), strip.position='top',scales='free_x', ncol=2,labeller = label_wrap_gen(18))
  }
  
  g <- g + 
    geom_tile(color='#f0f0f0', lwd=0.5, linetype=1, aes(fill = group)) +
    scale_fill_manual(
      values = subgroup_colors
    ) +
    geom_text(
      aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(pct, accuracy = 1), '\n', '(',N,' of ',total,')'))),#scales::comma(n))),
      size = sys_chart_text_font,
      color = "black"
    ) +
    scale_x_discrete(position='top', labels = label_wrap(15), expand = c(0,0)) +
    scale_y_discrete(labels = label_wrap(15), expand = c(0,0)) +
    labs(x='', y='') +
    theme(panel.spacing = unit(0, "lines"),
          strip.background = element_blank(),
          axis.line = element_blank(),
          panel.grid.major.y =element_blank(),
          strip.placement = "outside",
          strip.text.x.top = element_text(size=sys_axis_text_font, angle=0),
          axis.ticks = element_blank(),
          legend.position = 'none',
          axis.text.x = element_text(size=get_adj_font_size(sys_axis_text_font, isExport)),
          axis.text.y = element_text(size=sys_axis_text_font, hjust=1),#, hjust = 1),
          panel.background = element_rect(fill = 'white', colour = 'white')
    )
  
  g
  
}

## function for System Exits Comparison subpopulation table (below chart)
# get_syse_compare_subpop_table <- function(tab){
#   
#   subgroup_colors <- c(
#     "Subpopulation" = get_brand_color('med_purple'),
#     "Everyone Else" = get_brand_color('med_grey2')
#   )
#   
#   datatable(tab, 
#             colnames = c(' ' = 'subpop_summ',
#                          "<b>Permanent</b>" = "Permanent","<b>Homeless</b>" = "Homeless",
#                          "<b>Institutional</b>" = "Institutional","<b>Temporary</b>" = "Temporary",
#                          "<b>Other/Unknown</b>" = "Other/Unknown"),
#             options = list(
#               dom = 't',
#               ordering = FALSE,
#               columnDefs = list(
#                 list(width = "48px", targets = 0), # Set first column width
#                 list(className = 'dt-center', targets = '_all') # Center text
#               )
#             ),
#             selection = 'none',
#             escape = FALSE,
#             style = "default",
#             rownames = FALSE) %>% 
#     # Highlight only the first column of "Subpopulation" and "Everyone Else" rows
#     formatStyle(
#       columns = 1,  # First column
#       target = "cell",
#       backgroundColor = styleEqual(
#         names(subgroup_colors), unname(subgroup_colors)
#       ),
#       borderTop = styleEqual(
#         names(subgroup_colors),
#         c("2px solid black", "1px solid black")
#       ),
#       borderLeft = styleEqual(
#         names(subgroup_colors),
#         c(rep("2px solid black", 2))
#       ),
#       borderRight = styleEqual(
#         names(subgroup_colors),
#         c(rep("2px solid black", 2))
#       ),
#       borderBottom = styleEqual(
#         names(subgroup_colors),
#         c("1px solid black", "2px solid black")
#       )
#     ) %>% 
#     # Contrast font and background colors
#     formatStyle(
#       columns = 1,
#       target = "cell",
#       color = styleEqual(
#         names(subgroup_colors), 
#         rep("black", length(subgroup_colors))
#       )
#     )
# }
# 
# get_syse_compare_subpop_flextable <- function(tab) {
#   logToConsole(session, "In get_syse_compare_subpop_flextable")
#   
#   
#   ft <- flextable(tab %>%
#                     frename("subpop_summ" = " ")) %>%
#     width(j = 1, width = 0.9) %>% # make first col narrower
#     bold(part = "header") %>%
#     align(align = "center", part = "all") %>%
#     border(border.top = fp_border(), part = "header") %>%
#     border_inner_h(border = fp_border(color = "grey", width = 0.5), part = "body")
#   
#   ## formatting function for percentages with 0 decimal places and % sign
#   #fmt_func_pct <- function(x){sprintf("%.0f%%", x*100)}
#   
#   # ft <- set_formatter(
#   #   x = ft,
#   #   Permanent = fmt_func_pct,
#   #   Homeless = fmt_func_pct,
#   #   Institutional = fmt_func_pct,
#   #   Temporary = fmt_func_pct,
#   #   `Other/Unknown` = fmt_func_pct
#   # )
#   
#   row_labels <- tab[[1]]
#   
#   # Formatting the subpopulation row labels
#   subgroup_colors <- c(
#     "Subpopulation" = get_brand_color('med_purple'),
#     "Everyone Else" = get_brand_color('med_grey2')
#   )
#   
#   ft <- ft %>%
#     # Background colors from datatable's formatStyle
#     bg(i = 1:2, j = 1, bg = subgroup_colors) %>%
#     # thick borders for the first column - adjust adjacent ones to match same total width
#     border(i = 1, j = 1, 
#            border.top = fp_border(color = "black", width = 2),
#            border.left = fp_border(color = "black", width = 2),
#            border.right = fp_border(color = "black", width = 2),
#            border.bottom = fp_border(color = "black", width = 1)) %>% 
#     border(i = 2, j = 1, 
#            border.top = fp_border(color = "black", width = 1),
#            border.left = fp_border(color = "black", width = 2),
#            border.right = fp_border(color = "black", width = 2),
#            border.bottom = fp_border(color = "black", width = 1)) %>% 
#     border(i = 3, j = 1, 
#            border.top = fp_border(color = "black", width = 1),
#            border.left = fp_border(color = "black", width = 2),
#            border.right = fp_border(color = "black", width = 2),
#            border.bottom = fp_border(color = "black", width = 2)) %>% 
#     # expand to better fit slide width
#     autofit()
#   
#   ft
#   
# }

get_syse_compare_time_flextable <- function(tab) {
  logToConsole(session, "In get_syse_compare_time_flextable")
  
  
  ft <- flextable(tab%>%
                    frename("time_summ" = " ")) %>%
    width(j = 1, width = 0.9) %>% # make first col narrower
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    border(border.top = fp_border(), part = "header") %>%
    border_inner_h(border = fp_border(color = "grey", width = 0.5), part = "body")
  
  ## formatting function for percentages with 0 decimal places and % sign
  # fmt_func_pct <- function(x) sprintf("%.0f%%", x*100)
  # 
  # ft <- set_formatter(
  #   x = ft,
  #   Permanent = fmt_func_pct,
  #   Homeless = fmt_func_pct,
  #   Institutional = fmt_func_pct,
  #   Temporary = fmt_func_pct,
  #   `Other/Unknown` = fmt_func_pct
  # )
  
  row_labels <- tab[[1]]
  
  ## formatting the time row labels
  time_colors <- c(
    "Current Year" = get_brand_color('med_purple'),
    "Previous Year" = get_brand_color('med_grey2')
  )
  
  ft <- ft %>%
    # Background colors from datatable's formatStyle
    bg(i = 1:2, j = 1, bg = time_colors) %>%
    # thick borders for the first column - adjust adjacent ones to match same total width
    border(i = 1, j = 1, 
           border.top = fp_border(color = "black", width = 2),
           border.left = fp_border(color = "black", width = 2),
           border.right = fp_border(color = "black", width = 2),
           border.bottom = fp_border(color = "black", width = 1)) %>% 
    border(i = 2, j = 1, 
           border.top = fp_border(color = "black", width = 1),
           border.left = fp_border(color = "black", width = 2),
           border.right = fp_border(color = "black", width = 2),
           border.bottom = fp_border(color = "black", width = 1)) %>% 
    border(i = 3, j = 1, 
           border.top = fp_border(color = "black", width = 1),
           border.left = fp_border(color = "black", width = 2),
           border.right = fp_border(color = "black", width = 2),
           border.bottom = fp_border(color = "black", width = 2)) %>% 
    # expand to better fit slide width
    autofit()
  
  
  ft
  
}

output$syse_compare_subpop_chart <- renderPlot({
  ## check if filters have been changed from defaults before showing 
  subpop_chart_validation(input$syse_hh_type, input$syse_level_of_detail, input$syse_project_type,
                          input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age, show=TRUE, req=FALSE)
  syse_compare_subpop_chart(subpop = input$syse_race_ethnicity)
})

output$syse_compare_subpop2_chart <- renderPlot({
  ## check if filters have been changed from defaults before showing 
  subpop2_chart_validation(show=TRUE, req=FALSE)
  syse_compare_subpop2_chart(get_syse_compare_subpop2_data(output_type = 'chart'),
                             dest_type = input$subpop2_dest_type)
})

# output$syse_compare_subpop_table <- renderDT({
#   ## check if filters have been changed from defaults before showing 
#   subpop_chart_validation(input$syse_hh_type, input$syse_level_of_detail, input$syse_project_type,
#                           input$syse_race_ethnicity,input$syse_spec_pops,input$syse_age, show = FALSE, req=TRUE)
#   get_syse_compare_subpop_table(
#     get_syse_compare_subpop_data(output_type = 'table')
#   )
# })


## function to make System Exits comparison subpopulation chart
syse_compare_time_chart <- function( isExport = FALSE){
  
  time_colors <- c(
    "Current Year" = get_brand_color('med_purple'),
    "Previous Year" = get_brand_color('med_grey2')
  )
  
  ## use adjusted locations for point placement 
  adj_x_vals <- c(0.85, 1.83, 2.87, 3.94, 5.15)
  ## long format needed for plotting points
  time_chart_df <- get_syse_compare_time_data(output_type = 'chart') %>% 
    fsubset(time_summ != "Percent Change") %>% 
    pivot_longer(cols = -1, names_to = 'dest_type', values_to = 'time_pct') %>% 
    fmutate(dest_type = factor(dest_type, levels = c("Permanent","Homeless","Institutional","Temporary","Other/Unknown")) ) %>% 
    add_column(dest_type_adj = rep(adj_x_vals, times = 2))
  
  ## wide format needed for plotting arrows between points
  time_segment_df <- time_chart_df %>% 
    pivot_wider(names_from = 'time_summ', values_from = 'time_pct')
  
  ## add x-axis labels for PPT download only
  if(isExport){
    dest_type_labels <- time_segment_df$dest_type
    bar_width <- compare_export_bar_width
  } else {
    dest_type_labels <- rep(NA,5)    
    bar_width <- compare_bar_width
  }
  
  title_start <- paste0(c("Total Current Year System Exits for ", "Total Previous Year System Exits for "),
                        syse_level_of_detail_text(), " in ",
                        str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
                        if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"))
  
  title <- paste0(title_start, 
                  c(paste0(': ', scales::label_comma()(nrow(everyone() %>% fsubset(period == 'Current Year')))),
                    paste0(': ', scales::label_comma()(nrow(everyone() %>% fsubset(period == 'Previous Year'))))),
                  collapse='\n'
  )
  
  g <- ggplot(time_chart_df, aes(x = dest_type_adj, y = time_pct )) +
    geom_bar(aes(fill = factor(time_summ, levels=c('Previous Year', 'Current Year'))), color = 'black', width = bar_width, stat = "identity", position = 'dodge') +
    scale_fill_manual(values=rev(time_colors),guide =  guide_legend(ncol = 2)) +
    scale_y_continuous(limits=c(0,NA), labels = scales::label_percent(), expand = expansion(add=0.001, mult=c(0, 0.1))) +
    scale_x_continuous(labels=dest_type_labels, breaks=adj_x_vals, limits = c(min(adj_x_vals) - 0.2, max(adj_x_vals) + 0.2)) +
    labs(x = '', y = 'Percentage of System Exits', title = title) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill=NA, colour = 'black'),
      legend.title = element_blank(),
      legend.justification = 'left',
      legend.position = 'top',
      legend.text = element_text(size = get_adj_font_size(sys_legend_text_font, isExport)),
      axis.text.y = element_text(size = sys_axis_text_font),
      axis.title.y = element_text(size = sys_axis_text_font),
      plot.title = element_text(size = sys_chart_title_font, hjust =0.5)
    )
  if(isExport){
    g + theme(
      axis.text.x = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
    )
  } else {
    g + theme(axis.text.x = element_blank())
  }
}

## function for System Exits Comparison subpopulation table (below chart)
get_syse_compare_time_table <- function(tab){
  
  time_colors <- c(
    "Current Year" = get_brand_color('med_purple'),
    "Previous Year" = get_brand_color('med_grey2')
  )
  
  datatable(tab, 
            colnames = c(' ' = 'time_summ',
                         "<b>Permanent</b>" = "Permanent","<b>Homeless</b>" = "Homeless",
                         "<b>Institutional</b>" = "Institutional","<b>Temporary</b>" = "Temporary",
                         "<b>Other/Unknown</b>" = "Other/Unknown"),
            options = list(
              dom = 't',
              ordering = FALSE,
              columnDefs = list(
                list(width = "90px", targets = 0), # Set first column width
                list(className = 'dt-center', targets = '_all') # Center text
              )
            ),
            selection = 'none',
            escape = FALSE,
            style = "default",
            rownames = FALSE) %>% 
    # Highlight only the first column of "Current Year" and "Previous Year" rows
    formatStyle(
      columns = 1,  # First column
      target = "cell",
      backgroundColor = styleEqual(
        names(time_colors), unname(time_colors)
      ),
      borderTop = styleEqual(
        names(time_colors),
        c("2px solid black", "1px solid black")
      ),
      borderLeft = styleEqual(
        names(time_colors),
        c(rep("2px solid black", 2))
      ),
      borderRight = styleEqual(
        names(time_colors),
        c(rep("2px solid black", 2))
      ),
      borderBottom = styleEqual(
        names(time_colors),
        c("1px solid black", "2px solid black")
      )
    ) %>% 
    # Contrast font and background colors
    formatStyle(
      columns = 1,
      target = "cell",
      color = styleEqual(
        names(time_colors), 
        rep("black", length(time_colors))
      )
    )
  
  
}

output$syse_compare_time_chart <- renderPlot({
  time_chart_validation(startDate = session$userData$meta_HUDCSV_Export_Start, endDate = session$userData$meta_HUDCSV_Export_End,
                        input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age,
                        show = TRUE)
  syse_compare_time_chart()
})

output$syse_compare_time_table <- renderDT({
  # time_chart_validation(startDate = session$userData$ReportStart, endDate = session$userData$ReportEnd,
  #                       input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age,
  #                       show = FALSE)
  get_syse_compare_time_table(
    get_syse_compare_time_data(output_type = 'table')
  )
})

output$syse_time_download_btn <- downloadHandler(filename = date_stamped_filename("System Exits by Year Report - "),
                                                 content = function(file) {
                                                   logToConsole(session, "System Exits by Year data download")
                                                   
                                                   sheets <- list(
                                                     "SystemExitsByYear Metadata" = sys_export_summary_initial_df(type = 'exits_time') %>%
                                                       rowbind(
                                                         sys_export_filter_selections(type = 'exits')
                                                       ) %>% 
                                                       rowbind(
                                                         data.table(Chart = c('Total Current Year System Exits', 'Total Previous Year System Exits'),
                                                                    Value = scales::label_comma()(c(nrow(everyone() %>% fsubset(period == 'Current Year')),
                                                                                                    nrow(everyone() %>% fsubset(period == 'Previous Year')))
                                                                    )
                                                         )
                                                       ) %>% 
                                                       frename("System Exits by Year" = Value),
                                                     "YearComparisonData" = syse_time_export()
                                                     
                                                   )
                                                   
                                                   write_xlsx(
                                                     sheets,     
                                                     path = file,
                                                     format_headers = FALSE,
                                                     col_names = TRUE
                                                   )       
                                                   
                                                   logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
                                                                               if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
                                                 })

# output$syse_subpop_download_btn <- downloadHandler(filename = date_stamped_filename("System Exits by Subpopulation Report - "),
#                                                    content = function(file) {
#                                                      logToConsole(session, "System Exits by Subpopulation data download")
#                                                      
#                                                      sheets <- list(
#                                                        "SystemExitsBySubpop Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
#                                                          rowbind(
#                                                            sys_export_filter_selections(type = 'exits_subpop'),
#                                                            data.table(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
#                                                                       Value = scales::label_comma()(c(nrow(all_filtered_syse_subpop()),nrow(everyone_else())))
#                                                            )
#                                                          ) %>% 
#                                                          frename("System Exits by Subpopulation" = Value),
#                                                        "SubpopulationComparisonData" = syse_subpop_export()
#                                                      )
#                                                      
#                                                      write_xlsx(
#                                                        sheets,     
#                                                        path = file,
#                                                        format_headers = FALSE,
#                                                        col_names = TRUE
#                                                      )   
#                                                      
#                                                      logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
#                                                                                  if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
#                                                    })

output$syse_subpop2_download_btn <- downloadHandler(filename = date_stamped_filename("System Exits by Subpopulation Report - "),
                                                   content = function(file) {
                                                     logToConsole(session, "System Exits by Subpopulation data download")
                                                     
                                                     sheets <- list(
                                                       "SystemExitsBySubpop Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
                                                         rowbind(
                                                           sys_export_filter_selections(type = 'exits_subpop'),
                                                           data.table(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
                                                                      Value = scales::label_comma()(c(nrow(subpop2()),nrow(everyone_else2())))
                                                           )
                                                         ) %>% 
                                                         frename("System Exits by Subpopulation" = Value),
                                                       "SubpopulationComparisonSummary" = syse_subpop2_export_summary(),
                                                       "SubpopulationExitDetail" = syse_subpop2_export_detail()
                                                     )
                                                     
                                                     write_xlsx(
                                                       sheets,     
                                                       path = file,
                                                       format_headers = FALSE,
                                                       col_names = TRUE
                                                     )   
                                                     
                                                     logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
                                                                                 if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
                                                   })

## hide demographic filters when on PHD subtab
observeEvent(input$syse_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  if(input$syse_tabbox %in% c('<h4>Exits to PH Demographics</h4>','<h4>Exits by Subpopulation 2</h4>')){
    shinyjs::hide('syse_spec_pops')
    shinyjs::hide('syse_age')
    shinyjs::hide('syse_race_ethnicity')
  } else {
    shinyjs::show('syse_spec_pops')
    shinyjs::show('syse_age')
    shinyjs::show('syse_race_ethnicity')
  }
  
})

observeEvent(input$syse_types_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox, " - ", input$syse_types_subtabs,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$syse_time_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox, " - ", input$syse_time_subtabs,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$syse_subpop_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox, " - ", input$syse_subpop_subtabs,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(input$syse_phd_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox, " - ", input$syse_phd_subtabs,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Client-level flags, filtered ----------------------------------------------------
syse_client_categories_filtered <- reactive({
  
  logToConsole(session, "In syse_client_categories_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  req(nrow(session$userData$client_categories) > 0)
  
  session$userData$client_categories[
    AgeCategory %in% input$syse_age &
      (if(input$syse_race_ethnicity == "All") rep(TRUE, .N) else get(input$syse_race_ethnicity) == 1) & 
      (
        input$syse_spec_pops == "None" |
          (input$syse_spec_pops == "Veteran" &
             VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
          (input$syse_spec_pops == "NonVeteran" &
             VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17"))
      )
  ]
})

# Create passes-enrollment-filter flag to exclude enrollments from heatmap -------
enrollments_filtered_syse <- reactive({
  logToConsole(session, "in enrollments_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  en_unfilt <-  join(
    session$userData$enrollment_categories,
    session$userData$client_categories %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  )
  
  en_filt <- en_unfilt %>%
    fmutate(
      passes_enrollment_filters =
        # Household type filter
        (input$syse_hh_type == "All" |
           (input$syse_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (input$syse_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (input$syse_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (input$syse_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           input$syse_hh_type == HouseholdType
        ) &
        # Level of detail filter
        (input$syse_level_of_detail == "All" |
           (input$syse_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (input$syse_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        (input$syse_project_type == "All" |
           (input$syse_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
           (input$syse_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
           (input$syse_project_type == "SO" & ProjectType == out_project_type) |
           (input$syse_project_type == "AllNonRes" & ProjectType %in% non_res_project_types)
        )
    ) %>%
    fselect(-VeteranStatus)
  
  en_filt %>% 
    fsubset(passes_enrollment_filters)
  
})

enrollments_filtered_syse_prev <- reactive({
  logToConsole(session, "in enrollments_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  en_unfilt <-  join(
    session$userData$enrollment_categories_prev,
    session$userData$client_categories %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  )
  
  en_filt <- en_unfilt %>%
    fmutate(
      passes_enrollment_filters =
        # Household type filter
        (input$syse_hh_type == "All" |
           (input$syse_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (input$syse_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (input$syse_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (input$syse_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           input$syse_hh_type == HouseholdType
        ) &
        # Level of detail filter
        (input$syse_level_of_detail == "All" |
           (input$syse_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (input$syse_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        (input$syse_project_type == "All" |
           (input$syse_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
           (input$syse_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
           (input$syse_project_type == "SO" & ProjectType == out_project_type) |
           (input$syse_project_type == "AllNonRes" & ProjectType %in% non_res_project_types)
        )
    ) %>%
    fselect(-VeteranStatus)
  
  en_filt %>% 
    fsubset(passes_enrollment_filters)
  
})


all_filtered_syse <- reactive({
  logToConsole(session, "in all_filtered_syse")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  tmp <- join( 
    enrollments_filtered_syse(),
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  ) 
  
  period_data <- tmp %>% 
    expand_by_periods(chart_type = 'exits_types') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive'))
  period_data
  
})

all_filtered_syse_time <- reactive({
  logToConsole(session, "in all_filtered_syse_time")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode)) 
  
  tmp <- join( 
    enrollments_filtered_syse(),
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  ) 
  
  tmp_prev <- join( 
    enrollments_filtered_syse_prev(),
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  ) 
  
  period_data_cur <- tmp %>% 
    expand_by_periods(chart_type = 'exits_types') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive')) %>% 
    fmutate(period = "Current Year")
  
  period_data_prev <- tmp_prev %>% 
    expand_by_periods(chart_type = 'exits_types',
                      reportStart = session$userData$ReportStart %m-% years(1), 
                      reportEnd = session$userData$ReportEnd %m-% years(1)) %>% 
    get_active_info(tmp_prev, lh_info_df = session$userData$lh_info_prev,
                    reportStart = session$userData$ReportStart %m-% years(1), 
                    reportEnd = session$userData$ReportEnd %m-% years(1)) %>% 
    get_inflows_and_outflows(chart_type = 'exits',
                             reportStart = session$userData$ReportStart %m-% years(1), 
                             reportEnd = session$userData$ReportEnd %m-% years(1)) %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive')) %>% 
    fmutate(period = "Previous Year")
  
  period_data <- rowbind(period_data_cur, period_data_prev)
  
  period_data
  
})

all_filtered_syse_demog <- reactive({
  logToConsole(session, "in all_filtered_syse")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  tmp <-  enrollments_filtered_syse()
  
  period_data <- tmp %>% 
    expand_by_periods(chart_type = 'exits_demog') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive'))
  
  join( 
    period_data,
    session$userData$client_categories,
    on = "PersonalID",
    how = "inner"
  ) 
})

all_filtered_syse_subpop <- reactive({
  logToConsole(session, "in all_filtered_syse_subpop")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  
  client <- session$userData$client_categories
  
  
  tmp <- session$userData$enrollment_categories
  
  period_data <- tmp %>% 
    expand_by_periods(chart_type = 'exits_types') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive'))
  join( 
    period_data,
    client,
    on = "PersonalID",
    how = "inner"
  ) %>% 
    join(tmp %>% fselect(PersonalID, EnrollmentID, HouseholdType)) %>% 
    fmutate(HouseholdType = fct_collapse(HouseholdType, !!!hh_types_in_exports))
  
})

compute_subpop_and_everyone_else <- function(input_df){
  
  subpop_w_client_filters <- input_df
  
  if(input$syse_hh_type != 'All'){
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_hh_type = HouseholdType %in% input$syse_hh_type)
  } else {
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_hh_type = TRUE)
  }
  
  if('Age' %in% syse_subpop2_selections()){
    req(input$syse_subpop2_age)
    
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_age_filter = AgeCategory %in% input$syse_subpop2_age)
    
  } else {
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_age_filter = TRUE)
  }
  
  if('All Races/Ethnicities' %in% syse_subpop2_selections()){
    req(input$syse_subpop2_race_ethnicity1)
    
    subpop_race_eth <- subpop_w_client_filters[ (if(input$syse_subpop2_race_ethnicity1 == "All") rep(TRUE, .N) else get(input$syse_subpop2_race_ethnicity1) == 1)]
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_race_eth_filter = PersonalID %in% subpop_race_eth$PersonalID)
  } else if('Grouped Races/Ethnicities' %in% syse_subpop2_selections()){
    req(input$syse_subpop2_race_ethnicity2)
    subpop_race_eth <- subpop_w_client_filters[ (if(input$syse_subpop2_race_ethnicity2 == "All") rep(TRUE, .N) else get(input$syse_subpop2_race_ethnicity2) == 1)]
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_race_eth_filter = PersonalID %in% subpop_race_eth$PersonalID)
  } else {
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_race_eth_filter = TRUE)
  }
  
  if('Veteran Status (Adult Only)' %in% syse_subpop2_selections()){
    req(input$syse_subpop2_spec_pops)
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_vet_filter =input$syse_subpop2_spec_pops == "None" |
                (input$syse_subpop2_spec_pops == "Veteran" &
                   VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
                (input$syse_subpop2_spec_pops == "NonVeteran" &
                   VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17")))
  } else {
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_vet_filter = TRUE)
  }
  
  
  
  subpop_out <- subpop_w_client_filters[meets_hh_type & meets_age_filter & meets_race_eth_filter & meets_vet_filter]
  
  rest_of_pop <- subpop_w_client_filters[!meets_hh_type | !meets_age_filter | !meets_race_eth_filter | !meets_vet_filter] %>% 
    fmutate(meets_hh_type = factor(meets_hh_type, levels=c(T,F)),
            meets_age_filter = factor(meets_age_filter, levels=c(T,F)),
            meets_race_eth_filter = factor(meets_race_eth_filter, levels=c(T,F)),
            meets_vet_filter = factor(meets_vet_filter, levels=c(T,F)))
  return(list(subpop = subpop_out, everyone_else = rest_of_pop))
  
}

comps <- reactive({
  compute_subpop_and_everyone_else(all_filtered_syse_subpop())
})
subpop2 <- reactive({comps()$subpop})

output$syse_time_download_btn_ppt <- downloadHandler(filename = function(){
  paste("System Exits by Year_", Sys.Date(), ".pptx", sep = "")
},
content = function(file) {
  logToConsole(session, "In syse_time_download_btn_ppt")
  
  sys_perf_ppt_export(file = file, 
                      type = 'exits_comparison',
                      title_slide_title = "System Exits by Year",
                      summary_items = list(
                        "Summary" = sys_export_summary_initial_df(type = 'exits_time') %>%
                          rowbind(
                            sys_export_filter_selections(type = 'exits')
                          ) %>% 
                          rowbind(
                            data.table(Chart = c('Total Current Year System Exits', 'Total Previous Year System Exits'),
                                       Value = scales::label_comma()(c(nrow(everyone() %>% fsubset(period == 'Current Year')),
                                                                       nrow(everyone() %>% fsubset(period == 'Previous Year')))
                                       )
                            )
                          ) 
                      ),
                      plots = list(
                        "System Exits by Year - Chart" = syse_compare_time_chart(isExport = TRUE),
                        "System Exits by Year - Table" = get_syse_compare_time_flextable(
                          get_syse_compare_time_data(output_type = 'table')
                        )
                      ),
                      summary_font_size = 19,
                      startDate = session$userData$ReportStart, 
                      endDate = session$userData$ReportEnd, 
                      sourceID = session$userData$Export$SourceID,
                      in_demo_mode = input$in_demo_mode
  )
  
})

# output$syse_subpop_download_btn_ppt <- downloadHandler(filename = function(){
#   paste("System Exits by Subpopulation_", Sys.Date(), ".pptx", sep = "")
# },
# content = function(file) {
#   logToConsole(session, "In syse_subpop_download_btn_ppt")
#   
#   sys_perf_ppt_export(file = file, 
#                       type = 'exits_comparison',
#                       title_slide_title = "System Exits by Subpopulation",
#                       summary_items = list(
#                         "Summary" = sys_export_summary_initial_df(type = 'exits') %>%
#                           rowbind(
#                             sys_export_filter_selections(type = 'exits_subpop'),
#                             data.table(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
#                                        Value = scales::label_comma()(c(nrow(all_filtered_syse_subpop()),nrow(everyone_else())))
#                             )
#                           ) 
#                       ),
#                       plots = list(
#                         "System Exits by Subpopulation - Chart" =  syse_compare_subpop_chart(isExport = TRUE),
#                         "System Exits by Subpopulation - Table" = get_syse_compare_subpop_flextable(
#                           get_syse_compare_subpop_data(output_type = 'table')
#                         )
#                       ),
#                       summary_font_size = 19,
#                       startDate = session$userData$ReportStart, 
#                       endDate = session$userData$ReportEnd, 
#                       sourceID = session$userData$Export$SourceID,
#                       in_demo_mode = input$in_demo_mode
#   )
#   
# })

output$syse_subpop2_download_btn_ppt <- downloadHandler(filename = function(){
  paste("System Exits by Subpopulation_", Sys.Date(), ".pptx", sep = "")
},
content = function(file) {
    sys_perf_ppt_export(file = file,
                        type = 'exits_comparison',
                        title_slide_title = "System Exits by Subpopulation",
                        summary_items = list(
                          "Summary" = sys_export_summary_initial_df(type = 'exits') %>%
                            rowbind(
                              sys_export_filter_selections(type = 'exits_subpop'),
                              data.table(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
                                         Value = scales::label_comma()(c(nrow(subpop2()),nrow(everyone_else2())))
                              )
                            )
                        ),
                        plots = list(
                          "System Exits by Subpopulation - Permanent" =  syse_compare_subpop2_chart(dest_type = 'Permanent',isExport = TRUE),
                          "System Exits by Subpopulation - Homeless" =  syse_compare_subpop2_chart(dest_type = 'Homeless', isExport = TRUE),
                          "System Exits by Subpopulation - Institutional" =  syse_compare_subpop2_chart(dest_type = 'Institutional',isExport = TRUE),
                          "System Exits by Subpopulation - Temporary" =  syse_compare_subpop2_chart(dest_type = 'Temporary',isExport = TRUE),
                          "System Exits by Subpopulation - Other/Unknown" =  syse_compare_subpop2_chart(dest_type = 'Other/Unknown',isExport = TRUE)
                        ),
                        summary_font_size = 19,
                        startDate = session$userData$ReportStart,
                        endDate = session$userData$ReportEnd,
                        sourceID = session$userData$Export$SourceID,
                        in_demo_mode = input$in_demo_mode
    )
  
})

# System Exits Exits to PH Demographics (PHD) -----------------------
sys_phd_plot_df <- reactiveVal()

display_syse_counts <- function(){
  c(
    paste0(
      "Total ", 
      syse_level_of_detail_text(),
      " with System Exits",
      if_else(
        input$syse_hh_type == "All",
        "",
        paste0(" in ",
               str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
               " Households")
      )
    ),
    paste0(
      "Total ", 
      syse_level_of_detail_text(),
      " with PH System Exits",
      if_else(
        input$syse_hh_type == "All",
        "",
        paste0(" in ",
               str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
               " Households")
      )
    )
  )
}

full_unit_of_analysis_display_syse <- reactive({
  display_syse_counts()
})

syse_total_count_display <- function(total_count, total_ph_count) {
  
  return(paste0(
    str_wrap(
      paste0(
        display_syse_counts(),
        ": ",
        scales::comma(c(total_count, total_ph_count))
      ),
      width = 40
    ),collapse='',
    "\n")
  )
}

output$syse_phd_chart_1d <- renderPlot({
  
  req(session$userData$valid_file() == 1 &
        !is.null(input$syse_phd_selections) &
        length(input$syse_phd_selections) == 1)
  
  sys_phd_plot_1var(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
  
}, height = 700, width = 500,
alt = "A crosstab data table of the demographic make-up of the homeless system.")

output$syse_phd_chart_2d <- renderCachedPlot({
  
  req(session$userData$valid_file() == 1 &
        !is.null(input$syse_phd_selections) &
        length(input$syse_phd_selections) == 2)
  
  sys_phd_plot_2vars(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
  
}, cacheKeyExpr = {
  list(
    input$syse_phd_selections,
    input$syse_hh_type,
    input$syse_level_of_detail,
    input$syse_project_type,
    input$syse_methodology_type
  )
}, alt = "A crosstab data table of the demographic make-up of the homeless system.")

observeEvent(input$syse_phd_selections, {
  # they can select up to 2
  #disable all unchecked boxes if they've already selected 2
  shinyjs::runjs(str_glue("
    var numSelected = {length(input$syse_phd_selections)};
    $('input[name=syse_phd_selections]:not(\":checked\")')
      .attr('disabled', numSelected == 2);

    var reSelected = \"{
      \"All Races/Ethnicities\" %in% input$syse_phd_selections |
      \"Grouped Races/Ethnicities\" %in% input$syse_phd_selections
    }\";
    
    if(numSelected == 1)
      $('input[name=syse_phd_selections][value*=\"Races/Ethnicities\"]:not(\":checked\")')
        .attr('disabled', reSelected == 'TRUE');
    
  "))
}, ignoreNULL = FALSE)

output$syse_phd_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Exit Demographics_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    sys_perf_ppt_export(
      file = file,
      type = 'exits',
      title_slide_title = "System Exits Permanent Housing (PH) Demographics",
      summary_items = sys_export_summary_initial_df(type = 'exits') %>%
        fsubset(Chart != "Start Date" & Chart != "End Date") %>% 
        rowbind(sys_phd_selections_info()),
      plots = setNames(
        list(
          if (length(input$syse_phd_selections) == 1) {
            sys_phd_plot_1var(subtab = 'phd', 
                              methodology_type = input$syse_methodology_type, 
                              selection = input$syse_phd_selections, 
                              isExport = TRUE)
          } else {
            sys_phd_plot_2vars(subtab = 'phd', 
                               methodology_type = input$syse_methodology_type, 
                               selection = input$syse_phd_selections, 
                               isExport = TRUE)
          }
        ),
        ifelse(length(input$syse_phd_selections) == 1, 
               paste0(
                 "System Exits PH Demographics: ",
                 input$syse_phd_selections[1]
               ),
               paste0(
                 "System Exits PH Demographics: ",
                 input$syse_phd_selections[1],
                 " by ",
                 input$syse_phd_selections[2]
               )
        )
      ),
      summary_font_size = 28,
      startDate = session$userData$ReportStart, 
      endDate = session$userData$ReportEnd, 
      sourceID = session$userData$Export$SourceID,
      in_demo_mode = input$in_demo_mode
    )
  }
)

output$syse_phd_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Exit Demographics Report - "),
  content = function(file) {
    sys_heatmap_xl_export(file, 
                          type = 'exits',
                          methodology_type = input$syse_methodology_type,
                          selections = input$syse_phd_selections,
                          plot_df = sys_phd_plot_df,
                          in_demo_mode = input$in_demo_mode)
  }
)

observeEvent(input$syse_methodology_type, {
  
  updatePickerInput(
    session, 
    "syse_race_ethnicity", 
    choices = sys_race_ethnicity_cats(input$syse_methodology_type)
  )
  
},
ignoreInit = TRUE)

toggle_sys_components(prefix='syse', FALSE, init=TRUE) # initially hide them
