
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
                methodology_type = input$syse_methodology_type,
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
        low = "#D2E3D9",
        high = "#084954",
        breaks = seq(0, 1, by = 0.05),
        na.value = ifelse(
          is.na(plot_df_joined$wasRedacted) | !plot_df_joined$wasRedacted,
          "white",
          "#D2E3D9"
        )
      ) +
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
      low = "#D2E3D9",
      high = "#084954",
      breaks = seq(0,1,by=0.05),
      na.value = ifelse(
        is.na(plot_df_joined$wasRedacted) | !plot_df_joined$wasRedacted,
        "white",
        "#D2E3D9"
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
        low = "#ede7e3",
        high = "#73655e",
        breaks = seq(0,1,by=0.05),
        na.value = ifelse(h_total_joined$wasRedacted, "#ede7e3", 'white')
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
        low = "#ede7e3",
        high = "#73655e",
        breaks = seq(0,1,by=0.05),
        na.value = ifelse(v_total_joined$wasRedacted, "#ede7e3", 'white')
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
    "Permanent" = "#16697A",
    "Homeless" = "#C2462E",
    "Institutional" = "#CCC7C4",
    "Temporary" = "#CCC7C4",
    "Other/Unknown" = "#CCC7C4"
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
         "ExitsByType Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
           rowbind(
             sys_export_filter_selections(type = 'exits'),
              data.table(Chart = 'Total System Exits', Value = scales::label_comma()(nrow(tree_exits_data())))              
           ) %>% 
           frename('System Exits by Type' = Value),
         
         "SystemExitData" = tree_exits_data() %>% 
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

subpop_chart_validation <- function(raceeth, vetstatus, age, show = TRUE, req = FALSE) {
  logToConsole(session, "In subpop_chart_validation")
 
  cond <- raceeth != "All" | vetstatus != "None" | length(age) != length(sys_age_cats)
  
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
 
time_chart_validation <- function(startDate, endDate, raceeth, vetstatus, age, show = TRUE) {
  logToConsole(session, "In time_chart_validation")
  
  cond <- interval(startDate, endDate) > years(2)
  #cond <- raceeth != "All" | vetstatus != "None" | length(age) != length(sys_age_cats)
  
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

syse_subpop_export <- reactive({
  ## compute subcategories of destination types for data export - not shown in chart or table
  pct_subpop_sub <- tree_exits_data() %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_subpop = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_subpop = count_subpop /sum(count_subpop, na.rm=T))
  
  pct_comparison_sub <- everyone_else() %>% 
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
      fmutate(pct_diff =  scales::percent(pct_subpop - pct_comparison,accuracy = 0.1, scale = 100),
             total_count = count_subpop + count_comparison,
             pct_comparison = scales::percent(pct_comparison, accuracy = 0.1,scale=100),
             pct_subpop = scales::percent(pct_subpop, accuracy = 0.1,scale=100)) %>% 
      fselect(`Destination Type`, `Destination Type Detail`, 'Subpopulation %' = pct_subpop, 'Everyone Else %' = pct_comparison, 
             'Percent Difference' = pct_diff, 'Subpopulation Count' = count_subpop, 'Everyone Else Count' = count_comparison, 'Total Count' = total_count)
})

everyone_else <- reactive({
  all_unfiltered_syse() %>% 
    roworder(PersonalID, EntryDate, ExitAdjust) %>%
    fgroup_by(PersonalID) %>%
    fmutate(
      # Days_to_lookahead is simpler because if they have ANY enrollment <= 14 days ahead
      # then it was clearly not a system exit
      days_to_lookahead = L(EntryDate, n=-1) - ExitAdjust
    ) %>%
    fungroup() %>% 
    fsubset(days_to_lookahead > 14 ) %>% 
    ## drop rows that are in the filtered version - (everyone minus subpop)
    fsubset(!(EnrollmentID %in% all_filtered_syse()$EnrollmentID)) %>% 
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

get_syse_compare_subpop_data <- reactive({
  
  validate(need(nrow(all_filtered_syse()) > 0, no_data_msg))
  validate(need(nrow(all_filtered_syse()) > 10, suppression_msg))
  
  
  pct_subpop <- tree_exits_data() %>% fsummarize(
    'Permanent' = fmean(`Destination Type` == 'Permanent'),
    'Homeless'= fmean(`Destination Type` == 'Homeless'),
    'Institutional' = fmean(`Destination Type` == 'Institutional'),
    'Temporary' = fmean(`Destination Type` == 'Temporary'),
    'Other/Unknown' = fmean(`Destination Type` == 'Other/Unknown')
  )
  
  pct_everyone_else <- everyone_else() %>% fsummarize(
    'Permanent' = fmean(`Destination Type` == 'Permanent'),
    'Homeless' = fmean(`Destination Type` == 'Homeless'),
    'Institutional' = fmean(`Destination Type` == 'Institutional'),
    'Temporary' = fmean(`Destination Type` == 'Temporary'),
    'Other/Unknown' = fmean(`Destination Type` == 'Other/Unknown')
  )
  
  data.table(
    subpop_summ = c("Subpopulation","Everyone Else","Percent Difference"),
  round(
    rowbind(
      pct_subpop,
      pct_everyone_else,
      pct_subpop - pct_everyone_else
    ),
  2)
  )

})

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
      fmutate(pct_change = scales::percent(pct_cur_year - pct_prev_year, accuracy=0.1, scale=100),
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

get_syse_compare_time_data <- reactive({
  
  validate(need(nrow(all_filtered_syse_time()) > 0, no_data_msg))
  validate(need(nrow(all_filtered_syse_time()) > 10, suppression_msg))
  
  prev_year <- everyone() %>% 
    fsubset(period == 'Previous Year')

  current_year <- everyone() %>% 
    fsubset(period == 'Current Year')
  
  pct_prev_year <- prev_year %>% fsummarize(
    'Permanent' = fmean(`Destination Type` == 'Permanent'),
    'Homeless'= fmean(`Destination Type` == 'Homeless'),
    'Institutional' = fmean(`Destination Type` == 'Institutional'),
    'Temporary' = fmean(`Destination Type` == 'Temporary'),
    'Other/Unknown' = fmean(`Destination Type` == 'Other/Unknown')
  )
  
  pct_current_year <- current_year %>% fsummarize(
    'Permanent' = fmean(`Destination Type` == 'Permanent'),
    'Homeless' = fmean(`Destination Type` == 'Homeless'),
    'Institutional' = fmean(`Destination Type` == 'Institutional'),
    'Temporary' = fmean(`Destination Type` == 'Temporary'),
    'Other/Unknown' = fmean(`Destination Type` == 'Other/Unknown')
  )
  
  data.table(
    time_summ = c("Current Year","Previous Year","Percent Change"),
    round(rowbind(
      pct_current_year,
      pct_prev_year,
      pct_current_year - pct_prev_year
    ),2)
  )
  
})

## function to make System Exits comparison subpopulation chart
syse_compare_subpop_chart <- function(subpop, isExport = FALSE){
  
  subgroup_colors <- c(
   "Subpopulation" = "#D5BFE6",
   "Everyone Else" = "#9E958F"
  )
  
  ## use adjusted locations for point placement 
  adj_x_vals <- c(0.85, 1.83, 2.87, 3.94, 5.15)
  ## long format needed for plotting points
  subpop_chart_df <- get_syse_compare_subpop_data() %>% 
    fsubset(subpop_summ != "Percent Difference") %>% 
    pivot_longer(cols = -1, names_to = 'dest_type', values_to = 'subpop_pct') %>% 
    fmutate(dest_type = factor(dest_type, levels = c("Permanent","Homeless","Institutional","Temporary","Other/Unknown")) ) %>% 
    add_column(dest_type_adj = rep(adj_x_vals, times = 2))
  
  ## wide format needed for plotting arrows between points
  subpop_segment_df <- subpop_chart_df %>% 
    pivot_wider(names_from = 'subpop_summ', values_from = 'subpop_pct') 
  
  ## add x-axis labels for PPT download only
  if(isExport){
    dest_type_labels <- subpop_segment_df$dest_type
    bar_width <- compare_export_bar_width
    
  } else {
    dest_type_labels <- rep(NA,5)    
    bar_width <- compare_bar_width
    
  }
  g <- ggplot(subpop_chart_df, aes(x = dest_type_adj, y = subpop_pct)) +
    geom_bar(aes(fill = subpop_summ), color = 'black', width = bar_width, stat='identity', position='dodge') +
    scale_fill_manual(values=rev(subgroup_colors), guide = guide_legend(ncol = 2)) +
    scale_y_continuous(limits=c(0,NA), labels = scales::label_percent(), expand = expansion(add=0.001, mult=c(0, 0.1))) +
    scale_x_continuous(labels=dest_type_labels, breaks=adj_x_vals, limits = c(min(adj_x_vals) - 0.2, max(adj_x_vals) + 0.2)) +
    labs(x = '', y = 'Percentage of System Exits') +
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
          axis.title.y = element_text(size = sys_axis_text_font)
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
get_syse_compare_subpop_table <- function(tab){
  
  subgroup_colors <- c(
    "Subpopulation" = "#D5BFE6",
    "Everyone Else" = "#9E958F"
  )
  
  datatable(tab, 
            colnames = c(' ' = 'subpop_summ',
                         "<b>Permanent</b>" = "Permanent","<b>Homeless</b>" = "Homeless",
                         "<b>Institutional</b>" = "Institutional","<b>Temporary</b>" = "Temporary",
                        "<b>Other/Unknown</b>" = "Other/Unknown"),
            options = list(
    dom = 't',
    ordering = FALSE,
    columnDefs = list(
      list(width = "48px", targets = 0), # Set first column width
      list(className = 'dt-center', targets = '_all') # Center text
    )
  ),
  escape = FALSE,
  style = "default",
  rownames = FALSE) %>% DT::formatPercentage(
    columns = -1 
  ) %>% 
    # Highlight only the first column of "Subpopulation" and "Everyone Else" rows
    formatStyle(
      columns = 1,  # First column
      target = "cell",
      backgroundColor = styleEqual(
        names(subgroup_colors), unname(subgroup_colors)
      ),
      borderTop = styleEqual(
        names(subgroup_colors),
        c("2px solid black", "1px solid black")
      ),
      borderLeft = styleEqual(
        names(subgroup_colors),
        c(rep("2px solid black", 2))
      ),
      borderRight = styleEqual(
        names(subgroup_colors),
        c(rep("2px solid black", 2))
      ),
      borderBottom = styleEqual(
        names(subgroup_colors),
        c("1px solid black", "2px solid black")
      )
    ) %>% 
    # Contrast font and background colors
    formatStyle(
      columns = 1,
      target = "cell",
      color = styleEqual(
        names(subgroup_colors), 
        rep("black", length(subgroup_colors))
      )
    )
}

get_syse_compare_subpop_flextable <- function(tab) {
  logToConsole(session, "In get_syse_compare_subpop_flextable")
 
  
  ft <- flextable(tab %>%
                    frename("subpop_summ" = " ")) %>%
    width(j = 1, width = 0.9) %>% # make first col narrower
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    border(border.top = fp_border(), part = "header") %>%
    border_inner_h(border = fp_border(color = "grey", width = 0.5), part = "body")
  
  ## formatting function for percentages with 0 decimal places and % sign
  fmt_func_pct <- function(x){sprintf("%.0f%%", x*100)}
  
  ft <- set_formatter(
    x = ft,
    Permanent = fmt_func_pct,
    Homeless = fmt_func_pct,
    Institutional = fmt_func_pct,
    Temporary = fmt_func_pct,
    `Other/Unknown` = fmt_func_pct
  )
  
  row_labels <- tab[[1]]
  
  # Formatting the subpopulation row labels
  subgroup_colors <- c(
    "Subpopulation" = "#D5BFE6",
    "Everyone Else" = "#9E958F"
  )
  
  ft <- ft %>%
    # Background colors from datatable's formatStyle
    bg(i = 1:2, j = 1, bg = subgroup_colors) %>%
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
  fmt_func_pct <- function(x) sprintf("%.0f%%", x*100)
  
  ft <- set_formatter(
    x = ft,
    Permanent = fmt_func_pct,
    Homeless = fmt_func_pct,
    Institutional = fmt_func_pct,
    Temporary = fmt_func_pct,
    `Other/Unknown` = fmt_func_pct
  )
  
  row_labels <- tab[[1]]
  
  ## formatting the time row labels
  time_colors <- c(
    "Current Year" = "#D5BFE6",
    "Previous Year" = "#9E958F"
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
  subpop_chart_validation(input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age, show=TRUE, req=FALSE)
  syse_compare_subpop_chart(subpop = input$syse_race_ethnicity)
})

output$syse_compare_subpop_table <- renderDT({
  ## check if filters have been changed from defaults before showing 
  subpop_chart_validation(input$syse_race_ethnicity,input$syse_spec_pops,input$syse_age, show = FALSE, req=TRUE)
  get_syse_compare_subpop_table(
    get_syse_compare_subpop_data()
  )
})


## function to make System Exits comparison subpopulation chart
syse_compare_time_chart <- function( isExport = FALSE){
  
  time_colors <- c(
    "Current Year" = "#D5BFE6",
    "Previous Year" = "#9E958F"
  )
  
  ## use adjusted locations for point placement 
  adj_x_vals <- c(0.85, 1.83, 2.87, 3.94, 5.15)
  ## long format needed for plotting points
  time_chart_df <- get_syse_compare_time_data() %>% 
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
  g <- ggplot(time_chart_df, aes(x = dest_type_adj, y = time_pct )) +
    geom_bar(aes(fill = factor(time_summ, levels=c('Previous Year', 'Current Year'))), color = 'black', width = bar_width, stat = "identity", position = 'dodge') +
    scale_fill_manual(values=rev(time_colors),guide =  guide_legend(ncol = 2)) +
    scale_y_continuous(limits=c(0,NA), labels = scales::label_percent(), expand = expansion(add=0.001, mult=c(0, 0.1))) +
    scale_x_continuous(labels=dest_type_labels, breaks=adj_x_vals, limits = c(min(adj_x_vals) - 0.2, max(adj_x_vals) + 0.2)) +
    labs(x = '', y = 'Percentage of System Exits') +
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
      axis.title.y = element_text(size = sys_axis_text_font)
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
    "Current Year" = "#D5BFE6",
    "Previous Year" = "#9E958F"
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
            escape = FALSE,
            style = "default",
            rownames = FALSE) %>% DT::formatPercentage(
              columns = -1 
            ) %>% 
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
    get_syse_compare_time_data()
  )
})

output$syse_time_download_btn <- downloadHandler(filename = date_stamped_filename("System Exits by Year Report - "),
                                                    content = function(file) {
  logToConsole(session, "System Exits by Year data download")
  
    sheets <- list(
      "SystemExitsTimeMetadata" = sys_export_summary_initial_df(type = 'exits_time') %>%
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
      "Time" = syse_time_export()
      
    )
  
  write_xlsx(
    sheets,     
    path = file,
    format_headers = FALSE,
    col_names = TRUE
  )        
})

output$syse_subpop_download_btn <- downloadHandler(filename = date_stamped_filename("System Exits by Subpopulation Report - "),
                                                    content = function(file) {
      logToConsole(session, "System Exits by Subpopulation data download")
  
      sheets <- list(
        "System Exits by Subpopulation" = sys_export_summary_initial_df(type = 'exits') %>%
          rowbind(
            sys_export_filter_selections(type = 'exits_subpop'),
            data.table(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
                       Value = scales::label_comma()(c(nrow(tree_exits_data()),nrow(everyone_else())))
            )
          ) %>% 
          frename("System Exits by Subpopulation" = Value),
        "Subpopulation" = syse_subpop_export()
      )
                                                    
    write_xlsx(
        sheets,     
        path = file,
        format_headers = FALSE,
        col_names = TRUE
      )        
})

## hide demographic filters when on PHD subtab
observeEvent(input$syse_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  if(input$syse_tabbox == '<h4>Permanent Housing Demographics</h4>'){
    shinyjs::hide('syse_spec_pops')
    shinyjs::hide('syse_age')
    shinyjs::hide('syse_race_ethnicity')
  } else {
    shinyjs::show('syse_spec_pops')
    shinyjs::show('syse_age')
    shinyjs::show('syse_race_ethnicity')
  }
 
})

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

all_unfiltered_syse <- reactiveVal(NULL)

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
  all_unfiltered_syse(en_unfilt)
  
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
  #   roworder(PersonalID, EntryDate, ExitAdjust) %>%
  #   fgroup_by(PersonalID) %>%
  #   fmutate(
  #     # Days_to_lookahead is simpler because if they have ANY enrollment <= 14 days ahead
  #     # then it was clearly not a system exit
  #     days_to_lookahead = L(EntryDate, n=-1) - ExitAdjust
  #   ) %>%
  #   flast() %>% 
  #   fungroup() %>% 
  #   fsubset(is.na(days_to_lookahead ) | days_to_lookahead > 14) %>% 
   
  
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
    get_inflows_and_outflows(chart_type = 'exits')
  
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
  
  period_data <- tmp %>% 
    expand_by_periods(chart_type = 'exits_time') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits')
  
  period_data
 
})

all_filtered_syse_demog <- reactive({
  logToConsole(session, "in all_filtered_syse")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  tmp <-  enrollments_filtered_syse()

  period_data <- tmp %>% 
    expand_by_periods(chart_type = 'exits_demog') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits')
  
  join( 
    period_data,
    session$userData$client_categories,
    on = "PersonalID",
    how = "inner"
  ) 
})

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
                            get_syse_compare_time_data()
                          )
                        ),
                        summary_font_size = 19,
                        startDate = session$userData$ReportStart, 
                        endDate = session$userData$ReportEnd, 
                        sourceID = session$userData$Export$SourceID,
                        in_demo_mode = input$in_demo_mode
    )
 
})

output$syse_subpop_download_btn_ppt <- downloadHandler(filename = function(){
  paste("System Exits by Subpopulation_", Sys.Date(), ".pptx", sep = "")
},
  content = function(file) {
  logToConsole(session, "In syse_subpop_download_btn_ppt")
  
    sys_perf_ppt_export(file = file, 
                        type = 'exits_comparison',
                        title_slide_title = "System Exits by Subpopulation",
                        summary_items = list(
                          "Summary" = sys_export_summary_initial_df(type = 'exits') %>%
                            rowbind(
                              sys_export_filter_selections(type = 'exits_subpop'),
                              data.table(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
                                         Value = scales::label_comma()(c(nrow(tree_exits_data()),nrow(everyone_else())))
                              )
                            ) 
                        ),
                        plots = list(
                          "System Exits by Subpopulation - Chart" =  syse_compare_subpop_chart(isExport = TRUE),
                          "System Exits by Subpopulation - Table" = get_syse_compare_subpop_flextable(
                            get_syse_compare_subpop_data()
                          )
                        ),
                        summary_font_size = 19,
                        startDate = session$userData$ReportStart, 
                        endDate = session$userData$ReportEnd, 
                        sourceID = session$userData$Export$SourceID,
                        in_demo_mode = input$in_demo_mode
    )
  
})


# System Exits Permanent Housing Demographics (PHD) -----------------------
sys_phd_plot_df <- reactiveVal()

# output$syse_phd_chart <- renderPlot({
#   # req(
#   #   !is.null(input$syse_phd_selections) &
#   #     session$userData$valid_file() == 1 &
#   #     between(length(input$syse_phd_selections), 1, 2)
#   # )
# 
#    if( is.null(input$syse_phd_selections) |
#         session$userData$valid_file() != 1 |
#         !between(length(input$syse_phd_selections), 1, 2)){
#      validate("Please select one or more options to display the demographic chart.")
#    }
# 
#   if(length(input$syse_phd_selections) == 1) {
#     sys_phd_plot_1var(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
#   } else if(length(input$syse_phd_selections) == 2){
#     sys_phd_plot_2vars(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
#   }
# }, height = function() {
#   ifelse(!is.null(input$syse_phd_selections), 700, 100)
# }, width = function() {
#   input$syse_phd_subtabs
#   input$syse_tabbox
#   input$pageid
# 
#   if (num_selections() %in% c(0,1) |
#       isTRUE(getOption("shiny.testmode"))) {
#     500
#   } else {
#     "auto"
#   }
# 
# },
# alt = "A crosstab data table of the demographic make-up of the homeless system.")

full_unit_of_analysis_display_syse <- reactive({
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
})

syse_total_count_display <- function(total_count, total_ph_count) {
 
  return(paste0(
    str_wrap(
      paste0(
        full_unit_of_analysis_display_syse(),
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
