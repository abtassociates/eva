
sys_phd_plot_df <- reactiveVal()
sys_phd_export <- reactiveVal()


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
    roworderv(selection)
  names(export_df)[1] <- export_label1
  
  sys_phd_export(export_df)
  
  plot_df_joined <- plot_df_joined %>% fselect(-n_orig, -frac_export)
  plot_df <- plot_df_supp %>% fselect(-n_orig)
  
  
  return(
    ggplot(plot_df_joined %>% fmutate(frac= ifelse(is.na(frac) & wasRedacted, 0, frac)), aes("", .data[[selection]])) +
      # main data into cells for each cross-combination
      geom_tile(
        color = '#f0f0f0',
        lwd = 0.5,
        linetype = 1,
        aes(fill = frac)
      ) +
      scale_fill_gradient2(
        low = get_brand_color('very_light_green'),
        mid=get_brand_color('light_green'),
        high = get_brand_color('dark_green'),
        midpoint=0,
        na.value = 'white'
      ) +
      # scale_fill_gradient(
      #   low = get_brand_color('light_green'),
      #   high = get_brand_color('dark_green'),
      #   breaks = seq(0,1,by=0.05),
      #   na.value = 'white'
      # ) + # na.value makes 0s invisible
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
    roworderv(selections[2])
  names(export_df)[1:2] <- c(export_label1, export_label2)
  
  sys_phd_export(export_df)
  
  if(methodology_type == 1){
    h_total_joined <- join(h_total,h_total_phd %>% frename(num = N), how = 'left', on=selections) %>% 
      fmutate(frac = ifelse(N == 0 | is.na(N), NA, num / N))
    
    v_total_joined <- join(v_total,v_total_phd %>% frename(num = N), how = 'left', on=c(selections[1],selections[2])) %>% 
      fmutate(frac = ifelse(N == 0 | is.na(N), NA, num / N))
    
  }
  
  plot_df_joined <- plot_df_joined %>% fselect(-n_orig, -frac_export)
  plot_df <- plot_df_supp %>% fselect(-n_orig)
  browser()
  g <- ggplot(plot_df_joined %>% fmutate(frac= ifelse(is.na(frac) & wasRedacted, 0, frac)), 
              aes(.data[[selections[1]]], .data[[selections[2]]])) +
    # main data into cells for each cross-combination
    geom_tile(
      color = '#f0f0f0',
      lwd = 0.5,
      linetype = 1,
      aes(fill = frac)
    ) +
    scale_fill_gradient2(
      low = get_brand_color('very_light_green'),
      mid=get_brand_color('light_green'),
      high = get_brand_color('dark_green'),
      midpoint=0,breaks=seq(0,1,by=0.05),
      na.value = 'white'
    ) +
    # scale_fill_gradient(
    #   low = get_brand_color('light_green'),
    #   high = get_brand_color('dark_green'),
    #   breaks = seq(0,1,by=0.05),
    #   na.value = 'white'
    #   # na.value = ifelse(
    #   #   is.na(plot_df_joined$wasRedacted) | !plot_df_joined$wasRedacted,
    #   #   "white",
    #   #   get_brand_color('light_green')
    #   # )
    # ) + # na.value makes 0s invisible
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
        na.value = 'white'#ifelse(h_total_joined$wasRedacted, get_brand_color('light_grey'), 'white')
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
        na.value = 'white'#ifelse(v_total_joined$wasRedacted, get_brand_color('light_grey'), 'white')
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
  #browser()
  
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

output$syse_phd_chart_1d <- renderPlot({
  
  req(session$userData$valid_file() == 1 &
        !is.null(input$syse_phd_selections) &
        length(input$syse_phd_selections) == 1)
  
  sys_phd_plot_1var(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
  
}, height = 700, width = 500,
alt = "A crosstab data table of the demographic make-up of the homeless system.")

output$syse_phd_chart_2d <- renderCachedPlot({
  #browser()
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