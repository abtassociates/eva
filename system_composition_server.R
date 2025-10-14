sys_comp_plot_df <- reactiveVal()

sys_comp_selections_info <- reactive({
    sys_perf_selection_info(type ='overview',selection = input$system_composition_selections)
    
})

sys_comp_selections_summary <- function() {
  return(
    sys_export_summary_initial_df(type = 'overview') %>%
      bind_rows(sys_comp_selections_info()) %>%
      rename("System Demographics" = Value)
  )
}


sys_comp_plot_1var <- function(subtab = 'comp', methodology_type, selection, isExport = FALSE) {
  var_cols <- get_var_cols(methodology_type)
  
  
  comp_df <- get_people_universe_filtered() %>%
    remove_non_applicables(selection = selection) %>%
    select(PersonalID, unname(var_cols[[selection]]))
  
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
  
  # hide download buttons if not enough data
  toggle_download_buttons(subtab,plot_df)
  
  if(subtab == 'comp'){
    type <- 'overview'
  } else if (subtab == 'phd'){
    type <- 'exits'
  }
  
  selection_cats1 <- get_selection_cats(selection, type = type)
  if(is.null(names(selection_cats1))){
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  plot_df[selection] <- factor(
    plot_df[[selection]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels,
    ordered = TRUE)
  
  if(subtab == 'comp'){
    sys_comp_plot_df(plot_df)
  } else if(subtab == 'phd'){
    sys_phd_plot_df(plot_df)
  }
  
  
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(selection, "n")
  
  return(
    ggplot(plot_df, aes("", .data[[selection]])) +
      # main data into cells for each cross-combination
      geom_tile(
        color = '#f0f0f0',
        lwd = 0.5,
        linetype = 1,
        aes(fill = n)
      ) +
      scale_fill_gradient(
        low = "#D2E3D9",
        high = "#084954",
        na.value = ifelse(
          is.na(plot_df$wasRedacted) | !plot_df$wasRedacted,
          "white",
          "#D2E3D9"
        )
      ) +
      # set text color to be 508 compliant contrasting
      geom_text(
        aes(label = ifelse(wasRedacted, "***", scales::comma(n))),
        size = sys_chart_text_font,
        color = ifelse(
          plot_df$n > mean(plot_df$n, na.rm = TRUE) & !plot_df$wasRedacted,
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
      ggtitle(sys_total_count_display(
        nrow(comp_df)
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

sys_comp_plot_2vars <- function(subtab = 'comp', methodology_type, selections, isExport = FALSE) {
  # race/ethnicity, if selected, should always be on the row
  var_cols <- get_var_cols(methodology_type)
  
  if (selections[1] %in% c("All Races/Ethnicities", "Grouped Races/Ethnicities")) {
    selections <- c(selections[2], selections[1])
  }
  
  # get dataset underlying the freqs we will produce below
  comp_df <- get_people_universe_filtered() %>%
    remove_non_applicables(selection = selections) %>%
    select(
      PersonalID, 
      unname(var_cols[[selections[1]]]), 
      unname(var_cols[[selections[2]]])
    ) %>%
    funique()
  
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
  
  toggle_download_buttons(subtab, plot_df)
  
  if(subtab == 'comp'){
    type <- 'overview'
  } else if (subtab == 'phd'){
    type <- 'exits'
  }
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
  }
  
  # save before supressing the values
  # this will be used for the download/export
  if(subtab == 'comp'){
    sys_comp_plot_df(plot_df)
  } else {
    sys_phd_plot_df(plot_df)
  }
  
  # Suppress values <= 10
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[1], "n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[2], "n")
  
  
  g <- ggplot(plot_df, aes(.data[[selections[1]]], .data[[selections[2]]])) +
    # main data into cells for each cross-combination
    geom_tile(
      color = '#f0f0f0',
      lwd = 0.5,
      linetype = 1,
      aes(fill = n)
    ) +
    scale_fill_gradient(
      low = "#D2E3D9",
      high = "#084954",
      na.value = ifelse(
        is.na(plot_df$wasRedacted) | !plot_df$wasRedacted,
        "white",
        "#D2E3D9"
      )
    ) + # na.value makes 0s invisible
    # set text color to be 508 compliant contrasting
    geom_text(
      # aes(label = paste0(scales::comma(n), "\n", "(",scales::percent(pct, accuracy = 0.1),")")),
      aes(label = ifelse(wasRedacted, "***", scales::comma(n))),
      size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction, 1),
      color = ifelse(
        plot_df$n > mean(plot_df$n, na.rm = TRUE) & !plot_df$wasRedacted,
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
        data = h_total,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = N)
      ) +
      
      scale_fill_gradient(
        low = "#ede7e3",
        high = "#73655e",
        na.value = ifelse(h_total$wasRedacted, "#ede7e3", 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***",
                           scales::comma(N))),
        size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction, 1),
        color = ifelse(
          h_total$N > mean(h_total$N, na.rm = TRUE) & !h_total$wasRedacted,
          'white',
          'black'
        ),
        data = h_total
      ) +
      
      # column totals
      ggnewscale::new_scale("fill") +
      geom_tile(
        data = v_total,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = N)
      ) +
      scale_fill_gradient(
        low = "#ede7e3",
        high = "#73655e",
        na.value = ifelse(v_total$wasRedacted, "#ede7e3", 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***",
                           scales::comma(N))),
        size = sys_chart_text_font * ifelse(isExport, 0.7, 1),
        color = ifelse(
          v_total$N > mean(v_total$N, na.rm = TRUE) & !v_total$wasRedacted,
          'white',
          'black'
        ),
        data = v_total
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
    
    ggtitle(sys_total_count_display(
      nrow(comp_df)
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
      axis.text = element_text(size = sys_comp_axis_text_font * ifelse(windowSize()[1] < 1300, 0.8, 1) * ifelse(isExport, 0.6, 1))
    )
}


output$sys_comp_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Demographics Report - "),
  content = function(file) {
   sys_heatmap_xl_export(file, 
                         type = 'overview',
                         methodology_type = input$syso_methodology_type,
                         selections = input$system_composition_selections,
                         plot_df = sys_comp_plot_df,
                         in_demo_mode = input$in_demo_mode)
  }
)

observeEvent(input$system_composition_selections, {
  # they can select up to 2
  #disable all unchecked boxes if they've already selected 2
  shinyjs::runjs(str_glue("
    var numSelected = {length(input$system_composition_selections)};
    $('input[name=system_composition_selections]:not(\":checked\")')
      .attr('disabled', numSelected == 2);

    var reSelected = \"{
      \"All Races/Ethnicities\" %in% input$system_composition_selections |
      \"Grouped Races/Ethnicities\" %in% input$system_composition_selections
    }\";
    
    if(numSelected == 1)
      $('input[name=system_composition_selections][value*=\"Races/Ethnicities\"]:not(\":checked\")')
        .attr('disabled', reSelected == 'TRUE');
  "))
}, ignoreNULL = FALSE)


output$sys_comp_summary_selections <- renderUI({
  req(!is.null(input$system_composition_selections) & session$userData$valid_file() == 1)
  sys_detailBox( selection = input$system_composition_selections,
                 all_filters = FALSE,
                 detail_type = 'comp',
                 methodology_type = input$syso_methodology_type,
                 cur_project_types = input$syso_project_type,
                 startDate = session$userData$ReportStart,
                 endDate = session$userData$ReportEnd)
})

output$sys_comp_summary_ui_chart <- renderPlot({
  req(
    !is.null(input$system_composition_selections) &
    session$userData$valid_file() == 1 &
    between(length(input$system_composition_selections), 1, 2)
  )

  if(length(input$system_composition_selections) == 1) {
    sys_comp_plot_1var(subtab = 'comp', 
                          methodology_type = input$syso_methodology_type, 
                          selection = input$system_composition_selections, 
                          isExport = FALSE)
  } else {
    sys_comp_plot_2vars(subtab = 'comp', 
                           methodology_type = input$syso_methodology_type, 
                           selections = input$system_composition_selections, 
                           isExport = FALSE)
    
  }
}, height = function() {
  ifelse(!is.null(input$system_composition_selections), 700, 100)
}, width = function() {
  input$sys_comp_subtabs
  input$syso_tabbox
  input$pageid
  if (length(input$system_composition_selections) == 1 |
      isTRUE(getOption("shiny.testmode"))) {
    500
  } else {
    "auto"
  }
}, alt = "A crosstab data table of the demographic make-up of the homeless system.")


output$sys_comp_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Demographics_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    sys_perf_ppt_export(
      file = file,
      type = 'overview',
      title_slide_title = "System Demographics",
      summary_items = sys_export_summary_initial_df(type = 'overview') %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(sys_comp_selections_info()),
      plots = setNames(
        list(
          if (length(input$system_composition_selections) == 1) {
            sys_comp_plot_1var(subtab = 'comp', 
                                  methodology_type = input$syso_methodology_type, 
                                  selection = input$system_composition_selections, 
                                  isExport = TRUE)
          } else {
            sys_comp_plot_2vars(subtab = 'comp', 
                                  methodology_type = input$syso_methodology_type, 
                                  selection = input$system_composition_selections, 
                                  isExport = TRUE)
          }
        ),
        paste0(
          "System Demographics: ",
          input$system_composition_selections[1],
          " by ",
          input$system_composition_selections[2]
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

# System Composition/Demographics data for chart
get_people_universe_filtered <- reactive({
  full_data <- period_specific_data()[["Full"]]
  req(nrow(full_data) > 0)
  
  join(
    period_specific_data()[["Full"]] %>% fsubset(InflowTypeDetail !=" Excluded", PersonalID),
    session$userData$client_categories,
    on = "PersonalID"
  ) %>%
    funique()
})
