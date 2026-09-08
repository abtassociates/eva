
syse_phd_plot_df <- reactiveVal()
syse_phd_export <- reactiveVal()


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

# Helper to construct the PhD Excel export data frame (handles both 1D and 2D)
create_phd_export_df <- function(plot_df_joined, plot_df_supp, selections) {
  export_df <- plot_df_joined %>%
    join(
      plot_df_supp %>%
        fmutate(`Suppression Flag` = ifelse(!is.na(wasRedacted) & wasRedacted, "Yes", "No")) %>%
        fselect(-n, -wasRedacted, -n_orig),
      how = "left"
    ) %>%
    fmutate(
      frac_export = scales::percent(frac_export, accuracy = 0.1)
    ) %>%
    fselect(c(
      unlist(selections),
      "n_orig",
      "num",
      "frac_export",
      "Suppression Flag")
    ) %>%
    frename(
      'Total Count' = n_orig, 
      'Permanent Count' = num, 
      'Percent in Permanent' = frac_export
    ) %>%
    roworderv(tail(selections, 1))
  
  section_selections <- if (length(selections) == 2) rev(selections) else selections
  
  names(export_df)[seq_along(section_selections)] <- paste0(
    section_selections,
    " (Demographic Section ", seq_along(section_selections), ")"
  )
  
  return(export_df)
}

syse_phd_plot <- function(methodology_type, selections, isExport = FALSE) {
  logToConsole(session, paste0("In syse_phd_plot, where methodology_type = ", methodology_type, " and selections = ", selections, collapse = ", "))
  
  # 1. Prep Total Exits Universe
  raw_exits <- all_filtered_syse_demog()
  total_prep <- prepare_crosstab_data(raw_exits, selections, methodology_type, subtab = "exits")
  
  # 2. Prep Permanent Housing (PH) Exits Universe
  ph_exits <- raw_exits %>% fsubset(Destination %in% perm_livingsituation)
  ph_prep  <- prepare_crosstab_data(ph_exits, selections, methodology_type, subtab = "exits")
  
  # 3. Suppress Total Counts & Join with PH Counts
  plot_df_supp <- total_prep$plot_df %>% 
    suppress_values("n", keep_orig_var = TRUE)
  
  for (s in selections) {
    plot_df_supp <- plot_df_supp %>% suppress_next_val_if_one_suppressed_in_group(s, "n")
  }
  
  plot_df_joined <- join(
    plot_df_supp, 
    ph_prep$plot_df %>% frename(num = n), 
    how = 'left', 
    on = selections
  ) %>% 
    fmutate(
      frac = ifelse(n == 0 | is.na(n), NA, num / n),
      frac_export = ifelse(n_orig == 0 | is.na(n_orig), 0, num / n_orig)
    )
  
  # 4. Save export df
  syse_phd_plot_df(total_prep$plot_df)
  syse_phd_export(create_phd_export_df(plot_df_joined, plot_df_supp, selections))
  
  # 5. Calc margin totals (needs Phd dataset, vs. Syso Comp, which doesn't)
  margin_totals <- NULL
  if (length(selections) == 2 && methodology_type == 1) {
    margin_totals <- calc_margin_totals(total_prep$plot_df, selections[1], selections[2]) # uses helper from earlier
    
    h_total_phd <- ph_prep$plot_df %>%
      fgroup_by(selections[2]) %>%
      fsummarise(num = if (all(is.na(n))) NA_real_ else fsum(n)) %>%
      mutate(!!sym(selections[1]) := 'Total')
    
    h_total_joined <- join(margin_totals$h_total, h_total_phd, how = 'left', on = selections) %>%
      fmutate(
        frac = ifelse(N == 0 | is.na(N), NA_real_, num / N),
        val_col = frac
      )
    
    v_total_phd <- ph_prep$plot_df %>%
      fgroup_by(selections[1]) %>%
      fsummarise(num = if (all(is.na(n))) NA_real_ else fsum(n)) %>%
      mutate(!!sym(selections[2]) := 'Total')
    
    v_total_joined <- join(margin_totals$v_total, v_total_phd, how = 'left', on = selections) %>%
      fmutate(
        frac = ifelse(N == 0 | is.na(N), NA_real_, num / N),
        val_col = frac
      )
    
    margin_totals$h_total <- h_total_joined
    margin_totals$v_total <- v_total_joined
  }
  
  # 5. Render Heatmap
  build_demographic_heatmap(
    plot_df = plot_df_joined,
    selections = selections,
    metric_type = "ratio",
    palette_type = "green",
    methodology_type = methodology_type,
    margin_totals = margin_totals,
    total_count_display = syse_total_count_display(nrow(total_prep$clean_df), nrow(ph_prep$clean_df)),
    isExport = isExport
  )
}

output$syse_phd_chart_1d <- renderPlot({
  
  req(session$userData$valid_file() == 1 &
        !is.null(input$syse_phd_selections) &
        length(input$syse_phd_selections) == 1)
  
  validate(
    need(
      fnrow(session$userData$enrollment_categories) > 0,
      no_valid_data_msg
    )
  )
  
  syse_phd_plot(input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
  
}, height = 700, width = 500,
alt = "A crosstab data table of the demographic make-up of the homeless system.")

output$syse_phd_chart_2d <- renderCachedPlot({
  
  req(session$userData$valid_file() == 1 &
        !is.null(input$syse_phd_selections) &
        length(input$syse_phd_selections) == 2)
  
  validate(
    need(
      fnrow(session$userData$enrollment_categories) > 0,
      no_valid_data_msg
    )
  )
  
  syse_phd_plot(input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
  
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
  limit_checkbox_selections("syse_phd_selections", input$syse_phd_selection)
}, ignoreNULL = FALSE)