
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

sys_phd_plot <- function(subtab = 'phd', methodology_type, selections, isExport = FALSE) {
  # 1. Prep Total Exits Universe
  raw_exits <- all_filtered_syse_demog()
  total_prep <- prepare_crosstab_data(raw_exits, selections, methodology_type, subtab = subtab)
  
  # 2. Prep Permanent Housing (PH) Exits Universe
  ph_exits <- raw_exits %>% fsubset(Destination %in% perm_livingsituation)
  ph_prep    <- prepare_crosstab_data(ph_exits, selections, methodology_type, subtab = subtab)
  
  # 3. Suppress Total Counts & Join with PH Counts
  plot_df_supp <- total_prep$plot_df %>% 
    suppress_values("n", keep_orig_var = TRUE)
  for (s in selections) {
    plot_df_supp <- plot_df_supp %>% suppress_next_val_if_one_suppressed_in_group(s, "n")
  }
  
  plot_df_joined <- join(plot_df_supp, ph_prep$plot_df %>% frename(num = n), how = 'left', on = selections) %>% 
    fmutate(
      frac = ifelse(n == 0 | is.na(n), NA, num / n),
      frac_export = ifelse(n_orig == 0 | is.na(n_orig), 0, num / n_orig)
    )
  
  # 4. Save export df
  syse_phd_plot_df(total_prep$plot_df)
  syse_phd_export(create_phd_export_df(plot_df_joined, plot_df_supp, selections))
  
  # 5. Render Heatmap
  build_demographic_heatmap(
    plot_df = plot_df_joined,
    selections = selections,
    metric_type = "ratio",
    palette_type = "green",
    methodology_type = methodology_type,
    margin_totals = if (length(selections) == 2 && methodology_type == 1) list(h_total = h_total_joined, v_total = v_total_joined) else NULL,
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