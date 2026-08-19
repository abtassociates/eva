syso_comp_plot_df <- reactiveVal()

syso_comp_selections_info <- reactive({
  sys_demographics_selection_info(type ='overview',selection = input$syso_composition_selections)
  
})

# System Composition/Demographics data for chart
syso_get_people_universe_filtered <- reactive({
  full_data <- syso_enrollments_filtered() %>%
    join(session$userData$lh_info %>% fselect(EnrollmentID, lh_date), on="EnrollmentID", multiple = TRUE) %>%
    fsubset(ExitAdjust >= session$userData$ReportStart & (
      ProjectType %in% c(ph_project_types, lh_project_types_nonbn) | # defintiionally active the whole time
        EntryDate + days_lh_valid >= session$userData$ReportStart | # (active) entry in period
        (!Destination %in% other_livingsituation & !is.na(Destination)) |  # active exit
        lh_date >= session$userData$ReportStart | lh_date + days_lh_valid >= session$userData$ReportStart # active LH date in period
    )) %>%
    fselect(PersonalID) %>%
    funique()
  
  req(nrow(full_data) > 0)
  
  join(
    full_data,
    session$userData$client_categories,
    on = "PersonalID"
  )
})



syso_comp_plot <- function(methodology_type, selections, isExport = FALSE) {
  # 1. Run Data Preparation Pipeline
  prep <- prepare_crosstab_data(syso_get_people_universe_filtered(), selections, methodology_type, subtab = "comp")
  
  plot_df <- prep$plot_df
  
  # 2. Save unsuppressed df for downloads
  syso_comp_plot_df(plot_df)
  
  # 3. Calculate Method 1 Row/Col Totals (2D only)
  margin_totals <- NULL
  if (length(selections) == 2 && methodology_type == 1) {
    margin_totals <- calc_margin_totals(plot_df, selections[1], selections[2]) # uses helper from earlier
  }
  
  # 4. Apply Suppression
  plot_df_supp <- plot_df %>% suppress_values("n")
  for (s in selections) {
    plot_df_supp <- plot_df_supp %>% suppress_next_val_if_one_suppressed_in_group(s, "n")
  }
  
  # 5. Render Unified Heatmap
  build_demographic_heatmap(
    plot_df = plot_df_supp,
    selections = selections,
    metric_type = "count",
    palette_type = "purple",
    methodology_type = methodology_type,
    margin_totals = margin_totals,
    total_count_display = sys_total_count_display(nrow(prep$clean_df)),
    isExport = isExport
  )
}

syso_comp_selections_summary <- function() {
  return(
    sys_export_summary_initial_df(type = 'overview') %>%
      bind_rows(syso_comp_selections_info()) %>%
      rename("System Demographics" = Value)
  )
}

output$syso_comp_summary_selections <- renderUI({
  req(!is.null(input$syso_composition_selections) & session$userData$valid_file() == 1)
  sys_detailBox( selection = input$syso_composition_selections,
                 detail_type = 'comp',
                 methodology_type = input$syso_methodology_type,
                 cur_project_types = input$syso_project_type,
                 startDate = session$userData$ReportStart,
                 endDate = session$userData$ReportEnd)
})


output$syso_comp_summary_ui_chart <- renderPlot({
  req(
    !is.null(input$syso_composition_selections) &
      session$userData$valid_file() == 1 &
      between(length(input$syso_composition_selections), 1, 2)
  )
  
  validate(
    need(
      fnrow(session$userData$enrollment_categories) > 0,
      no_valid_data_msg
    )
  )
  
  syso_comp_plot(
    methodology_type = input$syso_methodology_type, 
    selections = input$syso_composition_selections, 
    isExport = FALSE
  )
  
}, height = function() {
  ifelse(!is.null(input$syso_composition_selections), 700, 100)
}, width = function() {
  input$syso_comp_subtabs
  input$syso_tabbox
  input$pageid
  if (length(input$syso_composition_selections) == 1 |
      isTRUE(getOption("shiny.testmode"))) {
    500
  } else {
    "auto"
  }
}, alt = "A crosstab data table of the demographic make-up of the homeless system.")


observeEvent(input$syso_composition_selections, {
  limit_checkbox_selections("syso_composition_selections", input$syso_composition_selections)
}, ignoreNULL = FALSE)