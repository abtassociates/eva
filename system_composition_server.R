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
