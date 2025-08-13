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
    selections <- input$system_composition_selections
    v1 <- gsub("Races/Ethnicities", "Race", selections[1])
    v1 <- gsub("Veteran Status \\(Adult Only\\)", "Veteran Status", v1)
   
    # multiple selections
    # reshape so the values of v1 are the column headers and v2 are the "row headers"
    # though technically just a column
    if(length(selections) > 1) {
      v2 <- gsub("Races/Ethnicities", "Race", selections[2])
      v2 <- gsub("Veteran Status \\(Adult Only\\)", "Veteran Status", v2)
      
      # make sure R/E is the rows, not the columns
      if (v1 %in% c("All Race", "Grouped Race")) {
        selections <- c(selections[2], selections[1])
      }
      
      num_df <- sys_comp_plot_df() %>%
        pivot_wider(
          names_from = selections[1],
          values_from = n,
          values_fill = list(n = 0)
        )

      # Create x.y% version
      pct_df <- num_df %>%
        ftransformv(vars = num_vars(., return="names"),  FUN = function(x) {
          (x / fsum(x) * 100) %>% 
            replace_na(0) %>%
            round(1) %>%
            paste0("%")
        })
      
      # create totals, but only for Method1
      if(input$syso_methodology_type == 1) { 
        # create total row
        total_num_row <- num_df %>%
          summarise(!!selections[1] := "Total",
                    across(where(is.numeric), sum, na.rm = TRUE)) %>%
          rename(!!selections[2] := !!selections[1])
        
        total_n <- sum(sys_comp_plot_df()$n, na.rm = TRUE)
        
        total_pct_row <- total_num_row %>% 
          mutate(
            across(where(is.numeric), ~ (. / total_n * 100) %>%
                     replace_na(0) %>%
                     round(1) %>%
                     paste0("%")))
        
        # Add Total Row and create a total column
        num_df <- num_df %>%
          bind_rows(total_num_row) %>%
          mutate(Total = rowSums(select(., where(is.numeric)), na.rm = TRUE))
        
        pct_df <- pct_df %>% 
          bind_rows(total_pct_row) %>%
          mutate(
            Total =  paste0(
              round(
                replace_na(num_df$Total / total_n * 100, 0),
                1
              ),
              "%"
            )
          )
      }
    } 
    # single selection
    else {
      num_df <- sys_comp_plot_df()
      
      pct_df <- num_df %>%
        mutate(across(where(is.numeric), ~ (. / sum(., na.rm = TRUE) * 100) %>%
                        round(1) %>%
                        paste0("%")))  %>% 
        rename("pct" = n)
      
      if(input$syso_methodology_type == 1) { 
        pct_df <- pct_df %>%
          bind_rows(
            setNames(
              data.frame("Total", "100%"), 
              c(selections, "pct")
            )
          )
        num_df <- num_df %>%
          bind_rows(summarise(., !!sym(selections) := "Total", n = sum(n, na.rm = TRUE)))
      }
    }
    
    if (length(selections) > 1) {
      num_tab_name <- glue("{v1} By {v2} #")
      pct_tab_name <- glue("{v1} By {v2} %")
    } else {
      num_tab_name <- glue("{v1} #")
      pct_tab_name <- glue("{v1} %")
    }
    
    write_xlsx(
      setNames(
        list(sys_comp_selections_summary(), num_df, pct_df),
        c("System Demographics Metadata", num_tab_name, pct_tab_name)
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    exportTestValues(sys_comp_df = get_people_universe_filtered())
    exportTestValues(sys_comp_report_num_df = num_df)
    exportTestValues(sys_comp_report_pct_df = pct_df)
    logMetadata(session, paste0("Downloaded System Overview Tabular Data: ", input$syso_tabbox,
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
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
    sys_heatmap_plot_1var(subtab = 'comp', 
                          methodology_type = input$syso_methodology_type, 
                          selection = input$system_composition_selections, 
                          isExport = FALSE)
  } else {
    sys_heatmap_plot_2vars(subtab = 'comp', 
                           methodology_type = input$syso_methodology_type, 
                           selections = input$system_composition_selections, 
                           isExport = FALSE)
    
  }
}, height = function() {
  ifelse(!is.null(input$system_composition_selections), 700, 100)
}, width = function() {
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
            sys_heatmap_plot_1var(subtab = 'comp', 
                                  methodology_type = input$syso_methodology_type, 
                                  selection = input$system_composition_selections, 
                                  isExport = TRUE)
          } else {
            sys_heatmap_plot_2vars(subtab = 'comp', 
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
  join(
    get_period_specific_enrollment_categories()[period == "Full", .(PersonalID)],
    session$userData$client_categories,
    on = "PersonalID"
  ) %>%
    funique()
})
