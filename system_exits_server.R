
# Set race/ethnicity filter options based on methodology type selection
# Set special populations options based on level of detail selection
syse_race_ethnicity_cats <- function(methodology = 1){
  if(methodology == 1) syse_race_ethnicity_method1 
  else syse_race_ethnicity_method2
}

#### DISPLAY FILTER SELECTIONS ###
syse_detailBox <- reactive({
  list(
    br(),
    strong("Date Range: "),
    
    format(session$userData$ReportStart, "%m-%d-%Y"), " to ", format(session$userData$ReportEnd, "%m-%d-%Y"), br(),
    
    if (input$syse_project_type != "All")
      chart_selection_detail_line("Project Type ", syse_project_types, str_remove(input$syse_project_type, "- ")),
    
    #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
    HTML(glue(
      "<b>Methodology Type:</b> {str_sub(getNameByValue(syse_methodology_types, input$syse_methodology_type), start = 1, end = 8)} <br>"
    )),
    
    if (length(input$syse_age) != length(syse_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syse_age, collapse = ', ')} <br>"
      )),
    
    if (input$syse_race_ethnicity != "All")
      chart_selection_detail_line("Race/Ethnicity", syse_race_ethnicity_cats(input$syse_methodology_type), input$syse_race_ethnicity),
    
    if(getNameByValue(syse_spec_pops_people, input$syse_spec_pops) != "All Statuses")
      HTML(glue(
        "<b>Veteran Status:</b> {paste(getNameByValue(syse_spec_pops_people, input$syse_spec_pops), '(Adult Only)')} <br>"
      ))
    
  )
})

syse_export_summary_initial_df <- function() {
  
  logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  return(data.frame(
    Chart = c(
      "Start Date",
      "End Date",
      "Methodology Type",
      "Household Type",
      "Level of Detail",
      "Project Type Group"
    ),
    Value = c(
      strftime(session$userData$ReportStart, "%m/%d/%y"),
      strftime(session$userData$ReportEnd, "%m/%d/%y"),
      getNameByValue(syse_methodology_types, input$syse_methodology_type),
      getNameByValue(syse_hh_types, input$syse_hh_type),
      getNameByValue(syse_level_of_detail, input$syse_level_of_detail),
      getNameByValue(syse_project_types, input$syse_project_type)
    )
  ))
}

syse_export_filter_selections <- function() {
  return(tibble(
    Chart = c(
      "Age",
      "Veteran Status",
      "Race/Ethnicity"
    ),
    Value = c(
      if(identical(syse_age_cats, input$syse_age)) {"All Ages"} else {paste(input$syse_age, collapse=", ")},
      getNameByValue(syse_spec_pops_people, input$syse_spec_pops),
      getNameByValue(syse_race_ethnicity_cats(input$syse_methodology_type), input$syse_race_ethnicity)
    )
  ))
}

output$syse_compare_subpop_filter_selections <- 
  output$syse_compare_time_filter_selections <- 
  output$syse_types_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  syse_detailBox() 
})

level_of_detail_text_syse <- reactive({
  case_when(
    input$syse_level_of_detail == "All" ~ "People",
    input$syse_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(syse_level_of_detail, input$syse_level_of_detail)
  )
})

output$syse_types_ui_chart <- renderPlot({
  
  syse_types_chart("Destination Type", input$syse_dest_type_filter)
})

syse_types_chart <- function(varname, status){
  
  tree_colors <- c(
    "Permanent" = "#16697A",
    "Homeless" = "#C2462E",
    "Institutional" = "#C1DDD7",
    "Temporary" = "#71B4CB",
    "Other/Unknown" = "#73655E"
  )
  
  
  
  ggplot(test_exits_summ, aes(area = Count, fill = `Destination Type`, 
                              label = str_c(`Destination Type`, ':\n', scales::label_comma()(Count),
                                            ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
                                                                                     ))) +
    labs(title = paste0(scales::label_comma()(sum(test_exits_data$Count)), " System Exits for ", 
                        level_of_detail_text_syse(), " in ", 
                        str_remove(getNameByValue(syse_hh_types, input$syse_hh_type), "- "), 
                        if_else(getNameByValue(syse_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"))
         ) +
    geom_treemap(start = "left", show.legend = FALSE) +
    geom_treemap_text(aes(color = text_color), fontface = 'bold',start = "left", place = "center", grow = FALSE) +
    scale_color_identity() +
    scale_fill_manual(values = tree_colors) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
}


# PowerPoint Export -------------------------------------------------------
sys_exits_ppt_export <- function(file,
                                    title_slide_title,
                                    summary_items,
                                    plots,
                                    summary_font_size) {
  logMetadata(session, paste0("Downloaded System Exits Powerpoint: ", title_slide_title,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  loc_title <- ph_location_type(type = "title")
  loc_footer <- ph_location_type(type = "ftr")
  loc_dt <- ph_location_type(type = "dt")
  loc_slidenum <- ph_location_type(type = "sldNum")
  loc_body <- ph_location_type(type = "body")
  loc_subtitle <- ph_location_type(type = "subTitle")
  loc_ctrtitle <- ph_location_type(type = "ctrTitle")
  
  fp_normal <- fp_text(font.size = summary_font_size)
  fp_title <- fp_text(font.size = ppt_chart_title_font_size)
  fp_bold <- update(fp_normal, bold = TRUE)
  fp_red <- update(fp_normal, color = "red")
  
  ppt <- read_pptx(here("system_pptx_template.pptx"))
  
  report_period <- paste0("Report Period: ", 
                          format(session$userData$ReportStart, "%m/%d/%Y"),
                          " - ",
                          format(session$userData$ReportEnd, "%m/%d/%Y")
  )
  
  add_footer <- function(.ppt) {
    return(
      .ppt %>%
        ph_with(value = paste0("CoC Code: ", session$userData$Export$SourceID), location = loc_footer) %>%
        ph_with(value = report_period, location = loc_dt) %>%
        ph_with(
          value = paste0(
            "Export Generated: ",
            format(Sys.Date()),
            "\n",
            "https://hmis.abtsites.com/eva/"
          ),
          location = loc_slidenum
        )
    )
  }
  
  # title Slide
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme") %>%
    ph_with(value = title_slide_title, location = loc_ctrtitle) %>%
    ph_with(value = "Eva Image Export", location = loc_subtitle) %>%
    add_footer()
  
  # Summary
  s_items <- do.call(block_list, lapply(1:nrow(summary_items), function(i) {
    fpar(
      ftext(paste0(summary_items$Chart[i], ": ", summary_items$Value[i]), fp_normal)
    )
  }))
  
  ppt <- add_slide(ppt, layout = "Title and Content") %>%
    ph_with(value = "Summary", location = loc_title) %>%
    ph_with(
      value = s_items,
      level_list = c(rep(1L, length(s_items))),
      location = loc_body
    ) %>% 
    add_footer()
  
  # Chart
  for(plot_slide_title in names(plots)) {
    p <- plots[[plot_slide_title]]
    if(!is.null(p)) {
      ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext(plot_slide_title, fp_title)), location = loc_title) %>%
        ph_with(value = p, location = loc_body) %>%
        add_footer()
    }
  }
  
  # Export the PowerPoint
  return(print(ppt, target = file))
}


output$syse_types_download_btn <- downloadHandler( filename = date_stamped_filename("System Exits Report - "),
                                                   content = function(file) 
    {
     logToConsole(session, "System Exit Types data download")
     
     write_xlsx(
       list(
         "ExitsByType Metadata" = syse_export_summary_initial_df() %>%
           bind_rows(
             syse_export_filter_selections()
           ),
         ## dummy dataset read-in from global.R for now
         "SystemExitData" = test_exits_data
       ),
       path = file,
       format_headers = FALSE,
       col_names = TRUE
     )        
     
})

output$syse_types_download_btn_ppt <- downloadHandler(filename = function() {
  paste("System Exits_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    logToConsole(session, "In syse_types_download_btn_ppt")
  
    sys_exits_ppt_export(file = file, 
                       title_slide_title = "System Exits by Type",
                       summary_items = syse_export_summary_initial_df() %>%
                         filter(Chart != "Start Date" & Chart != "End Date") %>% 
                         bind_rows(syse_export_filter_selections(),
                                   data.frame(Chart="Total System Exits", Value = scales::label_comma()(sum(test_exits_data$Count)))),
                       plots = list("System Exits by Type" = syse_types_chart("Destination Type", input$syse_dest_type_filter)),
                       summary_font_size = 19
                       )
})

output$syse_compare_download_btn <- downloadHandler(filename = 'tmp',{
  
})

output$syse_compare_download_btn_ppt <- downloadHandler(filename = 'tmp',{
  
})

output$syse_phd_download_btn <- downloadHandler(filename = 'tmp',{
  
})

output$syse_phd_download_btn_ppt <- downloadHandler(filename = 'tmp',{
  
})

observeEvent(input$syse_methodology_type, {
  
  updatePickerInput(
    session, 
    "syse_race_ethnicity", 
    choices = syse_race_ethnicity_cats(input$syse_methodology_type)
  )

},
ignoreInit = TRUE)
toggle_syse_components <- function(cond, init=FALSE) {
  # 1. toggles the filters (disabled for Composition)
  # 2. toggles subtabs and download button based if valid file has been uploaded
  # 3. moves download button to be in line with subtabs
  tabs <- c(
    "System Exit Types" = "types",
    "System Exit Comparisons" = "compare",
    "Permanent Housing Demographics" = "phd"
  )
  
  for (tab in tabs) {
    shinyjs::toggle(glue('syse_{tab}_subtabs'), condition = cond)
    shinyjs::toggle(selector = glue('#syse_{tab}_subtabs + div.tab-content'), condition = cond)
    shinyjs::toggle(glue('syse_{tab}_download_btn'), condition = cond)
    shinyjs::toggle(glue('syse_{tab}_download_btn_ppt'), condition = cond)
    
    # move download button to subtab row and only show if there's data
    if(init) {
      shinyjs::runjs(
        glue("
            document.getElementById('syse_{tab}_subtabs')
              .insertAdjacentHTML('beforeEnd', '<li class=\"syse_download_tab\" id=\"syse_{tab}_download_tab\"></li>');
            $('#syse_{tab}_download_btn').appendTo('#syse_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
            $('#syse_{tab}_download_btn_ppt').appendTo('#syse_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
          ")
      )
    }
  }
  
  shinyjs::toggle('syse_client_level_download_btn', condition = cond)
  if(init) {
    shinyjs::runjs("
      document.getElementById('syse_tabbox')
        .insertAdjacentHTML('beforeEnd', '<li class=\"syse_download_tab\" id=\"syse_client_level_download_tab\"></li>');
      $('#syse_client_level_download_btn').appendTo('#syse_client_level_download_tab')
        .toggle('{cond}' == 'TRUE');
    ")
  }
  
}
toggle_syse_components(FALSE, init=TRUE) # initially hide them