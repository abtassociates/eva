# when user changes chart tabs
# hide demographic filters for Composition chart
# hide other stuff if valid file is not uploaded
# move chart download button to be inline with subtabs
observeEvent(input$syso_tabbox, {
  req(valid_file() == 1)
  logMetadata(paste0("Clicked on ", input$syso_tabbox,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  toggleClass(
    id = "syso_inflowoutflow_filters",
    condition = input$syso_tabbox == "System Demographics",
    class = "filter-hidden"
  )
}, ignoreNULL = TRUE, ignoreInit = TRUE) #confirm if need to have ignore init?


observeEvent(input$sys_inflow_outflow_subtabs, {
  req(valid_file() == 1)
  logMetadata(paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_inflow_outflow_subtabs,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$sys_status_subtabs, {
  req(valid_file() == 1)
  logMetadata(paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_status_subtabs,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$sys_comp_subtabs, {
  req(valid_file() == 1)
  logMetadata(paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_comp_subtabs,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$methodology_type, {
  
  updatePickerInput(
    session, 
    "syso_race_ethnicity", 
    choices = syso_race_ethnicity_cats(input$methodology_type)
  )

  # update System Composition Grouped Races/Ethnicities label
  grouped_re_lbl_new <- ifelse(input$methodology_type == 1, "Grouped", "Hispanic-Focused")
  shinyjs::runjs(
    glue("
      $('#system_composition_selections input[value=\"Grouped Races/Ethnicities\"] + span').text('{grouped_re_lbl_new} Races/Ethnicities');
    ")
  )
},
ignoreInit = TRUE)

observeEvent(input$syso_level_of_detail, {
  updatePickerInput(session, "syso_spec_pops",
                    # label = "Special Populations",
                    choices = syso_spec_pops_people)
})

#### DISPLAY FILTER SELECTIONS ###
syso_detailBox <- reactive({
  # remove group names from race/ethnicity filter
  # so we can use getNameByValue() to grab the selected option label
  detail_line <- function(detail_label, val_list, inputVal) {
    return(
      HTML(glue(
        "<b>{detail_label}:</b> {getNameByValue(val_list, inputVal)} <br>"
      ))
    )
  }
  
  selected_race <- getNameByValue(
    syso_race_ethnicity_cats(input$methodology_type), 
    input$syso_race_ethnicity
  )
  
  race_ethnicity_line <- HTML(glue(
    "<b>Race/Ethnicity:</b> {selected_race} <br>"
  ))
  
  list(
    br(),
    strong("Date Range: "),
    
    format(ReportStart(), "%m-%d-%Y"), " to ", format(ReportEnd(), "%m-%d-%Y"), br(),
    
    if (input$syso_project_type != "All")
      chart_selection_detail_line("Project Type Group", syso_project_types, input$syso_project_type),
    
    #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
    HTML(glue(
      "<b>Methodology Type:</b> {str_sub(getNameByValue(syso_methodology_types, input$methodology_type), start = 1, end = 8)} <br>"
    )),
    
    if (length(input$syso_age) != length(syso_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syso_age, collapse = ', ')} <br>"
      )),
    
    if (selected_race != "All Races/Ethnicities")
      race_ethnicity_line,
    
    if(getNameByValue(syso_spec_pops_people, input$syso_spec_pops) != "All Statuses")
      HTML(glue(
        "<b>Veteran Status:</b> {paste(getNameByValue(syso_spec_pops_people, input$syso_spec_pops), '(Adult Only)')} <br>"
      ))
    
  )
})

output$sys_act_detail_filter_selections <- renderUI({ syso_detailBox() })
output$sys_act_summary_filter_selections <- renderUI({
  req(valid_file() == 1)
  syso_detailBox() 
})

toggle_sys_components <- function(cond, init=FALSE) {
  # 1. toggles the filters (disabled for Composition)
  # 2. toggles subtabs and download button based if valid file has been uploaded
  # 3. moves download button to be in line with subtabs
  tabs <- c(
    "System Flow" = "inflow_outflow",
    "Client System Status" = "status",
    "System Demographics" = "comp"
  )
  
  for (tab in tabs) {
    shinyjs::toggle(glue('sys_{tab}_subtabs'), condition = cond)
    shinyjs::toggle(selector = glue('#sys_{tab}_subtabs + div.tab-content'), condition = cond)
    shinyjs::toggle(glue('sys_{tab}_download_btn'), condition = cond)
    shinyjs::toggle(glue('sys_{tab}_download_btn_ppt'), condition = cond)
    
    # move download button to subtab row and only show if there's data
    if(init) {
      shinyjs::runjs(
        glue("
            document.getElementById('sys_{tab}_subtabs')
              .insertAdjacentHTML('beforeEnd', '<li class=\"syso_download_tab\" id=\"sys_{tab}_download_tab\"></li>');
            $('#sys_{tab}_download_btn').appendTo('#sys_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
            $('#sys_{tab}_download_btn_ppt').appendTo('#sys_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
          ")
      )
    }
  }
  
  shinyjs::toggle('client_level_download_btn', condition = cond)
  if(init) {
    shinyjs::runjs("
      document.getElementById('syso_tabbox')
        .insertAdjacentHTML('beforeEnd', '<li class=\"client_level_download_tab\" id=\"client_level_download_tab\"></li>');
      $('#client_level_download_btn').appendTo('#client_level_download_tab')
        .toggle('{cond}' == 'TRUE');
    ")
  }
  
}
toggle_sys_components(FALSE, init=TRUE) # initially hide them

sys_export_summary_initial_df <- function() {
  
  logMetadata(paste0("Downloaded System Overview Tabular Data: ", input$syso_tabbox,
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
      strftime(ReportStart(), "%m/%d/%y"),
      strftime(ReportEnd(), "%m/%d/%y"),
      getNameByValue(syso_methodology_types, input$methodology_type),
      getNameByValue(syso_hh_types, input$syso_hh_type),
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail),
      getNameByValue(syso_project_types, input$syso_project_type)
    )
  ))
}

sys_export_filter_selections <- function() {
  return(tibble(
    Chart = c(
      "Age",
      "Veteran Status",
      "Race/Ethnicity"
    ),
    Value = c(
      if(identical(syso_age_cats, input$syso_age)) {"All Ages"} else {paste(input$syso_age, collapse=", ")},
      getNameByValue(syso_spec_pops_people, input$syso_spec_pops),
      getNameByValue(syso_race_ethnicity_cats(input$methodology_type), input$syso_race_ethnicity)
    )
  ))
}

#### FILTERS ###

# Population reactives ----------------------------------------------------

# Set race/ethnicity filter options based on methodology type selection
# Set special populations options based on level of detail selection
syso_race_ethnicity_cats <- function(methodology = 1){
  ifelse(
    methodology == 1,
    list(syso_race_ethnicity_method1),
    list(syso_race_ethnicity_method2)
  )[[1]]
}

# PowerPoint Export -------------------------------------------------------
sys_overview_ppt_export <- function(file,
                                    title_slide_title,
                                    summary_items,
                                    plot_slide_title,
                                    plot1,
                                    plot2 = NULL,
                                    summary_font_size) {
  
  logMetadata(paste0("Downloaded System Overview Powerpoint: ", title_slide_title,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  #NEED TO UPDATE - if want to get more granular, need to detect with title slide
  
  report_period <- paste0("Report Period: ", 
                          format(ReportStart(), "%m/%d/%Y"),
                          " - ",
                          format(ReportEnd(), "%m/%d/%Y")
  )
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
  
  add_footer <- function(.ppt) {
    return(
      .ppt %>%
        ph_with(value = paste0("CoC Code: ", Export()$SourceID), location = loc_footer) %>%
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
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = fpar(ftext(plot_slide_title, fp_title)), location = loc_title) %>%
    ph_with(value = plot1, location = loc_body) %>%
    add_footer()
  
  if(!is.null(plot2)) {
    ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = fpar(ftext(plot_slide_title, fp_title)), location = loc_title) %>%
      ph_with(value = plot2, location = loc_body) %>%
      add_footer()
  }
  
  # Export the PowerPoint
  return(print(ppt, target = file))
}

suppression_msg <- "The chart cannot be displayed because there are fewer than 11 clients."
no_data_msg <- "No data to show."

# Display Filter Selection in Detail Box ----------------------------------

chart_selection_detail_line <- function(detail_label, val_list, inputVal) {
  return(
    HTML(glue(
      "<strong>{detail_label}:</strong> {getNameByValue(val_list, inputVal)} <br>"
    ))
  )
}


# Total Count Above Chart -------------------------------------------------

sys_total_count_display <- function(total_count) {
  return(paste0(
    str_wrap(
      paste0(
        "Total ",
        case_when(
          input$syso_level_of_detail == "All" ~ "People",
          input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
          TRUE ~
            getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
        ),
        if_else(
          input$syso_hh_type == "All",
          "",
          paste0(" in ",
                 str_remove(getNameByValue(syso_hh_types, input$syso_hh_type), "- "),
                 " Households")
        ),       ": ",
        scales::comma(total_count)
      ),
      width = 40
    ),
    "\n")
  )
}

get_adj_font_size <- function(font_size, isExport) {
  return(
    font_size*ifelse(isExport, sys_chart_export_font_reduction, 1)
  )
}

observeEvent(input$dimension,{
  windowSize(input$dimension)
})

output$client_level_download_btn <- downloadHandler(
  filename = date_stamped_filename("Client Level Export - "),
  content = function(file) {
    detail_client_fields <- c(
      "PersonalID",
      "AgeCategory",
      "VeteranStatus",
      
      "AmIndAKNative",
      "Asian",
      "BlackAfAmerican",
      "HispanicLatine" = "HispanicLatinaeo",
      "MidEastNAf" = "MidEastNAfrican",
      "NativeHIPacific",
      "White",
      "RaceEthnicityUnknown",
      
      "AmIndAKNativeAloneMethod1Detailed",
      "AmIndAKNativeLatineMethod1Detailed",
      "AsianAloneMethod1Detailed",
      "AsianLatineMethod1Detailed",
      "BlackAfAmericanAloneMethod1Detailed",
      "BlackAfAmericanLatineMethod1Detailed",
      "LatineAloneMethod1Detailed",
      "MidEastNAfricanAloneMethod1Detailed",
      "MidEastNAfricanLatineMethod1Detailed",
      "NativeHIPacificAloneMethod1Detailed",
      "NativeHIPacificLatineMethod1Detailed",
      "WhiteAloneMethod1Detailed",
      "WhiteLatineMethod1Detailed",
      "MultipleNotLatineMethod1Detailed",
      "MultipleLatineMethod1Detailed",
      
      "BILPOCMethod1Summarized",
      "WhiteMethod1Summarized",
      
      "AmIndAKNativeMethod2Detailed",
      "AsianMethod2Detailed",
      "BlackAfAmericanMethod2Detailed",
      "LatineMethod2Detailed",
      "MidEastNAfricanMethod2Detailed",
      "NativeHIPacificMethod2Detailed",
      "WhiteMethod2Detailed",
      
      "BlackAfAmericanLatineMethod2Summarized",
      "LatineMethod2Summarized",
      "LatineAloneMethod2Summarized"
    )
    
    report_status_fields <- c(
      "Earliest-ReportStatus" = "InflowTypeSummary",
      "Earliest-ReportStatusDetail" = "InflowTypeDetail",
      "Latest-ReportStatus" = "OutflowTypeSummary",
      "Latest-ReportStatusDetail" = "OutflowTypeDetail"
    )
    
    enrollment_fields <- c(
      "PersonalID",
      "eecr",
      "lecr",
      "EnrollmentID",
      "HouseholdType",
      "CorrectedHoH",
      "ProjectType",
      "EntryDate",
      "LivingSituation",
      "MoveInDateAdjust",
      "ExitAdjust",
      "Destination"
    )
    
    enrollment_info <- sys_universe_ppl_flags()[, ..enrollment_fields][
      , `:=`(
        Destination = living_situation(Destination),
        LivingSituation = living_situation(LivingSituation),
        HouseholdType = factor(
          case_when(
            HouseholdType %in% c("PY", "ACminusPY") ~ "AC",
            HouseholdType %in% c("UY", "AOminusUY") ~ "AO",
            TRUE ~ HouseholdType
          ),
          levels = c("AO", "AC", "CO", "UN")
        )
      )
    ]
    
    earliest_report_info <- enrollment_info[eecr == 1][, c("eecr","lecr") := NULL]
    setnames(earliest_report_info, 
             old = setdiff(names(earliest_report_info), "PersonalID"), 
             new = paste0("Earliest-", setdiff(names(earliest_report_info), "PersonalID")))
    
    latest_report_info <- enrollment_info[lecr == 1][,  c("eecr","lecr") := NULL]
    setnames(latest_report_info, 
             old = setdiff(names(latest_report_info), "PersonalID"), 
             new = paste0("Latest-", setdiff(names(latest_report_info), "PersonalID")))

    # details tab
    client_level_details <- unique(sys_universe_ppl_flags()[
      , 
      c(..detail_client_fields, ..report_status_fields)
    ])[
      earliest_report_info, on = "PersonalID", nomatch = 0
    ][
      latest_report_info, on = "PersonalID", nomatch = 0
    ]
    setnames(client_level_details, 
             old = report_status_fields, 
             new = names(report_status_fields))
    
    # User's filter selections - metadata tab
    export_date_info <- tibble(
      Chart = c(
        "ExportStart",
        "ExportEnd"
      ),
      Value = c(
        as.character(meta_HUDCSV_Export_Start()),
        as.character(meta_HUDCSV_Export_End())
      )
    )
    
    system_df_info <- system_activity_prep_detail() %>% 
      select(Status, values, Time, InflowOutflow, InflowOutflowSummary)
    
    filter_selections <- rbind(
      export_date_info, # ExportStart, Exportend
      sys_export_summary_initial_df(), # ReportStart, ReportEnd, Methodology Type, Household Type, Level of Detail, Project Type Group
      sys_export_filter_selections(), # Age, Veteran Status, Race/Ethnicity
      tibble(
        Chart = "Total Served (Start + Inflow) People",
        Value = sum(system_df_info %>% filter(InflowOutflow == 'Inflow') %>% pull(values), na.rm = TRUE)
      )
    )
    colnames(filter_selections) <- c("Filter","Selection")
    
    # probably want to read in the glossary tab as a csv or Excel and append to it.
    
    # all sheets for export
    client_level_export_list <- list(
      client_level_metadata = filter_selections,
      data_dictionary = setNames(
        read.csv(here("www/client-level-export-data-dictionary.csv")),
        c("Column Name", "Variable Type", "Definition")
      ),
      client_level_details = client_level_details
    )
    
    names(client_level_export_list) = c(
      "Metadata",
      "Data Dictionary",
      "Details"
    )
    
    write_xlsx(
      client_level_export_list,
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    logToConsole("Downloaded Client Level Export")
    logMetadata(paste0(
      "Downloaded Client Level Export",
      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")
    ))
    
    exportTestValues(client_level_export_details = client_level_details) 
  }
)