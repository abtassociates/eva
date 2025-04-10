
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
    
    enrollment_info <- get_client_level_export()[, ..enrollment_fields][
      , `:=`(
        Destination = living_situation(Destination),
        LivingSituation = living_situation(LivingSituation)
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
    client_level_details <- unique(get_client_level_export()[
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
        as.character(session$userData$meta_HUDCSV_Export_Start),
        as.character(session$userData$meta_HUDCSV_Export_End)
      )
    )
    
    system_df_info <- sys_inflow_outflow_chart_data() %>% 
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
    
    logToConsole(session, "Downloaded Client Level Export")
    logMetadata(session, paste0(
      "Downloaded Client Level Export",
      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")
    ))
    
    exportTestValues(client_level_export_details = client_level_details) 
  }
)

# Client-level download
get_client_level_export <- reactive({
  merge(
    period_specific_data()[["Full"]],
    session$userData$Client %>% select(PersonalID, !!race_cols), 
    by="PersonalID"
  )
})

source(here("sandbox/timeline_viewer.R"), local=TRUE)
