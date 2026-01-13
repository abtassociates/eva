
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
      "HispanicLatino" = "HispanicLatinao",
      "MidEastNAf" = "MidEastNAfrican",
      "NativeHIPacific",
      "White",
      "RaceEthnicityUnknown",
      
      "AmIndAKNativeAloneMethod1Detailed",
      "AmIndAKNativeLatinoMethod1Detailed",
      "AsianAloneMethod1Detailed",
      "AsianLatinoMethod1Detailed",
      "BlackAfAmericanAloneMethod1Detailed",
      "BlackAfAmericanLatinoMethod1Detailed",
      "LatinoAloneMethod1Detailed",
      "MidEastNAfricanAloneMethod1Detailed",
      "MidEastNAfricanLatinoMethod1Detailed",
      "NativeHIPacificAloneMethod1Detailed",
      "NativeHIPacificLatinoMethod1Detailed",
      "WhiteAloneMethod1Detailed",
      "WhiteLatinoMethod1Detailed",
      "MultipleNotLatinoMethod1Detailed",
      "MultipleLatinoMethod1Detailed",
      
      "BILPOCMethod1Summarized",
      "WhiteMethod1Summarized",
      
      "AmIndAKNativeMethod2Detailed",
      "AsianMethod2Detailed",
      "BlackAfAmericanMethod2Detailed",
      "LatinoMethod2Detailed",
      "MidEastNAfricanMethod2Detailed",
      "NativeHIPacificMethod2Detailed",
      "WhiteMethod2Detailed",
      
      "BlackAfAmericanLatinoMethod2Summarized",
      "LatinoMethod2Summarized",
      "LatinoAloneMethod2Summarized"
    )
    
    report_status_fields <- c(
      "Earliest-ReportStatus" = "InflowTypeSummary",
      "Earliest-ReportStatusDetail" = "InflowTypeDetail",
      "Latest-ReportStatus" = "OutflowTypeSummary",
      "Latest-ReportStatusDetail" = "OutflowTypeDetail"
    )
    logToConsole(session, "before eecr_lecr_enrollment_info")
    eecr_lecr_enrollment_info <- session$userData$enrollment_categories %>% 
      fselect(
        EnrollmentID,
        HouseholdType,
        CorrectedHoH,
        ProjectType,
        EntryDate,
        LivingSituation,
        MoveInDateAdjust,
        ExitAdjust,
        Destination
      ) %>%
      fmutate(
        Destination = living_situation(Destination),
        LivingSituation = living_situation(LivingSituation),
        HouseholdType = fct_collapse(HouseholdType, !!!hh_types_in_exports)
      )

    logToConsole(session, "after eecr_lecr_enrollment_info")
    
    earliest_report_info <- get_client_level_export() %>%
      fselect(PersonalID, EnrollmentID) %>%
      join(eecr_lecr_enrollment_info, on="EnrollmentID")


    setnames(earliest_report_info, 
             old = setdiff(names(earliest_report_info), "PersonalID"), 
             new = paste0("Earliest-", setdiff(names(earliest_report_info), "PersonalID")))
    
    latest_report_info <- get_client_level_export() %>%
      fselect(PersonalID, EnrollmentID = EnrollmentID_lecr) %>%
      join(eecr_lecr_enrollment_info, on="EnrollmentID")
    
    setnames(latest_report_info, 
             old = setdiff(names(latest_report_info), "PersonalID"), 
             new = paste0("Latest-", setdiff(names(latest_report_info), "PersonalID")))
    
    # details tab
    client_level_details <- get_client_level_export() %>%
      fselect(c(detail_client_fields, unname(report_status_fields))) %>%
      funique() %>%
      join(earliest_report_info, on = "PersonalID", how="inner") %>%
      join(latest_report_info, on = "PersonalID", how="inner")

    setnames(client_level_details, 
             old = report_status_fields, 
             new = names(report_status_fields))
    
    #Monthly Statuses
    logToConsole(session, "before monthly_statuses")
    
    monthly_statuses <- period_specific_data()[["Months"]] %>%
      join(
        session$userData$enrollment_categories %>% 
          fmutate(
            moved_into_housing = ProjectType %in% ph_project_types & 
              between(MoveInDateAdjust, session$userData$ReportStart, session$userData$ReportEnd, incbounds = FALSE)
          ) %>%
          fselect(EnrollmentID, moved_into_housing),
        on = "EnrollmentID"
      ) %>%
      fsubset(
        InflowTypeSummary == "Inflow" |
        OutflowTypeSummary == "Outflow" |
        !OutflowTypeDetail %in% outflow_statuses_to_exclude_from_export
      ) %>%
      fgroup_by(PersonalID) %>%
      fmutate(
        `Moved into Housing During Report` = anyv(moved_into_housing, TRUE),
        `Exited to Permanent Destination During Report` = anyv(OutflowTypeDetail, "Exited, Permanent"),
        last_outflow = flast(OutflowTypeDetail)
      ) %>%
      fungroup() %>%
      fmutate(
        `Moved into Housing or Exited to Permanent Destination by Report End` = case_match(
          last_outflow,
          "Exited, Permanent" ~ "Yes - Exited, Permanent",
          "Housed" ~ "Yes - Enrolled, Housed",
          "Homeless" ~ "No - Enrolled, Homeless",
          .default = paste0("No - ", last_outflow)
        )
      ) %>%
      fselect(
        PersonalID, 
        `Moved into Housing During Report`,
        `Exited to Permanent Destination During Report`,
        `Moved into Housing or Exited to Permanent Destination by Report End`,
        month,
        OutflowTypeDetail
      ) %>%
      funique() %>%
      pivot(
        ids = c("PersonalID", 
                "Moved into Housing During Report",
                "Exited to Permanent Destination During Report",
                "Moved into Housing or Exited to Permanent Destination by Report End"),        # Column(s) defining the rows of the output
        names = "month", 
        values = "OutflowTypeDetail",
        how = "wider"
      )
    
    logToConsole(session, "after monthly_statuses")
    
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

    system_df_info <- sys_inflow_outflow_annual_chart_data() %>% 
      select(Detail, N, Summary, InflowOutflow)
    
    filter_selections <- rbind(
      export_date_info, # ExportStart, Exportend
      sys_export_summary_initial_df(), # ReportStart, ReportEnd, Methodology Type, Household Type, Level of Detail, Project Type Group
      sys_export_filter_selections(), # Age, Veteran Status, Race/Ethnicity
      tibble(
        Chart = "Total Served (Start + Inflow) People",
        Value = sum(system_df_info[InflowOutflow == 'Inflow']$N, na.rm = TRUE)
      )
    )
    colnames(filter_selections) <- c("Filter","Selection")
    
    logToConsole(session, "after filter_selections")
    
    adjusted_non_res_enrl <- session$userData$enrollment_categories %>%
      fsubset(
        adjusted_dates == TRUE, 
        PersonalID, EnrollmentID, ProjectType, EntryDate_orig, ExitAdjust_orig, EntryDate, ExitAdjust, lh_prior_livingsituation
      )
    if(nrow(session$userData$lh_info) > 0) {
      adjusted_non_res_enrl <- adjusted_non_res_enrl %>%
        join(
          session$userData$lh_info %>% 
            fgroup_by(EnrollmentID) %>% 
            fsummarise(lh_dates = paste(lh_date, collapse = ",")) %>% 
            fungroup() %>%
            fsubset(lh_dates != "NA"),
          on = "EnrollmentID"
        )
    } else {
      logToConsole(session, "session lh_info was empty, so adjusted_non_res_enrl set to NULL")
      adjusted_non_res_enrl <- NULL
    }
    logToConsole(session, "after adjusted_non_res_enrl")
    
    # probably want to read in the glossary tab as a csv or Excel and append to it.
    
    # everything together
    client_level_export_list <- list(
      client_level_metadata = filter_selections,
      data_dictionary = setNames(
        read.csv(here("www/client-level-export-data-dictionary.csv")),
        c("Column Name", "Variable Type", "Definition")
      ),
      client_level_details = client_level_details,
      monthly_statuses,
      adjusted_non_res_enrl
    )
    logToConsole(session, paste0("before naming, length(client_level_export_list) = ", length(client_level_export_list)))
    
    names(client_level_export_list) = c(
      "Metadata",
      "Data Dictionary",
      "Client Details",
      "Monthly Statuses",
      "Adjusted Non-Res"
    )
    logToConsole(session, paste0("after naming, length(client_level_export_list) = ", length(client_level_export_list)))
    
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
  period_specific_data()[["Full"]] %>%
    join(
      session$userData$client_categories,
      on = "PersonalID"
    ) %>%
    join(
      session$userData$Client %>% select(PersonalID, !!race_cols), 
      on="PersonalID"
    ) %>%
    join(
      session$userData$enrollment_categories %>% fselect(EnrollmentID, HouseholdType, CorrectedHoH, LivingSituation),
      on = "EnrollmentID"
    )
})

#source(here("sandbox/timeline_viewer.R"), local=TRUE)
