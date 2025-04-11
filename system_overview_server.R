# when user changes chart tabs
# hide demographic filters for Composition chart
# hide other stuff if valid file is not uploaded
# move chart download button to be inline with subtabs
observeEvent(input$syso_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  toggleClass(
    id = "syso_inflowoutflow_filters",
    condition = input$syso_tabbox == "System Demographics",
    class = "filter-hidden"
  )
}, ignoreNULL = TRUE, ignoreInit = TRUE) #confirm if need to have ignore init?


observeEvent(input$sys_inflow_outflow_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_inflow_outflow_subtabs,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$sys_status_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_status_subtabs,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$sys_comp_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_comp_subtabs,
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
    
    format(session$userData$ReportStart, "%m-%d-%Y"), " to ", format(session$userData$ReportEnd, "%m-%d-%Y"), br(),
    
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
  
  logMetadata(session, paste0("Downloaded System Overview Tabular Data: ", input$syso_tabbox,
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
  
  logMetadata(session, paste0("Downloaded System Overview Powerpoint: ", title_slide_title,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  #NEED TO UPDATE - if want to get more granular, need to detect with title slide
  
  report_period <- paste0("Report Period: ", 
                          format(session$userData$ReportStart, "%m/%d/%Y"),
                          " - ",
                          format(session$userData$ReportEnd, "%m/%d/%Y")
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

observe({
  windowSize(input$dimension)
})

# if user changes filters, update the reactive vals
# which get used for the various System Overview charts
observeEvent({
  input$syso_hh_type
  input$syso_level_of_detail
  input$syso_project_type
  input$methodology_type
  input$syso_age
  input$syso_spec_pops
  input$syso_race_ethnicity
}, {
  # hide download buttons if < 11 records
  # All Served is handled in system_composition_server.R
  # for that chart, we also hide if all *cells* are < 11
  shinyjs::toggle(
    "sys_inflow_outflow_download_btn sys_inflow_outflow_download_btn_ppt", 
    condition = nrow(get_inflow_outflow_full()) > 10
  )
  shinyjs::toggle(
    "sys_status_download_btn sys_status_download_btn_ppt", 
    condition = sum(get_sankey_data()$freq) > 10
  )
}, ignoreInit = TRUE)

source("client_level_export_server.R", local=TRUE)

# Get period report_dates --------------------------------------------
get_months_in_report_period <- function() {
  seq.Date(from = session$userData$ReportStart, to = session$userData$ReportEnd, by = "months")
} 
get_report_dates <- function() {
  months_in_report_period <- get_months_in_report_period()
  c(
    list("Full" = c(session$userData$ReportStart, session$userData$ReportEnd)),
    setNames(
      lapply(months_in_report_period, function(d) {
        c(d, ceiling_date(d, "month") - days(1))
      }),
      months_in_report_period
    )
  )
}

# Cache management for period-specific universe_ppl_flag datasets --------------
# Check cache size - this keeps the cache manageable
check_cache_size <- function(cache, max_size_mb = 100) {
  cache_size <- utils::object.size(cache) / 1024^2  # Convert to MB
  if (cache_size > max_size_mb) {
    rm(list = ls(cache), envir = cache)
    gc()
    return(TRUE)
  }
  FALSE
}

# Get period-specific universe_ppl_flag datasets ---------------------------
period_specific_data <- reactive({
  if(is.null(session$userData$period_cache)) {
    session$userData$period_cache <- new.env()
  }
  cache <- session$userData$period_cache
  
  check_cache_size(cache)
  
  cache_key <- digest::digest(list(
    if(isTruthy(input$in_demo_mode)) "demo" else input$imported$name,
    
    # Client-level filters
    input$syso_age,
    input$syso_race_ethnicity,
    input$syso_spec_pops,
    
    # Enrollment-level filters
    input$syso_hh_type,
    input$syso_level_of_detail,
    input$syso_project_type
  ))
  
  cached_result <- cache[[cache_key]]
  if (!is.null(cached_result)) {
    return(cached_result)
  }
  
  upload_name <- ifelse(input$in_demo_mode, "DEMO", input$imported$name)
  
  results <- lapply(
    session$userData$report_dates,
    function(period) {
      # custom_rprof({
      enrollment_categories <- session$userData$get_period_specific_enrollment_categories(period, upload_name)
      nbn_services <- session$userData$get_period_specific_nbn_enrollment_services(period, upload_name)
        
      all_filtered <- universe_filtered(enrollment_categories, nbn_services)
      universe_w_enrl_flags <- universe_enrl_flags(all_filtered, period)
      universe_w_ppl_flags <- universe_ppl_flags(universe_w_enrl_flags, period)
      
      # Add month flag for month-periods
      if(!identical(period, session$userData$report_dates[["Full"]])) {
        universe_w_ppl_flags[, month := as.Date(period[1])]
      }
      # }, "system_overview_server.R")
      universe_w_ppl_flags
    }
  )
  cache[[cache_key]] <- results
  session$userData$period_cache <- cache
  results # %>% 
    # Doing this here for testing purposes, so can view fuller intermediate datasets
    # fsubset(eecr | lecr | lookback)
})

# Client-level flags, filtered ----------------------------------------------------
client_categories_filtered <- reactive({
  req(nrow(session$userData$client_categories) > 0)
  session$userData$client_categories[, All := 1][
    AgeCategory %in% input$syso_age &
      get(input$syso_race_ethnicity) == 1 &
      (
        input$syso_spec_pops == "None" |
          (input$syso_spec_pops == "Veteran" &
             VeteranStatus == 1 & !(AgeCategory %in% c("0 to 12", "13 to 17"))) |
          (input$syso_spec_pops == "NonVeteran" &
             VeteranStatus == 0 & !(AgeCategory %in% c("0 to 12", "13 to 17")))
      )
  ]
})

# Period-specific, user-filtered, enrollment-level universe applied ------------------
universe_filtered <- function(enrollment_categories, nbn_services) {
  # browser()
  join(
    enrollment_categories,
    nbn_services, 
    on = "EnrollmentID"
  ) %>%
    join( # Inner Join with client categories
      # This is necessary for bringing in Veteran Status, but will also make the rest faster
      client_categories_filtered(),
      # client_categories_filt, # this is used for mirai
      on = "PersonalID",
      how = "inner"
    ) %>%
    fsubset(
      # Household type filter
      (input$syso_hh_type == "All" |
         (input$syso_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
         (input$syso_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
         (input$syso_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
         (input$syso_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
         input$syso_hh_type == HouseholdType
      ) &
        # Level of detail filter
        (input$syso_level_of_detail == "All" |
           (input$syso_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (input$syso_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        (input$syso_project_type == "All" |
           input$syso_project_type == eecr_project_type
        )
    )
}

# Period-Specific, Filtered, Enrollment-Level Universe -------------------------
## Enrollment-level flags ------------------------
# hello weary traveler amongst these date ranges. you may find it helpful to
# find example clients and their Entry and Exit Dates and enter them into
# https://onlinetools.com/time/visualize-date-intervals <- here.
# add inflow type and active enrollment typed used for system overview plots
#
# While the following datasets appear to be inflow-outflow specific, 
# the reason they are stored in this system_overview script is because they 
# 
universe_enrl_flags <- function(all_filtered, period) {
  startDate <- period[1]
  endDate <- period[2]
  
  all_filtered[, `:=`(
    # INFLOW CALCULATOR COLUMNS
    # LOGIC: active homeless at start
    # basically it has to straddle report start
    # the entry date of the EECR needs to be on or before the reporting period
    # the exitadjust has to be after report start
    # OR the eecr & lookback1 have to end and start within 14 days of each
    # other and of the report start
    # EnrolledHomeless status of the EECR needs to be true
    # JUST FOR FULL DISCLOSURE, this means: 
    # ProjectType %in% project_types_enrolled_homeless |
    # lh_prior_livingsituation == TRUE
    
    active_at_start_homeless =
      eecr == TRUE &
      (
        # PH project types have move-in after start (or no move-in)
        (
          ProjectType %in% ph_project_types &
            (
              is.na(MoveInDateAdjust) |
                MoveInDateAdjust >= startDate
            )
        ) |
          
          ( # take only ce enrollments where the PLS or the CLS is <= 90 days
            # prior to ReportStart
            ProjectType == ce_project_type & 
              was_lh_at_start
          ) |
          # take any other enrollments if their PLS was literally homeless
          (
            !(ProjectType %in% ph_project_types) &
              EnrolledHomeless == TRUE
          )
      ) &
      # Enrollment straddles start or the enrollment is within 2 weeks from start
      # and within 2 weeks of prev enrollment
      (straddles_start == TRUE |
         (straddles_start == FALSE &
            EntryDate >= startDate &
            between(as.numeric(difftime(EntryDate, startDate, units = "days")),
                    0,
                    14) &
            !is.na(days_since_lookback) &
            between(as.numeric(days_since_lookback), 0, 14))),
    
    #LOGIC: enrolled housed at start
    # Exit.ExitDate is null or > ReportStartDate AND
    
    # Project.ProjectType IN (3, 9, 10, 13) AND
    # Enrollment.MoveInDate is !NULL OR <= ReportStartDate AND
    # Enrollment.LivingSituation is LiterallyHomeless*"
    active_at_start_housed = eecr == TRUE & 
      ProjectType %in% ph_project_types & 
      !is.na(MoveInDateAdjust) &
      MoveInDateAdjust < startDate,
    
    # LOGIC helper columns
    
    lookback1_perm_dest = lookback == 1 & 
      Destination %in% perm_livingsituation,
    
    eecr_lh_at_entry = eecr == TRUE &
      lh_at_entry == TRUE,
    
    at_least_14_days_to_eecr_enrl = eecr == TRUE &
      !is.na(days_since_lookback) &
      days_since_lookback >= 14,
    
    lookback1_temp_dest = lookback == 1 & 
      !(Destination %in% perm_livingsituation),
    
    unknown_at_start = eecr == TRUE &
      straddles_start & (
        # Non-Res Project Types and not lh
        (
          ProjectType %in% non_res_project_types &
            (!was_lh_at_start | is.na(was_lh_at_start))
        ) |
          # nbn shelter
          (ProjectType == es_nbn_project_type &
             (in_date_range == TRUE | NbN15DaysBeforeReportStart == FALSE))
        
      ),
    
    # outflow columns
    perm_dest_lecr = lecr == TRUE &
      Destination %in% perm_livingsituation &
      between(ExitAdjust, startDate, endDate),
    
    temp_dest_lecr = lecr == TRUE &
      !(Destination %in% perm_livingsituation) &
      between(ExitAdjust, startDate, endDate),
    
    homeless_at_end = lecr == TRUE & 
      straddles_end &
      ( # e/e shelter, th, sh
        ProjectType %in% lh_project_types_nc |
          
          # nbn shelter
          (ProjectType == es_nbn_project_type &
             (in_date_range == TRUE | NbN15DaysAfterReportEnd == TRUE)) |
          
          # Non-Res Project Types
          (
            ProjectType %in% non_res_project_types &
              was_lh_at_end
          ) |
          
          # PSH, OPH, RRH
          (ProjectType %in% ph_project_types &
             (is.na(MoveInDateAdjust) | MoveInDateAdjust >= period[2]))
      ),
    
    housed_at_end = lecr == TRUE & 
      straddles_end &
      ProjectType %in% ph_project_types & 
      !is.na(MoveInDateAdjust) &
      MoveInDateAdjust < endDate,
    
    unknown_at_end = lecr == TRUE &
      straddles_end & (
        # Non-Res Project Types and not lh
        (
          ProjectType %in% non_res_project_types &
            (!was_lh_at_end | is.na(was_lh_at_end))
        ) |
          # nbn shelter
          (ProjectType == es_nbn_project_type &
             (in_date_range == TRUE | NbN15DaysBeforeReportEnd == FALSE))
        
      )
  )]
}

## People-level flags ------------------------
# Need to keep it enrollment-level so other scripts can reference the enrollments
get_days_of_data <- function(period) {
  # Export Start Date - Adjusted to be the 1st of the month
  # if the start date's day of the month = 1, then that's the start date
  # otherwise go forward a month and use the 1st of that month.
  ExportStartAdjusted <- if_else(
    day(session$userData$meta_HUDCSV_Export_Start) == 1,
    session$userData$meta_HUDCSV_Export_Start,
    floor_date(session$userData$meta_HUDCSV_Export_Start %m+% months(1), unit = "month"))
  
  period_end_date <- period[2]
  
  return(period_end_date - ExportStartAdjusted)
}

universe_ppl_flags <- function(universe_df, period) {
  setkey(universe_df, PersonalID)
  
  days_of_data <- get_days_of_data(period)

  universe_df[, `:=`(
    # INFLOW
    active_at_start_homeless_client = any(active_at_start_homeless, na.rm = TRUE),
    
    active_at_start_housed_client = any(active_at_start_housed, na.rm = TRUE),
    
    return_from_perm_client = any(lookback1_perm_dest, na.rm = TRUE) & 
      any(at_least_14_days_to_eecr_enrl, na.rm = TRUE),
    
    reengaged_from_temp_client = any(lookback1_temp_dest, na.rm = TRUE) & 
      # max(eecr_lh_at_entry) == 1 & 
      any(at_least_14_days_to_eecr_enrl, na.rm = TRUE),
    
    newly_homeless_client = any(lookback == TRUE, na.rm = TRUE) |
      !any(eecr_lh_at_entry, na.rm = TRUE) | 
      !any(at_least_14_days_to_eecr_enrl, na.rm = TRUE),
    
    unknown_at_start_client = any(unknown_at_start, na.rm = TRUE),
    
    # OUTFLOW
    perm_dest_client = any(perm_dest_lecr, na.rm = TRUE),
    
    temp_dest_client = any(temp_dest_lecr, na.rm = TRUE),
    
    homeless_at_end_client = any(homeless_at_end, na.rm = TRUE),
    
    housed_at_end_client = any(housed_at_end, na.rm = TRUE),
    
    unknown_at_end_client = any(unknown_at_end, na.rm = TRUE),
    
    has_enrollment_after_lecr_client = any(has_enrollment_after_lecr, na.rm = TRUE)
  ), by = PersonalID
  ][, `:=`(
    InflowTypeSummary = factor(
      fcase(
        active_at_start_homeless_client | active_at_start_housed_client, "Active at Start",
        newly_homeless_client | return_from_perm_client | reengaged_from_temp_client | unknown_at_start, "Inflow",
        default = "something's wrong"
      ), levels = inflow_outflow_summary_levels
    ),
    
    InflowTypeDetail = factor(
      fcase(
        active_at_start_homeless_client, "Homeless",
        active_at_start_housed_client, "Housed",
        return_from_perm_client, "Returned from \nPermanent",
        reengaged_from_temp_client, "Re-engaged from \nNon-Permanent",
        newly_homeless_client & days_of_data >= 1094, "First-Time \nHomeless",
        newly_homeless_client & days_of_data < 1094, "Inflow\nUnspecified",
        unknown_at_start, "Inactive", 
        default = "something's wrong"
      ), levels = c(active_at_levels, inflow_detail_levels)
    ),
    
    OutflowTypeSummary = factor(
      fcase(
        (perm_dest_client | temp_dest_client | unknown_at_end_client) & 
          !has_enrollment_after_lecr_client, "Outflow",
        homeless_at_end_client | housed_at_end_client | has_enrollment_after_lecr_client, "Active at End",
        default = "something's wrong"
      ), levels = inflow_outflow_summary_levels
    ),
    
    OutflowTypeDetail = factor(
      fcase(
        perm_dest_client, "Exited,\nPermanent",
        temp_dest_client, "Exited,\nNon-Permanent",
        unknown_at_end_client, "Inactive",
        homeless_at_end_client & !is.na(homeless_at_end_client), "Homeless",
        housed_at_end_client | has_enrollment_after_lecr_client, "Housed",
        default = "something's wrong"
      ), levels = c(outflow_detail_levels, rev(active_at_levels))
    )
  )]
}
