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
  list(
    br(),
    strong("Date Range: "),
    
    format(session$userData$ReportStart, "%m-%d-%Y"), " to ", format(session$userData$ReportEnd, "%m-%d-%Y"), br(),
    
    if (input$syso_project_type != "All")
      chart_selection_detail_line("Project Type Group", syso_project_types, str_remove(input$syso_project_type, "- ")),
    
    #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
    HTML(glue(
      "<b>Methodology Type:</b> {str_sub(getNameByValue(syso_methodology_types, input$methodology_type), start = 1, end = 8)} <br>"
    )),
    
    if (length(input$syso_age) != length(syso_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syso_age, collapse = ', ')} <br>"
      )),
    
    if (input$syso_race_ethnicity != "All")
      chart_selection_detail_line("Race/Ethnicity", syso_race_ethnicity_cats(input$methodology_type), input$syso_race_ethnicity),
    
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
        .insertAdjacentHTML('beforeEnd', '<li class=\"syso_download_tab\" id=\"client_level_download_tab\"></li>');
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
  if(methodology == 1) syso_race_ethnicity_method1 
  else syso_race_ethnicity_method2
}

# PowerPoint Export -------------------------------------------------------
sys_overview_ppt_export <- function(file,
                                    title_slide_title,
                                    summary_items,
                                    plots,
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
        full_unit_of_analysis_display(),
        ": ",
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

# Get period-specific universe_ppl_flag datasets ---------------------------
period_specific_data <- reactive({
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  logToConsole(session, "in period_specific_data")
  
  # custom_rprof({
  all_filtered <- join( 
    get_period_specific_enrollment_categories(),
    client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  )
  
  all_filtered_w_lh <- add_lh_info(all_filtered)
  universe_w_enrl_flags <- universe_enrl_flags(all_filtered_w_lh)
  universe_w_ppl_flags <- universe_ppl_flags(universe_w_enrl_flags)
  # }, "system_overview_server")
  
  shinyjs::toggle(
    "sys_inflow_outflow_download_btn", 
    condition = fndistinct(universe_w_ppl_flags[period == "Full", PersonalID]) > 10
  )
  shinyjs::toggle(
    "sys_inflow_outflow_download_btn_ppt", 
    condition = fndistinct(universe_w_ppl_flags[period == "Full", PersonalID]) > 10
  )
  
  # Split into months and full-period datasets
  list(
    Full = fsubset(universe_w_ppl_flags,period == "Full"),
    Months = universe_w_ppl_flags %>%
      fsubset(period != "Full") %>%
      fmutate(month = factor(
        format(as.Date(period), "%b %y"), 
        levels = format(get_months_in_report_period(), "%b %y")
      ))
  )
}) %>%
  # This saves the *results* in the cache so if they change inputs back to 
  # something already seen, it doesn't have to re-run the code
  bindCache(
    if(isTruthy(input$in_demo_mode)) "demo" else input$imported$name,

    # Client-level filters
    input$syso_age,
    input$syso_race_ethnicity,
    input$syso_spec_pops,

    # Enrollment-level filters
    input$syso_hh_type,
    input$syso_level_of_detail,
    input$syso_project_type,
    cache = "session"
  )

# Client-level flags, filtered ----------------------------------------------------
client_categories_filtered <- reactive({
  logToConsole(session, "In client_categories_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  req(fnrow(session$userData$client_categories) > 0)
 
  fsubset(session$userData$client_categories,
                 AgeCategory %in% input$syso_age &
                   (if(input$syso_race_ethnicity == "All") rep(TRUE, fnrow(session$userData$client_categories)) else get(input$syso_race_ethnicity) == 1) & 
                   (
                     input$syso_spec_pops == "None" |
                       (input$syso_spec_pops == "Veteran" &
                          VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
                       (input$syso_spec_pops == "NonVeteran" &
                          VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17"))
                   ))
  
})

# Create passes-enrollment-filter flag to exclude enrollments from eecr -------
enrollments_filtered <- reactive({
  logToConsole(session, "in enrollments_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  join(
    session$userData$enrollment_categories,
    session$userData$client_categories %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  ) %>%
    fmutate(
      passes_enrollment_filters =
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
        (input$syso_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
        (input$syso_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
        (input$syso_project_type == "SO" & ProjectType == out_project_type) |
        (input$syso_project_type == "AllNonRes" & ProjectType %in% non_res_project_types)
        )
    ) %>%
    fselect(-VeteranStatus)
})

## LH info for non-res enrollments -----------
# continuing the work of the base lh_non_res dataset from 07_system_overview.R 
# we now make it period-specific, and collapse it down to the enrollment-level
# so this contains enrollments with LH CLS and an indicator as to 
# whether InformationDate is within to 60 or 90 days 
# (depending on project type, but only limited to Non-Res Project Types) 
# from the period start/end
# we then merge this with enrollment_categories to fully replace the homeless_cls_finder function
# this avoids having to re-filter and do the check for each enrollment
lh_non_res_period <- function() {
  logToConsole(session, "in lh_non_res_period")
  lh_non_res <- expand_by_periods(session$userData$lh_non_res) %>%
    # Calculate time windows
    ftransform(
      start_window = startDate - fifelse(ProjectType == ce_project_type, 90, 60),
      end_window = endDate - fifelse(ProjectType == ce_project_type, 90, 60)
    ) %>%
    ftransform(
      lh_cls_in_start_window = InformationDate %between% list(start_window, startDate + 15),
      lh_cls_in_end_window = InformationDate %between% list(end_window, endDate + 15),
      lh_cls_during_period = InformationDate %between% list(start_window, endDate + 15),
      entry_in_start_window = EntryDate %between% list(start_window, startDate + 15),
      entry_in_end_window = EntryDate %between% list(end_window, endDate),
      lh_entry_during_period = EntryDate %between% list(start_window, endDate) & lh_prior_livingsituation
    )  %>%
    fselect(
      period, EnrollmentID, ProjectType, lh_prior_livingsituation,
      lh_cls_in_start_window,
      lh_cls_in_end_window,
      lh_cls_during_period,
      entry_in_start_window,
      entry_in_end_window,
      lh_entry_during_period,
      straddles_start, straddles_end, days_since_lookback, days_to_lookahead
    ) %>%
    fsubset(
      lh_cls_in_start_window |
        lh_cls_in_end_window |
        lh_cls_during_period |
        entry_in_start_window |
        entry_in_end_window |
        lh_entry_during_period
    )

  if(fnrow(lh_non_res) == 0 ) {
    logToConsole(session, "no non-res lh records")
    return(lh_non_res)
  }
  
  lh_non_res %>%
    # Group by EnrollmentID and calculate window flags
    fgroup_by(period, EnrollmentID) %>%
    fmutate(
      lh_cls_in_start_window = any(lh_cls_in_start_window),#anyv(lh_cls_in_start_window, TRUE),
      lh_cls_in_end_window = any(lh_cls_in_end_window),#anyv(lh_cls_in_end_window, TRUE),
      lh_cls_during_period =any(lh_cls_during_period)#anyv(lh_cls_during_period, TRUE)
    ) %>%
    fungroup()
}

## LH info for NbN enrollments--------------
lh_nbn_period <- function() {
  logToConsole(session, "in lh_nbn_period")
  lh_nbn <- expand_by_periods(session$userData$lh_nbn) %>%
    ftransform(
      nbn_in_start_window = DateProvided %between% list(startDate - 15, startDate + 15),
      nbn_in_end_window = DateProvided %between% list(endDate - 15, endDate + 15),
      nbn_during_period = DateProvided %between% list(startDate - 15, endDate + 15),
      entry_in_start_window = EntryDate %between% list(startDate - 15, startDate + 15),
      entry_in_end_window = EntryDate %between% list(endDate - 15, endDate),
      lh_entry_during_period = EntryDate %between% list(startDate - 15, endDate + 15)
    ) %>%
    fselect(
      period, EnrollmentID, ProjectType, 
      nbn_in_start_window,
      nbn_in_end_window,
      nbn_during_period,
      entry_in_start_window,
      entry_in_end_window,
      lh_entry_during_period,
      straddles_start, straddles_end, days_since_lookback, days_to_lookahead
    ) %>%
    fsubset(
      nbn_in_start_window |
      nbn_in_end_window |
      nbn_during_period |
      entry_in_start_window |
      entry_in_end_window |
      lh_entry_during_period
    )
  
  if(fnrow(lh_nbn) == 0) {
    logToConsole(session, "no NbN lh records")
    return(lh_nbn)
  }
  
  lh_nbn %>%
    fgroup_by(period, EnrollmentID) %>%
    fmutate(
      nbn_in_start_window = any(nbn_in_start_window),#anyv(nbn_in_start_window, TRUE),
      nbn_in_end_window = any(nbn_in_end_window),#anyv(nbn_in_end_window, TRUE),
      entry_in_start_window = entry_in_start_window,
      entry_in_end_window = entry_in_end_window,
      nbn_during_period = any(nbn_during_period)#anyv(nbn_during_period, TRUE)
    ) %>%
    fungroup()
}

## LH info for Other enrollments --------------
lh_other_period <- function(all_filtered) {
  logToConsole(session, "in lh_other_period")
  all_filtered %>%
    fsubset(
      ProjectType %in% lh_project_types_nonbn | 
      (ProjectType %in% ph_project_types & (is.na(MoveInDateAdjust) | MoveInDateAdjust >= startDate))
    ) %>%
    ftransform(
      entry_in_start_window = EntryDate %between% list(startDate, startDate + 15)
    ) %>%
    fselect(
      period, EnrollmentID, ProjectType, MoveInDateAdjust,
      straddles_start, straddles_end,
      entry_in_start_window,
      days_since_lookback,
      days_to_lookahead,
      startDate, endDate
    )
}

# Combine lh_infos and add to filtered universe dataset-------------------
add_lh_info <- function(all_filtered) {
  logToConsole(session, "in add_lh_info")

  lh_other_info <- lh_other_period(all_filtered) %>%
    fmutate(
      
        # For Res projects (lh_project_types 0,2,8 and ph_project_types 3,9,10,13)
        # must either straddle or otherwise be close to (i.e. 14 days from)
        # start so we can make claims about status at start
        # and must be within 14 days of previous enrollment, otherwise it would be an exit
      was_lh_at_start = (straddles_start | days_since_lookback %between% c(0, 14)) & (
        ProjectType %in% lh_project_types_nonbn | 
          (ProjectType %in% ph_project_types & (is.na(MoveInDateAdjust) | MoveInDateAdjust >= startDate))
      ),
      
      was_lh_at_end = (straddles_end | days_to_lookahead %between% c(0, 14)) & (
        ProjectType %in% lh_project_types_nonbn | 
          (ProjectType %in% ph_project_types & (is.na(MoveInDateAdjust) | MoveInDateAdjust >= endDate))
      )
    ) %>%
    fselect(
    EnrollmentID,
    period, 
    was_lh_at_start,
    was_lh_at_end)
  join(
    all_filtered, 
    lh_other_info, 
    on = c("EnrollmentID", "period"), 
    how = "left",
    suffix = c("", ".new")
  ) %>%
    fmutate(
      was_lh_at_start = fcoalesce(was_lh_at_start, fcoalesce(was_lh_at_start.new, FALSE)), 
      was_lh_at_end = fcoalesce(was_lh_at_end, fcoalesce(was_lh_at_end.new, FALSE))
    ) %>%
    fselect(-c(was_lh_at_start.new, was_lh_at_end.new))
}

# Period-Specific Enrollment Categories ----------------------------------------
# "expand" the dataset to get repeated rows per period (full + each month)
# then filter based on the period start and end
expand_by_periods <- function(dt) {
  all_periods <- data.table(
    period = names(session$userData$report_dates),
    startDate = as.Date(sapply(session$userData$report_dates, `[`, 1)),
    endDate = as.Date(sapply(session$userData$report_dates, `[`, 2))
  ) %>% 
    ftransform(
      exit_cutoff = startDate %m-% years(2),
      temp_key = 1
    )
  
  dt %>%
    ftransform(temp_key = 1) %>%
    join(
      all_periods,
      on = "temp_key",
      multiple = TRUE
    ) %>%
    fsubset(EntryDate <= endDate & ExitAdjust >= exit_cutoff) %>%
    fselect(-temp_key, -exit_cutoff) %>%
    setkey(period) %>%
    ftransform(
      straddles_start = EntryDate <= startDate & ExitAdjust >= startDate,
      straddles_end = EntryDate <= endDate & ExitAdjust >= endDate,
      in_date_range = EntryDate <= endDate & ExitAdjust >= startDate
    )
}


get_lh_non_res_esnbn_info <- function() {
  lh_non_res_esnbn_info <- rowbind(
    list(
      lh_non_res_period(),
      lh_nbn_period()
    ),
    fill = TRUE
  ) %>% 
    fmutate(
      was_lh_at_start = (
        # Non-Res and LH CLS in 60/90-day window OR 
        # Entry in 60/90 day window and lh_prior_livingsituation
        (straddles_start | days_since_lookback %between% c(0, 14)) & (
          (ProjectType %in% non_res_project_types & (
            lh_cls_in_start_window | (entry_in_start_window & lh_prior_livingsituation)
          )) |
          # ES NbN and Bed Night in 15-day window
          # we don't need lh_prior_livingsituation here 
          # because ES NbN enrollment implies homelessness
          (ProjectType == es_nbn_project_type & (
            nbn_in_start_window | entry_in_start_window
          ))
        )
      ),
      was_lh_during_period = (
        ProjectType == es_nbn_project_type & (
          nbn_during_period | lh_entry_during_period
        )
      ) | (
        ProjectType %in% non_res_project_types & (
          lh_cls_during_period |lh_entry_during_period
        )
      ),
      
      was_lh_at_end =
        (straddles_end | days_to_lookahead %between% c(0, 14)) & (
          (ProjectType %in% non_res_project_types & (
            lh_cls_in_end_window | (entry_in_end_window & lh_prior_livingsituation)
          )) |
          (ProjectType == es_nbn_project_type & (
            nbn_in_end_window | entry_in_end_window
          ))
        )
    ) %>%
    fselect(
      period,
      EnrollmentID,
      was_lh_at_start,
      was_lh_during_period,
      was_lh_at_end
    ) %>%
    funique()
}

get_period_specific_enrollment_categories <- reactive({
  logToConsole(session, "in get_period_specific_enrollment_categories")
  period_enrollments_filtered <- expand_by_periods(enrollments_filtered())
  
  # enrollment_categories_period <- period_enrollments_filtered %>%
    # ftransform(
      # DomesticViolenceCategory = fcase(
      #   DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1, "DVFleeing",
      #   DomesticViolenceSurvivor == 1, "DVNotFleeing",
      #   default = "NotDV"
      # )
    # )
  
  enrollment_categories_period <- period_enrollments_filtered %>% 
    join(
      get_lh_non_res_esnbn_info(),
      on = c("period","EnrollmentID"),
      how = "left"
    )
  
  logToConsole(session, paste0("In get_period_specific_enrollment_categories, num enrollment_categories_period: ", nrow(enrollment_categories_period)))
  
  if(nrow(enrollment_categories_period) == 0) return(enrollment_categories_period)
  
  # browser()
  enrollment_categories_period <- enrollment_categories_period %>%
    fmutate(
      straddle_ends_nonresnbn_not_lh_at_end = straddles_end & 
        ProjectType %in% c(es_nbn_project_type, non_res_project_types) & 
        !fcoalesce(was_lh_at_end)
    ) 
  
  enrollment_categories_period <- enrollment_categories_period %>%
    # Flag if person had any straddling enrollments
    # to be used when calculating eecr/lecr in no-straddle cases
    fgroup_by(period, PersonalID)
  
  enrollment_categories_period <- enrollment_categories_period %>%
    fmutate(
      any_straddle_start = any(straddles_start),#anyv(straddles_start, TRUE),
      any_straddle_end = any(straddles_end),#anyv(straddles_end, TRUE),
      all_straddle_ends_nonresnbn_not_lh_at_end = all(straddle_ends_nonresnbn_not_lh_at_end)#allv(straddle_ends_nonresnbn_not_lh_at_end, TRUE)
    ) %>%
    # flag the first and last straddling enrollments, 
    # by (desc) ProjectTypeWeight and EntryDate
    roworder(period, -ProjectTypeWeight, EntryDate) %>%
    fmutate(
      eecr_straddle = ffirst(
        fifelse(straddles_start, EnrollmentID, NA)
      ) == EnrollmentID,
      lecr_straddle = ffirst(
        fifelse(straddles_end, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    # flag the first and last enrollments in the report period,
    # for people that have no straddles,
    # by EntryDate and (desc) ProjectTypeWeight
    roworder(period, EntryDate, -ProjectTypeWeight) %>%
    fmutate(
      eecr_no_straddle = ffirst(
        fifelse(in_date_range & (
          !any_straddle_start
        ), EnrollmentID, NA)
      ) == EnrollmentID,
      # AS 5/9/25 TO DO: a non-straddling enrollment can be an lecr if no other enrollments straddle OR those that do are non-res/NbN that are !was_lh_at_end
      # If this works as we'd like/expect, there should be Outflow: Inactives for Annual (maybe for MbM)
      lecr_no_straddle = flast(
        fifelse(in_date_range & (
          !any_straddle_end |
            all_straddle_ends_nonresnbn_not_lh_at_end
        ), EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    fungroup()
  
  enrollment_categories_period <- enrollment_categories_period %>%
    # Create eecr and lecr flags
    fmutate(
      # AS 5/7/25: Added restriction that NbN and Non-Res projects must have
      # evidence of LH at some point during period (or within the window 
      # according to project type) in order to be considered an EECR
      # Alternatively, such NbN and Non-Res enrollments can have exited in the month.
      #
      # Here's an example to explain:
      #
      # EnrollmentID  EntryDate   ProjectType ExitAdjust  InformationDate
      # <char>        <Date>      <num>       <Date>      <Date>         
      # 825777        2021-09-24  4           2022-04-12  2021-09-24     
      #
      # This person should have the following statuses:
      # Full Report: ASH
      # Oct: ASH
      # Nov: ASH
      # Dec - Mar: NOT IN DATASET because no EECR
      # Apr: Inflow = Inactive, Outflow = Exited
      #
      # Normally, they wouldn't have an EECR in Apr because there's no evidence of LH during period
      # However, we don't want to never count them as outflow. So to fix that,
      # we'll force them to get an EECR because they exited.
      in_nbn_non_res = ProjectType %in% c(es_nbn_project_type, non_res_project_types),
      eecr = (eecr_straddle | eecr_no_straddle) & passes_enrollment_filters & (
        period == "Full" | 
        (period != "Full" & (
          (in_nbn_non_res & (
            was_lh_during_period | ExitAdjust %between% list(startDate, endDate)
          )) | 
          !in_nbn_non_res
        ))
      ),
      eecr = fcoalesce(eecr, FALSE),
      lecr = (lecr_straddle | lecr_no_straddle) & passes_enrollment_filters,
      lecr = fcoalesce(lecr, FALSE),
      in_nbn_non_res = NULL
    )
  
  enrollment_categories_period <- enrollment_categories_period %>%
    # based on 6/9 guidance from VL, we're excluding enrollments with no info, 
    # so we can't see if they were experiencing homelessness during the period
    fsubset(!(
      ProjectType %in% non_res_project_types &
        straddles_start &
        !was_lh_at_start &
        !was_lh_during_period
    ))
  
  enrollment_categories_period <- enrollment_categories_period %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(eecr_entry = fmax(fifelse(eecr, EntryDate, NA))) %>%
    fungroup() 
    
  enrollment_categories_period <- enrollment_categories_period %>%
    fmutate(
      # 5/15/25: exclude from lookbacks non-res enrollments that didn't exit 
      # and had no evidence of LH at period start
      is_lookback = !eecr & !lecr & EntryDate <= eecr_entry & !(ProjectType %in% non_res_project_types & is.na(ExitDate)),
      perm_dest = is_lookback & Destination %in% perm_livingsituation,
      nonperm_dest = is_lookback & !Destination %in% perm_livingsituation
    ) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      has_lecr = any(lecr),#anyv(lecr, TRUE),
      has_eecr = any(eecr),#anyv(eecr, TRUE),
      # To be Return/Re-Engaged, they need a lookback with an exit to the corresponding destination
      any_lookbacks_with_exit_to_perm = any(perm_dest),#anyv(perm_dest, TRUE),
      any_lookbacks_with_exit_to_nonperm = any(nonperm_dest)#anyv(nonperm_dest, TRUE)
    ) %>%
    fungroup()
  
  enrollment_categories_period <- enrollment_categories_period %>%
    fsubset(has_eecr == TRUE)
  
  # need to split here
  if(nrow(enrollment_categories_period) == 0) return(data.table())
  
  enrollment_categories_period <- enrollment_categories_period %>%
    # "fill in" lecr as TRUE where eecr is the only enrollment
    ftransform(lecr = lecr | (eecr & !has_lecr)) %>%
    roworder(period, PersonalID, EntryDate, ExitAdjust) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      first_lookback = flast(fifelse(is_lookback, EnrollmentID, NA)) == EnrollmentID,
      first_lookback_exit = fmax(fifelse(first_lookback, ExitAdjust, NA)),
      first_lookback_destination = fmax(fifelse(first_lookback, Destination, NA)),
      first_lookback_movein = fmax(fifelse(first_lookback, MoveInDateAdjust, NA))
    ) %>%
    fungroup() %>%
    fmutate(
      lookback_dest_perm = eecr & first_lookback_destination %in% perm_livingsituation,
      lookback_movein_before_start = eecr & first_lookback_movein < startDate,
      # Beginning with the first month's Outflow and ending after the last month's Inflow, 
      # there should be "continuous_at_start" and "continuous_at_end" flags that 
      # capture EECRs/LECRs that begin AFTER period start/end BEFORE period end, 
      # but days_to_lookahead/lookback <= 14. These would not be included on the chart.
      # so both flags do not apply to first month. Continuous_at_end also doesn't apply to last
      continuous_at_start = startDate > session$userData$ReportStart &
        eecr & EntryDate >= startDate & days_since_lookback %between% c(0, 14),
      continuous_at_end = startDate > session$userData$ReportStart & 
        endDate < session$userData$ReportEnd &
        lecr & ExitAdjust <= endDate & days_to_lookahead %between% c(0, 14)
    ) 
  
  logToConsole(session, paste0("About to subset to eecr, lecr, and lookbacks: num enrollment_categories_period records = ", nrow(enrollment_categories_period)))
  
  enrollment_categories_period %>%
    fsubset(eecr | lecr | first_lookback) %>%
    fselect(-c(any_straddle_start, any_straddle_end, eecr_no_straddle, eecr_straddle, lecr_straddle, lecr_no_straddle,
               first_lookback_exit, first_lookback_destination, first_lookback_movein
    ))
})
