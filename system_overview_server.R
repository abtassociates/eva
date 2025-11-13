# when user changes chart tabs
# hide demographic filters for Composition chart
# hide other stuff if valid file is not uploaded
# move chart download button to be inline with subtabs
observeEvent(input$syso_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  # Toggle Age, Vet Status, and Race filters based on System OVerview subtab (i.e. hide for System Demographics/Composition)
  shinyjs::runjs(str_glue("
    $('#syso_spec_pops, #syso_age, #syso_race_ethnicity')
      .closest('.bslib-grid-item')
      .toggle({ifelse(input$syso_tabbox != '<h4>System Demographics</h4>', 'true','false')});
  "))
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

observeEvent(
  list(
    input$syso_age,
    input$syso_race_ethnicity,
    input$syso_spec_pops,
    
    # Enrollment-level filters
    input$syso_hh_type,
    input$syso_level_of_detail,
    input$syso_project_type
  ),
  {
    num_rows <- nrow(period_specific_data()[["Full"]])
    
    num_people <- ifelse(
      num_rows > 0,
      fndistinct(period_specific_data()[["Full"]] %>% fselect(PersonalID)),
      0
    )

    shinyjs::toggle(
      "sys_inflow_outflow_download_btn", 
      condition = num_people > 10
    )
    shinyjs::toggle(
      "sys_inflow_outflow_download_btn_ppt", 
      condition = num_people > 10
    )
  }
)


# observeEvent(input$syso_level_of_detail, {
#   updatePickerInput(session, "syso_spec_pops",
#                     # label = "Special Populations",
#                     choices = syso_spec_pops_people)
# })

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
  
  empty_return <- list(Full = data.table(), Months = data.table())
  
  # Get filtered records based on user selection
  filtered_enrollments <- enrollments_filtered()
  filtered_clients <- client_categories_filtered() 
  
  # If either are empty, return empty
  if(nrow(filtered_clients) == 0 || nrow(filtered_enrollments) == 0) return(empty_return)
  
  # Apply all filters
  all_filtered <- filtered_enrollments %>% 
    join( 
      if(!IN_DEV_MODE) fselect(filtered_clients, PersonalID) else filtered_clients,
      on = "PersonalID",
      how = "inner"
    )
  
  if(IN_DEV_MODE) store_enrollment_categories_all_for_qc(all_filtered)

  period_data <- all_filtered %>% 
    expand_by_periods() %>% # expand/repeat enrollments across periods
    get_active_info(all_filtered) %>%
    get_inflows_and_outflows()
  
  if(nrow(period_data) > 0) {
    period_data <- period_data %>%
      remove_sequential_inactives()
  }
  
  exportTestValues(period_data = period_data)
  if(IN_DEV_MODE) {
    inflow_outflow_qc_checks(period_data)
    # browser()
    export_for_qc(period_data)
  }
  
  # Split into months and full-period datasets
  list(
    Full = fsubset(period_data, period == "Full"),
    Months = period_data %>%
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
 
  session$userData$client_categories %>%
    fsubset(
      AgeCategory %in% input$syso_age &
      (if(input$syso_race_ethnicity == "All") rep(TRUE, fnrow(session$userData$client_categories)) else get(input$syso_race_ethnicity) == 1) & 
      (
        input$syso_spec_pops == "None" | (
          input$syso_spec_pops == "Veteran" &
          VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")
        ) | (
          input$syso_spec_pops == "NonVeteran" &
          VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17")
        )
      )
    )
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
      (input$syso_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
      (input$syso_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
      (input$syso_project_type == "SO" & ProjectType == out_project_type) |
      (input$syso_project_type == "AllNonRes" & ProjectType %in% non_res_project_types)
      )
    ) %>%
    fselect(-VeteranStatus)
})

# Period-Specific Enrollment Categories ----------------------------------------
# "expand" the dataset to get repeated rows per period (full + each month)
# then filter based on the period start and end
expand_by_periods <- function(dt) {
  all_periods <- data.table(
    period = factor(names(session$userData$report_dates)),
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
    fselect(-temp_key, -exit_cutoff) %>%
    setkeyv(cols=c("PersonalID", "period", "EnrollmentID"))
}

get_active_info <- function(all_filtered_by_period, all_filtered) {
  lh_info_filtered <- session$userData$lh_info %>%
    fselect(-first_lh_date, -last_lh_date, -lh_prior_livingsituation) %>%
    join(
      all_filtered %>% fselect(EnrollmentID, EntryDate, ExitAdjust),
      on = "EnrollmentID",
      drop.dup.cols = "x",
      how = "inner",
      multiple = TRUE
    ) %>%
    frename(
      active_start = lh_date
    )

  entry_as_active <- all_filtered %>%
    fselect(PersonalID, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, days_lh_valid) %>%
    fmutate(active_start = EntryDate)
  
  exit_as_active <- all_filtered %>%
    fsubset(ProjectType %in% nbn_non_res & !Destination %in% other_livingsituation & !is.na(Destination)) %>%
    fselect(PersonalID, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, days_lh_valid) %>%
    fmutate(active_start = pmax(ExitAdjust - 15, EntryDate, na.rm=TRUE))
    
  lh_spans <- rbindlist(list(
    lh_info_filtered,
    entry_as_active,
    exit_as_active
  ), use.names=TRUE) %>%
    funique() %>%
    fsubset(active_start >= EntryDate) %>%
    fmutate(
      MoveInDateAdjust = fifelse(MoveInDateAdjust > ExitAdjust, NA, MoveInDateAdjust),
        
      active_end = fcase(
        ProjectType %in% lh_project_types_nonbn, ExitAdjust,
        ProjectType %in% ph_project_types, fcoalesce(MoveInDateAdjust, ExitAdjust),
        default = pmin(active_start + days_lh_valid, ExitAdjust, na.rm=TRUE)
      )
    ) 
  
  ph_housed_spans <- lh_info_filtered %>%
    fsubset(ProjectType %in% ph_project_types & !is.na(MoveInDateAdjust)) %>%
    fmutate(
      active_start = MoveInDateAdjust,
      active_end = ExitAdjust
    )
  
  active_info <-  rbindlist(list(
    lh_spans,
    ph_housed_spans
  )) %>%
    fselect(PersonalID, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, active_start, active_end) %>%
    fmutate(
      active_in_full_period = active_start <= session$userData$ReportEnd & active_end >= session$userData$ReportStart
    )
  
  all_filtered_w_first_last_active <- all_filtered_by_period %>%
    fselect(
      period, 
      PersonalID, 
      EnrollmentID, 
      EntryDate, ExitAdjust,
      Destination,
      startDate, endDate,
      ProjectTypeWeight
    ) %>% 
    join(
      active_info,
      on = "EnrollmentID", 
      multiple = TRUE,
      drop.dup.cols = "y"
    ) %>%
    fmutate(
      active_in_period = startDate <= active_end & endDate >= active_start,
      exited_in_period = ExitAdjust %between% list(startDate, endDate),
      period_of_activity = fifelse(active_in_period & (endDate != session$userData$ReportEnd | startDate != session$userData$ReportStart), startDate, NA)
    ) %>%
    fgroup_by(PersonalID, active_start) %>%
    fmutate(
      first_period_for_active = fmin(period_of_activity)
    ) %>%
    fungroup() %>%
    fmutate(
      first_active_date_in_period0 = fcase(
        startDate %between% list(active_start, active_end), startDate,
        active_start %between% list(startDate, endDate), active_start
        # ExitAdjust %between% list(startDate, endDate) & has_active_in_full_period, ExitAdjust # (this would make some cases re-engaged simply because they exited in that period)
      ),
      last_active_date_in_period0 = fcase(
        endDate %between% list(active_start, active_end), endDate,
        active_end %between% list(startDate, endDate), active_end
        # ExitAdjust %between% list(startDate, endDate) & has_active_in_full_period, ExitAdjust
      )
    ) %>%
    funique() %>%
    fgroup_by(PersonalID, period) %>%
    fmutate(
      first_active_date_in_period = fmin(first_active_date_in_period0),
      last_active_date_in_period = fmax(last_active_date_in_period0)
    ) %>%
    fungroup() %>%
    fselect(-first_active_date_in_period0, -last_active_date_in_period0, -period_of_activity)
  
  # get days between the earliest active date in the period and the most recent active before that
  prev_info <- all_filtered_w_first_last_active %>%
    fsubset(!is.na(first_active_date_in_period) & active_start < first_active_date_in_period) %>%
    fselect(PersonalID, period, EnrollmentID, ProjectType, active_start, active_end, ExitAdjust, Destination, first_active_date_in_period) %>%
    fmutate(
      prev_active = pmin(active_end, first_active_date_in_period, na.rm=TRUE),
      prev_exit = fifelse(ExitAdjust <= first_active_date_in_period, ExitAdjust, NA)
    ) %>%
    fgroup_by(PersonalID, period) %>%
    fmutate(
      prev_active = fmax(prev_active),
      prev_exit = fmax(prev_exit)
    ) %>%
    fungroup() %>%
    fmutate(
      prev_exit_dest_perm = ExitAdjust == prev_exit & Destination %in% perm_livingsituation
    ) %>%
    fgroup_by(PersonalID, period) %>%
    ftransform(prev_exit_dest_perm = any(prev_exit_dest_perm, na.rm=TRUE)) %>%
    fungroup() %>%
    fselect(
      PersonalID, 
      period,
      prev_active, prev_exit, prev_exit_dest_perm
    ) %>%
    funique()
  
  next_info <- all_filtered_w_first_last_active %>%
    fsubset(!is.na(last_active_date_in_period) & active_end > last_active_date_in_period) %>%
    fselect(PersonalID, period, active_start, active_end, last_active_date_in_period, exited_in_period, Destination) %>%
    fmutate(
      next_active = pmax(active_start, last_active_date_in_period, na.rm=TRUE)
    ) %>%
    fgroup_by(PersonalID, period) %>%
    fmutate(next_active = fmin(next_active)) %>%
    fungroup() %>%
    fselect(
      PersonalID, 
      period, 
      next_active
    ) %>%
    funique()

  prev_next_info <- join(
    prev_info,
    next_info,
    on = c("PersonalID","period"),
    how = "full"
  )

  # all_filtered_w_active1 [PersonalID == 203140, .(PersonalID, period, EnrollmentID, active_start, active_end, active_in_period, EntryDate, ExitAdjust, first_period_for_active)]
  all_filtered_w_active_info <- all_filtered_w_first_last_active %>%
    fsubset(active_in_period | (ExitAdjust >= startDate & active_in_full_period & startDate >= first_period_for_active)) %>% # exited_in_period doesn't work because they might have exited after; ExitAdjust >= startDate doesn't work because then we get earlier periods before the active span (10674);
    join(prev_next_info, verbose = F, multiple=TRUE) %>%
    fmutate(MoveInDateAdjust = fcoalesce(MoveInDateAdjust, as.Date(Inf)))
  
  return(all_filtered_w_active_info)
}

get_inflows_and_outflows <- function(all_filtered_w_active_info) {
  eecrs <- all_filtered_w_active_info %>%
    fmutate(
      straddles_start = startDate %between% list(active_start, active_end),
      sort_var = fifelse(straddles_start, ProjectTypeWeight, -as.numeric(active_start))
    ) %>%
    # New eecr
    roworder(PersonalID, period, -straddles_start, -sort_var, -ProjectTypeWeight, ExitAdjust, verbose = F) %>%
    fgroup_by(PersonalID, period) %>%
    fslice(how="first") %>%
    fmutate(
      prev_active = fcoalesce(prev_active, as.Date(-Inf)),
      prev_exit = fcoalesce(prev_exit, as.Date(-Inf)),
      prev_exit_dest_perm = fcoalesce(prev_exit_dest_perm, FALSE),
      first_active_date_in_period = fcoalesce(first_active_date_in_period, as.Date(Inf)),
      
      active_at_start = (
        startDate == first_active_date_in_period & (
          startDate == session$userData$ReportStart |
            (first_active_date_in_period - prev_active) %between% c(0, 14)
        ) | (
          startDate == session$userData$ReportStart &
            startDate <= (first_active_date_in_period + 14) &
            (first_active_date_in_period - prev_active) %between% c(0, 14)
        )
      )
    ) %>%
    fselect(
      PersonalID, period, startDate, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, 
      active_at_start, first_active_date_in_period, prev_active, prev_exit, prev_exit_dest_perm
    )
  
  lecrs <- all_filtered_w_active_info %>%
    fmutate(
      straddles_end = endDate %between% list(active_start, active_end),
      sort_var = fifelse(straddles_end, ProjectTypeWeight, as.numeric(fifelse(ExitAdjust >= startDate, ExitAdjust, active_end))),
      sort_var2 = fifelse(straddles_end, -1*as.numeric(MoveInDateAdjust), fifelse(ExitAdjust >= startDate, Destination/400, 0)) # Destination/400 to make it a double and comparable to as.numeric(MoveInDateadjust) which is a double
    ) %>%
    roworder(PersonalID, period, straddles_end, sort_var, sort_var2, ProjectTypeWeight, verbose = F, na.last = FALSE) %>%
    fgroup_by(PersonalID, period) %>%
    fslice(how="last") %>%
    fmutate(
      next_active = fcoalesce(next_active, as.Date(Inf)),
      last_active_date_in_period  = fcoalesce(last_active_date_in_period, as.Date(-Inf)),
      
      active_at_end = (
        endDate == last_active_date_in_period & (
          endDate == session$userData$ReportEnd |
            (next_active - last_active_date_in_period) %between% c(0, 14)
        ) | (
          endDate == session$userData$ReportEnd &
            endDate > last_active_date_in_period &
            (next_active - last_active_date_in_period) %between% c(0, 14)
        )
      )
    ) %>%
    fselect(
      PersonalID, period, endDate, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, Destination,
      active_at_end, last_active_date_in_period, next_active, exited_in_period
    )
  
  inflows <- eecrs %>%
    fmutate(
      ## INFLOW ## -------------------------------------
      ### Active at Start: Housed/Homeless -----------
      as_housed = active_at_start & MoveInDateAdjust < startDate,
      as_homeless = active_at_start & MoveInDateAdjust >= startDate,
      
      ### First-Time Homeless  ----------
      first_time_homeless = !active_at_start & 
        (first_active_date_in_period - pmax(prev_active, prev_exit)) > 730 &
        first_active_date_in_period != as.Date(Inf),
      
      ### Continuous at Start  ----------
      continuous_at_start = !active_at_start &
        startDate > session$userData$ReportStart & 
        startDate <= first_active_date_in_period + 14 &
        (first_active_date_in_period - prev_active) %between% c(0, 14),
      
      ### Returned / Reengaged ----------
      returned_or_reengaged = !active_at_start & !first_time_homeless & !continuous_at_start &
        (
          (first_active_date_in_period - prev_active) %between% c(15, 730) |
          ((first_active_date_in_period - prev_exit) %between% c(15, 730) & prev_exit >= prev_active)
        ),
      
      returned = returned_or_reengaged & prev_exit_dest_perm & prev_exit == pmax(prev_exit, prev_active),
      reengaged = returned_or_reengaged & !returned,
      
      
      ### First-of-month Exit  ----------
      first_of_month_exit = startDate > session$userData$ReportStart &
        ExitAdjust == startDate,
      
      ### Unknown  ----------
      unknown = first_active_date_in_period == as.Date(Inf) |
        (first_active_date_in_period - prev_exit) %between% c(0, 14),
      
      ### InflowTypeDetail  ----------
      InflowTypeDetail = factor(
        fcase( 
          as_housed, "Housed",
          as_homeless, "Homeless",
          first_time_homeless, "First-Time Homeless",
          returned, "Returned from Permanent",
          reengaged, "Re-engaged from Non-Permanent",
          continuous_at_start, "Continuous at Start",
          unknown, "Unknown",
          first_of_month_exit, "First-of-Month Exit",
          default = "something's wrong"
        ),
        levels = c(active_at_levels, inflow_detail_levels)
      ),
      
      
      ### InflowTypeSummary  ----------
      InflowTypeSummary = fct_collapse(
        InflowTypeDetail, 
        `Active at Start` = active_at_levels, 
        Inflow = inflow_chart_detail_levels
      )
    )
  
  outflows <- lecrs %>%
    fmutate(
      ## OUTFLOW ## ----------------------
      ### AE: Housed --------
      ae_housed = active_at_end & MoveInDateAdjust < endDate,
      
      ### AE: Homeless ----------
      ae_homeless = active_at_end & MoveInDateAdjust >= endDate,
      
      ### Exited, Non-Permanent ----------
      exited_system = exited_in_period & (next_active - last_active_date_in_period) > 14,
      exited_nonperm = exited_system & !Destination %in% perm_livingsituation,
      
      ### Exited, Permanent ----------
      exited_perm = exited_system & Destination %in% perm_livingsituation,
      
      ### Continuous at End ----------
      continuous_at_end = endDate < session$userData$ReportEnd &
        !active_at_end & (next_active - last_active_date_in_period) %between% c(0, 14),
      
      ### Last-of-month Entry ----------
      last_of_the_month_entry = 
        endDate < session$userData$ReportEnd &
        EntryDate == endDate,
      
      ### Inactive ----------
      inactive = ExitAdjust > endDate & (next_active - last_active_date_in_period) > 14 &
        ProjectType %in% nbn_non_res,
      
      ### OutflowTypeDetail  ----------
      OutflowTypeDetail = factor(
        fcase(
          # Active at End (AE): (ExitAdjust > endDate | (ExitAdjust == endDate & days_to_next_lh %between% c(0,14)))
          # S------------------------x---------------------E------x----------------
          # S------------------------x--------------y-------Ex---y------------------
          #   
          # Continuous at End:  ExitAdjust < endDate & days_to_next_lh %between% c(0, 14),
          # S------------------------x--------------------x-E---y------------------
          #   
          # System Exit: ExitAdjust <= endDate & days_to_next_lh > 14
          # S------------------------x--------------------x-E--------------y-------y
          # S------------------------x--------------------Ex---------------y-------y
          ae_housed, "Housed",
          ae_homeless, "Homeless",
          inactive, "Inactive",
          exited_nonperm, "Exited, Non-Permanent",
          exited_perm, "Exited, Permanent",
          continuous_at_end, "Continuous at End",
          last_of_the_month_entry, "Last-of-Month Entry",
          default = "something's wrong"
        ),
        levels = c(active_at_levels, outflow_detail_levels)
      ),

      ### OutflowTypeSummary----------
      OutflowTypeSummary = fct_collapse(
        OutflowTypeDetail,
        `Active at End` = active_at_levels, 
        Outflow = outflow_chart_detail_levels
      )
    )
  
  inflows_and_outflows <- join(
    inflows, 
    outflows,
    on=c("PersonalID","period"), 
    how = "inner",
    suffix = "_lecr"
  ) %>%
    fselect(
      PersonalID, 
      period, 
      InflowTypeDetail, OutflowTypeDetail, 
      InflowTypeSummary, OutflowTypeSummary, 
      EnrollmentID, EnrollmentID_lecr, 
      first_active_date_in_period, prev_active
    )
  
  if(!IN_DEV_MODE) {
    # only need these vars for QC checks
    inflows_and_outflows <- inflows_and_outflows %>%
      fselect(-first_active_date_in_period, -prev_active)
  }

  # inflows_and_outflows[PersonalID == 637203, .(PersonalID, period, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, InflowTypeDetail, OutflowTypeDetail)]
  return(inflows_and_outflows)
}

remove_sequential_inactives <- function(inflows_and_outflows) {
  # Only want the first of multiple inactives in a row.
  enrollments_to_remove <- inflows_and_outflows %>%
    fsubset(period != "Full", PersonalID, period, InflowTypeDetail, OutflowTypeDetail) %>%
    funique() %>%
    setorder(PersonalID, period) %>%
    fmutate(prev_period_outflow = flag(OutflowTypeDetail, g = PersonalID)) %>%
    fsubset(
      OutflowTypeDetail == "Inactive" & prev_period_outflow == "Inactive"
    ) %>%
    fselect(PersonalID, period)
  
  inflows_and_outflows_clean <- inflows_and_outflows %>%
    join(enrollments_to_remove, on = c("PersonalID", "period"), how="anti")

  return(inflows_and_outflows_clean)
}

export_for_qc <- function(inflows_and_outflows_clean) {
  path <- "/media/sdrive/projects/CE_Data_Toolkit/QC Datasets/new_vs_old_mbm_v6.xlsx"
  
  new <- inflows_and_outflows_clean %>% fselect(PersonalID, period, InflowTypeDetail, OutflowTypeDetail)
  old <- readRDS("/media/sdrive/projects/CE_Data_Toolkit/QC Datasets/mbm_10.21.25.rda") %>% 
    fselect(names(new)) %>% 
    funique() %>%
    fmutate(
      InflowTypeDetail = str_remove(InflowTypeDetail, "\n"),
      OutflowTypeDetail = str_remove(OutflowTypeDetail, "\n")
    )
  x <- join(
    new, 
    old, 
    column=TRUE, 
    on=c("PersonalID","period"), 
    how="full"
  ) %>% 
    join(enrollment_categories_all, multiple=TRUE) %>% 
    fmutate(
      diff = fifelse(InflowTypeDetail != InflowTypeDetail_old | OutflowTypeDetail != OutflowTypeDetail_old, TRUE, NA),
      unknown_reengaged_diff = InflowTypeDetail == "Unknown" & InflowTypeDetail_old == "Re-engaged from Non-Permanent"
    ) %>% 
    fgroup_by(PersonalID) %>% 
    fmutate(
      has_unknown_reengaged_diff = any(unknown_reengaged_diff, na.rm=TRUE),
      has_other_diff = any(diff & !unknown_reengaged_diff, na.rm=TRUE)
    ) %>% 
    fungroup() %>% 
    fsubset(
      has_unknown_reengaged_diff | has_other_diff, 
      PersonalID, period, InflowTypeDetail, OutflowTypeDetail, InflowTypeDetail_old, OutflowTypeDetail_old, 
      has_unknown_reengaged_diff, has_other_diff, diff, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, Destination, lh_dates
    ) 
  
  # Approved diffs
  approved_inflow_outflows <- fread("/media/sdrive/projects/CE_Data_Toolkit/QC Datasets/approved_inflow_outflows.csv")
  x <- x %>%
    join(approved_inflow_outflows, on = c("PersonalID", "period")) %>%
    fmutate(new_status = InflowTypeDetail != InflowTypeDetail_approved_inflow_outflows | OutflowTypeDetail != OutflowTypeDetail_approved_inflow_outflows) %>%
    fsubset(!PersonalID %in% approved_inflow_outflows$PersonalID | new_status) %>%
    fselect(-InflowTypeDetail_approved_inflow_outflows, -OutflowTypeDetail_approved_inflow_outflows)
  
  message(paste0("Total diff: ",fndistinct(x$PersonalID)))
  message(paste0("Other diff: ",fndistinct(x %>% fsubset(has_other_diff) %>% fselect(PersonalID))))

# # Print as a nice table
# get_counts <- function(dt, dataset_name) {
#   rbindlist(list(
#     dt[, .(FlowType = "Inflow", Category = InflowTypeDetail, Count = uniqueN(PersonalID)), by = InflowTypeDetail],
#     dt[, .(FlowType = "Outflow", Category = OutflowTypeDetail, Count = uniqueN(PersonalID)), by = OutflowTypeDetail]
#   ))[, Dataset := dataset_name]
# }
# 
# get_counts_by_period <- function(dt, dataset_name) {
#   rbindlist(list(
#     dt[, .(FlowType = "Inflow", Category = InflowTypeDetail, Count = uniqueN(PersonalID)), by = .(period, InflowTypeDetail)],
#     dt[, .(FlowType = "Outflow", Category = OutflowTypeDetail, Count = uniqueN(PersonalID)), by = .(period, OutflowTypeDetail)]
#   ))[, Dataset := dataset_name]
# }
# 
# recent <- fread("/media/sdrive/projects/CE_Data_Toolkit/QC Datasets/full_inflow_outflows_mbm_simplified_newest_10.30.25.csv") %>%
#   fselect(PersonalID, period, InflowTypeDetail, OutflowTypeDetail)
# # Get counts for all datasets
# all_counts_by_period <- rbindlist(list(
#   get_counts_by_period(old, "Old"),
#   get_counts_by_period(recent, "Recent"),
#   get_counts_by_period(new, "Active Destination+Exit")
# ))
# wide_table_by_period <- dcast(all_counts_by_period, FlowType + Category + period ~ Dataset, value.var = "Count", fill = 0) %>%
#   colorder(FlowType, Category, Old, Recent, `Active Destination+Exit`)
# 
# 
# all_counts <- rbindlist(list(
#   get_counts(old, "Old"),
#   get_counts(recent, "Recent"),
#   get_counts(new, "Active Destination+Exit")
# ))
# 
# wide_table <- dcast(all_counts, FlowType + Category ~ Dataset, value.var = "Count", fill = 0) %>%
#   colorder(FlowType, Category, Old, Recent, `Active Destination+Exit`)
# 
# print(wide_table)
# browser()

  # update without removing (writes over existing data)
  # wb <- openxlsx2::wb_load(path)
  # unknown_reengaged <- x %>% fsubset(has_unknown_reengaged_diff, -has_other_diff, -has_unknown_reengaged_diff) %>% roworder(PersonalID)
  # wb$add_data(sheet = "Unknown-Reengaged", x = unknown_reengaged, start_col = 1, start_row = 1, na.strings="")
  # 
  # other_diffs <- x %>% fsubset(has_other_diff, -has_unknown_reengaged_diff, -has_other_diff) %>% roworder(PersonalID)
  # wb$add_data(sheet = "Other diffs", x = other_diffs, start_col = 1, start_row = 1, na.strings="")
  # 
  # raw_data <- enrollment_categories_all %>% fsubset(PersonalID %in% funique(x$PersonalID)) %>% roworder(PersonalID, EntryDate)
  # wb$add_data(sheet = "Raw data", x = raw_data, start_col = 1, start_row = 1, na.strings="")
  # 
  # # Save the workbook
  # wb$save(path)
}

store_enrollment_categories_all_for_qc <- function(all_filtered) {
  # Get an enrollment-level dataset with all enrollments and LH dates, for QC purposes
  # Get all InformationDates for a given enrollment in one cell
  lh_agg <- if(nrow(session$userData$lh_info) > 0) {
    session$userData$lh_info %>% 
      fgroup_by(EnrollmentID) %>% 
      fsummarise(lh_dates = paste(lh_date, collapse = ",")) %>% 
      fungroup() %>%
      fsubset(lh_dates != "NA")
  } else data.table(EnrollmentID = NA, lh_dates = NA)
  
  enrollment_categories_all <<- all_filtered %>%
    join(lh_agg, on = "EnrollmentID") %>%
    fselect(c(enrollment_cols, "Destination", "lh_dates")) %>%
    funique()
}


inflow_outflow_qc_checks <- function(inflows_and_outflows_clean) {
  
  ## Inflow Unknown in Full Period -------
  bad_records <- inflows_and_outflows_clean %>%
    fsubset(InflowTypeDetail == "Unknown" & period == "Full")
  if(nrow(bad_records) > 0) {
    logToConsole(session, "ERROR: There's an Inflow-Unknown in the Full Annual data")
    if(IN_DEV_MODE & !isTRUE(getOption("shiny.testmode"))) {
      bad_records <- get_all_enrollments_for_debugging(bad_records, inflows_and_outflows_clean) %>% 
        fselect(inflow_debug_cols)
      view(bad_records)
      browser()
    }
  }
  
  ## Something's Wrong -------
  bad_records <- inflows_and_outflows_clean %>%
    fsubset(
      InflowTypeSummary == "something's wrong" | 
        OutflowTypeSummary == "something's wrong"
    )
  if(nrow(bad_records) > 0) {
    logToConsole(session, "ERROR: There are clients whose Inflow or Outflow is 'something's wrong'")
    if(IN_DEV_MODE & !isTRUE(getOption("shiny.testmode"))) {
      somethings_wrongs <- get_all_enrollments_for_debugging(bad_records, inflows_and_outflows_clean, multiple=TRUE) %>%
        fgroup_by(PersonalID) %>%
        fmutate(
          has_inflow_wrong = anyv(InflowTypeDetail, "something's wrong"),
          has_outflow_wrong = anyv(OutflowTypeDetail, "something's wrong"),
          has_continuous_at_start = anyv(InflowTypeDetail, "Continuous at Start"),
          has_continuous_at_end = anyv(OutflowTypeDetail, "Continuous at End")
        ) %>%
        fungroup()
      
      if(nrow(somethings_wrongs[has_inflow_wrong == TRUE]) > 0) view(somethings_wrongs[has_inflow_wrong == TRUE] %>% fselect(inflow_debug_cols, "has_continuous_at_start"))
      if(nrow(somethings_wrongs[has_outflow_wrong == TRUE]) > 0) view(somethings_wrongs[has_outflow_wrong == TRUE] %>% fselect(outflow_debug_cols, "has_continuous_at_end"))
      browser()
    }
    # e.g. PersonalID 623725 in Nov and 601540 in Dec
    # e.g. PersonalID 305204 and 420232 in Nov and 601540 and 620079 in Dec
    # e.g. PersonalID 14780 in Oct and Nov
    # 613426 - in Nov, they should be Active at start Homeless but the problem is that the lookback has no exit or destination
    # If we restrict Return/Re-Engaged to those with lookbacks with Exits to corresponding destination, then:
    #   PersonalIDs: 306663, 619032, 119222, 11943    
    # AS 5/12/25: With new was_lh_at_end condition in creating lecr, PersonalID 305204 (ICF-good) is "something's wrong" for annual
    #
    # PersonalID 687862 has inflow issue
    # PersonalID 688880, DEMO mode, Jan 22, Outflow
    # PersonalID 690120, DEMO mode, Apr 22, Outflow
    
    logToConsole(session, "ERROR: There are something's wrong records in the universe_ppl_flags data")
  }
  
  ## First/Last Month Inflow/Outflow != Full Inflow/Outflow-------
  bad_records_enrollid <- inflows_and_outflows_clean %>%
    fgroup_by(PersonalID) %>%
    fsummarize(
      first_enrl_month_inflow = ffirst(fifelse(period != "Full", EnrollmentID, NA)),
      full_period_inflow = ffirst(fifelse(period == "Full", EnrollmentID, NA)),
      
      last_enrl_month_outflow = flast(fifelse(period != "Full", EnrollmentID_lecr, NA)),
      last_enrl_month_outflow_noninactive = flast(fifelse(period != "Full" & OutflowTypeDetail != "Inactive", EnrollmentID_lecr, NA)),
      full_period_outflow = flast(fifelse(period == "Full", EnrollmentID_lecr, NA)),
      full_period_outflow_status = flast(fifelse(period == "Full", OutflowTypeDetail, NA))
    ) %>%
    fungroup() %>%
    fsubset(
      first_enrl_month_inflow != full_period_inflow |
        (last_enrl_month_outflow != full_period_outflow & full_period_outflow_status == "Inactive") |
        (last_enrl_month_outflow_noninactive != full_period_outflow & full_period_outflow_status != "Inactive")
    ) %>%
    fmutate(
      disc = fcase(
        first_enrl_month_inflow != full_period_inflow & (
          (last_enrl_month_outflow != full_period_outflow & full_period_outflow_status == "Inactive") |
            (last_enrl_month_outflow_noninactive != full_period_outflow & full_period_outflow_status != "Inactive")
          ), "both",
        first_enrl_month_inflow != full_period_inflow, "inflow",
        (last_enrl_month_outflow != full_period_outflow & full_period_outflow_status == "Inactive") |
          (last_enrl_month_outflow_noninactive != full_period_outflow & full_period_outflow_status != "Inactive"), "outflow"
        
      )
    )

  if(nrow(bad_records) > 0)  {
    logToConsole(session, "ERROR: There are clients whose first-month Inflow != Full Period Inflow and/or last-month Outflow != Full Period outflow")
    if(IN_DEV_MODE & !isTRUE(getOption("shiny.testmode"))) {
      bad_first_inflow_records <- get_all_enrollments_for_debugging(
        bad_records[first_enrl_month_inflow != full_period_inflow],
        inflows_and_outflows_clean,
        multiple = TRUE
      )
      if(nrow(bad_first_inflow_records) > 0) {
        bad_first_inflow_records <- bad_first_inflow_records %>%
          fgroup_by(PersonalID) %>%
          fmutate(
            has_something_wrong = anyv(InflowTypeDetail, "something's wrong") | 
              anyv(OutflowTypeDetail, "something's wrong"),
            has_continuous_at_start = anyv(InflowTypeDetail, "Continuous at Start")
          ) %>%
          fungroup() %>%
          fsubset(!has_something_wrong)
        
        if(nrow(bad_first_inflow_records) > 0) {
          view(bad_first_inflow_records %>% fselect(c(inflow_debug_cols, "has_continuous_at_start")))
          browser()
        }
      }
      
      bad_last_outflow_records <- get_all_enrollments_for_debugging(
        bad_records[
          (last_enrl_month_outflow != full_period_outflow & full_period_outflow == "Inactive") |
            (last_enrl_month_outflow_noninactive != full_period_outflow & full_period_outflow != "Inactive")
        ],
        inflows_and_outflows_clean,
        multiple = TRUE
      )
      if(nrow(bad_last_outflow_records) > 0) {
        bad_last_outflow_records <- bad_last_outflow_records %>%
          fgroup_by(PersonalID) %>%
          fmutate(
            has_something_wrong = anyv(InflowTypeDetail, "something's wrong") | 
              anyv(OutflowTypeDetail, "something's wrong"),
            has_continuous_at_end = anyv(OutflowTypeDetail, "Continuous at End")
          ) %>%
          fungroup() %>%
          fsubset(!has_something_wrong)
        
        if(nrow(bad_last_outflow_records) > 0) {
          view(bad_last_outflow_records %>% fselect(c(outflow_debug_cols, "has_continuous_at_end")))
          browser()
        }
      }
      # inflows_and_outflows_clean[PersonalID == 565354, .(PersonalID, period, EnrollmentID, ProjectType, EntryDate, ExitAdjust, InflowTypeDetail, OutflowTypeDetail)]
    }
  }
  
  ## ASHomeless and EntryDate on first of month with no recent days_since_last_lh -------
  bad_records <- inflows_and_outflows_clean %>%
    join(enrollment_categories_all, on="PersonalID", how="left", multiple=TRUE) %>%
    fsubset(period != "Full") %>%
    fsubset(
      InflowTypeDetail == "Homeless" & 
        EntryDate == as.Date(period) &
        EntryDate != session$userData$ReportStart &
        (first_active_date_in_period - prev_active) > 14
    )
  if(nrow(bad_records) > 0) {
    if(IN_DEV_MODE & !isTRUE(getOption("shiny.testmode"))) {
      bad_ashomeless <- get_all_enrollments_for_debugging(
        bad_records,
        inflows_and_outflows_clean,
        multiple = TRUE
      )
      view(bad_ashomeless)
      browser()
    }
  }
  
  ## Re-Engaged/Return after Non-Exit ---
  bad_records <- inflows_and_outflows_clean %>%
    fsubset(
      period != "Full", 
      PersonalID, period, InflowTypeDetail, OutflowTypeDetail
    ) %>%
    funique(cols=c("PersonalID", "period", "InflowTypeDetail", "OutflowTypeDetail")) %>%
    setorder(PersonalID, period) %>%
    fmutate(
      inflow_flag = grepl("Return|Re-engaged", InflowTypeDetail),
      prev_outflow = flag(OutflowTypeDetail, g=PersonalID), 
      prev_outflow_flag = !grepl("Exited|Inactive", prev_outflow) & !is.na(prev_outflow)
    ) %>%
    fgroup_by(PersonalID) %>%
    fsummarize(has_issue = any(inflow_flag & prev_outflow_flag, na.rm=TRUE)) %>%
    fungroup() %>%
    fsubset(has_issue)
  if(nrow(bad_records) > 0) {
    if(IN_DEV_MODE) {
      bad_return_after_nonexit <- get_all_enrollments_for_debugging(
        bad_records,
        inflows_and_outflows_clean,
        multiple = TRUE
      ) %>%
        fselect(
          PersonalID, period, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, lh_dates, InflowTypeDetail, OutflowTypeDetail
        )
      if(nrow(bad_return_after_nonexit) > 0) {
        view(bad_return_after_nonexit)
        browser()
      }
      
    }
  }
  
  ## Non-Re-Engaged/Return after Exit ---
  bad_records <- inflows_and_outflows_clean %>%
    fsubset(
      period != "Full", 
      PersonalID, period, InflowTypeDetail, OutflowTypeDetail
    ) %>%
    funique(cols=c("PersonalID", "period", "InflowTypeDetail", "OutflowTypeDetail")) %>%
    setorder(PersonalID, period) %>%
    fmutate(
      inflow_flag = !grepl("Return|Re-engaged|Unknown|First-Time Homeless|Continuous at Start", InflowTypeDetail),
      prev_outflow = flag(OutflowTypeDetail, g=PersonalID), 
      prev_outflow_flag = grepl("Exited|Inactive", prev_outflow) & !is.na(prev_outflow)
    ) %>%
    fgroup_by(PersonalID) %>%
    fsummarize(has_issue = any(inflow_flag & prev_outflow_flag, na.rm=TRUE)) %>%
    fungroup() %>%
    fsubset(has_issue)
  if(nrow(bad_records) > 0) {
    if(IN_DEV_MODE) {
      bad_nonreturn_after_exit <- get_all_enrollments_for_debugging(
        bad_records,
        inflows_and_outflows_clean,
        multiple = TRUE
      ) %>%
        fselect(
          PersonalID, period, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, lh_dates, InflowTypeDetail, OutflowTypeDetail
        )
      if(nrow(bad_nonreturn_after_exit) > 0) {
        view(bad_nonreturn_after_exit)
        browser()
      }
      
    }
  }
  
}