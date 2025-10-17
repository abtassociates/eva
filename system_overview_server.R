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
      fndistinct(period_specific_data()[["Full"]] %>% fsubset(InflowTypeDetail !=" Excluded", PersonalID)),
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

get_lookbacks <- function(all_filtered) {
  # Calculate days_since_lookback, days_to_lookahead, and other lookback info
  # First, determine days_to_lookahead
  dt <- all_filtered %>%
    setkey(PersonalID, EntryDate, ExitAdjust)

  dt_starts <- dt[, .(
    PersonalID, 
    EnrollmentID, 
    EntryDate, 
    Date = EntryDate, 
    Type = "start"
  )]
  dt_ends <- dt[, .(
    PersonalID, 
    EnrollmentID,  
    Destination, 
    MoveInDateAdjust, 
    ProjectType, 
    ExitAdjust, 
    Date = ExitAdjust, 
    Type = "end"
  )]
  setkey(dt_ends, PersonalID, Date)
  setkey(dt_starts, PersonalID, Date)
  
  # Forward-looking rolling join to find the most recent end date before each start date
  # For each end date (ExitAdjust) in dt_ends, it finds the next start date (EntryDate) in dt_starts
  #   The roll = -Inf means "roll forward" - find the nearest future match
  lookback_info <- dt_ends[dt_starts, roll = TRUE][, .(
    PersonalID,
    EnrollmentID = i.EnrollmentID,
    days_since_lookback = EntryDate - ExitAdjust, # ExitDate is the lookup's ExitDate
    lookback_des = Destination,
    lookback_ptype = ProjectType,
    lookback_enrollment_id = EnrollmentID,
    lookback_dest_perm = Destination %in% perm_livingsituation,
    lookback_movein = MoveInDateAdjust,
    lookback_is_nonres_or_nbn = ProjectType %in% nbn_non_res
  )]
  
  lookahead_info <- dt_starts[dt_ends, roll = -Inf][, .(
    PersonalID,
    EnrollmentID = i.EnrollmentID,
    days_to_lookahead = EntryDate - ExitAdjust # EntryDate is the lookup's EntryDate
  )]
    
  return(
    dt %>%
      join(lookback_info, on = c("PersonalID", "EnrollmentID")) %>%
      join(lookahead_info, on = c("PersonalID", "EnrollmentID")) # %>%
      # join(lookahead_lh_info, on = c("PersonalID", "EnrollmentID"))
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
    ) %>%
    get_lookbacks()
  
  if(IN_DEV_MODE) store_enrollment_categories_all_for_qc(all_filtered)

  period_data <- all_filtered %>% 
    expand_by_periods() %>% # expand/repeat enrollments across periods
    get_ppl_flags()

  inflow_outflow_qc_checks(period_data)
  
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
    fsubset(EntryDate <= endDate & ExitAdjust >= exit_cutoff) %>%
    fselect(-temp_key, -exit_cutoff) %>%
    setkey(period) %>%
    ftransform(
      straddles_start = EntryDate <= startDate & ExitAdjust >= startDate,
      straddles_end = EntryDate <= endDate & ExitAdjust >= endDate,
      in_date_range = EntryDate <= endDate & ExitAdjust >= startDate
    )
}

get_ppl_flags <- function(all_filtered) {
  
  lh_info_filtered <- session$userData$lh_info %>%
    fselect(-first_lh_date) %>%
    join(
      all_filtered %>% fselect(EnrollmentID, EntryDate, ExitAdjust),
      on = "EnrollmentID",
      drop.dup.cols = "x",
      how = "inner"
    ) 
  
  entry_as_lh <- lh_info_filtered %>%
    funique(cols = "EnrollmentID") %>%
    fmutate(lh_date = EntryDate) %>%
    fsubset(lh_date >= EntryDate)
  
  lh_info <- rbindlist(list(
    lh_info_filtered,
    entry_as_lh
  )) %>%
    funique() %>%
    fsubset(!is.na(lh_date) & lh_date >= EntryDate) %>%
    roworder(PersonalID, lh_date, -ProjectTypeWeight) %>%
    fmutate(
      lh_start = lh_date,
      lh_end = pmin(lh_date + days_lh_valid, ExitAdjust, na.rm=TRUE)
    ) %>%
    fgroup_by(PersonalID) %>%
    fmutate(
      prev_lh_end = fifelse(lh_start <= L(lh_start), NA, fifelse(L(lh_end) > lh_start, lh_start, L(lh_end))),
      next_lh_start = fifelse(lh_end >= L(lh_end, -1), NA, fifelse(L(lh_start, -1) < lh_end, lh_end, L(lh_start, -1)))
    ) %>%
    fungroup() %>%
    fmutate(
      days_since_last_lh = fcoalesce(lh_start - prev_lh_end, as.difftime(Inf, units="days")),
      days_to_next_lh = fcoalesce(next_lh_start - lh_end, as.difftime(Inf, units="days"))
    ) %>%
    fselect(-lh_date)

  all_filtered_w_lh_info <- all_filtered %>%
    expand_by_periods() %>% 
    fselect(
      period, 
      PersonalID, 
      EnrollmentID, 
      EntryDate, ExitAdjust,
      Destination,
      startDate, endDate,
      in_date_range,
      straddles_start, straddles_end,
      lookback_dest_perm, lookback_movein, lookback_is_nonres_or_nbn, days_since_lookback,
      ProjectTypeWeight
    ) %>% 
    join(
      lh_info,
      on = "EnrollmentID", 
      multiple = TRUE,
      drop.dup.cols = "y"
    ) %>%
    fsubset(
      # was LH in full period
      last_lh_date >= session$userData$ReportStart |
       
      # Exited at some point in the future 
      (
        ExitAdjust %between% list(startDate, session$userData$ReportEnd) | 
        (endDate == session$userData$ReportEnd & ExitAdjust > endDate)
      ) |
      
      # was housed during period
      ProjectType %in% ph_project_types & 
        in_date_range & 
        MoveInDateAdjust <= endDate
    )
  all_filtered_w_lh_info <<- all_filtered_w_lh_info
  
  all_filtered_w_inflow_outflow <- all_filtered_w_lh_info %>%
    fmutate(
      MoveInDateAdjust = fcoalesce(MoveInDateAdjust, as.Date(Inf)),
      # days_since_lookback = fcoalesce(days_since_lookback, as.difftime(Inf, units="days")),
      
      ## INFLOW ## -------------------------------------
      ### AS: Housed  ----------
      as_housed = ProjectType %in% ph_project_types &
        MoveInDateAdjust < startDate & startDate <= ExitAdjust,
      
      ### AS: Homeless  ----------
      as_homeless = (
        startDate == session$userData$ReportStart & (
          startDate %between% list(lh_start, lh_end) |
          (startDate %between% list(lh_start - 14, lh_start) & days_since_last_lh %between% c(0,14) & prev_lh_end < startDate)
        )
      ) | (
        startDate > session$userData$ReportStart & (
          startDate %between% list(lh_start + 1, lh_end) |
          (startDate == lh_start & days_since_last_lh %between% c(0, 14) & prev_lh_end < startDate)
        )
      ),
      
      ### First-Time Homeless  ----------
      # in other cases, we have long non-res enrollments and then an LKH CLS pops up and they're re-engaged.
      # that's because there's a lookback or they were already categorized in a previous month
      first_time_homeless = days_since_last_lh > 730 & 
        EntryDate >= startDate &
        EntryDate > session$userData$ReportStart,
      
      ### Returned  ----------
      returned = lookback_dest_perm &
        (startDate == session$userData$ReportStart | ExitAdjust != startDate) & (
          days_since_last_lh %between% c(15, 730) |
          (days_since_lookback %between% c(0, 14) & lookback_is_nonres_or_nbn & days_since_last_lh == Inf)
        ),
      
      ### Re-Engaged  ----------
      reengaged = (startDate == session$userData$ReportStart | ExitAdjust != startDate) & (
        (days_since_last_lh %between% c(15, 730) & !lookback_dest_perm & startDate <= lh_start) |
        # (ProjectType %in% nbn_non_res & days_since_last_lh == Inf & startDate %between% list(fcoalesce(prev_lh_end, EntryDate), lh_start)) |
        (
          ProjectType %in% nbn_non_res &
            straddles_start & 
            ExitAdjust %between% list(startDate, endDate)
        )
      ),
      
      ### Continuous at Start  ----------
      continuous_at_start = startDate > session$userData$ReportStart &
        lh_start > startDate & days_since_last_lh %between% c(0, 14),
      
      ### Unknown  ----------
      unknown = straddles_start & 
        ProjectType %in% nbn_non_res,
      
      ### First-of-month Exit  ----------
      first_of_month_exit = startDate > session$userData$ReportStart &
        ExitAdjust == startDate,
      
      ### Excluded  ----------
      # Exclude non-res-only clients with incomplete or conflicting LH data
      excluded = ProjectType %in% nbn_non_res &
        days_since_lookback %between% c(0, 14) &
        is.na(days_since_last_lh),
      
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
          excluded, "Excluded",
          default = "something's wrong"
        ),
        levels = c(active_at_levels, inflow_detail_levels)
      ),
      
      ## OUTFLOW ## ----------------------
      ### AE: Housed --------
      ae_housed = ProjectType %in% ph_project_types &
        MoveInDateAdjust < endDate & (endDate < ExitAdjust | (ExitAdjust == endDate & days_to_next_lh %between% c(0,14))),
      
      ### AE: Homeless ----------
      ae_homeless = (
        endDate == session$userData$ReportEnd & (
          endDate %between% list(lh_start, lh_end) |
          (endDate %between% list(lh_end, lh_end + 14) & days_to_next_lh %between% c(0,14) & next_lh_start > endDate)
        )
      ) | (
        endDate < session$userData$ReportEnd & (
          endDate %between% list(lh_start, lh_end - 1) |
          (endDate == lh_end & ((days_to_next_lh %between% c(0, 14) & next_lh_start > endDate) | (MoveInDateAdjust >= endDate & MoveInDateAdjust != Inf)))
        )
      ),
      
      exited_system = ExitAdjust %between% list(startDate, endDate) & (
        (days_to_next_lh > 14 & (next_lh_start >= ExitAdjust | is.na(next_lh_start))) | next_lh_start < startDate | next_lh_start < ExitAdjust
      ),
      
      ### Exited, Non-Permanent ----------
      exited_nonperm = !Destination %in% perm_livingsituation & exited_system,
      
      ### Exited, Permanent ----------
      exited_perm = Destination %in% perm_livingsituation & exited_system,
      
      ### Continuous at End ----------
      continuous_at_end = endDate < session$userData$ReportEnd &
        ExitAdjust < endDate & days_to_next_lh %between% c(0, 14),
      
      ### Last-of-month Entry ----------
      last_of_the_month_entry = 
        endDate < session$userData$ReportEnd &
        EntryDate == endDate,
      
      ### Inactive ----------
      inactive = straddles_end & 
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
          exited_nonperm, "Exited, Non-Permanent",
          exited_perm, "Exited, Permanent",
          continuous_at_end, "Continuous at End",
          last_of_the_month_entry, "Last-of-Month Entry",
          inactive, "Inactive",
          default = "something's wrong"
        ),
        levels = c(active_at_levels, outflow_detail_levels)
      )
    ) %>%
    fsubset(in_date_range) %>%
    funique()
  
  universe_w_ppl_flags <- all_filtered_w_inflow_outflow %>%
    fgroup_by(PersonalID, period) %>%
    fmutate(
      lecr = ExitAdjust == fmax(fifelse(EntryDate < endDate, ExitAdjust, NA)),
      eecr = EntryDate == fmin(fifelse(ExitAdjust > startDate, EntryDate, NA)),
      all_exits = all(OutflowTypeDetail %in% c("Exited, Non-Permanent", "Exited, Permanent", "something's wrong")),
      all_returns = all(InflowTypeDetail %in% c("Returned from Permanent", "Re-engaged from Non-Permanent", "something's wrong"))
    ) %>%
    fungroup() %>%
    fmutate(
      eecr_inflow = fifelse(all_returns, fifelse(eecr, InflowTypeDetail, NA), InflowTypeDetail),
      lecr_outflow = fifelse(all_exits, fifelse(lecr, OutflowTypeDetail, NA), OutflowTypeDetail)
    ) %>%
    fgroup_by(PersonalID, period) %>%
    fmutate(
      InflowTypeDetail =  fmin(eecr_inflow),
      OutflowTypeDetail = fmin(lecr_outflow)
    ) %>%
    fungroup() %>%
    fmutate(
      ### InflowTypeSummary  ----------
      InflowTypeSummary = fct_collapse(
        InflowTypeDetail, 
        `Active at Start` = active_at_levels, 
        Inflow = inflow_chart_detail_levels
      ),
      
      ### OutflowTypeSummary----------
      OutflowTypeSummary = fct_collapse(
        OutflowTypeDetail,
        `Active at End` = active_at_levels, 
        Outflow = outflow_chart_detail_levels
      )
    )
  
  
  if(!IN_DEV_MODE) {
    universe_w_ppl_flags <- universe_w_ppl_flags %>%
      fselect(
        PersonalID,
        InflowTypeSummary,
        InflowTypeDetail,
        OutflowTypeSummary,
        OutflowTypeDetail,
        ProjectType,
        period,
        EnrollmentID,
        EntryDate,
        days_since_lookback,
        days_since_last_lh,
        days_to_next_lh,
        straddles_start,
        MoveInDateAdjust,
        HouseholdType, 
        CorrectedHoH, 
        LivingSituation, 
        ExitAdjust, 
        Destination
      ) %>%
      funique()
  }
  
  ####
  # Dropping first period Unknowns + multiple Inactives in a row ----------------
  ####
  enrollments_to_remove <- universe_w_ppl_flags %>%
    fsubset(period != "Full", PersonalID, period, InflowTypeDetail, OutflowTypeDetail) %>%
    funique() %>%
    setorder(PersonalID, period) %>%
    fgroup_by(PersonalID) %>%
    fmutate(first_inflow = ffirst(InflowTypeDetail)) %>%
    fungroup() %>%
    fmutate(prev_period_outflow = flag(OutflowTypeDetail, g = PersonalID)) %>%
    fsubset(
      # Remove first Unknown
      first_inflow == "Unknown" |
        # Remove multiple inactives
        (OutflowTypeDetail == "Inactive" & prev_period_outflow == "Inactive")
    ) %>%
    fselect(PersonalID, period)
  
  universe_w_ppl_flags_clean <- universe_w_ppl_flags %>%
    join(enrollments_to_remove, on = c("PersonalID", "period"), how="anti")
  
  browser()
  
  # universe_w_ppl_flags_clean[PersonalID == 585172 & period == "2021-10-01", .(PersonalID, period, EnrollmentID, ProjectType, ProjectTypeWeight, EntryDate, MoveInDateAdjust, ExitAdjust, lh_date, lh_start, lh_end, days_since_last_lh, prev_lh_end, eecr, lecr, InflowTypeDetail, InflowTypeDetail_new)]
  # universe_w_ppl_flags_clean[PersonalID == 666267 & period == "2021-10-01", .(PersonalID, period, EnrollmentID, ProjectType, ProjectTypeWeight, EntryDate, MoveInDateAdjust, ExitAdjust, lh_date, lh_start, lh_end, days_since_last_lh, prev_lh_end, InflowTypeDetail, InflowTypeDetail_new)]
  return(universe_w_ppl_flags_clean)
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
    fselect(c(enrollment_cols, "lh_dates")) %>%
    funique()
}
