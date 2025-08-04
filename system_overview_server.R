# when user changes chart tabs
# hide demographic filters for Composition chart
# hide other stuff if valid file is not uploaded
# move chart download button to be inline with subtabs
observeEvent(input$syso_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  shinyjs::runjs(str_glue("
    $('#syso_spec_pops, #syso_age, #syso_race_ethnicity')
      .closest('.bslib-grid-item')
      .toggle({ifelse(input$syso_tabbox != 'System Demographics', 'true','false')});
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
    num_people <- fndistinct(period_specific_data()[["Full"]]$PersonalID)
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
  
  # custom_rprof({
  all_filtered <- join( 
    get_eecr_and_lecr(),
    client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  )
  
  universe_w_enrl_flags <- universe_enrl_flags(all_filtered)
  universe_w_ppl_flags <- universe_ppl_flags(universe_w_enrl_flags)
  # }, "system_overview_server")
browser()
  # Split into months and full-period datasets
  list(
    Full = universe_w_ppl_flags[period == "Full"],
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
  req(nrow(session$userData$client_categories) > 0)
  
  session$userData$client_categories[
    AgeCategory %in% input$syso_age &
    (if(input$syso_race_ethnicity == "All") rep(TRUE, .N) else get(input$syso_race_ethnicity) == 1) & 
    (
      input$syso_spec_pops == "None" |
      (input$syso_spec_pops == "Veteran" &
         VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
      (input$syso_spec_pops == "NonVeteran" &
         VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17"))
    )
  ]
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
      # they are LH entry during period if they entered in the window with an LH PLS OR
      # simply because they were in a SO or ES-NBN project. This is sufficient to mark them as such
      # because these are LH Project Types.
      lh_entry_during_period = EntryDate %between% list(start_window, endDate) & (
        lh_prior_livingsituation | ProjectType == out_project_type
      )
    )  %>%
    fsubset(
      lh_cls_in_start_window |
        lh_cls_in_end_window |
        lh_cls_during_period |
        entry_in_start_window |
        entry_in_end_window |
        lh_entry_during_period,
      c(period, EnrollmentID, ProjectType, lh_prior_livingsituation,
      lh_cls_in_start_window,
      lh_cls_in_end_window,
      lh_cls_during_period,
      entry_in_start_window,
      entry_in_end_window,
      lh_entry_during_period,
      straddles_start, straddles_end, days_since_lookback, days_to_lookahead,
      last_lh_info_date)
    )

  if(nrow(lh_non_res) == 0 ) {
    logToConsole(session, "no non-res lh records")
    return(lh_non_res)
  }
  
  lh_non_res %>%
    # Group by EnrollmentID and calculate window flags
    fgroup_by(period, EnrollmentID) %>%
    fmutate(
      lh_cls_in_start_window = anyv(lh_cls_in_start_window, TRUE),
      lh_cls_in_end_window = anyv(lh_cls_in_end_window, TRUE),
      lh_cls_during_period = anyv(lh_cls_during_period, TRUE)
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
      straddles_start, straddles_end, days_since_lookback, days_to_lookahead,
      last_lh_info_date
    ) %>%
    fsubset(
      nbn_in_start_window |
      nbn_in_end_window |
      nbn_during_period |
      entry_in_start_window |
      entry_in_end_window |
      lh_entry_during_period
    )
  
  if(nrow(lh_nbn) == 0) {
    logToConsole(session, "no NbN lh records")
    return(lh_nbn)
  }
  
  lh_nbn %>%
    fgroup_by(period, EnrollmentID) %>%
    fmutate(
      nbn_in_start_window = anyv(nbn_in_start_window, TRUE),
      nbn_in_end_window = anyv(nbn_in_end_window, TRUE),
      entry_in_start_window = entry_in_start_window,
      entry_in_end_window = entry_in_end_window,
      nbn_during_period = anyv(nbn_during_period, TRUE)
    ) %>%
    fungroup()
}

## LH info for Other enrollments --------------
lh_other_period <- function() {
  logToConsole(session, "in lh_other_period")
  expand_by_periods(session$userData$enrollment_categories) %>%
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
      startDate, endDate,
      in_date_range
    )
}

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


get_lh_non_res_esnbn_info <- function() {
  lh_non_res_esnbn_info <- rbindlist(
    list(
      lh_non_res_period(),
      lh_nbn_period()
    ),
    fill = TRUE
  ) %>% 
    fmutate(
      was_lh_at_start = (straddles_start | days_since_lookback %between% c(0, 14)) & (
        # Non-Res and LH CLS in 60/90-day window OR 
        # Entry in 60/90 day window and lh_prior_livingsituation
        (ProjectType %in% non_res_project_types & (
          lh_cls_in_start_window | 
          (entry_in_start_window & (ProjectType == out_project_type | lh_prior_livingsituation))
        )) |
        # ES NbN and Bed Night in 15-day window
        # we don't need lh_prior_livingsituation here 
        # because ES NbN enrollment implies homelessness
        (ProjectType == es_nbn_project_type & (
          nbn_in_start_window | entry_in_start_window
        ))
      ),
      
      was_lh_during_period = (
        ProjectType == es_nbn_project_type & (
          nbn_during_period | lh_entry_during_period
        )
      ) | (
        ProjectType %in% non_res_project_types & (
          lh_cls_during_period | lh_entry_during_period
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
      was_lh_at_end,
      last_lh_info_date
    ) %>%
    funique()
  
  return(lh_non_res_esnbn_info)
}

get_res_lh_info <- function() {
  lh_other_period() %>% 
    fmutate(
      # For Res projects (lh_project_types 0,2,8 and ph_project_types 3,9,10,13)
      # must either straddle or otherwise be close to (i.e. 14 days from) 
      # start so we can make claims about status at start
      # and must be within 14 days of previous enrollment, otherwise it would be an exit
      was_lh_at_start = (straddles_start | days_since_lookback %between% c(0, 14)) & (
        ProjectType %in% lh_project_types_nonbn | 
        (ProjectType %in% ph_project_types & fcoalesce(MoveInDateAdjust, no_end_date) >= startDate)
      ),
        
      was_lh_during_period = ProjectType %in% c(lh_project_types_nonbn, ph_project_types) & in_date_range,
      
      was_lh_at_end = (straddles_end | days_to_lookahead %between% c(0, 14)) & (
        ProjectType %in% lh_project_types_nonbn | 
        (ProjectType %in% ph_project_types & fcoalesce(MoveInDateAdjust, no_end_date) >= endDate)
      ),
      
      last_lh_info_date = NA
    ) %>%
    fselect(
      period, 
      EnrollmentID, 
      was_lh_at_start, 
      was_lh_during_period, 
      was_lh_at_end,
      last_lh_info_date
    ) %>%
    funique()
}

get_eecr_and_lecr <- reactive({
  logToConsole(session, "in get_eecr_and_lecr")
  period_enrollments_filtered <- expand_by_periods(enrollments_filtered())
  
  logToConsole(session, paste0("In get_eecr_and_lecr, num period_enrollments_filtered: ", nrow(period_enrollments_filtered)))
  
  if(nrow(period_enrollments_filtered) == 0) return(period_enrollments_filtered)
  
  # Determine eecr/lecr-eligible records
  # get lh info and  limit to only enrollments that were LH during the given period 
  # or were not, but exited and HAD been LH at some point during the FULL period
  # the exit-but-was-once-LH is important because 
  all_enrollments <- period_enrollments_filtered %>% 
    join(
      rbindlist(
        list(get_lh_non_res_esnbn_info(), get_res_lh_info())
      ),
      on = c("period","EnrollmentID"),
      how = "left"
    ) %>%
    fmutate(
      was_housed_at_start = (straddles_start | days_since_lookback %between% c(0, 14)) & 
        ProjectType %in% ph_project_types &
        fcoalesce(MoveInDateAdjust, no_end_date) < startDate,
      
      was_housed_during_period = ProjectType %in% ph_project_types & 
        in_date_range & 
        fcoalesce(MoveInDateAdjust, no_end_date) <= endDate,
      
      was_housed_at_end = (straddles_end | days_to_lookahead %between% c(0, 14)) & 
        ProjectType %in% ph_project_types & 
        fcoalesce(MoveInDateAdjust, no_end_date) < endDate,
      
      was_lh_at_start = fcoalesce(was_lh_at_start, FALSE),
      was_lh_during_period = fcoalesce(was_lh_during_period, FALSE),
      was_lh_at_end = fcoalesce(was_lh_at_end, FALSE)
    ) %>%
    # flag if enrollment was EVER LH during the full period (or was in res project type). 
    # This will be important for selecting EECRs
    fgroup_by(EnrollmentID) %>%
    fmutate(
      was_lh_during_full_period = anyv(period == "Full" & was_lh_during_period, TRUE),
      nbn_non_res_no_future_lh = ProjectType %in% c(es_nbn_project_type, non_res_nonlh_project_types) & (is.na(last_lh_info_date) | last_lh_info_date <= endDate) & is.na(ExitDate)
    ) %>%
    fungroup() %>%
    fmutate(
      lookback_movein_before_start = lookback_movein < startDate
    ) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      no_lh_lookbacks = !anyv(was_lh_during_period, TRUE)
    ) %>%
    fungroup()
  
  
  if(in_dev_mode) {
    lh_non_res_agg <- if(nrow(session$userData$lh_non_res) > 0) {
      collap(
        session$userData$lh_non_res, 
        InformationDate ~ EnrollmentID, 
        FUN = function(x) paste(x[!is.na(x)], collapse = ", ")
      ) %>% fsubset(!is.na(InformationDate))
    } else data.table(EnrollmentID = NA, InformationDate = NA)
    
    lh_nbn_agg <- if(nrow(session$userData$lh_nbn) > 0) {
      collap(
        session$userData$lh_nbn, 
        DateProvided ~ EnrollmentID, 
        FUN = function(x) paste(x[!is.na(x)], collapse = ", ")
      ) %>% fsubset(!is.na(DateProvided))
    } else data.table(EnrollmentID = NA, DateProvided = NA)

    enrollment_categories_all <<- all_enrollments %>%
      join(lh_non_res_agg, on = "EnrollmentID") %>%
      join(lh_nbn_agg, on = "EnrollmentID") %>%
      fselect(c(enrollment_cols, non_res_lh_cols)) %>%
      funique()
  }
  
  potential_eecr_lecr <- all_enrollments %>%
    # only keep enrollments that were LH or housed during the period, or
    # neither but Exited in the period and were LH at SOME point during the period
    # AS 8/1: The problem with this is, e.g. PersonalID 684918 (ICf-good), for the first month, Nov, Enrollment 833423 is picked as the EECR, wehreas the full period picks 817330. 
    # 817330 it gets dropped here.
    fsubset(
      was_lh_during_period | 
      was_housed_during_period |
      (period != "Full" & (is.na(ExitDate) | ExitAdjust %between% list(startDate, endDate)) & was_lh_during_full_period)
    )
  
  e <- potential_eecr_lecr %>%
    fmutate(
      non_straddle_exit_dates = fifelse(!straddles_end, ExitAdjust, NA)
    ) %>%
    setorder(PersonalID, period, EntryDate, ExitAdjust) %>%
    fgroup_by(PersonalID, sort=FALSE) %>%
    fmutate(
      prev_non_straddle_exits = flag(na_locf(non_straddle_exit_dates))
    ) %>%
    fungroup() %>%
    fmutate(
      background_non_res_straddle = fcoalesce(straddles_end & nbn_non_res_no_future_lh & EntryDate < prev_non_straddle_exits, FALSE)
    )
  
  e2 <- e %>%
    # Flag if person had any straddling enrollments
    # to be used when calculating eecr/lecr in no-straddle cases
    fgroup_by(period, PersonalID) %>%
    fmutate(
      any_straddle_start = anyv(straddles_start, TRUE),
      any_straddle_end = anyv(straddles_end, TRUE)
      # all_straddle_ends_nonresnbn_not_lh_at_end = !anyv(straddle_ends_nonresnbn_not_lh_at_end, FALSE)
    ) %>%
    fungroup() %>%
    # flag the first and last straddling enrollments, 
    # by (desc) ProjectTypeWeight and EntryDate
    roworder(period, PersonalID, -ProjectTypeWeight, EntryDate) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      first_straddle_start = ffirst(
        fifelse(straddles_start, EnrollmentID, NA)
      ) == EnrollmentID,
      any_straddle_start = anyv(straddles_start, TRUE)
    ) %>%
    fungroup() %>%
    roworder(period, PersonalID, ProjectTypeWeight, EntryDate) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      last_straddle_end = flast(
        fifelse(straddles_end & !background_non_res_straddle, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    fungroup() %>%
    # flag the first non-straddling enrollments in the report period,
    # for people that have no eecr_straddles
    # We prioritize EntryDate over ProjectTypeWeight because we want the earliest
    roworder(period, PersonalID, EntryDate, -ProjectTypeWeight, ExitAdjust) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      first_non_straddle_start = ffirst(
        fifelse(in_date_range & !straddles_start, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    fungroup() %>%
    # flag last non-straddling enrollments in the report period,
    # for people that have no lecr_straddles
    # Since these have ExitDates, given that we want the LECR to represent a 
    # client's latest known Outflow status, we order by ExitAdjust to get the latest Exit
    roworder(period, PersonalID, ExitAdjust, ProjectTypeWeight, Destination, EntryDate) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      last_non_straddle_end = flast(
        fifelse(in_date_range & !straddles_end, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    fungroup()
  
  # In most cases, the EECR should be the first_straddle_start and the LECR should be the last_straddle_end (assuming they pass the enrollment filters).
  # last_straddle_ends must also be LH or Housed at end (was_lh_at_end or was_housed_at_end).
  # If there are no straddles, then it should be the first_non_straddle_start. Ditto for (non)straddle_ends.
  # However, there are lots of exceptions:
  #   LECR exceptions:
  #   1. if the straddling enrollment is a non-res (other than SO and nbn) with 
  #       no end date and no LH info in the current period or in the future, and 
  #       it starts before the previous period's last_non_straddle_end's exit ==> then it should not be selected (stored under background_non_res_straddle)
  #
  #   2. if last_straddle_end is neither LH nor housed at end, then 
  prep_for_exceptions <- e2 %>% 
    fgroup_by(period, PersonalID) %>%
    fmutate(
      last_straddle_end_lh_or_housed_at_end = anyv(last_straddle_end & (was_lh_at_end | was_housed_at_end), TRUE),
      only_period_enrollment = GRPN() == 1
      # all_straddle_end_nbn_non_res_no_future_lh = allv(straddles_end & nbn_non_res_no_future_lh, TRUE),
    ) %>%
    fungroup() # %>%
    # fmutate(
    #   eligible_for_eecr = (first_straddle_start | first_non_straddle_start) & passes_enrollment_filters,
    #   eligible_for_lecr = fifelse(
    #     passes_enrollment_filters,
    #     fifelse(
    #       (last_straddle_end & (was_lh_at_end | was_housed_at_end) & !background_non_res_straddle), 2,
    #       fifelse(
    #         last_non_straddle_end, 1, 0
    #       )
    #     ),
    #     0
    #   )
    # ) %>%
    # fgroup_by(PersonalID, period) %>%
    # fmutate(
    #   eecr = which.max(eligible_for_eecr),
    #   lecr = which.max(eligible_for_lecr)
    # ) %>%
    # fungroup()
  
  
  
  final <- prep_for_exceptions %>%
    # Create eecr and lecr flags
    fmutate(
      # If we add background_non_res_straddle, this handles 183338, by removing July - September, since they were inactive by then
      # but then
      eecr = (first_straddle_start | (first_non_straddle_start & !any_straddle_start)) & !background_non_res_straddle & passes_enrollment_filters,
      eecr = fcoalesce(eecr, FALSE),
      lecr = (
        # If we add (was_lh/housed_at_end), then Personal ID 346740 (Enrollment 846250) is not selected as LECR, and the last month outlfow != full outflow
        # but if we remove it, both 846250 AND 835362 are selected as the LECRs
        (last_straddle_end & (
          !background_non_res_straddle | 
          only_period_enrollment
        )) |
        # (last_straddle_end & (!background_non_res_straddle | only_period_enrollment)) |
        (fcoalesce(last_non_straddle_end, FALSE) & !last_straddle_end_lh_or_housed_at_end)
      ) & passes_enrollment_filters
      # lecr_exit = fifelse(lecr, ExitAdjust, NA)
    ) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      has_eecr = anyv(eecr, TRUE), # This avoids a period with an LECR but no EECR, e.g.
      has_lecr = anyv(lecr, TRUE)
      # lecr_exit_date = fmax(lecr_exit),
      # prev_month_lecr_exit = flag(lecr_exit_date)
    ) %>%
    fungroup() %>%
    # fmutate(
    #   # This is meant to fix, e.g. PersonalID 183338 (ICF-good), who has an "udnerlying" non-res enrollment with no future LH
    #   # that's causing them to show up in July and August, after they truly exited in June from their other enrollment, 852184
    #   #
    #   # AS 8/1: The problem with this is, e.g. PersonalID 689253 (demo) in June. This will drop 826534 as the EECR and they'll have no other EECR, because the other enrollment is not an eecr
    #   # eecr = fifelse(eecr, !(startDate != session$userData$ReportStart & EntryDate < prev_month_lecr_exit & nbn_non_res_no_future_lh), FALSE),
    #   
    #   # PersonalID 190679 (ICF-good), no LECR at this point?
    #   # PersonalID 513051     (ICF-good) no LECR at this point? But they weren't LH during many of the periods, even though eecr was being selected for each period
    #   # PersonalID 533867     (ICF-gooD) no LECR in Nov this point? 
    #   # PErsonalID 565354       (ICF-good) no LECR in Oct at this point. This is becasue they entered on 8/30 and InfoDate on 11/16. Both are outside the end window
    #   lecr = fifelse(!has_lecr & !background_non_res_straddle, eecr, lecr)
    # ) %>%
    # fgroup_by(period, PersonalID) %>%
    # fmutate(
    #   has_lecr = anyv(lecr, TRUE)
    # ) %>%
    # fungroup() %>%
    fsubset(has_eecr & has_lecr)
  
  #160649 - ICFgood (getting "something's wrong" inflow) 
  #689253 - Demo (getting "something's wrong" inflow in June) 
  # 183338 - ICF good
  # 540917 - ICF-good 
  # 123945 - ICF-good (something's wrong)
  # 104510 last outflow != full
  # 330303 last outflow != full. Enrollment 859594 should be picked
  # 346740 last outflow != full
browser()
#debug cols: setdiff(outflow_debug_cols, c(non_res_lh_cols, "OutflowTypeDetail")), with=FALSE
  # people must have an eecr or they can't be counted
  final
})

get_period_specific_enrollment_categories <- reactive({
  logToConsole(session, "in get_period_specific_enrollment_categories")
  eecr_and_lecrs <- get_eecr_and_lecr()
  logToConsole(session, paste0("In get_period_specific_enrollment_categories, num eecr_and_lecrs: ", nrow(eecr_and_lecrs)))
  
  if(nrow(eecr_and_lecrs) == 0) return(eecr_and_lecrs)
  
  
  # Join eecr_and_lecr and the full set of enrollments to be used in lookbacks 
  enrollment_categories_period <- join(
    eecr_and_lecrs,
    expand_by_periods(session$userData$enrollment_categories) %>%
      fselect(
        PersonalID, 
        EnrollmentID, 
        period, 
        EntryDate, 
        MoveInDateAdjust, 
        ExitAdjust, 
        Destination, 
        ProjectType
      ),
    how = "right"
  ) %>%
    fmutate(
      lookback_movein_before_start = eecr & lookback_movein < startDate
    )

  # get lookbacks
  enrollment_categories_period <- enrollment_categories_period %>%
    # Get EntryDate of the eecr (to be used for determining lookback days)
    fgroup_by(period, PersonalID) %>%
    fmutate(eecr_entrydate = fmax(fifelse(eecr, EntryDate, NA))) %>%
    fungroup() %>%
    fmutate(
      # 5/15/25: a lookback must have exited before the EECR started
      is_lookback = !is.na(days_since_lookback),
      lh_lookback = is_lookback & was_lh_during_period
    )
  
  # need to split here
  if(nrow(enrollment_categories_period) == 0) return(enrollment_categories_period)
  
  enrollment_categories_period <- enrollment_categories_period %>%
    roworder(period, PersonalID, EntryDate, ExitAdjust) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      first_lookback = flast(lookback_enrollment_id) == EnrollmentID,
      first_lookback_destination = fmax(fifelse(first_lookback, Destination, NA)),
      first_lookback_movein = fmax(fifelse(first_lookback, MoveInDateAdjust, NA)),
      no_lh_lookbacks = !anyv(lh_lookback, TRUE),
      first_lookback_projecttype = fmax(fifelse(first_lookback, ProjectType, NA))
    ) %>%
    fungroup() %>%
    fmutate(
      
      lookback_is_nonres_or_nbn = eecr & first_lookback_projecttype %in% nbn_non_res
    )

  # 599422
  # lookback_cols =  c("period", enrollment_cols, "eecr","lecr", "first_lookback") 
  browser()
  logToConsole(session, paste0("About to subset to eecr, lecr, and lookbacks: num enrollment_categories_period records = ", nrow(enrollment_categories_period)))
  
  # remove temp vars
  enrollment_categories_period <- enrollment_categories_period %>%
    fselect(-c(any_straddle_start, any_straddle_end, first_non_straddle_start, first_straddle_start, last_straddle_end, last_non_straddle_end,
               first_lookback_destination, first_lookback_movein, first_lookback_projecttype
    ))
  
  enrollment_categories_period %>% 
    fsubset(eecr | lecr | ifelse(in_dev_mode, first_lookback, FALSE))
})
