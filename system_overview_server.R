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


observeEvent(input$syso_methodology_type, {
  
  updatePickerInput(
    session, 
    "syso_race_ethnicity", 
    choices = sys_race_ethnicity_cats(input$syso_methodology_type)
  )

  # update System Composition Grouped Races/Ethnicities label
  grouped_re_lbl_new <- ifelse(input$syso_methodology_type == 1, "Grouped", "Hispanic-Focused")
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
#                     choices = sys_spec_pops_people)
# })

#### DISPLAY FILTER SELECTIONS ###
syso_detailBox <- reactive({
  
  sys_detailBox(
    all_filters = TRUE,
    methodology_type = input$syso_methodology_type,
    cur_project_types = input$syso_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syso_age,
    spec_pops = input$syso_spec_pops,
    race_eth = input$syso_race_ethnicity
    )
})


toggle_sys_components(prefix = 'sys', FALSE, init=TRUE) # initially hide them

#### FILTERS ###

# Population reactives ----------------------------------------------------

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
    syso_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  )
  
  all_filtered_w_lh <- add_lh_info(all_filtered)
  universe_w_enrl_flags <- universe_enrl_flags(all_filtered_w_lh)
  universe_w_ppl_flags <- universe_ppl_flags(universe_w_enrl_flags)
  # }, "system_overview_server")
  
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
syso_client_categories_filtered <- reactive({
  
  
  logToConsole(session, "In syso_client_categories_filtered")
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
      period, startDate, endDate
    )
}

# Combine lh_infos and add to filtered universe dataset-------------------
add_lh_info <- function(all_filtered) {
  logToConsole(session, "in add_lh_info")

  lh_other_info <- lh_other_period(all_filtered)[, .(
    EnrollmentID,
    period,
    
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
  )]
  
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
  lh_non_res_esnbn_info <- rbindlist(
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
      was_lh_at_end
    ) %>%
    funique()
  
  return(lh_non_res_esnbn_info)
}

get_eecr_and_lecr <- reactive({
  logToConsole(session, "in get_eecr_and_lecr")
  period_enrollments_filtered <- expand_by_periods(enrollments_filtered())
  
  logToConsole(session, paste0("In get_eecr_and_lecr, num period_enrollments_filtered: ", nrow(period_enrollments_filtered)))
  
  if(nrow(period_enrollments_filtered) == 0) return(period_enrollments_filtered)
  
  # get lh info and  limit to only enrollments that were LH during the given period 
  # or were not, but exited and HAD been LH at some point during the FULL period
  # the exit-but-was-once-LH is important because 
  e <- period_enrollments_filtered %>% 
    join(
      get_lh_non_res_esnbn_info(),
      on = c("period","EnrollmentID"),
      how = "left"
    ) %>%
    fmutate(
      was_lh_during_period = fcoalesce(
        was_lh_during_period, 
        ProjectType %in% c(lh_project_types_nonbn, ph_project_types)
      )
    ) %>% 
    # flag if enrollment was EVER LH during the full period (or was in res project type). 
    # This will be important for selecting EECRs
    fgroup_by(EnrollmentID) %>%
    fmutate(
      was_lh_during_full_period = anyv(period == "Full" & was_lh_during_period, TRUE)
    ) %>%
    fungroup() %>%
    # now ignore (for the purposes of eecr/lecr selection, enrollments that were neither LH during the period nor
    # exited wihtout being LH but were at least LH during the FULL period
    fsubset(
      was_lh_during_period | 
      (period != "Full" & ExitAdjust %between% list(startDate, endDate) & was_lh_during_full_period)
    )
  
  # used in determining lecr if all enrollments straddle the end
  # non-res enrollments that were not lh_at_end
  e <- e %>%
    fmutate(
      straddle_ends_nonresnbn_not_lh_at_end = straddles_end & 
        ProjectType %in% c(es_nbn_project_type, non_res_project_types) & 
        !was_lh_at_end
    )
  
  e <- e %>%
    # Flag if person had any straddling enrollments
    # to be used when calculating eecr/lecr in no-straddle cases
    fgroup_by(period, PersonalID)
  
  e <- e %>%
    fmutate(
      any_straddle_start = anyv(straddles_start, TRUE),
      any_straddle_end = anyv(straddles_end, TRUE),
      all_straddle_ends_nonresnbn_not_lh_at_end = allv(straddle_ends_nonresnbn_not_lh_at_end, TRUE)
    ) %>%
    # flag the first and last straddling enrollments, 
    # by (desc) ProjectTypeWeight and EntryDate
    roworder(period, -ProjectTypeWeight, EntryDate) %>%
    fmutate(
      eecr_straddle = ffirst(
        fifelse(straddles_start, EnrollmentID, NA)
      ) == EnrollmentID,
      lecr_straddle = flast(
        fifelse(straddles_end, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    # flag the first non-straddling enrollments in the report period,
    # for people that have no eecr_straddles
    # We prioritize EntryDate over ProjectTypeWeight because we want the earliest
    roworder(period, EntryDate, -ProjectTypeWeight, ExitAdjust) %>%
    fmutate(
      eecr_no_straddle = ffirst(
        fifelse(in_date_range & !any_straddle_start, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    # flag last non-straddling enrollments in the report period,
    # for people that have no lecr_straddles
    # Since these have ExitDates, given that we want the LECR to represent a 
    # client's latest known Outflow status, we order by ExitAdjust to get the latest Exit
    roworder(period, ExitAdjust, ProjectTypeWeight, Destination, EntryDate) %>%
    fmutate(
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
  
  e <- e %>%
    # Create eecr and lecr flags
    fmutate(
      in_nbn_non_res = ProjectType %in% c(es_nbn_project_type, non_res_project_types),
      eecr = (eecr_straddle | eecr_no_straddle) & passes_enrollment_filters,
      eecr = fcoalesce(eecr, FALSE),
      lecr = (lecr_straddle | lecr_no_straddle) & passes_enrollment_filters,
      lecr = fcoalesce(lecr, FALSE),
      in_nbn_non_res = NULL
    )
  
  # people must have an eecr or they can't be counted
  e %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      has_lecr = anyv(lecr, TRUE),
      has_eecr = anyv(eecr, TRUE)
    ) %>%
    fungroup() %>%
    fsubset(has_eecr == TRUE) %>%
    # "fill in" lecr as TRUE where eecr is the only enrollment
    ftransform(lecr = lecr | (eecr & !has_lecr)) %>%
    # only keep if it's an eecr or lecr
    fsubset(eecr | lecr)
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
  )

  # get lookbacks
  enrollment_categories_period <- enrollment_categories_period %>%
    # Get EntryDate of the eecr (to be used for determining lookback days)
    fgroup_by(period, PersonalID) %>%
    fmutate(eecr_entrydate = fmax(fifelse(eecr, EntryDate, NA))) %>%
    fungroup() %>%
    fmutate(
      # 5/15/25: a lookback must have exited before the EECR started
      is_lookback = ExitAdjust <= eecr_entrydate,
      perm_dest = is_lookback & Destination %in% perm_livingsituation,
      nonperm_dest = is_lookback & !Destination %in% perm_livingsituation
    ) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      # To be Return/Re-Engaged, they need a lookback with an exit to the corresponding destination
      any_lookbacks_with_exit_to_perm = anyv(perm_dest, TRUE),
      any_lookbacks_with_exit_to_nonperm = anyv(nonperm_dest, TRUE)
    ) %>%
    fungroup()
  
  # need to split here
  if(nrow(enrollment_categories_period) == 0) return(enrollment_categories_period)
  
  enrollment_categories_period <- enrollment_categories_period %>%
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
  
  enrollment_categories_period <- enrollment_categories_period %>%
    fselect(-c(any_straddle_start, any_straddle_end, eecr_no_straddle, eecr_straddle, lecr_straddle, lecr_no_straddle,
               first_lookback_exit, first_lookback_destination, first_lookback_movein
    ))
  
  enrollment_categories_period %>% 
    fsubset(eecr | lecr | ifelse(in_dev_mode, first_lookback, FALSE))
})
