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

get_days_since_last_lh <- function(all_filtered) {
  lh_info_all_enrl <- all_filtered %>%
    fselect(PersonalID, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, days_lh_valid) %>%
    join(
      session$userData$lh_info %>% fselect(EnrollmentID, lh_date, first_lh_date, last_lh_date),
      on="EnrollmentID",
      multiple=T
    ) %>%
    fmutate(EntryDateTemp = EntryDate, first_lh_date_temp = first_lh_date) %>%
    setorder(PersonalID, EntryDate)

  lh_info_dates <- lh_info_all_enrl[
    lh_info_all_enrl,
    on = .(PersonalID, first_lh_date <= EntryDate)
  ] %>%
    fsubset(EnrollmentID != i.EnrollmentID & EntryDate != i.EntryDateTemp) %>%
    fmutate(
      days_since_lh = i.EntryDateTemp - fifelse(
        last_lh_date <= i.EntryDateTemp,
        last_lh_date,
        fifelse(
          first_lh_date_temp + days_lh_valid >= i.EntryDateTemp, 
          i.EntryDateTemp,
          fifelse(
            ProjectType %in% c(lh_project_types_nonbn, ph_project_types), # if comparison project type is a lh-entire-time one (and we already know last_lh_date is <= the current enrollment's EntryDate), then we can assume they were LH up to and/or beyond the current EntryDate, so we'll take the min
            pmin(i.EntryDateTemp, last_lh_date, na.rm=TRUE),
            fifelse(
              fcoalesce(lh_date, no_end_date) + days_lh_valid >= i.EntryDateTemp, 
              i.EntryDateTemp,
              lh_date + days_lh_valid
            )
          )
        )
      ),
      EnrollmentID = i.EnrollmentID
    ) %>%
    fgroup_by(PersonalID, EnrollmentID) %>%
    fsummarize(
      days_since_last_lh = fmin(days_since_lh)
    )
  
  join(
    all_filtered,
    lh_info_dates,
    on = c("PersonalID", "EnrollmentID")
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
  
  # Rolling join to find the most recent end date before each start date
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
      join(lookahead_info, on = c("PersonalID", "EnrollmentID"))
  )
}

# Get period-specific universe_ppl_flag datasets ---------------------------
period_specific_data <- reactive({
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  logToConsole(session, "in period_specific_data")
  
  empty_return <- list(Full = data.table(), Months = data.table())
  
  # Get filtered records based on user selection
  filtered_enrollments <- enrollments_filtered()
  filtered_clients <- syso_client_categories_filtered() 

  # If either are empty, return empty
  if(nrow(filtered_clients) == 0 || nrow(filtered_enrollments) == 0) return(empty_return)
  
  # Apply all filters
  all_filtered <- filtered_enrollments %>% 
    join( 
      if(!IN_DEV_MODE) fselect(filtered_clients, PersonalID) else filtered_clients,
      on = "PersonalID",
      how = "inner"
    ) %>%
    get_lookbacks() %>% # add lookback info
    get_days_since_last_lh() #
  
  if(IN_DEV_MODE) store_enrollment_categories_all_for_qc(all_filtered)

  period_data <- all_filtered %>% 
    expand_by_periods(chart_type = 'mbm') %>% # expand/repeat enrollments across periods
    get_was_lh_info(all_filtered) %>% # Add was_lh/housed indicators
    get_eecr_and_lecr() %>% # select EECR/LECRs
    universe_enrl_flags() %>% # Add period-enrollment-level intermediate indicators (for Inflow and Outflow type)
    universe_ppl_flags() # Add period-person-level Inflow and Outflow types

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
syso_client_categories_filtered <- reactive({
  
  
  logToConsole(session, "In syso_client_categories_filtered")
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
expand_by_periods <- function(dt, chart_type = 'mbm') {
  if(chart_type == 'mbm'){
    all_periods <- data.table(
      period = factor(names(session$userData$report_dates)),
      startDate = as.Date(sapply(session$userData$report_dates, `[`, 1)),
      endDate = as.Date(sapply(session$userData$report_dates, `[`, 2))
    ) %>% 
      ftransform(
        exit_cutoff = startDate %m-% years(2),
        temp_key = 1
      )
  } else if(chart_type == 'exits_time'){
    all_periods <- data.table(
      period = c('Current Year','Previous Year'),
      startDate = c(session$userData$ReportStart,
                    session$userData$ReportStart %m-% years(1)),
      endDate = c(session$userData$ReportEnd,
                  session$userData$ReportEnd %m-% years(1))
    ) %>% 
      ftransform(
        exit_cutoff = startDate %m-% years(2),
        temp_key = 1
      )
  } else {
    all_periods <- data.table(
      period = c('Full'),
      startDate = session$userData$ReportStart,
      endDate = session$userData$ReportEnd
    ) %>% 
      ftransform(
        exit_cutoff = startDate %m-% years(2),
        temp_key = 1
      )
  }
  
  
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


get_was_lh_info <- function(period_enrollments_filtered, all_filtered) {
  all_filtered_w_lh <- period_enrollments_filtered %>% 
    fselect(
      period, 
      PersonalID, 
      EnrollmentID, 
      EntryDate, ExitAdjust,
      days_since_lookback, 
      days_to_lookahead, 
      days_lh_valid,
      days_since_last_lh,
      straddles_start, straddles_end,
      startDate, endDate,
      in_date_range,
      lookback_dest_perm, lookback_movein
    ) %>%
    fmutate(
      # We use the 15/60/90-day lookback when the enrollment straddles or, for the first/full period, has a lookback/lookahead within 2 weeks
      # That is, in these cases a person's LH-ness extends 15/60/90 days *beyond* the actual date
      # Used for lh_date and EntryDate
      start_minus_15_60_90_or_0 = startDate - fifelse(
        (straddles_start | (fcoalesce(days_since_lookback, as.difftime(9999, units="days")) %between% c(0, 14) & startDate == session$userData$ReportStart)), 
        days_lh_valid, 
        0
      ),
      end_minus_15_60_90_or_0 = endDate - fifelse(
        straddles_end | (fcoalesce(days_to_lookahead, as.difftime(9999, units="days")) %between% c(0, 14) & endDate == session$userData$ReportEnd), 
        days_lh_valid, 
        0
      ),
      
      # We use the 15-day lookahead for the first/full period when the enrollment straddles or there's a lookback/lookahead within 2 weeks
      # That is, in these cases a person's LH-ness extends 15 days *back* from the actual date
      # start-plus Used for lh_date and EntryDate; end-plus used for lh_date and ExitAdjust
      start_plus_15_or_0 = startDate + fifelse(
        startDate == session$userData$ReportStart &
        (straddles_start | fcoalesce(days_since_lookback, as.difftime(9999, units="days")) %between% c(0, 14)), 
        15, 
        0
      ),
      
      end_plus_15_or_0 = endDate + fifelse(
        endDate == session$userData$ReportEnd &
        (straddles_end | fcoalesce(days_to_lookahead, as.difftime(9999, units="days")) %between% c(0, 14)), 
        15, 
        0
      ),
      
      # Someone is Active at Start in the Full Period or First month if they:
      #   straddled (incl. Entry on first day) OR
      #   Entry within 2 weeks of the start but within a recent lookback
      # For the other months, they must straddle, but either:
      #   Entry BEFORE first day OR
      #   Entry ON first day, but with a recent LH date (from any enrollment)
      # The reason Entry AFTER first is not Active in the other months, even with a recent lookback, 
      # is because they'll be Continuous at Start
      active_at_start = 
        (
          startDate == session$userData$ReportStart & (
            straddles_start | (
              EntryDate %between% list(startDate, startDate + 14) &
              days_since_lookback %between% c(0, 14)
            )
          )
        ) | (
          startDate > session$userData$ReportStart & straddles_start & (
            EntryDate < startDate |
            days_since_last_lh %between% c(0, 14)
          )
        ),
      
      # Similarly for Active at End...
      active_at_end = 
        (
          endDate == session$userData$ReportEnd & (
            straddles_end | (
              ExitAdjust %between% list(endDate - 14, endDate) &
              days_to_lookahead %between% c(0, 14)
            )
          )
        ) | (
          endDate < session$userData$ReportEnd &
          (ExitAdjust > endDate | (ExitAdjust == endDate & days_to_lookahead %between% c(0,14)))
        )
    ) %>%
    join(
      session$userData$lh_info,
      on = c("PersonalID","EnrollmentID"),
      multiple=TRUE
    ) %>%
    fgroup_by(PersonalID, period) %>%
    fmutate(
      lh_in_any_other_enrollment_in_period = any(
        lh_date %between% list(startDate, endDate) |
        EntryDate %between% list(startDate, endDate) & lh_at_entry, 
        na.rm=TRUE
      )
    ) %>%
    fungroup() %>%
    fmutate(
      MoveInDateAdjust = fcoalesce(MoveInDateAdjust, no_end_date),
      lh_date = fcoalesce(lh_date, no_end_date),
      
      # An enrollment is LH at start if it was Active at Start and:
      #   - PH or res project with a Move-In after start (since these are LH until they move in)
      #   - have an LH CLS or NbN in the "start window" (which includes the 15/60/90 day lookback for straddles )
      was_lh_at_start = active_at_start & (
        (
          ProjectType %in% c(lh_project_types_nonbn, ph_project_types) & 
          MoveInDateAdjust >= startDate
        ) |
        lh_date %between% list(start_minus_15_60_90_or_0, start_plus_15_or_0) | 
        (EntryDate %between% list(start_minus_15_60_90_or_0, start_plus_15_or_0) & lh_at_entry)
      ),
        
      # These are *definitely* lh during period
      was_lh_during_period_def = in_date_range & (
        ProjectType %in% c(lh_project_types_nonbn, ph_project_types) |
        lh_date %between% list(start_minus_15_60_90_or_0, endDate) | 
        (EntryDate %between% list(start_minus_15_60_90_or_0, endDate) & lh_at_entry)
      ),
      
      # An enrollment can also be LH during period if the ExitAdjust is the only LH date during a non-Full period
      # as long as it's not the only LH date in the Full period. If it is, we'll drop as not being "lh during full period"
      was_lh_during_period = was_lh_during_period_def | 
        ExitAdjust %between% list(startDate, endDate) |
        lh_in_any_other_enrollment_in_period,
      
      was_lh_at_end = active_at_end & (
        (
          ProjectType %in% c(lh_project_types_nonbn, ph_project_types) &
          MoveInDateAdjust >= endDate
        ) | 
        lh_date %between% list(end_minus_15_60_90_or_0, end_plus_15_or_0) | 
        (EntryDate %between% list(end_minus_15_60_90_or_0, endDate) & lh_at_entry) | 
        ExitAdjust %between% list(endDate, end_plus_15_or_0)
      ),
      
      was_housed_at_start = active_at_start & 
        ProjectType %in% ph_project_types & (
          MoveInDateAdjust < startDate |
          (days_since_lookback %between% c(0, 14) & lookback_dest_perm & lookback_movein < startDate)
        ),
      
      was_housed_during_period = ProjectType %in% ph_project_types & 
        in_date_range & 
        MoveInDateAdjust <= endDate,
      
      was_housed_at_end = active_at_end & 
        ProjectType %in% ph_project_types & 
        MoveInDateAdjust < endDate
    ) %>%
    fgroup_by(EnrollmentID) %>%
    fmutate(
      was_lh_during_full_period = any(period == "Full" & was_lh_during_period_def, na.rm=TRUE)
    ) %>%
    fungroup()

  # We only want enrollments that were:
  # LH during Full Period AND (LH/Housed during the given period or Exited in the future)
  # This will end up including a lot of enrollments that were Inactive
  # We only want the first of these; the rest will be dropped in inflow_outflow_server

# all_filtered_w_lh[PersonalID == 612386, c("period", enrollment_cols, "was_lh_during_full_period", "was_lh_during_period", "was_housed_during_period"), with=FALSE]
  all_filtered_w_lh <- all_filtered_w_lh %>%
    fsubset(
      was_lh_during_full_period == TRUE,
      period, EnrollmentID, PersonalID,
      was_lh_at_start,
      was_lh_during_period_def,
      was_lh_during_period,
      was_lh_during_full_period,
      was_lh_at_end,
      was_housed_at_start,
      was_housed_during_period,
      was_housed_at_end
    ) %>%
    funique() %>%
    fgroup_by(period, PersonalID, EnrollmentID) %>%
    fsummarize(
      was_lh_at_start = any(was_lh_at_start, na.rm = TRUE),
      was_lh_during_period_def = any(was_lh_during_period_def, na.rm=TRUE),
      was_lh_during_period = any(was_lh_during_period, na.rm=TRUE),
      was_lh_at_end = any(was_lh_at_end, na.rm=TRUE),
      was_housed_at_start = any(was_housed_at_start, na.rm = TRUE),
      was_housed_during_period = any(was_housed_during_period, na.rm = TRUE),
      was_housed_at_end = any(was_housed_at_end, na.rm = TRUE)
    )

  return(
    period_enrollments_filtered %>% 
      join(
        all_filtered_w_lh, 
        on = c("period","EnrollmentID"),
        how = "inner"
      )
  )
}

get_eecr_and_lecr <- function(period_enrollments_filtered_was_lh) {
  logToConsole(session, paste0("In get_eecr_and_lecr, num period_enrollments_filtered: ", nrow(period_enrollments_filtered_was_lh)))

  e <- period_enrollments_filtered_was_lh %>%
    fsubset(
      was_lh_during_period |
      was_housed_during_period |
      (
        ExitAdjust %between% list(startDate, session$userData$ReportEnd) | 
        (endDate == session$userData$ReportEnd & ExitAdjust > endDate)
      )
    )
  
  e2 <- e %>%
    # flag the first and last straddling enrollments, 
    # by (desc) ProjectTypeWeight and EntryDate
    roworder(period, PersonalID, -ProjectTypeWeight, EntryDate) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      first_straddle_start = ffirst(
        fifelse(straddles_start, EnrollmentID, NA)
      ) == EnrollmentID,
      any_straddle_start = any(first_straddle_start, na.rm=TRUE)
    ) %>%
    fungroup() %>%
    roworder(period, PersonalID, ProjectTypeWeight, EntryDate) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      last_valid_straddle_end = flast(fifelse(straddles_end, EnrollmentID, NA)) == EnrollmentID,
      last_valid_straddle_end_exit = fmax(fifelse(last_valid_straddle_end, ExitAdjust, NA))
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
      last_valid_non_straddle_end = flast(
        fifelse(in_date_range & !straddles_end, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    fungroup()
  
  # In most cases, the EECR should be the first_straddle_start and the LECR should be the last_straddle_end (assuming they pass the enrollment filters).
  # last_straddle_ends must also be LH or Housed at end (was_lh_at_end or was_housed_at_end).
  # If there are no straddles, then it should be the first_non_straddle_start. Ditto for (non)straddle_ends.
  # However, there are lots of exceptions:
  #   LECR exceptions:
  #   1. if the straddling enrollment is a "background, non-res enrollment", i.e. 
  #     a non-res (other than SO and nbn) with 
  #     no end date and no LH info in the current period or in the future, and 
  #     it starts before the previous period's last_non_straddle_end's exit ==> 
  #     then it should NOT be selected
  #
  #   2. if last_straddle_end is neither LH nor housed at period end, then take the non-straddle
  prep_for_exceptions <- e2 %>% 
    fgroup_by(period, PersonalID) %>%
    fmutate(
      last_straddle_end_lh_or_housed_at_end = any(last_valid_straddle_end & (was_lh_at_end | was_housed_at_end), na.rm=TRUE),
      only_period_enrollment = GRPN() == 1
    ) %>%
    fungroup()
  
  final <- prep_for_exceptions %>%
    # Create eecr and lecr flags
    fmutate(
      eecr = (first_straddle_start | (first_non_straddle_start & !any_straddle_start)),
      lecr = (
        # If we add (was_lh/housed_at_end), then Personal ID 346740 (ICF-good, Enrollment 846250) is not selected as LECR, and the last month outlfow != full outflow
        # but if we remove it, both 846250 AND 835362 are selected as the LECRs
        last_valid_straddle_end |
        (
          fcoalesce(last_valid_non_straddle_end, FALSE) & !last_straddle_end_lh_or_housed_at_end &  
           # Prevents this:
           #    PersonalID     period EnrollmentID ProjectType  EntryDate ExitAdjust   eecr   lecr last_straddle_end_exit
           # 3:     688880 2021-11-01       826879           6 2021-10-05 2021-11-08  FALSE   TRUE             2022-01-07
          #   4:     688880 2021-11-01       826045           4 2021-09-28 2022-01-07   TRUE   TRUE             2022-01-07
          (EntryDate >= last_valid_straddle_end_exit | is.na(last_valid_straddle_end_exit))
        )
      )
    ) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      has_eecr = any(eecr, na.rm=TRUE),
      has_lecr = any(lecr, na.rm=TRUE) 
    ) %>%
    fungroup()
  
  #160649 - ICFgood (getting "something's wrong" inflow) 
  #689253 - Demo (getting "something's wrong" inflow in June) 
  # 183338 - ICF good last outflow != full
  # 540917 - ICF-good 
  # 123945 - ICF-good (something's wrong)
  # 104510 last outflow != full
  # 330303 last outflow != full. Enrollment 859594 should be picked
  # 346740 last outflow != full
  # 637552 (Demo Mode), First Inflow != Full Inflow. Enrollment 826535 should be selected, but 842850 is instead
  # 423741 (ICF-good) outflow != full.
  # 613089        (ICF-good) first inflow!= full
  # 613426 (ICF-good) first inflow!= full
  # 525922 (ICF-good) last outflow != full
  # 687550 (Demo mode) last outflow != full
  # 140224 (ICF-good) last outflow != full. EnrollmentID 848355 has a last_lh_date ini itlaly, but then it's NA
  # 423741 (ICF-good) last outflow != full
  # 596228 (ICF-good) last outflow != full
  # 531816 (ICf-good) first inflow != full (when HHType == "AO" and PrjectType == "LHRes")
  # 425572 (ICF-good) last outflow != full (when HHType == "AO")
  # 150484 (ICF-good) multiple inactives in a row
  # 186128 - missing June? (as of 8/25/25)
  
  # 607965
  # QC checks ---------------
#debug cols: final[PersonalID == 595646, c("period", enrollment_cols, "eecr", "lecr"), with=FALSE]
  # people must have an eecr or they can't be counted
  final <- final %>% fsubset(has_eecr & has_lecr)
  
  if(!IN_DEV_MODE) {
    final <- final %>%
      fselect(c(
        "period",
        enrollment_cols,
        "eecr",
        "lecr",
        "first_lh_date", "last_lh_date",
        "days_since_lookback", "days_to_lookahead", "days_since_last_lh",
        "straddles_start", "straddles_end",
        "startDate","endDate",
        "lookback_dest_perm", "lookback_is_nonres_or_nbn",
        "was_lh_at_start", "was_lh_during_period", "was_lh_at_end", "was_housed_at_start", "was_housed_at_end",
        "Destination", "LivingSituation",
        "HouseholdType", "CorrectedHoH"
      )) %>%
      funique()
  }

  return(final)
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
