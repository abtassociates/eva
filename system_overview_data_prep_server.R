
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
    input$syso_gender,
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
  
  # assign("upload_name", ifelse(input$in_demo_mode, "DEMO", input$imported$name), envir=.GlobalEnv)
  # assign("input_hh_type", input$syso_hh_type, envir=.GlobalEnv)
  # assign("input_level_detail", input$syso_level_of_detail, envir=.GlobalEnv)
  # assign("input_project_type", input$syso_project_type,envir= .GlobalEnv)
  # assign("client_categories_filt", client_categories_filtered(), envir=.GlobalEnv)
  upload_name <- ifelse(input$in_demo_mode, "DEMO", input$imported$name)
  
  # results <- mirai_map(
  results <- lapply(
    session$userData$report_dates,
    function(period) {
      # all_filtered <- universe_filtered_mirai(period, upload_name, client_categories_filt)
      all_filtered <- universe_filtered(period, upload_name)
      universe_w_enrl_flags <- universe_enrl_flags(all_filtered, period)
      universe_w_ppl_flags <- universe_ppl_flags(universe_w_enrl_flags)
      
      # Add month flag for month-periods
      if(!identical(period, session$userData$report_dates[["Full"]])) {
        universe_w_ppl_flags[, month := as.Date(period[1])]
      }
      universe_w_ppl_flags
    }#, menv
  )#[.progress, .stop]
  cache[[cache_key]] <- results
  session$userData$period_cache <- cache
  results
})

# Client-level flags, filtered ----------------------------------------------------
client_categories_filtered <- reactive({
  req(nrow(session$userData$client_categories) > 0)
  session$userData$client_categories[, All := 1][
    AgeCategory %in% input$syso_age &
      get(input$syso_gender) == 1 &
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
universe_filtered <- function(period, upload_name) {
  merge(
    session$userData$get_period_specific_enrollment_categories(period, upload_name),
    session$userData$get_period_specific_nbn_enrollment_services(period, upload_name), 
    by = "EnrollmentID",
    all.x = T
  )[ # Inner Join with client categories
    # This is necessary for bringing in Veteran Status, but will also make the rest faster
    client_categories_filtered(),
    on = "PersonalID",
    nomatch = NULL
  ][
    # Household type filter
    (input$syso_hh_type == "All" |
       (input$syso_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
       (input$syso_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
       (input$syso_hh_type == "AO" & HouseholdType %in% c("AO","UY")) | 
       (input$syso_hh_type == "AC" & HouseholdType %in% c("AC","PY")) | 
       input$syso_hh_type == HouseholdType
    ) &
      # Level of detail filter
      (input$syso_level_of_detail == "All" |
         (input$syso_level_of_detail == "HoHsAndAdults" &
            (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
         (input$syso_level_of_detail == "HoHsOnly" &
            CorrectedHoH == 1)) &
      # Project type filter
      ((input$syso_project_type == "All" |
          (input$syso_project_type == "Residential" &
             ProjectType %in% project_types_w_beds &
             eecr == TRUE) | eecr == FALSE) |
         ((input$syso_project_type == "NonResidential" &
             ProjectType %in% non_res_project_types &
             eecr == TRUE) | eecr == FALSE))
  ]
}

universe_filtered_mirai <- function(period, upload_name, client_categories_filt) {
  merge(
    session$userData$get_period_specific_enrollment_categories(period, upload_name),
    session$userData$get_period_specific_nbn_enrollment_services(period, upload_name), 
    by = "EnrollmentID",
    all.x = T
  )[ # Inner Join with client categories
    # This is necessary for bringing in Veteran Status, but will also make the rest faster
    client_categories_filt,
    on = "PersonalID",
    nomatch = NULL
  ][
    # Household type filter
    (input_hh_type == "All" |
       (input_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
       (input_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
       (input_hh_type == "AO" & HouseholdType %in% c("AO","UY")) | 
       (input_hh_type == "AC" & HouseholdType %in% c("AC","PY")) | 
       input_hh_type == HouseholdType
    ) &
      # Level of detail filter
      (input_level_detail == "All" |
         (input_level_detail == "HoHsAndAdults" &
            (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
         (input_level_detail == "HoHsOnly" &
            CorrectedHoH == 1)) &
      # Project type filter
      ((input_project_type == "All" |
          (input_project_type == "Residential" &
             ProjectType %in% project_types_w_beds &
             eecr == TRUE) | eecr == FALSE) |
         ((input_project_type == "NonResidential" &
             ProjectType %in% non_res_project_types &
             eecr == TRUE) | eecr == FALSE))
  ]
}

# DEPRECATED homeless cls finder function --------------------------------------
# This function aids in the categorization of people as 
# active_at_start, homeless_at_end, and unknown_at_end
# It does this by casting a wide net for Project Types that rely on CurrentLivingSituation
# homeless_cls_finder <- function(date, window = "before", days = 60, enrollments_filtered = NULL) {
#   plus_days <- ifelse(window == "before", 0, days)
#   minus_days <- ifelse(window == "after", 0, days)
#   
#   cls <- CurrentLivingSituation %>%
#     filter(
#       CurrentLivingSituation %in% homeless_livingsituation_incl_TH &
#         between(InformationDate,
#                 date - days(minus_days),
#                 date + days(plus_days))
#     ) %>%
#     pull(EnrollmentID) %>%
#     unique()
#   
#   if(is.null(enrollments_filtered)) {
#     cls
#   } else {
#     intersect(cls, enrollments_filtered$EnrollmentID)
#   }
# }

# Period-specific, user-filtered, enrollment-level universe with enrollment-level flags ------------------------
# hello weary traveler amongst these date ranges. you may find it helpful to
# find example clients and their Entry and Exit Dates and enter them into
# https://onlinetools.com/time/visualize-date-intervals <- here.
# add inflow type and active enrollment typed used for system overview plots
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
            !is.na(days_since_previous_exit) &
            between(as.numeric(days_since_previous_exit), 0, 14))),
    
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
    
    at_least_14_days_to_eecr_enrl = lookback == 1 & 
      !is.na(days_to_next_entry) &
      days_to_next_entry >= 14,
    
    lookback1_temp_dest = lookback == 1 & 
      !(Destination %in% perm_livingsituation),
    
    # outflow columns
    perm_dest_lecr = lecr == TRUE &
      Destination %in% perm_livingsituation &
      ExitAdjust <= endDate, 
    
    temp_dest_lecr = lecr == TRUE &
      !(Destination %in% perm_livingsituation) &
      ExitAdjust <= endDate,
    
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

# Period-specific, user-filtered, enrollment-level universe with people-level flags ------------------------
# Need to keep it enrollment-level so other scripts can reference the enrollments
universe_ppl_flags <- function(universe_df) {
  # browser()
  universe_df[, `:=`(
    # INFLOW
    active_at_start_homeless_client = any(active_at_start_homeless, na.rm = TRUE),
    
    active_at_start_housed_client = any(active_at_start_housed, na.rm = TRUE),
    
    return_from_perm_client = any(lookback1_perm_dest, na.rm = TRUE) & 
      any(at_least_14_days_to_eecr_enrl, na.rm = TRUE),
    
    reengaged_from_temp_client = any(lookback1_temp_dest, na.rm = TRUE) & 
      # max(eecr_lh_at_entry) == 1 & 
      any(at_least_14_days_to_eecr_enrl, na.rm = TRUE),
    
    newly_homeless_client = all(lookback == 0, na.rm = TRUE) |
      !any(eecr_lh_at_entry, na.rm = TRUE) | 
      !any(at_least_14_days_to_eecr_enrl, na.rm = TRUE),
    
    # OUTFLOW
    perm_dest_client = any(perm_dest_lecr, na.rm = TRUE),
    
    temp_dest_client = any(temp_dest_lecr, na.rm = TRUE),
    
    homeless_at_end_client = any(homeless_at_end, na.rm = TRUE),
    
    housed_at_end_client = any(housed_at_end, na.rm = TRUE),
    
    unknown_at_end_client = any(unknown_at_end, na.rm = TRUE)
  ), by = PersonalID
  ][, `:=`(
    InflowTypeSummary = fifelse(
      active_at_start_homeless_client | active_at_start_housed_client,
      "Active at Start",
      fifelse(
        newly_homeless_client | return_from_perm_client | reengaged_from_temp_client,
        "Inflow",
        "something's wrong"
      )
    ),
    
    InflowTypeDetail = fifelse(
      active_at_start_homeless_client == TRUE, "Homeless",
      fifelse(active_at_start_housed_client == TRUE, "Housed",
              fifelse(return_from_perm_client == TRUE, "Returned from \nPermanent",
                      fifelse(reengaged_from_temp_client == TRUE, "Re-engaged from \nNon-Permanent",
                              fifelse(newly_homeless_client == TRUE & session$userData$days_of_data >= 1094, "First-Time \nHomeless",
                                      fifelse(newly_homeless_client == TRUE & session$userData$days_of_data < 1094, "Inflow\nUnspecified",
                                              "something's wrong")))))
    ),
    
    OutflowTypeSummary = fifelse(
      perm_dest_client | temp_dest_client | unknown_at_end_client,
      "Outflow",
      fifelse(
        homeless_at_end_client | housed_at_end_client,
        "Active at End",
        "something's wrong")
    ),
    
    OutflowTypeDetail = fifelse(
      perm_dest_client == TRUE, "Exited,\nPermanent",
      fifelse(temp_dest_client == TRUE, "Exited,\nNon-Permanent",
              fifelse(unknown_at_end_client == TRUE, "Inactive",
                      fifelse(homeless_at_end_client & !is.na(homeless_at_end_client), "Homeless",
                              fifelse(housed_at_end_client == TRUE, "Housed",
                                      "something's wrong"))))
    )
  )]
}
