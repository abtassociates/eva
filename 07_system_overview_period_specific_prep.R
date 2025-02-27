# These steps and functions process data for a given period, which can be the full
# reporting period, or each month in between the start and end of the full period

# Get period report_dates --------------------------------------------
months_in_report_period <- seq.Date(from = ReportStart(), to = ReportEnd(), by = "months")
report_dates <- c(
  list("Full" = c(ReportStart(), ReportEnd())),
  setNames(
    lapply(months_in_report_period, function(d) {
      c(d, ceiling_date(d, "month") - days(1))
    }),
    months_in_report_period
  )
)

# Enrollments-level flags, filtered---------------------------------------------
enrollment_categories_filtered_df <- function(period, hh_type, level_detail, project_type, upload_name) {
  # Get period-specific data (can be memoized as discussed earlier)
  enrollment_categories_df <- session$userData$get_period_specific_enrollment_categories(period, upload_name)
  
  # Apply all filters at once and select needed columns
  enrollment_categories_df %>%
    filter(
      # Household type filter
      (hh_type == "All" |
         (hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
         (hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
         (hh_type == "AO" & HouseholdType %in% c("AO","UY")) | 
         (hh_type == "AC" & HouseholdType %in% c("AC","PY")) | 
         hh_type == HouseholdType
      ) &
        # Level of detail filter
        (level_detail == "All" |
           (level_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (level_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        ((project_type == "All" |
            (project_type == "Residential" &
               ProjectType %in% project_types_w_beds &
               eecr == TRUE) | eecr == FALSE) |
           ((project_type == "NonResidential" &
               ProjectType %in% non_res_project_types &
               eecr == TRUE) | eecr == FALSE))
    ) %>%
    select(
      EnrollmentID,
      PersonalID,
      ProjectType,
      EntryDate,
      MoveInDateAdjust,
      ExitAdjust,
      Destination,
      LivingSituation,
      CorrectedHoH,
      MostRecentAgeAtEntry,
      HouseholdType,
      lh_prior_livingsituation,
      lh_at_entry,
      EnrolledHomeless,
      straddles_start,
      straddles_end,
      in_date_range,
      days_to_next_entry,
      days_since_previous_exit,
      lecr,
      eecr,
      was_lh_at_start,
      was_lh_at_end,
      lookback,
      NbN15DaysAfterReportEnd,
      NbN15DaysBeforeReportEnd
    )
}

# Enrollment-level universe -----------------------
# only includes people and their lookback thru LECR enrollments

# homeless cls finder function --------------------------------------------
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

# hello weary traveler amongst these date ranges. you may find it helpful to
# find example clients and their Entry and Exit Dates and enter them into
# https://onlinetools.com/time/visualize-date-intervals <- here.
# add inflow type and active enrollment typed used for system overview plots
universe <- function(enrollments_filtered, period) {
  startDate <- period[1]
  endDate <- period[2]
  
  client_categories_filtered()[
    enrollments_filtered,
    on = .(PersonalID),
    nomatch = NULL
  ][, `:=`(
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

# Enrollment-level universe with client-level flags -----------------------
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
                              fifelse(newly_homeless_client == TRUE & days_of_data() >= 1094, "First-Time \nHomeless",
                                      fifelse(newly_homeless_client == TRUE & days_of_data() < 1094, "Inflow\nUnspecified",
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


## NbN prep ----------------------------------------------------------------
session$userData$get_period_specific_nbn_enrollment_services <- memoise::memoise(
  function(report_period, upload_name) {
    startDate <- report_period[1]
    endDate <- report_period[2]
    nbn_enrollments_services <- Services %>%
      filter(RecordType == 200) %>%
      inner_join(
        EnrollmentAdjust %>%
          filter(ProjectType == es_nbn_project_type) %>%
          select(EnrollmentID),
        join_by(EnrollmentID)
      ) %>%
      # ^ limits shelter night services to enrollments associated to NbN shelters
      mutate(
        NbN15DaysBeforeReportStart =
          between(DateProvided,
                  startDate - days(15),
                  startDate),
        NbN15DaysAfterReportEnd =
          between(DateProvided,
                  endDate,
                  endDate + days(15)),
        NbN15DaysBeforeReportEnd =
          between(DateProvided,
                  endDate - days(15),
                  endDate)
      )
    
    if(nbn_enrollments_services %>% nrow() > 0) nbn_enrollments_services <-
        nbn_enrollments_services %>%
        group_by(EnrollmentID) %>%
        summarise(
          NbN15DaysBeforeReportStart = max(NbN15DaysBeforeReportStart, na.rm = TRUE),
          NbN15DaysAfterReportEnd = max(NbN15DaysAfterReportEnd, na.rm = TRUE),
          NbN15DaysBeforeReportEnd = max(NbN15DaysBeforeReportEnd, na.rm = TRUE)) %>%
        mutate(
          NbN15DaysBeforeReportStart = replace_na(NbN15DaysBeforeReportStart, 0),
          NbN15DaysAfterReportEnd = replace_na(NbN15DaysAfterReportEnd, 0),
          NbN15DaysBeforeReportEnd = replace_na(NbN15DaysBeforeReportEnd, 0)
        ) %>%
        ungroup()
    
    nbn_enrollments_services %>%
      select(EnrollmentID,
             NbN15DaysBeforeReportStart,
             NbN15DaysAfterReportEnd,
             NbN15DaysBeforeReportEnd) %>%
      filter(NbN15DaysBeforeReportStart == 1 |
               NbN15DaysAfterReportEnd == 1 |
               NbN15DaysBeforeReportEnd == 1)
  })

## Get period-specific variables, like eecr and lecr -----------------
session$userData$get_period_specific_enrollment_categories <- memoise::memoise(
  function(report_period, upload_name) {
    startDate <- report_period[1]
    endDate <- report_period[2]
    
    # continuing the work of the base homeless_cls dataset form 07_system_overview.R 
    # we now make it period-specific, and collapse it down to the enrollment-level
    # so this contains enrollments with homeless CLS and an indicator as to 
    # whether InformationDate is within to 60 or 90 days (depending on project type, but only limited to Non-Res Project Types) 
    # from the period start/end
    # we then merge this with enrollment_categories to fully replace the homeless_cls_finder function
    # this avoids having to re-filter and do the check for each enrollment
    homeless_at_period_start <- homeless_cls[, {
      # Calculate time windows once
      start_window <- startDate - ifelse(ProjectType == ce_project_type, 90, 60)
      end_window <- endDate - ifelse(ProjectType == ce_project_type, 90, 60)
      
      info_in_start_window <- any(between(InformationDate, start_window, startDate))
      info_in_end_window <- any(between(InformationDate, end_window, endDate))
      entry_in_start_window <- between(EntryDate, start_window, startDate)
      entry_in_end_window <- between(EntryDate, end_window, endDate)
      
      .(
        was_lh_at_start = info_in_start_window |
          (entry_in_start_window & lh_prior_livingsituation) |
          (EntryDate > startDate & (lh_prior_livingsituation | info_equal_entry)),
        
        was_lh_at_end = info_in_end_window |
          (entry_in_end_window & lh_prior_livingsituation) |
          (ExitDate < endDate & (lh_prior_livingsituation | info_equal_exit))
      )
    }, by = "EnrollmentID"]
    
    e <- homeless_at_period_start[
      enrollment_categories[
        # keep enrollments in date range keep exits within the 2 yrs prior to start
        EntryDate <= endDate & ExitAdjust >= (startDate - years(2))
      ],
      on = .(EnrollmentID)
    ][, `:=`(
      straddles_start = EntryDate <= startDate & ExitAdjust >= startDate,
      straddles_end = EntryDate <= endDate & ExitAdjust >= endDate,
      in_date_range = EntryDate <= endDate & ExitAdjust >= startDate #,
      # DomesticViolenceCategory = fcase(
      #   DomesticViolenceSurvivor == 1 & CurrentlyFleeing == 1, "DVFleeing",
      #   DomesticViolenceSurvivor == 1, "DVNotFleeing",
      #   default = "NotDV"
      # )
    )][, `:=`(
      lead_EntryDate = shift(EntryDate, type = "lead"),
      lag_ExitAdjust = shift(ExitAdjust)
    ), by = PersonalID
    ][, `:=`(
      days_to_next_entry = as.numeric(difftime(lead_EntryDate, ExitAdjust, units = "days")),
      days_since_previous_exit = as.numeric(difftime(EntryDate, lag_ExitAdjust, units = "days")),
      # enrollment crossing period start/end (ecps/ecpe)
      # these are used below to construct the eecr and lecr
      ecps = in_date_range & (!(ProjectType %in% non_res_project_types) | was_lh_at_start),
      ecpe = in_date_range & (!(ProjectType %in% non_res_project_types) | was_lh_at_end)
    )][
      (days_to_next_entry < 730 | is.na(days_to_next_entry))
    ][
      # only keep folks who have an ecps, rather than also restricting to an ecpe
      # if, for Non-Res Project Types there is no ecpe, we will make the lecr the eecr
      , has_any_ecps := any(ecps), by = PersonalID
    ][
      has_any_ecps == TRUE
    ][
      # Order enrollments for selecting EECR/LECR
      order(PersonalID, -ProjectTypeWeight, EntryDate)
    ][, `:=`(
      first_ecps_idx = which.max(ecps),
      last_ecpe_idx = .N + 1 - which.max(rev(ecpe))
    ), by = PersonalID][, `:=`(
      # get the first ecps (so other ecps's become lookbacks)
      eecr = seq_len(.N) == first_ecps_idx & ecps,
      # get the last ecpe
      lecr = seq_len(.N) == last_ecpe_idx & ecpe,
      # incremental count of enrollments prior to eecr 
      lookback = first_ecps_idx - seq_len(.N)
    ), by=PersonalID][
      # set the lecr to be the eecr if there still isn't one
      # this addresses cases with Non-Res project types where the 
      # InformationDate/EntryDate doesn't fall within the 60/90 day period before the period end
      eecr & (is.na(lecr) | !lecr), lecr := TRUE
    ][
      lookback <= 1 # drop eextra lookbacks
    ]
    
    # extra filter: dropping the clients/enrollments who enter in as something 
    # other than LH, but who then have an LH CLS record at a later date
    # if(!identical(report_period, report_dates[["Full"]])) {
    #   e <- e[!(!was_lh_at_report_start & was_lh_at_start)]
    # }
    
    # missing_ids <- e[, .(has_eecr = any(eecr), has_lecr = any(lecr)), by = PersonalID][
    #   !(has_eecr | has_lecr), 
    #   PersonalID
    # ]
    # if(length(missing_ids) > 0)  {
    #   # View missing PersonalIDs
    #   browser()
    # }
    
    # this needs to be merge in case NbN dataset is empty
    merge(
      e,
      session$userData$get_period_specific_nbn_enrollment_services(report_period, upload_name), 
      by = "EnrollmentID",
      all.x = T
    )
  },
  cache = cachem::cache_mem(max_size = 100 * 1024^2) 
)

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

period_specific_data <- reactive({
  cache <- session$userData$period_cache
  
  check_cache_size(cache)
  
  cache_key <- digest::digest(list(
    input$imported$name,
    
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
  
  results <- lapply(report_dates, function(period) {
    enrollments_filtered <- enrollment_categories_filtered_df(
      period,
      input$syso_hh_type,
      input$syso_level_of_detail,
      input$syso_project_type,
      input$imported$name
    )
    # browser()
    universe_data <- universe(enrollments_filtered, period)
    universe_with_flags <- universe_ppl_flags(universe_data)
    
    if(!identical(period, report_dates[["Full"]])) {
      universe_with_flags[, month := as.Date(period[1])]
    }
    universe_with_flags
  })
  
  cache[[cache_key]] <- results

  results
})