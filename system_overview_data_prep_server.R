
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
  # assign("upload_name", upload_name, envir=menv)
  # assign("input_hh_type", input$syso_hh_type, envir=menv)
  # assign("input_level_detail", input$syso_level_of_detail, envir=menv)
  # assign("input_project_type", input$syso_project_type,envir= menv)
  # assign("client_categories_filt", client_categories_filtered(), envir=menv)
  
  
  # results <- mirai_map(
  results <- lapply(
    session$userData$report_dates,
    function(period) {
      # all_filtered <- universe_filtered_mirai(period, upload_name, client_categories_filt)
      # custom_rprof({
      all_filtered <- universe_filtered(period, upload_name)
      universe_w_enrl_flags <- universe_enrl_flags(all_filtered, period)
      universe_w_ppl_flags <- universe_ppl_flags(universe_w_enrl_flags, period)

      # Add month flag for month-periods
      if(!identical(period, session$userData$report_dates[["Full"]])) {
        universe_w_ppl_flags[, month := as.Date(period[1])]
      }
      # }, "system_overview_data_prep_server.R")
      # browser()
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
  # browser()
  join(
    session$userData$get_period_specific_enrollment_categories(period, upload_name) %>%
      fsubset(eecr | lecr | lookback),
    session$userData$get_period_specific_nbn_enrollment_services(period, upload_name), 
    on = "EnrollmentID",
    verbose = FALSE
  ) %>%
  join( # Inner Join with client categories
    # This is necessary for bringing in Veteran Status, but will also make the rest faster
    client_categories_filtered(),
    on = "PersonalID",
    how = "inner",
    verbose = FALSE
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

universe_filtered_mirai <- function(period, upload_name, client_categories_filt) {
  join(
    session$userData$get_period_specific_enrollment_categories(period, upload_name) %>%
      fsubset(eecr | lecr | lookback),
    session$userData$get_period_specific_nbn_enrollment_services(period, upload_name), 
    on = "EnrollmentID",
    verbose = FALSE
  ) %>%
  join( # Inner Join with client categories
    # This is necessary for bringing in Veteran Status, but will also make the rest faster
    client_categories_filt,
    on = "PersonalID",
    how = "inner",
    verbose = FALSE
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