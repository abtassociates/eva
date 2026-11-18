suppression_msg <- "The chart cannot be displayed because there are fewer than 11 clients."
no_data_msg <- "No data to show."
no_valid_data_msg <- "No valid data to show."
all_data_suppressed_msg <- "The chart will not display because all data has been suppressed."

sys_perf_validations <- list(
  syso = list(
    "demo"   = "syso_chart_validation_comp",
    "flow"   = c("syso_chart_validation_flow", "syso_chart_validation_mbm"),
    "status" = "syso_chart_validation_status"
  ),
  syse = list(
    "type"   = "syse_chart_validation_type",
    "time"   = "syse_chart_validation_time",
    "subpop" = "syse_chart_validation_subpop",
    "phd"    = "syse_chart_validation_phd"
  )
)

eval_chart_validity <- function(counts, min_count = 10) {
  # # 1. Check Master Data directly
  # AS 9/30/26: Is this needed or do we handle this in 07?
  master_df <- session$userData$enrollment_categories
  if (is.null(master_df) || fnrow(master_df) == 0) {
    return(list(valid = FALSE, message = no_valid_data_msg))
  }

  # Flatten to a numeric vector (handles single number or c(n1, n2))
  counts <- unlist(counts)
  
  # 2. Check if any count is zero or missing
  if (length(counts) == 0 || any(is.na(counts)) || any(counts == 0)) {
    return(list(valid = FALSE, message = no_data_msg))
  }
  
  # 3. Check suppression threshold (> 10)
  if (any(counts <= min_count)) {
    return(list(valid = FALSE, message = suppression_msg))
  }
  
  # Passed all validations
  list(valid = TRUE, message = NULL)
}

# Shiny UI wrapper for renderPlot / renderDT
validate_chart <- function(val) {
  validate(need(val$valid, message = val$message))
}

# VALIDATIONS -------------
syse_chart_validation_type <- reactive({
  eval_chart_validity(fnrow(all_filtered_syse()))
})
syse_chart_validation_time <- reactive({
  eval_chart_validity(fnrow(all_filtered_syse_time()))
})

syse_chart_validation_subpop <- reactive({
  eval_chart_validity(list(fnrow(subpop()), fnrow(everyone_else())))
})

syse_chart_validation_phd <- reactive({
  eval_chart_validity(list(fnrow(syse_phd_raw_exits()), fnrow(syse_phd_ph_exits())))
})


# VALIDATIONS --------------

syso_chart_validation_comp <- reactive({
  eval_chart_validity(fnrow(syso_comp_clean_df()))
})
syso_chart_validation_status <- reactive({
  eval_chart_validity(fnrow(get_sankey_data_raw()))
})
syso_chart_validation_flow <- reactive({
  eval_chart_validity(fnrow(get_inflow_outflow_full()))
})
syso_chart_validation_mbm <- reactive({
  eval_chart_validity(fnunique(
    get_inflow_outflow_monthly()$PersonalID
  ))
})

syso_chart_validation_mbm_inactive <- reactive({
  eval_chart_validity(fnunique(
    get_inflow_outflow_monthly()[OutflowTypeDetail == "Inactive"]$PersonalID
  ))
})

syso_chart_validation_mbm_fth <- reactive({
  eval_chart_validity(fnunique(
    get_inflow_outflow_monthly()[InflowTypeDetail == "First-Time Homeless"]$PersonalID
  ))
})
