# 1. Initialize previous states with current values (using isolate to prevent reactive cycles)
prev_long_stayers <- reactiveValues(
  "session$userData$valid_file" = isolate(session$userData$valid_file()),
  "input$ESNbNLongStayers"        = isolate(input$ESNbNLongStayers),
  "input$OUTLongStayers"          = isolate(input$OUTLongStayers),
  "input$ServicesOnlyLongStayers" = isolate(input$ServicesOnlyLongStayers),
  "input$OtherLongStayers"        = isolate(input$OtherLongStayers),
  "input$DayShelterLongStayers"   = isolate(input$DayShelterLongStayers),
  "input$CELongStayers"           = isolate(input$CELongStayers)
)

# 2. Group all inputs into a single reactive list
long_stayers_group <- reactive({
  list(
    "input$ESNbNLongStayers"        = input$ESNbNLongStayers,
    "input$OUTLongStayers"          = input$OUTLongStayers,
    "input$ServicesOnlyLongStayers" = input$ServicesOnlyLongStayers,
    "input$OtherLongStayers"        = input$OtherLongStayers,
    "input$DayShelterLongStayers"   = input$DayShelterLongStayers,
    "input$CELongStayers"           = input$CELongStayers
  )
})

# 3. Apply debounce to the grouped reactive expression
long_stayers_debounced <- debounce(long_stayers_group, 2000)


observeEvent(input$toggle_help, {
  shinyjs::toggleClass(id = "help_sidebar", class = "open")
})

# 2. Close via X button inside the sidebar
observeEvent(input$close_help, {
  shinyjs::removeClass(id = "help_sidebar", class = "open")
})

# Log changes to metadata
observeEvent(long_stayers_debounced(), {
  curr_values <- long_stayers_debounced()
  changed_input <- NULL
  
  # Identify which input changed and update the tracked state
  for (input_name in names(curr_values)) {
    if (!identical(curr_values[[input_name]], prev_long_stayers[[input_name]])) {
      changed_input <- input_name
      prev_long_stayers[[input_name]] <- curr_values[[input_name]]
    }
  }
  logMetadata(session, paste0("Changed long stayer setting: ", changed_input))
})

## DQ - Long Stayers
get_long_stayers <- function() {
  tryCatch(
    session$userData$long_stayers %>%
      fmutate(
        too_many_days = fcase(
          ProjectType == es_nbn_project_type, input$ESNbNLongStayers,
          ProjectType == out_project_type, input$OUTLongStayers,
          ProjectType == sso_project_type, input$ServicesOnlyLongStayers,
          ProjectType == other_project_project_type, input$OtherLongStayers,
          ProjectType == day_project_type, input$DayShelterLongStayers,
          ProjectType == ce_project_type, input$CELongStayers,
          default = 99999
        )
      ) %>%
      fsubset(DaysSinceLastKnown > too_many_days) %>%
      fselect(vars_we_want) %>%
      fmutate(Type = factor(Type, levels = issue_levels)),
    error = function(e){e}
  )
}

get_outstanding_referrals <- function() {
  tryCatch(
    session$userData$outstanding_referrals %>%
      fsubset(input$CEOutstandingReferrals < Days) %>%
      merge_check_info(checkIDs = 100) %>%
      fselect(vars_we_want) %>%
      fmutate(Type = factor(Type, levels = issue_levels)),
    error = function(e){e}
  )
}
long_stayers_tc <- reactiveVal(NULL)
outstanding_referrals_tc <- reactiveVal(NULL)

observeEvent(input$update_dq, {
  long_stayers_tc(get_long_stayers())
  outstanding_referrals_tc(get_outstanding_referrals())
  
  show_alert("Long Stayer and CE Outstanding Referral DQ checks have been updated!", "Your Long Stayer and CE Outstanding Referral DQ checks have been updated", type = "success")
  shinyjs::toggleState("update_dq", condition = FALSE)
})


## System Performance - days_lh_valid
set_user_data_lh_info <- function() {
  session$userData$lh_info <- get_lh_info(
    session$userData$enrollment_categories, 
    session$userData$lh_cls,
    session$userData$Services,
    session$userData$ReportStart, 
    session$userData$ReportEnd
  )
  
  cols_to_overwrite <- c(
    "first_lh_date", 
    "last_lh_date", 
    "EntryDate_orig",
    "ExitAdjust_orig", 
    "adjusted_dates"
  )
  session$userData$enrollment_categories <- trim_entry_exit(
    session$userData$enrollment_categories %>% get_vars(setdiff(names(.), cols_to_overwrite)),
    session$userData$lh_info
  )
  
  # Force run/calculate period_specific_data reactive
  # Better to do it up-front than while charts are loading
  period_specific_data()
  
  session$userData$lh_info_prev <- get_lh_info(
    session$userData$enrollment_categories_prev, 
    session$userData$lh_cls,
    session$userData$Services,
    session$userData$ReportStart, 
    session$userData$ReportEnd
  )
  
  session$userData$enrollment_categories_prev <- trim_entry_exit(
    session$userData$enrollment_categories_prev %>% get_vars(setdiff(names(.), cols_to_overwrite)),
    session$userData$lh_info_prev
  )
}
observeEvent(input$update_sys_perf, {
  req(session$userData$valid_file() == 1)
  
  curr_values <- long_stayers_debounced()
  changed_input <- NULL
  browser()
  # Identify which input changed and update the tracked state
  for (input_name in names(curr_values)) {
    if (!identical(curr_values[[input_name]], prev_long_stayers[[input_name]])) {
      changed_input <- input_name
      prev_long_stayers[[input_name]] <- curr_values[[input_name]]
    }
  }
  
  # Log the metadata with the specific input that changed
  if (!is.null(changed_input)) {
    browser()
  }
  
  set_user_data_lh_info()
  
  show_alert("System Performance Updated!", "Your System Performance dashboard has been updated", type = "success")
  shinyjs::toggleState("update_sys_perf", condition = FALSE)
}, ignoreInit = FALSE, ignoreNULL = TRUE)

observeEvent(long_stayers_debounced(), {
  show <- session$userData$valid_file() == 1
  shinyjs::toggleState("update_dq", condition = show)
  shinyjs::toggleState("update_sys_perf", condition = show)
})