
# reactives ---------------------------------------------------------------

syse_level_of_detail_text <- reactive({
  case_when(
    input$syse_level_of_detail == "All" ~ "People",
    input$syse_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(sys_level_of_detail, input$syse_level_of_detail)
  )
})

tree_exits_data <- reactive({
  all_filtered_syse()  %>% 
    fselect( Destination, PersonalID, EnrollmentID) %>% 
    add_destination_type()
})

everyone <- reactive({
  all_filtered_syse_time() %>% 
    add_destination_type(as_factor = TRUE)
})


# functions ---------------------------------------------------------------

display_syse_counts <- function(){
  c(
    paste0(
      "Total ", 
      syse_level_of_detail_text(),
      " with System Exits",
      if_else(
        input$syse_hh_type == "All",
        "",
        paste0(" in ",
               str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
               " Households")
      )
    ),
    paste0(
      "Total ", 
      syse_level_of_detail_text(),
      " with PH System Exits",
      if_else(
        input$syse_hh_type == "All",
        "",
        paste0(" in ",
               str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
               " Households")
      )
    )
  )
}

syse_total_count_display <- function(total_count, total_ph_count) {
  return(paste0(
    str_wrap(
      paste0(
        display_syse_counts(),
        ": ",
        scales::comma(c(total_count, total_ph_count))
      ),
      width = 40
    ),collapse='',
    "\n")
  )
}

# Client-level flags, filtered ----------------------------------------------------
syse_client_categories_filtered <- reactive({
  
  logToConsole(session, "In syse_client_categories_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  req(nrow(session$userData$client_categories) > 0)
  
  session$userData$client_categories[
    AgeCategory %in% input$syse_age &
      (if(input$syse_race_ethnicity == "All") rep(TRUE, .N) else get(input$syse_race_ethnicity) == 1) & 
      (
        input$syse_spec_pops == "None" |
          (input$syse_spec_pops == "Veteran" &
             VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
          (input$syse_spec_pops == "NonVeteran" &
             VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17"))
      )
  ]
})

get_system_exits <- function(
    enrl, 
    clients, 
    ctype, 
    reportStart = session$userData$ReportStart, 
    reportEnd = session$userData$ReportEnd, 
    append_client_info = FALSE,
    client_categories = NULL
) {
  df <- join( 
    enrl,
    clients,
    on = "PersonalID",
    how = "inner"
  ) 
  
  df <- df %>% 
    expand_by_periods(chart_type = ctype, reportStart = reportStart, reportEnd = reportEnd) %>% 
    get_active_info(df, reportStart = reportStart, reportEnd = reportEnd) %>%
    get_inflows_and_outflows(chart_type = 'exits', reportStart = reportStart, reportEnd = reportEnd) %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive'))
  
  # Though we merge in clients in `get_system_exits`, we do so only to obtain 
  # VeteranStatus for use in filtering enrollments by HouseholdType
  if(append_client_info && !is.null(client_categories))
    df <- join( 
      df,
      client_categories,
      on = "PersonalID",
      how = "inner"
    )
  
  df
}

# Dataset for System Exits by (Destination) Type
# Contains system exits, filtered by user-selected enrollment and client filters
all_filtered_syse <- reactive({
  logToConsole(session, "in all_filtered_syse")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  get_system_exits(
    syse_enrollments_filtered(),
    syse_client_categories_filtered(), 
    ctype = 'exits_types',
    reportStart = session$userData$ReportStart,
    reportEnd = session$userData$ReportEnd
  )
})

# Dataset for System Exits by Year
# Contains system exits, filtered by user-selected enrollment and client filters
# Compares these for current and previous years
all_filtered_syse_time <- reactive({
  logToConsole(session, "in all_filtered_syse_time")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode)) 
  
  period_data_cur <- all_filtered_syse() %>% 
    fmutate(period = "Current Year")
  
  period_data_prev <- get_system_exits(
    syse_enrollments_filtered_prev(), # Uses cached reactive
    syse_client_categories_filtered(), 
    ctype = 'exits_types', 
    reportStart = session$userData$ReportStart %m-% years(1), 
    reportEnd = session$userData$ReportEnd %m-% years(1)
  ) %>% 
    fmutate(period = "Previous Year")
  
  rowbind(period_data_cur, period_data_prev)
})

# Dataset for System Exits by Demographic (Age, (All/Grouped) Race/Ethnicity), Veteran Status)
# Contains system exits, filtered by user-selected enrollment filters
# Re-joins clients to get all client info
all_filtered_syse_demog <- reactive({
  logToConsole(session, "in all_filtered_syse_demog")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  get_system_exits(
    syse_enrollments_filtered(),
    session$userData$client_categories, 
    ctype = 'exits_demog',
    reportStart = session$userData$ReportStart,
    reportEnd = session$userData$ReportEnd,
    append_client_info = TRUE,
    client_categories = session$userData$client_categories
  )
})

# Dataset for System Exits by Subpop (HouseholdType and/or up to 2 of Age, Race/Ethnicity, Veteran Status)
# Contains system exits, filtered by user-selected enrollment filters (except HH Type)
# Re-joins clients to get all client info
# Re-joins filtered enrollments to get HouseholdType
all_filtered_syse_subpop <- reactive({
  logToConsole(session, "in all_filtered_syse_subpop")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
 
  get_subpop_exits <- function(filter_hh_type) {
    enrl_filtered <- get_enrollments_filtered(
      enrollment_cats      = session$userData$enrollment_categories,
      client_cats          = session$userData$client_categories,
      syse_hh_type         = input$syse_subpop_hh_type, # Dynamic input access
      syse_level_of_detail = input$syse_level_of_detail,
      syse_project_type    = input$syse_project_type,
      filter_hh_type       = filter_hh_type
    )
    
    get_system_exits(
      enrl_filtered,
      session$userData$client_categories, 
      ctype = 'exits_types',
      reportStart = session$userData$ReportStart,
      reportEnd = session$userData$ReportEnd,
      append_client_info = TRUE,
      client_categories = session$userData$client_categories
    ) 
  }
  
  out_subpop <- get_subpop_exits(filter_hh_type = TRUE) |>
    fmutate(meets_ev_else = FALSE)
  
  if (input$syse_subpop_hh_type != "All") {
    out_oth_hh_types <- get_subpop_exits(filter_hh_type = FALSE) %>%
      fmutate( 
        meets_ev_else = 
          (input$syse_subpop_hh_type == "YYA" & !(HouseholdType %in% c("PY", "UY","CO"))) |
          (input$syse_subpop_hh_type == "AO" & !(HouseholdType %in% c("AOminusUY","UY"))) |
          (input$syse_subpop_hh_type == "AC" & !(HouseholdType %in% c("ACminusPY","PY"))) |
          (!(input$syse_subpop_hh_type %in% c("YYA","AO","AC")) & input$syse_subpop_hh_type != HouseholdType)
      ) %>% 
      fsubset(meets_ev_else)

    rowbind(out_subpop, out_oth_hh_types)
  } else {
    out_subpop
  }
  
})

full_unit_of_analysis_display_syse <- reactive({
  display_syse_counts()
})


# observeEvent ------------------------------------------------------------


## hide demographic filters when on PHD subtab
observeEvent(input$syse_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  if(input$syse_tabbox %in% c('<h4>Exits to PH Demographics</h4>','<h4>Exits by Subpopulation</h4>')){
    shinyjs::hide('syse_spec_pops')
    shinyjs::hide('syse_age')
    shinyjs::hide('syse_race_ethnicity')
  } else {
    shinyjs::show('syse_spec_pops')
    shinyjs::show('syse_age')
    shinyjs::show('syse_race_ethnicity')
  }
  
  # Household Type is not a filter for Exits by Subpopulation
  shinyjs::toggle("syse_hh_type", condition = input$syse_tabbox != '<h4>Exits by Subpopulation</h4>')
})

observeEvent(input$syse_types_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox, " - ", input$syse_types_subtabs,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$syse_time_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox, " - ", input$syse_time_subtabs,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$syse_subpop_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox, " - ", input$syse_subpop_subtabs,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(input$syse_phd_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox, " - ", input$syse_phd_subtabs,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$syse_methodology_type, {
  
  updatePickerInput(
    session, 
    "syse_race_ethnicity", 
    choices = sys_race_ethnicity_cats(input$syse_methodology_type)
  )
  
  # update System Exits Grouped Races/Ethnicities label
  grouped_re_lbl_new <- ifelse(input$syse_methodology_type == 1, "Grouped", "Hispanic-Focused")
  shinyjs::runjs(
    glue("
      $('#syse_phd_selections input[value=\"Grouped Races/Ethnicities\"] + span').text('{grouped_re_lbl_new} Races/Ethnicities');
    ")
  )
},
ignoreInit = TRUE)

toggle_sys_components(prefix='syse', FALSE, init=TRUE) # initially hide them

# Then, create your specific reactives:
syse_enrollments_filtered <- create_filtered_enrollments_reactive("syse")
syse_enrollments_filtered_prev <- create_filtered_enrollments_reactive("syse", prev_yr = TRUE, TRUE)
syse_enrollments_filtered_no_hh <- create_filtered_enrollments_reactive("syse", filter_hh_type = FALSE)

