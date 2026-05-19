

#### DISPLAY FILTER SELECTIONS ###

## separate info for time chart tab since report period covers 2 years before ReportEnd


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

# Create passes-enrollment-filter flag to exclude enrollments from heatmap -------
enrollments_filtered_syse <- reactive({
  logToConsole(session, "in enrollments_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  en_unfilt <-  join(
    session$userData$enrollment_categories,
    session$userData$client_categories %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  )
  
  en_filt <- en_unfilt %>%
    fmutate(
      passes_enrollment_filters =
        # Household type filter
        (input$syse_hh_type == "All" |
           (input$syse_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (input$syse_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (input$syse_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (input$syse_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           input$syse_hh_type == HouseholdType
        ) &
        # Level of detail filter
        (input$syse_level_of_detail == "All" |
           (input$syse_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (input$syse_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        (input$syse_project_type == "All" |
           (input$syse_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
           (input$syse_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
           (input$syse_project_type == "SO" & ProjectType == out_project_type) |
           (input$syse_project_type == "AllNonRes" & ProjectType %in% non_res_project_types)
        )
    ) %>%
    fselect(-VeteranStatus)
  
  en_filt %>% 
    fsubset(passes_enrollment_filters)
  
})

enrollments_filtered_syse_prev <- reactive({
  logToConsole(session, "in enrollments_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  en_unfilt <-  join(
    session$userData$enrollment_categories_prev,
    session$userData$client_categories %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  )
  
  en_filt <- en_unfilt %>%
    fmutate(
      passes_enrollment_filters =
        # Household type filter
        (input$syse_hh_type == "All" |
           (input$syse_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (input$syse_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (input$syse_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (input$syse_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           input$syse_hh_type == HouseholdType
        ) &
        # Level of detail filter
        (input$syse_level_of_detail == "All" |
           (input$syse_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (input$syse_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        (input$syse_project_type == "All" |
           (input$syse_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
           (input$syse_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
           (input$syse_project_type == "SO" & ProjectType == out_project_type) |
           (input$syse_project_type == "AllNonRes" & ProjectType %in% non_res_project_types)
        )
    ) %>%
    fselect(-VeteranStatus)
  
  en_filt %>% 
    fsubset(passes_enrollment_filters)
  
})


all_filtered_syse <- reactive({
  logToConsole(session, "in all_filtered_syse")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  tmp <- join( 
    enrollments_filtered_syse(),
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  ) 
  
  period_data <- tmp %>% 
    expand_by_periods(chart_type = 'exits_types') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive'))
  period_data
  
})

all_filtered_syse_time <- reactive({
  logToConsole(session, "in all_filtered_syse_time")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode)) 
  
  tmp <- join( 
    enrollments_filtered_syse(),
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  ) 
  
  tmp_prev <- join( 
    enrollments_filtered_syse_prev(),
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  ) 
  
  period_data_cur <- tmp %>% 
    expand_by_periods(chart_type = 'exits_types') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive')) %>% 
    fmutate(period = "Current Year")
  
  period_data_prev <- tmp_prev %>% 
    expand_by_periods(chart_type = 'exits_types',
                      reportStart = session$userData$ReportStart %m-% years(1), 
                      reportEnd = session$userData$ReportEnd %m-% years(1)) %>% 
    get_active_info(tmp_prev, lh_info_df = session$userData$lh_info_prev,
                    reportStart = session$userData$ReportStart %m-% years(1), 
                    reportEnd = session$userData$ReportEnd %m-% years(1)) %>% 
    get_inflows_and_outflows(chart_type = 'exits',
                             reportStart = session$userData$ReportStart %m-% years(1), 
                             reportEnd = session$userData$ReportEnd %m-% years(1)) %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive')) %>% 
    fmutate(period = "Previous Year")
  
  period_data <- rowbind(period_data_cur, period_data_prev)
  
  period_data
  
})

all_filtered_syse_demog <- reactive({
  logToConsole(session, "in all_filtered_syse")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  tmp <-  enrollments_filtered_syse()
  
  period_data <- tmp %>% 
    expand_by_periods(chart_type = 'exits_demog') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive'))
  
  join( 
    period_data,
    session$userData$client_categories,
    on = "PersonalID",
    how = "inner"
  ) 
})

all_filtered_syse_subpop <- reactive({
  logToConsole(session, "in all_filtered_syse_subpop")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  
  client <- session$userData$client_categories
  
  
  tmp <- session$userData$enrollment_categories
  
  period_data <- tmp %>% 
    expand_by_periods(chart_type = 'exits_types') %>% 
    get_active_info(tmp) %>%
    get_inflows_and_outflows(chart_type = 'exits') %>% 
    fmutate(Destination = fix_missing_destination(Destination, OutflowTypeDetail)) %>% 
    fsubset(OutflowTypeDetail %in% c('Exited, Permanent','Exited, Non-Permanent', 'Inactive'))
  join( 
    period_data,
    client,
    on = "PersonalID",
    how = "inner"
  ) %>% 
    join(tmp %>% fselect(PersonalID, EnrollmentID, HouseholdType)) %>% 
    fmutate(HouseholdType = fct_collapse(HouseholdType, !!!hh_types_in_exports))
  
})


# System Exits to PH Demographics (PHD) -----------------------

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

full_unit_of_analysis_display_syse <- reactive({
  display_syse_counts()
})

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
