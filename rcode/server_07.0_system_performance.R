
## This script is a place for generalized functions that cut across all system
## performance functionality - system overview, system exits, ...

# Set race/ethnicity filter options based on methodology type selection
# Set special populations options based on level of detail selection
sys_race_ethnicity_cats <- function(methodology = 1){
  if(methodology == 1) sys_race_ethnicity_method1 
  else sys_race_ethnicity_method2
}

get_race_ethnicity_vars <- function(v, methodology_type, race_ethnicity_func) {
  if (v == "All") {
    sys_race_ethnicities_all <- unlist(c(race_ethnicity_func(methodology_type)["Detailed"],"Unknown" = "RaceEthnicityUnknown"))
    names(sys_race_ethnicities_all) <- gsub("Detailed.", "", names(sys_race_ethnicities_all))
    return(sys_race_ethnicities_all)
  } else if (v %in% c("Grouped")) {
    sys_race_ethnicities_grouped <- unlist(c(race_ethnicity_func(methodology_type)["Summarized"], "Unknown" = "RaceEthnicityUnknown"))
    names(sys_race_ethnicities_grouped) <- gsub("Summarized.", "", names(sys_race_ethnicities_grouped))
    return(sys_race_ethnicities_grouped)
  }
}

# Display Filter Selection in Detail Box ----------------------------------

chart_selection_detail_line <- function(detail_label, val_list, inputVal) {
  return(
    HTML(glue(
      "<strong>{detail_label}:</strong> {getNameByValue(val_list, inputVal)} <br>"
    ))
  )
}

get_adj_font_size <- function(font_size, isExport) {
  return(
    font_size*ifelse(isExport, sys_chart_export_font_reduction, 1)
  )
}

suppress_values <- function(.data, count_var, keep_orig_var = FALSE) {
  
  if(keep_orig_var){
    count_var_orig <- paste0(count_var, '_orig')
   
    return(
      .data %>% 
        mutate(
          !! count_var_orig := !!sym(count_var),
          wasRedacted = between(!!sym(count_var), 1, 10),
          !!count_var := ifelse(!!sym(count_var) <= 10, NA, !!sym(count_var))
        )
    )
  } else {
  return(mutate(
    .data,
    wasRedacted = between(!!sym(count_var), 1, 10),!!count_var := ifelse(!!sym(count_var) <= 10, NA, !!sym(count_var))
  ))
  }
}

# Suppression Rule 2: If only one cell in a group (i.e. row and/or column) is suppressed,
# then suppress the next lowest value in that group
suppress_next_val_if_one_suppressed_in_group <- function(.data, group_v, n_v) {
  if(length(input$syso_composition_selections) > 1) {
    .data <- .data %>% fgroup_by(group_v)
  }
  
  return(
    .data %>%
      fmutate(
        count_redacted = fsum(wasRedacted),
        next_lowest = fmin(get(n_v)),
        wasRedacted = fifelse(count_redacted == 1 & (
          (wasRedacted & is.na(n_v)) |
            (!wasRedacted & n_v == next_lowest)
        ), TRUE, wasRedacted)
      ) %>%
      fungroup() %>%
      fselect(-c(count_redacted, next_lowest))
  )
}

remove_non_applicables <- function(.data, selection = input$syso_composition_selections) {
  # remove children when vets is selected - since Vets can't be children
  if("Veteran Status (Adult Only)" %in% selection) {
    .data %>% fsubset(!(AgeCategory %in% c("0 to 12", "13 to 17")))
  } 
  # filter to just HoHs and Adults for DV
  else if ("Domestic Violence status" %in% selection) {
    .data %>% fsubset(!(AgeCategory %in% c("0 to 12", "13 to 17")) | CorrectedHoH == 1)
  } else {
    .data
  }
}

toggle_sys_components <- function(prefix = 'syso', cond, init=FALSE) {
  # 1. toggles the filters (disabled for Composition)
  # 2. toggles subtabs and download button based if valid file has been uploaded
  # 3. moves download button to be in line with subtabs
  tabs <- switch(prefix,
                 'syso' = c(
                   "System Flow" = "inflow_outflow",
                   "Client System Status" = "status",
                   "System Demographics" = "comp"
                 ),
                 'syse' = c(
                   "System Exit Types" = "types",
                   "Exits by Year" = "time",
                   "Exits by Subpopulation" = "subpop",
                   "Exits to PH Demographics" = "phd"
                 )
  )
  
  prefix4 <- ifelse(prefix == 'syso', 'syso',prefix)
  prefixnone <- ifelse(prefix == 'syso', '','syse_')
  
  for (tab in tabs) {
    shinyjs::toggle(glue('{prefix}_{tab}_subtabs'), condition = cond)
    shinyjs::toggle(selector = glue('#{prefix}_{tab}_subtabs + div.tab-content'), condition = cond)
    
    shinyjs::toggle(glue('{prefix}_{tab}_download_btn'), condition = cond)
    shinyjs::toggle(glue('{prefix}_{tab}_download_btn_ppt'), condition = cond)
    
    # move download button to subtab row and only show if there's data
    if(init) {
      shinyjs::runjs(
        glue("
            document.getElementById('{prefix}_{tab}_subtabs')
              .insertAdjacentHTML('beforeEnd', '<li class=\"sys_download_tab\" id=\"{prefix}_{tab}_download_tab\"></li>');
            $('#{prefix}_{tab}_download_btn').appendTo('#{prefix}_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
            $('#{prefix}_{tab}_download_btn_ppt').appendTo('#{prefix}_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
          ")
      )
    }
  }
  
    shinyjs::toggle(glue('{prefixnone}client_level_download_btn'), condition = cond)
    if(init) {
      shinyjs::runjs(glue("
      document.getElementById('{prefix4}_tabbox')
        .insertAdjacentHTML('beforeEnd', '<li class=\"sys_download_tab\" id=\"{prefixnone}client_level_download_tab\"></li>');
      $('#{prefixnone}client_level_download_btn').appendTo('#{prefixnone}client_level_download_tab')
        .toggle('{cond}' == 'TRUE');
    "))
    }
 
  
}

#### DISPLAY FILTER SELECTIONS ###
sys_detailBox <- function(
    selection = NULL,
    detail_type = 'overview',
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
) {
  
  # Date Range
  date_range <- list(
    strong(fifelse(detail_type == 'time', "Current Year Date Range: ","Date Range: ")),
    format(startDate, "%m-%d-%Y"), " to ", format(endDate, "%m-%d-%Y"), br()
  )
   
  if(detail_type == 'time') { 
    date_range <- c(
      date_range, 
      list(
        strong("Previous Year Date Range: "),
        format(startDate - years(1) , "%m-%d-%Y"), " to ", format(endDate - years(1), "%m-%d-%Y"), br()
      )
    )
  } else if (detail_type == 'subpop') {
    subpop_mini_header <- list(HTML("<b>Subpopulation Selections</b> <br>") )
  }
  
  if(!is.null(race_eth) && (race_eth != "All" | (!is.null(selection) & !is.na(methodology_type)))){
    
    race_eth_methodology_type <- list(
      #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
      HTML(glue(
        "<b>Race/Ethnicity Methodology Type:</b> {str_sub(getNameByValue(sys_methodology_types , methodology_type), start = 1, end = 8)} <br>"
      ))
    )
    
  } else {
    race_eth_methodology_type <- NULL
  }
  
  if (cur_project_types != "All"){
    
    project_type <- c(
      list(chart_selection_detail_line("Project Type Group", sys_project_types, str_remove(cur_project_types, "- ")))
    )
  } else {
    project_type <- NULL
  }
  
  # For System Comp/Demographics and System Exits, the demographic items to display 
  # are which checkboxes user selected for the chart. Otherwise, they're the selected filter values
  demographics <- if(!is.null(selection) & detail_type != 'subpop') {
    list(
      HTML(glue("<strong>Selections</strong>: {paste(selection, collapse=' and ')} <br>"))
    )
  } else if (detail_type == 'subpop'){
    list(
      if (length(age) != length(sys_age_cats))
        HTML(glue(
          "<div style='text-indent: 20px;'><b>Age:</b> {paste(age, collapse = ', ')}</div>"
        )),
      
      if (!is.null(race_eth) && race_eth != "All")
        div(style='text-indent: 20px;',
            chart_selection_detail_line("Race/Ethnicity", sys_race_ethnicity_cats(methodology_type), race_eth)),
      
      if(getNameByValue(sys_spec_pops_people, spec_pops) != "All Statuses")
        HTML(glue(
          "<div style='text-indent: 20px;'><b>Veteran Status:</b> {paste(getNameByValue(sys_spec_pops_people, spec_pops), '(Adult Only)')}</div>"
        ))
    )
  } else {
    list(
      if (length(age) != length(sys_age_cats))
        HTML(glue(
          "<b>Age:</b> {paste(age, collapse = ', ')} <br>"
        )),
      
      if (!is.null(race_eth) && race_eth != "All")
        chart_selection_detail_line("Race/Ethnicity", sys_race_ethnicity_cats(methodology_type), race_eth),
      
      if(getNameByValue(sys_spec_pops_people, spec_pops) != "All Statuses")
        HTML(glue(
          "<b>Veteran Status:</b> {paste(getNameByValue(sys_spec_pops_people, spec_pops), '(Adult Only)')} <br>"
        ))
    )
  }
  
  if(detail_type == 'subpop'){
    if (!is.null(race_eth) && race_eth != "All"){
      return(c(date_range, race_eth_methodology_type, subpop_mini_header, demographics))
    } else {
      return(c(date_range, subpop_mini_header, demographics))
    }
    
  }  else {
    return(c(date_range, race_eth_methodology_type, project_type, demographics))
  }
}

toggle_download_buttons <- function(subtab = 'comp',plot_df) {
  shinyjs::toggle(glue("sys_{subtab}_download_btn"), condition = sum(plot_df$n > 10, na.rm = TRUE) > 0)
  shinyjs::toggle(glue("sys_{subtab}_download_btn_ppt"), condition = sum(plot_df$n > 10, na.rm = TRUE) > 0)
}

get_var_cols <- function(methodology_type) {
  return(
    list(
      "Age" = "AgeCategory",
      "All Races/Ethnicities" = get_race_ethnicity_vars("All", methodology_type = methodology_type, 
                                                        race_ethnicity_func = sys_race_ethnicity_cats),
      "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped", methodology_type = methodology_type, 
                                                            race_ethnicity_func = sys_race_ethnicity_cats),
      #"Domestic Violence" = "DomesticViolenceCategory", #VL 9/20/24: Not including for launch
      # "Homelessness Type" =  "HomelessnessType",# Victoria, 8/15/24: Not including this for Launch
      "Veteran Status (Adult Only)" =  "VeteranStatus"
    )
  )
}

# Period-Specific Enrollment Categories ----------------------------------------
# "expand" the dataset to get repeated rows per period (full + each month)
# then filter based on the period start and end
expand_by_periods <- function(dt, chart_type = 'mbm', reportStart = session$userData$ReportStart, reportEnd = session$userData$ReportEnd) {
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
      startDate = c(reportStart,
                    reportStart %m-% years(1)),
      endDate = c(reportEnd,
                  reportEnd %m-% years(1))
    ) %>% 
      ftransform(
        exit_cutoff = startDate %m-% years(2),
        temp_key = 1
      )
  } else {
    all_periods <- data.table(
      period = c('Full'),
      startDate = reportStart,
      endDate = reportEnd
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
    fselect(-temp_key, -exit_cutoff) %>%
    setkeyv(cols=c("PersonalID", "period", "EnrollmentID"))
}

get_enrollments_filtered <- function(
    enrollment_cats,
    client_cats,
    syse_hh_type,
    syse_level_of_detail,
    syse_project_type,
    lh_res_types = lh_residential_project_types,
    ph_types = ph_project_types,
    out_type = out_project_type,
    non_res_types = non_res_project_types,
    filter_hh_type = TRUE
) {
  
  join(
    enrollment_cats,
    client_cats %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  ) %>%
    fsubset(
      # Household type filter
      (if (filter_hh_type) {
        (syse_hh_type == "All" |
           (syse_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (syse_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (syse_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (syse_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           syse_hh_type == HouseholdType)
      } else {
        TRUE
      }) &
        
      # Level of detail filter
      (syse_level_of_detail == "All" |
       (syse_level_of_detail == "HoHsAndAdults" &
          (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
       (syse_level_of_detail == "HoHsOnly" &
          CorrectedHoH == 1)) &
      
      # Project type filter (wrapped in parentheses to preserve logical order of operations)
      (syse_project_type == "All" |
       (syse_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_res_types) |
       (syse_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_types) |
       (syse_project_type == "SO" & ProjectType == out_type) |
       (syse_project_type == "AllNonRes" & ProjectType %in% non_res_types))
    ) |>
    fselect(-VeteranStatus)
}

# A function that returns a reactive expression
create_filtered_enrollments_reactive <- function(prefix, prev_yr = FALSE, filter_hh_type = TRUE) {
  reactive({
    enrollment_cats <- if (prev_yr) session$userData$enrollment_categories_prev else session$userData$enrollment_categories
    
    get_enrollments_filtered(
      enrollment_cats      = enrollment_cats,
      client_cats          = session$userData$client_categories,
      syse_hh_type         = input[[paste0(prefix, "_hh_type")]], # Dynamic input access
      syse_level_of_detail = input[[paste0(prefix, "_level_of_detail")]],
      syse_project_type    = input[[paste0(prefix, "_project_type")]],
      filter_hh_type       = filter_hh_type
    )
  })
}