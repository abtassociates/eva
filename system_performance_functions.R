
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

sys_export_summary_initial_df <- function(type = 'overview') {
  
  tabbox <- ifelse(type == 'overview', input$syso_tabbox, input$syse_tabbox)
  
  logMetadata(session, glue("Downloaded System {ttype} Tabular Data: {tabbox}{demotext}", 
                            ttype=str_to_title(type),
                            demotext = if_else(isTruthy(T), " - DEMO MODE", "")))
  
  df <- data.frame(
    Chart = c(
      "Start Date",
      "End Date",
      "Methodology Type",
      "Household Type",
      "Level of Detail",
      "Project Type Group"
    )
  )
  
  values <- switch(type,
                   'overview' = c(
                     strftime(session$userData$ReportStart, "%m/%d/%y"),
                     strftime(session$userData$ReportEnd, "%m/%d/%y"),
                     getNameByValue(sys_methodology_types, input$syso_methodology_type),
                     getNameByValue(sys_hh_types, input$syso_hh_type),
                     getNameByValue(sys_level_of_detail, input$syso_level_of_detail),
                     getNameByValue(sys_project_types, input$syso_project_type)
                   ),
                   'exits' = c(
                     strftime(session$userData$ReportStart, "%m/%d/%y"),
                     strftime(session$userData$ReportEnd, "%m/%d/%y"),
                     getNameByValue(sys_methodology_types, input$syse_methodology_type),
                     getNameByValue(sys_hh_types, input$syse_hh_type),
                     getNameByValue(sys_level_of_detail, input$syse_level_of_detail),
                     getNameByValue(sys_project_types, input$syse_project_type)
                   ))
    
  df$Value <- values
  return(df)
}

sys_export_filter_selections <- function(type = 'overview') {
  
  selections <- tibble(
    Chart = c(
      "Age",
      "Veteran Status",
      "Race/Ethnicity"
    ))
  
  values <- switch(type,
      'overview' = c(
          if(identical(sys_age_cats, input$syso_age)) {"All Ages"} else {paste(input$syso_age, collapse=", ")},
          getNameByValue(sys_spec_pops_people, input$syso_spec_pops),
          getNameByValue(sys_race_ethnicity_cats(input$syso_methodology_type), input$syso_race_ethnicity)
        ),
      'exits' = c(
          if(identical(sys_age_cats, input$syse_age)) {"All Ages"} else {paste(input$syse_age, collapse=", ")},
          getNameByValue(sys_spec_pops_people, input$syse_spec_pops),
          getNameByValue(sys_race_ethnicity_cats(input$syse_methodology_type), input$syse_race_ethnicity)
      )
  )
  
  selections$Value <- values
  
  return(selections)
}

suppress_values <- function(.data, count_var) {
  return(mutate(
    .data,
    wasRedacted = between(!!sym(count_var), 1, 10),!!count_var := ifelse(!!sym(count_var) <= 10, NA, !!sym(count_var))
  ))
}

# Suppression Rule 2: If only one cell in a group (i.e. row and/or column) is suppressed,
# then suppress the next lowest value in that group
suppress_next_val_if_one_suppressed_in_group <- function(.data, group_v, n_v) {
  if(length(input$system_composition_selections) > 1) {
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

# this gets all the categories of the selected variable
# this is used to make sure even empty categories are included in the chart
get_selection_cats <- function(selection,type = 'overview') {
  
  methodology_type <- switch(type,
                             'overview' = input$syso_methodology_type,
                             'exits' = input$syse_methodology_type)
  return(
    switch(
      selection,
      "Age" = sys_age_cats,
      "All Races/Ethnicities" = get_race_ethnicity_vars("All", methodology_type = methodology_type, 
                                                        race_ethnicity_func = sys_race_ethnicity_cats),
      "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped", methodology_type = methodology_type, 
                                                            race_ethnicity_func = sys_race_ethnicity_cats),
      #"Domestic Violence" = sys_dv_pops, VL 9/20/24: Not including for launch
      # Update Veteran status codes to 1/0, because that's how the underlying data are
      # we don't do that in the original hardcodes.R list 
      # because the character versions are needed for the waterfall chart
      "Veteran Status (Adult Only)" = {
        sys_veteran_pops$Veteran <- 1
        sys_veteran_pops$`Non-Veteran/Unknown` <- 0
        sys_veteran_pops
      }
      # "Homelessness Type" = c("Homelessness Type1", "Homelessness Type2") # Victoria, 8/15/24: Not including this for Launch
    )
  
  )
}

get_sys_plot_df_1var <- function(comp_df, var_col, selection = input$system_composition_selections) {
  # if number of variables associated with selection > 1, then they're dummies

  if (length(var_col) > 1) {
    plot_df <- comp_df %>%
      pivot_longer(
        cols = -PersonalID,
        names_to = selection,
        values_to = "value"
      ) %>%
      group_by(!!sym(selection)) %>%
      summarize(n = sum(value, na.rm = TRUE), .groups = 'drop')
  } else {
    plot_df <- as.data.frame(table(comp_df[[var_col]]))
    names(plot_df) <- c(selection, "n")
    
    if(selection == "Domestic Violence Status") {
      plot_df <- plot_df %>% bind_rows(tibble(
        `Domestic Violence Status` = "DVTotal",
        n = sum(plot_df %>% 
                  filter(`Domestic Violence Status` != "NotDV") %>%
                  pull(n), na.rm = TRUE)))
    }
  }
  return(plot_df)
}

remove_non_applicables <- function(.data, selection = input$system_composition_selections) {
  # remove children when vets is selected - since Vets can't be children
  if("Veteran Status (Adult Only)" %in% selection) {
    .data %>% filter(!(AgeCategory %in% c("0 to 12", "13 to 17")))
  } 
  # filter to just HoHs and Adults for DV
  else if ("Domestic Violence status" %in% selection) {
    .data %>% filter(!(AgeCategory %in% c("0 to 12", "13 to 17")) | CorrectedHoH == 1)
  } else {
    .data
  }
}

toggle_sys_components <- function(prefix = 'sys', cond, init=FALSE) {
  # 1. toggles the filters (disabled for Composition)
  # 2. toggles subtabs and download button based if valid file has been uploaded
  # 3. moves download button to be in line with subtabs
  tabs <- switch(prefix,
                 'sys' = c(
                   "System Flow" = "inflow_outflow",
                   "Client System Status" = "status",
                   "System Demographics" = "comp"
                 ),
                 'syse' = c(
                   "System Exit Types" = "types",
                   "System Exit Comparisons" = "compare",
                   "Permanent Housing Demographics" = "phd"
                 )
  )
  
  prefix4 <- ifelse(prefix == 'sys', 'syso',prefix)
  prefixnone <- ifelse(prefix == 'sys', '','syse_')
  
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