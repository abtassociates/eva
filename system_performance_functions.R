
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
