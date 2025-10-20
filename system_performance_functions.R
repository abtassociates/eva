
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
  
  if(type == 'exits_time'){
   
    df <- data.frame(
      Chart = c(
        "Current Year Start Date",
        "Current Year End Date",
        "Previous Year Start Date",
        "Previous Year End Date",
        "Methodology Type",
        "Household Type",
        "Level of Detail",
        "Project Type Group"
      )
    ) 
  } else {
  
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
  
  }
  
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
                   ),
                   'exits_time' = c(
                     strftime(session$userData$ReportStart, "%m/%d/%y"),
                     strftime(session$userData$ReportEnd, "%m/%d/%y"),
                     strftime(session$userData$ReportStart - years(1), "%m/%d/%y"),
                     strftime(session$userData$ReportEnd - years(1), "%m/%d/%y"),
                     getNameByValue(sys_methodology_types, input$syse_methodology_type),
                     getNameByValue(sys_hh_types, input$syse_hh_type),
                     getNameByValue(sys_level_of_detail, input$syse_level_of_detail),
                     getNameByValue(sys_project_types, input$syse_project_type)
                   ))
    
  df$Value <- values
  return(df)
}

sys_export_filter_selections <- function(type = 'overview') {
  
  if(type == 'exits_subpop'){
    selections <- tibble(
      Chart = c('Subpopulation Age', 'Subpopulation Veteran Status', 'Subpopulation Race/Ethnicity')
      )
  } else {
    selections <- tibble(
      Chart = c('Age', 'Veteran Status', 'Race/Ethnicity')
    )
  }
 
  
  values <- switch(type,
      'overview' = c(
          if(identical(sys_age_cats, input$syso_age)) {"All Ages"} else {paste(input$syso_age, collapse=", ")},
          getNameByValue(sys_spec_pops_people, input$syso_spec_pops),
          getNameByValue(sys_race_ethnicity_cats(input$syso_methodology_type), input$syso_race_ethnicity)
        ),
      'exits' = c(
          ifelse(identical(sys_age_cats, input$syse_age), "All Ages",paste(input$syse_age, collapse=", ")),
          getNameByValue(sys_spec_pops_people, input$syse_spec_pops),
          getNameByValue(sys_race_ethnicity_cats(input$syse_methodology_type), input$syse_race_ethnicity)
      ),
      'exits_subpop' = c(
        ifelse(identical(sys_age_cats, input$syse_age), "All Ages",paste(input$syse_age, collapse=", ")),
        getNameByValue(sys_spec_pops_people, input$syse_spec_pops),
        getNameByValue(sys_race_ethnicity_cats(input$syse_methodology_type), input$syse_race_ethnicity)
      )
  )
  selections$Value <- values
  
  return(selections)
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

remove_non_applicables <- function(.data, selection = input$system_composition_selections) {
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
  
  if(prefix == 'sys'){
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
 
  
}

sys_heatmap_xl_export <- function(file, 
                                  type = 'overview',
                                  methodology_type,
                                  selections,
                                  plot_df = sys_comp_plot_df,
                                  in_demo_mode = input$in_demo_mode
                                  ){
 
  #selections <- input$system_composition_selections
  v1 <- gsub("Races/Ethnicities", "Race", selections[1])
  v1 <- gsub("Veteran Status \\(Adult Only\\)", "Veteran Status", v1)
  
  # multiple selections
  # reshape so the values of v1 are the column headers and v2 are the "row headers"
  # though technically just a column
  if(length(selections) > 1) {
    v2 <- gsub("Races/Ethnicities", "Race", selections[2])
    v2 <- gsub("Veteran Status \\(Adult Only\\)", "Veteran Status", v2)
    
    # make sure R/E is the rows, not the columns
    if (v1 %in% c("All Race", "Grouped Race")) {
      selections <- c(selections[2], selections[1])
    }
    
    num_df <- plot_df() %>% #sys_comp_plot_df() %>%
      pivot(
        how = 'wider',
        names = selections[1],
        values = 'n',
        fill = list(n = 0)
      )
    
    # Create x.y% version
    pct_df <- num_df %>%
      ftransformv(vars = num_vars(., return="names"),  FUN = function(x) {
        (x / fsum(x) * 100) %>% 
          replace_na(0) %>%
          round(1) %>%
          paste0("%")
      })
    
    # create totals, but only for Method1
    if(methodology_type == 1) { 
      # create total row
      total_num_row <- num_df %>%
        summarise(!!selections[1] := "Total",
                  across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
        rename(!!selections[2] := !!selections[1])
      
      total_n <- sum(plot_df()$n, na.rm = TRUE)
      
      total_pct_row <- total_num_row %>% 
        mutate(
          across(where(is.numeric), ~ (. / total_n * 100) %>%
                   replace_na(0) %>%
                   round(1) %>%
                   paste0("%")))
      
      # Add Total Row and create a total column
      num_df <- num_df %>%
        rowbind(total_num_row) %>%
        mutate(Total = rowSums(select(., where(is.numeric)), na.rm = TRUE))
      
      pct_df <- pct_df %>% 
        rowbind(total_pct_row) %>%
        mutate(
          Total =  paste0(
            round(
              replace_na(num_df$Total / total_n * 100, 0),
              1
            ),
            "%"
          )
        )
    }
  } 
  # single selection
  else {
    num_df <- plot_df()
    
    pct_df <- num_df %>%
      fmutate(across(where(is.numeric), function(x) (x / sum(x, na.rm = TRUE) * 100) %>%
                      round(1) %>%
                      paste0("%")))  %>% 
      frename("pct" = n)
    
    if(methodology_type == 1) { 
      pct_df <- pct_df %>%
        rowbind(
          setNames(
            data.frame("Total", "100%"), 
            c(selections, "pct")
          )
        )
      num_df <- num_df %>%
        rowbind(summarise(., !!sym(selections) := "Total", n = sum(n, na.rm = TRUE)))
    }
  }
  
  if(type == 'overview'){
    if (length(selections) > 1) {
      num_tab_name <- glue("{v1} By {v2} #")
      pct_tab_name <- glue("{v1} By {v2} %")
    } else {
      num_tab_name <- glue("{v1} #")
      pct_tab_name <- glue("{v1} %")
    }
    
    write_xlsx(
      setNames(
        list(sys_comp_selections_summary(), num_df, pct_df),
        c("System Demographics Metadata", num_tab_name, pct_tab_name)
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    exportTestValues(sys_comp_df = get_people_universe_filtered())
    exportTestValues(sys_comp_report_num_df = num_df)
    exportTestValues(sys_comp_report_pct_df = pct_df)
    logMetadata(session, paste0("Downloaded System Overview Tabular Data: ", input$syso_tabbox,
                                if_else(isTruthy(in_demo_mode), " - DEMO MODE", "")))
  } else {
    if (length(selections) > 1) {
      
      tab_name <- glue(str_remove_all("{v2}By{v1}CrossTab", ' '))
    } else {
      tab_name <- glue(str_remove_all("{v1}CrossTab",' '))
    }
    write_xlsx(
      setNames(
        list(sys_phd_selections_summary(), sys_phd_export()),
        c("System Exits Metadata", tab_name)
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    exportTestValues(sys_phd_df = sys_phd_export())
    
    logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
                                if_else(isTruthy(in_demo_mode), " - DEMO MODE", "")))
  }
  
}

# PowerPoint Export -------------------------------------------------------
## joint ppt export function for System Overview and System Exits
sys_perf_ppt_export <- function(file,
                                type = 'overview',
                                title_slide_title,
                                summary_items,
                                plots,
                                summary_font_size,
                                startDate = session$userData$ReportStart, 
                                endDate = session$userData$ReportEnd, 
                                sourceID = session$userData$Export$SourceID,
                                in_demo_mode = input$in_demo_mode) {
  
  if(type == 'overview'){
    logMetadata(session, paste0("Downloaded System Overview Powerpoint: ", title_slide_title,
                                if_else(isTruthy(in_demo_mode), " - DEMO MODE", "")))
  } else if (type == 'exits'){
    logMetadata(session, paste0("Downloaded System Exits Powerpoint: ", title_slide_title,
                                if_else(isTruthy(in_demo_mode), " - DEMO MODE", "")))
  }
  
  #NEED TO UPDATE - if want to get more granular, need to detect with title slide
  
  report_period <- paste0("Report Period: ", 
                          format(startDate, "%m/%d/%Y"),
                          " - ",
                          format(endDate, "%m/%d/%Y")
  )
  loc_title <- ph_location_type(type = "title")
  loc_footer <- ph_location_type(type = "ftr")
  loc_dt <- ph_location_type(type = "dt")
  loc_slidenum <- ph_location_type(type = "sldNum")
  loc_body <- ph_location_type(type = "body")
  loc_subtitle <- ph_location_type(type = "subTitle")
  loc_ctrtitle <- ph_location_type(type = "ctrTitle")
  
  fp_normal <- fp_text(font.size = summary_font_size)
  fp_title <- fp_text(font.size = ppt_chart_title_font_size)
  fp_bold <- update(fp_normal, bold = TRUE)
  fp_red <- update(fp_normal, color = "red")
  
  ppt <- read_pptx(here("system_pptx_template.pptx"))
  
  add_footer <- function(.ppt) {
    return(
      .ppt %>%
        ph_with(value = paste0("CoC Code: ", sourceID), location = loc_footer) %>%
        ph_with(value = report_period, location = loc_dt) %>%
        ph_with(
          value = paste0(
            "Export Generated: ",
            format(Sys.Date()),
            "\n",
            "https://hmis.abtsites.com/eva/"
          ),
          location = loc_slidenum
        )
    )
  }
  
  # title Slide
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme") %>%
    ph_with(value = title_slide_title, location = loc_ctrtitle) %>%
    ph_with(value = "Eva Image Export", location = loc_subtitle) %>%
    add_footer()
  
 
  
  if(type == 'exits_comparison'){
    for(summary_slide_title in names(summary_items)) {
      p <- summary_items[[summary_slide_title]]
      if(!is.null(p)) {
        
        s_items <- do.call(block_list, lapply(1:nrow(p), function(i) {
          fpar(
            ftext(paste0(p$Chart[i], ": ", p$Value[i]), fp_normal)
          )
        }))
        
        add_slide(ppt, layout = "Title and Content") %>%
          ph_with(value = summary_slide_title, location = loc_title) %>%
          ph_with(
            value = s_items,
            level_list = c(rep(1L, length(s_items))),
            location = loc_body
          ) %>% 
          add_footer()
      }
    }
  } else {
    # Summary
    s_items <- do.call(block_list, lapply(1:nrow(summary_items), function(i) {
      fpar(
        ftext(paste0(summary_items$Chart[i], ": ", summary_items$Value[i]), fp_normal)
      )
    }))
    
    ppt <- add_slide(ppt, layout = "Title and Content") %>%
      ph_with(value = "Summary", location = loc_title) %>%
      ph_with(
        value = s_items,
        level_list = c(rep(1L, length(s_items))),
        location = loc_body
      ) %>% 
      add_footer()
  }
  
 
  # Chart
  for(plot_slide_title in names(plots)) {
    p <- plots[[plot_slide_title]]
    if(!is.null(p)) {
      ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext(plot_slide_title, fp_title)), location = loc_title) %>%
        ph_with(value = p, location = loc_body) %>%
        add_footer()
    }
  }
  
  # Export the PowerPoint
  return(print(ppt, target = file))
}

#### DISPLAY FILTER SELECTIONS ###
sys_detailBox <- function(
                              selection = NULL,
                               all_filters = FALSE,
                               detail_type = 'comp',
                               methodology_type = input$syse_methodology_type,
                               cur_project_types = input$syse_project_type,
                               startDate = session$userData$ReportStart,
                               endDate = session$userData$ReportEnd,
                               age = input$syse_age,
                               spec_pops = input$syse_spec_pops,
                               race_eth = input$syse_race_ethnicity
                               ) {
  
  
  
  if(!all_filters){
    l1 <- 
      list(
        strong("Date Range: "),
        
        format(startDate, "%m-%d-%Y"),
        " to ",
        format(endDate, "%m-%d-%Y"),
        br(),
        
        if (cur_project_types != "All")
          chart_selection_detail_line("Project Type Group", sys_project_types, str_remove(cur_project_types, "- ")),
        
        #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
        HTML(glue(
          "<b>Methodology Type:</b> {str_sub(getNameByValue(sys_methodology_types, methodology_type), start = 1, end = 8)} <br>"
        )),
        
        HTML(
          glue(
            "<strong>Selections</strong>: {paste(selection, collapse=' and ')} <br>"
          )
        )
      )
  } else if(detail_type == 'time'){
      l1 <- list(
        br(),
        strong("Current Year Date Range: "),
        format(startDate, "%m-%d-%Y"), " to ", format(endDate, "%m-%d-%Y"), br(),
        
        strong("Previous Year Date Range: "),
        format(startDate - years(1) , "%m-%d-%Y"), " to ", format(endDate - years(1), "%m-%d-%Y"), br(),
        
        
        if (cur_project_types != "All")
          chart_selection_detail_line("Project Type Group", sys_project_types, str_remove(cur_project_types, "- ")),
        
        #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
        HTML(glue(
          "<b>Methodology Type:</b> {str_sub(getNameByValue(sys_methodology_types, methodology_type), start = 1, end = 8)} <br>"
        )),
        
        if (length(age) != length(sys_age_cats))
          HTML(glue(
            "<b>Age:</b> {paste(age, collapse = ', ')} <br>"
          )),
        
        if (race_eth != "All")
          chart_selection_detail_line("Race/Ethnicity", sys_race_ethnicity_cats(methodology_type), race_eth),
        
        if(getNameByValue(sys_spec_pops_people, spec_pops) != "All Statuses")
          HTML(glue(
            "<b>Veteran Status:</b> {paste(getNameByValue(sys_spec_pops_people, spec_pops), '(Adult Only)')} <br>"
          ))
        
      )
  } else {
    l1 <- list(
      br(),
      strong("Date Range: "),
      
      format(startDate, "%m-%d-%Y"), " to ", format(endDate, "%m-%d-%Y"), br(),
      
      if (cur_project_types != "All")
        chart_selection_detail_line("Project Type Group", sys_project_types, str_remove(cur_project_types, "- ")),
      
      #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
      HTML(glue(
        "<b>Methodology Type:</b> {str_sub(getNameByValue(sys_methodology_types, methodology_type), start = 1, end = 8)} <br>"
      )),
      
      if (length(age) != length(sys_age_cats))
        HTML(glue(
          "<b>Age:</b> {paste(age, collapse = ', ')} <br>"
        )),
      
      if (race_eth != "All")
        chart_selection_detail_line("Race/Ethnicity", sys_race_ethnicity_cats(methodology_type), race_eth),
      
      if(getNameByValue(sys_spec_pops_people, spec_pops) != "All Statuses")
        HTML(glue(
          "<b>Veteran Status:</b> {paste(getNameByValue(sys_spec_pops_people, spec_pops), '(Adult Only)')} <br>"
        ))
      
    )
  }
  
  return(l1)
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


# Demographic chart-specific stuff-----------------
sys_demographics_selection_info <- function(type = 'overview', selection = input$sys_composition_selections){
  
  if(type == 'overview'){
    df <- data.frame(
      Chart = c(
        "Demographic Selection 1",
        "Demographic Selection 2",
        "Total Served People"
      ),
      Value = c(
        selection[1],
        selection[2],
        nrow(get_people_universe_filtered() %>% remove_non_applicables(selection = selection))
      )
    )
  } else if (type == 'exits'){
    if(length(selection) == 1){
      df <- data.frame(
        Chart = c(
          "Demographic Selection 1",
          "Total People with System Exit",
          "Total People with PH System Exit"
        ),
        Value = c(
          selection[1],
          nrow(tree_exits_data()),
          nrow(tree_exits_data() %>% fsubset(`Destination Type` == 'Permanent'))
          
        )
      )
    } else if(length(selection) == 2){
      
      df <- data.frame(
        Chart = c(
          "Demographic Selection 1",
          "Demographic Selection 2",
          "Total People with System Exit",
          "Total People with PH System Exit"
        ),
        Value = c(
          selection[1],
          selection[2],
          nrow(tree_exits_data()),
          nrow(tree_exits_data() %>% fsubset(`Destination Type` == 'Permanent'))
          
        )
      )
    }
  }
  
  return(df)
}

get_selection_cats <- function(selection,type = 'overview') {
  # this gets all the categories of the selected variable
  # this is used to make sure even empty categories are included in the chart
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
      pivot(
        how="longer",
        ids = "PersonalID",
        names = list(selection, "n")
      ) %>%
      fgroup_by(selection) %>%
      fsummarize(n = fsum(n))
    
  } else {
    plot_df <- as.data.frame(table(comp_df[[var_col]]))
    names(plot_df) <- c(selection, "n")
    
    if(selection == "Domestic Violence Status") {
      plot_df <- plot_df %>% rowbind(tibble(
        `Domestic Violence Status` = "DVTotal",
        n = sum(plot_df %>% 
                  fsubset(`Domestic Violence Status` != "NotDV") %>%
                  pull(n), na.rm = TRUE)))
    }
  }
  return(plot_df)
}

get_sys_plot_df_2vars <- function(comp_df, var_cols, selections = input$system_composition_selections) {
  
  # Function to process each combination of the variables underlying the all-served
  # selections E.g. if Age and Race (and Method 1),
  # then we'd combine 0 to 12 with White, 0 to 12 with Black,
  # 13 to 24 with White, etc.
  process_combination <- function(v1, v2, comp_df) {
    logToConsole(session, glue("processing combination of {v1} and {v2}"))
    freq_df <- as.data.frame(table(comp_df[[v1]], comp_df[[v2]]))
    names(freq_df) <- c(
      selections[1],
      selections[2],
      "n"
    )
    
    # for selections comprised of multiple (binary/dummy) vars (e.g. Race), 
    # filter to the 1s and change the 1 to the variable name
    for (i in seq_along(selections)) {
      v <- get(paste0("v", i))
      vname <- sym(selections[i])
      var_cats <- var_cols[[vname]]
      if (length(var_cats) > 1) {
        freq_df <- freq_df %>%
          filter(!!vname == 1) %>%
          mutate(!!vname := v)
      }
    }
    return(freq_df)
  }
  
  # Get a dataframe of the freqs of all combinations
  # along with percents
  freqs <- expand_grid(v1 = var_cols[[selections[1]]], v2 = var_cols[[selections[2]]]) %>%
    pmap_dfr(~ process_combination(..1, ..2, comp_df)) # %>%
  # mutate(pct = (n / sum(n, na.rm = TRUE)))
  
  # Handle DV, since the "Total" is not an actual value of DomesticViolenceCategory.
  if ("Domestic Violence Status" %in% selections) {
    dv_totals <- freqs %>%
      fsubset(`Domestic Violence Status` %in% c("DVFleeing", "DVNotFleeing")) %>%
      group_by(!!sym(
        ifelse(
          selections[1] == "Domestic Violence Status",
          selections[2],
          selections[1]
        )
      )) %>%
      summarize(`Domestic Violence Status` = "DVTotal",
                n = sum(n, na.rm = TRUE)) #,
    # pct = sum(pct, na.rm = TRUE))
    freqs <- rowbind(freqs, dv_totals)
  }
  
  return(freqs)
}

# Period-Specific Enrollment Categories ----------------------------------------
# "expand" the dataset to get repeated rows per period (full + each month)
# then filter based on the period start and end
expand_by_periods <- function(dt, chart_type = 'mbm') {
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
      startDate = c(session$userData$ReportStart,
                    session$userData$ReportStart %m-% years(1)),
      endDate = c(session$userData$ReportEnd,
                  session$userData$ReportEnd %m-% years(1))
    ) %>% 
      ftransform(
        exit_cutoff = startDate %m-% years(2),
        temp_key = 1
      )
  } else {
    all_periods <- data.table(
      period = c('Full'),
      startDate = session$userData$ReportStart,
      endDate = session$userData$ReportEnd
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
    fsubset(EntryDate <= endDate & ExitAdjust >= exit_cutoff) %>%
    fselect(-temp_key, -exit_cutoff) %>%
    setkey(period) %>%
    ftransform(
      straddles_start = EntryDate <= startDate & ExitAdjust >= startDate,
      straddles_end = EntryDate <= endDate & ExitAdjust >= endDate,
      in_date_range = EntryDate <= endDate & ExitAdjust >= startDate
    )
}
