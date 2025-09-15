
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
  
  if(type == 'exits_comparison'){
   
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
                   'exits_comparison' = c(
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
      pivot_wider(
        names_from = selections[1],
        values_from = n,
        values_fill = list(n = 0)
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
        bind_rows(total_num_row) %>%
        mutate(Total = rowSums(select(., where(is.numeric)), na.rm = TRUE))
      
      pct_df <- pct_df %>% 
        bind_rows(total_pct_row) %>%
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
      mutate(across(where(is.numeric), ~ (. / sum(., na.rm = TRUE) * 100) %>%
                      round(1) %>%
                      paste0("%")))  %>% 
      rename("pct" = n)
    
    if(methodology_type == 1) { 
      pct_df <- pct_df %>%
        bind_rows(
          setNames(
            data.frame("Total", "100%"), 
            c(selections, "pct")
          )
        )
      num_df <- num_df %>%
        bind_rows(summarise(., !!sym(selections) := "Total", n = sum(n, na.rm = TRUE)))
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

syse_time_detailBox <- function(
    selection = NULL,
    all_filters = FALSE,
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
  } else {
    l1 <- list(
      br(),
      strong("Current Year Date Range: "),
      
      format(startDate - years(1) , "%m-%d-%Y"), " to ", format(endDate - years(1), "%m-%d-%Y"), br(),
      strong("Previous Year Date Range: "),
      
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

sys_perf_selection_info <- function(type = 'overview', selection = input$sys_composition_selections){
  
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
      filter(`Domestic Violence Status` %in% c("DVFleeing", "DVNotFleeing")) %>%
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
    freqs <- bind_rows(freqs, dv_totals)
  }
  
  return(freqs)
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

sys_comp_plot_1var <- function(subtab = 'comp', methodology_type, selection, isExport = FALSE) {
  var_cols <- get_var_cols(methodology_type)
  
  #browser()
  comp_df <- get_people_universe_filtered() %>%
    remove_non_applicables(selection = selection) %>%
    select(PersonalID, unname(var_cols[[selection]]))
  
  validate(
    need(
      nrow(comp_df) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      nrow(comp_df) > 10,
      message = suppression_msg
    )
  )
  
  plot_df <- get_sys_plot_df_1var(comp_df, var_cols[[selection]], selection = selection)
  
  # hide download buttons if not enough data
  toggle_download_buttons(subtab,plot_df)
  
  if(subtab == 'comp'){
    type <- 'overview'
  } else if (subtab == 'phd'){
    type <- 'exits'
  }
  
  selection_cats1 <- get_selection_cats(selection, type = type)
  if(is.null(names(selection_cats1))){
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  plot_df[selection] <- factor(
    plot_df[[selection]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels,
    ordered = TRUE)
  
  if(subtab == 'comp'){
    sys_comp_plot_df(plot_df)
  } else if(subtab == 'phd'){
    sys_phd_plot_df(plot_df)
  }
  
  
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(selection, "n")
  
  return(
    ggplot(plot_df, aes("", .data[[selection]])) +
      # main data into cells for each cross-combination
      geom_tile(
        color = '#f0f0f0',
        lwd = 0.5,
        linetype = 1,
        aes(fill = n)
      ) +
      scale_fill_gradient(
        low = "#D2E3D9",
        high = "#084954",
        na.value = ifelse(
          is.na(plot_df$wasRedacted) | !plot_df$wasRedacted,
          "white",
          "#D2E3D9"
        )
      ) +
      # set text color to be 508 compliant contrasting
      geom_text(
        aes(label = ifelse(wasRedacted, "***", scales::comma(n))),
        size = sys_chart_text_font,
        color = ifelse(
          plot_df$n > mean(plot_df$n, na.rm = TRUE) & !plot_df$wasRedacted,
          'white',
          'black'
        )
      ) +
      scale_y_discrete(
        labels = label_wrap(30),
        limits = rev(levels(plot_df[[selection]])),
      ) +
      # other stuff
      theme_bw() +
      ggtitle(sys_total_count_display(
        nrow(comp_df)
      )) +
      labs(caption = "*** indicates the value is suppressed") +
      theme(
        text = element_text(size = sys_chart_text_font_pts),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
      )
  )
}

sys_comp_plot_2vars <- function(subtab = 'comp', methodology_type, selections, isExport = FALSE) {
  # race/ethnicity, if selected, should always be on the row
  var_cols <- get_var_cols(methodology_type)
  
  if (selections[1] %in% c("All Races/Ethnicities", "Grouped Races/Ethnicities")) {
    selections <- c(selections[2], selections[1])
  }
  
  # get dataset underlying the freqs we will produce below
  comp_df <- get_people_universe_filtered() %>%
    remove_non_applicables(selection = selections) %>%
    select(
      PersonalID, 
      unname(var_cols[[selections[1]]]), 
      unname(var_cols[[selections[2]]])
    ) %>%
    funique()
  
  validate(
    need(
      nrow(comp_df) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      nrow(comp_df) > 10,
      message = suppression_msg
    )
  )
  
  plot_df <- get_sys_plot_df_2vars(comp_df, var_cols, selections = selections)
  
  toggle_download_buttons(subtab, plot_df)
  
  if(subtab == 'comp'){
    type <- 'overview'
  } else if (subtab == 'phd'){
    type <- 'exits'
  }
  selection_cats1 <- get_selection_cats(selections[1], type = type)
  
  if (is.null(names(selection_cats1))) {
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  selection_cats2 <- get_selection_cats(selections[2], type = type)
  if (is.null(names(selection_cats2))) {
    selection_cats2_labels <- selection_cats2
  } else {
    selection_cats2_labels <- names(selection_cats2)
  }
  
  plot_df[selections[1]] <- factor(
    plot_df[[selections[1]]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels)
  
  plot_df[selections[2]] <- factor(
    plot_df[[selections[2]]], 
    levels = selection_cats2, 
    labels = selection_cats2_labels)
  
  plot_df <- plot_df %>%
    complete(
      !!sym(selections[1]),
      !!sym(selections[2])
    ) %>%
    replace(is.na(.), 0)
  
  if(methodology_type == 1) {
    h_total <- plot_df %>%
      group_by(!!!syms(selections[[2]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[1]] := 'Total') %>%
      suppress_values("N") %>%
      suppress_next_val_if_one_suppressed_in_group(selections[1], "N")
    
    v_total <- plot_df %>%
      group_by(!!!syms(selections[[1]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[2]] := 'Total') %>%
      suppress_values("N") %>%
      suppress_next_val_if_one_suppressed_in_group(selections[2], "N")
  }
  
  # save before supressing the values
  # this will be used for the download/export
  if(subtab == 'comp'){
    sys_comp_plot_df(plot_df)
  } else {
    sys_phd_plot_df(plot_df)
  }
  
  # Suppress values <= 10
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[1], "n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[2], "n")
  
  
  g <- ggplot(plot_df, aes(.data[[selections[1]]], .data[[selections[2]]])) +
    # main data into cells for each cross-combination
    geom_tile(
      color = '#f0f0f0',
      lwd = 0.5,
      linetype = 1,
      aes(fill = n)
    ) +
    scale_fill_gradient(
      low = "#D2E3D9",
      high = "#084954",
      na.value = ifelse(
        is.na(plot_df$wasRedacted) | !plot_df$wasRedacted,
        "white",
        "#D2E3D9"
      )
    ) + # na.value makes 0s invisible
    # set text color to be 508 compliant contrasting
    geom_text(
      # aes(label = paste0(scales::comma(n), "\n", "(",scales::percent(pct, accuracy = 0.1),")")),
      aes(label = ifelse(wasRedacted, "***", scales::comma(n))),
      size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction, 1),
      color = ifelse(
        plot_df$n > mean(plot_df$n, na.rm = TRUE) & !plot_df$wasRedacted,
        'white',
        'black'
      )
    )
  
  
  x_labels <- selection_cats1_labels
  x_limits <- levels(plot_df[[selections[1]]])
  y_labels <- rev(selection_cats2_labels)
  y_limits <- rev(levels(plot_df[[selections[2]]]))
  
  if(methodology_type == 1) {
    x_labels <- c(x_labels, "Total")
    x_limits <- c(x_limits, "Total")
    y_labels <- c("Total", y_labels)
    y_limits <- c("Total", y_limits)
    g <- g + 
      ggnewscale::new_scale("fill") +
      # Row totals
      geom_tile(
        data = h_total,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = N)
      ) +
      
      scale_fill_gradient(
        low = "#ede7e3",
        high = "#73655e",
        na.value = ifelse(h_total$wasRedacted, "#ede7e3", 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***",
                           scales::comma(N))),
        size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction, 1),
        color = ifelse(
          h_total$N > mean(h_total$N, na.rm = TRUE) & !h_total$wasRedacted,
          'white',
          'black'
        ),
        data = h_total
      ) +
      
      # column totals
      ggnewscale::new_scale("fill") +
      geom_tile(
        data = v_total,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = N)
      ) +
      scale_fill_gradient(
        low = "#ede7e3",
        high = "#73655e",
        na.value = ifelse(v_total$wasRedacted, "#ede7e3", 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***",
                           scales::comma(N))),
        size = sys_chart_text_font * ifelse(isExport, 0.7, 1),
        color = ifelse(
          v_total$N > mean(v_total$N, na.rm = TRUE) & !v_total$wasRedacted,
          'white',
          'black'
        ),
        data = v_total
      )
  }
  g + 
    # axis labels
    scale_x_discrete(
      labels = str_wrap(x_labels, width = 20),
      limits = x_limits,
      position = "top"
    ) +
    scale_y_discrete(
      labels = str_wrap(y_labels, width = 30),
      limits = y_limits
    ) +
    
    # other stuff
    theme_bw() +
    
    ggtitle(sys_total_count_display(
      nrow(comp_df)
    )) +
    labs(caption = "*** indicates the value is suppressed") +
    
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # axis.title.x.top = element_text(margin = margin(0, 0, 15, 0)),
      axis.text = element_text(size = sys_comp_axis_text_font * ifelse(windowSize()[1] < 1300, 0.8, 1) * ifelse(isExport, 0.6, 1))
    )
}


sys_phd_plot_1var <- function(subtab = 'comp', methodology_type, selection, isExport = FALSE) {
  var_cols <- get_var_cols(methodology_type)
  
  #browser()
  if(subtab == 'comp'){
    comp_df <- get_people_universe_filtered() %>%
      remove_non_applicables(selection = selection) %>%
      select(PersonalID, unname(var_cols[[selection]]))
  } else if(subtab == 'phd'){
    
    comp_df <- all_filtered_syse() %>% 
      remove_non_applicables(selection = selection) %>% 
      select(PersonalID, Destination, unname(var_cols[[selection]]))
    comp_df_phd <- comp_df %>% filter(Destination %in% perm_livingsituation)
    
  }
  
  validate(
    need(
      nrow(comp_df) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      nrow(comp_df) > 10,
      message = suppression_msg
    )
  )
  
  plot_df <- get_sys_plot_df_1var(comp_df, var_cols[[selection]], selection = selection)
  plot_df_phd <- get_sys_plot_df_1var(comp_df_phd, var_cols[[selection]], selection = selection)
  
  # hide download buttons if not enough data
  toggle_download_buttons(subtab,plot_df)
  
  if(subtab == 'comp'){
    type <- 'overview'
  } else if (subtab == 'phd'){
    type <- 'exits'
  }
  
  selection_cats1 <- get_selection_cats(selection, type = type)
  
  if(is.null(names(selection_cats1))){
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  plot_df[selection] <- factor(
    plot_df[[selection]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels,
    ordered = TRUE)
  
  plot_df_phd[selection] <- factor(
    plot_df_phd[[selection]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels,
    ordered = TRUE)
  
  if(subtab == 'comp'){
    sys_comp_plot_df(plot_df)
  } else if(subtab == 'phd'){
    sys_phd_plot_df(plot_df)
  }
  
  
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(selection, "n")
  
  plot_df_joined <- left_join(plot_df,plot_df_phd %>% rename(num = n), by=c(selection)) %>% 
    mutate(frac = ifelse(n == 0 | is.na(n), NA, num / n))
  #browser()
  return(
    ggplot(plot_df_joined, aes("", .data[[selection]])) +
      # main data into cells for each cross-combination
      geom_tile(
        color = '#f0f0f0',
        lwd = 0.5,
        linetype = 1,
        aes(fill = n)
      ) +
      scale_fill_gradient(
        low = "#D2E3D9",
        high = "#084954",
        na.value = ifelse(
          is.na(plot_df_joined$wasRedacted) | !plot_df_joined$wasRedacted,
          "white",
          "#D2E3D9"
        )
      ) +
      # set text color to be 508 compliant contrasting
      geom_text(
        aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n', '(',num,' of ',n,')'))),
        size = sys_chart_text_font,
        color = ifelse(
          plot_df_joined$n > mean(plot_df_joined$n, na.rm = TRUE) & !plot_df_joined$wasRedacted,
          'white',
          'black'
        )
      ) +
      scale_y_discrete(
        labels = label_wrap(30),
        limits = rev(levels(plot_df[[selection]])),
      ) +
      # other stuff
      theme_bw() +
      ggtitle(sys_total_count_display(
        nrow(comp_df)
      )) +
      labs(caption = "*** indicates the value is suppressed") +
      theme(
        text = element_text(size = sys_chart_text_font_pts),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
      )
  )
}

sys_phd_export <- reactiveVal()

sys_phd_plot_2vars <- function(subtab = 'comp', methodology_type, selections, isExport = FALSE) {
  # race/ethnicity, if selected, should always be on the row
  var_cols <- get_var_cols(methodology_type)
  
  if (selections[1] %in% c("All Races/Ethnicities", "Grouped Races/Ethnicities")) {
    selections <- c(selections[2], selections[1])
  }
  
  # get dataset underlying the freqs we will produce below
  
  
  if(subtab == 'comp'){
    
    comp_df <- get_people_universe_filtered() %>%
      remove_non_applicables(selection = selections) %>%
      select(
        PersonalID, 
        unname(var_cols[[selections[1]]]), 
        unname(var_cols[[selections[2]]])
      ) %>%
      funique()
  } else if(subtab == 'phd'){
    #browser()
    comp_df <- all_filtered_syse() %>% 
      remove_non_applicables(selection = selections) %>% 
      select(
        PersonalID, 
        Destination,
        unname(var_cols[[selections[1]]]), 
        unname(var_cols[[selections[2]]])
      ) %>%
      funique()
    comp_df_phd <- comp_df %>% filter(Destination %in% perm_livingsituation)
  }
  #browser()
  
  
  validate(
    need(
      nrow(comp_df) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      nrow(comp_df) > 10,
      message = suppression_msg
    )
  )
  
  plot_df <- get_sys_plot_df_2vars(comp_df, var_cols, selections = selections)
  plot_df_phd <- get_sys_plot_df_2vars(comp_df_phd, var_cols, selections = selections)
  
  
  
  toggle_download_buttons(subtab, plot_df)
  
  if(subtab == 'comp'){
    type <- 'overview'
  } else if (subtab == 'phd'){
    type <- 'exits'
  }
  selection_cats1 <- get_selection_cats(selections[1], type = type)
  
  if (is.null(names(selection_cats1))) {
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  selection_cats2 <- get_selection_cats(selections[2], type = type)
  if (is.null(names(selection_cats2))) {
    selection_cats2_labels <- selection_cats2
  } else {
    selection_cats2_labels <- names(selection_cats2)
  }
  
  plot_df[selections[1]] <- factor(
    plot_df[[selections[1]]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels)
  
  plot_df[selections[2]] <- factor(
    plot_df[[selections[2]]], 
    levels = selection_cats2, 
    labels = selection_cats2_labels)
  
  plot_df <- plot_df %>%
    complete(
      !!sym(selections[1]),
      !!sym(selections[2])
    ) %>%
    replace(is.na(.), 0)
  
  plot_df_phd[selections[1]] <- factor(
    plot_df_phd[[selections[1]]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels)
  
  plot_df_phd[selections[2]] <- factor(
    plot_df_phd[[selections[2]]], 
    levels = selection_cats2, 
    labels = selection_cats2_labels)
  
  plot_df_phd <- plot_df_phd %>%
    complete(
      !!sym(selections[1]),
      !!sym(selections[2])
    ) %>%
    replace(is.na(.), 0)
  
  if(methodology_type == 1) {
    #browser()
    h_total <- plot_df %>%
      group_by(!!!syms(selections[[2]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[1]] := 'Total') %>%
      suppress_values("N") %>%
      suppress_next_val_if_one_suppressed_in_group(selections[1], "N")
    
    v_total <- plot_df %>%
      group_by(!!!syms(selections[[1]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[2]] := 'Total') %>%
      suppress_values("N") %>%
      suppress_next_val_if_one_suppressed_in_group(selections[2], "N")
    
    h_total_phd <- plot_df_phd %>%
      group_by(!!!syms(selections[[2]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[1]] := 'Total') #%>%
      #suppress_values("N") %>%
      #suppress_next_val_if_one_suppressed_in_group(selections[1], "N")
    
    v_total_phd <- plot_df_phd %>%
      group_by(!!!syms(selections[[1]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[2]] := 'Total') #%>%
      #suppress_values("N") %>%
      #suppress_next_val_if_one_suppressed_in_group(selections[2], "N")
  }
  
  # save before supressing the values
  # this will be used for the download/export
  if(subtab == 'comp'){
    sys_comp_plot_df(plot_df)
  } else {
    sys_phd_plot_df(plot_df)
  }
  
  # Suppress values <= 10
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[1], "n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[2], "n")
  
  # plot_df_phd <- plot_df_phd %>%
  #   suppress_values("n") %>%
  #   suppress_next_val_if_one_suppressed_in_group(selections[1], "n") %>%
  #   suppress_next_val_if_one_suppressed_in_group(selections[2], "n")
  
  plot_df_joined <- left_join(plot_df,plot_df_phd %>% rename(num = n), by=c(selections[1],selections[2])) %>% 
    mutate(frac = ifelse(n == 0 | is.na(n), NA, num / n))
  
  if(methodology_type == 1){
    h_total_joined <- left_join(h_total,h_total_phd %>% rename(num = N), by=c(selections[1],selections[2])) %>% 
      mutate(frac = ifelse(N == 0 | is.na(N), NA, num / N))
    
    v_total_joined <- left_join(v_total,v_total_phd %>% rename(num = N), by=c(selections[1],selections[2])) %>% 
      mutate(frac = ifelse(N == 0 | is.na(N), NA, num / N))
    
  }
  
  export_label1 <- paste0(selections[2], ' (Demographic Section 1)')
  export_label2 <- paste0(selections[1], ' (Demographic Section 2)')
 
  export_df <- plot_df_joined %>% left_join(plot_df %>% 
                                              mutate(`Suppression Flag` = ifelse(!is.na(wasRedacted) & wasRedacted, "Yes","No")) %>% select(-n, -wasRedacted)) %>% 
    mutate(frac = scales::percent(frac, accuracy=0.1)) %>% 
    select(selections[2], selections[1], 'Total Count' = n, 'Permanent Count' = num, 'Percent in Permanent' = frac, `Suppression Flag`) %>% 
    arrange(!!sym(selections[2]))
  names(export_df)[1:2] <- c(export_label1, export_label2)
  
  sys_phd_export(export_df)
  
  g <- ggplot(plot_df_joined, aes(.data[[selections[1]]], .data[[selections[2]]])) +
    # main data into cells for each cross-combination
    geom_tile(
      color = '#f0f0f0',
      lwd = 0.5,
      linetype = 1,
      aes(fill = frac)
    ) +
    scale_fill_gradient(
      low = "#D2E3D9",
      high = "#084954",
      breaks = seq(0,1,by=0.05),
      na.value = ifelse(
        is.na(plot_df_joined$wasRedacted) | !plot_df_joined$wasRedacted,
        "white",
        "#D2E3D9"
      )
    ) + # na.value makes 0s invisible
    # set text color to be 508 compliant contrasting
    geom_text(
      # aes(label = paste0(scales::comma(n), "\n", "(",scales::percent(pct, accuracy = 0.1),")")),
      aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n', '(',num,' of ',n,')'))),#scales::comma(n))),
      size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction, 1),
      color = ifelse(
        plot_df_joined$frac > mean(plot_df_joined$frac, na.rm = TRUE) & !plot_df_joined$wasRedacted,
        'white',
        'black'
      )
    )
  
  
  x_labels <- selection_cats1_labels
  x_limits <- levels(plot_df[[selections[1]]])
  y_labels <- rev(selection_cats2_labels)
  y_limits <- rev(levels(plot_df[[selections[2]]]))
  
  if(methodology_type == 1) {
    x_labels <- c(x_labels, "Total")
    x_limits <- c(x_limits, "Total")
    y_labels <- c("Total", y_labels)
    y_limits <- c("Total", y_limits)
    g <- g + 
      ggnewscale::new_scale("fill") +
      # Row totals
      geom_tile(
        data = h_total_joined,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = frac)
      ) +
      
      scale_fill_gradient(
        low = "#ede7e3",
        high = "#73655e",
        na.value = ifelse(h_total_joined$wasRedacted, "#ede7e3", 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n', '(',num,' of ',N,')'))),
        size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction, 1),
        color = ifelse(
          h_total_joined$frac > mean(h_total_joined$frac, na.rm = TRUE) & !h_total_joined$wasRedacted,
          'white',
          'black'
        ),
        data = h_total_joined
      ) +
      
      # column totals
      ggnewscale::new_scale("fill") +
      geom_tile(
        data = v_total_joined,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = frac)
      ) +
      scale_fill_gradient(
        low = "#ede7e3",
        high = "#73655e",
        na.value = ifelse(v_total_joined$wasRedacted, "#ede7e3", 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n', '(',num,' of ',N,')'))),
        size = sys_chart_text_font * ifelse(isExport, 0.7, 1),
        color = ifelse(
          v_total_joined$frac > mean(v_total_joined$frac, na.rm = TRUE) & !v_total_joined$wasRedacted,
          'white',
          'black'
        ),
        data = v_total_joined
      )
  }
  g + 
    # axis labels
    scale_x_discrete(
      labels = str_wrap(x_labels, width = 20),
      limits = x_limits,
      position = "top"
    ) +
    scale_y_discrete(
      labels = str_wrap(y_labels, width = 30),
      limits = y_limits
    ) +
    
    # other stuff
    theme_bw() +
    
    ggtitle(sys_total_count_display(
      nrow(comp_df)
    )) +
    labs(caption = "*** indicates the value is suppressed") +
    
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # axis.title.x.top = element_text(margin = margin(0, 0, 15, 0)),
      axis.text = element_text(size = sys_comp_axis_text_font * ifelse(windowSize()[1] < 1300, 0.8, 1) * ifelse(isExport, 0.6, 1))
    )
}



get_syse_lookbacks <- function(filtered_enrollments) {
  # Calculate days_since_lookback, days_to_lookahead, and other lookback info
  # First, determine days_to_lookahead
  dt <- filtered_enrollments %>%
    setkey(PersonalID, EntryDate, ExitAdjust) %>%
    fmutate(
      days_to_lookahead = L(EntryDate, -1, g = PersonalID) - ExitAdjust
    ) %>%
    join(
      rbind(
        session$userData$lh_non_res %>% fselect(EnrollmentID, last_lh_info_date),
        session$userData$lh_nbn %>% fselect(EnrollmentID, last_lh_info_date)
      ),
      on = "EnrollmentID"
    ) %>%
    fmutate(last_lh_info_date = fifelse(
      ProjectType %in% c(lh_project_types_nonbn, ph_project_types),
      fcoalesce(MoveInDateAdjust, ExitAdjust),
      last_lh_info_date
    ))
  
  dt_starts <- dt[, .(PersonalID, EnrollmentID, EntryDate, Date = EntryDate, Type = "start")]
  dt_ends <- dt[, .(PersonalID, EnrollmentID,  Destination, last_lh_info_date, MoveInDateAdjust, ProjectType, ExitAdjust, Date = ExitAdjust, Type = "end")]
  setkey(dt_ends, PersonalID, Date)
  setkey(dt_starts, PersonalID, Date)
  
  # Rolling join to find the most recent end date before each start date
  lookback_info <- dt_ends[dt_starts, roll = TRUE][, .(
    PersonalID,
    EnrollmentID = i.EnrollmentID,
    days_since_lookback = EntryDate - ExitAdjust, # ExitDate is the lookup's ExitDate
    lookback_des = Destination,
    lookback_ptype = ProjectType,
    lookback_enrollment_id = EnrollmentID,
    lookback_dest_perm = Destination %in% perm_livingsituation,
    lookback_movein = MoveInDateAdjust,
    lookback_is_nonres_or_nbn = ProjectType %in% nbn_non_res,
    lookback_last_lh_date = last_lh_info_date
  )]
  
  
  return(join(
    dt %>% fselect(-last_lh_info_date),
    lookback_info,
    on = c("PersonalID", "EnrollmentID")
  ) )
}

expand_by_periods_syse <- function(dt, time_chart) {
  
  if(!time_chart){
    all_periods <- data.table(
      period = c('Full'),
      startDate = session$userData$ReportStart,
      endDate = session$userData$ReportEnd
    ) %>% 
      ftransform(
        exit_cutoff = startDate %m-% years(2),
        temp_key = 1
      )
  } else {
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


get_syse_eecr_and_lecr <- function(enrollments_filtered_w_lookbacks, time_chart) {
  logToConsole(session, "in get_eecr_and_lecr")
  period_enrollments_filtered <- expand_by_periods_syse(enrollments_filtered_w_lookbacks, time_chart = time_chart)
  
  logToConsole(session, paste0("In get_eecr_and_lecr, num period_enrollments_filtered: ", nrow(period_enrollments_filtered)))
  
  if(nrow(period_enrollments_filtered) == 0) return(period_enrollments_filtered)
  
  # Determine eecr/lecr-eligible records
  # get lh info and  limit to only enrollments that were LH during the given period 
  # or were not, but exited and HAD been LH at some point during the FULL period
  # the exit-but-was-once-LH is important because 
  all_enrollments <- period_enrollments_filtered %>% 
    join(
      rbindlist(
        list(
          get_lh_non_res_esnbn_info(enrollments_filtered_w_lookbacks, time_chart = time_chart), 
          get_res_lh_info(enrollments_filtered_w_lookbacks, time_chart = time_chart)
        )
      ),
      on = c("period","EnrollmentID"),
      how = "left"
    ) %>%
    fgroup_by(EnrollmentID) %>%
    fmutate(
      last_lh_info_date = na_locf(last_lh_info_date),
      first_lh_info_date = na_focb(first_lh_info_date)
    ) %>%
    fungroup() %>%
    fmutate(
      was_housed_at_start = (straddles_start | days_since_lookback %between% c(0, 14)) & 
        ProjectType %in% ph_project_types &
        fcoalesce(MoveInDateAdjust, no_end_date) < startDate,
      
      was_housed_during_period = ProjectType %in% ph_project_types & 
        in_date_range & 
        fcoalesce(MoveInDateAdjust, no_end_date) <= endDate,
      
      was_housed_at_end = (straddles_end | days_to_lookahead %between% c(0, 14)) & 
        ProjectType %in% ph_project_types & 
        fcoalesce(MoveInDateAdjust, no_end_date) < endDate,
      
      was_lh_at_start = fcoalesce(was_lh_at_start, FALSE),
      was_lh_during_period = fcoalesce(was_lh_during_period, FALSE),
      was_lh_at_end = fcoalesce(was_lh_at_end, FALSE)
    ) %>%
    # flag if enrollment was EVER LH during the full period (or was in res project type). 
    # This will be important for selecting EECRs
    fgroup_by(EnrollmentID) %>%
    fmutate(
      was_lh_during_full_period = anyv(period == "Full" & was_lh_during_period, TRUE),
      # Should the below include SO or not (i.e. use non_res_project_types or non_res_nonlh_project_types)
      nbn_non_res_no_future_lh = ProjectType %in% c(es_nbn_project_type, non_res_project_types) &
        (is.na(last_lh_info_date) | last_lh_info_date <= endDate)
    ) %>%
    fungroup() %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      no_lh_lookbacks = !anyv(was_lh_during_period, TRUE)
    ) %>%
    fungroup() %>%
    fmutate(
      lookback_movein_before_start = lookback_movein < startDate
    )
  
  potential_eecr_lecr <- all_enrollments %>%
    # only keep enrollments that were LH or housed during the period, or
    # neither but Exited in the period and were LH at SOME point during the period
    # AS 8/1: The problem with this is, e.g. PersonalID 684918 (ICf-good), for the first month, Nov, Enrollment 833423 is picked as the EECR, wehreas the full period picks 817330. 
    # 817330 it gets dropped here.
    fsubset(
      was_lh_during_period | 
        was_housed_during_period |
        (period != "Full" & (ExitAdjust >= session$userData$ReportEnd | ExitAdjust %between% list(startDate, endDate)) & was_lh_during_full_period)
    )
  
  e <- potential_eecr_lecr %>%
    fmutate(
      non_straddle_exit = fifelse(!straddles_end, ExitAdjust, NA),
      non_straddle_entry = fifelse(!straddles_end, EntryDate, NA)
    ) %>%
    setorder(PersonalID, period, straddles_end, EntryDate, ExitAdjust) %>%
    fgroup_by(PersonalID) %>%
    fmutate(
      max_non_straddle_exit = fmax(fifelse(non_straddle_exit <= endDate, non_straddle_exit, NA)),
      max_non_straddle_entry = fmax(fifelse(non_straddle_entry <= endDate, non_straddle_entry, NA))
    ) %>%
    fungroup() %>%
    fmutate(
      background_non_res_straddle_end = fcoalesce(
        straddles_end & 
          nbn_non_res_no_future_lh & 
          is.na(ExitDate) & 
          last_lh_info_date < max_non_straddle_exit & 
          max_non_straddle_entry <= endDate, 
        FALSE
      )
    )
  
  e2 <- e %>%
    # flag the first and last straddling enrollments, 
    # by (desc) ProjectTypeWeight and EntryDate
    roworder(period, PersonalID, -ProjectTypeWeight, EntryDate) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      first_straddle_start = ffirst(
        fifelse(straddles_start, EnrollmentID, NA)
      ) == EnrollmentID,
      any_straddle_start = any(first_straddle_start, na.rm=TRUE) #,#anyv(straddles_start, TRUE)
    ) %>%
    fungroup() %>%
    roworder(period, PersonalID, ProjectTypeWeight, EntryDate) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      last_valid_straddle_end = flast(
        fifelse(straddles_end & !background_non_res_straddle_end, EnrollmentID, NA)
      ) == EnrollmentID,
      last_valid_straddle_end_exit = fmax(fifelse(last_valid_straddle_end, ExitAdjust, NA))
    ) %>%
    fungroup() %>%
    # flag the first non-straddling enrollments in the report period,
    # for people that have no eecr_straddles
    # We prioritize EntryDate over ProjectTypeWeight because we want the earliest
    roworder(period, PersonalID, EntryDate, -ProjectTypeWeight, ExitAdjust) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      first_non_straddle_start = ffirst(
        fifelse(in_date_range & !straddles_start, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    fungroup() %>%
    # flag last non-straddling enrollments in the report period,
    # for people that have no lecr_straddles
    # Since these have ExitDates, given that we want the LECR to represent a 
    # client's latest known Outflow status, we order by ExitAdjust to get the latest Exit
    roworder(period, PersonalID, ExitAdjust, ProjectTypeWeight, Destination, EntryDate) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      last_valid_non_straddle_end = flast(
        fifelse(in_date_range & !straddles_end, EnrollmentID, NA)
      ) == EnrollmentID
    ) %>%
    fungroup()
  
  # In most cases, the EECR should be the first_straddle_start and the LECR should be the last_straddle_end (assuming they pass the enrollment filters).
  # last_straddle_ends must also be LH or Housed at end (was_lh_at_end or was_housed_at_end).
  # If there are no straddles, then it should be the first_non_straddle_start. Ditto for (non)straddle_ends.
  # However, there are lots of exceptions:
  #   LECR exceptions:
  #   1. if the straddling enrollment is a "background, non-res enrollment", i.e. 
  #     a non-res (other than SO and nbn) with 
  #     no end date and no LH info in the current period or in the future, and 
  #     it starts before the previous period's last_non_straddle_end's exit ==> 
  #     then it should NOT be selected
  #
  #   2. if last_straddle_end is neither LH nor housed at period end, then take the non-straddle
  prep_for_exceptions <- e2 %>% 
    fgroup_by(period, PersonalID) %>%
    fmutate(
      last_straddle_end_lh_or_housed_at_end = any(last_valid_straddle_end & (was_lh_at_end | was_housed_at_end), na.rm=TRUE),
      only_period_enrollment = GRPN() == 1
    ) %>%
    fungroup()
  
  
  final <- prep_for_exceptions %>%
    # Create eecr and lecr flags
    fmutate(
      eecr = (first_straddle_start | (first_non_straddle_start & !any_straddle_start)),
      lecr = (
        # If we add (was_lh/housed_at_end), then Personal ID 346740 (ICF-good, Enrollment 846250) is not selected as LECR, and the last month outlfow != full outflow
        # but if we remove it, both 846250 AND 835362 are selected as the LECRs
        last_valid_straddle_end |
          (
            fcoalesce(last_valid_non_straddle_end, FALSE) & !last_straddle_end_lh_or_housed_at_end &  
              # Prevents this:
              #    PersonalID     period EnrollmentID ProjectType  EntryDate ExitAdjust   eecr   lecr last_straddle_end_exit
              # 3:     688880 2021-11-01       826879           6 2021-10-05 2021-11-08  FALSE   TRUE             2022-01-07
              #   4:     688880 2021-11-01       826045           4 2021-09-28 2022-01-07   TRUE   TRUE             2022-01-07
              (EntryDate >= last_valid_straddle_end_exit | is.na(last_valid_straddle_end_exit))
          )
      )
    ) %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      has_eecr = any(eecr, na.rm=TRUE),
      has_lecr = any(lecr, na.rm=TRUE),
      person_last_lh_info_date = as.Date(cummax(fcoalesce(as.numeric(last_lh_info_date), 0)))
    ) %>%
    fungroup() %>%
    fmutate(has_recent_lh_info = eecr & (EntryDate - person_last_lh_info_date) %between% c(1, 14))
  
  #160649 - ICFgood (getting "something's wrong" inflow) 
  #689253 - Demo (getting "something's wrong" inflow in June) 
  # 183338 - ICF good last outflow != full
  # 540917 - ICF-good 
  # 123945 - ICF-good (something's wrong)
  # 104510 last outflow != full
  # 330303 last outflow != full. Enrollment 859594 should be picked
  # 346740 last outflow != full
  # 637552 (Demo Mode), First Inflow != Full Inflow. Enrollment 826535 should be selected, but 842850 is instead
  # 423741 (ICF-good) outflow != full.
  # 613089        (ICF-good) first inflow!= full
  # 613426 (ICF-good) first inflow!= full
  # 525922 (ICF-good) last outflow != full
  # 687550 (Demo mode) last outflow != full
  # 140224 (ICF-good) last outflow != full. EnrollmentID 848355 has a last_lh_info_date ini itlaly, but then it's NA
  # 423741 (ICF-good) last outflow != full
  # 596228 (ICF-good) last outflow != full
  # 531816 (ICf-good) first inflow != full (when HHType == "AO" and PrjectType == "LHRes")
  # 425572 (ICF-good) last outflow != full (when HHType == "AO")
  # 150484 (ICF-good) multiple inactives in a row
  
  # 607965
  # QC checks ---------------
  # browser()
  #debug cols: final[PersonalID == 595646, c("period", enrollment_cols, "eecr", "lecr"), with=FALSE]
  # people must have an eecr or they can't be counted
  final <- final %>% fsubset(has_eecr & has_lecr)
  
  if(!in_dev_mode) {
    final <- final %>%
      fselect(c(
        "period",
        enrollment_cols,
        "eecr",
        "lecr",
        "days_since_lookback", "days_to_lookahead",
        "straddles_start", "straddles_end",
        "startDate","endDate",
        "lookback_dest_perm", "lookback_movein_before_start", "lookback_is_nonres_or_nbn", "lookback_last_lh_date",
        "has_recent_lh_info",
        "was_lh_at_start", "was_lh_during_period", "was_lh_at_end", "was_housed_at_start", "was_housed_at_end",
        "first_lh_info_date", "last_lh_info_date", "no_lh_lookbacks",
        "Destination", "LivingSituation",
        "HouseholdType", "CorrectedHoH"
      )) %>%
      funique()
  }
  
  return(final)
}
