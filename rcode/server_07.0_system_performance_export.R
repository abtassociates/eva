# Preparing exports --------------
sys_export_summary_initial_df <- function(type = 'overview') {
  
  tabbox <- ifelse(type == 'overview', input$syso_tabbox, input$syse_tabbox)
  
  logMetadata(session, glue("Downloaded System {ttype} Tabular Data: {tabbox}{demotext}", 
                            ttype=str_to_title(type),
                            demotext = if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
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
  
  values <- c(
    strftime(session$userData$ReportStart, "%m/%d/%y"),
    strftime(session$userData$ReportEnd, "%m/%d/%y")
  )
  
  values <- c(
    values, 
    switch(type,
           'overview' = c(
             getNameByValue(sys_methodology_types, 
                            ifelse(input$syso_tabbox == '<h4>System Demographics',
                                   ifelse('All Races/Ethnicities' %in% input$syso_composition_selections, '1',
                                          ifelse('Grouped Races/Ethnicities' %in% input$syso_composition_selections, '2', NA)),
                                   input$syso_methodology_type))
           ),
           'exits' = c(
             getNameByValue(sys_methodology_types, 
                            ifelse(input$syse_tabbox == '<h4>Exits to PH Demographics</h4>',
                                   ifelse('All Races/Ethnicities' %in% input$syse_phd_selections, '1',
                                          ifelse('Grouped Races/Ethnicities' %in% input$syse_phd_selections, '2', NA)),
                                   input$syse_methodology_type))
           ),
           'exits_time' = c(
             strftime(session$userData$ReportStart - years(1), "%m/%d/%y"),
             strftime(session$userData$ReportEnd - years(1), "%m/%d/%y"),
             getNameByValue(sys_methodology_types, input$syse_methodology_type)
           )
    )
  )
  
  prefix <- fcase(
    type == "overview", "syso",
    grepl("exits", type), "syse",
    default = "syso"
  )
  
  values <- c(
    values,
    getNameByValue(sys_hh_types, input[[paste0(prefix, "_hh_type")]]),
    getNameByValue(sys_level_of_detail, input[[paste0(prefix, "_level_of_detail")]]),
    getNameByValue(sys_project_types, input[[paste0(prefix, "_project_type")]])
  )
  
  
  df$Value <- values
  # remove Methodology Type line if value was NA
  df <- df[df$Value != 'NA, NA',]
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
                     if(identical(sys_age_cats, input$syse_age)) {"All Ages"} else {paste(input$syse_age, collapse=", ")},
                     getNameByValue(sys_spec_pops_people, input$syse_spec_pops),
                     getNameByValue(sys_race_ethnicity_cats(input$syse_methodology_type), input$syse_race_ethnicity)
                   ),
                   'exits_subpop' = c(
                     if(identical(sys_age_cats, input$syse_subpop_age)) {"All Ages"} else {paste(input$syse_subpop_age, collapse=", ")},
                     getNameByValue(sys_spec_pops_people, input$syse_subpop_spec_pops),
                     getNameByValue(sys_race_ethnicity_cats(input$syse_methodology_type), input[[glue('syse_subpop_race_ethnicity{input$syse_methodology_type}')]])
                   )
  )
  selections$Value <- values
  
  return(selections)
}


sys_heatmap_xl_export <- function(file, 
                                  type = 'overview',
                                  methodology_type,
                                  selections,
                                  plot_df,
                                  in_demo_mode = input$in_demo_mode
){
  
  #selections <- input$syso_composition_selections
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
    
    num_df <- plot_df %>%
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
      
      total_n <- sum(plot_df$n, na.rm = TRUE)
      
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
        fmutate(
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
    num_df <- plot_df
    
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
        list(syso_comp_selections_summary(), num_df, pct_df),
        c("System Demographics Metadata", num_tab_name, pct_tab_name)
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    exportTestValues(syso_comp_df = syso_get_people_universe_filtered())
    exportTestValues(syso_comp_report_num_df = num_df)
    exportTestValues(syso_comp_report_pct_df = pct_df)
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
        list(sys_phd_selections_summary(), syse_phd_export()),
        c("SystemExitDemographics Metadata", tab_name)
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    exportTestValues(syse_phd_df = syse_phd_export())
    
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
  
  ppt <- read_pptx(here("www","system_pptx_template.pptx"))
  
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
        
        ppt <- add_slide(ppt, layout = "Title and Content") %>%
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

# Handle System Overview/System Exits exports
register_sys_export_server <- function(id_prefix, input, output, session) {
  export_config <- get(paste0(id_prefix, "_export_config")) # found in hardcodes.R
  display_name  <- if (id_prefix == "syso") "System Overview" else "System Exits"
  output_id     <- paste0(id_prefix, "_export_act")
  
  # Reactive flags to break feedback loops between master and sub-checkboxes
  updating_ppt  <- reactiveVal(FALSE)
  updating_data <- reactiveVal(FALSE)
  
  # 1. Evaluate active selections
  selected_reports <- reactive({
    sel <- list()
    for (item in export_config) {
      input_id <- get_sys_export_id(id_prefix, item)
      if (isTRUE(input[[input_id]]))
        sel[[length(sel) + 1]] <- list(
          name = item$name, 
          gen = item$gen, # func name found in hardcodes.R, individual funcs found in respective _server.R scripts
          ext = item$ext
        )
    }
    sel
  })
  
  # 2. Unified download handler
  output[[output_id]] <- downloadHandler(
    filename = function() {
      reports <- selected_reports()
      if (length(reports) == 0) {
        return("no_selection.txt")
      } else if (length(reports) == 1) {
        return(date_stamped_filename(reports[[1]]$name, ext = reports[[1]]$ext))
      } else {
        return(date_stamped_filename(paste0("Consolidated ", display_name, " Export - "), ext = ".zip"))
      }
    },
    
    content = function(file) {
      reports <- selected_reports()
      if (length(reports) == 0) {
        writeLines("Please check at least one box to export.", file)
        return()
      }
      
      temp_dir <- file.path(tempdir(), paste0("shiny_export_", session$token, "_", id_prefix))
      if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
      dir.create(temp_dir)
      
      file_paths <- c()
      
      for (report in reports) {
        message(paste0("Running ", report$name, " where func = ", report$gen))
        local_path <- file.path(temp_dir, paste0(report$name, report$ext))
        if(report$gen %in% c("populate_client_level_export", "sys_comp_ppt_download", "sys_comp_data_download"))
          get(report$gen)(local_path, type = id_prefix) 
        else
          get(report$gen)(local_path) 
        file_paths <- c(file_paths, local_path)
      }
      
      if (length(reports) == 1) {
        file.copy(file_paths[1], file)
      } else {
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        zip::zipr(zipfile = file, files = basename(file_paths))
      }
    }
  )
  
  # Keep output bindings active when dropdown is closed
  # outputOptions(output, output_id, suspendWhenHidden = FALSE)
  
  
  # 3. POWERPOINT COLUMN OBSERVERS (Bidirectional)
  
  # Downward Cascade: Master -> Sub-checkboxes
  observeEvent(input[[paste0(id_prefix, "_export_all_ppt")]], {
    # If the master changed because of a sub-checkbox update, reset the flag and exit
    if (updating_ppt()) {
      updating_ppt(FALSE)
      return()
    }
    
    val <- input[[paste0(id_prefix, "_export_all_ppt")]]
    for (item in export_config) {
      if (item$ext == ".pptx") {
        input_id <- get_sys_export_id(id_prefix, item)
        updateCheckboxInput(session, input_id, value = val)
      }
    }
  })
  
  # Upward Sync: Sub-checkboxes -> Master
  observe({
    ppt_vals <- sapply(export_config, function(item) {
      if (item$ext == ".pptx") {
        input_id <- get_sys_export_id(id_prefix, item)
        input[[input_id]]
      } else {
        NULL
      }
    })
    ppt_vals <- unlist(Filter(Negate(is.null), ppt_vals))
    
    if (length(ppt_vals) > 0) {
      all_checked <- all(ppt_vals)
      master_id   <- paste0(id_prefix, "_export_all_ppt")
      
      if (!is.null(input[[master_id]]) && isTRUE(input[[master_id]]) != all_checked) {
        updating_ppt(TRUE) # Raise flag before programmatic update
        updateCheckboxInput(session, master_id, value = all_checked)
      }
    }
  })
  
  
  # 4. DATA COLUMN OBSERVERS (Bidirectional)
  
  # Downward Cascade: Master -> Sub-checkboxes
  observeEvent(input[[paste0(id_prefix, "_export_all_data")]], {
    if (updating_data()) {
      updating_data(FALSE)
      return()
    }
    
    val <- input[[paste0(id_prefix, "_export_all_data")]]
    for (item in export_config) {
      if (item$ext == ".xlsx") {
        input_id <- get_sys_export_id(id_prefix, item)
        updateCheckboxInput(session, input_id, value = val)
      }
    }
  })
  
  # Upward Sync: Sub-checkboxes -> Master
  observe({
    data_vals <- sapply(export_config, function(item) {
      if (item$ext == ".xlsx") {
        input_id <- get_sys_export_id(id_prefix, item)
        input[[input_id]]
      } else {
        NULL
      }
    })
    data_vals <- unlist(Filter(Negate(is.null), data_vals))
    
    if (length(data_vals) > 0) {
      all_checked <- all(data_vals)
      master_id   <- paste0(id_prefix, "_export_all_data")
      
      if (!is.null(input[[master_id]]) && isTRUE(input[[master_id]]) != all_checked) {
        updating_data(TRUE) # Raise flag before programmatic update
        updateCheckboxInput(session, master_id, value = all_checked)
      }
    }
  })
}

source(here("rcode", "server_07.0_system_performance_client_level_export.R"), local=TRUE)

register_sys_export_server("syso", input, output, session)
register_sys_export_server("syse", input, output, session)

