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
                                   ifelse('All Races/Ethnicities' %in% input$system_composition_selections, '1',
                                          ifelse('Grouped Races/Ethnicities' %in% input$system_composition_selections, '2', NA)),
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
  
  values <- c(
    values,
    getNameByValue(sys_hh_types, input$syse_hh_type),
    getNameByValue(sys_level_of_detail, input$syse_level_of_detail),
    getNameByValue(sys_project_types, input$syse_project_type)
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
        return(date_stamped_filename(paste0(reports[[1]]$name, reports[[1]]$ext)))
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
      
      temp_dir <- file.path(tempdir(), paste0("shiny_export_", id_prefix))
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

