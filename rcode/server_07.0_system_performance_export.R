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

