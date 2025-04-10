sys_comp_plot_df <- reactiveVal()

get_race_ethnicity_vars <- function(v) {
  if (v == "All") {
    syso_race_ethnicities_all <- unlist(c(syso_race_ethnicity_cats(input$methodology_type)["Detailed"],"Unknown" = "RaceEthnicityUnknown"))
    names(syso_race_ethnicities_all) <- gsub("Detailed.", "", names(syso_race_ethnicities_all))
    return(syso_race_ethnicities_all)
  } else if (v %in% c("Grouped")) {
    syso_race_ethnicities_grouped <- unlist(c(syso_race_ethnicity_cats(input$methodology_type)["Summarized"], "Unknown" = "RaceEthnicityUnknown"))
    names(syso_race_ethnicities_grouped) <- gsub("Summarized.", "", names(syso_race_ethnicities_grouped))
    return(syso_race_ethnicities_grouped)
  }
}

syscomp_detailBox <- function(session) {
  return(
    list(
      strong("Date Range: "),
      
      format(ReportStart(), "%m-%d-%Y"),
      " to ",
      format(ReportEnd(), "%m-%d-%Y"),
      br(),
      
      if (input$syso_project_type != "All")
        chart_selection_detail_line("Project Type Group", syso_project_types, input$syso_project_type),
      
      #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
      HTML(glue(
        "<b>Methodology Type:</b> {str_sub(getNameByValue(syso_methodology_types, input$methodology_type), start = 1, end = 8)} <br>"
      )),
      
      HTML(
        glue(
          "<strong>Selections</strong>: {paste(input$system_composition_selections, collapse=' and ')} <br>"
        )
      )
    )
  )
}

get_var_cols <- function() {
  return(
    list(
      "Age" = "AgeCategory",
      "All Races/Ethnicities" = get_race_ethnicity_vars("All"),
      "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
      #"Domestic Violence" = "DomesticViolenceCategory", #VL 9/20/24: Not including for launch
      # "Homelessness Type" =  "HomelessnessType",# Victoria, 8/15/24: Not including this for Launch
      "Veteran Status (Adult Only)" =  "VeteranStatus"
    )
  )
}
remove_non_applicables <- function(.data) {
  # remove children when vets is selected - since Vets can't be children
  if("Veteran Status (Adult Only)" %in% input$system_composition_selections) {
    .data %>% filter(!(AgeCategory %in% c("0 to 12", "13 to 17")))
  } 
  # filter to just HoHs and Adults for DV
  else if ("Domestic Violence status" %in% input$system_composition_selections) {
    .data %>% filter(!(AgeCategory %in% c("0 to 12", "13 to 17")) | CorrectedHoH == 1)
  } else {
    .data
  }
}

get_sys_comp_plot_df_1var <- function(comp_df, var_col) {
  # if number of variables associated with selection > 1, then they're dummies
  selection <- input$system_composition_selections
  
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

get_sys_comp_plot_df_2vars <- function(comp_df) {
  # named list of all selected options and
  # the corresponding variables in the underlying data
  var_cols <- get_var_cols()
  selections <- input$system_composition_selections
  
  # Function to process each combination of the variables underlying the all-served
  # selections E.g. if Age and Race (and Method 1),
  # then we'd combine 0 to 12 with White, 0 to 12 with Black,
  # 13 to 24 with White, etc.
  process_combination <- function(v1, v2, comp_df) {
    logToConsole(glue("processing combination of {v1} and {v2}"))
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

# this gets all the categories of the selected variable
# this is used to make sure even empty categories are included in the chart
get_selection_cats <- function(selection) {
  return(switch(
    selection,
    "Age" = syso_age_cats,
    "All Races/Ethnicities" = get_race_ethnicity_vars("All"),
    "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
    #"Domestic Violence" = syso_dv_pops, VL 9/20/24: Not including for launch
    # Update Veteran status codes to 1/0, because that's how the underlying data are
    # we don't do that in the original hardcodes.R list 
    # because the character versions are needed for the waterfall chart
    "Veteran Status (Adult Only)" = {
      syso_veteran_pops$Veteran <- 1
      syso_veteran_pops$`Non-Veteran/Unknown` <- 0
      syso_veteran_pops
    }
    # "Homelessness Type" = c("Homelessness Type1", "Homelessness Type2") # Victoria, 8/15/24: Not including this for Launch
  ))
}

# Suppression Rule 2: If only one cell in a group (i.e. row and/or column) is suppressed,
# then suppress the next lowest value in that group
suppress_next_val_if_one_suppressed_in_group <- function(.data, group_v, n_v) {
  if(length(input$system_composition_selections) > 1) {
    .data <- .data %>% group_by(!!sym(group_v))
  }
  return(
    .data %>%
      mutate(
        count_redacted = sum(wasRedacted, na.rm = TRUE),
        next_lowest = min(!!sym(n_v), na.rm = TRUE),
        wasRedacted = ifelse(count_redacted == 1 & (
          (wasRedacted & is.na(!!sym(n_v))) |
            (!wasRedacted & !!sym(n_v) == next_lowest)
        ), TRUE, wasRedacted)
      ) %>%
      ungroup() %>%
      select(-c(count_redacted, next_lowest))
  )
}

toggle_download_buttons <- function(plot_df) {
  shinyjs::toggle("sys_comp_download_btn", condition = sum(plot_df$n > 10, na.rm = TRUE) > 0)
  shinyjs::toggle("sys_comp_download_btn_ppt", condition = sum(plot_df$n > 10, na.rm = TRUE) > 0)
}

sys_comp_plot_1var <- function(isExport = FALSE) {
  var_cols <- get_var_cols()
  selection <- input$system_composition_selections

  comp_df <- sys_df_people_universe_filtered_r() %>%
    remove_non_applicables() %>%
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
 
  plot_df <- get_sys_comp_plot_df_1var(comp_df, var_cols[[selection]])
  
  # hide download buttons if not enough data
  toggle_download_buttons(plot_df)
  
  selection_cats1 <- get_selection_cats(selection)
  selection_cats1_labels <- if (is.null(names(selection_cats1))) {
    selection_cats1
  } else {
    names(selection_cats1)
  }
  
  plot_df[selection] <- factor(
    plot_df[[selection]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels,
    ordered = TRUE)
  
  sys_comp_plot_df(plot_df)
  
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

suppress_values <- function(.data, count_var) {
  return(mutate(
    .data,
    wasRedacted = between(!!sym(count_var), 1, 10),!!count_var := ifelse(!!sym(count_var) <= 10, NA, !!sym(count_var))
  ))
}

sys_comp_plot_2vars <- function(isExport = FALSE) {
  # race/ethnicity, if selected, should always be on the row
  var_cols <- get_var_cols()
  selections <- input$system_composition_selections
  
  if (selections[1] %in% c("All Races/Ethnicities", "Grouped Races/Ethnicities")) {
    selections <- c(selections[2], selections[1])
  }
  
  # get dataset underlying the freqs we will produce below
  comp_df <- sys_df_people_universe_filtered_r() %>%
    remove_non_applicables() %>%
    select(
      PersonalID, 
      unname(var_cols[[selections[1]]]), 
      unname(var_cols[[selections[2]]]))
  
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
  
  plot_df <- get_sys_comp_plot_df_2vars(comp_df)

  toggle_download_buttons(plot_df)
  
  selection_cats1 <- get_selection_cats(selections[1])
  selection_cats1_labels <- if (is.null(names(selection_cats1))) {
    selection_cats1
  } else {
    names(selection_cats1)
  }
  
  selection_cats2 <- get_selection_cats(selections[2])
  selection_cats2_labels <- if (is.null(names(selection_cats2))) {
    selection_cats2
  } else {
    names(selection_cats2)
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
  
  if(input$methodology_type == 1) {
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
  sys_comp_plot_df(plot_df)
  
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
  
  if(input$methodology_type == 1) {
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

sys_comp_selections_info <- reactive({
  data.frame(
    Chart = c(
      "Demographic Selection 1",
      "Demographic Selection 2",
      "Total Served People"
    ),
    Value = c(
      input$system_composition_selections[1],
      input$system_composition_selections[2],
      nrow(sys_df_people_universe_filtered_r() %>% remove_non_applicables())
    )
  )
})
sys_comp_selections_summary <- function() {
  return(
    sys_export_summary_initial_df() %>%
      bind_rows(sys_comp_selections_info()) %>%
      rename("System Demographics" = Value)
  )
}

output$sys_comp_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Demographics Report - "),
  content = function(file) {
    selections <- input$system_composition_selections
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
      
      num_df <- sys_comp_plot_df() %>%
        pivot_wider(
          names_from = selections[1],
          values_from = n,
          values_fill = list(n = 0)
        )

      # Create x.y% version
      pct_df <- num_df %>%
        mutate(across(where(is.numeric), ~ (. / sum(., na.rm = TRUE) * 100) %>%
                        replace_na(0) %>%
          round(1) %>%
          paste0("%")))
      
      # create totals, but only for Method1
      if(input$methodology_type == 1) { 
        # create total row
        total_num_row <- num_df %>%
          summarise(!!selections[1] := "Total",
                    across(where(is.numeric), sum, na.rm = TRUE)) %>%
          rename(!!selections[2] := !!selections[1])
        
        total_n <- sum(sys_comp_plot_df()$n, na.rm = TRUE)
        
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
      num_df <- sys_comp_plot_df()
      
      pct_df <- num_df %>%
        mutate(across(where(is.numeric), ~ (. / sum(., na.rm = TRUE) * 100) %>%
                        round(1) %>%
                        paste0("%")))  %>% 
        rename("pct" = n)
      
      if(input$methodology_type == 1) { 
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
    
    exportTestValues(sys_comp_report_num_df = num_df)
    exportTestValues(sys_comp_report_pct_df = pct_df)
  }
)

sys_comp_p <- reactive({
  req(
    !is.null(input$system_composition_selections) &
      valid_file() == 1 &
      between(length(input$system_composition_selections), 1, 2)
  )
  
  if(length(input$system_composition_selections) == 1) {
    sys_comp_plot_1var()
  } else {
    sys_comp_plot_2vars()
  }
})

observeEvent(input$system_composition_selections, {
  # they can select up to 2
  #disable all unchecked boxes if they've already selected 2
  shinyjs::runjs(str_glue("
    var numSelected = {length(input$system_composition_selections)};
    $('input[name=system_composition_selections]:not(\":checked\")')
      .attr('disabled', numSelected == 2);

    var reSelected = \"{
      \"All Races/Ethnicities\" %in% input$system_composition_selections |
      \"Grouped Races/Ethnicities\" %in% input$system_composition_selections
    }\";
    
    if(numSelected == 1)
      $('input[name=system_composition_selections][value*=\"Races/Ethnicities\"]:not(\":checked\")')
        .attr('disabled', reSelected == 'TRUE');
  "))
}, ignoreNULL = FALSE)


output$sys_comp_summary_selections <- renderUI({
  req(!is.null(input$system_composition_selections) & valid_file() == 1)
  syscomp_detailBox()
})

output$sys_comp_summary_ui_chart <- renderPlot({
  sys_comp_p()
}, height = function() {
  ifelse(!is.null(input$system_composition_selections), 700, 100)
}, width = function() {
  if (length(input$system_composition_selections) == 1 |
      isTRUE(getOption("shiny.testmode"))) {
    500
  } else {
    "auto"
  }
}, alt = "A crosstab data table of the demographic make-up of the homeless system.")


output$sys_comp_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Demographics_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    sys_overview_ppt_export(
      file = file,
      title_slide_title = "System Demographics",
      summary_items = sys_export_summary_initial_df() %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(sys_comp_selections_info()),
      plot_slide_title = paste0(
        "System Demographics: ",
        input$system_composition_selections[1],
        " by ",
        input$system_composition_selections[2]
      ),
      plot1 = if (length(input$system_composition_selections) == 1) {
        sys_comp_plot_1var(isExport = TRUE)
      } else {
        sys_comp_plot_2vars(isExport = TRUE)
      },
      summary_font_size = 28
    )
  }
)
