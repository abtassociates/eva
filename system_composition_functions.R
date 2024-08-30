font_size <- 14 / .pt

get_race_ethnicity_vars <- function(v) {
  if (v == "All") {
    syso_race_ethnicities_all <- unlist(syso_race_ethnicity_cats(input$methodology_type)["Detailed"])
    names(syso_race_ethnicities_all) <- gsub("Detailed.", "", names(syso_race_ethnicities_all))
    return(syso_race_ethnicities_all)
  } else if (v %in% c("Grouped")) {
    syso_race_ethnicities_grouped <- unlist(syso_race_ethnicity_cats(input$methodology_type)["Summarized"])
    names(syso_race_ethnicities_grouped) <- gsub("Summarized.", "", names(syso_race_ethnicities_grouped))
    return(syso_race_ethnicities_grouped)
  }
}

syscomp_detailBox <- function(session) {
  return(
    list(
      strong("Date Range: "),
      
      ReportStart(),
      " to ",
      ReportEnd(),
      br(),
      
      if (input$syso_project_type != "All")
        chart_selection_detail_line("Project Type", syso_project_types, input$syso_project_type),
      
      chart_selection_detail_line(
        "Methodology Type",
        syso_methodology_types,
        input$methodology_type
      ),
      
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
      "Hispanic-Focused Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
      "Domestic Violence" = "DomesticViolenceCategory",
      "Gender" = unlist(
        syso_gender_cats(input$methodology_type) %>% discard_at("All Genders")
      ),
      # "Homelessness Type" =  "HomelessnessType",# Victoria, 8/15/24: Not including this for Launch
      "Veteran Status" =  "VeteranStatus"
    )
  )
}

get_sys_comp_plot_df <- function() {
  # named list of all selected options and
  # the corresponding variables in the underlying data
  var_cols <- get_var_cols()
  
  # get dataset underlying the freqs we will produce below
  comp_df <- sys_df_people_universe_filtered_r() %>%
    select(PersonalID, unname(var_cols[[input$system_composition_selections[1]]]), unname(var_cols[[input$system_composition_selections[2]]]))
  
  # Function to process each combination of the variables underlying the all-served
  # selections E.g. if Age and Gender (and Exclusive methopdology type),
  # then we'd combine 0 to 12 with ManExclusive, 0 to 12 with WomanExclusive,
  # 13 to 24 with ManExclusive, etc.
  process_combination <- function(v1, v2, comp_df) {
    freq_df <- as.data.frame(table(comp_df[[v1]], comp_df[[v2]]))
    names(freq_df) <- c(
      input$system_composition_selections[1],
      input$system_composition_selections[2],
      "n"
    )
    
    # for selections comprised of multiple (binary/dummy) vars (e.g. Gender or Race), 
    # filter to the 1s and change the 1 to the variable name
    for (i in seq_along(input$system_composition_selections)) {
      v <- get(paste0("v", i))
      vname <- sym(input$system_composition_selections[i])
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
  freqs <- expand_grid(v1 = var_cols[[input$system_composition_selections[1]]], v2 = var_cols[[input$system_composition_selections[2]]]) %>%
    pmap_dfr(~ process_combination(..1, ..2, comp_df)) # %>%
  # mutate(pct = (n / sum(n, na.rm = TRUE)))
  
  # Handle DV, since the "Total" is not an actual value of DomesticViolenceCategory.
  if ("Domestic Violence" %in% input$system_composition_selections) {
    dv_totals <- freqs %>%
      filter(`Domestic Violence` %in% c("DVFleeing", "DVNotFleeing")) %>%
      group_by(!!sym(
        ifelse(
          input$system_composition_selections[1] == "Domestic Violence",
          input$system_composition_selections[2],
          input$system_composition_selections[1]
        )
      )) %>%
      summarize(`Domestic Violence` = "DVTotal",
                n = sum(n, na.rm = TRUE)) #,
                # pct = sum(pct, na.rm = TRUE))
    freqs <- bind_rows(freqs, dv_totals)
  }
  
  return(freqs)
}

# this gets all the categories of the selected variable
# this is used to make sure even empty categories are included in the chart
get_v_cats <- function(v) {
  return(switch(
    v,
    "Gender" = syso_gender_cats(input$methodology_type) %>% discard_at("All Genders"),
    "Age" = syso_age_cats,
    "All Races/Ethnicities" = get_race_ethnicity_vars("All"),
    "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
    "Hispanic-Focused Races/Ethnicities" = get_race_ethnicity_vars("Grouped"),
    "Domestic Violence" = syso_dv_pops,
    "Veteran Status" = syso_veteran_pops,
    # "Homelessness Type" = c("Homelessness Type1", "Homelessness Type2") # Victoria, 8/15/24: Not including this for Launch
  ))
}

# Suppression Rule 2: If only one cell in a group (i.e. row and/or column) is suppressed,
# then suppress the next lowest value in that group
suppress_next_val_if_one_suppressed_in_group <- function(.data, group_v, n_v) {
  return(
    .data %>%
      group_by(!!sym(group_v)) %>%
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

sys_comp_plot_1var <- function() {
  var_cols <- get_var_cols()
  v <- var_cols[[input$system_composition_selections]]
  comp_df <- sys_df_people_universe_filtered_r() %>%
    select(PersonalID, unname(v))
  
  v_cats1 <- get_v_cats(input$system_composition_selections)
  v_cats1_labels <- if (is.null(names(v_cats1))) {
    v_cats1
  } else {
    names(v_cats1)
  }
  
  if (length(v) > 1) {
    plot_df <- comp_df %>%
      pivot_longer(
        cols = -PersonalID,
        names_to = input$system_composition_selections,
        values_to = "value"
      ) %>%
      group_by(!!sym(input$system_composition_selections)) %>%
      summarize(n = sum(value, na.rm = TRUE), .groups = 'drop')
    
    plot_df[input$system_composition_selections] <- factor(
      plot_df[[input$system_composition_selections]], 
      levels = v_cats1, 
      labels = v_cats1_labels)
  } else {
    plot_df <- as.data.frame(table(comp_df[[v]]))
    names(plot_df) <- c(selection, "n")
  }
  
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(input$system_composition_selections, "n")
  
  return(
    ggplot(plot_df, aes("", .data[[input$system_composition_selections]])) +
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
        size = font_size,
        color = ifelse(
          plot_df$n > mean(plot_df$n, na.rm = TRUE) & !plot_df$wasRedacted,
          'white',
          'black'
        )
      ) +
      scale_y_discrete(
        labels = str_wrap(c("Total", v_cats1_labels), width = 30),
        limits = c("Total", levels(plot_df[[input$system_composition_selections]])),
      ) +
      # other stuff
      theme_bw() +
      ggtitle(sys_total_count_display(
        nrow(sys_df_people_universe_filtered_r())
      )) +
      theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 17, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(input$system_composition_selections, size = 13),
        axis.text = element_text(size = 14)
      )
  )
}

suppress_values <- function(.data, count_var) {
  return(mutate(
    .data,
    wasRedacted = between(!!sym(count_var), 1, 10),!!count_var := ifelse(!!sym(count_var) <= 10, NA, !!sym(count_var))
  ))
}

sys_comp_plot_df <- reactiveVal()

sys_comp_plot_2vars <- function() {
  # race/ethnicity, if selected, should always be on the row
  if (input$system_composition_selections[1] == "All Races/Ethnicities" |
      input$system_composition_selections[1] == "Grouped Races/Ethnicities" |
      input$system_composition_selections[1] == "Hispanic-Focused Races/Ethnicities") {
    x <- input$system_composition_selections[2]
    input$system_composition_selections[2] <- input$system_composition_selections[1]
    input$system_composition_selections[1] <- x
  }
  
  plot_df <- get_sys_comp_plot_df()
  validate(
    need(sum(plot_df$n > 0, na.rm = TRUE) > 0, message = "No data to show"),
    need(sum(plot_df$n > 10, na.rm = TRUE) > 0, message = "Not enough data to show")
  )
  
  if (all(is.na(plot_df$n)))
    return()
  v_cats1 <- get_v_cats(input$system_composition_selections[1])
  v_cats1_labels <- if (is.null(names(v_cats1))) {
    v_cats1
  } else {
    names(v_cats1)
  }
  
  v_cats2 <- get_v_cats(input$system_composition_selections[2])
  v_cats2_labels <- if (is.null(names(v_cats2))) {
    rev(v_cats2)
  } else {
    rev(names(v_cats2))
  }
  
  plot_df[input$system_composition_selections[1]] <- factor(
    plot_df[[input$system_composition_selections[1]]], 
    levels = v_cats1, 
    labels = v_cats1_labels)
  
  plot_df[input$system_composition_selections[2]] <- factor(
    plot_df[[input$system_composition_selections[2]]], 
    levels = rev(v_cats2), 
    labels = v_cats2_labels)
  
  plot_df <- plot_df %>%
    complete(
      !!sym(input$system_composition_selections[1]),
      !!sym(input$system_composition_selections[2])
    ) %>%
    replace(is.na(.), 0)
  
  h_total <- plot_df %>%
    group_by(!!!syms(input$system_composition_selections[[2]])) %>%
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
    mutate(!!input$system_composition_selections[[1]] := 'Total') %>%
    suppress_values("N") %>%
    suppress_next_val_if_one_suppressed_in_group(input$system_composition_selections[1], "N")
  
  v_total <- plot_df %>%
    group_by(!!!syms(input$system_composition_selections[[1]])) %>%
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
    mutate(!!input$system_composition_selections[[2]] := 'Total') %>%
    suppress_values("N") %>%
    suppress_next_val_if_one_suppressed_in_group(input$system_composition_selections[2], "N")
  
  # save before supressing the values
  # this will be used for the download/export
  sys_comp_plot_df(plot_df)
  
  # Suppress values <= 10
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(input$system_composition_selections[1], "n") %>%
    suppress_next_val_if_one_suppressed_in_group(input$system_composition_selections[2], "n")
  
  return(
    ggplot(plot_df, aes(.data[[input$system_composition_selections[1]]], .data[[input$system_composition_selections[2]]])) +
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
      # display like 14/(0.8%)
      # set text color to be 508 compliant contrasting
      geom_text(
        # aes(label = paste0(scales::comma(n), "\n", "(",scales::percent(pct, accuracy = 0.1),")")),
        aes(label = ifelse(wasRedacted, "***", scales::comma(n))),
        size = font_size,
        color = ifelse(
          plot_df$n > mean(plot_df$n, na.rm = TRUE) & !plot_df$wasRedacted,
          'white',
          'black'
        )
      ) +
      
      # Row totals
      ggnewscale::new_scale("fill") +
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
        aes(label = ifelse(wasRedacted, "***", # paste0(scales::comma(N), "\n", "(",scales::percent(N/sum(N, na.rm=TRUE), accuracy = 0.1),")")
                           scales::comma(N))),
        size = font_size,
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
        aes(label = ifelse(wasRedacted, "***", # paste0(N, "\n", "(",scales::percent(N/sum(N, na.rm=TRUE), accuracy = 0.1),")")
                           scales::comma(N))),
        size = font_size,
        color = ifelse(
          v_total$N > mean(v_total$N, na.rm = TRUE) & !v_total$wasRedacted,
          'white',
          'black'
        ),
        data = v_total
      ) +
      
      # axis labels
      scale_x_discrete(
        labels = str_wrap(c(v_cats1_labels, "Total"), width = 20),
        limits = c(levels(plot_df[[input$system_composition_selections[1]]]), "Total"),
        position = "top"
      ) +
      scale_y_discrete(
        labels = str_wrap(c("Total", v_cats2_labels), width = 30),
        limits = c("Total", levels(plot_df[[input$system_composition_selections[2]]])),
      ) +
      
      # other stuff
      theme_bw() +
      
      ggtitle(sys_total_count_display(
        nrow(sys_df_people_universe_filtered_r())
      )) +
      
      theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 17, hjust = 0.5),
        axis.title.x = element_text(input$system_composition_selections[1], size = 13),
        axis.title.y = element_text(input$system_composition_selections[2], size = 13),
        axis.title.x.top = element_text(margin = margin(0, 0, 15, 0)),
        axis.text = element_text(size = 14)
      )
  )
}

sys_comp_selections_summary <- function() {
  return(
    sys_export_summary_initial_df() %>%
      bind_rows(data.frame(
        Chart = c(
          "Demographic Selection 1",
          "Demographic Selection 2",
          "Total Served People"
        ),
        Value = c(
          input$system_composition_selections[1],
          input$system_composition_selections[2],
          nrow(sys_df_people_universe_filtered_r())
        )
      )) %>%
      rename("Composition of All Served" = Value)
  )
}

output$sys_comp_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Composition Report - "),
  content = function(file) {
    v1 <- gsub(input$system_composition_selections[1], "Races/Ethnicities", "Race")
    v2 <- gsub(input$system_composition_selections[2], "Races/Ethnicities", "Race")
    
    # get the n matrix
    num_matrix <- sys_comp_plot_df() %>%
      xtabs(n ~ 
              .[[input$system_composition_selections[2]]] +
              .[[input$system_composition_selections[1]]], 
            data = .) %>%
      replace(is.na(.), 0)
    
    # get the pct matrix with x.y% format
    pct_matrix <- ((num_matrix / sum(num_matrix)) * 100) %>%
      replace(is.na(.), 0) %>%
      addmargins(FUN = sum) %>% # add total row and column 
      apply(c(1, 2), function(x) paste0(format(round(x, 1), nsmall = 1), "%")) 
    
    
    # convert to dataframe
    # rename the Total row/column
    # add row names (i.e. var1) as separate column so it won't be excluded
    convert_to_df_for_export <- function(.data) {
      return(
        .data  %>%
          as.data.frame.matrix() %>%
          `rownames<-`(c(rownames(.)[-nrow(.)], "Total")) %>%
          `colnames<-`(c(colnames(.)[-ncol(.)], "Total")) %>%
          cbind(" " = rownames(.), .)
      )
    }
    num_matrix_df <- num_matrix %>% 
      addmargins(FUN = sum) %>% # add total row and column 
      convert_to_df_for_export()
    
    pct_matrix_df <- pct_matrix %>% 
      convert_to_df_for_export()
    
    # create a list of the 3 excel tabs and export
    tab_names <- list(
      "Composition All Served Summary" = sys_comp_selections_summary()
    )
    tab_names[[glue("Selected {v1} By {v2} #")]] <- num_matrix_df
      
    tab_names[[glue("Selected {v1} By {v2} %")]] <- pct_matrix_df
    
    write_xlsx(
      tab_names,
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    logMetadata(paste0(
      "Downloaded Sys Comp Report",
      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")
    ))
    
    exportTestValues(sys_comp_report = sys_comp_p())
  }
)

sys_comp_p <- reactive({
  req(!is.null(input$system_composition_selections) & valid_file() == 1)
  
  if(length(input$system_composition_selections) == 1) {
    sys_comp_plot_1var()
  } else {
    sys_comp_plot_2vars()
  }
})

observeEvent(input$system_composition_filter, {
  # they can select up to 2
  if(length(input$system_composition_filter) > 2){
    updateCheckboxGroupInput(
      session, 
      "system_composition_filter", 
      selected = tail(input$system_composition_filter,2),
      inline = TRUE)
  }
})

observeEvent(input$system_composition_selections, {
  # they can select up to 2
  #disable all unchecked boxes if they've already selected 2
  shinyjs::runjs(str_glue("
      var numSelected = {length(input$system_composition_selections)};
      $('#system_composition_selections input[type=checkbox]:not(\":checked\")')
        .attr('disabled', numSelected == 2);
    "))
  
  #disable all other Race/Ethnicity boxes if they've already selected 1
  shinyjs::runjs(str_glue("
      var reSelected = \"{
        \"All Races/Ethnicities\" %in% input$system_composition_selections |
        \"Hispanic-Focused Races/Ethnicities\" %in% input$system_composition_selections |
        \"Grouped Races/Ethnicities\" %in% input$system_composition_selections
      }\";
      $('#system_composition_selections input[type=checkbox][value*=\"Races/Ethnicities\"]:not(\":checked\")')
        .attr('disabled', reSelected == 'TRUE');
    "))
})


output$sys_comp_summary_selections <- renderUI({
  req(!is.null(input$system_composition_selections) & valid_file() == 1)
  syscomp_detailBox()
})

output$sys_comp_summary_ui_chart <- renderPlot({
  sys_comp_p()
}, height = function() { 
  if_else(!is.null(input$system_composition_selections), 600, 100) 
})
