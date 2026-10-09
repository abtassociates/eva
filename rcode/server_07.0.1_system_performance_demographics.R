
# Demographic chart-specific stuff-----------------
sys_demographics_selection_info <- function(type = 'overview', selection){
  
  Chart <- "Demographic Selection 1"
  Value <- selection[1]
  
  if(length(selection) == 2) {
    Chart <- c(Chart, "Demographic Selection 2")
    Value <- c(Value, selection[2])
  }
  
  if(type == 'overview'){
    df <- data.frame(
      Chart = c(
        Chart,
        "Total Served People"
      ),
      Value = c(
        Value,
        fnrow(syso_get_people_universe_filtered() %>% remove_non_applicables(selection = selection))
      )
    )
  } else if (type == 'exits'){
    df <- data.frame(
      Chart = c(
        Chart, 
        "Total People with System Exit",
        "Total People with PH System Exit"
      ),
      Value = c(
        Value, 
        fnrow(all_filtered_syse_demog()),
        fnrow(all_filtered_syse_demog() %>% fsubset(Destination %in% perm_livingsituation))
      )
    )
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

get_sys_plot_df_1var <- function(comp_df, var_col, selection) {
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
      plot_df <- plot_df %>% 
        rowbind(
          tibble(
            `Domestic Violence Status` = "DVTotal",
            n = plot_df %>% 
              fsubset(`Domestic Violence Status` != "NotDV") %>%
              fsum(n)
          )
        )
    }
  }
  return(plot_df)
}

get_sys_plot_df_2vars <- function(comp_df, var_cols, selections) {
  
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
      var_cats <- var_cols[[selections[i]]]
      if (length(var_cats) > 1) {
        freq_df <- freq_df %>%
          fsubset(gv(., selections[i]) == 1) %>%
          mutate(!!sym(selections[i]) := v)
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
      fgroup_by(!!sym(
        ifelse(
          selections[1] == "Domestic Violence Status",
          selections[2],
          selections[1]
        )
      )) %>%
      fsummarize(
        `Domestic Violence Status` = "DVTotal",
        n = fsum(n)
      ) #,
    # pct = sum(pct, na.rm = TRUE))
    freqs <- rowbind(freqs, dv_totals)
  }
  
  return(freqs)
}

build_demographic_heatmap <- function(plot_df, 
                                      selections, 
                                      metric_type = c("count", "ratio"), # "count" = System Comp, "ratio" = Exits/PHD
                                      palette_type = c("purple", "green"),
                                      methodology_type = 1,
                                      margin_totals = NULL, # list(h_total = df, v_total = df) for 2D Method 1
                                      total_count_display = "", # Title text generated by syse_total_count_display()
                                      isExport = FALSE) {
  
  is_2d        <- length(selections) == 2
  
  # 1. Axis Variable Mapping
  # For 1D, X is empty "" and Y is the selection. For 2D, X is selections[1], Y is selections[2]
  x_var <- if (is_2d) selections[1] else ""
  y_var <- if (is_2d) selections[2] else selections[1]
  
  # 2. Color Palette Setup
  colors <- list(
    low = get_brand_color(paste0('very_light_', palette_type)),
    mid = get_brand_color(paste0('light_', palette_type)),
    high = get_brand_color(paste0('dark_', palette_type))
  )
  
  # 3. Pre-calculate Display Values (Fill Value, Text Labels)
  plot_df <- plot_df %>%
    fmutate(
      # Fill value for gradient
      fill_val = if (metric_type == "ratio") {
        ifelse(is.na(frac) & wasRedacted, 0, frac)
      } else {
        ifelse(is.na(n) & wasRedacted, 0, n)
      },
      # Cell text labels
      label_text = if (metric_type == "ratio") {
        ifelse(wasRedacted, "***", paste0(scales::percent(frac, accuracy = 1), '\n(', 
                                          format(num, big.mark = ',', scientific = FALSE, trim = TRUE), ' of ', 
                                          format(n, big.mark = ',', scientific = FALSE, trim = TRUE), ')'))
      } else {
        ifelse(wasRedacted, "***", format(n, big.mark = ',', scientific = FALSE, trim = TRUE))
      }
    )
  
  font_size <- sys_chart_text_font * if (isExport) sys_chart_export_font_reduction * 0.6 else 1
  
  # 4. Base Heatmap Plot
  g <- ggplot(plot_df, aes(x = if (is_2d) .data[[x_var]] else "", y = .data[[y_var]])) +
    geom_tile(
      color = '#f0f0f0', lwd = 0.5, linetype = 1,
      aes(fill = fill_val) # <--- fill_val used here
    ) +
    scale_fill_gradient2(
      low = colors$low, mid = colors$mid, high = colors$high,
      midpoint = 0, na.value = 'white'
    ) +
    geom_text(
      aes(
        label = label_text,
        color = ifelse(fill_val > mean(fill_val, na.rm = TRUE) & !wasRedacted, 'white', 'black')
      ),
      size = font_size
    ) +
    scale_color_identity()
  
  # 5. Handle Discrete Axis Scales (1D vs 2D)
  y_labels <- get_selection_cats(y_var, type = ifelse(metric_type == "ratio", "exits", "overview"))
  y_labels_names <- if (is.null(names(y_labels))) y_labels else names(y_labels)
  
  if (is_2d) {
    x_labels <- get_selection_cats(x_var, type = ifelse(metric_type == "ratio", "exits", "overview"))
    x_labels_names <- if (is.null(names(x_labels))) x_labels else names(x_labels)
    
    if (methodology_type == 1 && !is.null(margin_totals)) {
      x_labels_names <- c(x_labels_names, "Total")
      y_labels_names <- c("Total", y_labels_names)
    }
    
    g <- g + 
      scale_x_discrete(
        labels = str_wrap(x_labels_names, width = 20),
        limits = if (methodology_type == 1) c(levels(plot_df[[x_var]]), "Total") else levels(plot_df[[x_var]]),
        position = "top"
      ) +
      scale_y_discrete(
        labels = str_wrap(y_labels_names, width = 30),
        limits = if (methodology_type == 1) c("Total", rev(levels(plot_df[[y_var]]))) else rev(levels(plot_df[[y_var]]))
      )
  } else {
    g <- g + scale_y_discrete(
      labels = label_wrap(30),
      limits = rev(levels(plot_df[[y_var]]))
    )
  }
  
  # 6. Append Margin Totals for 2D Methodology 1
  if (is_2d && methodology_type == 1 && !is.null(margin_totals)) {
    add_total_layer <- function(plot_obj, total_df) {
      val_col <- if ("N" %in% names(total_df)) "N" else "n"
      plot_obj +
        ggnewscale::new_scale("fill") +
        geom_tile(data = total_df, aes(fill = .data[[val_col]]), color = "white", lwd = 0.5) +
        scale_fill_gradient(low = get_brand_color('light_grey'), high = get_brand_color('dark_grey'), na.value = 'white') +
        geom_text(
          data = total_df,
          aes(label = ifelse(wasRedacted, "***", format(.data[[val_col]], big.mark = ','))),
          size = font_size,
          color = "black"
        )
    }
    
    g <- g %>% 
      add_total_layer(margin_totals$h_total) %>% 
      add_total_layer(margin_totals$v_total)
  }
  
  # 7. Add Styling, Theme, Title
  g +
    theme_bw() +
    ggtitle(total_count_display) +
    labs(caption = "*** indicates the value is suppressed") +
    theme(
      text = element_text(size = sys_chart_text_font_pts),
      legend.position = "none",
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
    )
}

prepare_crosstab_data <- function(df, selections, methodology_type, subtab = 'comp') {
  # 1. Enforce Race/Ethnicity ordering (2D)
  if (length(selections) == 2 && selections[1] %in% c("All Races/Ethnicities", "Grouped Races/Ethnicities")) {
    selections <- c(selections[2], selections[1])
  }
  
  # 2. Extract dynamic variable columns & filter applicables
  var_cols <- get_var_cols(methodology_type)
  sel_cols <- unname(unlist(var_cols[selections]))
  
  clean_df <- df %>% 
    remove_non_applicables(selection = selections) %>% 
    fselect(c("PersonalID", sel_cols)) %>% 
    funique()
  
  # 3. Validate minimum thresholds
  validate(need(nrow(clean_df) > 0, message = no_data_msg))
  validate(need(nrow(clean_df) > 10, message = suppression_msg))
  
  # 4. Generate frequency table (1D vs 2D)
  plot_df <- if (length(selections) == 1) {
    get_sys_plot_df_1var(clean_df, var_cols[[selections]], selection = selections)
  } else {
    get_sys_plot_df_2vars(clean_df, var_cols, selections = selections)
  }
  
  toggle_download_buttons(subtab, plot_df)
  
  # 5. Factor Level Assignment & Labeling
  type <- if (subtab == 'comp') 'overview' else 'exits'
  for (sel in selections) {
    cats <- get_selection_cats(sel, type = type)
    cat_labels <- if (is.null(names(cats))) cats else names(cats)
    plot_df[[sel]] <- factor(plot_df[[sel]], levels = cats, labels = cat_labels, ordered = (length(selections) == 1))
  }
  
  # 6. Complete missing grid combinations (2D only)
  if (length(selections) == 2) {
    plot_df <- plot_df %>%
      complete(!!!syms(selections)) %>%
      replace(is.na(.), 0)
  }
  
  list(clean_df = clean_df, plot_df = plot_df)
}

calc_margin_totals <- function(df, row_var, col_var, value_var = "n") {
  h_total <- df %>%
    fgroup_by(col_var) %>%
    fsummarise(N = if(allNA(get(value_var))) NA_real_ else fsum(get(value_var))) %>%
    mutate(!!sym(row_var) := 'Total') %>%
    suppress_values("N") %>%
    suppress_next_val_if_one_suppressed_in_group(row_var, "N")
  
  v_total <- df %>%
    fgroup_by(row_var) %>%
    fsummarise(N = if(allNA(get(value_var))) NA_real_ else fsum(get(value_var))) %>%
    mutate(!!sym(col_var) := 'Total') %>%
    suppress_values("N") %>%
    suppress_next_val_if_one_suppressed_in_group(col_var, "N")
  
  list(h_total = h_total, v_total = v_total)
}

sys_comp_data_download <- function(file, type = 'syso') {
  logToConsole(session, paste0("In sys_comp_data_download, where type = ", type))
  
  subtab <- ifelse(type == 'syso', 'comp', 'phd')
  methodology_type <- ifelse(type == 'syso', input$syso_methodology_type, input$syse_methodology_type)
  selections <- if(type == 'syso') input$syso_composition_selections else input$syse_phd_selections
  
  plot_df <- if(type == "syso") syso_comp_plot_df() else syse_phd_plot_df()
  if(is.null(plot_df)) {
    plot_df <- if(type == "syso")
      syso_comp_plot(
        methodology_type = methodology_type,
        selections = selections,
        isExport = FALSE
      )
    else
      syse_phd_plot(
        methodology_type = methodology_type,
        selections = selections,
        isExport = FALSE
      )
  }
  
  sys_heatmap_xl_export(
    file, 
    type = ifelse(type == 'syso', "overview","exits"),
    methodology_type = methodology_type,
    selections = selections,
    plot_df = plot_df,
    in_demo_mode = input$in_demo_mode
  )
}

limit_checkbox_selections <- function(input_id, selected_values) {
  num_selected <- length(selected_values)
  re_selected  <- any(c("All Races/Ethnicities", "Grouped Races/Ethnicities") %in% selected_values)
  
  js_code <- sprintf("
    var numSelected = %d;
    var reSelected = %s;
    
    // Disable unchecked options if 2 are selected
    $('input[name=\"%s\"]:not(\":checked\")').attr('disabled', numSelected >= 2);
    
    // If one Race/Ethnicity is checked, disable the other Race/Ethnicity variant
    if (numSelected === 1 && reSelected) {
      $('input[name=\"%s\"][value*=\"Races/Ethnicities\"]:not(\":checked\")').attr('disabled', true);
    }
  ", num_selected, tolower(as.character(re_selected)), input_id, input_id)
  
  shinyjs::runjs(js_code)
}

sys_comp_ppt_download <- function(file, type = 'syso') {
  logToConsole(session, paste0("In sys_comp_ppt_download, where type = ", type))
  type <- ifelse(type == 'syso', 'overview', 'exits')
  
  selections <- if(type == 'overview') input$syso_composition_selections else input$syse_phd_selections
  slide_title <- ifelse(type == 'overview', "System Demographics", "System Exits Permanent Housing (PH) Demographics")
  plot_title <- paste0(
    ifelse(type == 'overview', "System Demographics: ",  "System Exits PH Demographics: "),
    selections[1],
    ifelse(length(selections) == 1, "", paste0(" by ", selections[2]))
  )
  selections_info <- if(type == 'overview') syso_comp_selections_info() else sys_phd_selections_info()
  methodology_type <- ifelse(type == 'overview', input$syso_methodology_type, input$syse_methodology_type)
  
  active_plot <- if (type == 'overview') {
    syso_comp_plot(
      methodology_type = methodology_type, 
      selections = selections, 
      isExport = TRUE
    )
  } else {
    syse_phd_plot(
      methodology_type = methodology_type, 
      selections = selections, 
      isExport = TRUE
    )
  }
  
  sys_perf_ppt_export(
    file = file,
    type = type,
    title_slide_title = slide_title,
    summary_items = sys_export_summary_initial_df(type = type) %>%
      fsubset(Chart != "Start Date" & Chart != "End Date") %>% 
      rowbind(selections_info),
    plots = setNames(
      list(active_plot),
      plot_title
    ),
    summary_font_size = 28,
    startDate = session$userData$ReportStart, 
    endDate = session$userData$ReportEnd, 
    sourceID = session$userData$Export$SourceID,
    in_demo_mode = input$in_demo_mode
  )
}
