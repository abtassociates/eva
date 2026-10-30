mod_system_demog_ui <- function(id, title_prefix, instructions) {
  ns <- NS(id)
  nav_panel(
    title = headerTab(glue("{title_prefix} Demographics")),
    navset_underline(
      id = ns("demog_subtabs"),
      selected = headerSubTab("Chart"),
      nav_panel(
        title = headerSubTab("Chart"),
        card(
          strong("Select Demographic Crosstab Categories (up to 2)"),
          p(str_glue(
            "For a simple count of totals within a demographic 
                                   category, select only one category. To see the 
                                   intersection of two demographic categories, select 
                                   both categories to create a crosstab chart. To 
                                   change your crosstab selection, uncheck at least 
                                   one of your previous selections before selecting 
                                   new categories. Note that you can only select one Race/Ethnicity 
                                   category to display in the chart at a time."
          )),
          checkboxGroupInput(
            ns("demog_selections"),
            label = "",
            choices = sys_heatmap_selection_choices,
            selected = c("All Races/Ethnicities", "Age"),
            inline = TRUE
          ),
          width = 12
        ),
        # uiOutput(outputId = glue("demog_filter_selections")) %>%
        #   withSpinner(),
        plotOutput(outputId = ns('demog_chart')) %>% withSpinner()
      ),
      nav_panel(
        title = headerSubTab("Information"),
        br(),
        instructions#tab_unsh_demog_subtabs_information
      )
    )
  )
}

mod_system_demog_server <- function(id, type = 'overview', people_univ, display_var, filter_vals, global_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      
      demog_comp_plot_df <- reactiveVal()
      
      demog_selection_info <- reactive({
        sys_demographics_selection_info(type = type, selection = input$demog_selections)
        
      })
      
      sys_demog_clean_df <- reactive({
        people_univ %>% #demog_people_universe_filtered()
          remove_non_applicables(selection = input$demog_selections) %>% 
          clean_comp_data(filter_vals$methodology_type(), input$demog_selections)
      })
      
      syso_comp_selections_summary <- function() {
        return(
          sys_export_summary_initial_df(type = 'overview') %>%
            bind_rows(demog_selection_info()) %>%
            rename("System Demographics" = Value)
        )
      }
      
      output$demog_filter_selections <-renderUI({ 
        req(session$userData$valid_file() == 1 )
        
        sys_detailBox(
          detail_type = id,
          methodology_type = filter_vals$methodology_type(),
          cur_project_types = filter_vals$project_type(),
          startDate = session$userData$ReportStart,
          endDate = session$userData$ReportEnd,
          age = filter_vals$age(),
          spec_pops = filter_vals$spec_pops(),
          race_eth = filter_vals$race_ethnicity()
        )
      })
      
      demog_chart_validation <- reactive({
        eval_chart_validity(counts = fnrow(people_univ), min_count = 10, session = session)
      })
      
      output$demog_chart <- renderPlot({
        req(
          !is.null(input$demog_selections) &
            session$userData$valid_file() == 1 &
            between(length(input$demog_selections), 1, 2)
        )
        
        validate_chart(demog_chart_validation())
        
        build_demog_plot(subtab='demog',
                         prefix='unsh',
          methodology_type = filter_vals$methodology_type(), 
          selections = input$demog_selections, 
          people_univ = people_univ,
          isExport = FALSE,
          session = session, 
          display_var = display_var
        )
        
      }, height = function() {
        ifelse(!is.null(input$demog_selections), 700, 100)
      }, width = function() {
        input$demog_subtabs
        input[[glue('{id}_tabbox')]]
        input$pageid
        if (length(input$demog_selections) == 1 |
            isTRUE(getOption("shiny.testmode"))) {
          500
        } else {
          "auto"
        }
      }, alt = "A crosstab data table of the demographic make-up of the homeless system.")
      
      
      observeEvent(input$demog_selections, {
        
        limit_checkbox_selections(session$ns('demog_selections'), input$demog_selections)
      }, ignoreNULL = FALSE)
    
    }
  )
}

build_demog_plot <- function(subtab, prefix, methodology_type, selections, people_univ = NULL, 
                           session = session, isExport = FALSE, display_var) {
  # 1. Run Data Preparation Pipeline
  prep <- prepare_crosstab_data(people_univ, selections, methodology_type, subtab = subtab, prefix=prefix, session = session)
  
  plot_df <- prep$plot_df
  
  # 2. Save unsuppressed df for downloads
  #demog_comp_plot_df(plot_df)
  
  # 3. Calculate Method 1 Row/Col Totals (2D only)
  margin_totals <- NULL
  if (length(selections) == 2 && methodology_type == 1) {
    margin_totals <- calc_margin_totals(plot_df, selections[1], selections[2]) # uses helper from earlier
  }
  
  # 4. Apply Suppression
  plot_df_supp <- plot_df %>% suppress_values("n")
  for (s in selections) {
    plot_df_supp <- plot_df_supp %>% suppress_next_val_if_one_suppressed_in_group(s, "n")
  }
  
  enough_data <- fsum(plot_df_supp$n) > 0
  validate(need(enough_data, message = all_data_suppressed_msg))
  
  # 5. Render Unified Heatmap
  build_demographic_heatmap(
    plot_df = plot_df_supp,
    selections = selections,
    metric_type = "count",
    palette_type = "purple",
    methodology_type = methodology_type,
    margin_totals = margin_totals,
    total_count_display = sys_total_count_display(nrow(prep$clean_df), display_var),
    isExport = isExport
  )
}
# Demographic chart-specific stuff-----------------
sys_demographics_selection_info <- function(type = 'overview', selection, people_univ){
  
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
        fnrow(people_univ %>% remove_non_applicables(selection = selection))
        #fnrow(syso_get_people_universe_filtered() %>% remove_non_applicables(selection = selection))
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
        fnrow(people_univ),#fnrow(all_filtered_syse_demog()),
        fnrow(people_univ %>% fsubset(Destination %in% perm_livingsituation))
      )
    )
  }
  
  return(df)
}

## clean
get_selection_cats <- function(selection, methodology_type) {
  # this gets all the categories of the selected variable
  # this is used to make sure even empty categories are included in the chart
  # methodology_type <- switch(type,
  #                            'overview' = input$syso_methodology_type,
  #                            'exits' = input$syse_methodology_type)
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

## clean
eval_chart_validity <- function(counts, min_count = 10, session = session) {
  # # 1. Check Master Data directly
  # AS 9/30/26: Is this needed or do we handle this in 07?
  master_df <- session$userData$enrollment_categories
  if (is.null(master_df) || fnrow(master_df) == 0) {
    return(list(valid = FALSE, message = no_valid_data_msg))
  }
  
  # Flatten to a numeric vector (handles single number or c(n1, n2))
  counts <- unlist(counts)
  
  # 2. Check if any count is zero or missing
  if (length(counts) == 0 || any(is.na(counts)) || any(counts == 0)) {
    return(list(valid = FALSE, message = no_data_msg))
  }
  
  # 3. Check suppression threshold (> 10)
  if (any(counts <= min_count)) {
    return(list(valid = FALSE, message = suppression_msg))
  }
  
  # Passed all validations
  list(valid = TRUE, message = NULL)
}

# Shiny UI wrapper for renderPlot / renderDT
validate_chart <- function(val) {
  validate(need(val$valid, message = val$message))
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
      aes(fill = fill_val)
    ) +
    scale_fill_gradient2(
      low = colors$low, mid = colors$mid, high = colors$high,
      midpoint = 0, na.value = 'white'
    ) +
    geom_text(
      aes(
        label = label_text,
        color = ifelse(fill_val > fmean(fill_val) & !wasRedacted, 'white', 'black')
      ),
      size = font_size
    ) +
    scale_color_identity()
  
  # 5. Handle Discrete Axis Scales (1D vs 2D)
  y_labels <- get_selection_cats(y_var, methodology_type)#, type = ifelse(metric_type == "ratio", "exits", "overview"))
  y_labels_names <- if (is.null(names(y_labels))) y_labels else names(y_labels)
  
  if (is_2d) {
    x_labels <- get_selection_cats(x_var, methodology_type)#, type = ifelse(metric_type == "ratio", "exits", "overview"))
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
        # Row totals
        geom_tile(
          data = total_df, 
          color = "white", 
          lwd = 0.5,
          linetype = 1,
          aes(fill = .data[[val_col]])
        ) +
        
        scale_fill_gradient(
          low = get_brand_color('light_grey'), 
          high = get_brand_color('dark_grey'), 
          na.value = 'white'
        ) +
        
        geom_text(
          aes(label = ifelse(wasRedacted, "***", format(.data[[val_col]], big.mark = ',', scientific = FALSE, trim = TRUE))),
          size = font_size,
          color = ifelse(
            total_df$N > fmean(total_df$N) & !total_df$wasRedacted,
            'white', 
            'black'
          ),
          na.rm = TRUE,
          data = total_df
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

## clean
prepare_crosstab_data <- function(clean_df, selections, methodology_type, subtab = 'comp', prefix = 'syso', session = session) {
  
  
  # 1. Enforce Race/Ethnicity ordering (2D)
  if (length(selections) == 2 && selections[1] %in% c("All Races/Ethnicities", "Grouped Races/Ethnicities")) {
    selections <- c(selections[2], selections[1])
  }
  
  # 2. Extract dynamic variable columns & filter applicables
  var_cols <- get_var_cols(methodology_type, race_ethnicity_func = sys_race_ethnicity_cats)
  sel_cols <- unname(unlist(var_cols[selections]))
  
  # 4. Generate frequency table (1D vs 2D)
  plot_df <- if (length(selections) == 1) {
    get_sys_plot_df_1var(clean_df, var_cols[[selections]], selection = selections, session = session)
  } else {
    get_sys_plot_df_2vars(clean_df, var_cols, selections = selections, session = session)
  }
  
  toggle_download_buttons(prefix = prefix, subtab, plot_df)
  
  # 5. Factor Level Assignment & Labeling
  type <- switch(subtab,
         'comp' = 'overview',
         'phd' = 'exits',
         'demog' = 'unsh',
         # otherwise, use default
         'unsh')
  
  for (sel in selections) {
    cats <- get_selection_cats(sel, methodology_type = methodology_type)
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

## clean
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

sys_comp_data_download <- function(file, page='syso', plot_df, selections, methodology_type, global_vars) {
  logToConsole(session, paste0("In sys_comp_data_download, where type = ", type))
  
  type <- switch(page, 'syso'='overview', 'syse'='exits','unsh'='unsheltered')
  subtab <- switch(type, 'syso'='comp', 'syse'='phd','unsh'='demog')
 #methodology_type <- ifelse(type == 'syso', input$syso_methodology_type, input$syse_methodology_type)
  #selections <- if(type == 'syso') input$syso_composition_selections else input$syse_phd_selections
  
  #plot_df <- if(type == "syso") syso_comp_plot_df() else syse_phd_plot_df()
  if(is.null(plot_df)) {
    if(type == 'syso'){
      plot_df <-
        syso_comp_plot(
          methodology_type = methodology_type,
          selections = selections,
          isExport = FALSE
        )
    } else if (type == 'syse'){
      syse_phd_plot(
        methodology_type = methodology_type,
        selections = selections,
        isExport = FALSE
      )
    } else if (type == 'unsh'){
      
    }
  }
  
  sys_heatmap_xl_export(
    file, 
    type = type,#ifelse(type == 'syso', "overview","exits"),
    methodology_type = methodology_type,
    selections = selections,
    plot_df = plot_df,
    summary_df = NULL,
    export_df = NULL,
    in_demo_mode = global_vars$in_demo_mode
  )
}

## clean
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

sys_comp_ppt_download <- function(file, page = 'syso', selections, methodology_type, people_univ, selection_info, display_var, session = session) {
  logToConsole(session, paste0("In sys_comp_ppt_download, where type = ", page))
  type <- switch(page, 'syso'='overview', 'syse'='exits','unsh'='unsheltered')
  
  #selections <- if(type == 'overview') input$syso_composition_selections else input$syse_phd_selections
  slide_title <- switch(type == 'overview', 
                        "overview"="System Demographics", 
                        "exits"="System Exits Permanent Housing (PH) Demographics",
                        "unsheltered"="Unsheltered Homelessness Demographics")
  plot_title <- paste0(
    switch(type, 'overview' = "System Demographics: ", 
           "exits"="System Exits PH Demographics: ", 
           "unsheltered" = "Unsheltered Demographics: "),
    selections[1],
    ifelse(length(selections) == 1, "", paste0(" by ", selections[2]))
  )
  #selections_info <- if(type == 'overview') syso_comp_selections_info() else sys_phd_selections_info()
  #methodology_type <- ifelse(type == 'overview', input$syso_methodology_type, input$syse_methodology_type)
  
  if (type == 'overview'){
    active_plot <-  
      syso_comp_plot(
        subtab = 'syso',
        methodology_type = methodology_type, 
        selections = selections,
        people_univ = people_univ,
        isExport = TRUE,
        session = session,
        display_var = display_var
      )
  } else if(type == 'exits'){
    active_plot <- syse_phd_plot(
      methodology_type = methodology_type, 
      selections = selections, 
      isExport = TRUE
    )
  } else if (type == 'unsheltered'){
    active_plot <- syso_comp_plot(subtab = 'unsh',
      methodology_type = methodology_type, 
      selections = selections, 
      people_univ = people_univ,
      isExport = TRUE,
      session = session,
      display_var = display_var
    )
  }
  
  sys_perf_ppt_export(
    file = file,
    type = type,
    title_slide_title = slide_title,
    summary_items = sys_export_summary_initial_df(type = type) %>%
      fsubset(Chart != "Start Date" & Chart != "End Date") %>% 
      rowbind(selection_info),
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

## clean
clean_comp_data <- function(data, methodology_type, selections) {
  var_cols <- get_var_cols(methodology_type)
  sel_cols <- unname(unlist(var_cols[selections]))
  
  data %>%
    fselect(c("PersonalID", sel_cols)) %>% 
    funique()
}