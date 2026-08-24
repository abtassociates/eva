# ==============================================================================
# Helpers
# ==============================================================================

get_subpop_labels <- function(which_factors) {
  m_type <- input$syse_methodology_type
  re_val <- input$syse_subpop_race_ethnicity
  
  changed <- c(
    meets_hh_type = if ('meets_hh_type' %in% which_factors && input$syse_subpop_hh_type != 'All') {
      gsub('^- ', '', getNameByValue(sys_hh_types, input$syse_subpop_hh_type))
    } else NA_character_,
    meets_age_filter = if ('meets_age_filter' %in% which_factors && length(input$syse_subpop_age) < length(sys_age_cats)) {
      paste0(input$syse_subpop_age, collapse = ', ')
    } else NA_character_,
    meets_race_eth_filter = if ('meets_race_eth_filter' %in% which_factors && !re_val %in% c('All', 'None Selected')) {
      paste0(getNameByValue(sys_race_ethnicity_cats(m_type), re_val), collapse = ',')
    } else NA_character_,
    meets_vet_filter = if ('meets_vet_filter' %in% which_factors && !input$syse_subpop_spec_pops %in% c('None', 'None Selected')) {
      input$syse_subpop_spec_pops
    } else NA_character_
  )
  
  all_other <- c(
    meets_hh_type = 'All Other Household Types',
    meets_age_filter = 'All Other Ages',
    meets_race_eth_filter = 'All Other Races/Ethnicities',
    meets_vet_filter = if ('meets_vet_filter' %in% which_factors) paste0(setdiff(c('Veteran', 'Non-Veteran'), input$syse_subpop_spec_pops), 's') else NA_character_
  )
  
  list(changed = changed, all_other = all_other)
}

apply_subpop_labels <- function(df, which_factors, labels) {
  for (f in which_factors) {
    if (f %in% names(df) && !is.na(labels$changed[f])) {
      levels(df[[f]]) <- c(labels$changed[f], labels$all_other[f])
    }
  }
  df
}

subpop_metadata_summary <- function() {
  sys_export_summary_initial_df(type = 'exits') %>%
    rowbind(
      sys_export_filter_selections(type = 'exits_subpop'),
      data.table(
        Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
        Value = scales::label_comma()(c(nrow(subpop()), nrow(everyone_else())))
      )
    )
}

# ==============================================================================
# Reactives & Logic
# ==============================================================================

subpop_chart_validation <- function(show = TRUE, req = FALSE) {
  logToConsole(session, "In subpop_chart_validation")
  validate(need(fnrow(session$userData$enrollment_categories) > 0, no_valid_data_msg))
  
  cond <- any(did_factors_change())
  if (show) {
    validate(need(cond, "Please select a household type or one or more demographic filters to generate the subpopulation chart."))
  } else if (req) {
    req(cond)
  } else {
    return(cond)
  }
}

syse_subpop_selections <- reactive({
  m_type <- input$syse_methodology_type
  default_race_val <- if (isTruthy(m_type) && m_type == 2) sys_race_ethnicity_method2[1] else sys_race_ethnicity_method1[1]
  default_vet_val <- sys_spec_pops_people[1]
  
  active <- character(0)
  
  # 1. Age is active if not all categories are selected (and not empty)
  if (isTruthy(input$syse_subpop_age) && 
      length(input$syse_subpop_age) > 0 && 
      length(input$syse_subpop_age) < length(sys_age_cats)) {
    active <- c(active, "Age")
  }
  
  # 2. Race/Ethnicity is active if changed from default / "All" / "None Selected"
  if (isTruthy(input$syse_subpop_race_ethnicity) && 
      !input$syse_subpop_race_ethnicity %in% c("None Selected", "All", default_race_val, "")) {
    active <- c(active, "Race/Ethnicity")
  }
  
  # 3. Veteran Status is active if changed from default / "None" / "None Selected"
  if (isTruthy(input$syse_subpop_spec_pops) && 
      !input$syse_subpop_spec_pops %in% c("None Selected", "None", default_vet_val, "")) {
    active <- c(active, "Veteran Status (Adult Only)")
  }
  
  active
})

did_factors_change <- reactive({
  c(
    meets_hh_type = (isTruthy(input$syse_subpop_hh_type) && input$syse_subpop_hh_type != 'All'),
    meets_age_filter = ('Age' %in% syse_subpop_selections()),
    meets_race_eth_filter = ('Race/Ethnicity' %in% syse_subpop_selections()),
    meets_vet_filter = ('Veteran Status (Adult Only)' %in% syse_subpop_selections())
  )
})

observeEvent(input$syse_methodology_type, {
  req(input$syse_methodology_type)
  
  raw_choices <- if (input$syse_methodology_type == 1) {
    sys_race_ethnicity_method1
  } else {
    sys_race_ethnicity_method2
  }
  
  updatePickerInput(
    session = session,
    inputId = "syse_subpop_race_ethnicity",
    choices = setNames(raw_choices, nm = c("None Selected", names(raw_choices[-1]))),
    selected = raw_choices[1]
  )
})


compute_subpop_and_everyone_else <- function(input_df) {
  re_val <- input$syse_subpop_race_ethnicity
  
  df <- input_df %>%
    fmutate(
      meets_hh_type = if (input$syse_subpop_hh_type != 'All') !meets_ev_else else TRUE,
      meets_age_filter = if ('Age' %in% syse_subpop_selections()) { 
        req(input$syse_subpop_age)
        AgeCategory %in% input$syse_subpop_age 
      } else TRUE,
      meets_race_eth_filter = if ('Race/Ethnicity' %in% syse_subpop_selections()) {
        req(re_val)
        if (re_val %in% c("All", "None Selected")) TRUE else get(re_val) == 1
      } else TRUE,
      meets_vet_filter = if ('Veteran Status (Adult Only)' %in% syse_subpop_selections()) {
        req(input$syse_subpop_spec_pops)
        input$syse_subpop_spec_pops %in% c("None", "None Selected") |
          (input$syse_subpop_spec_pops == "Veteran" & VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
          (input$syse_subpop_spec_pops == "NonVeteran" & VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17"))
      } else TRUE
    )
  
  mask <- df$meets_hh_type & df$meets_age_filter & df$meets_race_eth_filter & df$meets_vet_filter
  
  rest <- df[!mask] %>%
    fmutate(
      meets_hh_type = factor(meets_hh_type, levels = c(TRUE, FALSE)),
      meets_age_filter = factor(meets_age_filter, levels = c(TRUE, FALSE)),
      meets_race_eth_filter = factor(meets_race_eth_filter, levels = c(TRUE, FALSE)),
      meets_vet_filter = factor(meets_vet_filter, levels = c(TRUE, FALSE))
    )
  
  list(subpop = df[mask], everyone_else = rest)
}

comps <- reactive({ compute_subpop_and_everyone_else(all_filtered_syse_subpop()) })
subpop <- reactive({ comps()$subpop })
everyone_else <- reactive({ comps()$everyone_else %>% add_destination_type() })

get_syse_compare_subpop_data <- function(output_type = 'table') {
  validate(
    need(nrow(subpop()) > 0, no_data_msg),
    need(nrow(subpop()) > 10, suppression_msg),
    need(nrow(everyone_else()) > 0, no_data_msg),
    need(nrow(everyone_else()) > 10, suppression_msg)
  )
  
  which_factors_changed <- names(which(did_factors_change()))
  filt_vars <- c('meets_hh_type', 'meets_age_filter', 'meets_race_eth_filter', 'meets_vet_filter')
  filt_unchanged <- setdiff(filt_vars, which_factors_changed)
  
  # 1. Combine data together
  # Get counts by Destination Type and ONLY the active factors
  all_data <- rowbind(
    subpop() %>% add_destination_type(as_factor = TRUE),
    everyone_else()
  ) %>%
    fcountv(cols = c("Destination Type", which_factors_changed), drop = FALSE)
  
  # 2. Identify subpopulation (where all active factors == TRUE)
  # browser()
  is_subpop <- Reduce(`&`, lapply(all_data[[which_factors_changed]], \(x) x == TRUE))
  
  # 3. Add unchanged factor columns back as TRUE for downstream consistency
  if (length(filt_unchanged) > 0) {
    for (v in filt_unchanged) all_data[[v]] <- factor(TRUE, levels = c(TRUE, FALSE))
  }
  
  # 4. Compute metrics
  all_data %>%
    fmutate(group = fifelse(is_subpop, "subpop", "everyone_else")) %>%
    fgroup_by(which_factors_changed) %>%
    fmutate(
      total = fsum(N),
      wasRedacted = total < 10, 
      pct = fifelse(wasRedacted & output_type == "chart", NA, N / total)
    ) %>%
    fungroup()
}

# ==============================================================================
# UI Observers & Renderers
# ==============================================================================
id_map <- list(
  "age_picker" = "Age",
  "vet_picker" = "Veteran Status (Adult Only)",
  "race_eth_picker" = "Race/Ethnicity"
)
observe({
  req(syse_subpop_selections())
  for(div_id in names(id_map)) {
    shinyjs::toggleState(
      id = div_id,
      condition = length(syse_subpop_selections()) < 2 || id_map[[div_id]] %in% syse_subpop_selections()
    )
  }
})

output$syse_compare_subpop_filter_selections <- renderUI({
  req(session$userData$valid_file() == 1 & did_factors_change())
  sys_detailBox(
    selection = syse_subpop_selections(),
    detail_type = 'subpop',
    methodology_type = input$syse_methodology_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_subpop_age,
    spec_pops = input$syse_subpop_spec_pops,
    race_eth = input$syse_subpop_race_ethnicity
  )
})

# ==============================================================================
# Plot & Exports
# ==============================================================================

syse_compare_subpop_chart <- function(subpop_data = get_syse_compare_subpop_data(output_type = 'chart'),
                                      dest_type = input$subpop_dest_type, isExport = FALSE) {
  req(all_filtered_syse_subpop())
  
  which_changed <- names(which(did_factors_change()))
  labels <- get_subpop_labels(which_changed)
  
  subpop_chart_df <- subpop_data %>%
    fsubset(`Destination Type` == dest_type) %>%
    apply_subpop_labels(which_changed, labels)
  
  title <- paste0("Total System Exits for \nSubpopulation: ", 
                  scales::comma(nrow(subpop())), "\nEveryone Else: ", scales::comma(nrow(everyone_else())))
  
  # Determine plot aesthetics based on factor counts
  n_factors <- length(which_changed)
  if (n_factors == 1) {
    g <- ggplot(subpop_chart_df, aes(x = !!sym(which_changed), y = 1))
  } else if (n_factors == 2) {
    x_var <- if ('meets_hh_type' %in% which_changed) 'meets_hh_type' else setdiff(which_changed, 'meets_race_eth_filter')[1]
    y_var <- setdiff(which_changed, x_var)
    g <- ggplot(subpop_chart_df, aes(x = !!sym(x_var), y = fct_rev(!!sym(y_var))))
  } else {
    vert_var <- if ('meets_race_eth_filter' %in% which_changed) 'meets_race_eth_filter' else which_changed[3]
    rem <- setdiff(which_changed, vert_var)
    horiz_inner <- if ('meets_hh_type' %in% which_changed) setdiff(rem, 'meets_hh_type') else rem[1]
    horiz_outer <- setdiff(rem, horiz_inner)
    
    g <- ggplot(subpop_chart_df, aes(x = !!sym(horiz_inner), y = fct_rev(!!sym(vert_var)))) +
      facet_wrap(as.formula(paste0('~ ', horiz_outer)), strip.position = 'top', scales = 'free_x', ncol = 2, labeller = label_wrap_gen(18))
  }
  
  g +
    geom_tile(color = '#f0f0f0', lwd = 0.5, aes(fill = group)) +
    scale_fill_manual(values = c('subpop' = get_brand_color('med_purple'), 'everyone_else' = get_brand_color('med_grey2'))) +
    geom_text(
      aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(pct, accuracy = 1), '\n(', scales::comma(N), ' of ', scales::comma(total), ')'))),
      size = sys_chart_text_font, color = "black"
    ) +
    scale_x_discrete(position = 'top', labels = label_wrap(25), expand = c(0, 0)) +
    scale_y_discrete(labels = label_wrap(25), expand = c(0, 0)) +
    labs(x = '', y = '', title = title) +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
      axis.line = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.placement = "outside",
      strip.text.x.top = element_text(size = sys_axis_text_font),
      axis.ticks = element_blank(),
      legend.position = 'none',
      axis.text.x = element_text(size = get_adj_font_size(sys_axis_text_font, isExport)),
      axis.text.y = element_text(size = sys_axis_text_font, hjust = 1),
      panel.background = element_rect(fill = 'white', colour = 'white')
    )
}

output$syse_compare_subpop_chart <- renderPlot({
  subpop_chart_validation(show = TRUE, req = FALSE)
  syse_compare_subpop_chart(dest_type = input$subpop_dest_type)
})

syse_subpop_export_summary <- reactive({
  which_changed <- names(which(did_factors_change()))
  labels <- get_subpop_labels(which_changed)
  
  export_names <- c(
    'Household Type' = 'meets_hh_type', 
    'Race/Ethnicity' = 'meets_race_eth_filter',
    'Age' = 'meets_age_filter', 
    'Veteran Status' = 'meets_vet_filter',
    'Suppression Flag' = 'wasRedacted', 
    'Count' = 'N',
    'Total System Exits' = 'total', 
    'Percent of Total System Exits' = 'pct'
  )
  
  get_syse_compare_subpop_data(output_type = 'table') %>%
    apply_subpop_labels(which_changed, labels) %>%
    fmutate(pct = scales::percent(ifelse(is.nan(pct), 0, pct), accuracy = 0.1)) %>%
    get_vars(vars = c(which_changed, 'Destination Type', 'N', 'total', 'pct', 'wasRedacted')) %>%
    roworderv(cols = c(which_changed, 'Destination Type')) %>%
    rename(any_of(export_names))
})

syse_subpop_export_detail <- reactive({
  summarize_dest <- function(df, prefix) {
    sub <- df %>%
      add_destination_type() %>%
      fmutate(`Destination Type Detail` = living_situation(Destination)) %>%
      fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>%
      fsummarize(count = GRPN()) %>%
      fungroup() %>%
      fmutate(pct = count / sum(count, na.rm = TRUE))
    
    totals <- sub %>%
      fgroup_by(`Destination Type`) %>%
      fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
                 count = sum(count), pct = sum(pct))
    
    rowbind(sub, totals) %>%
      frename(setNames(c("count", "pct"), paste0(c("count_", "pct_"), prefix)))
  }
  
  summarize_dest(subpop(), "subpop") %>%
    join(
      summarize_dest(everyone_else(), "comparison"),
      on = c('Destination Type', 'Destination Type Detail'),
      how = "full"
    ) %>%
    list_all_destinations(fill_zero = TRUE, add_totals = TRUE) %>%
    fmutate(
      pct_comparison = scales::percent(pct_comparison, accuracy = 0.1, scale = 100),
      pct_subpop = scales::percent(pct_subpop, accuracy = 0.1, scale = 100)
    ) %>%
    fselect(`Destination Type`, `Destination Type Detail`,
            'Subpopulation %' = pct_subpop, 'Subpopulation Count' = count_subpop,
            'Everyone Else %' = pct_comparison, 'Everyone Else Count' = count_comparison)
})

syse_subpop_data_download <- function(file) {
  logToConsole(session, "System Exits by Subpopulation data download")
  write_xlsx(
    list(
      "SystemExitsBySubpop Metadata" = subpop_metadata_summary() %>% frename("System Exits by Subpopulation" = Value),
      "SubpopulationComparisonSummary" = syse_subpop_export_summary(),
      "SubpopulationExitDetail" = syse_subpop_export_detail()
    ),
    path = file, format_headers = FALSE, col_names = TRUE
  )
  logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox, if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}

syse_subpop_ppt_download <- function(file) {
  dest_types <- c('Permanent', 'Homeless', 'Institutional', 'Temporary', 'Other/Unknown')
  sys_perf_ppt_export(
    file = file,
    type = 'exits_comparison',
    title_slide_title = "System Exits by Subpopulation",
    summary_items = list("Summary" = subpop_metadata_summary()),
    plots = setNames(lapply(dest_types, \(d) syse_compare_subpop_chart(dest_type = d, isExport = TRUE)),
                     paste0("System Exits by Subpopulation - ", dest_types)),
    summary_font_size = 19,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    sourceID = session$userData$Export$SourceID,
    in_demo_mode = input$in_demo_mode
  )
}