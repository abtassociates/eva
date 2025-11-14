# Factor levels for Inflow and Outflow types -----------------------------------
active_at_levels <- c(
  "Housed",
  "Homeless"
)

# All possible levels
inflow_detail_levels <- c(
  "First-Time Homeless", 
  "Returned from Permanent",
  "Re-engaged from Non-Permanent",
  "Continuous at Start",
  "Unknown",
  "First-of-Month Exit",
  "something's wrong"
)

outflow_detail_levels <- c(
  "Exited, Non-Permanent",
  "Exited, Permanent",
  "Continuous at End",
  "Last-of-Month Entry",
  "Inactive",
  "something's wrong"
)

# Levels for detail chart
inflow_chart_detail_levels <- c(
  "First-Time Homeless", 
  "Returned from Permanent",
  "Re-engaged from Non-Permanent"
)

outflow_chart_detail_levels <- c(
  "Exited, Non-Permanent",
  "Exited, Permanent",
  "Inactive"
)

inflow_outflow_levels <- c("Inflow","Outflow")

mbm_inflow_levels <- c("Active at Start: Homeless", "Inflow")
mbm_outflow_levels <- c("Outflow", "Active at End: Housed")

# Levels for summary chart
inflow_summary_chart_levels <- c(
  "Active at Start",
  "Inflow",
  "something's wrong"
)

outflow_summary_chart_levels <- c(
  "Outflow",
  "Active at End",
  "something's wrong"
)

inflow_statuses_to_exclude_from_chart <- c(
  "Continuous at Start",
  "Unknown",
  "First-of-Month Exit",
  "something's wrong"
)
inflow_statuses_to_exclude_from_export <- c(
  "First-of-Month Exit",
  "something's wrong"
)

outflow_statuses_to_exclude_from_chart <- c(
  "Continuous at End",
  "Last-of-Month Entry",
  "something's wrong"
)
outflow_statuses_to_exclude_from_export <- c(
  "something's wrong"
)

collapse_details <- list(
  "Outflow" = outflow_chart_detail_levels, 
  "Inflow" = inflow_chart_detail_levels
)


# 0.2 seems to be the right value to space the bars correctly
# higher than this and outflow bars start to overlap with next month's inflow
# lower and the bars are too thin, or space within a month is about the same as across
mbm_bar_width = 0.2
mbm_export_bar_width = 0.4

level_of_detail_text <- reactive({
  fcase(
    input$syso_level_of_detail == "All", "People",
    input$syso_level_of_detail == "HoHsOnly", "Heads of Household",
    default =
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
  )
})

full_unit_of_analysis_display <- reactive({
  paste0(
    "Total ", 
    level_of_detail_text(),
    if_else(
      input$syso_hh_type == "All",
      "",
      paste0(" in ",
             str_remove(getNameByValue(syso_hh_types, input$syso_hh_type), "- "),
             " Households")
    )
  )
})

# Inflow/Outflow Client-Level Data ---------------------------
## Summary (Annual) ----------------------------
# This also gets used by the Status chart
get_inflow_outflow_full <- reactive({
  logToConsole(session, "In get_inflow_outflow_full")

  full_data <- period_specific_data()[["Full"]]
  if(nrow(full_data) == 0) return(full_data)
  logToConsole(session, paste0("In get_inflow_outflow_full, num full_data records: ", nrow(full_data)))
  
  # AS 6/8/25: Do we want to remove *people* that are Continuous? Or just exclude from those Inflow/Outflow bars?
  # ditto for Inflow = Unknown
  # 637203 is an example of someone with Inflow = Unknown but has a regular Outflow
  data <- full_data %>%
    fselect(PersonalID,
            InflowTypeSummary,
            InflowTypeDetail,
            OutflowTypeSummary,
            OutflowTypeDetail
    ) %>%
    funique()
  
  data
})

## Monthly ---------------------------------
# combine individual month datasets
get_inflow_outflow_monthly <- reactive({
  logToConsole(session, paste0("In get_inflow_outflow_monthly"))
  months_data <- period_specific_data()[["Months"]]
  
  logToConsole(session, paste0("In get_inflow_outflow_monthly, num months_data records: ", nrow(months_data)))
  
  if(nrow(months_data) == 0) return(months_data)
  
  data.table::copy(months_data %>%
    fselect(
      PersonalID, 
      InflowTypeDetail, 
      OutflowTypeDetail, 
      InflowTypeSummary, 
      OutflowTypeSummary, 
      month
    ) %>% 
    funique()
  )
})

# Filter Selections UI -----------------------------------------------------------
# (Reported above the chart)
output$sys_inflow_outflow_detail_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  syso_detailBox() 
})
output$sys_inflow_outflow_summary_filter_selections <- renderUI({
  req(session$userData$valid_file() == 1)
  syso_detailBox() 
})
output$sys_inflow_outflow_monthly_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  syso_detailBox() 
})

# Chart Data Prep ---------------------------------------
## Summary/Detail (Annual) ---------------------------------------
# This can be used for both the Summary and Detail charts
# Final dataset looks like this:
#                             Detail                         Summary  InflowOutflow PlotFillGroups     N order
#                             <fctr>                          <fctr>         <fctr>         <fctr> <int> <int>
# 1:                          Housed                 Active at Start         Inflow         Housed    73     1
# 2:                        Homeless                 Active at Start         Inflow       Homeless   153     2
# 3:           First-Time Homeless           First-Time Homeless         Inflow         Inflow   747     3
# 4:       Returned from Permanent       Returned from Permanent         Inflow         Inflow     0     4
# 5: Re-engaged from Non-Permanent Re-engaged from Non-Permanent         Inflow         Inflow     0     5
# 6:                         Unknown                         Unknown         Inflow         Inflow     0     6
# 7:          Exited,\nNon-Permanent          Exited,\nNon-Permanent        Outflow        Outflow   281     7
# 8:              Exited,\nPermanent              Exited,\nPermanent        Outflow        Outflow   355     8
# 9:                        Inactive                        Inactive        Outflow        Outflow    50     9
# 10:                         Housed                   Active at End        Outflow         Housed    50    11
# 11:                       Homeless                   Active at End        Outflow       Homeless   237    10

sys_inflow_outflow_annual_chart_data <- reactive({
  logToConsole(session, "In sys_inflow_outflow_annual_chart_data")

  full_combinations <- data.frame(
    Detail = c(
      active_at_levels,
      inflow_chart_detail_levels,
      outflow_chart_detail_levels,
      active_at_levels
    ),
    Summary = c(
      rep("Active at Start", length(active_at_levels)),
      inflow_chart_detail_levels,
      outflow_chart_detail_levels,
      rep("Active at End", length(active_at_levels))
    ),
    InflowOutflow = c(
      rep("Inflow", length(c(inflow_chart_detail_levels, active_at_levels))),
      rep("Outflow", length(c(outflow_chart_detail_levels, active_at_levels)))
    ),
    PlotFillGroups = c(
      active_at_levels,
      rep("Inflow", length(inflow_chart_detail_levels)),
      rep("Outflow", length(outflow_chart_detail_levels)),
      active_at_levels
    )
  )

  inflow_outflow_full_data <- get_inflow_outflow_full() %>% 
    ftransform(
      InflowTypeDetail = factor(InflowTypeDetail, levels = c(active_at_levels, inflow_chart_detail_levels)),
      OutflowTypeDetail = factor(OutflowTypeDetail, levels = c(outflow_chart_detail_levels, active_at_levels))
    )

   rowbind(
    inflow_outflow_full_data %>%
      fcompute(
        Detail = InflowTypeDetail,
        Summary = fct_collapse(InflowTypeDetail, `Active at Start` = active_at_levels),
        InflowOutflow = factor("Inflow", levels = inflow_outflow_levels),
        PlotFillGroups = fct_collapse(InflowTypeDetail, Inflow = inflow_chart_detail_levels)
      ),

    inflow_outflow_full_data %>%
      fcompute(
        Detail = OutflowTypeDetail,
        Summary = fct_collapse(OutflowTypeDetail, `Active at End` = active_at_levels),
        InflowOutflow = factor("Outflow", levels = inflow_outflow_levels),
        PlotFillGroups = fct_collapse(OutflowTypeDetail, Outflow = outflow_chart_detail_levels)
      )
  ) %>% 
  fsubset(!Detail %in% c(inflow_statuses_to_exclude_from_chart, outflow_statuses_to_exclude_from_chart)) %>%
  fcount() %>%
  join(full_combinations, how="full", on=names(full_combinations), overid=0) %>%
  collapse::replace_na(cols = "N", value = 0) %>%
  fsubset(!(Detail == "something's wrong" & N == 0)) %>%
  roworder(Summary, Detail) %>%
  fmutate(
    # switching Homeless and Housed order for Outflow is important for lining up 
    # the bars, which are created by calculating ystart+yend based on the ordering
    order = case_when(
      Summary == "Active at End" & Detail == "Homeless" ~ fnrow(.) - 1,
      Summary == "Active at End" & Detail == "Housed" ~ fnrow(.),
      TRUE ~ seq_row(.)  # fallback for other statuses/times
    )
  ) %>%
  roworder(order)
})

## Monthly ---------------------------------------
### MbM ---------------------------
# Get records to be counted in MbM. Doing this step separately from the counts allows us to easily validate
sys_inflow_outflow_monthly_chart_data <- reactive({
  logToConsole(session, "In sys_inflow_outflow_monthly_chart_data")
  monthly_data <- get_inflow_outflow_monthly() 
  
  if(nrow(monthly_data) == 0) return(monthly_data)
  monthly_data <- monthly_data %>%
    fmutate(
      InflowPlotFillGroups = fct_collapse(
        InflowTypeDetail, 
        `Active at Start: Housed` = "Housed", 
        `Active at Start: Homeless` = "Homeless", 
        Inflow = inflow_detail_levels
      ),
      OutflowPlotFillGroups = fct_collapse(
        OutflowTypeDetail, 
        `Active at End: Homeless` = "Homeless", 
        `Active at End: Housed` = "Housed",
        Outflow = outflow_detail_levels
      )
    ) %>% 
    get_counts_by_month_for_mbm() %>%
    fsubset(
      !Detail %in% c(inflow_statuses_to_exclude_from_chart, outflow_statuses_to_exclude_from_chart)
    )
})

# Get counts of Inflow/Outflow statuses by month (long-format, 1 row per month-status)
get_counts_by_month_for_mbm <- function(monthly_data) {
  monthly_counts <- rbind(
    monthly_data[, .(
      PersonalID, 
      month, 
      PlotFillGroups = InflowPlotFillGroups, 
      Detail = InflowTypeDetail,
      Summary = InflowTypeSummary
    )],
    monthly_data[, .(
      PersonalID, 
      month, 
      PlotFillGroups = OutflowPlotFillGroups, 
      Detail = OutflowTypeDetail,
      Summary = OutflowTypeSummary
    )]
  ) %>%
    funique() %>%
    fgroup_by(month, Summary, PlotFillGroups, Detail) %>%
    fsummarise(Count = GRPN()) %>%
    roworder(month, Summary, PlotFillGroups, Detail)

  all_months <- data.table(month = get_months_in_report_period()) %>%
    fmutate(month = factor(format(month, "%b %y")))
  # PlotFillGroups %in% c(mbm_inflow_levels, mbm_outflow_levels) &
    
  full_combinations <- data.table(
    Detail = c(
      active_at_levels,
      inflow_chart_detail_levels,
      outflow_chart_detail_levels,
      active_at_levels
    ),
    Summary = c(
      rep("Active at Start", length(active_at_levels)),
      rep("Inflow", length(inflow_chart_detail_levels)),
      rep("Outflow", length(outflow_chart_detail_levels)),
      rep("Active at End", length(active_at_levels))
    ),
    PlotFillGroups = c(
      paste0("Active at Start: ", active_at_levels),
      rep("Inflow", length(inflow_chart_detail_levels)),
      rep("Outflow", length(outflow_chart_detail_levels)),
      paste0("Active at End: ", active_at_levels)
    )
  ) %>%
    fmutate(k = 1) %>%
    join(
      all_months[, k:= 1],
      on = "k", 
      multiple = TRUE
    ) %>%
    fselect(-k)

  # Make sure all month-type combinations are reflected
  join(
    monthly_counts,
    full_combinations,
    how = "full",
    overid = 0
  ) %>%
  collapse::replace_na(value = 0, cols = "Count")
}

### Monthly_chart_data, wide format
sys_monthly_chart_data_wide <- reactive({
  logToConsole(session, "In sys_monthly_chart_data_wide")
  monthly_counts_long <- sys_inflow_outflow_monthly_chart_data()
  req(nrow(monthly_counts_long) > 0)

  summary_data <- pivot(
    monthly_counts_long,
    ids = c("Summary", "PlotFillGroups", "Detail"),        # Column(s) defining the rows of the output
    names = "month",       # Column whose values become column names
    values = "Count",  # An arbitrary column to count (used with fun)
    how = "wider",
    fill = 0             # Fill missing combinations with 0
  )
  
  # Get Monthly Change (Inflow - Outflow)
  month_cols <- names(summary_data)[-1:-3]
  inflow_vals <- summary_data[PlotFillGroups == "Inflow", ..month_cols]
  outflow_vals <- summary_data[PlotFillGroups == "Outflow", ..month_cols]
  change_row <- fsum(inflow_vals) - fsum(outflow_vals)

  rbind(
    summary_data,
    add_vars(as.list(change_row), 
             PlotFillGroups = "Monthly Change", 
             Detail = "Monthly Change",
             Summary = "Monthly Change")
  ) %>%
    roworder(PlotFillGroups)
})

### Inactive + FTH ------------------------
sys_inflow_outflow_monthly_single_status_chart_data <- function(monthly_status_data) {
  logToConsole(session, "In sys_inflow_outflow_monthly_single_status_chart_data")
  
  monthly_status_data %>%
    fgroup_by(month) %>%
    fsummarise(Count = GRPN()) %>%
    roworder(month) %>%
    join(
      data.table(month = unique(monthly_status_data$month)),
      on = "month",
      how = "right"
    ) %>%
    replace_na(value = 0, cols = "Count")
}

# Summary/Detail (Annual) Chart Prep ---------------------------------------
# Function called in the renderPlot and exports
get_sys_inflow_outflow_annual_plot <- function(id, isExport = FALSE) {
  logToConsole(session, paste0("Getting sys inflow/outflow plot for ", id, ". For export? ", isExport))
  
  df <- sys_inflow_outflow_annual_chart_data()
  
  if (id == "sys_inflow_outflow_summary_ui_chart") {
    df <- df %>%
      # collapse the detailed levels of inflow/outflow
      fmutate(Summary = fct_collapse(Summary, !!!collapse_details)) %>%
      collap(cols="N", ~ InflowOutflow + Summary + PlotFillGroups, fsum, sort=FALSE)
    mid_plot <- 2.5
    fill_var <- 'PlotFillGroups'
    fill_breaks <- c("Housed","Homeless","Inflow","Outflow")
  } else {
    # re-label Inflow Unspecified if export is < 1094 days
    if(session$userData$days_of_data < 1094)
      df <- df %>%
        ftransform(
          Summary = fct_recode(Summary, "Inflow Unspecified" = "First-Time Homeless")
        )

    mid_plot <- 4.5
    fill_var <- 'Detail'
    # Use Housed, Homeless, one Inflow, and one Outflow for legend in Detail chart
    fill_breaks <- c("Housed","Homeless","First-Time Homeless","Inactive")
  }
  
  total_clients <- df[InflowOutflow == "Inflow", sum(N)]

  validate(
    need(
      total_clients > 10,
      message = suppression_msg
    )
  )

  # Order: Housed (start), Homeless (start), Inflow, Outflow, Homeless (end), Housed
  df <- df %>%
    fmutate(
      N = ifelse(InflowOutflow == "Outflow", N * -1, N),
      ystart = lag(cumsum(N), default = 0),
      yend = round(cumsum(N)),
      group.id = GRPid(Summary),
      N_formatted = scales::comma(abs(N))
    ) %>%
    # Remove Active at Start/End bars that are 0, since there's no label other 
    # than legend, which makes it hard to interpret the floating 0
    fsubset(!(N == 0 & PlotFillGroups %in% active_at_levels))
  
  # s <- max(df$yend) + 20
  # num_segments <- 20
  # segment_size <- get_segment_size(s/num_segments)
  total_change <- as.integer(sys_inflow_outflow_totals()[Chart == "Total Change", Value])

  uniq_vals <- unique(df[[fill_var]])
  cat_order <- as.character(uniq_vals[order(uniq_vals)])
  
  bar_patterns <- unname(mbm_pattern_fills[cat_order])
  bar_bg <- unname(bar_colors[cat_order])
  
  bar_fg <- bar_bg
  bar_fg[!(cat_order %in% c('Housed','Homeless'))] <- 'black'
  
  # https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
  ggplot(df, aes(x = group.id, fill = .data[[fill_var]])) +
    geom_rect(
      aes(
        # control bar gap width
        xmin = group.id - 0.25,
        xmax = group.id + 0.25,
        ymin = ystart,
        ymax = yend,
      ),
      colour = "black",
      linewidth = .5,
      alpha = 0.8
    ) +
    # the connecting segments between bars
    geom_segment(
      data = df %>%
        flast(g = .$group.id) %>%
        fselect(yend, group.id),
      aes(
        x = group.id,
        xend = if_else(group.id == last(group.id), last(group.id), group.id + 1),
        y = yend
      ),
      linewidth = .3,
      color = "gray25",
      linetype = "dashed",
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    # numeric labels for Active at Start/End
    ggrepel::geom_text_repel(
      aes(
        x = group.id,
        label = if_else(grepl("Active at", Summary), N_formatted, NA),
        y = rowSums(cbind(ystart, N / 2))
      ),
      hjust = 1,
      # direction = "y",
      segment.colour = NA,
      nudge_x = ifelse(windowSize()[1] < 1300, -.4, -.3),
      color = get_brand_color('med_grey2'),
      size = sys_chart_text_font,
      inherit.aes = FALSE
    ) +
    # numeric labels for Inflow/Outflow
    geom_text(
      aes(
        x = group.id,
        label = if_else(!grepl("Active at", Summary), N_formatted, NA),
        y = if_else(PlotFillGroups == "Inflow", yend, ystart), 
        vjust = -.6
      ),
      size = sys_chart_text_font,
      color = "black"
    ) +
    
    ggtitle(
      paste0(
        sys_total_count_display(total_clients),
        "Total Change: ",
        if(total_change > 0) "+" else "", scales::comma(total_change),
        "\n",
        "\n"
      )
    ) +
    
    # pattern fills
    scale_fill_pattern_eva(bg = bar_bg, fg = bar_fg, patterns = bar_patterns, min_size = unit(1, 'mm'), lwd = 2,
                        breaks = fill_breaks, labels = c("Housed","Homeless","Inflow","Outflow")) +
   
    # distance between bars and x axis line
    scale_y_continuous(expand = expansion()) +
    # x axis labels
    scale_x_continuous(
      labels = str_wrap(df$Summary %>% unique(), width = 10),
      breaks = df$group.id %>% unique()
    ) +
    coord_cartesian(clip = "off") +
    # totally clear all theme elements
    theme_void() +
    # add back in what theme elements we want
    theme(
      text = element_text(size = sys_chart_text_font, colour = get_brand_color('med_grey2')),
      axis.text.x = element_text(
        size = get_adj_font_size(
          sys_axis_text_font * ifelse(windowSize()[1] < 1300, 0.9,1), 
          isExport),
        vjust = -.2), 
      axis.ticks.x = element_line(),
      axis.line.x = element_line(color = get_brand_color('med_grey2'), linewidth = 0.5),
      plot.margin = unit(c(3, 1, 1, 1), "lines"),
      legend.text = element_text(size = get_adj_font_size(sys_legend_text_font, isExport)),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.margin = margin(.5, 0, 0, 0, unit = "inch"),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
}

# custom round to the smaller of the nearest 10, 100, etc.
# good for chart segment sizing
# get_segment_size <- function(x) {
#   thresholds <- c(1, 10, 100, 200, 500, 1000, 1500, 2000, 2500, 5000, 10000)
#   rounded <- sapply(thresholds, function(t) {
#     if (x > t) {
#       return(t * ceiling(x / t))
#     } else {
#       return(NA)
#     }
#   })
#   min(rounded, na.rm = TRUE)
# }

# Render Charts ----------------------------------------
renderInflowOutflowFullPlot <- function(chart_id, alt_text) {
  output[[chart_id]] <- renderPlot({
      req(session$userData$valid_file() == 1)
    
      validate(
        need(
          nrow(get_inflow_outflow_full()) > 0,
          message = no_data_msg
        )
      )
      
      get_sys_inflow_outflow_annual_plot(chart_id)
    },
    alt = alt_text,
    width = ifelse(isTRUE(getOption("shiny.testmode")), 1113, "auto")
  )
}
## Summary (Annual) --------------------------------
renderInflowOutflowFullPlot(
  chart_id = "sys_inflow_outflow_summary_ui_chart",
  alt_text = "A waterfall bar chart of the homeless system's inflow and outflow during 
      the report period. The summary view of this chart includes four components: 
      Active at Start, Inflow, Outflow, and Active at End."
)
## Detail (Annual) -----------------------------------------
renderInflowOutflowFullPlot(
  chart_id = "sys_inflow_outflow_detail_ui_chart",
  alt_text = "A waterfall bar chart of the homeless system's inflow and 
      outflow during the report period. The detailed view of this chart 
      shows inflow as three subcategories: first-time homeless, returned from 
      permanent, and re-engaged from non-permanent and outflow as three 
      subcategories: exited non-permanent, exited permanent, and inactive."
)

## Monthly --------------------------------------------
### MbM Chart --------------------------------------
# Bar - Active at Start + Inflow/Outflow
get_sys_inflow_outflow_monthly_plot <- function(isExport = FALSE) {
  reactive({
    logToConsole(session, "In get_sys_inflow_outflow_monthly_plot")

    plot_data <- sys_inflow_outflow_monthly_chart_data() %>%
      fsubset(PlotFillGroups %in% c(mbm_inflow_levels, mbm_outflow_levels)) %>%
      collap(cols = "Count", FUN=fsum, by = ~ month + PlotFillGroups + Summary) %>%
      fmutate(InflowOutflow = fct_collapse(
        Summary,
        Inflow = inflow_summary_chart_levels,
        Outflow = outflow_summary_chart_levels
      ))
    
    # Get Average Info for Title Display
    averages <- fmean(plot_data$Count, g=plot_data$PlotFillGroups)
    
    # Inflow and Outflow here include Inflow+Active at Start and Outflow+Active at End
    totals_start <- fsum(plot_data[PlotFillGroups == "Inflow", Count])
    totals_end <- fsum(plot_data[PlotFillGroups == "Outflow", Count])
    avg_monthly_change <- (totals_start - totals_end)/(length(session$userData$report_dates) - 1)
    
    plot_data$month_numeric <- as.numeric(as.factor(plot_data$month))
    
    # width = 0.5 means the bar is half the distance between adjacent ticks
    # so width = 0.5 means the bars from adjacent months will be touching
    # 0.4 is probably the highest we can go to maintain the distinction between months
    bar_width <- if_else(isExport, mbm_export_bar_width, mbm_bar_width)
    
    cat_order <- c(rev(mbm_inflow_levels), mbm_outflow_levels)
  
    bar_patterns <- unname(mbm_pattern_fills[cat_order])
    bar_bg <- unname(mbm_bar_colors[cat_order])
    
    bar_fg <- unname(c('Inflow' = 'black', 'Outflow' = 'black',  
                       mbm_bar_colors['Active at Start: Homeless'], 
                       mbm_bar_colors['Active at End: Housed'])[cat_order])
    
    # just = 0.5 means bar is centered around tick
    # just = 1 means bar's right side is aligned with tick
    # just = -1 means bar's left side is aligned with tick
    # this parameter obviously "eats into" into the distance between ticks
    bar_adjust <- if_else(isExport, 1, 1.2)
    
    g <- ggplot(plot_data, aes(x = interaction(month, InflowOutflow), y = Count, fill = PlotFillGroups)) +
      geom_bar(
        data = plot_data[InflowOutflow == "Inflow"] %>%
          fmutate(PlotFillGroups = fct_relevel(
            PlotFillGroups,
            rev(mbm_inflow_levels)
          )),
        aes(x = month, y = Count,  fill = PlotFillGroups), 
        stat = "identity",
        position = "stack",
        width = bar_width,
        color = 'black',
        just = bar_adjust
      ) +
      geom_bar(
        data = plot_data[InflowOutflow == "Outflow"] %>%
          fmutate(PlotFillGroups = fct_relevel(
            PlotFillGroups,
            mbm_outflow_levels
          )),
        aes(x = month, y = Count, fill = PlotFillGroups),
        stat = "identity",
        position = "stack",
        width = bar_width,
        color = 'black',
        just = 1 - bar_adjust
      ) +
      theme_minimal() +
      labs(
        x = "Month",
        y = paste0("Count of ", level_of_detail_text()),
        fill = ''
      ) +
      scale_x_discrete(expand = expansion(mult = c(0.045, 0.045))) + # make plto take up more space horizontally
      # pattern fills
      scale_fill_pattern_eva(bg = bar_bg, fg = bar_fg, patterns = bar_patterns, lwd = 1,
                          breaks = c('Active at Start: Homeless','Inflow','Outflow','Active at End: Housed')) +
      # Update legend title
      ggtitle(
        paste0(
          "Average Monthly Inflow: +", scales::comma(averages["Inflow"], accuracy = 0.1), "\n",
          "Average Monthly Outflow: -", scales::comma(averages["Outflow"], accuracy = 0.1), "\n",
          "Average Monthly Change in ", 
          level_of_detail_text(), " in ", 
          str_remove(getNameByValue(syso_hh_types, input$syso_hh_type), "- "), 
          if_else(getNameByValue(syso_hh_types, input$syso_hh_type) == "All Household Types", "", " Households"),
          ": ", 
          scales::comma(avg_monthly_change, accuracy = 0.1)
        )
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = if(!isExport) element_blank() else element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = sys_axis_text_font),   
        legend.position = if(!isExport) "none" else "bottom",
        panel.grid = element_blank(),       
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        plot.margin = margin(l = 55),        # Increase left margin
        axis.ticks = element_blank(),
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
      )
    
    # For PPT export, add data labels, centered horizontally and vertically within a bar
    if(isExport) {
      
      g <- g + 
        geom_label(
          data = plot_data[InflowOutflow == "Inflow"] %>%
            #data = plot_data[InflowOutflow == "Inflow"] %>%
            fmutate(PlotFillGroups = fct_relevel(
              PlotFillGroups,
              rev(mbm_inflow_levels)
            )) %>% 
            # hide labels if value is 0
            fmutate(Count = na_if(Count, 0)),
          aes(x = month_numeric - mbm_export_bar_width/2, y = Count, label = Count, group = PlotFillGroups),
          stat = "identity",
          color = "black",
          fill = "white",
          position = position_stack(vjust = 0.5),
          size = 10,
          size.unit = "pt"
        ) +
        geom_label(
          data = plot_data[InflowOutflow == "Outflow"] %>% 
            fmutate(PlotFillGroups = fct_relevel(
              PlotFillGroups,
              mbm_outflow_levels
            )) %>% 
            # hide labels if value is 0
            fmutate(Count = na_if(Count, 0)),
          aes(x = month_numeric + mbm_export_bar_width/2, y = Count, label = Count, group = PlotFillGroups),
          stat = "identity",
          color = "black",
          fill = "white",
          position = position_stack(vjust = 0.5),
          size = 10,
          size.unit = "pt"
        ) 
    }
    g
  })
}

output$sys_inflow_outflow_monthly_ui_chart <- renderPlot({
  monthly_chart_validation()
  get_sys_inflow_outflow_monthly_plot()()
})

# Pure line chart -------
# output$sys_inflow_outflow_monthly_ui_chart_line <- renderPlot({
#   plot_data <- sys_inflow_outflow_monthly_chart_data()
#   
#   # Get Average Info for Title Display
#   averages <- plot_data %>%
#     collap(cols = "Count", FUN=fmean, by = ~ Summary)
#   
#   avg_monthly_change <- fmean(
#     plot_data[plot_data$Summary == "Inflow", "Count"] - 
#       plot_data[plot_data$Summary == "Outflow", "Count"]
#   )
#   
#   level_of_detail_text <- case_when(
#     input$syso_level_of_detail == "All" ~ "People",
#     input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
#     TRUE ~
#       getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
#   )
#   
#   ggplot(plot_data, aes(x = month, y = Count, color = PlotFillGroups, group = PlotFillGroups)) +
#     # Use geom_line to draw the lines
#     geom_line(linewidth = 1.2) + # `linewidth` is preferred over `size` for lines >= ggplot2 3.4.0
#     # Optional: Add points to mark the actual data points each month
#     geom_point(size = 3) +
#     
#     scale_color_manual(values = bar_colors, name = "Group") + # Adjust legend title if needed
#     
#     theme_minimal() +
#     labs(
#       x = "Month",
#       # Update Y-axis label to reflect what's plotted
#       y = paste0("Count of ", level_of_detail_text, " (Active at Start)")
#     ) + 
#     # Adjust title to reflect the line chart's focus, but keep avg inflow/outflow context
#     ggtitle(
#       paste0(
#         "Average Monthly Inflow: +", scales::comma(averages[Summary == "Inflow", Count], accuracy = 0.1), "\n",
#         "Average Monthly Outflow: -", scales::comma(averages[Summary == "Outflow", Count], accuracy = 0.1), "\n",
#         "Average Monthly Change in ", 
#           level_of_detail_text, " in ", getNameByValue(syso_hh_types, input$syso_hh_type), ": ", 
#           scales::comma(avg_monthly_change, accuracy = 0.1)
#       )
#     ) +
#     scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 8)) + 
#     theme(
#       axis.text.x = element_blank(),
#       axis.title.x = element_blank(), 
#       axis.title.y = element_text(size = sys_axis_text_font),   
#       legend.position = "none",
#       panel.grid = element_blank(), 
#       axis.text.y = element_text(size = 10, color = "black"), # Example: adjust size and color
#       axis.ticks.y = element_line(color = "black"),
#       axis.line.y = element_line(),          # Remove axis lines
#       axis.line.x = element_line(),          # Remove axis lines
#       plot.margin = margin(l = 55),        # Increase left margin
#       axis.ticks.x = element_blank(),
#       plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
#     )
# })

# Combined line + bar chart -------------
# output$sys_inflow_outflow_monthly_ui_chart_combined <- renderPlot({
#   plot_data <- sys_inflow_outflow_monthly_chart_data()
#   
#   # Get Average Info for Title Display
#   averages <- plot_data %>%
#     collap(cols = "Count", FUN=fmean, by = ~ Summary) # Assuming Summary is equivalent to PlotFillGroups for averages
#   
#   avg_monthly_change <- fmean(
#     plot_data[plot_data$PlotFillGroups == "Inflow", "Count"] - # Use PlotFillGroups here too
#       plot_data[plot_data$PlotFillGroups == "Outflow", "Count"] # Use PlotFillGroups here too
#   )
#   
#   level_of_detail_text <- case_when(
#     input$syso_level_of_detail == "All" ~ "People",
#     input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
#     TRUE ~
#       getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
#   )
#   
#   # --- Define categories for easier filtering ---
#   bar_categories <- c("Active at Start: Housed", "Active at Start: Homeless")
#   line_categories <- c("Inflow", "Outflow")
#   
#   # --- Build the ggplot ---
#   ggplot(mapping = aes(x = month)) +  # Base plot with only x aesthetic
#     
#     # --- Stacked Bars Layer ---
#     geom_col(
#       data = plot_data[Summary == "Active at Start"],
#       aes(y = Count, fill = PlotFillGroups),
#       position = "stack",
#       width = 0.3
#     ) +
#     
#     # --- Lines Layer ---
#     geom_line(
#       data = plot_data %>% filter(PlotFillGroups %in% line_categories),
#       aes(y = Count, color = PlotFillGroups, group = PlotFillGroups), # Group is important for lines
#       linewidth = 1.4
#     ) +
#     
#     # --- Points Layer (for lines) ---
#     geom_point(
#       data = plot_data %>% filter(PlotFillGroups %in% line_categories),
#       aes(y = Count, fill = PlotFillGroups), # No group needed if color is mapped
#       shape = 21,                            # <--- ADDED: Use shape 21 for border/fill control
#       color = "black",                       # <--- ADDED: Set border color to black (outside aes)
#       size = 3
#     ) +
#     
#     # --- Scales ---
#     # Use scale_fill_manual for the bars
#     scale_fill_manual(values = bar_colors) +
#     
#     # Use scale_color_manual for the lines/points
#     scale_color_manual(values = bar_colors, name = "Group") +
#     
#     # --- Theme and Labels ---
#     theme_minimal() +
#     labs(
#       x = "Month",
#       # Update Y-axis label to be more general
#       y = paste0("Count of ", level_of_detail_text)
#     ) +
#     ggtitle(
#       paste0(
#         "Average Monthly Inflow: +", scales::comma(averages[Summary == "Inflow", Count], accuracy = 0.1), "\n",
#         "Average Monthly Outflow: -", scales::comma(averages[Summary == "Outflow", Count], accuracy = 0.1), "\n",
#         "Average Monthly Change in ",
#         level_of_detail_text, " in ", getNameByValue(syso_hh_types, input$syso_hh_type), ": ",
#         scales::comma(avg_monthly_change, accuracy = 0.1)
#       )
#     ) +
#     scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 8)) +
#     theme(
#       axis.text.x = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = sys_axis_text_font),
#       legend.position = "none",
#       panel.grid = element_blank(),
#       axis.text.y = element_text(size = 10, color = "black"),
#       axis.ticks.y = element_line(color = "black"),
#       axis.line.y = element_line(),
#       axis.line.x = element_line(),
#       plot.margin = margin(l = 55),
#       axis.ticks.x = element_blank(),
#       plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
#     )
#   
# })


### Table --------------------------------------
monthly_chart_data_wide_for_tables <- function() {
  sys_monthly_chart_data_wide() %>%
    fsubset(
      PlotFillGroups %in% c(mbm_inflow_levels, mbm_outflow_levels, "Monthly Change") &
        !Detail %in% c(inflow_statuses_to_exclude_from_chart, outflow_statuses_to_exclude_from_chart)
    ) %>% 
    fmutate(PlotFillGroups = fct_relevel(PlotFillGroups, mbm_inflow_levels, mbm_outflow_levels)) %>% 
    roworder(PlotFillGroups)
}
# The table is positioned directly under the chart
# Making the month labels looks like both the chart's x-axis and the table's column headers
get_sys_inflow_outflow_monthly_table <- reactive({
  logToConsole(session, "In sys_inflow_outflow_monthly_table")

  summary_data_wide <- monthly_chart_data_wide_for_tables()
  
  req(nrow(summary_data_wide) > 0)
  
  if(input$mbm_status_filter == "First-Time Homeless")
    summary_data_with_change <- summary_data_wide %>%
      fsubset(PlotFillGroups == "Inflow" & Detail == "First-Time Homeless") %>%
      ftransform(PlotFillGroups = input$mbm_status_filter) %>%
      fselect(-Detail, -Summary)
  else if(input$mbm_status_filter == "Inactive")
    summary_data_with_change <- summary_data_wide %>%
      fsubset(PlotFillGroups == "Outflow" & Detail == "Inactive") %>%
      ftransform(PlotFillGroups = input$mbm_status_filter) %>%
      fselect(-Detail, -Summary)
  else {
    summary_data_wide <- summary_data_wide %>% fselect(-Detail, -Summary)
    summary_data_with_change <- collap(
      summary_data_wide, 
      cols=names(summary_data_wide %>% fselect(-PlotFillGroups)),
      FUN="fsum", 
      by = ~ PlotFillGroups
    )
  }
  
  req(nrow(summary_data_with_change) > 0)
  
  monthly_dt <- datatable(
    summary_data_with_change %>% 
      ## add div around outflow and inflow text to target with CSS
      fmutate(
        PlotFillGroups = as.character(PlotFillGroups),
        PlotFillGroups = fcase(PlotFillGroups == 'Inflow','<div>Inflow</div>',
                               PlotFillGroups == 'Outflow','<div>Outflow</div>',
                               default = PlotFillGroups)
      ) %>% 
      frename("PlotFillGroups" = " ") ,
    options = list(
      dom = 't',
      ordering = FALSE,
      # pageLength = 4,
      columnDefs = list(
        list(width = "48px", targets = 0), # Set first column width
        list(className = 'dt-center', targets = '_all') # Center text
      )
    ),
    escape = FALSE,
    style = "default",
    rownames = FALSE,
    selection = "none"
  )  %>%
    # Highlight only the first column of "Inflow" and "Outflow" rows
    formatStyle(
      columns = 1,  # First column
      target = "cell",
      backgroundColor = styleEqual(
        c(mbm_inflow_levels, mbm_outflow_levels),
        unname(mbm_bar_colors)
      ),
      border = styleEqual(
        c(mbm_inflow_levels, mbm_outflow_levels),
        c(rep("2px solid black", 4))
      )
    ) %>%
    # Contrast font and background colors
    formatStyle(
      columns = 1,
      target = "cell",
      color = styleEqual(
        c("Active at Start: Homeless", "Active at End: Housed"), 
        rep("white", 2)
      )
    )
  
  ## Commenting out this section which highlights max and min inflow/outfow values
  ## since we are now using the same background color with a pattern fill instead
  ## and users cannot distinguish between them in table form
  
  # if(input$mbm_status_filter == "All") {
  #   # Highlight max inflow
  #   month_cols <- names(summary_data_with_change)[-1]
  #   change_row <- as.integer(summary_data_with_change[PlotFillGroups == "Monthly Change", ..month_cols])
  # 
  #   if(any(change_row > 0, na.rm=TRUE)) {
  #     monthly_dt <- monthly_dt %>%
  #       formatStyle(
  #         columns = month_cols[which.max(change_row)],
  #         target = "cell",
  #         backgroundColor = styleRow(
  #           nrow(summary_data_with_change), 
  #           mbm_bar_colors["Inflow"]
  #         )
  #       )
  #   }
  #   
  #   if(any(change_row < 0, na.rm=TRUE)) {
  #     monthly_dt <- monthly_dt %>%
  #       formatStyle(
  #         columns = month_cols[which.min(change_row)],
  #         target = "cell",
  #         color = styleRow(nrow(summary_data_with_change), 'white'),
  #         backgroundColor = styleRow(nrow(summary_data_with_change), mbm_bar_colors["Outflow"])
  #       )
  #   }
  # }
  monthly_dt
})

get_sys_inflow_outflow_monthly_flextable <- function() {
  logToConsole(session, "In get_sys_inflow_outflow_monthly_flextable")
  d <- monthly_chart_data_wide_for_tables() %>% 
    fselect(-Detail, -Summary)
    
  d <- collap(
    d, 
    cols=names(d %>% fselect(-PlotFillGroups)),
    FUN="fsum", 
    by = ~ PlotFillGroups
  ) %>%
    frename("PlotFillGroups" = " ")
  
  ft <- flextable(d) %>%
    width(j = 1, width = 0.9) %>% # make first col narrower
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    border(border.top = fp_border(), part = "header") %>%
    border_inner_h(border = fp_border(color = "grey", width = 0.5), part = "body")
    
  row_labels <- d[[1]]
  
  # Formatting the inflow/outflow row labels - now using black text for Housed + Homeless, white for others
  inflow_outflow_row_indices <- which(row_labels %in% c("Inflow","Outflow")) 
  active_row_indices <- which(row_labels %in% c("Active at Start: Homeless", "Active at End: Housed")) 
  
  ft <- ft %>%
    # Background colors from datatable's formatStyle
    bg(i = inflow_outflow_row_indices, j = 1, bg = mbm_bar_colors[c('Inflow','Outflow')]) %>%
    bg(i = active_row_indices, j = 1, bg = mbm_bar_colors[c("Active at Start: Homeless", "Active at End: Housed")]) %>%
    # thick borders for the first column
    border(i = c(inflow_outflow_row_indices, active_row_indices), j = 1, border = fp_border(color = "black", width = 2)) %>%
    # Make outflow cells have contrasting white font color
    color(i = active_row_indices, j = 1, color = "white")

  # Highlight the monthly change inflow and outflow vals
  monthly_change_row <- which(row_labels == "Monthly Change")
  monthly_change_vals <- d[monthly_change_row, names(d)[-1]]

  ft %>%
    bg(i = monthly_change_row, j = which.max(monthly_change_vals) + 1, mbm_bar_colors["Inflow"]) %>%
    bg(i = monthly_change_row, j = which.min(monthly_change_vals) + 1, mbm_bar_colors["Outflow"]) %>%
    color(i = monthly_change_row, j = which.min(monthly_change_vals) + 1, color = "white")
}

output$sys_inflow_outflow_monthly_table <- renderDT({
  monthly_chart_validation()
  get_sys_inflow_outflow_monthly_table()
})

### Inactive + FTH chart --------------------------------------
sys_monthly_single_status_ui_chart <- function(varname, status) {
  logToConsole(session, "In sys_monthly_single_status_ui_chart")

  monthly_status_data <- get_inflow_outflow_monthly() %>%
    fsubset(.[[varname]] == status)
  
  if(nrow(monthly_status_data) == 0) 
    return(
      ggplot() + 
        labs(title = no_data_msg) + 
        theme_minimal()
    )
  
  if(fndistinct(monthly_status_data$PersonalID) <= 10) 
    return(
      ggplot() + 
        labs(title = suppression_msg) + 
        theme_minimal()
    )
  
  plot_data <- sys_inflow_outflow_monthly_single_status_chart_data(monthly_status_data)
  plot_data$PlotFillGroups <- status
 
  cat_order <- as.character(unique(plot_data$PlotFillGroups))
  
  ggplot(plot_data, aes(x = month, y = Count)) +
    geom_col(aes(fill = PlotFillGroups), width = 0.3, color = "black") +
    geom_text(aes(label = Count), vjust = -0.5, size = sys_chart_text_font) +
    theme_minimal() +
    labs(
      x = "Month",
      y = paste0("Count of ", level_of_detail_text())
    ) +
    ## pattern fills
    fillpattern::scale_fill_pattern(bg = mbm_single_status_chart_colors[status], fg='black', patterns = mbm_pattern_fills[[status]], lwd = 1) +
    scale_x_discrete(expand = expansion(mult = c(0.045, 0.045))) + # make plto take up more space horizontally
    theme(
      axis.text.x = element_text(size = sys_axis_text_font, face = "bold"),
      axis.text.y = element_blank(),
      axis.title.y = element_text(size = sys_axis_text_font), 
      axis.title.x = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.line.x = element_line(),          
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(l = 55),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
}
output$sys_inactive_monthly_ui_chart <- renderPlot({
  monthly_chart_validation()
  sys_monthly_single_status_ui_chart("OutflowTypeDetail", "Inactive")
})

output$sys_fth_monthly_ui_chart <- renderPlot({
  monthly_chart_validation()
  sys_monthly_single_status_ui_chart("InflowTypeDetail", "First-Time Homeless")
})

monthly_chart_validation <- function() {
  logToConsole(session, "In monthly_chart_validation")
  num_people <- length(unique(get_inflow_outflow_monthly()$PersonalID))
  
  validate(
    need(
      num_people > 0,
      message = no_data_msg
    )
  )
  
  validate(
    need(
      num_people > 10,
      message = suppression_msg
    )
  )
}

# Info to include in Inflow/Outflow Exports -----------------------------------
sys_inflow_outflow_totals <- reactive({
  logToConsole(session, "In sys_inflow_outflow_totals")
  
  df <- sys_inflow_outflow_annual_chart_data()
  data.table(
    Chart = c(
      paste0(
        full_unit_of_analysis_display(),
        " Served (Start + Inflow)"
      ),
      "Total Inflow",
      "Total Outflow",
      "Total Change"
    ),
    Value = as.character(c(
      sum(df[InflowOutflow == 'Inflow']$N, na.rm = TRUE),
      sum(df[Summary %in% inflow_detail_levels]$N, na.rm = TRUE),
      sum(df[Summary %in% outflow_detail_levels]$N, na.rm = TRUE),   
      sum(df[PlotFillGroups == "Inflow"]$N, na.rm = TRUE) -
        sum(df[PlotFillGroups == "Outflow"]$N, na.rm = TRUE)
    ))
  )
})

# Tabular/Excel Export --------------------------------------------------------
## Monthly Export function------
sys_export_monthly_info <- function() {
  logToConsole(session, "In sys_export_monthly_info")
  monthly_counts_wide <- sys_monthly_chart_data_wide()
  
  month_cols <- names(monthly_counts_wide)[-1:-3]

  monthly_counts_detail = monthly_counts_wide %>%
    fselect(-PlotFillGroups)
    
  monthly_totals <- monthly_counts_wide %>%
    fsubset(PlotFillGroups %in% inflow_outflow_levels) %>%
    collap(cols=month_cols, FUN="fsum", by = ~ PlotFillGroups) %>%
    frename(PlotFillGroups = "Detail") %>%
    fmutate(
      Summary = Detail, 
      Detail = factor(paste0("Total ", Detail))
    )
  
  monthly_counts <- rowbind(monthly_counts_detail, monthly_totals) %>%
    roworder(Summary, Detail)
  
  monthly_average_cols <- c("Total Inflow", "Total Outflow", "Monthly Change")
  monthly_averages <- data.table(
    Chart = paste0("Average ", gsub("Total ", "", monthly_average_cols)),
    Value = as.character(
      scales::comma(
        rowMeans(
          monthly_counts[Detail %in% monthly_average_cols, ..month_cols]
        )*c(1,-1,1),
        accuracy = 0.1
      )
    )
  )
  return(
    list(
      monthly_counts = monthly_counts,
      monthly_averages = monthly_averages
    )
  )
}

## Sys Inflow/Outflow Download Handler ------
# downloads all Inflow/Outflow chart data, including MbMs
output$sys_inflow_outflow_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Flow Report - "),
  content = function(file) {
    logToConsole(session, "Inflow/Outflow data download")

    df <- sys_inflow_outflow_annual_chart_data() %>% 
      ftransform(
        Summary = fct_collapse(Summary, !!!collapse_details)
      )
    
    if(session$userData$days_of_data < 1094) {
      df <- df %>%
        ftransform(
          Detail = fct_recode(Detail, "Inflow Unspecified" = "First-Time Homeless")
        )
    }
    
    totals_df <- df %>% 
      fgroup_by(Summary) %>% 
      fsummarise(Detail = paste0("Total ", Summary[1]),
                 N = fsum(N, na.rm = TRUE))

    monthly_data <- sys_export_monthly_info()

    write_xlsx(
      list(
        "System Flow Metadata" = sys_export_summary_initial_df() %>%
          bind_rows(
            sys_export_filter_selections(),
            sys_inflow_outflow_totals(),
            monthly_data$monthly_averages
          ) %>%
          mutate(Value = replace_na(Value, 0)) %>%
          rename("System Flow" = Value),
        "System Flow Summary" = bind_rows(df, totals_df) %>%
          roworder(Summary) %>%
          fselect(
            "Summary Category" = Summary,
            "Detail Category" = Detail,
            "Count" = N
          ),
        "System Flow Data Monthly" = monthly_data$monthly_counts
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )

    logMetadata(session, paste0("Downloaded System Overview Tabular Data: ", input$syso_tabbox,
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
    exportTestValues(sys_inflow_outflow_report = df)
  }
)

# PowerPoint/Image Export -----------------------------------------------------
output$sys_inflow_outflow_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Flow_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    logToConsole(session, "In sys_inflow_outflow_download_btn_ppt")
    monthly_data <- sys_export_monthly_info()

    sys_overview_ppt_export(
      file = file,
      title_slide_title = "System Flow",
      summary_items = sys_export_summary_initial_df() %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(
          sys_export_filter_selections(),
          sys_inflow_outflow_totals(),
          monthly_data$monthly_averages
        ),
      plots = list(
        "System Inflow/Outflow Summary" = get_sys_inflow_outflow_annual_plot(
          "sys_inflow_outflow_summary_ui_chart",
          isExport = TRUE
        ),
        "System Inflow/Outflow Detail" = get_sys_inflow_outflow_annual_plot(
          "sys_inflow_outflow_detail_ui_chart",
          isExport = TRUE
        ),
        "System Inflow/Outflow Monthly – All" = get_sys_inflow_outflow_monthly_plot(isExport = TRUE)(),
        "System Inflow/Outflow Monthly – Table" = get_sys_inflow_outflow_monthly_flextable(),
        "System Inflow/Outflow Monthly – First-Time Homeless" = sys_monthly_single_status_ui_chart("InflowTypeDetail", "First-Time Homeless"),
        "System Inflow/Outflow Monthly – Inactive" = sys_monthly_single_status_ui_chart("OutflowTypeDetail", "Inactive")
      ),
      summary_font_size = 19
    )
  }
)
