# Factor levels for Inflow and Outflow types -----------------------------------
active_at_levels <- c(
  "Housed",
  "Homeless"
)

inflow_detail_levels <- c(
  "First-Time \nHomeless", 
  "Returned from \nPermanent",
  "Re-engaged from \nNon-Permanent",
  "Unknown"
  # "something's wrong"
)

outflow_detail_levels <- c(
  "Exited,\nNon-Permanent",
  "Exited,\nPermanent",
  "Inactive"
)

inflow_outflow_summary_levels <- c(
  "Active at Start",
  "Inflow",
  "Outflow",
  "Active at End",  
  "something's wrong"
)

collapse_details <- list(
  "Outflow" = outflow_detail_levels, 
  "Inflow" = inflow_detail_levels
)

bar_colors <- c(
  "Inflow" = "#BDB6D7", 
  "Outflow" = '#6A559B',
  # "Inactive" = "#E78AC3"
  "Homeless" = '#ECE7E3',
  "Housed" = '#9E958F'
)

inactive_levels <- c("Unknown", "Inactive")
inactive_levels_explicit <- c("Unknown-Inflow", "Inactive-Outflow")

inactive_bar_colors <- c(
  "Unknown-Inflow" = "#DAD6E9",   # lighter version of "#BDB6D7"
  "Inactive-Outflow" = "#9B87C0"   # lighter version of "#6A559B"
)

# 0.2 seems to be the right value to space the bars correctly
# higher than this and outflow bars start to overlap with next month's inflow
# lower and the bars are too thin, or space within a month is about the same as across
active_at_start_bar_width = 0.2


# Inflow/Outflow Client-Level Data ---------------------------
## Summary (Annual) ----------------------------
# This also gets used by the Status chart
get_inflow_outflow_full <- reactive({
  period_specific_data()[["Full"]] %>%
    fselect(PersonalID,
            InflowTypeSummary,
            InflowTypeDetail,
            OutflowTypeSummary,
            OutflowTypeDetail
    ) %>%
    funique()
})

## Monthly ---------------------------------
get_inflow_outflow_monthly <- reactive({
  rbindlist(period_specific_data()[-1]) %>%
    fselect(
      PersonalID, 
      InflowTypeDetail, 
      OutflowTypeDetail, 
      InflowTypeSummary, 
      OutflowTypeSummary, 
      month
    ) %>% 
    funique() %>%
    # as of 4/9/25 - team decided to only show inflow/outflow (including inactive), 
    # and Active at Start (stacked) bars
    fsubset(
      InflowTypeSummary %in% c("Inflow", "Active at Start") |
        OutflowTypeSummary == "Outflow"
    ) %>%
    fmutate(
      # factorize month for easier processing
      month = factor(
        format(month, "%b %y"), 
        levels = format(get_months_in_report_period(), "%b %y")
      )
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
# 3:           First-Time \nHomeless           First-Time \nHomeless         Inflow         Inflow   747     3
# 4:       Returned from \nPermanent       Returned from \nPermanent         Inflow         Inflow     0     4
# 5: Re-engaged from \nNon-Permanent Re-engaged from \nNon-Permanent         Inflow         Inflow     0     5
# 6:                         Unknown                         Unknown         Inflow         Inflow     0     6
# 7:          Exited,\nNon-Permanent          Exited,\nNon-Permanent        Outflow        Outflow   281     7
# 8:              Exited,\nPermanent              Exited,\nPermanent        Outflow        Outflow   355     8
# 9:                        Inactive                        Inactive        Outflow        Outflow    50     9
# 10:                         Housed                   Active at End        Outflow         Housed    50    11
# 11:                       Homeless                   Active at End        Outflow       Homeless   237    10

sys_inflow_outflow_annual_chart_data <- reactive({
  full_combinations <- data.frame(
    Detail = c(
      active_at_levels,
      inflow_detail_levels,
      outflow_detail_levels,
      active_at_levels
    ),
    Summary = c(
      rep("Active at Start", length(active_at_levels)),
      inflow_detail_levels,
      outflow_detail_levels,
      rep("Active at End", length(active_at_levels))
    ),
    InflowOutflow = c(
      rep("Inflow", length(c(inflow_detail_levels, active_at_levels))),
      rep("Outflow", length(c(outflow_detail_levels, active_at_levels)))
    ),
    PlotFillGroups = c(
      active_at_levels,
      rep("Inflow", length(inflow_detail_levels)),
      rep("Outflow", length(outflow_detail_levels)),
      active_at_levels
    )
  )

  rbind(
    get_inflow_outflow_full()[, .(
      Detail = InflowTypeDetail, 
      Summary = fct_collapse(InflowTypeDetail, `Active at Start` = active_at_levels),
      InflowOutflow = factor("Inflow", levels = c("Inflow","Outflow")),
      PlotFillGroups = fct_collapse(InflowTypeDetail, Inflow = inflow_detail_levels)
    )],
    get_inflow_outflow_full()[, .(
      Detail = OutflowTypeDetail,
      Summary = fct_collapse(OutflowTypeDetail, `Active at End` = active_at_levels),
      InflowOutflow = factor("Outflow", levels = c("Inflow","Outflow")),
      PlotFillGroups = fct_collapse(OutflowTypeDetail, Outflow = outflow_detail_levels)
    )]
  ) %>% 
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
# Get counts of Inflow/Outflow statuses by month (long-format, 1 row per month-status)
sys_inflow_outflow_monthly_chart_data <- reactive({
  monthly_data <- get_inflow_outflow_monthly() %>%
    fmutate(
      # We want Housed, Homeless (start/end) and Inflow/Outflow
      InflowPlotFillGroups = fct_collapse(InflowTypeDetail, `Inflow` = inflow_detail_levels),
      OutflowPlotFillGroups = fct_collapse(OutflowTypeDetail, `Outflow` = outflow_detail_levels)
    )
  
  # First-time homeless filter
  if(input$mbm_fth_filter == "First-Time Homeless") {
    monthly_data <- monthly_data %>%
      fgroup_by(PersonalID) %>%
      fmutate(FirstTimeHomeless = anyv(InflowTypeDetail, "First-Time \nHomeless")) %>%
      fungroup() %>%
      fsubset(FirstTimeHomeless == TRUE)
  } 
  
  # Get counts of each type by month
  monthly_counts <- rbind(
    monthly_data[, .(PersonalID, month, PlotFillGroups = InflowPlotFillGroups)],
    monthly_data[, .(PersonalID, month, PlotFillGroups = OutflowPlotFillGroups)]
  ) %>%
    funique() %>%
    fgroup_by(month, PlotFillGroups) %>%
    fsummarise(Count = GRPN()) %>%
    roworder(month, PlotFillGroups)

  # Make sure all month-type combinations are reflected
  join(
    monthly_counts,
    CJ(
      month = levels(monthly_counts$month), 
      PlotFillGroups = unique(monthly_counts$PlotFillGroups), 
      sorted = FALSE
    ),
    on = c("month","PlotFillGroups"),
    how = "full"
  ) %>%
  fmutate(
    Summary = fct_collapse(PlotFillGroups, `Active at Start` = active_at_levels),
    PlotFillGroups = factor(PlotFillGroups, levels = c(rev(active_at_levels), "Inflow", "Outflow"))  
  ) %>%
  collapse::replace_na(value = 0, cols = "Count")
})

### Inactive ------------------------
get_inactive_counts <- function() {
  monthly_data <- get_inflow_outflow_monthly() %>%
    fsubset(OutflowTypeDetail == "Inactive" | InflowTypeDetail == "Unknown")

  monthly_counts <- rbind(
    monthly_data[, .(PersonalID, month, Type = InflowTypeDetail, source="Inflow")],
    monthly_data[, .(PersonalID, month, Type = OutflowTypeDetail, source="Outflow")]
  ) %>%
    funique() %>%
    fsubset(Type %in% inactive_levels) %>%
    fmutate(
      Type = factor(
        paste0(Type, "-", source),
        levels = inactive_levels_explicit
      )
    ) %>%
    fgroup_by(month, Type) %>%
    fsummarise(Count = GRPN()) %>%
    roworder(month, Type)

  join(
    monthly_counts,
    CJ(
      month = levels(monthly_counts$month), 
      Type = levels(monthly_counts$Type),
      sorted = FALSE
    ),
    how = "full",
    on = c("month","Type")
  ) %>%
  collapse::replace_na(value = 0, cols = "Count")
}

# Summary/Detail (Annual) Chart Prep ---------------------------------------
# Function called in the renderPlot and exports
get_system_inflow_outflow_annual_plot <- function(id, isExport = FALSE) {
  logToConsole(session, paste0("Getting sys inflow/outflow plot for ", id, ". For export? ", isExport))
  if (id == "sys_inflow_outflow_summary_ui_chart") {
    df <- sys_inflow_outflow_annual_chart_data() %>%
      # collapse the detailed levels of inflow/outflow
      fmutate(Summary = fct_collapse(Summary, !!!collapse_details)) %>%
      collap(cols="N", ~ InflowOutflow + Summary + PlotFillGroups, fsum, sort=FALSE)
    mid_plot <- 2.5
  } else {
    df <- sys_inflow_outflow_annual_chart_data()
    # re-label FTH if export is >= 1094 days
    if(session$userData$days_of_data >= 1094)
      df[, Detail := fct_recode(Detail, "Inflow\nUnspecified" = "First-Time \nHomeless")]
    mid_plot <- 4.5
  }
  
  total_clients <- df[InflowOutflow == "Inflow", sum(N)]
  
  validate(
    need(
      total_clients > 0,
      message = no_data_msg
    )
  )
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
    )
  
  # s <- max(df$yend) + 20
  # num_segments <- 20
  # segment_size <- get_segment_size(s/num_segments)
  inflow_to_outflow <- df[PlotFillGroups %in% active_at_levels, sum(N)*-1]
  
  # https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
  ggplot(df, aes(x = group.id, fill = PlotFillGroups)) +
    # the bars
    geom_rect(
      aes(
        # control bar gap width
        xmin = group.id - 0.25,
        xmax = group.id + 0.25,
        ymin = ystart,
        ymax = yend
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
      color = "#4e4d47",
      size = sys_chart_text_font,
      inherit.aes = FALSE
    ) +
    # numeric labels for Inflow/Outflow
    geom_text(
      aes(
        x = group.id,
        label = if_else(!grepl("Active at", Summary), N_formatted, NA),
        y = if_else(Summary == "Inflow", yend, ystart), vjust = -.6
      ),
      size = sys_chart_text_font
    ) +
    
    ggtitle(
      paste0(
        sys_total_count_display(total_clients),
        "Total Change: ",
        if(inflow_to_outflow > 0) "+" else "", scales::comma(inflow_to_outflow),
        "\n",
        "\n"
      )
    ) +
    
    # color palette
    scale_fill_manual(values = bar_colors) +
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
      text = element_text(size = sys_chart_text_font, colour = "#4e4d47"),
      axis.text.x = element_text(
        size = get_adj_font_size(
          sys_axis_text_font * ifelse(windowSize()[1] < 1300, 0.9,1), 
          isExport),
        vjust = -.2), 
      axis.ticks.x = element_line(),
      axis.line.x = element_line(color = "#4e4d47", linewidth = 0.5),
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
      req(nrow(sys_inflow_outflow_annual_chart_data()) > 0)
      get_system_inflow_outflow_annual_plot(chart_id)
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
output$sys_inflow_outflow_monthly_ui_chart <- renderPlot({
  plot_data <- sys_inflow_outflow_monthly_chart_data()

  # Get Average Info for Title Display
  averages <- plot_data %>%
    collap(cols = "Count", FUN=fmean, by = ~ Summary)
  
  avg_monthly_change <- fmean(
    plot_data[plot_data$Summary == "Inflow", "Count"] - 
    plot_data[plot_data$Summary == "Outflow", "Count"]
  )
  
  level_of_detail_text <- case_when(
    input$syso_level_of_detail == "All" ~ "People",
    input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
  )
  
  ggplot(plot_data, aes(x = interaction(month, Summary), y = Count, fill = PlotFillGroups)) +
    geom_bar(
      data = plot_data[Summary == "Active at Start"],
      aes(x = month, y = Count, fill = PlotFillGroups),
      stat = "identity",
      position = "stack",
      width = active_at_start_bar_width,
      color = 'black',
      just = 1.8 # this moves the bar to the left of the month-center
    ) +
    geom_bar(
      data = plot_data[Summary == "Inflow"],
      aes(x = month, y = Count, fill = PlotFillGroups),
      stat = "identity",
      color = 'black',
      width = active_at_start_bar_width,
    ) +
    geom_bar(
      data = plot_data[Summary == "Outflow"],
      aes(x = month, y = Count, fill = PlotFillGroups),
      stat = "identity",
      width = active_at_start_bar_width,
      color = 'black',
      just = -0.8 # this moves the bar to the right of the month-center
    ) +
    
    scale_fill_manual(values = bar_colors, name = "Inflow/Outflow Types") + # Update legend title
    theme_minimal() +
    labs(
      x = "Month",
      y = paste0("Count of ", level_of_detail_text)
    ) +
    scale_x_discrete(expand = expansion(mult = c(0.045, 0.045))) + # make plto take up more space horizontally
    ggtitle(
      paste0(
        "Average Monthly Inflow: +", scales::comma(averages[Summary == "Inflow", Count], accuracy = 0.1), "\n",
        "Average Monthly Outflow: -", scales::comma(averages[Summary == "Outflow", Count], accuracy = 0.1), "\n",
        "Average Monthly Change in ", 
          level_of_detail_text, " in ", getNameByValue(syso_hh_types, input$syso_hh_type), ": ", 
          scales::comma(avg_monthly_change, accuracy = 0.1)
      )
    ) +
    theme(
      axis.text = element_blank(),
      axis.title.x = element_blank(), 
      axis.title.y = element_text(size = 15),   
      legend.position = "none",
      panel.grid = element_blank(),        # Remove gridlines
      axis.line.y = element_blank(), 
      axis.line.x = element_line(),          # Remove axis lines
      plot.margin = margin(l = 55),        # Increase left margin
      axis.ticks = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
})

# Pure line chart
output$sys_inflow_outflow_monthly_ui_chart_line <- renderPlot({
  plot_data <- sys_inflow_outflow_monthly_chart_data()
  
  # Get Average Info for Title Display
  averages <- plot_data %>%
    collap(cols = "Count", FUN=fmean, by = ~ Summary)
  
  avg_monthly_change <- fmean(
    plot_data[plot_data$Summary == "Inflow", "Count"] - 
      plot_data[plot_data$Summary == "Outflow", "Count"]
  )
  
  level_of_detail_text <- case_when(
    input$syso_level_of_detail == "All" ~ "People",
    input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
  )
  
  ggplot(plot_data, aes(x = month, y = Count, color = PlotFillGroups, group = PlotFillGroups)) +
    # Use geom_line to draw the lines
    geom_line(linewidth = 1.2) + # `linewidth` is preferred over `size` for lines >= ggplot2 3.4.0
    # Optional: Add points to mark the actual data points each month
    geom_point(size = 3) +
    
    scale_color_manual(values = bar_colors, name = "Group") + # Adjust legend title if needed
    
    theme_minimal() +
    labs(
      x = "Month",
      # Update Y-axis label to reflect what's plotted
      y = paste0("Count of ", level_of_detail_text, " (Active at Start)")
    ) + 
    # Adjust title to reflect the line chart's focus, but keep avg inflow/outflow context
    ggtitle(
      paste0(
        "Average Monthly Inflow: +", scales::comma(averages[Summary == "Inflow", Count], accuracy = 0.1), "\n",
        "Average Monthly Outflow: -", scales::comma(averages[Summary == "Outflow", Count], accuracy = 0.1), "\n",
        "Average Monthly Change in ", 
          level_of_detail_text, " in ", getNameByValue(syso_hh_types, input$syso_hh_type), ": ", 
          scales::comma(avg_monthly_change, accuracy = 0.1)
      )
    ) +
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 8)) + 
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(), 
      axis.title.y = element_text(size = 15),   
      legend.position = "none",
      panel.grid = element_blank(), 
      axis.text.y = element_text(size = 10, color = "black"), # Example: adjust size and color
      axis.ticks.y = element_line(color = "black"),
      axis.line.y = element_line(),          # Remove axis lines
      axis.line.x = element_line(),          # Remove axis lines
      plot.margin = margin(l = 55),        # Increase left margin
      axis.ticks.x = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
})

# Combined line + bar chart
output$sys_inflow_outflow_monthly_ui_chart_combined <- renderPlot({
  plot_data <- sys_inflow_outflow_monthly_chart_data()
  
  # --- Assume plot_data has columns: month, Count, PlotFillGroups ---
  # Example PlotFillGroups values: "Active at Start: Housed", "Active at Start: Homeless", "Inflow", "Outflow"
  
  # --- Assume bar_colors is a NAMED vector like this: ---
  # bar_colors <- c(
  #   "Active at Start: Housed" = "skyblue3",
  #   "Active at Start: Homeless" = "steelblue",
  #   "Inflow" = "darkgreen",
  #   "Outflow" = "firebrick"
  # )
  # Make sure your actual bar_colors object matches the names in PlotFillGroups
  
  # Get Average Info for Title Display
  averages <- plot_data %>%
    collap(cols = "Count", FUN=fmean, by = ~ Summary) # Assuming Summary is equivalent to PlotFillGroups for averages
  
  avg_monthly_change <- fmean(
    plot_data[plot_data$PlotFillGroups == "Inflow", "Count"] - # Use PlotFillGroups here too
      plot_data[plot_data$PlotFillGroups == "Outflow", "Count"] # Use PlotFillGroups here too
  )
  
  level_of_detail_text <- case_when(
    input$syso_level_of_detail == "All" ~ "People",
    input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
  )
  
  # --- Define categories for easier filtering ---
  bar_categories <- c("Active at Start: Housed", "Active at Start: Homeless")
  line_categories <- c("Inflow", "Outflow")
  
  # --- Build the ggplot ---
  ggplot(mapping = aes(x = month)) +  # Base plot with only x aesthetic
    
    # --- Stacked Bars Layer ---
    geom_col(
      data = plot_data[Summary == "Active at Start"],
      aes(y = Count, fill = PlotFillGroups),
      position = "stack",
      width = 0.3
    ) +
    
    # --- Lines Layer ---
    geom_line(
      data = plot_data %>% filter(PlotFillGroups %in% line_categories),
      aes(y = Count, color = PlotFillGroups, group = PlotFillGroups), # Group is important for lines
      linewidth = 1.4
    ) +
    
    # --- Points Layer (for lines) ---
    geom_point(
      data = plot_data %>% filter(PlotFillGroups %in% line_categories),
      aes(y = Count, fill = PlotFillGroups), # No group needed if color is mapped
      shape = 21,                            # <--- ADDED: Use shape 21 for border/fill control
      color = "black",                       # <--- ADDED: Set border color to black (outside aes)
      size = 3
    ) +
    
    # --- Scales ---
    # Use scale_fill_manual for the bars
    scale_fill_manual(values = bar_colors) +
    
    # Use scale_color_manual for the lines/points
    scale_color_manual(values = bar_colors, name = "Group") +
    
    # --- Theme and Labels ---
    theme_minimal() +
    labs(
      x = "Month",
      # Update Y-axis label to be more general
      y = paste0("Count of ", level_of_detail_text)
    ) +
    ggtitle(
      paste0(
        "Average Monthly Inflow: +", scales::comma(averages[Summary == "Inflow", Count], accuracy = 0.1), "\n",
        "Average Monthly Outflow: -", scales::comma(averages[Summary == "Outflow", Count], accuracy = 0.1), "\n",
        "Average Monthly Change in ",
        level_of_detail_text, " in ", getNameByValue(syso_hh_types, input$syso_hh_type), ": ",
        scales::comma(avg_monthly_change, accuracy = 0.1)
      )
    ) +
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 8)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 15),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.line.y = element_line(),
      axis.line.x = element_line(),
      plot.margin = margin(l = 55),
      axis.ticks.x = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
  
})


### Table --------------------------------------
# The table is positioned directly under the chart
# Making the month labels looks like both the chart's x-axis and the table's column headers
output$sys_inflow_outflow_monthly_table <- renderDT({
  summary_data <- pivot(
    sys_inflow_outflow_monthly_chart_data(),
    ids = "PlotFillGroups",        # Column(s) defining the rows of the output
    names = "month",       # Column whose values become column names
    values = "Count",  # An arbitrary column to count (used with fun)
    how = "wider",
    fill = 0             # Fill missing combinations with 0
  ) %>%
    # prepend Active at Start to Housed and Homeless
    ftransform(
      PlotFillGroups = fifelse(
        PlotFillGroups %in% active_at_levels, 
        paste0("Active at Start: ", PlotFillGroups),
        as.character(PlotFillGroups)
      )
    )
  
  # Get Monthly Change (Inflow - Outflow)
  month_cols <- names(summary_data)[-1]

  change_row <- summary_data[PlotFillGroups == "Inflow", ..month_cols] -
    summary_data[PlotFillGroups == "Outflow", ..month_cols]
  summary_data_with_change <- rbind(
    summary_data, 
    add_vars(change_row, PlotFillGroups = "Monthly Change")
  )

  # Prepend Active at Start to the bar_colors
  active_at_levels_explicit <- paste0("Active at Start: ", active_at_levels)
  indices_to_modify <- names(bar_colors) %in% active_at_levels
  names(bar_colors)[indices_to_modify] <- active_at_levels_explicit
  
  setnames(summary_data_with_change, "PlotFillGroups", " ")
  datatable(summary_data_with_change,
            options = list(
              dom = 't',
              ordering = FALSE,
              # pageLength = 4,
              columnDefs = list(
                list(width = "48px", targets = 0), # Set first column width
                list(className = 'dt-center', targets = '_all') # Center text
              )
            ),
            rownames = FALSE)  %>%
    # Highlight only the first column of "Inflow" and "Outflow" rows
    formatStyle(
      columns = 1,  # First column
      target = "cell",
      backgroundColor = styleEqual(
        names(bar_colors),
        unname(bar_colors)
      ),
      border = styleEqual(
        c(active_at_levels_explicit, "Inflow", "Outflow"),
        c(rep("2px solid black", 2), "2px solid black", "2px solid black")
      )
    ) %>%
    # Highlight max change
    formatStyle(
      columns = month_cols[which.max(change_row)],
      target = "cell",
      backgroundColor = styleRow(nrow(summary_data_with_change), bar_colors["Inflow"])
    ) %>%
    # Contrast font and background colors
    formatStyle(
      columns = 1,
      target = "cell",
      color = styleEqual(
        c("Active at Start: Homeless", "Outflow"),
        c("white", "white")
      )
    ) %>%
    # Highlight min change
    formatStyle(
      columns = month_cols[which.min(change_row)],
      target = "cell",
      backgroundColor = styleRow(nrow(summary_data_with_change), bar_colors["Outflow"])
    )
})

### Inactive chart --------------------------------------
output$sys_inactive_monthly_ui_chart <- renderPlot({
  plot_data <- get_inactive_counts()
  
  level_of_detail_text <- case_when(
    input$syso_level_of_detail == "All" ~ "People",
    input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
  )

  ggplot(plot_data, aes(x = month, y = Count, fill = Type)) +
    geom_col(position = position_dodge(
      preserve="single", 
      width = 0.6 # space between bars within a group
    ), width = 0.5) +
    scale_fill_manual(values = inactive_bar_colors, name = "Inactive Type") + # Update legend title
    theme_minimal() +
    labs(
      x = "Month",
      y = paste0("Count of ", level_of_detail_text)
    ) +
    theme(
      axis.text = element_text(size = 11),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 15),
      axis.line.x = element_line()
    )
})

# Info to include in Inflow/Outflow Exports -----------------------------------
sys_inflow_outflow_export_info <- function() {
  df <- sys_inflow_outflow_annual_chart_data()
  
  data.table(
    Chart = c(
      "Total Served (Start + Inflow) People",
      "Total Inflow",
      "Total Outflow",
      "Total Change"
    ),
    Value = as.character(c(
      sum(df[InflowOutflow == 'Inflow']$N, na.rm = TRUE),
      sum(df[Summary %in% inflow_detail_levels]$N, na.rm = TRUE),
      sum(df[Summary %in% outflow_detail_levels]$N, na.rm = TRUE),   
      sum(df[Summary == "Active at End"]$N, na.rm = TRUE) -
        sum(df[Summary == "Active at Start"]$N, na.rm = TRUE)
    ))
  )
}

# Tabular/Excel Export --------------------------------------------------------
output$sys_inflow_outflow_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Flow Report - "),
  content = function(file) {
    df <- sys_inflow_outflow_annual_chart_data() %>% 
      fmutate(Summary = fct_collapse(Summary, !!!collapse_details))
    
    totals_df <- df %>% 
      fgroup_by(Summary) %>% 
      fsummarise(Detail = paste0("Total ", Summary[1]),
                 N = fsum(N, na.rm = TRUE))
    
    write_xlsx(
      list(
        "System Flow Metadata" = sys_export_summary_initial_df() %>%
          bind_rows(sys_export_filter_selections()) %>%
          bind_rows(sys_inflow_outflow_export_info()) %>%
          mutate(Value = replace_na(Value, 0)) %>%
          rename("System Flow" = Value),
        "System Flow Data" = bind_rows(df, totals_df) %>%
          roworder(Summary) %>%
          fselect(
            "Summary Category" = Summary,
            "Detail Category" = Detail,
            "Count" = N
          )
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    logMetadata(session, paste0(
      "Downloaded Sys Inflow Outflow Report",
      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")
    ))
    
    exportTestValues(sys_inflow_outflow_report = summarize_df(get_inflow_outflow_full()))
  }
)

# PowerPoint/Image Export -----------------------------------------------------
output$sys_inflow_outflow_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Flow_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    sys_overview_ppt_export(
      file = file,
      title_slide_title = "System Flow",
      summary_items = sys_export_summary_initial_df() %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(sys_export_filter_selections()) %>%
        bind_rows(sys_inflow_outflow_export_info()),
      plot_slide_title = "System Flow Summary",
      plot1 = get_system_inflow_outflow_annual_plot("sys_inflow_outflow_summary_ui_chart",
                                             isExport = TRUE),
      plot2 = get_system_inflow_outflow_annual_plot("sys_inflow_outflow_detail_ui_chart",
                                             isExport = TRUE),
      summary_font_size = 19
    )
  }
)

qc_checks <- function() {
  browser()

  # TESTING DIFF BETWEEN FULL AND MBM
  full <- period_specific_data()[[1]]
  all_months <- rbindlist(period_specific_data()[-1])
  setdiff(sort(unique(full$PersonalID)), sort(unique(all_months$PersonalID)))
  setdiff(sort(unique(all_months$PersonalID)), sort(unique(full$PersonalID)))
  # 
  # 
  # json_str <- jsonlite::toJSON(
  #   unique(
  #     merge(
  #       enrollment_categories[
  #         !(PersonalID %in% unique(full$PersonalID)),
  #         .(EnrollmentID, PersonalID, EntryDate, ExitAdjust, ProjectType, lh_prior_livingsituation)
  #       ],
  #       homeless_cls[, .(EnrollmentID, InformationDate)],
  #       by = "EnrollmentID"
  #     )[, .(PersonalID, InformationDate, EntryDate, ExitAdjust, ProjectType, lh_prior_livingsituation)]
  #   ),
  #   null="list",
  #   na="string",
  #   pretty = TRUE, auto_unbox = TRUE
  # )
  # json_str <- gsub("true", "True", json_str)
  # json_str <- gsub("false", "False", json_str)
  # 
  #
  # # Check that one enrollment isn't considered an inflow in multiple months
  # # someone can have an exit in between or they can be active at start
  #
  monthly_universe_ppl_flags <- get_inflow_outflow_monthly()
  
  # check prior month's status (L() is the collapse package's Lag function)
  # if they outflowed last month, they should have inflowed this month
  # and if they were active at end last month, they should be active at start, this month
  full_combinations <- CJ(
    PersonalID = unique(monthly_universe_ppl_flags$PersonalID), 
    month = levels(monthly_universe_ppl_flags$month)
  )
  
  # check for dup First-Time Homeless 
  first_time_homeless_dups <- all_months[
    InflowTypeDetail == "First-Time \nHomeless"
  ][, .N, by = PersonalID][N > 1]
  if(nrow(first_time_homeless_dups) > 0) {
    warning(glue("There are people that have multiple First-Time Homeless values in the mbm."))
    print(all_months[PersonalID %in% first_time_homeless_dups$PersonalID, .(
      PersonalID, 
      EnrollmentID,
      EntryDate,
      ExitAdjust,
      month,
      InflowTypeDetail,
      active_at_start_homeless,
      active_at_start_housed,
      at_least_14_days_to_eecr_enrl,
      first_lookback_perm_dest,
      first_lookback_temp_dest,
      return_from_perm_client,
      reengaged_from_temp_client,
      first_time_homeless_client,
      unknown_at_start
    )])
  }
  
  qc <- monthly_universe_ppl_flags %>%
    join(
      full_combinations,
      on = c("PersonalID", "month"),
      how = "full"
    ) %>%
    fsubset(order(PersonalID, month)) %>%
    fmutate(problem = !(
      (L(OutflowTypeSummary, g = PersonalID) == "Outflow" & Summary == "Inflow") | 
      (L(OutflowTypeSummary, g = PersonalID) == "Active at End" & InflowTypeSummary %in% c("Inflow","Active at Start")) |
      (L(OutflowTypeSummary, g = PersonalID) == "Inactive" & InflowTypeSummary %in% c("Inflow","Active at Start"))
    )) %>%
    fgroup_by(PersonalID) %>%
    fmutate(
      has_problem = anyv(problem, TRUE)
    ) %>%
    fungroup() %>%
    fsubset(has_problem)

}
