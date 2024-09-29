# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
# Define the hardcoded values for Time and Status
# we need all combinations for the 0s

frame_detail <- 
  data.frame(
    Status = c(
      "Housed",
      "Homeless",
      "First-Time \nHomeless",
      "Returned from \nPermanent",
      "Re-engaged from \nNon-Permanent",
      "Exited,\nPermanent",
      "Exited,\nNon-Permanent",
      "Inactive",
      "Homeless",
      "Housed"
    ),
    Time = c(
      rep("Active at Start", 2),
      "First-Time \nHomeless",
      "Returned from \nPermanent",
      "Re-engaged from \nNon-Permanent",
      "Exited,\nPermanent",
      "Exited,\nNon-Permanent",
      "Inactive",
      rep("Active at End", 2)
    ),
    InflowOutflow = c(rep("Inflow", 5), rep("Outflow", 5)),
    PlotFillGroups = 
      c("Housed",
        "Homeless",
        rep("Inflow", 3),
        rep("Outflow", 3),
        "Homeless",
        "Housed")
  )

# frame_summary <-
#   data.frame(
#     Status = c("Housed",
#                "Homeless",
#                "Inflow",
#                "Outflow",
#                "Homeless",
#                "Housed"),
#     Time = c(rep(paste0("Active at Start"), 2),
#              "Inflow",
#              "Outflow",
#              rep(paste0("Active at End"), 2)),
#     InflowOutflow = c(rep("Inflow", 3), rep("Outflow", 3)),
#     PlotFillGroups = c("Housed", "Homeless",
#                        "Inflow", "Outflow",
#                        "Homeless", "Housed")
#   )

system_activity_prep_detail <- reactive({
  inflow <- sys_inflow_outflow_plot_data() %>%
    select(PersonalID,
           InflowTypeSummary,
           InflowTypeDetail) %>%
    group_by(InflowTypeDetail) %>%
    summarise(values = n()) %>%
    ungroup() %>%
    rename("Status" = InflowTypeDetail) %>%
    full_join(frame_detail %>%
                filter(InflowOutflow == "Inflow")) %>%
    mutate(values = replace_na(values, 0))
  
  outflow <- sys_inflow_outflow_plot_data() %>%
    select(PersonalID,
           OutflowTypeSummary,
           OutflowTypeDetail) %>%
    group_by(OutflowTypeDetail) %>%
    summarise(values = n()) %>%
    ungroup() %>%
    rename("Status" = OutflowTypeDetail) %>%
    full_join(frame_detail %>%
                filter(InflowOutflow == "Outflow")) %>%
    mutate(values = replace_na(values, 0))
  
  inflow %>%
    full_join(outflow,
              join_by(Time, values, Status, InflowOutflow, PlotFillGroups)
              ) %>%
    mutate(
      Time = factor(
        Time,
        levels = c("Active at Start",
                   "First-Time \nHomeless",
                   "Returned from \nPermanent",
                   "Re-engaged from \nNon-Permanent",
                   "Exited,\nNon-Permanent",
                   "Exited,\nPermanent",
                   "Inactive",
                   "Active at End")
      ),
      Status = factor(
        Status,
        levels = c(
          "Housed",
          "Homeless",                          
          "First-Time \nHomeless",
          "Returned from \nPermanent",
          "Re-engaged from \nNon-Permanent",
          "Exited,\nNon-Permanent",
          "Exited,\nPermanent",
          "Inactive"
        )
      ),
      InflowOutflowSummary = factor(
        case_when(
          str_detect(Time, "Exited") | Time == "Inactive" ~ "Outflow",
          str_detect(Time, "Active at") ~ Time,
          TRUE ~ "Inflow"
        ),
        levels = c("Active at Start",
                   "Inflow",
                   "Outflow",
                   "Active at End"))
    ) %>%
    group_by(Time) %>%
    mutate(group.id = cur_group_id()) %>%
    ungroup() %>%
    arrange(Time,  case_when(
      Time == "Active at Start" & Status == "Housed" ~ 1,
      Time == "Active at Start" & Status == "Homeless" ~ 2,
      Time == "Active at End" & Status == "Homeless" ~ 1,
      Time == "Active at End" & Status == "Housed" ~ 2,
      TRUE ~ 3  # fallback for other statuses/times
    ))
})

system_activity_prep_summary <- reactive({
  setDT(system_activity_prep_detail())[, .(
    values = sum(values, na.rm = TRUE)
  ), by = .(InflowOutflow, PlotFillGroups, InflowOutflowSummary)
  ][, group.id := .GRP, by = InflowOutflowSummary
  ][, Time := InflowOutflowSummary
  ][, .SD[!duplicated(.SD)], by = .(Time, PlotFillGroups)
  ][order(Time, case_when(
    Time == "Active at Start" & PlotFillGroups == "Housed" ~ 1,
    Time == "Active at Start" & PlotFillGroups == "Homeless" ~ 2,
    Time == "Active at End" & PlotFillGroups == "Homeless" ~ 1,
    Time == "Active at End" & PlotFillGroups == "Housed" ~ 2,
    TRUE ~ 3
  ))
  ]
})

get_system_inflow_outflow_plot <- function(id, isExport = FALSE) {
  if (id == "sys_act_summary_ui_chart") {
    df <- system_activity_prep_summary()
    mid_plot <- 2.5
  } else {
    df <- system_activity_prep_detail()
    mid_plot <- 4.5
  }
  
  total_clients <- df %>%
    filter(InflowOutflow == "Inflow") %>%
    pull(values) %>%
    sum()
  
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
  
  df <- df %>%
    mutate(
      values = ifelse(InflowOutflow == "Outflow", values * -1, values),
      ystart = lag(cumsum(values), default = 0),
      yend = round(cumsum(values))
    )
  
  colors <- c('#ECE7E3', '#9E958F', '#BDB6D7', '#6A559B')
  s <- max(df$yend) + 20
  # num_segments <- 20
  # segment_size <- get_segment_size(s/num_segments)

  
  inflow_to_outflow <- df %>%
    filter(PlotFillGroups %in% c("Housed", "Homeless")) %>%
    pull(values) %>%
    sum() * -1
  
  total_homeless_clients <- df %>%
    filter(InflowOutflow == "Inflow" & PlotFillGroups != "Housed") %>%
    pull(values) %>%
    sum()
  
  # waterfall plot ----------------------------------------------------------
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
        filter(group.id == group.id) %>%
        group_by(group.id) %>%
        slice_tail() %>%
        ungroup() %>%
        select(group.id, yend),
      aes(
        x = group.id,
        xend = if_else(group.id == last(group.id), last(group.id), group.id + 1),
        y = yend,
        yend = yend
      ),
      linewidth = .3,
      colour = "gray25",
      linetype = "dashed",
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    # numeric labels for Active at Start/End
    ggrepel::geom_text_repel(
      aes(
        x = group.id,
        label = if_else(!PlotFillGroups %in% c("Inflow", "Outflow") &
                          values != 0,
                        paste0(scales::comma(abs(values))), NA),
        y = rowSums(cbind(ystart, values / 2))
      ),
      direction = "y",
      min.segment.length = Inf,
      nudge_x = ifelse(windowSize()[1] < 1300, -.4, -.35),
      colour = "#4e4d47",
      size = sys_chart_text_font,
      inherit.aes = FALSE
    ) +
    # numeric labels for Inflow/Outflow
    geom_text(
      aes(
        x = group.id,
        label = if_else(PlotFillGroups %in% c("Inflow", "Outflow"),
                        paste0(scales::comma(abs(values))), NA),
        y = if_else(PlotFillGroups == "Inflow", yend, ystart), vjust = -.6
      ),
      size = sys_chart_text_font
    ) +
    
    ggtitle(
      paste0(
        sys_total_count_display(total_clients),
        "Total Change: ",
        case_when(
          inflow_to_outflow > 0 ~ paste0("+", scales::comma(inflow_to_outflow)),
          inflow_to_outflow == 0 ~ "0",
          inflow_to_outflow < 0 ~ scales::comma(inflow_to_outflow)
        ),
        "\n",
        "Total Homeless: ",
        scales::comma(total_homeless_clients),
        "\n",
        "\n"
      )
    ) +
    
    # color palette
    scale_fill_manual(values = colors) +
    # distance between bars and x axis line
    scale_y_continuous(expand = expansion()) +
    # x axis labels
    scale_x_continuous(
      labels = str_wrap(df$Time %>% unique(), width = 10),
      breaks = df$group.id %>% unique()
    ) +
    coord_cartesian(clip = "off") +
    # totally clear all theme elements
    theme_void() +
    # add back in what theme elements we want
    theme(
      text = element_text(size = sys_chart_text_font, colour = "#4e4d47"),
      axis.text.x = element_text(size = get_adj_font_size(sys_axis_text_font, isExport), vjust = -.2),
      axis.ticks.x = element_line(),
      axis.line.x = element_line(colour = "#4e4d47", linewidth = 0.5),
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

renderSystemPlot <- function(id) {
  output[[id]] <- renderPlot({
    req(valid_file() == 1)
    get_system_inflow_outflow_plot(id)
  },
  alt = case_when(id == "sys_act_summary_ui_chart" ~ 
                "A waterfall bar chart of the homeless system's inflow and outflow during 
              the report period. The summary view of this chart includes four components: 
              Active at Start, Inflow, Outflow, and Active at End.",
              TRUE ~ "A waterfall bar chart of the homeless system's inflow and 
              outflow during the report period. The detailed view of this chart 
              shows inflow as three subcategories: first-time homeless, returned from 
              permanent, and re-engaged from non-permanent and outflow as three 
              subcategories: exited non-permanent, exited permanent, and inactive.")
  )
}

#### DISPLAY CHART ###
renderSystemPlot("sys_act_summary_ui_chart")
renderSystemPlot("sys_act_detail_ui_chart")

sys_inflow_outflow_export_info <- function(df) {
  tibble(
    Chart = c(
      "Total Served (Start + Inflow) People",
      "Total Inflow",
      "Total Outflow",
      "Total Change",
      "Total Homeless"
    ),
    Value = as.character(c(
      sum(df[df$InflowOutflow == 'Inflow', 'values'], na.rm = TRUE),
      sum(df[df$InflowOutflowSummary == 'Inflow', 'values'], na.rm = TRUE),
      sum(df[df$InflowOutflowSummary == 'Outflow', 'values'], na.rm = TRUE),   
      sum(df[df$Time == "Active at End", 'values'], na.rm = TRUE) -
        sum(df[df$Time == "Active at Start", 'values'], na.rm = TRUE),
      sum(df[(df$InflowOutflow == 'Inflow' & df$Status != 'Housed'), 'values'], na.rm = TRUE)
    ))
  )
}
output$sys_inflow_outflow_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Flow Report - "),
  content = function(file) {
    df <- system_activity_prep_detail() %>% 
      select(Status, values, Time, InflowOutflow, InflowOutflowSummary)

    write_xlsx(
      list(
        "System Flow Metadata" = sys_export_summary_initial_df() %>%
          bind_rows(sys_export_filter_selections()) %>%
          bind_rows(sys_inflow_outflow_export_info(df)) %>%
          mutate(Value = replace_na(Value, 0)) %>%
          rename("System Flow" = Value),
        "System Flow Data" = bind_rows(
          df, df %>% 
            group_by(InflowOutflowSummary) %>% 
            reframe(Status = paste0("Total ",  InflowOutflowSummary),
                    Totals = sum(values, na.rm = TRUE)) %>%
            unique()
        ) %>%
          arrange(InflowOutflowSummary) %>%
          select("Summary Category" = InflowOutflowSummary,
                 "Detail Category" = Status,
                 "Count" = values,
                 Totals)
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )
    
    logMetadata(paste0(
      "Downloaded Sys Inflow Outflow Report",
      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")
    ))
    
    exportTestValues(sys_inflow_outflow_report = summary(sys_inflow_outflow_plot_data()))
  }
)

output$sys_inflow_outflow_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Flow_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    df <- system_activity_prep_detail() %>% 
      select(Status, values, Time, InflowOutflow, InflowOutflowSummary)
    
    sys_overview_ppt_export(
      file = file,
      title_slide_title = "System Flow",
      summary_items = sys_export_summary_initial_df() %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(sys_export_filter_selections()) %>%
        bind_rows(sys_inflow_outflow_export_info(df)),
      plot_slide_title = "System Flow Summary",
      plot1 = get_system_inflow_outflow_plot("sys_act_summary_ui_chart", isExport = TRUE),
      plot2 = get_system_inflow_outflow_plot("sys_act_detail_ui_chart", isExport = TRUE),
      summary_font_size = 19
    )
  }
)
