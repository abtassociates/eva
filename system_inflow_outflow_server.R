# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
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


# System Activity Detail Chart Prep ---------------------------------------

system_activity_prep_detail <- function() {
  # Define the hardcoded values for Time and Status
  # we need all combinations for the 0s
  status_levels_detail <- c(
    "Housed",
    "Homeless",                          
    if_else(
      session$userData$days_of_data >= 1094,
      "First-Time \nHomeless",
      "Inflow\nUnspecified"
    ),
    "Returned from \nPermanent",
    "Re-engaged from \nNon-Permanent",
    "Exited,\nNon-Permanent",
    "Exited,\nPermanent",
    "Inactive"
  )
  
  time_levels_detail <- c(
    "Active at Start",
    if_else(
      session$userData$days_of_data >= 1094,
      "First-Time \nHomeless",
      "Inflow\nUnspecified"
    ),
    "Returned from \nPermanent",
    "Re-engaged from \nNon-Permanent",
    "Exited,\nNon-Permanent",
    "Exited,\nPermanent",
    "Inactive",
    "Active at End"
  )
  
  time_levels_summary <- c("Active at Start",
                           "Inflow",
                           "Outflow",
                           "Active at End")
  
  frame_detail <- data.frame(
    Status = c(
      status_levels_detail,
      "Homeless",
      "Housed"),
    Time = c(
      rep(time_levels_detail[1], 2),
      time_levels_detail[2:7],
      rep(time_levels_detail[8], 2)),
    InflowOutflow = c(rep("Inflow", 5), rep("Outflow", 5)),
    PlotFillGroups = 
      c("Housed",
        "Homeless",
        rep("Inflow", 3),
        rep("Outflow", 3),
        "Homeless",
        "Housed")
  )

  inflow <- sys_plot_data$inflow_outflow_full %>%
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
  
  outflow <- sys_plot_data$inflow_outflow_full %>%
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
        levels = time_levels_detail
      ),
      Status = factor(
        Status,
        levels = status_levels_detail
      ),
      
      InflowOutflowSummary = factor(
        case_when(
          str_detect(Time, "Exited") | Time == "Inactive" ~ "Outflow",
          str_detect(Time, "Active at") ~ Time,
          TRUE ~ "Inflow"
        ),
        levels = time_levels_summary)
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
}

# System Activity Summary Chart Prep ---------------------------------------

system_activity_prep_summary <- function() {
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
}

get_system_inflow_outflow_plot <- function(id, isExport = FALSE) {
  if (id == "sys_act_summary_ui_chart") {
    df <- system_activity_prep_summary()
    mid_plot <- 2.5
  } else {
    df <- system_activity_prep_detail()
    mid_plot <- 4.5
  }
  
  # browser()
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
      hjust = 1,
      # direction = "y",
      segment.colour = NA,
      nudge_x = ifelse(windowSize()[1] < 1300, -.4, -.3),
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
      axis.text.x = element_text(
        size = get_adj_font_size(
          sys_axis_text_font * ifelse(windowSize()[1] < 1300, 0.9,1), 
          isExport),
        vjust = -.2), 
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
    req(session$userData$valid_file() == 1)
    req(sys_plot_data$inflow_outflow_full)
    get_system_inflow_outflow_plot(id)
  },
  alt = case_when(
    id == "sys_act_summary_ui_chart" ~ "A waterfall bar chart of the homeless system's inflow and outflow during 
      the report period. The summary view of this chart includes four components: 
      Active at Start, Inflow, Outflow, and Active at End.",
    id == "sys_act_detail_ui_chart" ~ "A waterfall bar chart of the homeless system's inflow and 
      outflow during the report period. The detailed view of this chart 
      shows inflow as three subcategories: first-time homeless, returned from 
      permanent, and re-engaged from non-permanent and outflow as three 
      subcategories: exited non-permanent, exited permanent, and inactive.",
    id == "sys_act_monthly_ui_chart" ~ "A waterfall bar chart of the homeless system's inflow and outflow during each month of the report period."
  ),
  width = ifelse(isTRUE(getOption("shiny.testmode")), 1113, "auto")
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
      "Total Change"
    ),
    Value = as.character(c(
      sum(df[df$InflowOutflow == 'Inflow', 'values'], na.rm = TRUE),
      sum(df[df$InflowOutflowSummary == 'Inflow', 'values'], na.rm = TRUE),
      sum(df[df$InflowOutflowSummary == 'Outflow', 'values'], na.rm = TRUE),   
      sum(df[df$Time == "Active at End", 'values'], na.rm = TRUE) -
        sum(df[df$Time == "Active at Start", 'values'], na.rm = TRUE)
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
          df, 
          df %>% 
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
    
    exportTestValues(client_level_export_df = sys_plot_data$client_level_export_df %>% nice_names())
    exportTestValues(sys_inflow_outflow_report = summarize_df(sys_plot_data$inflow_outflow_full))
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
      plot1 = get_system_inflow_outflow_plot("sys_act_summary_ui_chart",
                                             isExport = TRUE),
      plot2 = get_system_inflow_outflow_plot("sys_act_detail_ui_chart",
                                             isExport = TRUE),
      summary_font_size = 19
    )
  }
)

# Month-by-Month Chart+Table ----------------------------------------------
output$sys_act_monthly_ui_chart <- renderPlot({
  data <- sys_plot_data$inflow_outflow_monthly
  
  # create and append inflow and outflow bar datasets
  plot_data <- rbindlist(list(
    data[, .(month = month, 
             Count = Inflow, 
             Flow_Type = "Inflow")],
    data[, .(month = month, 
             Count = Outflow, 
             Flow_Type = "Outflow")]
  ))
  setorder(plot_data, month)

  ggplot(plot_data, 
         aes(x = month, y = Count, fill = Flow_Type, group=Flow_Type)) +
    geom_col(position = position_dodge(
      preserve="single", 
      width = 0.6 # space between bars within a group
    ), width = 0.5) + # width of bar
    scale_fill_manual(values = c("Inflow" = "#BDB6D7", "Outflow" = "#7F5D9D")) +
    theme_minimal() +
    labs(
      x = "Month",
      y = paste0("Count of ", case_when(
        input$syso_level_of_detail == "All" ~ "People",
        input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
        TRUE ~
          getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
      )),
      fill = "Flow Type"
    ) +
    scale_x_discrete(expand = expansion(mult = c(0.045, 0.045))) + #increase space between groups
    theme(
      axis.text = element_blank(),
      axis.title.x = element_blank(), 
      axis.title.y = element_text(size = 15),   
      legend.position = "none",
      panel.grid = element_blank(),        # Remove gridlines
      axis.line.y = element_blank(), 
      axis.line.x = element_line(),          # Remove axis lines
      plot.margin = margin(l = 48),        # Increase left margin
      axis.ticks = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
})

# Create summary table
output$sys_act_monthly_table <- renderDT({
  summary_data <- suppressWarnings(dcast(
    melt(sys_plot_data$inflow_outflow_monthly, id.vars = "month", variable.name = "Type"),
    Type ~ month,
    value.var = "value"
  ))
  
  max_change <- max(summary_data[Type == "Monthly Change", -1, with = FALSE])
  min_change <- min(summary_data[Type == "Monthly Change", -1, with = FALSE])
  
  datatable(summary_data,
            options = list(
              dom = 't',
              ordering = FALSE,
              pageLength = 3,
              columnDefs = list(
                list(
                  width = "48px",    # Set to specific width
                  targets = 0
                ),
                # center table text
                list(
                  className = 'dt-center',
                  targets = '_all'  
                )
              )
            ),
            rownames = FALSE)  %>%
    formatStyle(
      names(summary_data),
      backgroundColor = styleEqual(
        c(
          max_change,
          min_change,
          "Inflow",
          "Outflow"
        ),
        c(
          scales::alpha("#BDB6D7", 0.5),  # 50% transparency for max
          scales::alpha("#7F5D9D", 0.5),  # 50% transparency for min
          "#BDB6D7", 
          "#7F5D9D"
        )
      ),
      border = styleEqual(
        c("Inflow", "Outflow", max_change, min_change),  # Match values where highlight=1 and max/min values
        c(
          "2px solid black",     # For highlighted cells
          "2px solid black",     # For highlighted cells
          "2px solid black",     # For max value
          "2px solid black"      # For min value
        )
      )
    )
})

# Client-level enrollment summary data reactive ---------------------------
# get final people-level, inflow/outflow dataframe by joining the filtered 
# enrollment and people dfs, as well as flagging their inflow and outflow types
# browser()
get_inflow_outflow_full <- function() {
  period_specific_data()[["Full"]] %>%
    select(PersonalID,
           InflowTypeSummary,
           InflowTypeDetail,
           OutflowTypeSummary,
           OutflowTypeDetail
    ) %>%
    unique()
}
# AS QC check:
#   missing_types <- universe() %>%
#     inner_join(
#       plot_data %>%
#         filter(
#           OutflowTypeDetail == "something's wrong" |
#             InflowTypeDetail == "something's wrong"),
#       by = "PersonalID") %>%
#     mutate(
#       missing_inflow = eecr == TRUE & InflowTypeDetail == "something's wrong",
#       missing_outflow = lecr == TRUE & OutflowTypeDetail == "something's wrong",
#     ) %>%
#     filter(missing_inflow == TRUE | missing_outflow == TRUE)
#   
# # browser()
#   
#   category_counts <- plot_data %>%
#     select(PersonalID, InflowTypeDetail, OutflowTypeDetail) %>%
#     pivot_longer(
#       cols = c(InflowTypeDetail, OutflowTypeDetail), 
#       names_to = "Time", 
#       values_to = "Status") %>%
#     group_by(Time, Status) %>%
#     summarise(values = n()) %>%
#     ungroup() %>%
#     filter(!is.na(Status)) %>%
#     mutate(
#       values = ifelse(Time == "OutflowTypeDetail", values * -1, values),
#       inflow_outflow = Time,
#       Time = case_when(
#         Time == "InflowTypeDetail" &
#           Status %in% c("Homeless", "Housed")
#         ~ paste0("Active as of \n", session$userData$ReportStart),
#         
#         Time == "OutflowTypeDetail" &
#           Status %in% c("Homeless", "Housed")
#         ~ paste0("Active as of \n", session$userData$ReportEnd),
#         
#         Time == "InflowTypeDetail"
#         ~ "Inflow",
#         
#         Time == "OutflowTypeDetail"
#         ~ "Outflow"
#       )
#     )

# newly_homeless_clients <- plot_data %>%
#   filter(InflowTypeDetail == "Newly Homeless") %>%
#   pull(PersonalID) %>%
#   unique()
# 
# enrollment_categories  %>%
#   group_by(PersonalID) %>%
#   mutate(Count = n()) %>%
#   ungroup() %>%
#   filter(PersonalID %in% c(newly_homeless_clients) & Count > 1) %>%
#   mutate(DestinationDescription = living_situation(Destination),
#          ReportStart = session$userData$ReportStart,
#          ReportEnd = session$userData$ReportEnd,
#          ExportStart = ExportStartAdjusted,
#          ExportEnd = ExportEndAdjusted,
#          LookbackBegins = session$userData$ReportStart - years(2),
#          ProjectType = project_type_abb(ProjectType),
#          LivingSituation = living_situation(LivingSituation)) %>%
#   select(
#     PersonalID,
#     EnrollmentID,
#     ExportStart,
#     LookbackBegins,
#     ReportStart,
#     EntryDate,
#     ExitAdjust,
#     ReportEnd,
#     ExportEnd,
#     ProjectType,
#     LivingSituation,
#     DestinationDescription,
#     days_to_next_entry,
#     days_since_previous_exit,
#     lecr,
#     eecr,
#     lookback
#   ) -> for_review
# 
# write_csv(for_review, here("newly_homeless_20240912a.csv"))

# Month-by-Month Prep ---------------------------------------------------
# browser()
get_inflow_outflow_monthly <- function() {
  unique(
    rbindlist(period_specific_data()[-1])[, `:=`(
      # remove "Unknowns"/"Inactives"
      InflowTypeSummary = fifelse(
        InflowTypeDetail == "Inactive",
        "Inactive",
        InflowTypeSummary
      ),
      OutflowTypeSummary = fifelse(
        OutflowTypeDetail == "Inactive",
        "Inactive",
        OutflowTypeSummary
      )
    )][, .(
      # Count unique PersonalIDs for each category using system flow logic
      Inflow = uniqueN(PersonalID[InflowTypeSummary == "Inflow"]),
      Outflow = uniqueN(PersonalID[OutflowTypeSummary == "Outflow"])
    ), by = month
    ][, `:=`(
      `Monthly Change` = Inflow - Outflow,
      month = factor(format(month, "%b"), 
                     levels = format(get_months_in_report_period(), "%b"))
    )]
  )
}

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
monthly_universe_ppl_flags <- unique(
rbindlist(period_specific_data()[-1])[,
# PersonalID %in% c("686041","684918","349625","556533","693996","614071","677683","701796","702055"),
.(PersonalID, EnrollmentID, month, InflowTypeSummary, OutflowTypeSummary)
][order(PersonalID, month)][, month:= format(month, "%b")])

qc <- monthly_universe_ppl_flags %>%
  fsubset(!(
    (L(OutflowTypeSummary, g = PersonalID) == "Outflow" & InflowTypeSummary == "Inflow") | 
    (L(OutflowTypeSummary, g = PersonalID) == "Active at End" & InflowTypeSummary == "Active at Start") 
  ))

}
# browser()
# monthly_universe_ppl_flags
