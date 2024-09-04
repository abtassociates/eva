# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
# Define the hardcoded values for Time and Status
# we need all combinations for the 0s

frame_detail <- 
  data.frame(
    Status = c(
      "Housed",
      "Homeless",
      "Newly Homeless",
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
      "Newly Homeless",
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
                   "Newly Homeless",
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
          "Newly Homeless",
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

renderSystemPlot <- function(id) {
  output[[id]] <- renderPlot({
    req(valid_file() == 1)
    if (id == "sys_act_summary_ui_chart") {
      df <- system_activity_prep_summary()
      mid_plot <- 2.5
    } else {
      df <- system_activity_prep_detail()
      mid_plot <- 4.5
    }
    
    df <- df %>%
      mutate(
        values = ifelse(InflowOutflow == "Outflow", values * -1, values),
        ystart = lag(cumsum(values), default = 0),
        yend = round(cumsum(values))
      )
    
    colors <- c('#C6BDB9', '#73655E', '#C34931', '#16697A')
    s <- max(df$yend) + 20
    num_segments <- 20
    segment_size <- get_segment_size(s/num_segments)
    
    total_clients <- df %>%
      filter(InflowOutflow == "Inflow") %>%
      pull(values) %>%
      sum()
    
    validate(
      need(
        total_clients > 10,
        message = "The dataset has been filtered to fewer than 11 records, therefore
          the plot will not be displayed for privacy purposes."
      )
    )
    
    inflow_to_outflow <- df %>%
      filter(PlotFillGroups %in% c("Housed", "Homeless")) %>%
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
        colour = "#4e4d47",
        linewidth = .2,
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
      # the labels
      ggrepel::geom_text_repel(
        aes(
          x = group.id,
          label = paste0(scales::comma(abs(values))),
          y = rowSums(cbind(ystart, values / 2)),
          segment.colour = "gray33"
        ),
        direction = "y",
        nudge_x = -.35,
        colour = "#4e4d47",
        size = 5,
        inherit.aes = FALSE
      ) +
      # annotation: refer to helper_functions.R for sys_total_count_display() code
      annotate(
        geom = "text",
        x = mid_plot,
        y = max(df$yend) * 1.1,
        size = 16 / .pt,
        label = paste0(
          sys_total_count_display(total_clients),
          "\nTotal Change: ",
          case_when(
            inflow_to_outflow > 0 ~ paste0("+", scales::comma(inflow_to_outflow)),
            inflow_to_outflow == 0 ~ "0",
            inflow_to_outflow < 0 ~ scales::comma(inflow_to_outflow))
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
        text = element_text(size = 16, colour = "#4e4d47"),
        axis.text.x = element_text(size = 16),
        axis.ticks.x = element_line(),
        axis.line.x = element_line(colour = "#4e4d47", linewidth = 0.5),
        plot.margin = unit(c(3, 1, 1, 1), "lines"),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(.5, 0, 0, 0, unit = "inch")
      )
    
  })}


# Plot prompts for plot subtitle ------------------------------------------

syso_detailBox <- reactive({
  # remove group names from race/ethnicity filter
  # so we can use getNameByValue() to grab the selected option label
  # if (input$methodology_type == 2) {
    # browser()
  # }
  detail_line <- function(detail_label, val_list, inputVal) {
    return(
      HTML(glue(
        "<b>{detail_label}:</b> {getNameByValue(val_list, inputVal)} <br>"
      ))
    )
  }
  
  selected_race <- getNameByValue(
    unlist(syso_race_ethnicity_cats(input$methodology_type)),
    input$syso_race_ethnicity
  )
  
  race_ethnicity_line <- HTML(glue(
    "<b>Race/Ethnicity:</b> {
          str_sub(
            selected_race, 
            start = str_locate(
              selected_race,
              '\\\\.'
            )[, 1] + 1,
            end = -1L
          )
        } <br>"
  ))
  
  list(
    br(),
    strong("Date Range: "),
    
    ReportStart(), " to ", ReportEnd(), br(),
    
    if (getNameByValue(syso_project_types, input$syso_project_type) != "All")
      detail_line("Project Type", syso_project_types, input$syso_project_type),
    
    detail_line("Methodology Type", syso_methodology_types, input$methodology_type),
    
    if (length(input$syso_age) != length(syso_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syso_age, collapse = ', ')} <br>"
      )),
    
    if (getNameByValue(syso_gender_cats(), input$syso_gender) != "All Genders")
      detail_line("Gender", syso_gender_cats(input$methodology_type), input$syso_gender),
    
    if (selected_race != "All.All Races/Ethnicities")
      race_ethnicity_line,
    
    if(getNameByValue(syso_spec_pops_people, input$syso_spec_pops) != "None")
      detail_line("Special Populations", syso_spec_pops_people, input$syso_spec_pops)
    
  )
})

#### DISPLAY CHART ###
renderSystemPlot("sys_act_summary_ui_chart")
renderSystemPlot("sys_act_detail_ui_chart")

output$sys_inflow_outflow_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Inflow/Outflow Report - "),
  content = function(file) {
    df <- system_activity_prep_detail() %>% 
      select(Status, values, Time, InflowOutflow, InflowOutflowSummary)

    write_xlsx(
      list(
        "System Flow Metadata" = sys_export_summary_initial_df() %>%
          bind_rows(sys_export_filter_selections()) %>%
          bind_rows(tibble(
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
              sum(df[df$Time == "Active at Start", 'values'], na.rm = TRUE) -
                sum(df[df$Time == "Active at End", 'values'], na.rm = TRUE)
            ))
          )) %>%
          mutate(Value = replace_na(Value, 0)) %>%
          rename("System Inflow/Outflow" = Value),
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
    
    exportTestValues(sys_comp_report = sys_inflow_outflow_plot_data())
  }
)