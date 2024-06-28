# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
# Define the hardcoded values for Time and Status
# we need all combinations for the 0s

status_summary_values <- c(
  "Unknown Status",
  "Homeless", 
  "Housed", 
  "Inflow", 
  "Outflow"
)

status_detail_values <- c(
  "Homeless", 
  "Housed",
  "Unknown Status",
  "Newly Homeless", 
  "Returned from \nPermanent", 
  "Re-engaged from \nNon-Permanent",
  "Continued system \nengagement",
  "Exited to \nPermanent Destination",
  "Exited to \nNon-Permanent Destination"
)

time_summary_values <- reactive(
    c(paste0("Active as of \n", ReportStart()),
    "Inflow",
    "Outflow",
    paste0("Active as of \n", ReportEnd()))
  )

time_detail_values <- reactive({
  c(paste0("Active as of \n", ReportStart()),
  "Newly Homeless",
  "Returned from \nPermanent",
  "Re-engaged from \nNon-Permanent",
  "Continued system \nengagement",
  "Exited to \nPermanent Destination",
  "Exited to \nNon-Permanent Destination",
  paste0("Active as of \n", ReportEnd()))
})

system_activity_prep <- reactive({
# browser()
  sys_inflow_outflow_plot_data()() %>% # this is a people-level df
    # filter(InflowTypeDetail != "something's wrong" &
    #          OutflowTypeDetail != "something's wrong") %>%
    pivot_longer(
      cols = c(InflowTypeDetail, OutflowTypeDetail), 
      names_to = "Time", 
      values_to = "Status") %>%
    group_by(Time, Status) %>%
    summarise(values = n()) %>%
    # filter(!is.na(Status)) %>%
    mutate(
      values = ifelse(Time == "OutflowTypeDetail", values * -1, values),
      inflow_outflow = Time,
      Time = case_when(
        Time == "InflowTypeDetail" &
          Status %in% c("Homeless", "Housed")
        ~ paste0("Active as of \n", ReportStart()),
        
        Time == "OutflowTypeDetail" &
          Status %in% c("Homeless", "Housed", "Unknown Status")
        ~ paste0("Active as of \n", ReportEnd()),
          
        Time == "InflowTypeDetail"
        ~ "Inflow",
        
        Time == "OutflowTypeDetail"
        ~ "Outflow"
      )
    )
})

# Combine the inflow types (newly homeless, returned from permanent, re-engaged
# into one group)
system_activity_summary_prep <- reactive({
  system_activity_prep() %>%
    mutate(
      Status = case_when(
        Time == "Inflow" &
        !(Status %in% c("Homeless", "Housed"))
        ~ "Inflow",
  
        Time == "Outflow" &
        !(Status %in% c("Homeless", "Housed", "Unknown Status"))
        ~ "Outflow",
  
        TRUE ~ Status
      )
    ) %>%
    group_by(Time, Status) %>%
    mutate(values = sum(values)) %>%
    ungroup() %>%
    unique()
})

# Rename the x-axis.var values to be the inflow and outflow types, 
# which are in Status
system_activity_detail_prep <- reactive({
  system_activity_prep() %>%
    mutate(
      Time = ifelse(
        !(Status %in% c("Homeless", "Housed", "Unknown Status")),
        Status,
        Time
      )
    )
})

prep_for_chart <- function(df, catvar_values, xvar_values) {
  # expand to make sure all combinations are included so the spacing is right
  # also factor, sort, and group for the chart
  df %>%
    right_join(
      expand.grid(Time = xvar_values,
                  Status = catvar_values),
      by = c("Time", "Status")) %>%
    replace_na(list(values = 0)) %>%
    mutate(
      Time = factor(Time, levels = xvar_values),
      Status = factor(Status, levels = catvar_values)
    ) %>%
    arrange(Time, desc(Status)) %>%
    group_by(Time) %>%
    mutate(group.id = cur_group_id()) %>%
    ungroup() %>%
    mutate(end.Bar = cumsum(values),
           start.Bar = c(0, head(end.Bar, -1))) %>%
    select(inflow_outflow, Time, Status, values, group.id, end.Bar, start.Bar)
}

renderSystemPlot <- function(id) {
  output[[id]] <- renderPlot({
    req(valid_file() == 1)
    
    if(id == "sys_act_summary_ui_chart") {
      colors <- c('#73655E', '#C6BDB9', '#ede7e3', '#C34931', '#16697A')
      df <- prep_for_chart(
        system_activity_summary_prep(),
        status_summary_values,
        time_summary_values()
      ) %>%
        filter(!is.na(inflow_outflow))
    } else {
      colors <-
        c(
          '#73655E',
          '#C6BDB9',
          '#ede7e3',
          '#C34931',
          '#C34931',
          '#C34931',
          '#C34931',
          '#16697A',
          '#16697A')
      df <- prep_for_chart(
        system_activity_detail_prep(),
        status_detail_values,
        time_detail_values()
      ) %>%
        filter(!is.na(inflow_outflow))
    }

    s <- max(df$end.Bar) + 20
    num_segments <- 20
    segment_size <- get_segment_size(s/num_segments)
browser()

# waterfall plot ----------------------------------------------------------
ggplot(df, aes(x = group.id, fill = Status)) +
  geom_rect( # the bars
    aes(
      xmin = group.id - 0.25,
      # control bar gap width
      xmax = group.id + 0.25,
      ymin = end.Bar,
      ymax = start.Bar
    ),
    colour = "#4e4d47",
    linewidth = .2,
    alpha = 0.8
  ) +
  geom_segment( # the connecting segments between bars
    data = df %>%
      filter(group.id == group.id) %>%
      group_by(group.id) %>% summarise(y = max(end.Bar)) %>%
      ungroup(),
    aes(
      x = group.id,
      xend = if_else(group.id == last(group.id), last(group.id), group.id + 1),
      y = y,
      yend = y
    ),
    linewidth = .3,
    colour = "gray25",
    linetype = "dashed",
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  ggrepel::geom_text_repel(# the labels
    aes(
      label = ifelse(
        !is.na(inflow_outflow) |
          as.character(Time) == as.character(Status),
        scales::comma(values),
        ""
      ),
      y = rowSums(cbind(start.Bar, values / 2)),
      segment.colour = "gray33"
    ),
    nudge_x = -.5,
    colour = "#4e4d47",
    # fontface = "bold",
    size = 6
  ) +
  scale_fill_manual(values = colors) + # color palette
  scale_y_continuous(expand = c(0,0)) + # distance between bars and x axis line
  scale_x_continuous(labels = df$Time %>% unique(), # x axis labels
                   breaks = df$group.id %>% unique()) +
  theme_void() + # totally clear all theme elements
  theme(# add back in what theme elements we want
    text = element_text(size = 16, colour = "#4e4d47"),
    axis.text.x = element_text(size = 16),
    axis.ticks.x = element_line(),
    axis.line.x = element_line(colour = "#4e4d47", linewidth = 0.5),
    axis.ticks.length.x = unit(.15, "cm"),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    legend.text = element_text(size = 16),
    legend.title = element_blank()
  )
  })
 # return(plotOutput(id, height = 400))
} 


# Plot prompts for plot subtitle ------------------------------------------

syso_detailBox <- reactive({
  # remove group names from race/ethnicity filter
  # so we can use getNameByValue() to grab the selected option label
  syso_race_ethnicities <- unlist(syso_race_ethnicity_cats())
  names(syso_race_ethnicities) <- gsub("Group [0-9]+\\.", "",
                                       names(syso_race_ethnicities))
  
  list(
    strong("Date Range: "),
    ReportStart(),
    " to ",
    ReportEnd(), 
    br(),
    strong("Household Type: "),
    # getNameByValue(syso_hh_types, input$syso_hh_type),
    " | ",
    strong("Level of Detail: "),
    # getNameByValue(syso_level_of_detail, input$syso_level_of_detail),
    " | ",
    strong("Project Type: "),
    # getNameByValue(syso_project_types, input$syso_project_type), 
    br(),
    strong("Age: "),
    # if_else(
    #   setequal(syso_age_cats, input$syso_age) |
    #     is.null(input$syso_age),
    #   "All Ages"#,
    #   # getNameByValue(syso_age_cats, input$syso_age)
    # ),
    " | ",
    strong("Gender: "),
    # getNameByValue(syso_gender_cats(), input$syso_gender),
    " | ",
    strong("Race/Ethnicity: "),
    # getNameByValue(syso_race_ethnicities, input$syso_race_ethnicity),
    " | ",
    strong("Special Populations: "),
    # getNameByValue(syso_spec_pops_cats(), input$syso_spec_pops), 
    br(),
    strong("Methodology Type: ")#,
    # getNameByValue(syso_methodology_types, input$methodology_type) 
  )
})

syso_chartSubheader <- reactive({
  list(
    strong(""), 
    # formatC(
    #   nrow(system_df_people_filtered()),
    #   format = "d",
    #   big.mark = ","
    # ),
    br()
  )
})
