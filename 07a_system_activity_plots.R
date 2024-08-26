# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
# Define the hardcoded values for Time and Status
# we need all combinations for the 0s

frame_detail <- 
  data.frame(
    Status = c("Homeless",
               "Housed",
               "Newly Homeless",
               "Returned from \nPermanent",
               "Re-engaged from \nNon-Permanent",
               "Exited to \nPermanent Destination",
               "Exited to \nNon-Permanent Destination",
               "Inactive",
               "Homeless",
               "Housed"),
    Time = c(rep("Active at Start", 2),
             rep("Inflow", 3),
             rep("Outflow", 3),
             rep("Active at End", 2)),
    InflowOutflow = c(rep("Inflow", 5), rep("Outflow", 5)))

frame_summary <-
  data.frame(
    Status = c("Homeless",
               "Housed",
               "Inflow",
               "Outflow",
               "Homeless",
               "Housed"),
    Time = c(rep(paste0("Active at Start"), 2),
             "Inflow",
             "Outflow",
             rep(paste0("Active at End"), 2)),
    InflowOutflow = c(rep("Inflow", 3), rep("Outflow", 3))
  )

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
  
  outflow <- sys_inflow_outflow_plot_data()() %>%
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
    full_join(outflow, join_by(Time, values, Status, InflowOutflow)) %>%
    mutate(
      Time = factor(
        Time,
        levels = c("Active at Start",
                   "Inflow",
                   "Outflow",
                   "Active at End")
      ),
      Status = factor(
        Status,
        levels = c(
          "Homeless",                          
          "Housed",
          "Newly Homeless",
          "Returned from \nPermanent",
          "Re-engaged from \nNon-Permanent",
          "Exited to \nNon-Permanent Destination",
          "Exited to \nPermanent Destination",
          "Inactive"
        )
      )
    ) %>%
    arrange(Time, Status) %>%
    group_by(Time) %>%
    mutate(group.id = cur_group_id()) %>%
    ungroup() %>%
    mutate(
      values = ifelse(InflowOutflow == "Outflow", values * -1, values),
      ystart = lag(cumsum(values), default = 0),
      yend = round(cumsum(values))
    )
  
  
  
})

system_activity_prep_summary <- reactive({
  # browser()
  prep <- sys_inflow_outflow_plot_data() %>% # this is a people-level df
    mutate(
      InflowSummaryMatrix = case_when(
        InflowTypeSummary == "Active at Start" & InflowTypeDetail == "Homeless" ~
          "Homeless",
        InflowTypeSummary == "Active at Start" & InflowTypeDetail == "Housed" ~
          "Housed",
        TRUE ~ InflowTypeSummary
      ),
      OutflowSummaryMatrix = case_when(
        OutflowTypeSummary == "Active at End" & OutflowTypeDetail == "Homeless" ~
          "Homeless",
        OutflowTypeSummary == "Active at End" & OutflowTypeDetail == "Housed" ~
          "Housed",
        TRUE ~ OutflowTypeSummary
      )
    )
  
  inflow <- prep %>%
    select(PersonalID,
           InflowTypeSummary,
           InflowTypeDetail,
           InflowSummaryMatrix) %>%
    group_by(InflowSummaryMatrix) %>%
    summarise(values = n()) %>%
    ungroup() %>%
    rename("Status" = InflowSummaryMatrix) %>%
    mutate(
      Time = if_else(Status != "Inflow",
                     "Active at Start",
                     Status))
  
  outflow <- prep %>%
    select(PersonalID,
           OutflowTypeSummary,
           OutflowTypeDetail,
           OutflowSummaryMatrix) %>%
    group_by(OutflowSummaryMatrix) %>%
    summarise(values = n()) %>%
    ungroup() %>%
    rename("Status" = OutflowSummaryMatrix) %>%
    mutate(
      Time = if_else(Status != "Outflow",
                     "Active at End",
                     Status))
  
  inflow %>%
    full_join(outflow, join_by(Status, values, Time)) %>%
    full_join(frame_summary, join_by(Status, Time)) %>%
    mutate(
      values = replace_na(values, 0),
      Time = factor(
        Time,
        levels = c("Active at Start",
                   "Inflow",
                   "Outflow",
                   "Active at End")
      )
    ) %>%
    arrange(Time) %>%
    group_by(Time) %>%
    mutate(group.id = cur_group_id()) %>%
    ungroup() %>%
    mutate(
      values = ifelse(Time %in% c("Outflow", "Active at End"), values * -1, values),
      ystart = lag(cumsum(values), default = 0),
      yend = round(cumsum(values))
    )
})

renderSystemPlot <- function(id) {
  output[[id]] <- renderPlot({
    req(valid_file() == 1)
    # browser()
    if (id == "sys_act_summary_ui_chart") {
      colors <- c('#73655E', '#C6BDB9', '#C34931', '#16697A')
      df <- system_activity_prep_summary()
    } else {
      colors <- c(
        '#73655E',
        '#C6BDB9',
        "#e5a699",
        '#b7452e',
        "#66261a",
        '#93dcec',
        "#3dc1dc",
        '#1b8297'
      )
         df <- system_activity_prep_detail()
       }
       
    s <- max(df$yend) + 20
    num_segments <- 20
    segment_size <- get_segment_size(s/num_segments)

# waterfall plot ----------------------------------------------------------
ggplot(df, aes(x = group.id, fill = Status)) +
  geom_rect( # the bars
    aes(
      xmin = group.id - 0.25,
      # control bar gap width
      xmax = group.id + 0.25,
      ymin = ystart,
      ymax = yend
    ),
    colour = "#4e4d47",
    linewidth = .2,
    alpha = 0.8
  ) +
  geom_segment( # the connecting segments between bars
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
  ggrepel::geom_text_repel(# the labels
    aes(
      x = group.id,
      label = paste0(scales::comma(abs(values))),
      y = rowSums(cbind(ystart, values / 2)),
      segment.colour = "gray33"
    ),
    nudge_x = -.5,
    arrow = arrow(type = "open", length = unit(.1, "inches")),
    colour = "#4e4d47",
    # alpha = .7,
    size = 5,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = colors) + # color palette
  scale_y_continuous(expand = c(0,0)) + # distance between bars and x axis line
  scale_x_continuous(labels = str_wrap(df$Time %>% unique(), width = 10), # x axis labels
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
    legend.title = element_blank()#,
    # legend.position = "none"
  )
  })
 # return(plotOutput(id, height = 400))
} 


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
    strong("Date Range: "),
    
    ReportStart(), " to ", ReportEnd(), br(),
    
    if (getNameByValue(syso_hh_types, input$syso_hh_type) != "All People")
      detail_line("Household Type", syso_hh_types, input$syso_hh_type),
    
    detail_line("Level of Detail", syso_level_of_detail, input$syso_level_of_detail),
    
    if (getNameByValue(syso_project_types, input$syso_project_type) != "All")
      detail_line("Project Type", syso_project_types, input$syso_project_type),
    
    detail_line("Methodology Type", syso_methodology_types, input$methodology_type),
    
    if (length(input$syso_age) != length(syso_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syso_age, collapse = ', ')} <br>"
      )),
    
    if (length(input$syso_gender) != length(syso_gender_cats(input$methodology_type)))
      detail_line("Gender", syso_gender_cats(input$methodology_type), input$syso_gender),
    
    if (selected_race != "All.All Races/Ethnicities")
      race_ethnicity_line,
    
    if(getNameByValue(syso_spec_pops_people, input$syso_spec_pops) != "None")
      detail_line("Special Populations", syso_spec_pops_people, input$syso_spec_pops)
    
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
