# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
# Define the hardcoded values for Time and Status
# we need all combinations for the 0s

status_summary_values <- c(
  "Homeless", 
  "Housed", 
  "Inflow", 
  "Outflow"
)

status_detail_values <- c(
  "Homeless", 
  "Housed", 
  "Newly Homeless", 
  "Returned from \nPermanent", 
  "Re-engaged from \nNon-Permanent",
  "Continued system \nengagement",
  "Exited to \nPermanent Destination",
  "Exited to \nNon-Permanent Destination"
)

time_summary_values <- c(
    paste0("Active as of \n", ReportStart()),
    "Inflow",
    "Outflow",
    paste0("Active as of \n", ReportEnd())
  )

time_detail_values <- c(
    paste0("Active as of \n", ReportStart()),
    "Newly Homeless",
    "Returned from \nPermanent",
    "Re-engaged from \nNon-Permanent",
    "Continued system \nengagement",
    "Exited to \nPermanent Destination",
    "Exited to \nNon-Permanent Destination",
    paste0("Active as of \n", ReportEnd())
  )

system_activity_prep <- reactive({

  sys_inflow_outflow_plot_data()() %>% # this is a people-level df
    filter(InflowTypeDetail != "something's wrong" &
             OutflowTypeDetail != "something's wrong") %>%
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
          Status %in% c("Homeless", "Housed")
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
        !(Status %in% c("Homeless", "Housed"))
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
        !(Status %in% c("Homeless", "Housed")),
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
      colors <- c('#73655E','#C6BDB9','#C34931', '#16697A')
      df <- prep_for_chart(
        system_activity_summary_prep(),
        status_summary_values,
        time_summary_values
      )
    } else {
      colors <-
        c('#73655E',
          '#C6BDB9',
          '#C34931',
          '#C34931',
          '#C34931',
          '#C34931',
          '#16697A',
          '#16697A')
      df <- prep_for_chart(
        system_activity_detail_prep(),
        status_detail_values,
        time_detail_values
      )
    }

    s = max(df$end.Bar) + 20
    num_segments <- 20
    segment_size <- get_segment_size(s/num_segments)

    ggplot(df, aes(x = group.id, fill = Status)) + 
      # \_Simple Waterfall Chart ----
      geom_rect(aes(xmin = group.id - 0.25, # control bar gap width
                      xmax = group.id + 0.25, 
                      ymin = end.Bar,
                      ymax = start.Bar),
                color = "black",
                alpha = 0.8
      ) + 
      # \_Lines Between Bars ----
      geom_segment(aes(
        x = ifelse(group.id == last(group.id),
                   last(group.id),
                   group.id + 0.25),
        xend = ifelse(group.id == last(group.id),
                      last(group.id),
                      group.id + 0.75),
        y = ifelse(Status == last(Status),
                   end.Bar,
                    # these will be removed once we set the y limits
                   s + segment_size),
        yend = ifelse(Status == last(Status),
                      end.Bar,
                    # these will be removed once we set the y limits
                    s + segment_size),
        colour = "black"
      ), show.legend = FALSE) +
      # \_Numbers inside bars (each category) ----
      geom_text(
          mapping = aes(
            label = ifelse(
              !is.na(inflow_outflow) |
                as.character(Time) == as.character(Status),
              scales::comma(values),
              ""
            ),
            y = ifelse(abs(values) < segment_size / 4,
                       end.Bar + 10,
                       rowSums(cbind(start.Bar, values / 2)))
          ),
          color = "black",
          fontface = "bold",
          size = 5
      ) +
      # \_Change colors ----
      scale_fill_manual(values = colors) +
      # \_Change y axis to same scale as original ----
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, s + segment_size / 2)
      ) +
      # \_Add tick marks on x axis to look like the original plot ----
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(min(df$group.id) - 0.4, max(df$group.id) + 0.4),
        breaks = c(min(df$group.id) - 0.4,
                   unique(df$group.id),
                   unique(df$group.id) + 0.4),
        labels =
          c("",
            as.character(unique(df$Time)),
            rep(c(""), length(unique(df$Time))))
      ) +
      # \_Theme options to make it look like the original plot ----
    theme(
      text = element_text(size = 14, color = "#4e4d47"),
      axis.text = element_text(
        size = 10,
        color = "#4e4d47",
        face = "bold"
      ),
      axis.text.x = element_text(vjust = 1),
      axis.ticks.x = 
        element_line(color =
                       c("black",
                         rep(NA, length(
                           unique(df$Time)
                         )),
                         rep("black", length(
                           unique(df$Time)
                         ) - 1))),
      axis.line = element_line(colour = "#4e4d47", linewidth = 0.5),
      axis.ticks.length = unit(.15, "cm"),
      axis.title.x =      element_blank(),
      # hide y axis
      axis.title.y =      element_blank(),
      axis.ticks.y =      element_blank(),
      axis.line.y =       element_blank(),
      axis.text.y =       element_blank(),
      panel.background =  element_blank(),
      panel.border    =   element_blank(),
      panel.grid.major =  element_blank(),
      panel.grid.minor =  element_blank(),
      plot.background =   element_blank(),
      plot.margin =       unit(c(1, 1, 1, 1), "lines"),
      legend.text =       element_text(
        size = 10,
        color = "#4e4d47",
        face = "bold",
        margin = margin(l = 0.25, unit = "cm"
        )
      ),
      legend.title =       element_blank()
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
    input$syso_date_range[1],
    " to ",
    input$syso_date_range[2], 
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
