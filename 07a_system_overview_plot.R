# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
# Define the hardcoded values for x.axis.var and cat.var
# we need all combinations for the 0s
x.axis.var_summary_values <- reactive({
  c(
    paste0("Active as of ",input$syso_date_range[1]),
    "Inflow",
    "Outflow",
    paste0("Active as of ",input$syso_date_range[2])
  )
})

x.axis.var_detail_values <- reactive({
  c(
    paste0("Active as of ",input$syso_date_range[1]), 
    "Newly Homeless", 
    "Returned from Permanent", 
    "Re-engaged from Temporary or Unknown",
    "Permanent Destination",
    "Temporary/Unknown Destination",
    paste0("Active as of ",input$syso_date_range[2])
  )
})

cat.var_summary_values <- c(
  "Enrolled: Homeless", 
  "Enrolled: Housed", 
  "Inflow", 
  "Outflow"
)

cat.var_detail_values <- c(
  "Enrolled: Homeless", 
  "Enrolled: Housed", 
  "Newly Homeless", 
  "Returned from Permanent", 
  "Re-engaged from Temporary or Unknown",
  "Permanent Destination",
  "Temporary/Unknown Destination"
)

system_activity_prep <- reactive({
  system_df_filtered() %>%
    pivot_longer(
      cols = c(InflowType, OutflowType), 
      names_to = "x.axis.var", 
      values_to = "cat.var") %>%
    group_by(x.axis.var, cat.var) %>%
    summarise(values = n()) %>%
    filter(!is.na(cat.var)) %>%
    mutate(
      values = ifelse(x.axis.var == "OutflowType", values*-1, values),
      inflow_outflow = x.axis.var,
      x.axis.var = case_when(
        x.axis.var == "InflowType" &
          cat.var %in% c("Enrolled: Homeless", "Enrolled: Housed")
        ~ paste0("Active as of ",input$syso_date_range[1]),
        
        x.axis.var == "OutflowType" &
          cat.var %in% c("Enrolled: Homeless", "Enrolled: Housed")
        ~ paste0("Active as of ",input$syso_date_range[2]),
          
        x.axis.var == "InflowType"
        ~ "Inflow",
        
        x.axis.var == "OutflowType"
        ~ "Outflow"
      )
    )
})

# Combine the inflow types (newly homeless, returned from permanent, re-engaged
# into one group)
system_activity_summary_prep <- reactive({
  system_activity_prep() %>%
    mutate(
      cat.var = case_when(
        x.axis.var == "Inflow" &
        !(cat.var %in% c("Enrolled: Homeless", "Enrolled: Housed"))
        ~ "Inflow",
  
        x.axis.var == "Outflow" &
        !(cat.var %in% c("Enrolled: Homeless", "Enrolled: Housed"))
        ~ "Outflow",
  
        TRUE ~ cat.var
      )
    ) %>%
    group_by(x.axis.var, cat.var) %>%
    mutate(values = sum(values)) %>%
    ungroup() %>%
    unique()
})

# Rename the x-axis.var values to be the inflow and outflow types, 
# which are in cat.var
system_activity_detail_prep <- reactive({
  system_activity_prep() %>%
    mutate(
      x.axis.var = ifelse(
        !(cat.var %in% c("Enrolled: Homeless", "Enrolled: Housed")),
        cat.var,
        NA
      )
    )
})

prep_for_chart <- function(df, catvar_values, xvar_values) {
  # expand to make sure all combinations are included so the spacing is right
  # also factor, sort, and group for the chart
  df %>%
    right_join(
      expand.grid(x.axis.var = xvar_values,
                  cat.var = catvar_values),
      by = c("x.axis.var", "cat.var")) %>%
    replace_na(list(values=0)) %>%
    mutate(
      x.axis.var = factor(x.axis.var, levels = xvar_values),
      cat.var = factor(cat.var, levels = catvar_values)
    ) %>%
    arrange(x.axis.var, desc(cat.var)) %>%
    group_by(x.axis.var) %>%
    mutate(group.id = cur_group_id()) %>%
    ungroup() %>%
    mutate(end.Bar = cumsum(values),
           start.Bar = c(0, head(end.Bar, -1))) %>%
    select(inflow_outflow, x.axis.var, cat.var, values, group.id, end.Bar, start.Bar)
}

renderSystemPlot <- function(id) {
  req(valid_file() == 1)
  
  output[[id]] <- renderPlot({
    if(id == "sys_act_summary_ui_chart") {
      colors <- c('#73655E','#C6BDB9','#C34931', '#16697A')
      df <- prep_for_chart(
        system_activity_summary_prep(),
        cat.var_summary_values,
        x.axis.var_summary_values()
      )
    } else {
      colors <- c('#73655E','#C6BDB9','#C34931', '#C34931', '#C34931', '#16697A', '#16697A')
      df <- prep_for_chart(
        system_activity_detail_prep(),
        cat.var_detail_values,
        x.axis.var_detail_values()
      )
    }

    s = max(df$end.Bar)+20
    num_segments <- 20
    segment_size <- get_segment_size(s/num_segments)
    
    ggplot(df, aes(x = group.id, fill = cat.var)) + 
      # \_Simple Waterfall Chart ----
      geom_rect(aes(xmin = group.id - 0.25, # control bar gap width
                      xmax = group.id + 0.25, 
                      ymin = end.Bar,
                      ymax = start.Bar),
                  color="black", 
                  alpha=0.95
      ) + 
      # \_Lines Between Bars ----
      geom_segment(aes(
        x=ifelse(group.id == last(group.id),
                                  last(group.id),
                                  group.id+0.25), 
        xend=ifelse(group.id == last(group.id),
                    last(group.id),
                    group.id+0.75), 
        y=ifelse(cat.var == last(cat.var),
                    end.Bar,
                    # these will be removed once we set the y limits
                    s+500), 
        yend=ifelse(cat.var == last(cat.var),
                    end.Bar,
                    # these will be removed once we set the y limits
                    s+500),
        colour="black"
      ), show.legend = FALSE) +
      # \_Numbers inside bars (each category) ----
      geom_text(
          mapping = aes(
            label = ifelse(
              !is.na(inflow_outflow) | 
                as.character(x.axis.var) == as.character(cat.var),
              as.character(values),
              ""),
            y = ifelse(
              abs(values) < 150, 
              end.Bar + 10, 
              rowSums(cbind(start.Bar,values/2))
            )
          ),
          color = "black",
          fontface = "bold",
          size=4
      ) +
      # \_Change colors ----
      scale_fill_manual(values = colors) +
      # \_Change y axis to same scale as original ----
      scale_y_continuous(
          expand=c(0,0),
          limits = c(0, s+200)
      ) +
      # \_Add tick marks on x axis to look like the original plot ----
      scale_x_continuous(
          expand=c(0,0),
          limits = c(min(df$group.id)-0.4,max(df$group.id)+0.4),
          breaks = c(min(df$group.id)-0.4,
                  unique(df$group.id), 
                  unique(df$group.id) + 0.4
                  ),
          labels = 
          c("", 
              as.character(unique(df$x.axis.var)), 
              rep(c(""), length(unique(df$x.axis.var)))
          )
      ) +
      # \_Theme options to make it look like the original plot ----
      theme(
          text = element_text(size = 14, color = "#4e4d47"),
          axis.text = element_text(size = 10, color = "#4e4d47", face = "bold"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.ticks.x = element_line(
              color =
                  c("black",
                  rep(NA, length(unique(df$x.axis.var))),
                  rep("black", length(unique(df$x.axis.var))-1)
                  )
          ),
          axis.line = element_line(colour = "#4e4d47", linewidth = 0.5),
          axis.ticks.length = unit(.15, "cm"),
          axis.title.x =      element_blank(),
          # hide y axis
          axis.title.y =      element_blank(),
          axis.ticks.y =      element_blank(),
          axis.line.y =      element_blank(),
          axis.text.y =      element_blank(),
          panel.background =  element_blank(),
          panel.border    =   element_blank(),
          panel.grid.major=   element_blank(),
          panel.grid.minor=   element_blank(),
          plot.background=    element_blank(),
          plot.margin =        unit(c(1, 1, 1, 1), "lines"),
          legend.text =        element_text(size = 10, 
                                          color = "#4e4d47",
                                          face = "bold",
                                          margin = margin(l = 0.25, unit = "cm")
                                          ),
          legend.title =       element_blank()
      )
  })
 # return(plotOutput(id, height = 400))
}