output$sankey_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  syso_detailBox() 
})

render_sankey_plot <- function(plot_data, isExport = FALSE) {
  begin_labels <- plot_data %>%
    fgroup_by(Begin) %>%
    fsummarize(freq = fsum(freq)) %>%
    roworder(-Begin) %>%
    fmutate(label_pos = fcumsum(freq) - freq/2,
           End = 0,
           Begin = glue("{freq} {Begin}"))
  
  end_labels <- plot_data %>%
    fgroup_by(End) %>%
    fsummarize(freq = fsum(freq)) %>%
    roworder(-End) %>%
    fmutate(label_pos = fcumsum(freq) - freq/2,
           Begin = 0,
           End = glue("{freq} {End}"))
  
  # need to construct the Begin bars
  # will overlay on top of the Begin stratum
  # this way, if there's only one End gorup for a Begin group, the Begin bar
  # will remain gray, not take on the color of the End group
  plot_data <- plot_data %>%
    join(
      plot_data %>%
        fgroup_by(Begin) %>%
        fsummarise(cumfreq = fsum(freq)) %>%
        fungroup() %>%
        roworder(-Begin) %>%
        fmutate(
          ystart = flag(cumfreq, fill = 0),
          yend = ystart + cumfreq
        ),
      on = "Begin", how = 'left'
    )
  
 
  cat_order <- unique(plot_data$End)
  
  ## set up pattern fills for Period End strata
  if('Inactive' %in% plot_data$End){
    stratum_bg <- sankey_bar_colors[names(sankey_bar_colors) %in% cat_order]
    stratum_fg <- c('black',sankey_bar_colors['Enrolled, Homeless'],sankey_bar_colors['Inactive'],'black',sankey_bar_colors['Enrolled, Housed'])
    stratum_patterns <- c(sankey_pattern,'','',sankey_pattern,'')
  } else {
    stratum_bg <- sankey_bar_colors[names(sankey_bar_colors) %in% cat_order]
    stratum_fg <- c('black',sankey_bar_colors['Enrolled, Homeless'],'black',sankey_bar_colors['Enrolled, Housed'])
    stratum_patterns <- c(sankey_pattern,'',sankey_pattern,'')
  }
 
  ggplot(
    data = plot_data,
    aes(axis1 = Begin, axis2 = End, y = freq)
  ) +
    geom_alluvium(aes(fill = End, colour = End), reverse = TRUE, alpha = 0.8) +
    geom_stratum(aes(fill = End)) +
    
    # construct the Begin bars
    geom_rect(
      aes(
        fill = Begin,
        xmin = 0.83,
        xmax = 1.17,
        ymin = ystart,
        ymax = yend
      ),
      colour ='black'
    ) +
    
    #Color for End stratum and alluvial flows
    scale_fill_manual(values = sankey_bar_colors) +

    #Color for alluvial flow borders
    scale_color_manual(values = sankey_border_colors) +
    
    # Bar (Text) Labels
    geom_text(
      data = begin_labels,
      aes(x = 1, y = label_pos, label = Begin), 
      hjust = 1,
      nudge_x = -0.2,
      size = sys_chart_text_font
    ) +
    geom_text(
      data = end_labels,
      aes(x = 1, y = label_pos, label = End), 
      hjust = 0,
      nudge_x = 1.2,
      size = sys_chart_text_font
    ) +
    
    ## use ggnewscale package to reset scales for pattern fill
    new_scale_fill() +
    
    ## add boxes at end with pattern fills
    geom_stratum(aes(fill=End)) +
    scale_fill_pattern(bg = stratum_bg, 
                       fg = stratum_fg, 
                       patterns = stratum_patterns, min_size = unit(1, 'mm')) +
    
    # X Axis Labels
    scale_x_discrete(label = c("Period Start", "Period End"),
                     limits = c("Period Start", "Period End"),
                     expand = c(0.5, 0.5)) +
    
    # Total People
    # annotate(
    #   geom = "text",
    #   x = 1.5,
    #   y = max(plot_data$yend) * 1.1,
    #   size = sys_chart_title_font,
    #   label = sys_total_count_display(sum(plot_data$freq))
    # ) +
    
    ggtitle(sys_total_count_display(sum(plot_data$freq))) +
    
    # remove legend, axis sizing
    theme_void() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        color = "black",
        size = get_adj_font_size(sys_axis_text_font, isExport),
        vjust = 2.5
      ),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
}
output$sankey_ui_chart <- renderPlot({
  logToConsole(session, "in sankey_ui_chart")
  req(session$userData$valid_file() == 1)
  
  plot_data <- get_sankey_data()
  
  validate(
    need(
      sum(plot_data$freq) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      sum(plot_data$freq) > 10,
      message = suppression_msg
    )
  )
  
  render_sankey_plot(plot_data)
},
alt = "A Sankey diagram of the end-of-year housing status of clients that were active in the homeless system at the start of the report period.",
width = ifelse(isTRUE(getOption("shiny.testmode")), 1113, "auto"))

sys_status_export_info <- function(spd) {
  tibble(
    Chart = c(
      "Total People",
      "Total Permanent at Period End",
      "Total Non-Permanent at Period End"
    ),
    Value = as.character(c(
      sum(spd$freq),
      sum(spd[spd$End %in% c("Exited, Permanent", "Enrolled, Homeless", "Inactive"), "freq"]),
      sum(spd[spd$End %in% c("Exited, Non-Permanent", "Enrolled, Housed"), "freq"])
    ))
  )
}

output$sys_status_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Status Report - "),
  content = function(file) {
    # create a list of the 3 excel tabs and export
    spd <- get_sankey_data() %>% 
      xtabs(freq ~ End + Begin, data=.) %>% 
      addmargins(FUN = sum) %>% 
      as.data.frame.matrix() %>%
      `rownames<-`(c(rownames(.)[-nrow(.)], "Total")) %>%
      `colnames<-`(c(colnames(.)[-ncol(.)], "Total")) %>%
      cbind("Status at Period End" = rownames(.), .) %>%
      select("Status at Period End", everything())
    
    tab_names <- list(
      "System Status Metadata" = sys_export_summary_initial_df() %>%
        bind_rows(sys_export_filter_selections()) %>%
        bind_rows(sys_status_export_info(get_sankey_data())) %>%
        rename("System Status" = Value),
      "System Status Detail" = spd
    )

    write_xlsx(
      tab_names,
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )

    exportTestValues(sys_status_report = get_sankey_data())
    
    logMetadata(session, paste0("Downloaded System Overview Tabular Data: ", input$syso_tabbox,
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  }
)

output$sys_status_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Status_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    sys_overview_ppt_export(
      file = file,
      title_slide_title = "Client System Status",
      summary_items = sys_export_summary_initial_df() %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(sys_export_filter_selections()) %>%
        bind_rows(sys_status_export_info(get_sankey_data())),
      plots = list(
        "Client System Status" = render_sankey_plot(get_sankey_data(), isExport=TRUE)
      ),
      summary_font_size = 21
    )
  }
)


# The universe is anyone who was Housed or Homeless at Period Start
# We also need the latest exit for the folks in the Exited categories
get_sankey_data <- reactive({
  logToConsole(session, "in get_sankey_data")
  full_data <- get_inflow_outflow_full()
  
  req(nrow(full_data) > 0)
  
  plot_df <- full_data[
    InflowTypeDetail %in% active_at_levels,
    .(PersonalID, InflowTypeDetail, OutflowTypeDetail)
  ]
  
  shinyjs::toggle(
    "sys_status_download_btn",
    condition = if(nrow(full_data) > 0) nrow(plot_df) > 10 else FALSE
  )
  shinyjs::toggle(
    "sys_status_download_btn_ppt",
    condition = if(nrow(full_data) > 0) nrow(plot_df) > 10 else FALSE
  )
  
  req(nrow(plot_df) > 0)

  allu <- plot_df %>%
    count(Begin = InflowTypeDetail, End = OutflowTypeDetail, name = "freq") %>%
    mutate(
      Begin = factor(Begin, levels = rev(active_at_levels)), # Or c("Housed", "Homeless") depending on desired order
      
      End = str_remove(End, "\n"), # Remove newlines
      End = ifelse( # Prepend "Enrolled, " for specific values
        End %in% active_at_levels,
        paste0("Enrolled, ", End),
        End
      ),
      
      End = factor(
        End,
        levels = c(
          "Exited, Non-Permanent",
          "Enrolled, Homeless",
          "Inactive",
          "Exited, Permanent",
          "Enrolled, Housed"
        )
      )
    )
  
  allu
})
