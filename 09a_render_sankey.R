renderSankeyChart <- function(plot_data) {
  begin_labels <- plot_data %>%
    group_by(Begin) %>%
    summarize(freq = sum(freq)) %>%
    arrange(desc(Begin)) %>%
    mutate(label_pos = cumsum(freq) - freq/2,
           End = 0)
  
  end_labels <- plot_data %>%
    group_by(End) %>%
    summarize(freq = sum(freq)) %>%
    arrange(desc(End)) %>%
    mutate(label_pos = cumsum(freq) - freq/2,
           Begin = 0)
  
  # need to construct the Begin bars
  # will overlay on top of the Begin stratum
  # this way, if there's only one End gorup for a Begin group, the Begin bar
  # will remain gray, not take on the color of the End group
  plot_data <- plot_data %>%
    left_join(
      plot_data %>%
        group_by(Begin) %>%
        summarise(cumfreq = sum(freq)) %>%
        ungroup() %>%
        arrange(desc(Begin)) %>%
        mutate(
          ystart = lag(cumfreq, default = 0),
          yend = ystart + cumfreq
        ),
      by = "Begin"
    )
  
  colors <- c(
    "Housed" = "#73655E", 
    "Homeless" = "#C6BDB9",
    "Exited, Non-Permanent" = "#f0c9c1",
    "Enrolled, Homeless" = "#c2462e",
    "Inactive" = "#d5d1cf",
    "Exited, Permanent" = "#beeaf3",
    "Enrolled, Housed" = "#16697a"
  )

  return(
    ggplot(
      data = plot_data,
      aes(axis1 = Begin, axis2 = End, y = freq)
    ) +
    geom_alluvium(aes(fill = End, colour = End), reverse = TRUE) +
    geom_stratum(aes(fill = End), reverse = TRUE) +
    
    # construct the Begin bars
    geom_rect(
      aes(
        fill = Begin,
        xmin = 0.83,
        xmax = 1.17,
        ymin = ystart,
        ymax = yend
      )
    ) +
        
    #Color for End stratum and alluvial flows
    scale_fill_manual(values = colors) +
      
    #Color for alluvial flow borders
    scale_color_manual(values = colors) +

    # Numbers in bars
    geom_text(stat = "stratum",
              aes(label = after_stat(count)),
              size = 5,
              ) +
    
    # Bar (Text) Labels
    geom_text(
      data = begin_labels,
      aes(x = 1, y = label_pos, label = Begin), 
      hjust = 1,
      nudge_x = -0.2,
      size = 4
    ) +
    geom_text(
      data = end_labels,
      aes(x = 1, y = label_pos, label = End), 
      hjust = 0,
      nudge_x = 1.2,
      size = 4
    ) +
    
    # X Axis Labels
    scale_x_discrete(label = c("Period Start", "Period End"),
                     limits = c("Period Start", "Period End"),
                     expand = c(0.5, 0.5)) +
    
    # Total People
    annotate(
      geom = "text",
      x = 1.5,
      y = max(plot_data$yend) * 1.1,
      size = 16/.pt,
      label = sys_total_count_display(sum(plot_data$freq))
    ) +
      
    # remove legend, axis sizing
    theme_void() +
    theme(legend.position = "none",
         axis.text.x = element_text(color = "black", size = 17, vjust = 2.5))
  )
}