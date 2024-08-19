renderSankeyChart <- function(sankey_plot_data) {
  begin_labels <- sankey_plot_data %>%
    group_by(Begin) %>%
    summarize(freq = sum(freq)) %>%
    arrange(desc(Begin)) %>%
    mutate(label_pos = cumsum(freq) - freq/2,
           End = 0)
  
  end_labels <- sankey_plot_data %>%
    group_by(End) %>%
    summarize(freq = sum(freq)) %>%
    arrange(desc(End)) %>%
    mutate(label_pos = cumsum(freq) - freq/2,
           Begin = 0)

  return(
    ggplot(data = sankey_plot_data,
         aes(axis1 = Begin,
             axis2 = End,
             y = freq)) +
    geom_alluvium(aes(fill = End, colour = End),
                  reverse=TRUE) +
    geom_stratum(aes(fill = End), reverse = TRUE) +
    
    #Color for End stratum and alluvial flows
    scale_fill_manual(
      values = c(
        "Exited, Permanent" = "#beeaf3",
        "Enrolled, Housed" = "#16697a",
        "Inactive" = "#d5d1cf",
        "Exited, Non-Permanent" = "#f0c9c1",
        "Enrolled, Homeless" = "#c2462e"),
      na.value = c("#aba39e", "#73655E")) +
    
    #Color for alluvial flow borders
    scale_color_manual(
      values = c(
        "Exited, Permanent" = "#beeaf3",
        "Enrolled, Housed" = "#16697a",
        "Inactive" = "#d5d1cf",
        "Exited, Non-Permanent" = "#f0c9c1",
        "Enrolled, Homeless" = "#c2462e")) +
    
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
    
    # remove legend, axis sizing
    theme_void() +
    theme(legend.position = "none",
         axis.text.x = element_text(color = "black", size = 17, vjust = 2.5))
  )
}