profile_script("server_profile.R", profile_open = tictoc::tic(msg = glue::glue("{.lo}-{.lc}")), profile_close = tictoc::toc(log = TRUE))
timings <- readRDS("profvis/ttlog.rds")

dplyr::bind_rows(timings) %>%
  dplyr::rename(Message = msg) %>% 
  dplyr::mutate(Duration = toc - tic) %>% 
  dplyr::group_by(Message) %>% 
  dplyr::summarise(Duration = mean(Duration)) %>% 
  dplyr::filter(Duration > (2.5 * stats::IQR(Duration))) %>%
  dplyr::arrange(dplyr::desc(Duration)) %>% 
  dplyr::mutate_at(dplyr::vars(Message), ~stringr::str_remove(., "lines")) %>% 
  {ggplot2::ggplot(., mapping = ggplot2::aes(x = reorder(Message, order(Duration, decreasing = TRUE)))) +
  ggplot2::geom_col(ggplot2::aes(y = Duration), fill = "orange", alpha = .3) +
  ggplot2::scale_y_continuous(breaks = seq(min(.$Duration), max(.$Duration), by = stats::IQR(.$Duration)), minor_breaks = seq(min(.$Duration), max(.$Duration), by = .25)) +
  ggplot2::labs(title = "Rminor Timings",
                subtitle = "",
                caption = "",
                x = "Lines", 
                y = "Duration (s)") +
  ggplot2::theme(
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.text.x = element_text(angle = 45))}
