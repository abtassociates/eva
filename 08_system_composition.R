syso_race_ethnicities <- unlist(syso_race_ethnicity_cats())
names(syso_race_ethnicities) <- gsub("Group [0-9]+\\.", "",
                                     names(syso_race_ethnicities))

get_v_cats <- function(v) {
  return(
    switch(v,
      "Gender" = syso_gender_cats(), 
      "Age" = syso_age_cats, 
      "RaceEthnicity" = syso_race_ethnicities, 
    )
  )
}

get_sys_comp_plot_df <- function(vars) {
  cats <- lapply(vars, get_v_cats)
  names(cats) <- vars
  
  return(
    system_df_people_universe_filtered() %>%
      select(PersonalID, Age, Gender, RaceEthnicity, VeteranStatus) %>%
      count(!!!syms(vars)) %>%
      filter(n > 10) %>%
      mutate(pct = prop.table(n)) %>%
      right_join(
        do.call(expand.grid, cats),
        by = vars
      )
  )
}

sys_comp_plot <- function(vars) {
  req(length(vars) == 2)
  
  # remove the / from Race/Ethnicity, since the varname is RaceEthnicity
  vars <- str_remove(vars, "/")
  v1 <- vars[1]
  v2 <- vars[2]
  
  cats <- lapply(vars, get_v_cats)
  
  plot_df <- get_sys_comp_plot_df(vars)
  plot_df[[v1]] <- factor(plot_df[[v1]], levels = cats[[1]])
  plot_df[[v2]] <- factor(plot_df[[v2]], levels = cats[[2]])
  
  h_total <- plot_df %>% 
    group_by(!!!syms(v2)) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!v1 := 'Total')
  
  v_total <- plot_df %>% 
    group_by(!!!syms(v1)) %>% 
    summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>% 
    mutate(!!v2 := 'Total')

  font_size <- 10/.pt
  
  return(
    ggplot(plot_df, aes(.data[[v1]], .data[[v2]])) +   
    # main data into cells for each cross-combination
    geom_tile(
      color = 'white',
      lwd = 0.5,
      linetype = 1,
      aes(fill = n)) +
    scale_fill_gradient(low = "#D2E3D9", 
                        high = "#084954",
                        na.value = 'white') + # na.value makes 0s invisible
    # display like 14/(0.8%)
    # set text color to be 508 compliant contrasting
    geom_text(
      aes(label = paste0(n, "\n", "(",scales::percent(pct, accuracy = 0.1),")")), 
      size = font_size,
      color = ifelse(
        plot_df$n > mean(plot_df$n,na.rm=TRUE),
        'white', 
        'black'
      )
    ) +
    
    # Row totals
    ggnewscale::new_scale("fill") +
    geom_tile(data = h_total,
              color = "white",
              lwd = 0.5,
              linetype = 1,
              aes(fill = N)) +

    scale_fill_gradient(low = "#ede7e3", high = "#73655e", na.value = 'white') +

    geom_text(
      aes(label= ifelse(
        is.na(N),
        "",
        paste0(N, "\n", "(",scales::percent(N/sum(N, na.rm=TRUE), accuracy = 0.1),")")
      )), 
      size = font_size,
      color = ifelse(
        h_total$N > mean(h_total$N,na.rm=TRUE),
        'white', 
        'black'
      ),
      data= h_total
    ) +

    # column totals
    ggnewscale::new_scale("fill") +
    geom_tile(data = v_total,
              color = "white",
              lwd = 0.5,
              linetype = 1,
              aes(fill = N)) +
    scale_fill_gradient(low = "#ede7e3", high = "#73655e", na.value = 'white') +
      
    geom_text(
      aes(label= ifelse(
        is.na(N),
        "",
        paste0(N, "\n", "(",scales::percent(N/sum(N, na.rm=TRUE), accuracy = 0.1),")")
      )),
      size = font_size,
      color = ifelse(
        v_total$N > mean(v_total$N,na.rm=TRUE),
        'white', 
        'black'
      ),
      data= v_total
    ) +
      
    # axis labels
    scale_x_discrete(
      labels = str_wrap(c(names(cats[[1]]), "Total"), width=20),
      limits = c(levels(plot_df[[v1]]), "Total"),
      position = "top"
    ) +
    scale_y_discrete(
      labels = str_wrap(c("Total", names(cats[[2]])), width=30),
      limits = c("Total", rev(levels(plot_df[[v2]])))
    ) +
      
    # other stuff
    theme_bw() +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_text(v1),
          axis.title.y = element_text(v2),
          axis.text = element_text(size = 13))
  )
}
