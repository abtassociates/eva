# The universe is anyone who was Housed or Homeless at Period Start
# We also need the latest exit for the folks in the Exited categories
plot_data <- reactive({
  plot_df <- sys_df_universe() %>%
    filter(InflowTypeDetail == "Housed" | InflowTypeDetail == "Homeless") %>%
    group_by(PersonalID) %>%
    filter(max(ExitAdjust) == ExitAdjust) %>%
    ungroup() %>%
    select(PersonalID, InflowTypeDetail, OutflowTypeDetail, Destination, EnrollmentID, ExitAdjust) %>%
    unique()
    
  # if any enrollments share the same exit date, dedup them using the following logic
    # 1. take permanent over temp over other
    # 2. within a destination type (perm, temp, oother) take the lower destination value
    # 3. within the same destination number, take the highest enrollment ID
  if(any(duplicated(plot_df$PersonalID))) {
    destination_categories <- c("Permanent" = 1, "Temp" = 2, "Other" = 3)
    plot_df <- plot_df %>%
      group_by(PersonalID) %>%
      mutate(
        DestinationCategory = factor(
          case_when(
            Destination %in% perm_livingsituation ~ 1,
            Destination %in% c(
              temp_livingsituation,
              institutional_livingsituation,
              homeless_livingsituation_incl_TH
            ) ~ 2,
            TRUE ~ 3
          ), 
          levels = destination_categories,
          labels = names(destination_categories)
        )
      ) %>%
      arrange(DestinationCategory, Destination, desc(EnrollmentID)) %>%
      slice(1) %>%
      ungroup()
  }

  startBind <- plot_df %>%
    select(PersonalID, "Type" = InflowTypeDetail) %>%
    mutate("Period" = "Begin")
  
  endBind <- plot_df %>%
    mutate(
      "Period" = "End",
      "Type" = case_when(
        OutflowTypeDetail == "Housed" ~ "Enrolled, Housed",
        OutflowTypeDetail == "Homeless" ~ "Enrolled, Homeless",
        Destination %in% perm_livingsituation &
          ExitAdjust <= ReportEnd() ~ "Exited, Permanent",
        !(Destination %in% perm_livingsituation) &
          ExitAdjust <= ReportEnd() ~ "Exited, Non-Permanent",
        TRUE ~ OutflowTypeDetail
      )
    ) %>%
    select(PersonalID, Period, Type)
  
    
  allBind <- rbind(startBind, endBind)
  
  #Create df with both Homeless and Housed at start
  d_hh <- data.frame(cbind(startBind$Type, endBind$Type))
  names(d_hh) <- c("Period Start", "Period End")
  
  #Basic alluvial chart - both homeless and housed at start
  allu <- d_hh %>%
    group_by(d_hh$`Period Start`, d_hh$`Period End`) %>%
    summarise(Freq = n())
  
  names(allu) <- c("Begin", "End", "freq")
  
  #Convert statuses as factors and re-order levels
  allu$Begin <- factor(allu$Begin, levels = c("Homeless",
                                              "Housed"))
  
  allu$End <- factor(
    allu$End,
    levels = c(
      "Exited, Non-Permanent",
      "Enrolled, Homeless",
      "Inactive",
      "Exited, Permanent",
      "Enrolled, Housed"
    )
  )
  allu
})