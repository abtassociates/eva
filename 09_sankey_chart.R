# The universe is anyone who was Housed or Homeless at Period Start
# We also need the latest exit for the folks in the Exited categories
plot_data <- reactive({
  plot_df <- sys_df_universe() %>%
    filter(InflowTypeDetail == "Housed" | InflowTypeDetail == "Homeless") %>%
    group_by(PersonalID) %>%
    mutate(
      lastExit = max(ExitAdjust) == ExitAdjust
    ) %>%
    filter(max(lastExit) == lastExit) %>%
    ungroup() %>%
    select(PersonalID, InflowTypeDetail, OutflowTypeDetail, lastExit, ExitAdjust, Destination) %>%
    unique()

  startBind <- plot_df %>%
    select(PersonalID, "Type" = InflowTypeDetail) %>%
    mutate("Period" = "Begin")
  
  endBind <- plot_df %>%
    mutate(
      "Period" = "End",
      "Type" = case_when(
        OutflowTypeDetail == "Housed" ~ "Enrolled, Housed",
        OutflowTypeDetail == "Homeless" ~ "Enrolled, Homeless",
        lastExit == TRUE & 
          Destination %in% perm_livingsituation &
          ExitAdjust <= ReportEnd() ~ "Exited, Permanent",
        lastExit == TRUE & 
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
  allu$Begin <- factor(allu$Begin, levels = c("Housed",
                                              "Homeless"))
  
  allu$End <- factor(allu$End, levels = c("Exited, Permanent",
                     "Enrolled, Housed",
                     "Inactive",
                     "Exited, Non-Permanent",
                     "Enrolled, Homeless"))
  allu
})