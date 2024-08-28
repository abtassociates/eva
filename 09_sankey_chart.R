# The universe is anyone who was Housed or Homeless at Period Start
# We also need the latest exit for the folks in the Exited categories
plot_data <- reactive({
  
  plot_df <- sys_inflow_outflow_plot_data() %>%
    filter(InflowTypeDetail == "Housed" | InflowTypeDetail == "Homeless") %>%

  startBind <- plot_df %>%
    select(PersonalID, "Type" = InflowTypeDetail) %>%
    mutate("Period" = "Begin")
  
  endBind <- plot_df %>%
    select(PersonalID, "Type" = OutflowTypeDetail) %>%
    mutate("Period" = "End")
    
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