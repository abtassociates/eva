plot_data <-sys_inflow_outflow_plot_data()()

startBind <- plot_data %>%
  filter(InflowTypeDetail %in% c("Homeless", "Housed")) %>%
  select(PersonalID, "Type" = InflowTypeDetail) %>%
  mutate("Period" = "Begin")

endBind <- plot_data %>%
  filter(InflowTypeDetail %in% c("Homeless", "Housed")) %>%
  mutate(
    "Period" = "End",
    "Type" = case_when(
      unknown_at_end_client == TRUE ~ "Inactive", 
      OutflowTypeDetail == "Housed" ~ "Enrolled, Housed",
      OutflowTypeDetail == "Homeless" ~ "Enrolled, Homeless",
      OutflowTypeDetail == "Exited to \nPermanent Destination" ~ "Exited, Permanent",
      OutflowTypeDetail == "Exited to \nNon-Permanent Destination" ~ "Exited, Non-Permanent"
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

sankey_plot_data(allu)