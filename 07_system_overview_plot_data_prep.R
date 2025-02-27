# Client-level enrollment summary data reactive ---------------------------
# get final people-level, inflow/outflow dataframe by joining the filtered 
# enrollment and people dfs, as well as flagging their inflow and outflow types
sys_inflow_outflow_plot_data({
  period_specific_data()[["Full"]] %>%
    select(PersonalID,
           InflowTypeSummary,
           InflowTypeDetail,
           OutflowTypeSummary,
           OutflowTypeDetail
    ) %>%
    unique()
  # AS QC check:
  #   missing_types <- universe() %>%
  #     inner_join(
  #       plot_data %>%
  #         filter(
  #           OutflowTypeDetail == "something's wrong" |
  #             InflowTypeDetail == "something's wrong"),
  #       by = "PersonalID") %>%
  #     mutate(
  #       missing_inflow = eecr == TRUE & InflowTypeDetail == "something's wrong",
  #       missing_outflow = lecr == TRUE & OutflowTypeDetail == "something's wrong",
  #     ) %>%
  #     filter(missing_inflow == TRUE | missing_outflow == TRUE)
  #   
  # # browser()
  #   
  #   category_counts <- plot_data %>%
  #     select(PersonalID, InflowTypeDetail, OutflowTypeDetail) %>%
  #     pivot_longer(
  #       cols = c(InflowTypeDetail, OutflowTypeDetail), 
  #       names_to = "Time", 
  #       values_to = "Status") %>%
  #     group_by(Time, Status) %>%
  #     summarise(values = n()) %>%
  #     ungroup() %>%
  #     filter(!is.na(Status)) %>%
  #     mutate(
  #       values = ifelse(Time == "OutflowTypeDetail", values * -1, values),
  #       inflow_outflow = Time,
  #       Time = case_when(
  #         Time == "InflowTypeDetail" &
  #           Status %in% c("Homeless", "Housed")
  #         ~ paste0("Active as of \n", ReportStart()),
  #         
  #         Time == "OutflowTypeDetail" &
  #           Status %in% c("Homeless", "Housed")
  #         ~ paste0("Active as of \n", ReportEnd()),
  #         
  #         Time == "InflowTypeDetail"
  #         ~ "Inflow",
  #         
  #         Time == "OutflowTypeDetail"
  #         ~ "Outflow"
  #       )
  #     )
})

# newly_homeless_clients <- plot_data %>%
#   filter(InflowTypeDetail == "Newly Homeless") %>%
#   pull(PersonalID) %>%
#   unique()
# 
# enrollment_categories  %>%
#   group_by(PersonalID) %>%
#   mutate(Count = n()) %>%
#   ungroup() %>%
#   filter(PersonalID %in% c(newly_homeless_clients) & Count > 1) %>%
#   mutate(DestinationDescription = living_situation(Destination),
#          ReportStart = ReportStart(),
#          ReportEnd = ReportEnd(),
#          ExportStart = ExportStartAdjusted,
#          ExportEnd = ExportEndAdjusted,
#          LookbackBegins = ReportStart() - years(2),
#          ProjectType = project_type_abb(ProjectType),
#          LivingSituation = living_situation(LivingSituation)) %>%
#   select(
#     PersonalID,
#     EnrollmentID,
#     ExportStart,
#     LookbackBegins,
#     ReportStart,
#     EntryDate,
#     ExitAdjust,
#     ReportEnd,
#     ExportEnd,
#     ProjectType,
#     LivingSituation,
#     DestinationDescription,
#     days_to_next_entry,
#     days_since_previous_exit,
#     lecr,
#     eecr,
#     lookback
#   ) -> for_review
# 
# write_csv(for_review, here("newly_homeless_20240912a.csv"))

# Month-by-Month Prep ---------------------------------------------------
sys_inflow_outflow_monthly_data({
  browser()
  unique(
    rbindlist(period_specific_data()[-1])[, .(
      # Count unique PersonalIDs for each category using system flow logic
      Inflow = uniqueN(PersonalID[InflowTypeSummary == "Inflow"]),
      Outflow = uniqueN(PersonalID[OutflowTypeSummary == "Outflow"])
    ), by = month
    ][, `:=`(
      `Monthly Change` = Inflow - Outflow,
      month = factor(format(month, "%b"), 
                     levels = format(months_in_report_period, "%b"))
    )]
  )
  # 
  # json_str <- jsonlite::toJSON(
  #   unique(
  #     merge(
  #       all_months[
  #         !(PersonalID %in% unique(full$PersonalID)), 
  #         .(EnrollmentID, PersonalID, EntryDate, ExitAdjust, ProjectType, lh_prior_livingsituation)
  #       ], 
  #       homeless_cls[, .(EnrollmentID, InformationDate)], 
  #       by = "EnrollmentID"
  #     )[, .(PersonalID, InformationDate, EntryDate, ExitAdjust, ProjectType, lh_prior_livingsituation)]
  #   ), 
  #   null="list", 
  #   na="string",
  #   pretty = TRUE, auto_unbox = TRUE
  # )
  # json_str <- gsub("true", "True", json_str)
  # json_str <- gsub("false", "False", json_str)
  # 
  # unique(
  # rbindlist(period_specific_data()[-1])[
  # PersonalID %in% c("686041","684918","349625","556533","693996","614071","677683","701796","702055"),
  # .(PersonalID, month, InflowTypeDetail, OutflowTypeDetail)
  # ][order(PersonalID, month)][, month:= format(month, "%b")])
  
  # TESTING DIFF BETWEEN FULL AND MBM
  # full <- period_specific_data()[[1]]
  # all_months <- rbindlist(period_specific_data()[-1])
  # setdiff(sort(unique(full$PersonalID)), sort(unique(all_months$PersonalID)))
  # setdiff(sort(unique(all_months$PersonalID)), sort(unique(full$PersonalID)))
  
  # first_renamed <- monthly_universe_ppl_flags[month == as.Date("2021-10-01")][
  #   , `:=`(
  #     First_Inflow = InflowTypeSummary,
  #     First_Outflow = OutflowTypeSummary
  #   )][, c("InflowTypeSummary", "OutflowTypeSummary") := NULL][
  #     , .(PersonalID, EnrollmentID, First_Inflow, First_Outflow)
  #   ]
  # 
  # second_renamed <- monthly_universe_ppl_flags[month == as.Date("2021-11-01")][
  #   , `:=`(
  #     Second_Inflow = InflowTypeSummary,
  #     Second_Outflow = OutflowTypeSummary
  #   )][, c("InflowTypeSummary", "OutflowTypeSummary") := NULL][
  #     , .(PersonalID, EnrollmentID, Second_Inflow, Second_Outflow)
  #   ]
  # 
  # # Merge the data tables to get matching records
  # merged_dt <- merge(
  #   first_renamed,
  #   second_renamed,
  #   by = c("PersonalID","EnrollmentID"),
  #   all.x = TRUE
  # )[First_Inflow != Second_Inflow | First_Outflow != Second_Outflow]
  # browser()
  # monthly_universe_ppl_flags
})


# System Composition/Demographics data for chart
sys_df_people_universe_filtered_r({
  cols_to_keep <- colnames(client_categories_filtered())
  
  unique(
    period_specific_data()[["Full"]][, ..cols_to_keep]
  )
})

# The universe is anyone who was Housed or Homeless at Period Start
# We also need the latest exit for the folks in the Exited categories
sankey_plot_data({
  req(nrow(sys_inflow_outflow_plot_data()) > 0)
  plot_df <- sys_inflow_outflow_plot_data() %>%
    filter(InflowTypeDetail == "Housed" | InflowTypeDetail == "Homeless")
  
  startBind <- plot_df %>%
    select(PersonalID, "Type" = InflowTypeDetail) %>%
    mutate("Period" = "Begin")
  
  endBind <- plot_df %>%
    select(PersonalID, "Type" = OutflowTypeDetail) %>%
    mutate(
      "Period" = "End",
      Type = case_when(
        Type == "Exited,\nPermanent" ~ "Exited, Permanent",
        Type == "Exited,\nNon-Permanent" ~ "Exited, Non-Permanent",
        Type == "Homeless" ~ "Enrolled, Homeless",
        Type == "Housed" ~ "Enrolled, Housed",
        TRUE ~ Type
      ))
  
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

# Client-level download
client_level_export_df(
  merge(
    period_specific_data()[["Full"]],
    Client %>% select(PersonalID, !!gender_cols, !!race_cols), 
    by="PersonalID"
  )
)