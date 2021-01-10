qpr_expr$income <- list()
qpr_expr$income$expr <- rlang::expr({
  
  .dat <- qpr_income %>%
    filter(ProjectName == input$region &
             stayed_between(., input$date_range[1], input$date_range[2]))
  # input <- list(region = "Richland - Harmony House Homeless Services - HCRP RRH",
  #               date_range = c(lubridate::ymd("2020-01-01"), Sys.Date()))
  .detail <- .dat %>%
    mutate(EntryIncome = dollar(EntryIncome, accuracy = .01),
           RecentIncome = dollar(RecentIncome, accuracy = .01),
           Difference = dollar(Difference, accuracy = .01),
           PersonalID = as.character(PersonalID)) %>%
    select(
      "Client ID" = PersonalID,
      "Entry Date" = EntryDate,
      "Exit Date" = ExitDate,
      "Income at Entry" = EntryIncome,
      "Most Recent Income" = RecentIncome,
      "Increase Amount" = Difference
    )
  
  
  .summary  <- 
    left_join(
      # all_hhs
      .dat %>% 
        group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) %>%
        summarise(TotalHHs = n(), .groups = "drop_last"),
      # meeting_objective
      .dat %>% 
        filter(Difference > 0) %>% 
        group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) %>%
        summarise(Increased = n(), .groups = "drop_last"),
      by = c("ProjectName", "ProjectType", "ProjectCounty", "ProjectRegion")
    ) %>% 
    {tidyr::replace_na(., setNames(as.list(rep(0, length(.))), nm = names(.)))} %>% 
    mutate(Percent = Increased / TotalHHs)
  
  
  
  list(summary = .summary,
       detail = .detail)
})

qpr_expr$income$infobox <- rlang::expr({
  .data <- data_env()$summary
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  title = "Households Increasing Their Income",
                  color = "green",
                  icon = "hand-holding-usd",
                  value = scales::percent(.data$Percent),
                  subtitle = paste(.data$Increased, 
                                   "out of",
                                   .data$TotalHHs, 
                                   "households served")
    )
  } else {
    .args <- list(title = "Something's wrong- email us at hmis@cohhio.org!", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})

qpr_expr$income$datatable <- rlang::expr({
  qpr_datatable(data_env()$detail)
})

