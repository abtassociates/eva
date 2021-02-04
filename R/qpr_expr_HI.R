qpr_expr$HI <- list()
qpr_expr$HI$expr <- rlang::expr({
  
  .dat <- qpr_benefits() %>%
    filter(ProjectName == input$region &
             exited_between(., input$date_range[1], input$date_range[2]))
  # input <- list(region = "Richland - Harmony House Homeless Services - HCRP RRH",
  #               date_range = c(lubridate::ymd("2020-01-01"), Sys.Date()))
  .detail <- .dat %>%
    mutate(
      InsuranceFromAnySource = case_when(
        InsuranceFromAnySource == 0 ~ "No (HUD)",
        InsuranceFromAnySource == 1 ~ "Yes (HUD)",
        InsuranceFromAnySource == 8 ~ "Client doesn't know (HUD)",
        InsuranceFromAnySource == 9 ~ "Client refused (HUD)",
        InsuranceFromAnySource == 99 ~ "Data Not Collected (HUD)"
      )
    ) %>%
    mutate(PersonalID = as.character(PersonalID)) %>%
    select(
      "Client ID" = PersonalID,
      "Entry Date" = EntryDate,
      "Exit Date" = ExitDate,
      "Health Insurance from Any Source (at Exit)" = InsuranceFromAnySource
    )
  
  
  .summary  <- 
    left_join(
      # all_hhs
      .dat %>% 
        group_by(ProjectName) %>%
        summarise(TotalHHs = n(), .groups = "drop_last"),
      # meeting_objective
      .dat %>% 
        filter(InsuranceFromAnySource == 1) %>% 
        group_by(ProjectName) %>%
        summarise(InsuranceAtExit = n(), .groups = "drop_last"),
      by = c("ProjectName")
    ) %>% 
    {tidyr::replace_na(., setNames(as.list(rep(0, length(.))), nm = names(.)))} %>% 
    mutate(Percent = InsuranceAtExit / TotalHHs)
  
  
  
  list(summary = .summary,
       detail = .detail)
})

qpr_expr$HI$infobox <- rlang::expr({
  .data <- data_env()$summary
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  title = "Total Households Exiting With Health Insurance",
                  color = "black",
                  icon = "medkit",
                  value = scales::percent(.data$Percent),
                  subtitle = paste(.data$InsuranceAtExit, 
                                   "out of",
                                   .data$TotalHHs,
                                   "households")
    )
  } else {
    .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})

qpr_expr$HI$datatable <- rlang::expr({
  qpr_datatable(data_env()$detail)
})

