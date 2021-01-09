qpr_expr$NCB <- list()
qpr_expr$NCB$expr <- rlang::expr({
  
  .dat <- qpr_benefits %>%
    filter(ProjectName == input$region &
             exited_between(., input$date_range[1], input$date_range[1]))
  
  .detail <- .dat %>%
    mutate(
      BenefitsFromAnySource = case_when(
        BenefitsFromAnySource == 0 ~ "No (HUD)",
        BenefitsFromAnySource == 1 ~ "Yes (HUD)",
        BenefitsFromAnySource == 8 ~ "Client doesn't know (HUD)",
        BenefitsFromAnySource == 9 ~ "Client refused (HUD)",
        BenefitsFromAnySource == 99 ~ "Data Not Collected (HUD)"
      ),
      PersonalID = as.character(PersonalID)
    ) %>%
    select(
      "Client ID" = PersonalID,
      "Entry Date" = EntryDate,
      "Exit Date" = ExitDate,
      "Benefits from Any Source (at Exit)" = BenefitsFromAnySource
    )
  
  
  .summary  <- 
    left_join(
      # all_hhs
      .dat %>% 
        group_by(ProjectName) %>%
        summarise(TotalHHs = n(), .groups = "drop_last"),
      # meeting_objective
      .dat %>% 
        filter(BenefitsFromAnySource == 1) %>% 
        group_by(ProjectName) %>%
        summarise(BenefitsAtExit = n(), .groups = "drop_last"),
      by = c("ProjectName")
    ) %>% 
    {tidyr::replace_na(., setNames(as.list(rep(0, length(.))), nm = names(.)))} %>% 
    mutate(Percent = BenefitsAtExit / TotalHHs)
  
  
  
  list(summary = .summary,
       detail = .detail)
})

qpr_expr$NCB$infobox <- rlang::expr({
  if (nrow(data_env()$summary) > 0) {
    .args <- list(.data = data_env()$summary,
                icon = "shopping-cart",
                color = "fuchsia",
                value = scales::percent(.data$Percent),
                title = "Households Exiting With Non Cash Benefits",
                subtitle = paste(.data$BenefitsAtExit, 
                                 "out of",
                                 .data$TotalHHs, 
                                 "households")
    )
  } else {
    .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})

qpr_expr$NCB$datatable <- rlang::expr({
  qpr_datatable(data_env()$detail)
})

