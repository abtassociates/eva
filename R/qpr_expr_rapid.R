qpr_expr$rapid <- list()
qpr_expr$rapid$expr <- rlang::expr({
  
  .dat <- qpr_rrh_enterers() %>%
    filter(
      !is.na(MoveInDateAdjust) &
        ProjectName %in% c(input$region) &
        entered_between(., input$date_range[1], input$date_range[2])
    ) 
  
  .detail <- .dat %>%
    mutate(PersonalID = as.character(PersonalID)) %>%
    arrange(DaysToHouse) %>%
    select(
      "Client ID" = PersonalID,
      "Entry Date" = EntryDate,
      "Move In Date" = MoveInDate,
      "Days to House" = DaysToHouse
    )

  .summary  <- .dat %>%
    mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) %>%
    summarise(AvgDaysToHouse = round(mean(DaysToHouse), 0), .groups = "drop_last")
  
  
  
  list(summary = .summary,
       detail = .detail)
})

qpr_expr$rapid$infobox <- rlang::expr({
  .data <- data_env()$summary
  qpr_infobox(
    .data = .data,
    title = "Average Days to House",
    color = "purple",
    icon = "hourglass-half",
    value = .data$AvgDaysToHouse,
    subtitle = "See table below for detail."
  )
  
})

qpr_expr$rapid$datatable <- rlang::expr({
  qpr_datatable(data_env()$detail)
})

