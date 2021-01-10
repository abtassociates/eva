qpr_expr$LoS <- list()
qpr_expr$LoS$expr <- rlang::expr({
  
  .dat <- qpr_leavers %>%
    filter(((
      !is.na(MoveInDateAdjust) & ProjectType == 13
    ) |
      (
        !is.na(ExitDate) & ProjectType %in% c(1, 2, 8)
      )) &
      exited_between(., input$date_range[1], input$date_range[2]) &
      ProjectName == input$region
    ) 
    
  
  
  .detail <- .dat %>%
    mutate(PersonalID = as.character(PersonalID)) %>%
    arrange(desc(DaysinProject)) %>%
    select(
      "Client ID" = PersonalID,
      "Bed Start" = EntryAdjust,
      "Exit Date" = ExitDate,
      "Days in Project" = DaysinProject
    )
  
  .summary <- .dat %>% 
    group_by(ProjectName) %>%
    summarise(Average = format(mean(DaysinProject),
                               digits = 1),
              Median = median(DaysinProject), .groups = "drop_last")
  
  
  list(summary = .summary,
       detail = .detail)
})

qpr_expr$LoS$infobox <- rlang::expr({
  .data <- data_env()$summary
  qpr_infobox(.data,
              icon = "clock",
              value = paste("Average", 
                            .data$Average, 
                            "/ Median", 
                            .data$Median,
                            "days"),
              title = "Average and Median Length of Stay",
              subtitle = "Length of Stay in Project's Housing"
  )
})

qpr_expr$LoS$datatable <- rlang::expr({
  qpr_datatable(data_env()$detail)
})

