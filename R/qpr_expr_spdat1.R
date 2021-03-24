qpr_expr$spdat1 <- list()
qpr_expr$spdat1$expr <- rlang::expr({
  # used in both summary and detail, filter by inputs
  .dat <- qpr_spdats_project() %>%
    HMIS::entered_between(input$date_range[1],
                          input$date_range[2]) %>%
    dplyr::left_join(., regions(), by = c("CountyServed" = "County")) %>%
    dplyr::filter(RegionName == input$region)
  
  .detail <- .dat %>% 
    mutate(PersonalID = as.character(PersonalID)) %>%
    arrange(ScoreAdjusted) %>%
    select(
      "Client ID" = PersonalID,
      Project = ProjectName,
      "Entry Date" = EntryDate,
      "County Served" = CountyServed,
      "Score Date" = ScoreDate,
      Score,
      "Score Adjusted" = ScoreAdjusted
    )
  .summary <- .dat %>% 
    dplyr::group_by(RegionName) %>%
    dplyr::summarise(AvgScore = round(mean(ScoreAdjusted), 0), , .groups = "drop_last")
  list(summary = .summary,
       detail = .detail)
})

qpr_expr$spdat1$infobox <- rlang::expr({
  qpr_infobox(data_env()$summary,
              icon = "parachute-box",
              subtitle = "Households who were Housed in RRH or PSH in the Selected Region"
              )
})

qpr_expr$spdat1$datatable <- rlang::expr({
  qpr_datatable(data_env()$detail)
})
