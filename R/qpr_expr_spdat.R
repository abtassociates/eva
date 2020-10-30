qpr_expr$spdat <- list()
qpr_expr$spdat$expr <- rlang::expr({
  # used in both summary and detail, filter by inputs
  .dat <- qpr_spdats_project %>%
    HMIS::entered_between(Report()$Start,
                           Report()$End) %>%
    dplyr::left_join(., regions, by = c("CountyServed" = "County")) %>%
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
    dplyr::summarise(AvgScore = as.integer(mean(ScoreAdjusted)))
  .out <- list(summary = .summary,
       detail = .detail)
})

qpr_expr$spdat$ib <- rlang::expr({
  qpr_infobox(data_env()$summary,
              icon = "parachute-box",
              subtitle = "Households who were Housed in RRH or PSH in the Selected Region"
              )
})

qpr_expr$spdat$dt <- rlang::expr({
  qpr_datatable(data_env()$detail)
})
