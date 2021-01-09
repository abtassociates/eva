qpr_expr$spdat2 <- list()
qpr_expr$spdat2$expr <- rlang::expr({
  # counting all households who were scored AND SERVED between the report dates
  .dat <- qpr_spdats_county %>%
    served_between(input$date_range[1],
                   input$date_range[2]) %>%
    left_join(regions, by = c("CountyServed" = "County")) %>%
    filter(RegionName == input$region)
  
  .detail <- .dat %>% 
    mutate(PersonalID = as.character(PersonalID)) %>%
    select(
      Project = ProjectName,
      "Client ID" = PersonalID,
      "Entry Date" = EntryDate,
      "Exit Date" = ExitDate,
      "County Served" = CountyServed,
      Score
    ) %>%
    arrange(Score)
  
  .summary <- .dat %>%
    group_by(RegionName) %>%
    summarise(AvgScore = as.integer(mean(Score)), .groups = "drop_last")
  
  list(summary = .summary,
       detail = .detail)
})

qpr_expr$spdat2$infobox <- rlang::expr({
  qpr_infobox(data_env()$summary,
              icon = "shoe-prints",
              subtitle = "Literally Homeless Households in the Selected Region"
  )
})

qpr_expr$spdat2$datatable <- rlang::expr({
  qpr_datatable(data_env()$detail)
})

