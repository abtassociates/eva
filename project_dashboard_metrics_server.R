metrics <- reactive({
  all_hohs_adults <- session$userData$Enrollment |> 
    fsubset(RelationshipToHoH == 1 | AgeAtEntry >= 18, EnrollmentID, LivingSituation)
  
  zero_income_dt <- session$userData$IncomeBenefits |>
    fsubset(DataCollectionStage == 1, EnrollmentID, IncomeFromAnySource) |>
    join(
      session$userData$Enrollment |> fsubset(RelationshipToHoH == 1 | AgeAtEntry >= 18, EnrollmentID),
      how = "inner"
    )
  
  income_growth_dt <- session$userData$IncomeBenefits |>
    fmutate(
      income_at_entry = fifelse(DataCollectionStage == 1, TotalMonthlyIncome, NA),
      income_at_exit = fifelse(DataCollectionStage == 3, TotalMonthlyIncome, NA),
      income_growth = income_at_exit > income_at_entry
    ) |>
    fgroup_by(EnrollmentID) |>
    fmutate(has_income_at_entry = anyv(income_at_entry, TRUE)) |>
    fungroup() |>
    fsubset(has_income_at_entry == TRUE)
  
  browser()
  list(
    avg_los = mean(session$userData$Enrollment$LengthOfStay, na.rm=TRUE),
    median_los = median(session$userData$Enrollment$LengthOfStay, na.rm=TRUE),
    entered_non_habitat_pct = fnrow(all_hohs_adults[LivingSituation == 116L])/fnrow(all_hohs_adults),
    successful_exit_pct = fnrow(session$userData$Exit[Destination %in% c(116L)])/fnrow(session$userData$Exit[!is.na(Destination)]),
    zero_income_pct = fnrow(zero_income_dt[is.na(IncomeFromAnySource) | IncomeFromAnySource == 0])/fnrow(zero_income_dt),
    income_growth_pct = fnrow(income_growth_dt[income_growth > 0])/fnrow(income_growth_dt),
    ce_assessments = 10
  )  
})

output$los_avg <- renderText({
  sprintf("%.1f days", metrics()$avg_los)
})

output$los_median <- renderText({
  sprintf("%d days", metrics()$median_los)
})

output$entered_homeless_pct <- renderText({
  sprintf("%.0f%%", metrics()$entered_homeless_pct)
})

output$successful_exit_pct <- renderText({
  sprintf("%.0f%%", metrics()$successful_exit_pct)
})

output$zero_income_pct <- renderText({
  sprintf("%.0f%%", metrics()$zero_income_pct)
})

output$income_growth_pct <- renderText({
  sprintf("%.0f%%", metrics()$income_growth_pct)
})

output$ce_assessments <- renderText({
  scales::comma(metrics()$ce_assessments)
})
