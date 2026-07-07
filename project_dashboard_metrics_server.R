metrics <- reactive({
  enrollment_w_project_type <- session$userData$Enrollment |> 
    fsubset(EntryDate >= input$dateRangeCount[1] & ExitAdjust <= input$dateRangeCount[2]) |>
    join(session$userData$Project0 |> fselect(ProjectID, ProjectType), on = "ProjectID") 
  
  los_dt <- enrollment_w_project_type |>
    fsubset(ProjectType %in% c(lh_residential_project_types, ph_project_types))
  
  entered_non_habitat_dt <- enrollment_w_project_type |>
    fsubset(
      ProjectType %in% c(lh_residential_project_types, setdiff(non_res_project_types, hp_project_type))	&
        (RelationshipToHoH == 1 | AgeAtEntry >= 18), 
      EnrollmentID, LivingSituation
    )
  
  zero_income_dt <- session$userData$IncomeBenefits |>
    fsubset(DataCollectionStage == 1, EnrollmentID, IncomeFromAnySource) |>
    join(
      enrollment_w_project_type |> fsubset(ProjectType == hp_project_type & (RelationshipToHoH == 1 | AgeAtEntry >= 18), EnrollmentID),
      on = "EnrollmentID",
      how = "inner"
    )
  
  income_growth_dt <- session$userData$IncomeBenefits |>
    join(
      enrollment_w_project_type |> fsubset(ProjectType %in% c(ph_project_types, hp_project_type), EnrollmentID, EntryDate, ExitAdjust),
      on = "EnrollmentID",
      how = "inner"
    ) |>
    fmutate(TotalIncome = TotalMonthlyIncome*12) |>
    # fselect(EnrollmentID, DataCollectionStage, TotalIncome, InformationDate, EntryDate, ExitAdjust) |>
    fmutate(
      income_at_entry = fifelse(DataCollectionStage == 1, TotalIncome, NA),
      income_at_exit = fifelse(DataCollectionStage == 3, TotalIncome, NA)
    ) |>
    roworder(EnrollmentID, EntryDate) |>
    fgroup_by(EnrollmentID) |>
    fmutate(income_at_entry = ffirst(income_at_entry)) |>
    fungroup() |>
    roworder(EnrollmentID, ExitAdjust) |>
    fgroup_by(EnrollmentID) |>
    fmutate(income_at_exit = flast(income_at_entry)) |>
    fslice(how = "first") |>
    fungroup()
  
  list(
    avg_los = mean(los_dt$LengthOfStay, na.rm=TRUE),
    median_los = median(los_dt$LengthOfStay, na.rm=TRUE),
    entered_non_habitat_pct = fnrow(entered_non_habitat_dt[LivingSituation == 116L])/fnrow(entered_non_habitat_dt),
    successful_exit_pct = fnrow(session$userData$Exit[Destination >= 400])/fnrow(session$userData$Exit[!is.na(Destination)]),
    zero_income_pct = fnrow(zero_income_dt[is.na(IncomeFromAnySource) | IncomeFromAnySource == 0])/fnrow(zero_income_dt),
    income_growth_pct = fnrow(income_growth_dt[income_growth > 0])/fnrow(income_growth_dt),
    ce_assessments = session$userData$CEAssessedHouseholds
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
