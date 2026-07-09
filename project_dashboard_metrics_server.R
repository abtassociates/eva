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
  
  success_ful_exit_dt <- session$userData$Exit |>
    fsubset(!is.na(ExitDate)) |>
    join(
      enrollment_w_project_type |> fselect(EnrollmentID, ProjectType),
      on = "EnrollmentID",
      how = "inner"
    ) |>
    fmutate(
      successful_exit = fcase(
        ProjectType == out_project_type, Destination %in% setdiff(c(100:499),c(116,206,207,329)),
        ProjectType %in% c(es_ee_project_type, es_nbn_project_type, th_project_type) = c(332,400:499),
        default = c(400:499)
      )
    ) |>
    fsubset(
      !(
        (Destination %in% c(24, 206) & ProjectType %in% lh_ph_hp_project_types) |
        (Destination %in% c(215, 225) & ProjectType %in% setdiff(lh_ph_hp_project_types, out_project_type)) |
        (Destination == 329 & ProjectType == out_project_type)
      )
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
    successful_exit_pct = fnrow(success_ful_exit_dt[successful_exit == TRUE])/fnrow(success_ful_exit_dt),
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
