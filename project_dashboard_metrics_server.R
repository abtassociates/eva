metrics <- reactive({
  enrollment_w_project_type <- session$userData$Enrollment |> 
    fsubset(EntryDate %between% input$dateRangeCount | ExitAdjust %between% input$dateRangeCount) |>
    join(session$userData$Project0 |> fselect(ProjectID, ProjectType), on = "ProjectID") 
  
  latest_entry <- enrollment_w_project_type[EntryDate %between% input$dateRangeCount]
  latest_entry <- if(fnrow(latest_entry) > 0)
    latest_entry |>
      fgroup_by(PersonalID, EntryDate) |>
      fslice(how = "last")
  else
    data.table()
    
  latest_exit <- enrollment_w_project_type[ExitDate %between% input$dateRangeCount]
  latest_exit <- if(fnrow(latest_exit) > 0)
    latest_exit |>
      fsubset(ExitDate %between% input$dateRangeCount) |>
      fgroup_by(PersonalID, ExitDate) |>
      fslice(how = "last")
  else
    data.table()
  
  los_dt <- latest_entry |>
    fsubset(ProjectType %in% c(lh_residential_project_types, ph_project_types))
  
  entered_non_habitat_dt <- latest_entry |>
    fsubset(
      ProjectType %in% c(lh_residential_project_types, setdiff(non_res_project_types, hp_project_type))	&
      (RelationshipToHoH == 1 | AgeAtEntry >= 18), 
      EnrollmentID, LivingSituation
    )
  
  zero_income_dt <- session$userData$IncomeBenefits |>
    fsubset(DataCollectionStage == 1, EnrollmentID, IncomeFromAnySource) |>
    join(
      latest_entry |> fsubset(ProjectType == hp_project_type & (RelationshipToHoH == 1 | AgeAtEntry >= 18), EnrollmentID),
      on = "EnrollmentID",
      how = "inner"
    )
    
  
  successful_exit_dt <- session$userData$Exit |>
    join(
      latest_exit |> fselect(EnrollmentID, ProjectType),
      on = "EnrollmentID",
      how = "inner"
    ) |>
    fmutate(
      successful_exit = fcase(
        ProjectType == out_project_type, Destination %in% setdiff(c(100:499),c(116,206,207,329)),
        ProjectType %in% c(es_ee_project_type, es_nbn_project_type, th_project_type), Destination %in% c(332,400:499),
        default = Destination %in% c(400:499)
      )
    ) |>
    fsubset(
      !(
        (Destination %in% c(24, 206) & ProjectType %in% lh_ph_hp_project_types) |
          (Destination %in% c(215, 225) & ProjectType %in% setdiff(lh_ph_hp_project_types, out_project_type)) |
          (Destination == 329 & ProjectType == out_project_type)
      )
    )
  
  income_growth_latest_enrl <- latest_exit[ProjectType %in% c(ph_project_types, hp_project_type)]
  income_growth_dt <- if(fnrow(session$userData$IncomeBenefits) > 0 && fnrow(income_growth_latest_enrl) > 0)
    session$userData$IncomeBenefits |>
      join(
        income_growth_latest_enrl |> fselect(EnrollmentID),
        on = "EnrollmentID",
        how = "inner"
      ) |>
      fmutate(
        income_at_entry = fifelse(DataCollectionStage == 1, TotalMonthlyIncome, NA),
        income_at_exit = fifelse(DataCollectionStage == 3, TotalMonthlyIncome, NA)
      ) |>
      fgroup_by(EnrollmentID) |>
      fmutate(
        income_at_entry = ffirst(income_at_entry),
        income_at_exit = flast(income_at_exit)
      ) |>
      fslice(how = "first") |>
      fungroup() |>
      fmutate(has_income_growth = income_at_exit > income_at_entry)
    else
      data.table()
  
  list(
    los_avg = mean(los_dt$LengthOfStay, na.rm=TRUE),
    los_median = median(los_dt$LengthOfStay, na.rm=TRUE),
    los_nmiss = fsum(is.na(los_dt$LengthOfStay)),
    
    entered_non_habitat_pct = fsum(entered_non_habitat_dt$LivingSituation == 116L)/fnrow(entered_non_habitat_dt[!is.na(LivingSituation)]),
    entered_non_habitat_nmiss = fsum(is.na(enrollment_w_project_type$LivingSituation)),
    
    successful_exit_pct = fsum(successful_exit_dt$successful_exit)/fnrow(successful_exit_dt),
    successful_exit_nmiss = fsum(is.na(successful_exit_dt$Destination)),
    
    zero_income_pct = fnrow(zero_income_dt[is.na(IncomeFromAnySource) | IncomeFromAnySource == 0])/fnrow(zero_income_dt),
    zero_income_nmiss = fsum(is.na(zero_income_dt$IncomeFromAnySource)),
    
    income_growth_pct = if(fnrow(income_growth_dt) > 0) fsum(income_growth_dt$has_income_growth)/fnrow(income_growth_dt[!is.na(TotalMonthlyIncome)]) else NA,
    income_growth_nmiss = fsum(is.na(income_growth_dt$TotalMonthlyIncome)),
    
    ce_assessments = session$userData$CEAssessedHouseholds
  )  
})

render_pct <- function(val) {
  if(length(val) > 0)
    return(percent(val))
  else
    return("-")
}

render_nmiss <- function(val) {
  sprintf("%d enrollments", scales::comma(val))
}

output$los_avg <- renderText({sprintf("%.1f days", metrics()$los_avg)})
output$los_median <- renderText({sprintf("%d days", metrics()$los_median)})
output$los_nmiss <- renderText({render_nmiss(metrics()$los_nmiss)})

output$entered_non_habitat_pct <- renderText({render_pct(metrics()$entered_non_habitat_pct)})
output$entered_non_habitat_nmiss <- renderText({render_nmiss(metrics()$entered_non_habitat_nmiss)})

output$successful_exit_pct <- renderText({render_pct(metrics()$successful_exit_pct)})
output$successful_exit_nmiss <- renderText({render_nmiss(metrics()$successful_exit_nmiss)})

output$zero_income_pct <- renderText({render_pct(metrics()$zero_income_pct)})
output$zero_income_nmiss <- renderText({render_nmiss(metrics()$zero_income_nmiss)})

output$income_growth_pct <- renderText({render_pct(metrics()$income_growth_pct)})
output$income_growth_nmiss <- renderText({render_nmiss(metrics()$income_growth_nmiss)})

output$ce_assessments <- renderText({scales::comma(metrics()$ce_assessments)})
