metrics <- reactive({
  # prep basic enrollment datasets --------------------
  latest_enrollments <- get_latest_enrollments()
  
  metric_specific_datasets <- get_metric_specific_datasets(latest_enrollments)
  
  detail_dataset <- get_details_by_hh_type(metric_specific_datasets)
  
  list(
    los_avg = fmean(metric_specific_datasets$los$LengthOfStay),
    los_median = fmedian(metric_specific_datasets$los$LengthOfStay),
    los_nmiss = fsum(is.na(metric_specific_datasets$los$LengthOfStay)),
    
    entered_non_habitat_pct = calc_entered_non_habitat_pct(metric_specific_datasets$entered_non_habitat),
    entered_non_habitat_nmiss = fsum(is.na(latest_enrollments$LivingSituation)),
    
    successful_exit_pct = fsum(metric_specific_datasets$successful_exit$successful_exit)/fnrow(successful_exit_dt),
    successful_exit_nmiss = fsum(is.na(successful_exit_dt$Destination)),
    
    zero_income_pct = fnrow(metric_specific_datasets$zero_income[is.na(IncomeFromAnySource) | IncomeFromAnySource == 0])/fnrow(zero_income_dt),
    zero_income_nmiss = fsum(is.na(metric_specific_datasets$zero_income$IncomeFromAnySource)),
    
    income_growth_pct = if(fnrow(metric_specific_datasets$income_growth) > 0) 
      fsum(metric_specific_datasets$income_growth$has_income_growth)/fnrow(income_growth_dt[!is.na(TotalMonthlyIncome)]) 
    else NA,
    income_growth_nmiss = fsum(is.na(metric_specific_datasets$income_growth$TotalMonthlyIncome)),
    
    ce_assessments = session$userData$CEAssessedHouseholds
  )  
})

get_metric_specific_datasets <- function(latest_enrollments) {
  # Metric-specific data prep -----------
  ## LOS -------------
  los_dt <- latest_enrollments |>
    fsubset(
      ProjectType %in% c(lh_residential_project_types, ph_project_types), 
      EnrollmentID, LengthOfStay, HouseholdType
    )
  
  ## Entered Place Not Meant for Habitation -------------
  entered_non_habitat_dt <- latest_enrollments |>
    fsubset(
      ProjectType %in% c(lh_residential_project_types, setdiff(non_res_project_types, hp_project_type))	&
        (RelationshipToHoH == 1 | AgeAtEntry >= 18), 
      EnrollmentID, LivingSituation, HouseholdType
    )
  
  ## Zero Income -------------
  zero_income_dt <- session$userData$IncomeBenefits |>
    fsubset(DataCollectionStage == 1, EnrollmentID, IncomeFromAnySource) |>
    join(
      latest_enrollments |> 
        fsubset(
          ProjectType == hp_project_type & 
            (RelationshipToHoH == 1 | AgeAtEntry >= 18), 
          EnrollmentID, HouseholdType
        ),
      on = "EnrollmentID",
      how = "inner"
    )
  
  ## Successful Exit -------------
  successful_exit_dt <- session$userData$Exit |>
    fselect(EnrollmentID, Destination) |>
    join(
      latest_enrollments |> 
        fsubset(!is.na(ExitAdjust), EnrollmentID, ProjectType, HouseholdType),
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
  
  ## Income Growth -------------
  income_growth_latest_enrl <- latest_enrollments |>
    fsubset(
      ProjectType %in% c(ph_project_types, hp_project_type) &
        (RelationshipToHoH == 1 | AgeAtEntry >= 18), 
      EnrollmentID, HouseholdType
    )
  
  income_growth_dt <- if(fnrow(session$userData$IncomeBenefits) > 0 && fnrow(income_growth_latest_enrl) > 0)
    session$userData$IncomeBenefits |>
    fselect(EnrollmentID, DataCollectionStage, TotalMonthlyIncome) |>
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
  
  return(
    list(
      latest_enrollments = latest_enrollments,
      los = los_dt,
      entered_non_habitat = entered_non_habitat_dt,
      zero_income = zero_income_dt,
      successful_exit = successful_exit_dt,
      income_growth = income_growth_dt  
    )
    
  )
}
get_latest_enrollments <- function() {
  ## first, only need enrollments in the Project Dashboard Date Range ---------
  enrollment_w_project_type <- session$userData$Enrollment |> 
    fsubset(EntryDate %between% input$dateRangeCount | ExitAdjust %between% input$dateRangeCount) |>
    join(session$userData$Project0 |> fselect(ProjectID, ProjectType), on = "ProjectID") |>
    fselect(PersonalID, EnrollmentID, EntryDate, ExitAdjust, HouseholdType, ProjectType, AgeAtEntry, LivingSituation, RelationshipToHoH, LengthOfStay)
  
  ## Only want the "latest" Entry when calculating metrics
  latest_enrollments <- if(fnrow(enrollment_w_project_type) > 0)
    enrollment_w_project_type |>
      fgroup_by(PersonalID, EntryDate) |>
      fslice(how = "last") |>
      fselect(PersonalID, EnrollmentID, HouseholdType, ProjectType, AgeAtEntry, LivingSituation, RelationshipToHoH, LengthOfStay, ExitAdjust)
  else
    data.table()
  
  return(latest_enrollments)
}

groups <- list(
  "All Household Types"    =  c("AOminusUY", "ACminusPY", "CO", "UN", "PY", "UY"),
  "Adult-Only Households"  = c("AOminusUY", "UY"),
  "Adult-Child Households" = c("ACminusPY", "PY"),
  "Child Only Households"  = "CO",
  "Unknown Households"     = "UN"
)

calc_by_hh_group <- function(data, func, col = NULL) {
  lapply(groups, function(g) {
    d <- data[HouseholdType %in% g]
    if(!is.null(col)) d <- d$col
    
    func(d)
  })
}

calc_entered_non_habitat_pct <- function(dt) {
  fsum(dt$LivingSituation == 116L)/fnrow(dt[!is.na(LivingSituation)])
}
get_details_by_hh_type <- function(m) {
  # We Want:
  # ====================================================================================================================================================================================================================
  # Metric                                                       All Household Types   Adult Only Households   Adult-Child Households   Child Only Households   Unknown Households   Metric Data Table Display - Applicable Project Types
  # --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Total Clients Served                                         #                     #                       #                        #                       #                    All
  # Total Households Served                                      #                     #                       #                        #                       #                    All
  # Average Households Size                                      #                     #                       #                        #                       #                    All
  # Average Length of Participation (All Clients)                #                     #                       #                        #                       #                    All
  # Median Length of Participation (All Clients)                 #                     #                       #                        #                       #                    All
  # Average Length of Stay in Residence (All Clients)            #                     #                       #                        #                       #                    project_types_w_beds
  # Median Length of Stay in Residence (All Clients)             #                     #                       #                        #                       #                    project_types_w_beds
  # Average Time to Housing Move-In (All Clients)                #                     #                       #                        #                       #                    ph_project_types
  # Median Time to Housing Move-In (All Clients)                 #                     #                       #                        #                       #                    ph_project_types
  # Moved into Housing (HoHs)                                    %                     %                       %                        %                       %                    ph_project_types
  # Entered from Place Not Meant for Habitation (HoHs/Adults)    %                     %                       %                        %                       %                    All
  # Entered from Permanent Housing Situation (HoHs/Adults)       %                     %                       %                        %                       %                    All
  # Zero Income at Entry (HoHs/Adults)                           %                     %                       %                        %                       %                    All
  # Income Growth from Entry to Exit (HoHs/Adults)               %                     %                       %                        %                       %                    All
  # Non-Cash Benefits Growth from Entry to Exit (HoHs/Adults)    %                     %                       %                        %                       %                    All
  # Successful Exits (All Clients)                               %                     %                       %                        %                       %                    All
  # CE Assessed Households                                       #                     #                       #                        #                       #                    ce_project_type, any other project that has CE Assessment data within report
  # Current Living Situation Records: Total                      #                     #                       #                        #                       #                    es_nbn_project_type, setdiff(non_res_project_types, hp_project_type)
  # ====================================================================================================================================================================================================================
  total_clients_served <- calc_by_hh_group(m$latest_enrollments, fnunique, "PersonalID") 
  browser()
  length_of_participation_dt <- m$latest_enrollments |> 
    fmutate(length_of_participation = ExitAdjust - EntryDate)
  
  time_to_movein <- m$latest_enrollments |> 
    fmutate(time_to_move_in = MoveInDateAdjust - EntryDate)
  
  avg_household_size_dt <- m$latest_enrollments |>
    fgroup_by(HouseholdID) |>
    fmutate(hh_size = fcount(PersonalID)) |>
    fungroup() |>
    funique(cols = c("HouseholdID", "hh_size"))

  rbindlist(
    list(
      "Total Clients Served" = calc_by_hh_group(m$latest_enrollments, fnunique, "PersonalID"),
      "Total Households Served" = calc_by_hh_group(m$latest_enrollments, fnunique, "HouseholdID"),
      "Average Households Size" = calc_by_hh_group(avg_household_size_dt, fmean, "hh_size"),
      "Average Length of Participation (All Clients)" = calc_by_hh_group(length_of_participation_dt, fmean, "length_of_participation"),
      "Median Length of Participation (All Clients)" = calc_by_hh_group(length_of_participation_dt, fmedian, "length_of_participation"),
      "Average Length of Stay in Residence (All Clients)" = calc_by_hh_group(m$los, fmean, "LengthOfStay"),
      "Median Length of Stay in Residence (All Clients)" = calc_by_hh_group(m$los, fmedian, "LengthOfStay"),
      "Average Time to Housing Move-In (All Clients)" = calc_by_hh_group(time_to_movein, fmean, "time_to_move_in"),
      "Median Time to Housing Move-In (All Clients)" = calc_by_hh_group(time_to_movein, fmedian, "time_to_move_in"),
      "Moved into Housing (HoHs)" = 1,
      "Entered from Place Not Meant for Habitation (HoHs/Adults)" = calc_by_hh_group(metric_specific_datasets$entered_non_habitat, calc_entered_non_habitat_pct),  
      "Entered from Permanent Housing Situation (HoHs/Adults)" = 1,
      "Zero Income at Entry (HoHs/Adults)" = 1,
      "Income Growth from Entry to Exit (HoHs/Adults)" = 1,
      "Non-Cash Benefits Growth from Entry to Exit (HoHs/Adults)" = 1,
      "Successful Exits (All Clients)" = 1,
      "CE Assessed Households" = 1,
      "Current Living Situation Records: Total" = 1
    )
  )
}
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

output$metricsDT <- renderDT({
  req(session$userData$valid_file() == 1)
  
  metrics_data <- qDT(metricsData())
  
  exportTestValues(metricsData = metrics_data)
  
  datatable(
    client_count_summary_df() %>%
      nice_names(),
    rownames = FALSE,
    filter = 'none',
    options = list(dom = 't'),
    style = "default"
  )
})