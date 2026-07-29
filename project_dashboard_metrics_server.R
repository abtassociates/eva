# ==========================================
# 1. HELPER CONSTANTS, GROUPS & FORMATTERS
# ==========================================

groups <- list(
  "All Household Types"    = c("AOminusUY", "ACminusPY", "CO", "UN", "PY", "UY"),
  "Adult-Only Households"  = c("AOminusUY", "UY"),
  "Adult-Child Households" = c("ACminusPY", "PY"),
  "Child Only Households"  = c("CO"),
  "Unknown Households"     = c("UN")
)

# Helper for standard grouping calculations
calc_by_hh_group <- function(data, func, col = NULL) {
  sapply(groups, function(g) {
    if (is.null(data) || fnrow(data) == 0) return(NA_real_)
    sub_dt <- data[HouseholdType %in% g]
    if (fnrow(sub_dt) == 0) return(NA_real_)
    
    if (!is.null(col)) {
      vals <- sub_dt[[col]]
      func(vals)
    } else {
      func(sub_dt)
    }
  })
}

# Metric formatters
format_val <- function(val, unit_type = "clients") {
  if (is.na(val) || is.null(val)) return("-")
  
  if(unit_type %in% c("days", "clients", "households", "people", "records", "enrollments"))
    glue::glue("{comma(val, accuracy = ifelse(val %% 1 == 0, 1, 0.1))} {unit_type}")
  else if(unit_type == "pct")
    percent(val, accuracy = 0.1)
  else
    as.character(val)
}


# Metric formatters
metric_units <- list(
  "Total Clients Served"                                      = "clients",
  "Total Households Served"                                   = "households",
  "Average Households Size"                                   = "people",
  "Average Length of Participation (All Clients)"             = "days",
  "Median Length of Participation (All Clients)"              = "days",
  "Average Length of Stay in Residence (All Clients)"         = "days",
  "Median Length of Stay in Residence (All Clients)"          = "days",
  "Average Time to Housing Move-In (All Clients)"             = "days",
  "Median Time to Housing Move-In (All Clients)"              = "days",
  "Moved into Housing (HoHs)"                                 = "pct",
  "Entered from Place Not Meant for Habitation (HoHs/Adults)" = "pct",
  "Entered from Permanent Housing Situation (HoHs/Adults)"    = "pct",
  "Zero Income at Entry (HoHs/Adults)"                        = "pct",
  "Income Growth from Entry to Exit (HoHs/Adults)"            = "pct",
  "Non-Cash Benefits Growth from Entry to Exit (HoHs/Adults)" = "pct",
  "Successful Exits (All Clients)"                            = "pct",
  "CE Assessed Households"                                    = "households",
  "Current Living Situation Records: Total"                   = "records"
)

# ==========================================
# 2. VALUE BOX GENERATORS & KEYS
# ==========================================
# ===========================================================================================================================================================================================
# Metric                                                       | lh_residential_project_types | ph_project_types | non_res_project_types (excl. ce & hp) | ce_project_type | hp_project_type
# -------------------------------------------------------------+------------------------------+------------------+---------------------------------------+-----------------+------------------
# Total Clients Served                                         | X                            | X                | X                                     |                 | X
# Total Households Served                                      | X                            | X                | X                                     |                 | X
# Length of Stay in Residence (All Clients)                    | X                            | X                |                                       |                 |
# Time to Housing Move-In (All Clients)                        |                              | X                |                                       |                 |
# Length of Participation (All Clients)                        |                              |                  | X                                     | X               | X
# Entered from Place Not Meant for Habitation (HoHs/Adults)    | X                            |                  | X                                     | X               |
# Entered from Permanent Housing Situation (HoHs/Adults)       | X                            |                  |                                       | X               |
# Zero Income at Entry (HoHs/Adults)                           |                              |                  |                                       |                 | X
# Income Growth from Entry to Exit (HoHs/Adults)               |                              | X                |                                       |                 | X
# Successful Exits (All Clients)                               | X                            | X                | X                                     | X
get_summary_metric_keys <- function(selected_project_type) {
  if (selected_project_type %in% lh_residential_project_types) {
    c("total_clients", "total_households", "los", "entered_non_habitat", "entered_permanent", "successful_exits")
  } else if (selected_project_type %in% ph_project_types) {
    c("total_clients", "total_households", "los", "movein_time", "income_growth", "successful_exits")
  } else if (selected_project_type == ce_project_type) {
    c("lop", "entered_non_habitat", "entered_permanent", "successful_exits", "ce_assessments", "cls_records")
  } else if (selected_project_type == hp_project_type) {
    c("total_clients", "total_households", "lop", "zero_income", "income_growth", "successful_exits")
  } else {
    c("total_clients", "total_households", "lop", "entered_non_habitat", "successful_exits", "cls_records")
  }
}

create_metric_value_box <- function(metric_key, m_data) {
  switch(
    metric_key,
    
    "total_clients" = value_box(
      class = "project_dashboard_valbox",
      title = "Total Clients Served",
      value = tagList(
        div("Total Clients: ", format_val(m_data$total_clients$val, "clients")),
        div("Missing ID: ", format_val(m_data$total_clients$nmiss, "clients"))
      ),
      showcase = bs_icon("people"),
      id = "total_clients_box"
    ),
    
    "total_households" = value_box(
      class = "project_dashboard_valbox",
      title = "Total Households Served",
      value = tagList(
        div("Total Households: ", format_val(m_data$total_households$val, "households")),
        div("Missing HouseholdID: ", format_val(m_data$total_households$nmiss, "enrollments"))
      ),
      showcase = bs_icon("house"),
      id = "total_households_box"
    ),
    
    "los" = value_box(
      class = "project_dashboard_valbox",
      title = "Length of Stay in Residence (All Clients)",
      value = tagList(
        div("Average: ", format_val(fcoalesce(m_data$los$avg, 0), "days")),
        div("Median: ", format_val(fcoalesce(m_data$los$median, 0), "days")),
        div("Missing LOS: ", format_val(m_data$los$nmiss, "enrollments"))
      ),
      showcase = bs_icon("building-add"),
      id = "los_box"
    ),
    
    "movein_time" = value_box(
      class = "project_dashboard_valbox",
      title = "Time to Housing Move-In (All Clients)",
      value = tagList(
        div("Average: ", format_val(fcoalesce(m_data$movein_time$avg, 0), "days")),
        div("Median: ", format_val(fcoalesce(m_data$movein_time$median, 0), "days")),
        div("Missing MoveInDate: ", format_val(m_data$movein_time$nmiss, "enrollments"))
      ),
      showcase = bs_icon("clock-history"),
      id = "movein_time_box"
    ),
    
    "lop" = value_box(
      class = "project_dashboard_valbox",
      title = "Length of Participation (All Clients)",
      value = tagList(
        div("Average: ", format_val(fcoalesce(m_data$lop$avg, 0), "days")),
        div("Median: ", format_val(fcoalesce(m_data$lop$median, 0), "days")),
        div("Missing LOP: ", format_val(m_data$lop$nmiss, "enrollments"))
      ),
      showcase = bs_icon("calendar-range"),
      id = "lop_box"
    ),
    
    "entered_non_habitat" = value_box(
      class = "project_dashboard_valbox",
      title = "Entered from Place Not Meant for Habitation (HoHs/Adults)",
      value = tagList(
        div("Percent of all HoHs/Adults: ", format_val(m_data$entered_non_habitat$pct, "pct")),
        div("Missing LivingSituation: ", format_val(m_data$entered_non_habitat$nmiss, "enrollments"))
      ),
      showcase = bs_icon("signpost-split"),
      id = "entered_non_habitat_box"
    ),
    
    "entered_permanent" = value_box(
      class = "project_dashboard_valbox",
      title = "Entered from Permanent Housing Situation (HoHs/Adults)",
      value = tagList(
        div("Percent of all HoHs/Adults: ", format_val(m_data$entered_permanent$pct, "pct")),
        div("Missing LivingSituation: ", format_val(m_data$entered_permanent$nmiss, "enrollments"))
      ),
      showcase = bs_icon("house-check"),
      id = "entered_permanent_box"
    ),
    
    "zero_income" = value_box(
      class = "project_dashboard_valbox",
      title = "Zero Income at Entry (HoHs/Adults)",
      value = tagList(
        div("Percent of all HoHs/Adults: ", format_val(m_data$zero_income$pct, "pct")),
        div("Missing IncomeFromAnySource: ", format_val(m_data$zero_income$nmiss, "enrollments"))
      ),
      showcase = bs_icon("wallet2"),
      id = "zero_income_box"
    ),
    
    "income_growth" = value_box(
      class = "project_dashboard_valbox",
      title = "Income Growth from Entry to Exit (HoHs/Adults)",
      value = tagList(
        div("Percent of all exited HoHs/Adults: ", format_val(m_data$income_growth$pct, "pct")),
        div("Missing TotalMonthlyIncome: ", format_val(m_data$income_growth$nmiss, "enrollments"))
      ),
      showcase = bs_icon("graph-up-arrow"),
      id = "income_growth_box"
    ),
    
    "successful_exits" = value_box(
      class = "project_dashboard_valbox",
      title = "Successful Exits (All Clients)",
      value = tagList(
        div("Percent of all exited clients: ", format_val(m_data$successful_exits$pct, "pct")),
        div("Missing Destination: ", format_val(m_data$successful_exits$nmiss, "enrollments"))
      ),
      showcase = bs_icon("check-circle"),
      id = "successful_exits_box"
    ),
    
    "ce_assessments" = value_box(
      class = "project_dashboard_valbox",
      title = "CE Assessed Households",
      value = div("Number of CE Assessments: ", format_val(m_data$ce_assessments$val, "households")),
      showcase = bs_icon("clipboard-check"),
      id = "ce_assessments_box"
    ),
    
    "cls_records" = value_box(
      class = "project_dashboard_valbox",
      title = "Current Living Situation Records: Total",
      value = div("Total CLS Records: ", format_val(m_data$cls_records$val, "records")),
      showcase = bs_icon("geo-alt"),
      id = "cls_records_box"
    )
  )
}

# ==========================================
# 3. DATASET PREPARATION FUNCTIONS
# ==========================================

get_latest_enrollments <- function() {
  req(input$dateRangeCount)
  
  enrollment_w_project_type <- session$userData$Enrollment |> 
    fsubset(EntryDate %between% input$dateRangeCount | ExitAdjust %between% input$dateRangeCount) |>
    join(session$userData$Project0 |> fselect(ProjectID, ProjectType), on = "ProjectID") |>
    fselect(
      PersonalID, EnrollmentID, HouseholdID, HouseholdType, ProjectType, 
      EntryDate, MoveInDateAdjust, ExitDate, 
      AgeAtEntry, LivingSituation, RelationshipToHoH, LengthOfStay
    )
  
  if (fnrow(enrollment_w_project_type) > 0) {
    enrollment_w_project_type |>
      fgroup_by(PersonalID, EntryDate) |>
      fslice(how = "last") |>
      fungroup()
  } else {
    data.table()
  }
}

get_metric_specific_datasets <- function(latest_enrollments) {
  
  ## 1. Household Size ----
  avg_household_size_dt <- latest_enrollments |>
    fgroup_by(HouseholdID) |>
    fmutate(hh_size = GRPN()) |>
    fungroup() |>
    fselect(HouseholdID, hh_size, HouseholdType) |>
    funique()
  
  ## 2. Length of Participation ----
  length_of_participation_dt <- latest_enrollments |> 
    fmutate(
      length_of_participation = as.integer(difftime(
        fifelse(is.na(ExitDate), session$userData$meta_HUDCSV_Export_End, ExitDate), 
        EntryDate, 
        unit = "days"
      ))
    )
  
  ## 3. Length of Stay in Residence ----
  los_dt <- latest_enrollments |>
    fsubset(
      ProjectType %in% project_types_w_beds, 
      EnrollmentID, LengthOfStay, HouseholdType
    )
  
  ## 4. Time to Housing Move-In ----
  time_to_movein_dt <- latest_enrollments |> 
    fsubset(
      ProjectType %in% ph_project_types & 
        fcoalesce(MoveInDateAdjust, no_end_date) <= session$userData$meta_HUDCSV_Export_End,
      EnrollmentID, EntryDate, MoveInDateAdjust, HouseholdType
    ) |>
    fmutate(
      time_to_move_in = as.integer(difftime(MoveInDateAdjust, EntryDate, units = "days"))
    )
  
  ## 5. Moved into Housing ----
  moved_into_housing_dt <- latest_enrollments |>
    fsubset(RelationshipToHoH == 1 & ProjectType %in% ph_project_types) |>
    fmutate(
      moved_into_housing = fcoalesce(MoveInDateAdjust, no_end_date) <= session$userData$meta_HUDCSV_Export_End
    )
  
  ## 6. Entered From ----
  entered_from_dt <- latest_enrollments |>
    fsubset(
      ProjectType %in% c(lh_residential_project_types, setdiff(non_res_project_types, hp_project_type)) &
        (RelationshipToHoH == 1 | AgeAtEntry >= 18), 
      EnrollmentID, LivingSituation, HouseholdType
    )
  
  ## 7. Zero Income at Entry ----
  zero_income_dt <- session$userData$IncomeBenefits |>
    fsubset(
      DataCollectionStage == 1 & !(IncomeFromAnySource %in% c(8,9,99) | is.na(IncomeFromAnySource)), 
      EnrollmentID, IncomeFromAnySource
    ) |>
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
  
  ## 8. Successful Exits ----
  successful_exit_dt <- session$userData$Exit |>
    fselect(EnrollmentID, Destination) |>
    join(
      latest_enrollments |> fsubset(!is.na(ExitDate), EnrollmentID, ProjectType, HouseholdType),
      on = "EnrollmentID",
      how = "inner"
    ) |>
    fmutate(
      successful_exit = as.integer(fcase(
        ProjectType == out_project_type, Destination %in% setdiff(c(100:499), c(116, 206, 207, 329)),
        ProjectType %in% c(es_ee_project_type, es_nbn_project_type, th_project_type), Destination %in% c(332, 400:499),
        default = Destination %in% c(400:499)
      ))
    ) |>
    fsubset(
      !(
        (Destination %in% c(24, 206) & ProjectType %in% lh_ph_hp_project_types) |
          (Destination %in% c(215, 225) & ProjectType %in% setdiff(lh_ph_hp_project_types, out_project_type)) |
          (Destination == 329 & ProjectType == out_project_type)
      )
    )
  
  ## 9. Income & Benefits Growth ----
  income_growth_latest_enrl <- latest_enrollments |>
    fsubset(
      ProjectType %in% c(ph_project_types, hp_project_type) &
        !is.na(ExitDate) &
        (RelationshipToHoH == 1 | AgeAtEntry >= 18), 
      EnrollmentID, HouseholdType
    )
  
  get_growth_dt <- function(ib_dt, var_name) {
    if (fnrow(ib_dt) > 0 && fnrow(income_growth_latest_enrl) > 0) {
      ib_dt |>
        fsubset(DataCollectionStage %in% c(1, 3)) |>
        fselect(EnrollmentID, DataCollectionStage, val = get(var_name)) |>
        join(income_growth_latest_enrl, on = "EnrollmentID", how = "inner") |>
        fgroup_by(EnrollmentID) |>
        fmutate(
          at_entry = ffirst(fifelse(DataCollectionStage == 1, val, NA_real_)),
          at_exit  = flast(fifelse(DataCollectionStage == 3, val, NA_real_))
        ) |>
        fslice(how = "first") |>
        fungroup() |>
        fmutate(has_growth = as.integer(at_exit > at_entry))
    } else {
      data.table()
    }
  }
  
  income_growth_dt <- get_growth_dt(session$userData$IncomeBenefits, "TotalMonthlyIncome")
  
  non_cash_dt <- session$userData$IncomeBenefits |> 
    fmutate(non_cash_benefits = SNAP + WIC + TANFChildCare + TANFTransportation + OtherTANF + OtherBenefitsSource)
  non_cash_growth_dt <- get_growth_dt(non_cash_dt, "non_cash_benefits")
  
  ## 10. CE Assessed Households ----
  ce_assessments_dt <- latest_enrollments |>
    fsubset(!is.na(EnrollmentID))
  
  ## 11. Current Living Situation Records ----
  cls_records_dt <- session$userData$CurrentLivingSituation |>
    join(latest_enrollments |> fselect(EnrollmentID, HouseholdType, ProjectType), on = "EnrollmentID")
  
  return(list(
    latest_enrollments      = latest_enrollments,
    avg_hh_size             = avg_household_size_dt,
    length_of_participation = length_of_participation_dt,
    los                     = los_dt,
    time_to_movein          = time_to_movein_dt,
    moved_into_housing      = moved_into_housing_dt,
    entered_from            = entered_from_dt,
    zero_income             = zero_income_dt,
    successful_exit         = successful_exit_dt,
    income_growth           = income_growth_dt,
    non_cash_growth         = non_cash_growth_dt,
    ce_assessments          = ce_assessments_dt,
    cls_records             = cls_records_dt
  ))
}

# ==========================================
# 4. DETAIL TAB DATA TABLE GENERATION
# ==========================================

get_details_by_hh_type <- function(m, selected_project_type) {
  # =====================================================================================================================================================================================================================
  # Metric                                                       | All Household Types | Adult Only Households | Adult-Child Households | Child Only Households | Unknown Households | Metric Data Table Display - Applicable Project Types
  # -------------------------------------------------------------+---------------------+-----------------------+------------------------+-----------------------+--------------------+-----------------------------------------------------------------------------
  # Total Clients Served                                         | #                   | #                     | #                      | #                     | #                  | All
  # Total Households Served                                      | #                   | #                     | #                      | #                     | #                  | All
  # Average Households Size                                      | #                   | #                     | #                      | #                     | #                  | All
  # Average Length of Participation (All Clients)                | #                   | #                     | #                      | #                     | #                  | All
  # Median Length of Participation (All Clients)                 | #                   | #                     | #                      | #                     | #                  | All
  # Average Length of Stay in Residence (All Clients)            | #                   | #                     | #                      | #                     | #                  | project_types_w_beds
  # Median Length of Stay in Residence (All Clients)             | #                   | #                     | #                      | #                     | #                  | project_types_w_beds
  # Average Time to Housing Move-In (All Clients)                | #                   | #                     | #                      | #                     | #                  | ph_project_types
  # Median Time to Housing Move-In (All Clients)                 | #                   | #                     | #                      | #                     | #                  | ph_project_types
  # Moved into Housing (HoHs)                                    | %                   | %                     | %                      | %                     | %                  | ph_project_types
  # Entered from Place Not Meant for Habitation (HoHs/Adults)    | %                   | %                     | %                      | %                     | %                  | All
  # Entered from Permanent Housing Situation (HoHs/Adults)       | %                   | %                     | %                      | %                     | %                  | All
  # Zero Income at Entry (HoHs/Adults)                           | %                   | %                     | %                      | %                     | %                  | All
  # Income Growth from Entry to Exit (HoHs/Adults)               | %                   | %                     | %                      | %                     | %                  | All
  # Non-Cash Benefits Growth from Entry to Exit (HoHs/Adults)    | %                   | %                     | %                      | %                     | %                  | All
  # Successful Exits (All Clients)                               | %                   | %                     | %                      | %                     | %                  | All
  # CE Assessed Households                                       | #                   | #                     | #                      | #                     | #                  | ce_project_type, any other project that has CE Assessment data within report
  # Current Living Situation Records: Total                      | #                   | #                     | #                      | #                     | #                  | es_nbn_project_type, setdiff(non_res_project_types, hp_project_type)
  # =====================================================================================================================================================================================================================
  pct_calc <- function(dt, num_col) {
    if (is.null(dt) || fnrow(dt) == 0) return(NA_real_)
    fsum(dt[[num_col]]) / fnrow(dt)
  }
  
  # List of metrics that must be formatted as percentages
  pct_metrics <- c(
    "Moved into Housing (HoHs)",
    "Entered from Place Not Meant for Habitation (HoHs/Adults)",
    "Entered from Permanent Housing Situation (HoHs/Adults)",
    "Zero Income at Entry (HoHs/Adults)",
    "Income Growth from Entry to Exit (HoHs/Adults)",
    "Non-Cash Benefits Growth from Entry to Exit (HoHs/Adults)",
    "Successful Exits (All Clients)"
  )
  
  # Base calculations for all 18 metrics
  detail_list <- list(
    "Total Clients Served"                                      = calc_by_hh_group(m$latest_enrollments, fnunique, "PersonalID"),
    "Total Households Served"                                   = calc_by_hh_group(m$latest_enrollments, fnunique, "HouseholdID"),
    "Average Households Size"                                   = calc_by_hh_group(m$avg_hh_size, fmean, "hh_size"),
    "Average Length of Participation (All Clients)"             = calc_by_hh_group(m$length_of_participation, fmean, "length_of_participation"),
    "Median Length of Participation (All Clients)"              = calc_by_hh_group(m$length_of_participation, fmedian, "length_of_participation"),
    "Average Length of Stay in Residence (All Clients)"         = calc_by_hh_group(m$los, fmean, "LengthOfStay"),
    "Median Length of Stay in Residence (All Clients)"          = calc_by_hh_group(m$los, fmedian, "LengthOfStay"),
    "Average Time to Housing Move-In (All Clients)"             = calc_by_hh_group(m$time_to_movein, fmean, "time_to_move_in"),
    "Median Time to Housing Move-In (All Clients)"              = calc_by_hh_group(m$time_to_movein, fmedian, "time_to_move_in"),
    "Moved into Housing (HoHs)"                                 = calc_by_hh_group(m$moved_into_housing, function(d) pct_calc(d, "moved_into_housing")),
    "Entered from Place Not Meant for Habitation (HoHs/Adults)" = calc_by_hh_group(m$entered_from, function(d) fsum(d$LivingSituation == 116L) / fnrow(d[!is.na(LivingSituation)])),
    "Entered from Permanent Housing Situation (HoHs/Adults)"    = calc_by_hh_group(m$entered_from, function(d) fsum(d$LivingSituation %in% 400:499) / fnrow(d[!is.na(LivingSituation)])),
    "Zero Income at Entry (HoHs/Adults)"                        = calc_by_hh_group(m$zero_income, function(d) fsum(d$IncomeFromAnySource == 0) / fnrow(d)),
    "Income Growth from Entry to Exit (HoHs/Adults)"            = calc_by_hh_group(m$income_growth, function(d) pct_calc(d, "has_growth")),
    "Non-Cash Benefits Growth from Entry to Exit (HoHs/Adults)" = calc_by_hh_group(m$non_cash_growth, function(d) pct_calc(d, "has_growth")),
    "Successful Exits (All Clients)"                            = calc_by_hh_group(m$successful_exit, function(d) pct_calc(d, "successful_exit")),
    "CE Assessed Households"                                    = calc_by_hh_group(m$ce_assessments, fnunique, "HouseholdID"),
    "Current Living Situation Records: Total"                   = calc_by_hh_group(m$cls_records, fnrow)
  )
  
  dt_res <- rbindlist(lapply(names(detail_list), function(m_name) {
    vals <- detail_list[[m_name]]
    unit <- metric_units[[m_name]]
    formatted_vals <- sapply(vals, format_val, unit_type = unit)
    as.list(c(Metric = m_name, formatted_vals))
  }))
  
  
  # Row filtering based on Project Type rules (Image 1)
  applicable_rules <- list(
    "Average Length of Stay in Residence (All Clients)"         = selected_project_type %in% project_types_w_beds,
    "Median Length of Stay in Residence (All Clients)"          = selected_project_type %in% project_types_w_beds,
    "Average Time to Housing Move-In (All Clients)"             = selected_project_type %in% ph_project_types,
    "Median Time to Housing Move-In (All Clients)"              = selected_project_type %in% ph_project_types,
    "Moved into Housing (HoHs)"                                 = selected_project_type %in% ph_project_types,
    "CE Assessed Households"                                    = selected_project_type == ce_project_type,
    "Current Living Situation Records: Total"                   = selected_project_type %in% c(es_nbn_project_type, setdiff(non_res_project_types, hp_project_type))
  )
  
  keep_rows <- sapply(dt_res$Metric, function(m) {
    if (m %in% names(applicable_rules)) applicable_rules[[m]] else TRUE
  })
  
  return(dt_res[keep_rows])
}

# ==========================================
# 5. SHINY REACTIVE PIPELINES & RENDERING
# ==========================================

# Active Datasets Reactive Pipeline
metric_datasets <- reactive({
  latest_enrl <- get_latest_enrollments()
  get_metric_specific_datasets(latest_enrl)
})

selected_proj_type <- reactive({
  req(input$currentProviderList)
  session$userData$Project0[ProjectName == input$currentProviderList, ProjectType][1]
})

# Consolidated Card Metrics Calculation
summary_metrics <- reactive({
  m <- metric_datasets()
  latest_enrl <- m$latest_enrollments
  
  list(
    total_clients = list(
      val   = fnunique(latest_enrl$PersonalID),
      nmiss = fsum(is.na(latest_enrl$PersonalID))
    ),
    total_households = list(
      val   = fnunique(latest_enrl$HouseholdID),
      nmiss = fsum(is.na(latest_enrl$HouseholdID))
    ),
    los = list(
      avg    = fmean(m$los$LengthOfStay),
      median = fmedian(m$los$LengthOfStay),
      nmiss  = fsum(is.na(m$los$LengthOfStay))
    ),
    movein_time = list(
      avg    = fmean(m$time_to_movein$time_to_move_in),
      median = fmedian(m$time_to_movein$time_to_move_in),
      nmiss  = fsum(is.na(m$time_to_movein$MoveInDateAdjust))
    ),
    lop = list(
      avg    = fmean(m$length_of_participation$length_of_participation),
      median = fmedian(m$length_of_participation$length_of_participation),
      nmiss  = fsum(is.na(m$length_of_participation$ExitDate))
    ),
    entered_non_habitat = list(
      pct   = if (fnrow(m$entered_from[!is.na(LivingSituation)]) > 0) fsum(m$entered_from$LivingSituation == 116L) / fnrow(m$entered_from[!is.na(LivingSituation)]) else NA,
      nmiss = fsum(is.na(m$entered_from$LivingSituation))
    ),
    entered_permanent = list(
      pct   = if (fnrow(m$entered_from[!is.na(LivingSituation)]) > 0) fsum(m$entered_from$LivingSituation %in% 400:499) / fnrow(m$entered_from[!is.na(LivingSituation)]) else NA,
      nmiss = fsum(is.na(m$entered_from$LivingSituation))
    ),
    zero_income = list(
      pct   = if (fnrow(m$zero_income) > 0) fsum(is.na(m$zero_income$IncomeFromAnySource) | m$zero_income$IncomeFromAnySource == 0) / fnrow(m$zero_income) else NA,
      nmiss = fsum(is.na(m$zero_income$IncomeFromAnySource))
    ),
    income_growth = list(
      pct   = if (fnrow(m$income_growth) > 0) fsum(m$income_growth$has_growth) / fnrow(m$income_growth) else NA,
      nmiss = fsum(is.na(m$income_growth$at_entry) | is.na(m$income_growth$at_exit))
    ),
    successful_exits = list(
      pct   = if (fnrow(m$successful_exit) > 0) fsum(m$successful_exit$successful_exit) / fnrow(m$successful_exit) else NA,
      nmiss = fsum(is.na(m$successful_exit$Destination))
    ),
    ce_assessments = list(
      val = fnunique(m$ce_assessments$HouseholdID)
    ),
    cls_records = list(
      val = fnrow(m$cls_records)
    )
  )
})

# Summary UI Card Output
output$summary_value_boxes <- renderUI({
  req(session$userData$valid_file() == 1, input$currentProviderList)
  
  proj_type   <- selected_proj_type()
  m_calculated <- summary_metrics()
  
  keys_to_display <- get_summary_metric_keys(proj_type)
  
  box_list <- lapply(keys_to_display, function(key) {
    create_metric_value_box(key, m_calculated)
  })
  
  layout_column_wrap(
    width = "30%",
    gap = "1rem",
    !!!box_list
  )
})

# Detail Table Output
output$metrics_detail <- renderDT({
  req(session$userData$valid_file() == 1, input$currentProviderList)
  
  dt_detail <- get_details_by_hh_type(metric_datasets(), selected_proj_type())
  
  datatable(
    dt_detail,
    rownames = FALSE,
    filter   = 'none',
    options  = list(dom = 't', pageLength = 20),
    style    = "default"
  )
})