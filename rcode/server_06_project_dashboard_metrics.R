# ==========================================
# 1. HELPER CONSTANTS & FORMATTERS
# ==========================================

groups <- list(
  "All Household Types"    = c("AOminusUY", "ACminusPY", "CO", "UN", "PY", "UY"),
  "Adult-Only Households"  = c("AOminusUY", "UY"),
  "Adult-Child Households" = c("ACminusPY", "PY"),
  "Child Only Households"  = c("CO"),
  "Unknown Households"     = c("UN")
)

exclude_vals <- c(8, 9, 99, NA)

format_val <- function(val, unit_type = "clients") {
  if(!is.list(val)) 
    if(allNA(val)) return("-")
  
  if (is.null(val) || length(val) == 0) return("-")
  
  if (unit_type %in% c("days", "assessments", "people", "records", "enrollments")) {
    paste0(comma(val, accuracy = ifelse(val %% 1 == 0, 1, 0.1)), " ", unit_type)
  } else if(unit_type %in% c("clients","households")) {
    paste0(comma(val, accuracy = ifelse(val %% 1 == 0, 1, 0.1)))
  } else if (unit_type == "pct") {
    if(val == 0) "0%"
    else if(val == 100) "100%"
    else percent(val, accuracy = 0.1)
  } else {
    as.character(val)
  }
}

# Helper to evaluate metric calculations dynamically
eval_metric_kpi <- function(metric_name, metric_dataset) {
  def <- METRIC_DEFINITIONS[[metric_name]]
  
  if (is.null(metric_dataset) || fnrow(metric_dataset) == 0) {
    return(list(val = NA_real_, nmiss = NA_real_))
  }
  
  val <- def$calc_func(metric_dataset)
  
  nmiss <- if (!is.null(def$calc_nmiss)) def$calc_nmiss(metric_dataset) else NA_real_
  
  list(val = val, nmiss = nmiss)
}

# ==========================================
# 2. MASTER METRIC DEFINITIONS (SUMMARY & DETAIL)
# ==========================================

get_leavers <- function(dt) {
  dt |> fsubset(ExitAdjust %between% input$dateRangeCount)
}
get_stayers <- function(dt) {
  reportEnd <- input$dateRangeCount[2]
  dt |> fsubset(EntryDate <= reportEnd & ExitAdjust > reportEnd)
}

METRIC_DEFINITIONS <- list(
  # --- 1. CLIENTS & HOUSEHOLDS ---
  "Clients Served" = list(
    dt_key         = "total_clients",
    unit           = "clients",
    calc_func      = function(dt) fnunique(dt[RelationshipToHoH == 1 | AgeAtReportStart > 17]$PersonalID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) pt %in% setdiff(all_project_types, ce_project_type),
    export_only    = FALSE
  ),
  "  Adults Served (age 18 or over)" = list(
    dt_key         = "total_clients",
    unit           = "clients",
    calc_func      = function(dt) fnunique(dt[AgeGroup == "Adult"]$PersonalID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Children Served (under age 18)" = list(
    dt_key         = "total_clients",
    unit           = "clients",
    calc_func      = function(dt) fnunique(dt[AgeGroup == "Child"]$PersonalID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Clients Served with Unknown Age" = list(
    dt_key         = "total_clients",
    unit           = "clients",
    calc_func      = function(dt) fnunique(dt[AgeGroup == "Unknown"]$PersonalID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Heads of Household and Adults Served (HoHs/Adults)" = list(
    dt_key         = "total_clients",
    unit           = "clients",
    calc_func      = function(dt) fnunique(dt[RelationshipToHoH == 1]$PersonalID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Clients Served - Leavers" = list(
    dt_key         = "total_clients",
    unit           = "clients",
    calc_func      = function(dt) fnunique(get_leavers(dt)$PersonalID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Clients Served - Stayers" = list(
    dt_key         = "total_clients",
    unit           = "clients",
    calc_func      = function(dt) fnunique(get_stayers(dt)$PersonalID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "Households Served" = list(
    dt_key         = "total_households_served",
    unit           = "households",
    calc_func      = function(dt) fnunique(dt$HouseholdID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) pt %in% setdiff(all_project_types, ce_project_type),
    export_only    = FALSE
  ),
  "  Households Served - Leavers" = list(
    dt_key         = "total_households_served",
    unit           = "households",
    calc_func      = function(dt) fnunique(get_leavers(dt)$HouseholdID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Households Served - Stayers" = list(
    dt_key         = "total_clients",
    unit           = "households",
    calc_func      = function(dt) fnunique(get_stayers(dt)$HouseholdID),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Average Household Size" = list(
    dt_key         = "avg_hh_size",
    unit           = "people",
    calc_func      = function(dt) fmean(dt$hh_size),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "Households who Moved into Housing" = list(
    dt_key         = "moved_into_housing",
    unit           = "pct",
    calc_func      = function(dt) if (fnrow(dt) > 0) fsum(dt$moved_into_housing) / fnunique(dt$EnrollmentID) else NA_real_,
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) FALSE,
    export_only    = FALSE
  ),
  "  Households who Moved into Housing - Leavers" = list(
    dt_key         = "moved_into_housing",
    unit           = "pct",
    calc_func      = function(dt) fsum(get_leavers(dt)$moved_into_housing),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Households who Moved into Housing - Stayers" = list(
    dt_key         = "moved_into_housing",
    unit           = "pct",
    calc_func      = function(dt) fsum(get_stayers(dt)$moved_into_housing),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Households who Exited without Moving Into Housing" = list(
    dt_key         = "moved_into_housing",
    unit           = "households",
    calc_func      = function(dt) fsum(!is.na(dt$ExitDate) & !dt$moved_into_housing),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  
  # --- 2. PRIOR LIVING SITUATIONS & INCOME ---
  "Entered from Place Not Meant for Habitation (HoHs/Adults)" = list(
    dt_key         = "entered_non_habitat",
    unit           = "pct",
    calc_func      = function(dt) {
      denom <- fnrow(dt)
      if (denom > 0) fsum(dt$entered_from_place_not_meant) / denom else NA_real_
    },
    calc_func_det  = function(dt) fsum(dt$entered_from_place_not_meant),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) pt %in% c(lh_residential_project_types, setdiff(non_res_project_types, hp_project_type)),
    export_only    = FALSE
  ),
  "Entered from Permanent Housing Situation (HoHs/Adults)" = list(
    dt_key         = "entered_permanent",
    unit           = "pct",
    calc_func      = function(dt) {
      denom <- fnrow(dt)
      if (denom > 0) fsum(dt$entered_from_ph) / denom else NA_real_
    },
    calc_func_det  = function(dt) fsum(dt$entered_from_ph),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) pt %in% c(lh_residential_project_types, ce_project_type),
    export_only    = FALSE
  ),
  "Zero Income at Entry (HoHs/Adults)" = list(
    dt_key         = "zero_income",
    unit           = "pct",
    calc_func      = function(dt) {
      denom <- fsum(dt$IncomeFromAnySource %in% c(0, 1))
      if (denom > 0) fsum(dt$zero_income) / denom else NA_real_
    },
    calc_func_det  = function(dt) fsum(dt$zero_income),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) pt == hp_project_type,
    export_only    = FALSE
  ),
  "Income Growth from Entry to Exit (HoHs/Adults)" = list(
    dt_key         = "income_growth",
    unit           = "pct",
    calc_func      = function(dt) {
      denom <- fsum(dt$denom)
      if (denom > 0) fsum(dt$has_growth) / denom else NA_real_
    },
    calc_func_det  = function(dt) fsum(dt$has_growth),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) pt %in% c(ph_project_types, hp_project_type),
    export_only    = FALSE
  ),
  "  Excluded from Income Growth Metric Due to Unknown/Missing Income at Entry or Exit (HoHs/Adults)" = list(
    dt_key         = "income_growth",
    unit           = "clients",
    calc_func      = function(dt) fsum(dt$nmiss),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  
  # --- 3. EXITS ---
  "Successful Exits (All Clients)" = list(
    dt_key         = "successful_exit",
    unit           = "pct",
    calc_func      = function(dt) {
      denom <- fsum(dt$denom)
      if (denom > 0) fsum(dt$successful_exit) / denom else NA_real_
    },
    calc_func_det  = function(dt) {fsum(dt$successful_exit)},
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) TRUE,
    export_only    = FALSE
  ),
  "  Total Exits (Including Unknown/Missing Destinations)" = list(
    dt_key         = "successful_exit",
    unit           = "clients",
    calc_func      = function(dt) fnrow(dt),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Excluded from Successful Exits Metric Due to Specific Exit Destination" = list(
    dt_key         = "successful_exit",
    unit           = "clients",
    calc_func      = function(dt) fsum(dt$nmiss),
    applies        = function(pt) TRUE,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  
  # --- 4. LENGTH OF PARTICIPATION ---
  "Average Length of Participation (All Clients)" = list(
    dt_key         = "length_of_participation",
    unit           = "days",
    calc_func      = function(dt) fmean(dt$length_of_participation),
    applies        = function(pt) pt %in% c(non_res_project_types, ph_project_types),
    show_KPI       = function(pt) pt %in% non_res_project_types,
    export_only    = FALSE
  ),
  "  Average Length of Participation - Leavers" = list(
    dt_key         = "length_of_participation",
    unit           = "days",
    calc_func      = function(dt) fmean(dt[!is.na(ExitDate)]$length_of_participation),
    applies        = function(pt) pt %in% c(non_res_project_types, ph_project_types),
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Average Length of Participation - Stayers" = list(
    dt_key         = "length_of_participation",
    unit           = "days",
    calc_func      = function(dt) fmean(dt[is.na(ExitDate)]$length_of_participation),
    applies        = function(pt) pt %in% c(non_res_project_types, ph_project_types),
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "Median Length of Participation (All Clients)" = list(
    dt_key         = "length_of_participation",
    unit           = "days",
    calc_func      = function(dt) fmedian(dt$length_of_participation),
    applies        = function(pt) pt %in% c(non_res_project_types, ph_project_types),
    show_KPI       = function(pt) pt %in% non_res_project_types,
    export_only    = FALSE
  ),
  "  Median Length of Participation - Leavers" = list(
    dt_key         = "length_of_participation",
    unit           = "days",
    calc_func      = function(dt) fmedian(
      get_leavers(dt)$length_of_participation
    ),
    applies        = function(pt) pt %in% c(non_res_project_types, ph_project_types),
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Median Length of Participation - Stayers" = list(
    dt_key         = "length_of_participation",
    unit           = "days",
    calc_func      = function(dt) fmedian(
      get_stayers(dt)$length_of_participation
    ),
    applies        = function(pt) pt %in% c(non_res_project_types, ph_project_types),
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  
  # --- 5. LENGTH OF STAY IN RESIDENCE ---
  "Average Length of Stay in Residence (All Clients)" = list(
    dt_key         = "los",
    unit           = "days",
    calc_func      = function(dt) fmean(dt$los_res),
    applies        = function(pt) pt %in% project_types_w_beds,
    show_KPI       = function(pt) pt %in% project_types_w_beds,
    export_only    = FALSE
  ),
  "  Average Length of Stay in Residence - Leavers (All Clients)" = list(
    dt_key         = "los",
    unit           = "days",
    calc_func      = function(dt) fmean(dt[!is.na(ExitDate)]$los_res),
    applies        = function(pt) pt %in% project_types_w_beds,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Average Length of Stay in Residence - Stayers (All Clients)" = list(
    dt_key         = "los",
    unit           = "days",
    calc_func      = function(dt) fmean(dt[is.na(ExitDate)]$los_res),
    applies        = function(pt) pt %in% project_types_w_beds,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "Median Length of Stay in Residence (All Clients)" = list(
    dt_key         = "los",
    unit           = "days",
    calc_func      = function(dt) fmedian(dt$los_res),
    applies        = function(pt) pt %in% project_types_w_beds,
    show_KPI       = function(pt) pt %in% project_types_w_beds,
    export_only    = FALSE
  ),
  "  Median Length of Stay in Residence - Leavers (All Clients)" = list(
    dt_key         = "los",
    unit           = "days",
    calc_func      = function(dt) fmedian(dt[!is.na(ExitDate)]$los_res),
    applies        = function(pt) pt %in% project_types_w_beds,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Median Length of Stay in Residence - Stayers (All Clients)" = list(
    dt_key         = "los",
    unit           = "days",
    calc_func      = function(dt) fmedian(dt[is.na(ExitDate)]$los_res),
    applies        = function(pt) pt %in% project_types_w_beds,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  
  # --- 6. TIME TO HOUSING MOVE-IN ---
  "Average Time to Housing Move-In (All Clients)" = list(
    dt_key         = "time_to_movein",
    unit           = "days",
    calc_func      = function(dt) fmean(dt$time_to_move_in),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) pt %in% ph_project_types,
    export_only    = FALSE
  ),
  "  Average Time to Housing Move-In - Leavers (All Clients)" = list(
    dt_key         = "time_to_movein",
    unit           = "days",
    calc_func      = function(dt) fmean(dt[!is.na(ExitDate)]$time_to_move_in),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Average Time to Housing Move-In - Stayers (All Clients)" = list(
    dt_key         = "time_to_movein",
    unit           = "days",
    calc_func      = function(dt) fmean(dt[is.na(ExitDate)]$time_to_move_in),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "Median Time to Housing Move-In (All Clients)" = list(
    dt_key         = "time_to_movein",
    unit           = "days",
    calc_func      = function(dt) fmedian(dt$time_to_move_in),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) pt %in% ph_project_types,
    export_only    = FALSE
  ),
  "  Median Time to Housing Move-In - Leavers (All Clients)" = list(
    dt_key         = "time_to_movein",
    unit           = "days",
    calc_func      = function(dt) fmedian(dt[!is.na(ExitDate)]$time_to_move_in),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "  Median Time to Housing Move-In - Stayers (All Clients)" = list(
    dt_key         = "time_to_movein",
    unit           = "days",
    calc_func      = function(dt) fmedian(dt[is.na(ExitDate)]$time_to_move_in),
    applies        = function(pt) pt %in% ph_project_types,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  
  # --- 7. CE ASSESSMENTS & CLS ---
  "CE Assessed Households (HoHs)" = list(
    dt_key         = "ce_assessments",
    unit           = "assessments",
    calc_func      = function(dt) fnunique(dt$EnrollmentID),
    applies        = function(pt) pt == ce_project_type,
    show_KPI       = function(pt) pt == ce_project_type,
    export_only    = FALSE
  ),
  "  Excluded CE Assessed Households Due to Issues with Assessment Dates or CE Participation Data (HoHs)" = list(
    dt_key         = "ce_assessments",
    unit           = "assessments",
    calc_func      = function(dt) fsum(dt$nmiss),
    applies        = function(pt) pt == ce_project_type,
    show_KPI       = function(pt) FALSE,
    export_only    = TRUE
  ),
  "Current Living Situation Records (HoHs/Adults)" = list(
    dt_key         = "cls_records",
    unit           = "records",
    calc_func      = function(dt) fnunique(dt$CurrentLivingSitID),
    applies        = function(pt) pt %in% c(es_nbn_project_type, setdiff(non_res_project_types, hp_project_type)),
    show_KPI       = function(pt) pt %in% setdiff(project_types_w_cls, es_nbn_project_type),
    export_only    = FALSE
  )
)

# # Generic calculation function for Household Grouping (Table details)
# calc_by_hh_group <- function(metric_name, m_datasets, calc_type) {
#   def <- METRIC_DEFINITIONS[[metric_name]]
#   sub_dt <- m_datasets[[def$dt_key]]
#   
#   vals <- lapply(groups, function(g) {
#     if (is.null(sub_dt) || fnrow(sub_dt) == 0) return(NA_real_)
#     grp_dt <- sub_dt[HHTypeAtReportStart %in% g]
#     if (fnrow(grp_dt) == 0) return(NA_real_)
#     
#     if(calc_type == "detail")
#       def$calc_func_det(grp_dt)
#     else
#       def$calc_func(grp_dt)
#   })
#   
#   if (!is.null(def$calc_nmiss)) {
#     vals["Total Missing"] <- if (!is.null(sub_dt) && fnrow(sub_dt) > 0) def$calc_nmiss(sub_dt) else NA_real_
#   }
#   
#   vals
# }

# Dynamic Value Box Builder
metric_val_box <- function(title, value, showcase, id) {
  value_box(
    class = "project_dashboard_valbox border-primary",
    title = title,
    value = value,
    showcase = showcase,
    id = id,
    theme = "text-primary"
  )
}

create_metric_value_box <- function(box_key, metric_dataset) {
  switch(
    box_key,
    
    "total_clients" = {
      m_tot <- eval_metric_kpi("Clients Served", metric_dataset)
      m_ad  <- eval_metric_kpi("  Adults Served (age 18 or over)", metric_dataset)
      m_ch  <- eval_metric_kpi("  Children Served (under age 18)", metric_dataset)
      m_uk  <- eval_metric_kpi("  Clients Served with Unknown Age", metric_dataset)
      
      metric_val_box(
        title = "Clients Served",
        value = tagList(
          div("Total: ", format_val(m_tot$val, "clients")),
          div("Adults: ", format_val(m_ad$val, "clients")),
          div("Children: ", format_val(m_ch$val, "clients")),
          div("Unknown: ", format_val(m_uk$val, "clients"))
        ),
        showcase = bs_icon("people"),
        id = "total_clients_box"
      )
    },
    
    "total_households_served" = {
      m_tot <- eval_metric_kpi("Households Served", metric_dataset)
      m_ao <- fnunique(metric_dataset[HHGroup == "Adult Only"]$HouseholdID)
      m_ac <- fnunique(metric_dataset[HHGroup == "Adult-Child"]$HouseholdID)
      m_co <- fnunique(metric_dataset[HHGroup == "Child Only"]$HouseholdID)
      m_un <- fnunique(metric_dataset[HHGroup == "Unknown"]$HouseholdID)
      
      metric_val_box(
        title = "Households Served",
        value = tagList(
          div("Total: ", format_val(m_tot$val, "households")),
          div("Adult Only: ", format_val(m_ao, "households")),
          div("Adult-Child: ", format_val(m_ac, "households")),
          div("Child Only: ", format_val(m_co, "households")),
          div("Unknown: ", format_val(m_un, "households"))
        ),
        showcase = bs_icon("house"),
        id = "total_households_box"
      )
    },
    
    "los" = {
      m_avg <- eval_metric_kpi("Average Length of Stay in Residence (All Clients)", metric_dataset)
      m_med <- eval_metric_kpi("Median Length of Stay in Residence (All Clients)", metric_dataset)
      metric_val_box(
        title = "Length of Stay in Residence (All Clients)",
        value = tagList(
          div("Average: ", format_val(fcoalesce(m_avg$val, 0), "days")),
          div("Median: ", format_val(fcoalesce(m_med$val, 0), "days"))
        ),
        showcase = bs_icon("building-add"),
        id = "los_box"
      )
    },
    
    "time_to_movein" = {
      m_avg <- eval_metric_kpi("Average Time to Housing Move-In (All Clients)", metric_dataset)
      m_med <- eval_metric_kpi("Median Time to Housing Move-In (All Clients)", metric_dataset)
      metric_val_box(
        title = "Time to Housing Move-In (All Clients)",
        value = tagList(
          div("Average: ", format_val(fcoalesce(m_avg$val, 0), "days")),
          div("Median: ", format_val(fcoalesce(m_med$val, 0), "days"))
        ),
        showcase = bs_icon("clock-history"),
        id = "time_to_movein_box"
      )
    },
    
    "length_of_participation" = {
      m_avg <- eval_metric_kpi("Average Length of Participation (All Clients)", metric_dataset)
      m_med <- eval_metric_kpi("Median Length of Participation (All Clients)", metric_dataset)
      metric_val_box(
        title = "Length of Participation (All Clients)",
        value = tagList(
          div("Average: ", format_val(fcoalesce(m_avg$val, 0), "days")),
          div("Median: ", format_val(fcoalesce(m_med$val, 0), "days"))
        ),
        showcase = bs_icon("calendar-range"),
        id = "length_of_participation_box"
      )
    },
    
    "entered_non_habitat" = {
      m <- eval_metric_kpi("Entered from Place Not Meant for Habitation (HoHs/Adults)", metric_dataset)
      metric_val_box(
        title = "Entered from Place Not Meant for Habitation (HoHs/Adults)",
        value = tagList(
          div(format_val(m$val, "pct"), " of all HoHs/Adults")
          # div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("signpost-split"),
        id = "entered_non_habitat_box"
      )
    },
    
    "entered_permanent" = {
      m <- eval_metric_kpi("Entered from Permanent Housing Situation (HoHs/Adults)", metric_dataset)
      metric_val_box(
        title = "Entered from Permanent Housing Situation (HoHs/Adults)",
        value = tagList(
          div(format_val(m$val, "pct"), " of all HoHs/Adults")
          # div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("house-check"),
        id = "entered_permanent_box"
      )
    },
    
    "zero_income" = {
      m <- eval_metric_kpi("Zero Income at Entry (HoHs/Adults)", metric_dataset)
      metric_val_box(
        title = "Zero Income at Entry (HoHs/Adults)",
        value = tagList(
          div(format_val(m$val, "pct"), " of all HoHs/Adults")
          # div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("wallet2"),
        id = "zero_income_box"
      )
    },
    
    "income_growth" = {
      m <- eval_metric_kpi("Income Growth from Entry to Exit (HoHs/Adults)", metric_dataset)
      metric_val_box(
        title = "Income Growth from Entry to Exit (HoHs/Adults)",
        value = tagList(
          div(format_val(m$val, "pct"), " of all exited HoHs/Adults")
          # div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("graph-up-arrow"),
        id = "income_growth_box"
      )
    },
    
    "successful_exit" = {
      m <- eval_metric_kpi("Successful Exits (All Clients)", metric_dataset)
      metric_val_box(
        title = "Successful Exits (All Clients)",
        value = tagList(
          div(format_val(m$val, "pct"), "of all exited clients")
          # div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("check-circle"),
        id = "successful_exits_box"
      )
    },
    
    "ce_assessments" = {
      m <- eval_metric_kpi("CE Assessed Households (HoHs)", metric_dataset)
      metric_val_box(
        title = "CE Assessed Households",
        value = tagList(
          div(format_val(m$val, "assessments"), " CE Assessments"),
          # div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("clipboard-check"),
        id = "ce_assessments_box"
      )
    },
    
    "cls_records" = {
      m <- eval_metric_kpi("Current Living Situation Records (HoHs/Adults)", metric_dataset)
      metric_val_box(
        title = "Current Living Situation Records: Total",
        value = div(format_val(m$val, "records"), " CLS Records"),
        showcase = bs_icon("geo-alt"),
        id = "cls_records_box"
      )
    }
  )
}

# ==========================================
# 4. DATASET PREPARATION FUNCTIONS
# ==========================================
latest_enrollments_all_proj <- reactive({
  req(input$dateRangeCount)
  
  enrollment_w_project_type <- session$userData$Enrollment |> 
    fsubset(EntryDate %between% input$dateRangeCount | 
              ExitAdjust %between% input$dateRangeCount,
      PersonalID, EnrollmentID, HouseholdID, HHTypeAtReportStart, ProjectID, ProjectType, 
      EntryDate, MoveInDateAdjust, ExitDate, ExitAdjust,
      AgeAtReportStart, LivingSituation, RelationshipToHoH, LengthOfStay
    )
  
  if (fnrow(enrollment_w_project_type) > 0) {
    enrollment_w_project_type |>
      roworder(ProjectID, PersonalID, EntryDate) |>
      fgroup_by(ProjectID, PersonalID) |>
      fslice(how = "last") |>
      fungroup()
  } else {
    data.table()
  }
})

latest_enrollments <- reactive({
  req(input$currentProviderList)
  
  enrollment_w_project_type <- latest_enrollments_all_proj() |>
    fsubset(ProjectID == input$currentProviderList)
})

get_metric_specific_datasets <- function(latest_enrollments) {
  total_clients_dt <- latest_enrollments |>
    fmutate(
      AgeGroup = fcase(
        AgeAtReportStart > 17, "Adult",
        AgeAtReportStart <= 17, "Child",
        default = "Unknown"
      )
    )
  
  total_households_served_dt <- latest_enrollments |>
    fsubset(RelationshipToHoH == 1) %>%
    fmutate(
      HHGroup = fcase(
        HHTypeAtReportStart %in% groups[["Adult-Only Households"]], "Adult Only",
        HHTypeAtReportStart %in% groups[["Adult-Child Households"]], "Adult-Child",
        HHTypeAtReportStart %in% groups[["Child Only Households"]], "Child Only",
        default = "Unknown"
      )
    )
  
  avg_hh_size_dt <- latest_enrollments |>
    fgroup_by(ProjectID, HouseholdID) |>
    fmutate(hh_size = GRPN()) |>
    fungroup() |>
    fselect(ProjectID, HouseholdID, hh_size, HHTypeAtReportStart) |>
    funique()
  
  length_of_participation_dt <- latest_enrollments |> 
    fmutate(
      end_date = fifelse(is.na(ExitDate), session$userData$ReportEnd, ExitDate),
      length_of_participation = as.integer(end_date - EntryDate)
    ) |>
    fselect(-end_date)
  
  los_dt <- latest_enrollments |>
    fsubset(
      ProjectType %in% project_types_w_beds, 
      EnrollmentID, ProjectID, ProjectType, EntryDate, MoveInDateAdjust, ExitDate, HHTypeAtReportStart
    ) |>
    join(
      session$userData$Services |>
        fgroup_by(EnrollmentID) |>
        fsummarize(num_bednights = fnunique(DateProvided)),
      on = "EnrollmentID"
    ) |>
    fmutate(
      start = fcase(
        ProjectType %in% lh_project_types_nonbn, EntryDate,
        ProjectType %in% ph_project_types, MoveInDateAdjust
      ),
      los_res = fcase(
        ProjectType == es_nbn_project_type, 
        num_bednights,
        ProjectType %in% c(lh_project_types_nonbn, ph_project_types), 
        as.integer(difftime(pmax(ExitDate, session$userData$ReportEnd, na.rm=TRUE), start, unit = "days")),
        default = NA
      )
    ) |>
    fselect(EnrollmentID, ProjectID, HHTypeAtReportStart, los_res)
  
  time_to_movein_dt <- latest_enrollments |> 
    fsubset(
      ProjectType %in% ph_project_types & 
        !is.na(MoveInDateAdjust) & MoveInDateAdjust <= session$userData$ReportEnd,
      EnrollmentID, ProjectID, EntryDate, MoveInDateAdjust, HHTypeAtReportStart
    ) |>
    fmutate(
      time_to_move_in = as.integer(difftime(MoveInDateAdjust, EntryDate, units = "days"))
    )
  
  moved_into_housing_dt <- latest_enrollments |>
    fsubset(
      ProjectType %in% ph_project_types & RelationshipToHoH == 1, 
      EnrollmentID, ProjectID, MoveInDateAdjust, HHTypeAtReportStart
    ) |>
    fmutate(
      moved_into_housing = !is.na(MoveInDateAdjust) & MoveInDateAdjust <= session$userData$ReportEnd
    )
  
  entered_from_dt <- latest_enrollments |>
    fsubset(
      ProjectType %in% c(lh_residential_project_types, setdiff(non_res_project_types, hp_project_type)) &
        (RelationshipToHoH == 1 | AgeAtReportStart > 17), 
      EnrollmentID, ProjectID, LivingSituation, HHTypeAtReportStart
    ) |>
    fmutate(
      entered_from_place_not_meant = LivingSituation == 116L,
      entered_from_ph = LivingSituation %in% perm_livingsituation
    )
  
  zero_income_dt <- session$userData$IncomeBenefits |>
    fsubset(
      DataCollectionStage == 1,
      EnrollmentID, IncomeFromAnySource
    ) |>
    join(
      latest_enrollments |> 
        fsubset(
          (RelationshipToHoH == 1 | AgeAtReportStart > 17), 
          EnrollmentID, ProjectID, HHTypeAtReportStart
        ),
      on = "EnrollmentID",
      how = "inner"
    ) |>
    fmutate(
      zero_income = IncomeFromAnySource == 0,
      nmiss = IncomeFromAnySource %in% exclude_vals
    )
  
  successful_exit_dt <- session$userData$Exit |>
    fselect(EnrollmentID, Destination) |>
    join(
      latest_enrollments |> 
        fsubset(
          !is.na(ExitDate), 
          EnrollmentID, ProjectID, ProjectType, HHTypeAtReportStart
        ),
      on = "EnrollmentID",
      how = "inner"
    ) |>
    fmutate(
      successful_exit = as.integer(fcase(
        ProjectType == out_project_type, Destination %in% setdiff(c(100:499), c(116, 206, 207, 329)),
        ProjectType %in% c(es_ee_project_type, es_nbn_project_type, th_project_type), Destination %in% c(332, perm_livingsituation),
        default = Destination %in% perm_livingsituation
      )),
      denom = as.integer(
        fifelse(ProjectType == out_project_type, !Destination %in% c(24, 206, 329), !Destination %in% c(24, 206, 215, 225))
      ),
      nmiss = Destination %in% exclude_vals | 
        (ProjectType == out_project_type & Destination %in% c(24, 206, 329)) |
        (ProjectType != out_project_type & Destination %in% c(24, 206, 215, 225))
    )
  
  income_growth_latest_enrl <- latest_enrollments |>
    fsubset(
      !is.na(ExitDate) & (RelationshipToHoH == 1 | AgeAtReportStart > 17), 
      EnrollmentID, ProjectID, HHTypeAtReportStart
    )
  
  get_growth_dt <- function(ib_dt, var_name) {
    if (fnrow(ib_dt) > 0 && fnrow(income_growth_latest_enrl) > 0) {
      ib_dt |>
        fsubset(DataCollectionStage %in% c(1, 3)) |>
        fselect(EnrollmentID, DataCollectionStage, IncomeFromAnySource, val = get(var_name)) |>
        join(income_growth_latest_enrl, on = "EnrollmentID", how = "inner") |>
        fgroup_by(EnrollmentID) |>
        fmutate(
          at_entry = ffirst(fifelse(DataCollectionStage == 1, val, NA_real_)),
          at_exit  = flast(fifelse(DataCollectionStage == 3, val, NA_real_))
        ) |>
        fslice(how = "first") |>
        fungroup() |>
        fmutate(
          has_growth = as.integer(at_exit > at_entry),
          denom =  IncomeFromAnySource %in% c(0, 1),
          nmiss = (IncomeFromAnySource %in% exclude_vals | (IncomeFromAnySource == 1 & is.na(val)))
        )
    } else {
      data.table()
    }
  }
  
  income_growth_dt <- get_growth_dt(session$userData$IncomeBenefits, "TotalMonthlyIncome")
  
  # TO EXCLUDE:
  # - CEParticipation.AccessPoint == 0
  # - AssessmentDate not within project’s CE Participation Period
  # - ProjectID not found in CEParticipation.csv
  ce_assessments_dt <- session$userData$CEParticipation |>
    join(
      latest_enrollments |> 
        fsubset(
          RelationshipToHoH == 1,
          EnrollmentID, ProjectID, ProjectType, HouseholdID, HHTypeAtReportStart
        ),
      on = "ProjectID",
      how = "inner",
      column = TRUE
    ) |>
    join(
      session$userData$Assessment |> fselect(AssessmentID, EnrollmentID, AssessmentDate), 
      on = "EnrollmentID"
    ) |>
    fmutate(
      nmiss = AccessPoint == 0 | 
        !AssessmentDate %inrange% list(CEParticipationStatusStartDate, CEParticipationStatusEndDate) |
        .join == "CEParticipation"
    ) |>
    fselect(EnrollmentID, ProjectID, ProjectType, HouseholdID, HHTypeAtReportStart, AssessmentDate, nmiss) |>
    funique() |>
    fsubset(ProjectType == ce_project_type | AssessmentDate %in% input$dateRangeCount)
  
  cls_records_dt <- session$userData$CurrentLivingSituation |>
    join(
      latest_enrollments |> fselect(EnrollmentID, ProjectID, HHTypeAtReportStart, ProjectType), 
      on = "EnrollmentID",
      how = "inner"
    )
  
  list(
    total_clients           = total_clients_dt,
    total_households_served = total_households_served_dt,
    avg_hh_size             = avg_hh_size_dt,
    length_of_participation = length_of_participation_dt,
    los                     = los_dt,
    time_to_movein          = time_to_movein_dt,
    moved_into_housing      = moved_into_housing_dt,
    entered_non_habitat     = entered_from_dt,
    entered_permanent       = entered_from_dt,
    zero_income             = zero_income_dt,
    successful_exit         = successful_exit_dt,
    income_growth           = income_growth_dt,
    ce_assessments          = ce_assessments_dt,
    cls_records             = cls_records_dt
  )
}

# ==========================================
# 5. DETAIL TAB DATA TABLE GENERATION + DOWNLOADS
# ==========================================
# Universal table builder for UI Data Table and Download tabs
build_metrics_tables_batch <- function(m_datasets, proj_table, is_export = TRUE) {
  if (fnrow(proj_table) == 0) {
    return(list(summary = data.table(), detail = data.table()))
  }
  
  target_proj_ids <- proj_table$ProjectID
  all_rows <- list()
  
  # Loop over Metrics first
  for (m_name in names(METRIC_DEFINITIONS)) {
    m_def <- METRIC_DEFINITIONS[[m_name]]
    # 1. Filter projects applicable to this metric
    applicable_projs <- proj_table[sapply(ProjectType, m_def$applies)]
    if (fnrow(applicable_projs) == 0) next
    
    # 2. Get dataset for this metric & subset to relevant projects once
    sub_dt <- m_datasets[[m_def$dt_key]]
    if (is.null(sub_dt) || fnrow(sub_dt) == 0) {
      sub_dt <- data.table()
    } else {
      sub_dt <- sub_dt[ProjectID %in% applicable_projs$ProjectID]
    }
    
    calc_fn <- if ("calc_func_det" %in% names(m_def)) m_def$calc_func_det else m_def$calc_func
    unit_fmt <- if (m_def$unit == "pct") "records" else m_def$unit
    
    # 3. Calculate each Household Group across ALL projects at once
    group_results <- list()
    for (g_name in names(groups)) {
      g_vals <- groups[[g_name]]
      
      if (fnrow(sub_dt) > 0) {
        grp_dt <- sub_dt[HHTypeAtReportStart %in% g_vals]
      } else {
        grp_dt <- data.table()
      }
      
      # Split by ProjectID to avoid repetitive queries
      if (fnrow(grp_dt) > 0) {
        split_by_proj <- split(grp_dt, by = "ProjectID", keep.by = TRUE)
        calc_res <- vapply(applicable_projs$ProjectID, function(pid) {
          p_sub <- split_by_proj[[as.character(pid)]]
          if (is.null(p_sub) || fnrow(p_sub) == 0) return("-")
          
          # print(paste0("pid = ", pid))
          # print(paste0("g_name = ", g_name))
          # print(paste0("m_name = ", m_name))
          val <- tryCatch(calc_fn(p_sub), error = function(e) NA_real_)
          
          format_val(val, unit_type = unit_fmt)
        }, character(1))
      } else {
        calc_res <- rep("-", fnrow(applicable_projs))
      }
      
      group_results[[g_name]] <- calc_res
    }
    
    # 4. Construct table rows for this metric across applicable projects
    if(!is_export && m_def$export_only)
      next
    
    ptype <- if (exists("project_type", mode = "function")) project_type(applicable_projs$ProjectType) else applicable_projs$ProjectType
    metric_dt <- data.table(
      "Organization Name" = applicable_projs$OrganizationName,
      "Project ID"        = applicable_projs$ProjectID,
      "Project Name"      = applicable_projs$ProjectName,
      "Project Type"      = ptype,
      "Metric"            = m_name,
      show_KPI            = m_def$show_KPI(ptype)
    )
    
    for (g_name in names(groups)) {
      set(metric_dt, j = g_name, value = group_results[[g_name]])
    }
    
    all_rows[[length(all_rows) + 1]] <- metric_dt
  }
  
  if (length(all_rows) == 0) {
    return(list(summary = data.table(), detail = data.table()))
  }
  
  combined_dt <- rowbind(all_rows, fill = TRUE) %>%
    fmutate(Metric_Order = match(Metric, names(METRIC_DEFINITIONS))) %>%
    roworder(`Project ID`, Metric_Order)
  
  cols_to_remove <- c("Metric_Order", "show_KPI")
  
  if (!is_export)
    cols_to_remove <- c(cols_to_remove, "Project Name", "Project Type", "Organization Name", "Project ID")
  
  # Split into Summary and Detail without re-computing
  summary_dt <- combined_dt[show_KPI == TRUE, .SD, .SDcols = !cols_to_remove]
  detail_dt  <- combined_dt[, .SD, .SDcols = !cols_to_remove]
  
  list(summary = summary_dt, detail = detail_dt)
}

# ==========================================
# 6. SHINY REACTIVE PIPELINES & RENDERING
# ==========================================

metric_datasets <- reactive({
  get_metric_specific_datasets(latest_enrollments())
})

metric_datasets_all_proj <- reactive({
  get_metric_specific_datasets(latest_enrollments_all_proj())
})

applicable_dt_keys_for_project <- reactive({
  req(session$userData$valid_file() == 1, input$currentProviderList)
  
  lapply(METRIC_DEFINITIONS, function(m_def) {
    # Remove non-applicable
    if (m_def$applies(selected_proj_type())) m_def$dt_key
  }) |>
    purrr::compact()
})

kpi_applicable_dt_keys_for_project <- reactive({
  req(session$userData$valid_file() == 1, input$currentProviderList)
  
  lapply(METRIC_DEFINITIONS, function(m_def) {
    # Remove non-applicable
    if (m_def$show_KPI(selected_proj_type())) 
      m_def$dt_key
  }) |>
    unlist(use.names = FALSE) |>
    funique() |>
    purrr::compact()
})

selected_proj_type <- reactive({
  req(input$currentProviderList)
  session$userData$Project0[ProjectID == input$currentProviderList, ProjectType][1]
})

# Summary UI Card Output
output$summary_value_boxes <- renderUI({
  req(session$userData$valid_file() == 1, input$currentProviderList)
  
  # Filter boxes applicable to current project type
  box_list <- lapply(kpi_applicable_dt_keys_for_project(), function(dt_key) {
    create_metric_value_box(dt_key, metric_dataset = metric_datasets()[[dt_key]])
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
  
  dt_detail <- build_metrics_tables_batch(
    m_datasets      = metric_datasets(),
    proj_table      = session$userData$Project0[ProjectID == input$currentProviderList],
    is_export       = FALSE
  )$detail
  
  
  datatable(
    dt_detail,
    rownames = FALSE,
    filter   = 'none',
    options  = list(dom = 't', pageLength = 50),
    style    = "default"
  )
})
