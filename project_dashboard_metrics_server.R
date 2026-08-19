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
  if (allNA(val) || is.null(val) || length(val) == 0) return("-")
  
  if (unit_type %in% c("days", "clients", "assessments", "households", "people", "records", "enrollments")) {
    paste0(comma(val, accuracy = ifelse(val %% 1 == 0, 1, 0.1)), " ", unit_type)
  } else if (unit_type == "pct") {
    percent(val, accuracy = 0.1)
  } else {
    as.character(val)
  }
}

format_table_val <- function(val, unit_type = "clients") {
  if (allNA(val) || is.null(val) || length(val) == 0 || is.na(val)) {
    return('<span data-order="-999999999">-</span>')
  }
  
  disp_val <- format_val(val, unit_type = unit_type)
  # DataTables reads `data-order` for numeric sorting while displaying `disp_val`
  paste0('<span data-order="', val, '">', disp_val, '</span>')
}

# Helper to evaluate metric calculations dynamically
eval_metric <- function(metric_name, metric_dataset) {
  def <- METRIC_DEFINITIONS[[metric_name]]
  
  if (is.null(metric_dataset) || fnrow(metric_dataset) == 0) {
    return(list(val = NA_real_, nmiss = NA_real_))
  }
  
  val   <- def$calc_func(metric_dataset)
  nmiss <- if (!is.null(def$calc_nmiss)) def$calc_nmiss(metric_dataset) else NA_real_
  
  list(val = val, nmiss = nmiss)
}

# ==========================================
# 2. MASTER METRIC DEFINITIONS
# ==========================================

METRIC_DEFINITIONS <- list(
  "Total Clients Served" = list(
    dt_key     = "total_clients",
    unit       = "clients",
    calc_func  = function(dt) if(fnrow(dt) > 0) dt |> 
      fgroup_by(AgeGroup) |> 
      fsummarise(n_unique = fnunique(PersonalID)) else 0,
    # calc_nmiss = function(dt) fsum(is.na(dt$PersonalID))
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type %in% setdiff(all_project_types, ce_project_type)
  ),
  
  "Total Households Served" = list(
    dt_key     = "total_households_served",
    unit       = "households",
    calc_func  = function(dt) fnunique(dt$PersonalID),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type %in% setdiff(all_project_types, ce_project_type)
  ),
  
  "Average Households Size" = list(
    dt_key     = "avg_hh_size",
    unit       = "people",
    calc_func  = function(dt) fmean(dt$hh_size),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) FALSE
  ),
  
  "Average Length of Participation (All Clients)" = list(
    dt_key     = "length_of_participation",
    unit       = "days",
    calc_func  = function(dt) fmean(dt$length_of_participation),
    # calc_nmiss = function(dt) fsum(is.na(dt$ExitDate))
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type %in% non_res_project_types
  ),
  
  "Median Length of Participation (All Clients)" = list(
    dt_key     = "length_of_participation",
    unit       = "days",
    calc_func  = function(dt) fmedian(dt$length_of_participation),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type %in% non_res_project_types
  ),
  
  "Average Length of Stay in Residence (All Clients)" = list(
    dt_key     = "los",
    unit       = "days",
    calc_func  = function(dt) fmean(dt$los_res),
    # calc_nmiss = function(dt) fsum(is.na(dt$los_res)),
    applies    = function(proj_type) proj_type %in% project_types_w_beds,
    show_KPI   = function(proj_type) proj_type %in% project_types_w_beds
  ),
  
  "Median Length of Stay in Residence (All Clients)" = list(
    dt_key     = "los",
    unit       = "days",
    calc_func  = function(dt) fmedian(dt$los_res),
    applies    = function(proj_type) proj_type %in% project_types_w_beds,
    show_KPI   = function(proj_type) proj_type %in% project_types_w_beds
  ),
  
  "Average Time to Housing Move-In (All Clients)" = list(
    dt_key     = "time_to_movein",
    unit       = "days",
    calc_func  = function(dt) fmean(dt$time_to_move_in),
    # calc_nmiss = function(dt) fsum(is.na(dt$MoveInDateAdjust)),
    applies    = function(proj_type) proj_type %in% ph_project_types, 
    show_KPI   = function(proj_type) proj_type %in% ph_project_types
  ),
  
  "Median Time to Housing Move-In (All Clients)" = list(
    dt_key     = "time_to_movein",
    unit       = "days",
    calc_func  = function(dt) fmedian(dt$time_to_move_in),
    applies    = function(proj_type) proj_type %in% ph_project_types,
    show_KPI   = function(proj_type) proj_type %in% ph_project_types
  ),
  
  "Moved into Housing (HoHs)" = list(
    dt_key     = "moved_into_housing",
    unit       = "pct",
    calc_func  = function(dt) if (fnrow(dt) > 0) fsum(dt$moved_into_housing) / fnobs(dt$EnrollmentID) else NA_real_,
    applies    = function(proj_type) proj_type %in% ph_project_types,
    show_KPI   = function(proj_type) FALSE
  ),
  
  "Entered from Place Not Meant for Habitation (HoHs/Adults)" = list(
    dt_key     = "entered_non_habitat",
    unit       = "pct",
    calc_func  = function(dt) {
      denom <- fsum(!dt$LivingSituation %in% exclude_vals)
      if (denom > 0) fsum(dt$entered_from_place_not_meant) / denom else NA_real_
    },
    calc_nmiss = function(dt) fsum(dt$nmiss),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type %in% c(lh_residential_project_types, setdiff(non_res_project_types, hp_project_type))
  ),
  
  "Entered from Permanent Housing Situation (HoHs/Adults)" = list(
    dt_key     = "entered_permanent",
    unit       = "pct",
    calc_func  = function(dt) {
      denom <- fsum(!dt$LivingSituation %in% exclude_vals)
      if (denom > 0) fsum(dt$entered_from_ph) / denom else NA_real_
    },
    calc_nmiss = function(dt) fsum(dt$nmiss),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type %in% c(lh_residential_project_types, ce_project_type)
  ),
  
  "Zero Income at Entry (HoHs/Adults)" = list(
    dt_key     = "zero_income",
    unit       = "pct",
    calc_func  = function(dt) {
      denom <- fsum(dt$IncomeFromAnySource %in% c(0, 1))
      if (denom > 0) fsum(dt$zero_income) / denom else NA_real_
    },
    calc_nmiss = function(dt) fsum(dt$nmiss),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type == hp_project_type
  ),
  
  "Income Growth from Entry to Exit (HoHs/Adults)" = list(
    dt_key     = "income_growth",
    unit       = "pct",
    calc_func  = function(dt) {
      denom <- fsum(dt$IncomeFromAnySource %in% c(0, 1))
      if (denom > 0) fsum(dt$has_growth) / denom else NA_real_
    },
    calc_nmiss = function(dt) fsum(dt$nmiss),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type %in% c(ph_project_types, hp_project_type)
  ),
  
  "Successful Exits (All Clients)" = list(
    dt_key     = "successful_exit",
    unit       = "pct",
    calc_func  = function(dt) {
      denom <- fsum(dt$denom)
      if (denom > 0) fsum(dt$successful_exit) / denom else NA_real_
    },
    calc_nmiss = function(dt) fsum(dt$nmiss),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) TRUE
  ),
  
  "CE Assessed Households" = list(
    dt_key     = "ce_assessments",
    unit       = "assessments",
    calc_func  = function(dt) fnobs(dt$EnrollmentID),
    calc_nmiss = function(dt) fsum(dt$nmiss),
    applies    = function(proj_type) TRUE,
    show_KPI   = function(proj_type) proj_type == ce_project_type
  ),
  
  "Current Living Situation Records: Total" = list(
    dt_key     = "cls_records",
    unit       = "records",
    calc_func  = function(dt) fnobs(dt$CurrentLivingSitID),
    applies    = function(proj_type) proj_type %in% project_types_w_cls,
    show_KPI   = function(proj_type) proj_type %in% setdiff(project_types_w_cls, es_nbn_project_type)
  )
)

# Generic calculation function for Household Grouping (Table details)
calc_by_hh_group <- function(metric_name, m_datasets) {
  def <- METRIC_DEFINITIONS[[metric_name]]
  sub_dt <- m_datasets[[def$dt_key]]
  
  vals <- lapply(groups, function(g) {
    if (is.null(sub_dt) || fnrow(sub_dt) == 0) return(NA_real_)
    grp_dt <- sub_dt[HHTypeAtReportStart %in% g]
    if (fnrow(grp_dt) == 0) return(NA_real_)
    
    def$calc_func(grp_dt)
  })
  
  if (!is.null(def$calc_nmiss)) {
    vals["Total Missing"] <- if (!is.null(sub_dt) && fnrow(sub_dt) > 0) def$calc_nmiss(sub_dt) else NA_real_
  }
  
  vals
}

# Dynamic Value Box Builder
create_metric_value_box <- function(box_key, metric_dataset) {
  
  # theme for value boxes
  vb_theme <- "text-primary"
  
  switch(
    box_key,
    
    "total_clients" = {
      m <- eval_metric("Total Clients Served", metric_dataset)
      # Safe extraction helper function
      get_age_count <- function(res_dt, target_group) {
        if (is.data.frame(res_dt) && fnrow(res_dt) > 0) {
          val <- res_dt[AgeGroup == target_group, n_unique]
          if (length(val) > 0) return(val)
        }
        NA_real_
      }
      
      adult_val   <- get_age_count(m$val, "Adult")
      child_val   <- get_age_count(m$val, "Child")
      unknown_val <- get_age_count(m$val, "Unknown")
      
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Total Clients Served",
        value = tagList(
          div("Adults: ", format_val(adult_val, "clients")),
          div("Children: ", format_val(child_val, "clients")),
          div("Unknown: ", format_val(unknown_val, "clients"))
        ),
        showcase = bs_icon("people"),
        theme = vb_theme,
        id = "total_clients_box"
      )
    },
    
    "total_households_served" = {
      m <- eval_metric("Total Households Served", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Total Households Served",
        value = tagList(
          div("Total Households: ", format_val(m$val, "households"))
        ),
        showcase = bs_icon("house"),
        theme = vb_theme,
        id = "total_households_box"
      )
    },
    
    "los" = {
      m_avg <- eval_metric("Average Length of Stay in Residence (All Clients)", metric_dataset)
      m_med <- eval_metric("Median Length of Stay in Residence (All Clients)", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Length of Stay in Residence (All Clients)",
        value = tagList(
          div("Average: ", format_val(fcoalesce(m_avg$val, 0), "days")),
          div("Median: ", format_val(fcoalesce(m_med$val, 0), "days"))
        ),
        showcase = bs_icon("building-add"),
        theme = vb_theme,
        id = "los_box"
      )
    },
    
    "movein_time" = {
      m_avg <- eval_metric("Average Time to Housing Move-In (All Clients)", metric_dataset)
      m_med <- eval_metric("Median Time to Housing Move-In (All Clients)", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Time to Housing Move-In (All Clients)",
        value = tagList(
          div("Average: ", format_val(fcoalesce(m_avg$val, 0), "days")),
          div("Median: ", format_val(fcoalesce(m_med$val, 0), "days"))
        ),
        showcase = bs_icon("clock-history"),
        theme = vb_theme,
        id = "movein_time_box"
      )
    },
    
    "lop" = {
      m_avg <- eval_metric("Average Length of Participation (All Clients)", metric_dataset)
      m_med <- eval_metric("Median Length of Participation (All Clients)", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Length of Participation (All Clients)",
        value = tagList(
          div("Average: ", format_val(fcoalesce(m_avg$val, 0), "days")),
          div("Median: ", format_val(fcoalesce(m_med$val, 0), "days"))
        ),
        showcase = bs_icon("calendar-range"),
        theme = vb_theme,
        id = "lop_box"
      )
    },
    
    "entered_non_habitat" = {
      m <- eval_metric("Entered from Place Not Meant for Habitation (HoHs/Adults)", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Entered from Place Not Meant for Habitation (HoHs/Adults)",
        value = tagList(
          div("Percent of all HoHs/Adults: ", format_val(m$val, "pct")),
          div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("signpost-split"),
        theme = vb_theme,
        id = "entered_non_habitat_box"
      )
    },
    
    "entered_permanent" = {
      m <- eval_metric("Entered from Permanent Housing Situation (HoHs/Adults)", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Entered from Permanent Housing Situation (HoHs/Adults)",
        value = tagList(
          div("Percent of all HoHs/Adults: ", format_val(m$val, "pct")),
          div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("house-check"),
        theme = vb_theme,
        id = "entered_permanent_box"
      )
    },
    
    "zero_income" = {
      m <- eval_metric("Zero Income at Entry (HoHs/Adults)", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Zero Income at Entry (HoHs/Adults)",
        value = tagList(
          div("Percent of all HoHs/Adults: ", format_val(m$val, "pct")),
          div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("wallet2"),
        theme = vb_theme,
        id = "zero_income_box"
      )
    },
    
    "income_growth" = {
      m <- eval_metric("Income Growth from Entry to Exit (HoHs/Adults)", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Income Growth from Entry to Exit (HoHs/Adults)",
        value = tagList(
          div("Percent of all exited HoHs/Adults: ", format_val(m$val, "pct")),
          div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("graph-up-arrow"),
        theme = vb_theme,
        id = "income_growth_box"
      )
    },
    
    "successful_exit" = {
      m <- eval_metric("Successful Exits (All Clients)", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Successful Exits (All Clients)",
        value = tagList(
          div("Percent of all exited clients: ", format_val(m$val, "pct")),
          div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("check-circle"),
        theme = vb_theme,
        id = "successful_exits_box"
      )
    },
    
    "ce_assessments" = {
      m <- eval_metric("CE Assessed Households", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "CE Assessed Households",
        value = tagList(
          div("Number of CE Assessments: ", format_val(m$val, "assessments")),
          div("Excluded: ", format_val(m$nmiss, "enrollments"))
        ),
        showcase = bs_icon("clipboard-check"),
        theme = vb_theme,
        id = "ce_assessments_box"
      )
    },
    
    "cls_records" = {
      m <- eval_metric("Current Living Situation Records: Total", metric_dataset)
      value_box(
        class = "project_dashboard_valbox border-primary",
        title = "Current Living Situation Records: Total",
        value = div("Total CLS Records: ", format_val(m$val, "records")),
        showcase = bs_icon("geo-alt"),
        theme = vb_theme,
        id = "cls_records_box"
      )
    }
  )
}

# ==========================================
# 4. DATASET PREPARATION FUNCTIONS
# ==========================================

latest_enrollments <- reactive({
  req(input$dateRangeCount, input$currentProviderList)
  
  enrollment_w_project_type <- session$userData$Enrollment |> 
    fsubset(
      ProjectID == input$currentProviderList & (
        EntryDate %between% input$dateRangeCount | 
        ExitAdjust %between% input$dateRangeCount
      ),
      PersonalID, EnrollmentID, HouseholdID, HHTypeAtReportStart, ProjectID, ProjectType, 
      EntryDate, MoveInDateAdjust, ExitDate, ExitAdjust,
      AgeAtReportStart, LivingSituation, RelationshipToHoH, LengthOfStay
    )
  
  if (fnrow(enrollment_w_project_type) > 0) {
    enrollment_w_project_type |>
      fgroup_by(ProjectID, PersonalID, EntryDate) |>
      fslice(how = "last") |>
      fungroup()
  } else {
    data.table()
  }
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
    fsubset(RelationshipToHoH == 1)
  
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
      entered_from_ph = LivingSituation %in% perm_livingsituation,
      nmiss = LivingSituation %in% exclude_vals
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
        (ProjectType == out_project_type & !Destination %in% c(24, 206, 329)) |
        (ProjectType != out_project_type & !Destination %in% c(24, 206, 215, 225))
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
# 5. DETAIL TAB DATA TABLE GENERATION
# ==========================================
get_details_by_hh_type <- function(m_datasets, selected_project_type) {
  dt_list <- lapply(names(METRIC_DEFINITIONS), function(m_name) {
    m_def <- METRIC_DEFINITIONS[[m_name]]
    
    # Filter by applicability
    if (!m_def$applies(selected_project_type)) return(NULL)
    
    vals <- calc_by_hh_group(m_name, m_datasets)
    if (m_name == "Total Clients Served") {
      age_groups <- c("Adult", "Child", "Unknown")
      
      rows <- lapply(age_groups, function(ag) {
        formatted_vals <- lapply(vals, function(group_res) {
          # Check if group_res is a valid data.frame returned by calc_func
          if (is.data.frame(group_res) && fnrow(group_res) > 0) {
            cnt <- group_res[AgeGroup == ag, n_unique]
            val_to_fmt <- if (length(cnt) > 0 && !is.na(cnt)) cnt else 0
          } else {
            val_to_fmt <- NA_real_
          }
          format_table_val(val_to_fmt, unit_type = m_def$unit)
        })
        
        as.list(c(Metric = paste0("Total Clients Served (", ag, "s)"), formatted_vals))
      })
      return(rbindlist(rows, fill = TRUE))
      
    } else {
      # Standard handling for single-value scalar metrics
      formatted_vals <- lapply(vals, format_table_val, unit_type = m_def$unit)
      return(as.list(c(Metric = m_name, formatted_vals)) |> qDT())
    }
  })
  
  dt_list <- dt_list[!sapply(dt_list, is.null)]
  rbindlist(dt_list, fill = TRUE)
}

# ==========================================
# 6. SHINY REACTIVE PIPELINES & RENDERING
# ==========================================

metric_datasets <- reactive({
  get_metric_specific_datasets(latest_enrollments())
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
  
  metric_datasets_for_project <- metric_datasets()[names(metric_datasets()) %in% applicable_dt_keys_for_project()]
  
  dt_detail <- get_details_by_hh_type(metric_datasets_for_project, selected_proj_type())
  
  datatable(
    dt_detail,
    rownames = FALSE,
    filter   = 'none',
    options  = list(dom = 't', pageLength = 20),
    style    = "default",
    escape   = FALSE
  )
})