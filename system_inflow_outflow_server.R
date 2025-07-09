# Factor levels for Inflow and Outflow types -----------------------------------
active_at_levels <- c(
  "Housed",
  "Homeless"
)

# All possible levels
inflow_detail_levels <- c(
  "First-Time \nHomeless", 
  "Returned from \nPermanent",
  "Re-engaged from \nNon-Permanent",
  "Unknown",
  "Continuous at Start",
  "something's wrong"
)

outflow_detail_levels <- c(
  "Exited, \nNon-Permanent",
  "Exited, \nPermanent",
  "Inactive",
  "Continuous at End",
  "something's wrong"
)

# Levels for detail chart
inflow_chart_detail_levels <- c(
  "First-Time \nHomeless", 
  "Returned from \nPermanent",
  "Re-engaged from \nNon-Permanent"
)

outflow_chart_detail_levels <- c(
  "Exited, \nNon-Permanent",
  "Exited, \nPermanent",
  "Inactive"
)

inflow_outflow_levels <- c("Inflow","Outflow")

mbm_inflow_levels <- c("Active at Start: Homeless", "Inflow")
mbm_outflow_levels <- c("Outflow", "Active at End: Housed")

# Levels for summary chart
inflow_summary_levels <- c(
  "Active at Start",
  "Inflow",
  "something's wrong"
)
outflow_summary_levels <- c(
  "Outflow",
  "Active at End",
  "something's wrong"
)

inflow_chart_summary_levels <- c(
  "Active at Start",
  "Inflow"
)
outflow_chart_summary_levels <- c(
  "Outflow",
  "Active at End"
)

collapse_details <- list(
  "Outflow" = outflow_chart_detail_levels, 
  "Inflow" = inflow_chart_detail_levels
)

bar_colors <- c(
  "Inflow" = "#BDB6D7", 
  "Outflow" = '#6A559B',
  "Homeless" = '#ECE7E3',
  "Housed" = '#9E958F'
)

mbm_inflow_bar_colors <- c(
  "Active at Start: Homeless" = '#ECE7E3',
  "Inflow" = "#BDB6D7"
)

mbm_outflow_bar_colors <- c(
  "Outflow" = '#6A559B',
  # "Inactive" = "#E78AC3"
  "Active at End: Housed" = '#9E958F'
)

mbm_bar_colors <- c(
  mbm_inflow_bar_colors,
  mbm_outflow_bar_colors
)

mbm_single_status_chart_colors <- c(
  "First-Time \nHomeless" = bar_colors[["Inflow"]],
  "Inactive" = colorspace::lighten(bar_colors[["Outflow"]], amount = 0.3)
)

# 0.2 seems to be the right value to space the bars correctly
# higher than this and outflow bars start to overlap with next month's inflow
# lower and the bars are too thin, or space within a month is about the same as across
mbm_bar_width = 0.2
mbm_export_bar_width = 0.4

level_of_detail_text <- reactive({
  case_when(
    input$syso_level_of_detail == "All" ~ "People",
    input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
  )
})

full_unit_of_analysis_display <- reactive({
  paste0(
    "Total ", 
    level_of_detail_text(),
    if_else(
      input$syso_hh_type == "All",
      "",
      paste0(" in ",
             str_remove(getNameByValue(syso_hh_types, input$syso_hh_type), "- "),
             " Households")
    )
  )
})
# Period-Specific, Filtered, Enrollment-Level Universe -------------------------

## Enrollment-level flags ------------------------
# hello weary traveler amongst these date ranges. you may find it helpful to
# find example clients and their Entry and Exit Dates and enter them into
# https://onlinetools.com/time/visualize-date-intervals <- here.
# add inflow type and active enrollment typed used for system overview plots
#
# While the following datasets appear to be inflow-outflow specific, 
# the reason they are stored in this system_overview script is because they 
universe_enrl_flags <- function(all_filtered_w_lh) {
  logToConsole(session, "In universe_enrl_flags")
  
  all_filtered_w_lh[, `:=`(
    # INFLOW CALCULATOR COLUMNS
    active_at_start_homeless = eecr & was_lh_at_start & (
      startDate == session$userData$ReportStart | EntryDate <= startDate
    ),
    
    active_at_start_housed = eecr & ProjectType %in% ph_project_types & (
      (fcoalesce(MoveInDateAdjust, no_end_date) < startDate) | 
      (days_since_lookback %between% c(0, 14) & lookback_dest_perm & lookback_movein_before_start)
    ),
    
    return_from_perm = eecr & 
      days_since_lookback %between% c(15, 730) & lookback_dest_perm &
      any_lookbacks_with_exit_to_perm,
    
    return_from_nonperm = eecr & (
      (days_since_lookback %between% c(15, 730) & !lookback_dest_perm) |
      (ProjectType %in% c(es_nbn_project_type, non_res_project_types) & !was_lh_at_start) 
    ) & (
      any_lookbacks_with_exit_to_nonperm |
      was_lh_during_period 
    ),
    
    first_time_homeless = (days_since_lookback > 730 | is.na(days_since_lookback)) & 
      EntryDate >= startDate,
    
    unknown_at_start = eecr & 
      straddles_start & 
      ProjectType %in% c(es_nbn_project_type, non_res_project_types) &
      !was_lh_at_start,
    
    # OUTFLOW CALCULATOR COLUMNS
    exited_system = lecr & ExitAdjust %between% list(startDate, endDate) & (!continuous_at_end | is.na(continuous_at_end)),
    
    homeless_at_end = lecr & was_lh_at_end,
    
    housed_at_end = lecr & 
      ProjectType %in% ph_project_types & 
      fcoalesce(MoveInDateAdjust, no_end_date) < endDate &
      (straddles_end | days_to_lookahead %between% c(0, 14)),
    
    unknown_at_end = lecr &
      straddles_end & 
      ProjectType %in% c(es_nbn_project_type, non_res_project_types) &
      !was_lh_at_end
  )]
}

## People-level flags ------------------------
# Need to keep it enrollment-level so other scripts can reference the enrollments
universe_ppl_flags <- function(universe_df) {
  logToConsole(session, "In universe_ppl_flags")
  
  setkey(universe_df, period, PersonalID)

  # PersonalIDs: 637203, 678824, 681240
  # InflowTypeDetail is NA
  universe_df[, `:=`(
    exited_perm = exited_system & Destination %in% perm_livingsituation,
    exited_temp = exited_system & !Destination %in% perm_livingsituation
  )]
  
  universe_w_ppl_flags <- universe_df %>%
    fgroup_by(period, PersonalID) %>%
    fmutate(
      # INFLOW
      active_at_start_homeless_client = anyv(active_at_start_homeless, TRUE),
      active_at_start_housed_client = anyv(active_at_start_housed, TRUE),
      return_from_perm_client = anyv(return_from_perm, TRUE),
      reengaged_from_temp_client = anyv(return_from_nonperm, TRUE),
      first_time_homeless_client = anyv(first_time_homeless, TRUE),
      unknown_at_start_client = anyv(unknown_at_start, TRUE),
      continuous_at_start_client = anyv(continuous_at_start, TRUE),
      
      # OUTFLOW
      perm_dest_client = anyv(exited_perm, TRUE),
      temp_dest_client = anyv(exited_temp, TRUE),
      homeless_at_end_client = anyv(homeless_at_end, TRUE),
      housed_at_end_client = anyv(housed_at_end, TRUE),
      unknown_at_end_client = anyv(unknown_at_end, TRUE),
      continuous_at_end_client = anyv(continuous_at_end , TRUE)
    ) %>%
    fungroup() %>%
    ftransform(
      InflowTypeSummary = factor(
        fcase(
          active_at_start_homeless_client | active_at_start_housed_client, "Active at Start",
          first_time_homeless_client | return_from_perm_client | reengaged_from_temp_client | unknown_at_start_client, "Inflow",
          continuous_at_start_client, "Continuous at Start",
          default = "something's wrong"
        ), levels = inflow_summary_levels
      ),
      
      InflowTypeDetail = factor(
        fcase(
          active_at_start_homeless_client, "Homeless",
          active_at_start_housed_client, "Housed",
          return_from_perm_client, "Returned from \nPermanent",
          reengaged_from_temp_client, "Re-engaged from \nNon-Permanent",
          first_time_homeless_client, "First-Time \nHomeless",
          continuous_at_start_client, "Continuous at Start",
          unknown_at_start_client, "Unknown",
          default = "something's wrong"
        ), levels = c(active_at_levels, inflow_detail_levels)
      ),
      
      OutflowTypeSummary = factor(
        fcase(
          perm_dest_client | temp_dest_client | unknown_at_end_client, "Outflow",
          homeless_at_end_client | housed_at_end_client, "Active at End",
          continuous_at_end_client, "Continuous at End",
          default = "something's wrong"
        ), levels = outflow_summary_levels
      ),
      
      OutflowTypeDetail = factor(
        fcase(
          perm_dest_client, "Exited, \nPermanent",
          temp_dest_client, "Exited, \nNon-Permanent",
          unknown_at_end_client, "Inactive",
          homeless_at_end_client, "Homeless",
          housed_at_end_client, "Housed",
          continuous_at_end_client, "Continuous at End",
          default = "something's wrong"
        ), levels = c(outflow_detail_levels, rev(active_at_levels))
      )
    ) %>%
    fselect(-startDate, -endDate)
  
  if(!in_dev_mode) {
    universe_w_ppl_flags[, .(
      PersonalID,
      InflowTypeSummary,
      InflowTypeDetail,
      OutflowTypeSummary,
      OutflowTypeDetail,
      ProjectType,
      month,
      EnrollmentID, 
      eecr,
      lecr,
      MoveInDateAdjust
    )]
  }
  
  if(nrow(universe_w_ppl_flags[InflowTypeDetail == "Unknown" & period == "Full"]) > 0) {
    if(in_dev_mode) browser()
    stop("There's an Inflow-Unknown in the Full Annual data!")
  }

  bad_records <- universe_w_ppl_flags[
    InflowTypeSummary == "something's wrong" |
    OutflowTypeSummary == "something's wrong"
  ]
  if(nrow(bad_records) > 0) {
    if(in_dev_mode) {
      view(bad_records[InflowTypeSummary == "something's wrong", c("period", inflow_debug_cols), with=FALSE])
      view(bad_records[OutflowTypeSummary == "something's wrong", c("period", outflow_debug_cols), with=FALSE])
      browser()
    }
    # e.g. PersonalID 623725 in Nov and 601540 in Dec
    # e.g. PersonalID 305204 and 420232 in Nov and 601540 and 620079 in Dec
    # e.g. PersonalID 14780 in Oct and Nov
    # 613426 - in Nov, they should be Active at start Homeless but the problem is that the lookback has no exit or destination
    # If we restrict Return/Re-Engaged to those with lookbacks with Exits to corresponding destination, then:
    #   PersonalIDs: 306663, 619032, 119222, 11943    
    # AS 5/12/25: With new was_lh_at_end condition in creating lecr, PersonalID 305204 (ICF-good) is "something's wrong" for annual
    #
    # PersonalID 687862 has inflow issue
    # PersonalID 688880, DEMO mode, Jan 22, Outflow
    # PersonalID 690120, DEMO mode, Apr 22, Outflow
    
    logToConsole(session, "There are something's wrong records in the universe_ppl_flags data")
  }

  # PersonalID: 529378, enrollment 825777 - 
  # Oct - Active at Start Homeless 
  # Nov - Active at Start Homeless
  # Dec - NOT IN DATASET BECAUSE NO EECR
  # Jan - NOT IN DATASET BECAUSE NO EECR
  # Feb - NOT IN DATASET BECAUSE NO EECR
  # Mar - NOT IN DATASET BECAUSE NO EECR
  # Apr - Inflow: Inactive, Outflow, Exited Non-Perm
  # browser()
  # print(
  #   universe_w_ppl_flags[PersonalID == 613426, .(EnrollmentID, eecr, lecr, period[1])]
  # )
  universe_w_ppl_flags
}

# Inflow/Outflow Client-Level Data ---------------------------
## Summary (Annual) ----------------------------
# This also gets used by the Status chart
get_inflow_outflow_full <- reactive({
  logToConsole(session, "In get_inflow_outflow_full")
  full_data <- period_specific_data()[["Full"]]
  
  logToConsole(session, paste0("In get_inflow_outflow_full, num full_data records: ", nrow(full_data)))
  
  if(nrow(full_data) == 0) return(full_data)
  
  if(in_dev_mode) export_bad_records("Full", full_data)
  
  # AS 6/8/25: Do we want to remove *people* that are Continuous? Or just exclude from those Inflow/Outflow bars?
  # ditto for Inflow = Unknown
  # 637203 is an example of someone with Inflow = Unknown but has a regular Outflow
  full_data %>%
    fselect(PersonalID,
            InflowTypeSummary,
            InflowTypeDetail,
            OutflowTypeSummary,
            OutflowTypeDetail
    ) %>%
    fsubset(
      InflowTypeDetail != "Continuous at Start" &
      OutflowTypeDetail != "Continuous at End"
    ) %>%
    funique()
})

## Monthly ---------------------------------
# combine individual month datasets
get_inflow_outflow_monthly <- reactive({
  logToConsole(session, paste0("In get_inflow_outflow_monthly"))
  months_data <- period_specific_data()[["Months"]]
  
  logToConsole(session, paste0("In get_inflow_outflow_monthly, num months_data records: ", nrow(months_data)))
  
  if(nrow(months_data) == 0) return(months_data)
  
  if(in_dev_mode) export_bad_records("Month", months_data)
  
  data.table::copy(months_data %>%
    fselect(
      PersonalID, 
      InflowTypeDetail, 
      OutflowTypeDetail, 
      InflowTypeSummary, 
      OutflowTypeSummary, 
      month
    ) %>% 
    funique()
  )
})

export_bad_records <- function(period, df) {
  ds_name <- ifelse(isTRUE(input$in_demo_mode), "DEMO", input$imported$name)
  if(period == "Month") {
    df_multiple_inactives <- df %>%
      fselect(PersonalID,
              InflowTypeSummary,
              InflowTypeDetail,
              OutflowTypeSummary,
              OutflowTypeDetail,
              month
      ) %>%
      funique() %>%
      roworder(PersonalID, month) %>%
      fmutate(is_inactive = OutflowTypeDetail == "Inactive")
    
    # 3. Identify consecutive blocks of status (active or inactive)
    #    rleid() assigns a unique ID to consecutive runs of identical values
    df_multiple_inactives[, block_id := rleid(is_inactive), by = PersonalID]
    
    # 4. Calculate the length of each block
    df_multiple_inactives[, block_length := .N, by = .(PersonalID, block_id)]
    
    df_multiple_inactives <- df_multiple_inactives[is_inactive == TRUE & block_length > 1]
    if(nrow(df_multiple_inactives) > 0)
      write_xlsx(
        df_multiple_inactives[is_inactive == TRUE & block_length > 1],
        path = glue::glue("/media/sdrive/projects/CE_Data_Toolkit/debugs/{ds_name}-multiple-inactives-in-a-row-for-{period}-{today()}.xlsx")
      )
  }
  
  df <- df[InflowTypeSummary == "something's wrong" | OutflowTypeSummary == "something's wrong"]
  
  if(nrow(df) == 0) return(NULL)

  dfs <- list(
    "Inflows" = df %>%
      fsubset(InflowTypeSummary == "something's wrong") %>% 
      fselect(inflow_debug_cols, if(period == "Month") "month"),
    "Outflows" = df %>%
      fsubset(OutflowTypeSummary == "something's wrong") %>% 
      fselect(outflow_debug_cols, if(period == "Month") "month")
  )

  write_xlsx(
    dfs, 
    glue::glue("/media/sdrive/projects/CE_Data_Toolkit/debugs/{ds_name}-somethings-wrongs-for-{period}-{today()}.xlsx")
  )
}

# Filter Selections UI -----------------------------------------------------------
# (Reported above the chart)
output$sys_inflow_outflow_detail_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  syso_detailBox() 
})
output$sys_inflow_outflow_summary_filter_selections <- renderUI({
  req(session$userData$valid_file() == 1)
  syso_detailBox() 
})
output$sys_inflow_outflow_monthly_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  syso_detailBox() 
})

# Chart Data Prep ---------------------------------------
## Summary/Detail (Annual) ---------------------------------------
# This can be used for both the Summary and Detail charts
# Final dataset looks like this:
#                             Detail                         Summary  InflowOutflow PlotFillGroups     N order
#                             <fctr>                          <fctr>         <fctr>         <fctr> <int> <int>
# 1:                          Housed                 Active at Start         Inflow         Housed    73     1
# 2:                        Homeless                 Active at Start         Inflow       Homeless   153     2
# 3:           First-Time \nHomeless           First-Time \nHomeless         Inflow         Inflow   747     3
# 4:       Returned from \nPermanent       Returned from \nPermanent         Inflow         Inflow     0     4
# 5: Re-engaged from \nNon-Permanent Re-engaged from \nNon-Permanent         Inflow         Inflow     0     5
# 6:                         Unknown                         Unknown         Inflow         Inflow     0     6
# 7:          Exited,\nNon-Permanent          Exited,\nNon-Permanent        Outflow        Outflow   281     7
# 8:              Exited,\nPermanent              Exited,\nPermanent        Outflow        Outflow   355     8
# 9:                        Inactive                        Inactive        Outflow        Outflow    50     9
# 10:                         Housed                   Active at End        Outflow         Housed    50    11
# 11:                       Homeless                   Active at End        Outflow       Homeless   237    10

sys_inflow_outflow_annual_chart_data <- reactive({
  logToConsole(session, "In sys_inflow_outflow_annual_chart_data")

  full_combinations <- data.frame(
    Detail = c(
      active_at_levels,
      inflow_chart_detail_levels,
      outflow_chart_detail_levels,
      active_at_levels
    ),
    Summary = c(
      rep("Active at Start", length(active_at_levels)),
      inflow_chart_detail_levels,
      outflow_chart_detail_levels,
      rep("Active at End", length(active_at_levels))
    ),
    InflowOutflow = c(
      rep("Inflow", length(c(inflow_chart_detail_levels, active_at_levels))),
      rep("Outflow", length(c(outflow_chart_detail_levels, active_at_levels)))
    ),
    PlotFillGroups = c(
      active_at_levels,
      rep("Inflow", length(inflow_chart_detail_levels)),
      rep("Outflow", length(outflow_chart_detail_levels)),
      active_at_levels
    )
  )

  inflow_outflow_full_data <- get_inflow_outflow_full() %>% 
    ftransform(
      InflowTypeDetail = factor(InflowTypeDetail, levels = c(active_at_levels, inflow_chart_detail_levels)),
      OutflowTypeDetail = factor(OutflowTypeDetail, levels = c(outflow_chart_detail_levels, active_at_levels))
    )

  rbind(
    inflow_outflow_full_data[, .(
      Detail = InflowTypeDetail,
      Summary = fct_collapse(InflowTypeDetail, `Active at Start` = active_at_levels),
      InflowOutflow = factor("Inflow", levels = inflow_outflow_levels),
      PlotFillGroups = fct_collapse(InflowTypeDetail, Inflow = inflow_chart_detail_levels)
    )],
    inflow_outflow_full_data[, .(
      Detail = OutflowTypeDetail,
      Summary = fct_collapse(OutflowTypeDetail, `Active at End` = active_at_levels),
      InflowOutflow = factor("Outflow", levels = inflow_outflow_levels),
      PlotFillGroups = fct_collapse(OutflowTypeDetail, Outflow = outflow_chart_detail_levels)
    )]
  ) %>% 
  fsubset(Detail != "Unknown") %>%
  fcount() %>%
  join(full_combinations, how="full", on=names(full_combinations), overid=0) %>%
  collapse::replace_na(cols = "N", value = 0) %>%
  fsubset(!(Detail == "something's wrong" & N == 0)) %>%
  roworder(Summary, Detail) %>%
  fmutate(
    # switching Homeless and Housed order for Outflow is important for lining up 
    # the bars, which are created by calculating ystart+yend based on the ordering
    order = case_when(
      Summary == "Active at End" & Detail == "Homeless" ~ fnrow(.) - 1,
      Summary == "Active at End" & Detail == "Housed" ~ fnrow(.),
      TRUE ~ seq_row(.)  # fallback for other statuses/times
    )
  ) %>%
  roworder(order)
})

## Monthly ---------------------------------------
### MbM ---------------------------
# Get records to be counted in MbM. Doing this step separately from the counts allows us to easily validate
sys_inflow_outflow_monthly_chart_data <- reactive({
  logToConsole(session, "In sys_inflow_outflow_monthly_chart_data")
  monthly_data <- get_inflow_outflow_monthly() 
  
  if(nrow(monthly_data) == 0) return(monthly_data)
  monthly_data <- monthly_data %>%
    fsubset(InflowTypeDetail != "Continuous at Start" & OutflowTypeDetail != "Continuous at End") %>%
    fmutate(
      InflowPlotFillGroups = fct_collapse(
        InflowTypeDetail, 
        `Active at Start: Housed` = "Housed", 
        `Active at Start: Homeless` = "Homeless", 
        Inflow = inflow_detail_levels
      ),
      OutflowPlotFillGroups = fct_collapse(
        OutflowTypeDetail, 
        `Active at End: Homeless` = "Homeless", 
        `Active at End: Housed` = "Housed",
        Outflow = outflow_detail_levels
      )
    )
  
  get_counts_by_month_for_mbm(monthly_data)
})

# Get counts of Inflow/Outflow statuses by month (long-format, 1 row per month-status)
get_counts_by_month_for_mbm <- function(monthly_data) {
  monthly_counts <- rbind(
    monthly_data[, .(
      PersonalID, 
      month, 
      PlotFillGroups = InflowPlotFillGroups, 
      Detail = InflowTypeDetail,
      Summary = InflowTypeSummary
    )],
    monthly_data[, .(
      PersonalID, 
      month, 
      PlotFillGroups = OutflowPlotFillGroups, 
      Detail = OutflowTypeDetail,
      Summary = OutflowTypeSummary
    )]
  ) %>%
    funique() %>%
    fsubset(Detail != "Unknown" | is.na(Detail)) %>%
    fgroup_by(month, Summary, PlotFillGroups, Detail) %>%
    fsummarise(Count = GRPN()) %>%
    roworder(month, Summary, PlotFillGroups, Detail)

  all_months <- data.table(month = get_months_in_report_period()) %>%
    fmutate(month = factor(format(month, "%b %y")))
  # PlotFillGroups %in% c(mbm_inflow_levels, mbm_outflow_levels) &
    
  full_combinations <- data.table(
    Detail = c(
      active_at_levels,
      inflow_chart_detail_levels,
      outflow_chart_detail_levels,
      active_at_levels
    ),
    Summary = c(
      rep("Active at Start", length(active_at_levels)),
      rep("Inflow", length(inflow_chart_detail_levels)),
      rep("Outflow", length(outflow_chart_detail_levels)),
      rep("Active at End", length(active_at_levels))
    ),
    PlotFillGroups = c(
      paste0("Active at Start: ", active_at_levels),
      rep("Inflow", length(inflow_chart_detail_levels)),
      rep("Outflow", length(outflow_chart_detail_levels)),
      paste0("Active at End: ", active_at_levels)
    )
  ) %>%
    fmutate(k = 1) %>%
    join(
      all_months[, k:= 1],
      on = "k", 
      multiple = TRUE
    ) %>%
    fselect(-k)

  # Make sure all month-type combinations are reflected
  join(
    monthly_counts,
    full_combinations,
    how = "full",
    overid = 0
  ) %>%
  collapse::replace_na(value = 0, cols = "Count")
}

### Monthly_chart_data, wide format
sys_monthly_chart_data_wide <- reactive({
  logToConsole(session, "In sys_monthly_chart_data_wide")
  monthly_counts_long <- sys_inflow_outflow_monthly_chart_data()
  req(nrow(monthly_counts_long) > 0)

  summary_data <- pivot(
    monthly_counts_long,
    ids = c("Summary", "PlotFillGroups", "Detail"),        # Column(s) defining the rows of the output
    names = "month",       # Column whose values become column names
    values = "Count",  # An arbitrary column to count (used with fun)
    how = "wider",
    fill = 0             # Fill missing combinations with 0
  )
  
  # Get Monthly Change (Inflow - Outflow)
  month_cols <- names(summary_data)[-1:-3]
  inflow_vals <- summary_data[PlotFillGroups == "Inflow", ..month_cols]
  outflow_vals <- summary_data[PlotFillGroups == "Outflow", ..month_cols]
  change_row <- fsum(inflow_vals) - fsum(outflow_vals)

  rbind(
    summary_data,
    add_vars(as.list(change_row), 
             PlotFillGroups = "Monthly Change", 
             Detail = "Monthly Change",
             Summary = "Monthly Change")
  ) %>%
    roworder(PlotFillGroups)
})

### Inactive + FTH ------------------------
sys_inflow_outflow_monthly_single_status_chart_data <- function(varname, status) {
  logToConsole(session, "In sys_inflow_outflow_monthly_single_status_chart_data")
  
  monthly_data <- get_inflow_outflow_monthly() 
  if(nrow(monthly_data) == 0) return(monthly_data)
  
  monthly_data %>%
    fsubset(.[[varname]] == status) %>%
    fgroup_by(month) %>%
    fsummarise(Count = GRPN()) %>%
    roworder(month) %>%
    join(
      data.table(month = unique(monthly_data$month)),
      on = "month",
      how = "right"
    ) %>%
    replace_na(value = 0, cols = "Count")
}

# Summary/Detail (Annual) Chart Prep ---------------------------------------
# Function called in the renderPlot and exports
get_sys_inflow_outflow_annual_plot <- function(id, isExport = FALSE) {
  logToConsole(session, paste0("Getting sys inflow/outflow plot for ", id, ". For export? ", isExport))
  
  df <- sys_inflow_outflow_annual_chart_data()
  
  if (id == "sys_inflow_outflow_summary_ui_chart") {
    df <- df %>%
      # collapse the detailed levels of inflow/outflow
      fmutate(Summary = fct_collapse(Summary, !!!collapse_details)) %>%
      collap(cols="N", ~ InflowOutflow + Summary + PlotFillGroups, fsum, sort=FALSE)
    mid_plot <- 2.5
  } else {
    # re-label Inflow Unspecified if export is < 1094 days
    if(session$userData$days_of_data < 1094)
      df <- df %>%
        ftransform(
          Summary = fct_relabel(Summary, "Inflow \nUnspecified" = "First-Time \nHomeless")
        )

    mid_plot <- 4.5
  }
  
  total_clients <- df[InflowOutflow == "Inflow", sum(N)]

  validate(
    need(
      total_clients > 10,
      message = suppression_msg
    )
  )

  # Order: Housed (start), Homeless (start), Inflow, Outflow, Homeless (end), Housed
  df <- df %>%
    fmutate(
      N = ifelse(InflowOutflow == "Outflow", N * -1, N),
      ystart = lag(cumsum(N), default = 0),
      yend = round(cumsum(N)),
      group.id = GRPid(Summary),
      N_formatted = scales::comma(abs(N))
    ) %>%
    # Remove Active at Start/End bars that are 0, since there's no label other 
    # than legend, which makes it hard to interpret the floating 0
    fsubset(!(N == 0 & PlotFillGroups %in% active_at_levels))
  
  # s <- max(df$yend) + 20
  # num_segments <- 20
  # segment_size <- get_segment_size(s/num_segments)
  total_change <- as.integer(sys_inflow_outflow_totals()[Chart == "Total Change", Value])

  # https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
  ggplot(df, aes(x = group.id, fill = PlotFillGroups)) +
    # the bars
    geom_rect(
      aes(
        # control bar gap width
        xmin = group.id - 0.25,
        xmax = group.id + 0.25,
        ymin = ystart,
        ymax = yend
      ),
      colour = "black",
      linewidth = .5,
      alpha = 0.8
    ) +
    # the connecting segments between bars
    geom_segment(
      data = df %>%
        flast(g = .$group.id) %>%
        fselect(yend, group.id),
      aes(
        x = group.id,
        xend = if_else(group.id == last(group.id), last(group.id), group.id + 1),
        y = yend
      ),
      linewidth = .3,
      color = "gray25",
      linetype = "dashed",
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    # numeric labels for Active at Start/End
    ggrepel::geom_text_repel(
      aes(
        x = group.id,
        label = if_else(grepl("Active at", Summary), N_formatted, NA),
        y = rowSums(cbind(ystart, N / 2))
      ),
      hjust = 1,
      # direction = "y",
      segment.colour = NA,
      nudge_x = ifelse(windowSize()[1] < 1300, -.4, -.3),
      color = "#4e4d47",
      size = sys_chart_text_font,
      inherit.aes = FALSE
    ) +
    # numeric labels for Inflow/Outflow
    geom_text(
      aes(
        x = group.id,
        label = if_else(!grepl("Active at", Summary), N_formatted, NA),
        y = if_else(PlotFillGroups == "Inflow", yend, ystart), 
        vjust = -.6
      ),
      size = sys_chart_text_font
    ) +
    
    ggtitle(
      paste0(
        sys_total_count_display(total_clients),
        "Total Change: ",
        if(total_change > 0) "+" else "", scales::comma(total_change),
        "\n",
        "\n"
      )
    ) +
    
    # color palette
    scale_fill_manual(values = bar_colors) +
    # distance between bars and x axis line
    scale_y_continuous(expand = expansion()) +
    # x axis labels
    scale_x_continuous(
      labels = str_wrap(df$Summary %>% unique(), width = 10),
      breaks = df$group.id %>% unique()
    ) +
    coord_cartesian(clip = "off") +
    # totally clear all theme elements
    theme_void() +
    # add back in what theme elements we want
    theme(
      text = element_text(size = sys_chart_text_font, colour = "#4e4d47"),
      axis.text.x = element_text(
        size = get_adj_font_size(
          sys_axis_text_font * ifelse(windowSize()[1] < 1300, 0.9,1), 
          isExport),
        vjust = -.2), 
      axis.ticks.x = element_line(),
      axis.line.x = element_line(color = "#4e4d47", linewidth = 0.5),
      plot.margin = unit(c(3, 1, 1, 1), "lines"),
      legend.text = element_text(size = get_adj_font_size(sys_legend_text_font, isExport)),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.margin = margin(.5, 0, 0, 0, unit = "inch"),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
}

# custom round to the smaller of the nearest 10, 100, etc.
# good for chart segment sizing
# get_segment_size <- function(x) {
#   thresholds <- c(1, 10, 100, 200, 500, 1000, 1500, 2000, 2500, 5000, 10000)
#   rounded <- sapply(thresholds, function(t) {
#     if (x > t) {
#       return(t * ceiling(x / t))
#     } else {
#       return(NA)
#     }
#   })
#   min(rounded, na.rm = TRUE)
# }

# Render Charts ----------------------------------------
renderInflowOutflowFullPlot <- function(chart_id, alt_text) {
  output[[chart_id]] <- renderPlot({
      req(session$userData$valid_file() == 1)
    
      validate(
        need(
          nrow(get_inflow_outflow_full()) > 0,
          message = no_data_msg
        )
      )
      
      get_sys_inflow_outflow_annual_plot(chart_id)
    },
    alt = alt_text,
    width = ifelse(isTRUE(getOption("shiny.testmode")), 1113, "auto")
  )
}
## Summary (Annual) --------------------------------
renderInflowOutflowFullPlot(
  chart_id = "sys_inflow_outflow_summary_ui_chart",
  alt_text = "A waterfall bar chart of the homeless system's inflow and outflow during 
      the report period. The summary view of this chart includes four components: 
      Active at Start, Inflow, Outflow, and Active at End."
)
## Detail (Annual) -----------------------------------------
renderInflowOutflowFullPlot(
  chart_id = "sys_inflow_outflow_detail_ui_chart",
  alt_text = "A waterfall bar chart of the homeless system's inflow and 
      outflow during the report period. The detailed view of this chart 
      shows inflow as three subcategories: first-time homeless, returned from 
      permanent, and re-engaged from non-permanent and outflow as three 
      subcategories: exited non-permanent, exited permanent, and inactive."
)

## Monthly --------------------------------------------
### MbM Chart --------------------------------------
# Bar - Active at Start + Inflow/Outflow
get_sys_inflow_outflow_monthly_plot <- function(isExport = FALSE) {
  reactive({
    logToConsole(session, "In get_sys_inflow_outflow_monthly_plot")

    plot_data <- sys_inflow_outflow_monthly_chart_data() %>%
      fsubset(PlotFillGroups %in% c(mbm_inflow_levels, mbm_outflow_levels)) %>%
      collap(cols = "Count", FUN=fsum, by = ~ month + PlotFillGroups + Summary) %>%
      fmutate(InflowOutflow = fct_collapse(
        Summary,
        Inflow = inflow_summary_levels,
        Outflow = outflow_summary_levels
      ))
    
    # Get Average Info for Title Display
    averages <- fmean(plot_data$Count, g=plot_data$PlotFillGroups)
    
    # Inflow and Outflow here include Inflow+Active at Start and Outflow+Active at End
    totals_start <- fsum(plot_data[PlotFillGroups == "Inflow", Count])
    totals_end <- fsum(plot_data[PlotFillGroups == "Outflow", Count])
    avg_monthly_change <- (totals_start - totals_end)/(length(session$userData$report_dates) - 1)
    
    plot_data$month_numeric <- as.numeric(as.factor(plot_data$month))
    
    # width = 0.5 means the bar is half the distance between adjacent ticks
    # so width = 0.5 means the bars from adjacent months will be touching
    # 0.4 is probably the highest we can go to maintain the distinction between months
    bar_width <- if_else(isExport, mbm_export_bar_width, mbm_bar_width)
    
    # just = 0.5 means bar is centered around tick
    # just = 1 means bar's right side is aligned with tick
    # just = -1 means bar's left side is aligned with tick
    # this parameter obviously "eats into" into the distance between ticks
    bar_adjust <- if_else(isExport, 1, 1.2)
    
    g <- ggplot(plot_data, aes(x = interaction(month, InflowOutflow), y = Count, fill = PlotFillGroups)) +
      geom_bar(
        data = plot_data[InflowOutflow == "Inflow"] %>%
          fmutate(PlotFillGroups = fct_relevel(
            PlotFillGroups,
            rev(mbm_inflow_levels)
          )),
        aes(x = month, y = Count, fill = PlotFillGroups),
        stat = "identity",
        position = "stack",
        width = bar_width,
        color = 'black',
        just = bar_adjust
      ) +
      geom_bar(
        data = plot_data[InflowOutflow == "Outflow"],
        aes(x = month, y = Count, fill = PlotFillGroups),
        stat = "identity",
        position = "stack",
        width = bar_width,
        color = 'black',
        just = 1 - bar_adjust
      ) +
      theme_minimal() +
      labs(
        x = "Month",
        y = paste0("Count of ", level_of_detail_text())
      ) +
      scale_x_discrete(expand = expansion(mult = c(0.045, 0.045))) + # make plto take up more space horizontally
      scale_fill_manual(
        values = mbm_bar_colors, 
        name = NULL,
        breaks = c(mbm_inflow_levels, mbm_outflow_levels)
      ) + # Update legend title
      ggtitle(
        paste0(
          "Average Monthly Inflow: +", scales::comma(averages["Inflow"], accuracy = 0.1), "\n",
          "Average Monthly Outflow: -", scales::comma(averages["Outflow"], accuracy = 0.1), "\n",
          "Average Monthly Change in ", 
          level_of_detail_text(), " in ", 
          str_remove(getNameByValue(syso_hh_types, input$syso_hh_type), "- "), 
          if_else(getNameByValue(syso_hh_types, input$syso_hh_type) == "All Household Types", "", " Households"),
          ": ", 
          scales::comma(avg_monthly_change, accuracy = 0.1)
        )
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = if(!isExport) element_blank() else element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = sys_axis_text_font),   
        legend.position = if(!isExport) "none" else "bottom",
        panel.grid = element_blank(),       
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        plot.margin = margin(l = 55),        # Increase left margin
        axis.ticks = element_blank(),
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
      )
    
    # For PPT export, add data labels, centered horizontally and vertically within a bar
    if(isExport) {
      g <- g + geom_text(
        data = plot_data[InflowOutflow == "Inflow"] %>%
          fmutate(PlotFillGroups = fct_relevel(
            PlotFillGroups,
            "Inflow",  "Active at Start: Homeless"
          )),
        aes(x = month_numeric - mbm_export_bar_width/2, y = Count, label = Count, group = PlotFillGroups),
        stat = "identity",
        position = position_stack(vjust = 0.5),
        size = 10,
        size.unit = "pt"
      ) +
      geom_text(
        data = plot_data[InflowOutflow == "Outflow"],
        aes(x = month_numeric + mbm_export_bar_width/2, y = Count, label = Count, group = PlotFillGroups),
        stat = "identity",
        position = position_stack(vjust = 0.5),
        color = 'white',
        size = 10,
        size.unit = "pt"
      )
    }
    g
  })
}

output$sys_inflow_outflow_monthly_ui_chart <- renderPlot({
  monthly_chart_validation()
  get_sys_inflow_outflow_monthly_plot()()
})

# Pure line chart -------
# output$sys_inflow_outflow_monthly_ui_chart_line <- renderPlot({
#   plot_data <- sys_inflow_outflow_monthly_chart_data()
#   
#   # Get Average Info for Title Display
#   averages <- plot_data %>%
#     collap(cols = "Count", FUN=fmean, by = ~ Summary)
#   
#   avg_monthly_change <- fmean(
#     plot_data[plot_data$Summary == "Inflow", "Count"] - 
#       plot_data[plot_data$Summary == "Outflow", "Count"]
#   )
#   
#   level_of_detail_text <- case_when(
#     input$syso_level_of_detail == "All" ~ "People",
#     input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
#     TRUE ~
#       getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
#   )
#   
#   ggplot(plot_data, aes(x = month, y = Count, color = PlotFillGroups, group = PlotFillGroups)) +
#     # Use geom_line to draw the lines
#     geom_line(linewidth = 1.2) + # `linewidth` is preferred over `size` for lines >= ggplot2 3.4.0
#     # Optional: Add points to mark the actual data points each month
#     geom_point(size = 3) +
#     
#     scale_color_manual(values = bar_colors, name = "Group") + # Adjust legend title if needed
#     
#     theme_minimal() +
#     labs(
#       x = "Month",
#       # Update Y-axis label to reflect what's plotted
#       y = paste0("Count of ", level_of_detail_text, " (Active at Start)")
#     ) + 
#     # Adjust title to reflect the line chart's focus, but keep avg inflow/outflow context
#     ggtitle(
#       paste0(
#         "Average Monthly Inflow: +", scales::comma(averages[Summary == "Inflow", Count], accuracy = 0.1), "\n",
#         "Average Monthly Outflow: -", scales::comma(averages[Summary == "Outflow", Count], accuracy = 0.1), "\n",
#         "Average Monthly Change in ", 
#           level_of_detail_text, " in ", getNameByValue(syso_hh_types, input$syso_hh_type), ": ", 
#           scales::comma(avg_monthly_change, accuracy = 0.1)
#       )
#     ) +
#     scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 8)) + 
#     theme(
#       axis.text.x = element_blank(),
#       axis.title.x = element_blank(), 
#       axis.title.y = element_text(size = sys_axis_text_font),   
#       legend.position = "none",
#       panel.grid = element_blank(), 
#       axis.text.y = element_text(size = 10, color = "black"), # Example: adjust size and color
#       axis.ticks.y = element_line(color = "black"),
#       axis.line.y = element_line(),          # Remove axis lines
#       axis.line.x = element_line(),          # Remove axis lines
#       plot.margin = margin(l = 55),        # Increase left margin
#       axis.ticks.x = element_blank(),
#       plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
#     )
# })

# Combined line + bar chart -------------
# output$sys_inflow_outflow_monthly_ui_chart_combined <- renderPlot({
#   plot_data <- sys_inflow_outflow_monthly_chart_data()
#   
#   # Get Average Info for Title Display
#   averages <- plot_data %>%
#     collap(cols = "Count", FUN=fmean, by = ~ Summary) # Assuming Summary is equivalent to PlotFillGroups for averages
#   
#   avg_monthly_change <- fmean(
#     plot_data[plot_data$PlotFillGroups == "Inflow", "Count"] - # Use PlotFillGroups here too
#       plot_data[plot_data$PlotFillGroups == "Outflow", "Count"] # Use PlotFillGroups here too
#   )
#   
#   level_of_detail_text <- case_when(
#     input$syso_level_of_detail == "All" ~ "People",
#     input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
#     TRUE ~
#       getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
#   )
#   
#   # --- Define categories for easier filtering ---
#   bar_categories <- c("Active at Start: Housed", "Active at Start: Homeless")
#   line_categories <- c("Inflow", "Outflow")
#   
#   # --- Build the ggplot ---
#   ggplot(mapping = aes(x = month)) +  # Base plot with only x aesthetic
#     
#     # --- Stacked Bars Layer ---
#     geom_col(
#       data = plot_data[Summary == "Active at Start"],
#       aes(y = Count, fill = PlotFillGroups),
#       position = "stack",
#       width = 0.3
#     ) +
#     
#     # --- Lines Layer ---
#     geom_line(
#       data = plot_data %>% filter(PlotFillGroups %in% line_categories),
#       aes(y = Count, color = PlotFillGroups, group = PlotFillGroups), # Group is important for lines
#       linewidth = 1.4
#     ) +
#     
#     # --- Points Layer (for lines) ---
#     geom_point(
#       data = plot_data %>% filter(PlotFillGroups %in% line_categories),
#       aes(y = Count, fill = PlotFillGroups), # No group needed if color is mapped
#       shape = 21,                            # <--- ADDED: Use shape 21 for border/fill control
#       color = "black",                       # <--- ADDED: Set border color to black (outside aes)
#       size = 3
#     ) +
#     
#     # --- Scales ---
#     # Use scale_fill_manual for the bars
#     scale_fill_manual(values = bar_colors) +
#     
#     # Use scale_color_manual for the lines/points
#     scale_color_manual(values = bar_colors, name = "Group") +
#     
#     # --- Theme and Labels ---
#     theme_minimal() +
#     labs(
#       x = "Month",
#       # Update Y-axis label to be more general
#       y = paste0("Count of ", level_of_detail_text)
#     ) +
#     ggtitle(
#       paste0(
#         "Average Monthly Inflow: +", scales::comma(averages[Summary == "Inflow", Count], accuracy = 0.1), "\n",
#         "Average Monthly Outflow: -", scales::comma(averages[Summary == "Outflow", Count], accuracy = 0.1), "\n",
#         "Average Monthly Change in ",
#         level_of_detail_text, " in ", getNameByValue(syso_hh_types, input$syso_hh_type), ": ",
#         scales::comma(avg_monthly_change, accuracy = 0.1)
#       )
#     ) +
#     scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 8)) +
#     theme(
#       axis.text.x = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = sys_axis_text_font),
#       legend.position = "none",
#       panel.grid = element_blank(),
#       axis.text.y = element_text(size = 10, color = "black"),
#       axis.ticks.y = element_line(color = "black"),
#       axis.line.y = element_line(),
#       axis.line.x = element_line(),
#       plot.margin = margin(l = 55),
#       axis.ticks.x = element_blank(),
#       plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
#     )
#   
# })


### Table --------------------------------------
# The table is positioned directly under the chart
# Making the month labels looks like both the chart's x-axis and the table's column headers
get_sys_inflow_outflow_monthly_table <- reactive({
  logToConsole(session, "In sys_inflow_outflow_monthly_table")

  summary_data_wide <- sys_monthly_chart_data_wide() %>%
    fsubset(PlotFillGroups %in% c(mbm_inflow_levels, mbm_outflow_levels, "Monthly Change"))
  
  req(nrow(summary_data_wide) > 0)
  
  if(input$mbm_status_filter == "First-Time Homeless")
    summary_data_with_change <- summary_data_wide %>%
      fsubset(PlotFillGroups == "Inflow" & Detail == "First-Time \nHomeless") %>%
      ftransform(PlotFillGroups = input$mbm_status_filter) %>%
      fselect(-Detail, -Summary)
  else if(input$mbm_status_filter == "Inactive")
    summary_data_with_change <- summary_data_wide %>%
      fsubset(PlotFillGroups == "Outflow" & Detail == "Inactive") %>%
      ftransform(PlotFillGroups = input$mbm_status_filter) %>%
      fselect(-Detail, -Summary)
  else {
    summary_data_wide <- summary_data_wide %>% fselect(-Detail, -Summary)
    summary_data_with_change <- collap(
      summary_data_wide, 
      cols=names(summary_data_wide %>% fselect(-PlotFillGroups)),
      FUN="fsum", 
      by = ~ PlotFillGroups
    )
  }
  
  req(nrow(summary_data_with_change) > 0)
  
  monthly_dt <- datatable(
    summary_data_with_change %>% frename("PlotFillGroups" = " "),
    options = list(
      dom = 't',
      ordering = FALSE,
      # pageLength = 4,
      columnDefs = list(
        list(width = "48px", targets = 0), # Set first column width
        list(className = 'dt-center', targets = '_all') # Center text
      )
    ),
    style = "default",
    rownames = FALSE
  )  %>%
    # Highlight only the first column of "Inflow" and "Outflow" rows
    formatStyle(
      columns = 1,  # First column
      target = "cell",
      backgroundColor = styleEqual(
        c(mbm_inflow_levels, mbm_outflow_levels),
        unname(mbm_bar_colors)
      ),
      border = styleEqual(
        c(mbm_inflow_levels, mbm_outflow_levels),
        c(rep("2px solid black", 4))
      )
    ) %>%
    # Contrast font and background colors
    formatStyle(
      columns = 1,
      target = "cell",
      color = styleEqual(
        mbm_outflow_levels, 
        rep("white", length(mbm_outflow_levels))
      )
    )
  
  if(input$mbm_status_filter == "All") {
    # Highlight max inflow
    month_cols <- names(summary_data_with_change)[-1]
    change_row <- as.integer(summary_data_with_change[PlotFillGroups == "Monthly Change", ..month_cols])

    if(any(change_row > 0, na.rm=TRUE)) {
      monthly_dt <- monthly_dt %>%
        formatStyle(
          columns = month_cols[which.max(change_row)],
          target = "cell",
          backgroundColor = styleRow(
            nrow(summary_data_with_change), 
            mbm_bar_colors["Inflow"]
          )
        )
    }
    
    if(any(change_row < 0, na.rm=TRUE)) {
      monthly_dt <- monthly_dt %>%
        formatStyle(
          columns = month_cols[which.min(change_row)],
          target = "cell",
          color = styleRow(nrow(summary_data_with_change), 'white'),
          backgroundColor = styleRow(nrow(summary_data_with_change), mbm_bar_colors["Outflow"])
        )
    }
  }
  monthly_dt
})

get_sys_inflow_outflow_monthly_flextable <- function() {
  logToConsole(session, "In get_sys_inflow_outflow_monthly_flextable")
  d <- sys_monthly_chart_data_wide() %>% 
    fselect(-Detail, -Summary) %>%
    fsubset(PlotFillGroups %in% c(mbm_inflow_levels, mbm_outflow_levels, "Monthly Change"))
  d <- collap(
    d, 
    cols=names(d %>% fselect(-PlotFillGroups)),
    FUN="fsum", 
    by = ~ PlotFillGroups
  ) %>%
    frename("PlotFillGroups" = " ")
  
  ft <- flextable(d) %>%
    width(j = 1, width = 0.9) %>% # make first col narrower
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    border(border.top = fp_border(), part = "header") %>%
    border_inner_h(border = fp_border(color = "grey", width = 0.5), part = "body")
    
  row_labels <- d[[1]]
  
  # Formatting the inflow/outflow row labels
  inflow_outflow_row_indices <- which(row_labels %in% c(mbm_inflow_levels, mbm_outflow_levels))
  outflow_row_indices <- which(row_labels %in% c("Active at End: Housed", "Outflow"))

  ft <- ft %>%
    # Background colors from datatable's formatStyle
    bg(i = inflow_outflow_row_indices, j = 1, bg = mbm_bar_colors) %>%
    # thick borders for the first column
    border(i = inflow_outflow_row_indices, j = 1, border = fp_border(color = "black", width = 2)) %>%
    # Make outflow cells have contrasting white font color
    color(i = outflow_row_indices, j = 1, color = "white")

  # Highlight the monthly change inflow and outflow vals
  monthly_change_row <- which(row_labels == "Monthly Change")
  monthly_change_vals <- d[monthly_change_row, names(d)[-1]]

  ft %>%
    bg(i = monthly_change_row, j = which.max(monthly_change_vals) + 1, mbm_inflow_bar_colors["Inflow"]) %>%
    bg(i = monthly_change_row, j = which.min(monthly_change_vals) + 1, mbm_outflow_bar_colors["Outflow"]) %>%
    color(i = monthly_change_row, j = which.min(monthly_change_vals) + 1, color = "white")
}

output$sys_inflow_outflow_monthly_table <- renderDT({
  monthly_chart_validation()
  get_sys_inflow_outflow_monthly_table()
})

### Inactive + FTH chart --------------------------------------
sys_monthly_single_status_ui_chart <- function(varname, status, isExport = FALSE) {
  logToConsole(session, "In sys_monthly_single_status_ui_chart")

  plot_data <- sys_inflow_outflow_monthly_single_status_chart_data(
    varname, 
    status
  )

  ggplot(plot_data, aes(x = month, y = Count)) +
    geom_col(fill = mbm_single_status_chart_colors[[status]], width = 0.3, color = "black") +
    geom_text(aes(label = Count), vjust = -0.5, size = sys_chart_text_font) +
    theme_minimal() +
    labs(
      x = "Month",
      y = paste0("Count of ", level_of_detail_text())
    ) +
    scale_x_discrete(expand = expansion(mult = c(0.045, 0.045))) + # make plto take up more space horizontally
    theme(
      axis.text.x = if(isExport) element_text(size = sys_axis_text_font, face = "bold") else element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_text(size = sys_axis_text_font), 
      axis.title.x = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.line.x = element_line(),          
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(l = 55),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
}
output$sys_inactive_monthly_ui_chart <- renderPlot({
  monthly_chart_validation()
  sys_monthly_single_status_ui_chart("OutflowTypeDetail", "Inactive")
})

output$sys_fth_monthly_ui_chart <- renderPlot({
  monthly_chart_validation()
  sys_monthly_single_status_ui_chart("InflowTypeDetail", "First-Time \nHomeless")
})

monthly_chart_validation <- function() {
  logToConsole(session, "In monthly_chart_validation")
  num_people <- length(unique(get_inflow_outflow_monthly()$PersonalID))
  
  validate(
    need(
      num_people > 0,
      message = no_data_msg
    )
  )
  
  validate(
    need(
      num_people > 10,
      message = suppression_msg
    )
  )
}

# Info to include in Inflow/Outflow Exports -----------------------------------
sys_inflow_outflow_totals <- reactive({
  logToConsole(session, "In sys_inflow_outflow_totals")
  
  df <- sys_inflow_outflow_annual_chart_data()
  data.table(
    Chart = c(
      paste0(
        full_unit_of_analysis_display(),
        " Served (Start + Inflow)"
      ),
      "Total Inflow",
      "Total Outflow",
      "Total Change"
    ),
    Value = as.character(c(
      sum(df[InflowOutflow == 'Inflow']$N, na.rm = TRUE),
      sum(df[Summary %in% inflow_detail_levels]$N, na.rm = TRUE),
      sum(df[Summary %in% outflow_detail_levels]$N, na.rm = TRUE),   
      sum(df[PlotFillGroups == "Inflow"]$N, na.rm = TRUE) -
        sum(df[PlotFillGroups == "Outflow"]$N, na.rm = TRUE)
    ))
  )
})

# Tabular/Excel Export --------------------------------------------------------
## Monthly Export function------
sys_export_monthly_info <- function() {
  logToConsole(session, "In sys_export_monthly_info")
  monthly_counts_wide <- sys_monthly_chart_data_wide()
  
  month_cols <- names(monthly_counts_wide)[-1:-3]

  monthly_counts_detail = monthly_counts_wide %>%
    ftransform(Detail = fct_relabel(Detail, function(d) gsub(" \n"," ",d))) %>%
    fselect(-PlotFillGroups)
    
  monthly_totals <- monthly_counts_wide %>%
    fsubset(PlotFillGroups %in% inflow_outflow_levels) %>%
    collap(cols=month_cols, FUN="fsum", by = ~ PlotFillGroups) %>%
    frename(PlotFillGroups = "Detail") %>%
    fmutate(
      Summary = Detail, 
      Detail = factor(paste0("Total ", Detail))
    )
  
  monthly_counts <- bind_rows(monthly_counts_detail, monthly_totals) %>%
    roworder(Summary, Detail)
  
  monthly_average_cols <- c("Total Inflow", "Total Outflow", "Monthly Change")
  monthly_averages <- data.table(
    Chart = paste0("Average ", gsub("Total ", "", monthly_average_cols)),
    Value = as.character(
      scales::comma(
        rowMeans(
          monthly_counts[Detail %in% monthly_average_cols, ..month_cols]
        )*c(1,-1,1),
        accuracy = 0.1
      )
    )
  )
  return(
    list(
      monthly_counts = monthly_counts,
      monthly_averages = monthly_averages
    )
  )
}

## Sys Inflow/Outflow Download Handler ------
# downloads all Inflow/Outflow chart data, including MbMs
output$sys_inflow_outflow_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Flow Report - "),
  content = function(file) {
    logToConsole(session, "Inflow/Outflow data download")

    df <- sys_inflow_outflow_annual_chart_data() %>% 
      ftransform(
        Summary = fct_collapse(Summary, !!!collapse_details),
        Detail = fct_relabel(Detail, function(d) gsub(" \n"," ",d))
      )
    
    if(session$userData$days_of_data < 1094) {
      df <- df %>%
        ftransform(
          Detail = fct_recode(Detail, "Inflow Unspecified" = "First-Time Homeless")
        )
    }
    
    totals_df <- df %>% 
      fgroup_by(Summary) %>% 
      fsummarise(Detail = paste0("Total ", Summary[1]),
                 N = fsum(N, na.rm = TRUE))

    monthly_data <- sys_export_monthly_info()

    write_xlsx(
      list(
        "System Flow Metadata" = sys_export_summary_initial_df() %>%
          bind_rows(
            sys_export_filter_selections(),
            sys_inflow_outflow_totals(),
            monthly_data$monthly_averages
          ) %>%
          mutate(Value = replace_na(Value, 0)) %>%
          rename("System Flow" = Value),
        "System Flow Summary" = bind_rows(df, totals_df) %>%
          roworder(Summary) %>%
          fselect(
            "Summary Category" = Summary,
            "Detail Category" = Detail,
            "Count" = N
          ),
        "System Flow Data Monthly" = monthly_data$monthly_counts
      ),
      path = file,
      format_headers = FALSE,
      col_names = TRUE
    )

    logMetadata(session, paste0("Downloaded System Overview Tabular Data: ", input$syso_tabbox,
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
    exportTestValues(sys_inflow_outflow_report = summarize_df(df))
  }
)

# PowerPoint/Image Export -----------------------------------------------------
output$sys_inflow_outflow_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Flow_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    logToConsole(session, "In sys_inflow_outflow_download_btn_ppt")
    monthly_data <- sys_export_monthly_info()
    
    sys_overview_ppt_export(
      file = file,
      title_slide_title = "System Flow",
      summary_items = sys_export_summary_initial_df() %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(
          sys_export_filter_selections(),
          sys_inflow_outflow_totals(),
          monthly_data$monthly_averages
        ),
      plots = list(
        "System Inflow/Outflow Summary" = get_sys_inflow_outflow_annual_plot(
          "sys_inflow_outflow_summary_ui_chart",
          isExport = TRUE
        ),
        "System Inflow/Outflow Detail" = get_sys_inflow_outflow_annual_plot(
          "sys_inflow_outflow_detail_ui_chart",
          isExport = TRUE
        ),
        "System Inflow/Outflow Monthly – All" = get_sys_inflow_outflow_monthly_plot(isExport = TRUE)(),
        "System Inflow/Outflow Monthly – Table" = get_sys_inflow_outflow_monthly_flextable(),
        "System Inflow/Outflow Monthly – First-Time Homeless" = sys_monthly_single_status_ui_chart("InflowTypeDetail", "First-Time \nHomeless", isExport = TRUE),
        "System Inflow/Outflow Monthly – Inactive" = sys_monthly_single_status_ui_chart("OutflowTypeDetail", "Inactive", isExport = TRUE)
      ),
      summary_font_size = 19
    )
  }
)

qc_checks <- function() {
  browser()

  # TESTING DIFF BETWEEN FULL AND MBM
  full <- period_specific_data()[["Full"]]
  all_months <- period_specific_data()[["Months"]]
  setdiff(sort(unique(full$PersonalID)), sort(unique(all_months$PersonalID)))
  setdiff(sort(unique(all_months$PersonalID)), sort(unique(full$PersonalID)))
  # 
  # 
  # json_str <- jsonlite::toJSON(
  #   unique(
  #     merge(
  #       enrollment_categories[
  #         !(PersonalID %in% unique(full$PersonalID)),
  #         .(EnrollmentID, PersonalID, EntryDate, ExitAdjust, ProjectType, lh_prior_livingsituation)
  #       ],
  #       homeless_cls[, .(EnrollmentID, InformationDate)],
  #       by = "EnrollmentID"
  #     )[, .(PersonalID, InformationDate, EntryDate, ExitAdjust, ProjectType, lh_prior_livingsituation)]
  #   ),
  #   null="list",
  #   na="string",
  #   pretty = TRUE, auto_unbox = TRUE
  # )
  # json_str <- gsub("true", "True", json_str)
  # json_str <- gsub("false", "False", json_str)
  # 
  #
  # # Check that one enrollment isn't considered an inflow in multiple months
  # # someone can have an exit in between or they can be active at start
  #
  monthly_universe_ppl_flags <- get_inflow_outflow_monthly()
  
  # check prior month's status (L() is the collapse package's Lag function)
  # if they outflowed last month, they should have inflowed this month
  # and if they were active at end last month, they should be active at start, this month
  full_combinations <- CJ(
    PersonalID = unique(monthly_universe_ppl_flags$PersonalID), 
    month = levels(monthly_universe_ppl_flags$month)
  )
  
  # check for dup First-Time Homeless 
  first_time_homeless_dups <- all_months[
    InflowTypeDetail == "First-Time \nHomeless"
  ][, .N, by = PersonalID][N > 1]
  if(nrow(first_time_homeless_dups) > 0) {
    warning(glue("There are people that have multiple First-Time Homeless values in the mbm."))
    print(all_months[PersonalID %in% first_time_homeless_dups$PersonalID, .(
      PersonalID, 
      EnrollmentID,
      EntryDate,
      ExitAdjust,
      month,
      InflowTypeDetail,
      active_at_start_homeless,
      active_at_start_housed,
      # at_least_14_days_to_eecr_enrl,
      # first_lookback_perm_dest,
      # first_lookback_temp_dest,
      return_from_perm_client,
      reengaged_from_temp_client,
      first_time_homeless_client
      # unknown_at_start
    )])
  }
  
  qc <- monthly_universe_ppl_flags %>%
    join(
      full_combinations,
      on = c("PersonalID", "month"),
      how = "full"
    ) %>%
    fsubset(order(PersonalID, month)) %>%
    fmutate(problem = !(
      (L(OutflowTypeSummary, g = PersonalID) == "Outflow" & Summary == "Inflow") | 
      (L(OutflowTypeSummary, g = PersonalID) == "Active at End" & InflowTypeSummary %in% c("Inflow","Active at Start")) |
      (L(OutflowTypeSummary, g = PersonalID) == "Inactive" & InflowTypeSummary %in% c("Inflow","Active at Start"))
    )) %>%
    fgroup_by(PersonalID) %>%
    fmutate(
      has_problem = anyv(problem, TRUE)
    ) %>%
    fungroup() %>%
    fsubset(has_problem)

}
