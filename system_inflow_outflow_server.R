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
  "Excluded",
  "First-of-Month Exit",
  "something's wrong"
)

outflow_detail_levels <- c(
  "Exited, \nNon-Permanent",
  "Exited, \nPermanent",
  "Inactive",
  "Continuous at End",
  "Last-of-Month Entry",
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
  "Continuous at Start",
  "Excluded",
  "First-of-Month Exit",
  "something's wrong"
)
inflow_summary_chart_levels <- c(
  "Active at Start",
  "Inflow",
  "something's wrong"
)
outflow_summary_levels <- c(
  "Outflow",
  "Active at End",
  "Continuous at End",
  "Last-of-Month Entry",
  "something's wrong"
)

outflow_summary_chart_levels <- c(
  "Outflow",
  "Active at End",
  "something's wrong"
)

inflow_statuses_to_exclude_from_chart <- c(
  "Continuous at Start",
  "Unknown",
  "Excluded",
  "First-of-Month Exit",
  "something's wrong"
)
inflow_statuses_to_exclude_from_export <- c(
  "Excluded",
  "First-of-Month Exit",
  "something's wrong"
)

outflow_statuses_to_exclude_from_chart <- c(
  "Continuous at End",
  "Last-of-Month Entry",
  "something's wrong"
)
outflow_statuses_to_exclude_from_export <- c(
  "something's wrong"
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
  fcase(
    input$syso_level_of_detail == "All", "People",
    input$syso_level_of_detail == "HoHsOnly", "Heads of Household",
    default =
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
  
  # ----|-z---z------a-nov-a------------- ===> ASH
  # ----|----------a-nov-a--------------- ===> ASH
  # ----|-z----z---nov-a----------a-------===> continuous
  # ----|----------nov-a----------a-------===> continuous
  # ----|-z----z---nova----------a------- ===> continuous
  # ----|----------nova----------a------- ===> FTH
  # z---|----------nova----------a------- ===> Re-engaged/returned
  all_filtered_w_lh %>% fmutate(
    # INFLOW CALCULATOR COLUMNS
    active_at_start_homeless = eecr & was_lh_at_start,
    
    active_at_start_housed = eecr & was_housed_at_start,
    
    return_from_perm = eecr & lookback_dest_perm &
      (startDate == session$userData$ReportStart | ExitAdjust != startDate) & 
      !(EntryDate < startDate & ProjectType %in% nbn_non_res) & (
        days_since_lookback %between% c(15, 730) |
        (days_since_lookback %between% c(0, 14) & lookback_is_nonres_or_nbn & (days_since_last_lh >= 15 | is.na(days_since_last_lh)))
      ),
    
    return_from_nonperm = eecr & 
      (startDate == session$userData$ReportStart | ExitAdjust != startDate) & (
        (days_since_lookback %between% c(15, 730) & !lookback_dest_perm & !(EntryDate < startDate & ProjectType %in% nbn_non_res)) |
        (days_since_lookback %between% c(0, 14) & !(EntryDate < startDate & ProjectType %in% nbn_non_res) & lookback_is_nonres_or_nbn &  (days_since_last_lh >= 15 | is.na(days_since_last_lh))) |
        # This condition is meant to capture cases where Inactive nonres/nbn enrollments 
        # are exited and then immediately followed up with a new enrollment.
        (
          ProjectType %in% nbn_non_res &
          !was_lh_at_start &
          straddles_start & 
          was_lh_during_period
        )
      ),
    
    # enrollments can be FTH even if they fully straddle, as long as it's a non-res
    # with no LH PLS. This is because, if they're in the dataset at all, they must have had
    # LH somewhere during the enrollment, and they're already in the period, so the 
    # LH must have been somewhere 
    # --L/no L------------------||--x----------x--|-----------------|-------------------|------------||
    # --L/no L--------x(no LH)--||--x-------------|-----------------|-------------------|------------||
    # e.g. 614071, ICF-good, should be FTH inflow for Enrollment 837695 in March
    # we know it's the only enrollment they have an there's lookback within last 2 yrs
    
    
    # in other cases, we have long non-res enrollments and then an LKH CLS pops up and they're re-engaged.
    # that's because there's a lookback or they were already categorized in a previous month
    first_time_homeless = eecr & 
      (days_since_lookback > 730 | is.na(days_since_lookback)) & 
      first_lh_date >= startDate &
      EntryDate > session$userData$ReportStart,
    
    unknown_at_start = eecr & 
      straddles_start & 
      ProjectType %in% nbn_non_res &
      !was_lh_at_start,
    
    # Exclude non-res-only clients with incomplete or conflicting LH data
    non_res_excluded = eecr & 
      ProjectType %in% nbn_non_res &
      !was_lh_at_start & 
      days_since_lookback %between% c(0, 14) &
      (is.na(days_since_last_lh) | days_since_last_lh < 0),
    
    # Beginning with the first month's Outflow and ending after the last month's Inflow, 
    # there should be "continuous_at_start" and "continuous_at_end" flags that 
    # capture EECRs/LECRs that begin AFTER/BEFORE period start/end, 
    # but days_to_lookahead/lookback <= 14. These would not be included on the chart.
    # so both flags do not apply to first month. Continuous_at_end also doesn't apply to last
    continuous_at_start = eecr & 
      startDate > session$userData$ReportStart &
      EntryDate >= startDate & days_since_lookback %between% c(0, 14),
    
    continuous_at_end = lecr & 
      endDate < session$userData$ReportEnd &
      ExitAdjust <= endDate & days_to_lookahead %between% c(0, 14),
    
    # New Inflow category:"first_of_the_month_exit" should not show up in chart 
    # or export, even though the person's outflow should be counted
    first_of_the_month_exit = eecr & 
      startDate > session$userData$ReportStart &
      ExitAdjust == startDate,
    
    # similar outflow status
    last_of_the_month_entry = lecr & 
      endDate < session$userData$ReportEnd &
      EntryDate == endDate,
    
    # OUTFLOW CALCULATOR COLUMNS
    exited_system = lecr &
      ExitAdjust %between% list(startDate, endDate) &
      (!continuous_at_end | is.na(continuous_at_end)),
    
    homeless_at_end = lecr & was_lh_at_end,
    
    housed_at_end = lecr & was_housed_at_end,
    
    unknown_at_end = lecr &
      straddles_end & 
      ProjectType %in% nbn_non_res &
      !was_lh_at_end
  )
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
      active_at_start_homeless_client = any(active_at_start_homeless),
      active_at_start_housed_client = any(active_at_start_housed),
      return_from_perm_client = any(return_from_perm),
      reengaged_from_temp_client = any(return_from_nonperm),
      first_time_homeless_client = any(first_time_homeless),
      unknown_at_start_client = any(unknown_at_start),
      non_res_excluded_client = any(non_res_excluded),
      first_of_the_month_exit_client = any(first_of_the_month_exit),
      continuous_at_start_client = any(continuous_at_start),
      
      # OUTFLOW
      perm_dest_client = any(exited_perm),
      temp_dest_client = any(exited_temp),
      homeless_at_end_client = any(homeless_at_end),
      housed_at_end_client = any(housed_at_end),
      unknown_at_end_client = any(unknown_at_end),
      last_of_the_month_entry_client = any(last_of_the_month_entry),
      continuous_at_end_client = any(continuous_at_end)
    ) %>%
    fungroup() %>%
    ftransform(
      InflowTypeSummary = factor(
        fcase(
          active_at_start_homeless_client | active_at_start_housed_client, "Active at Start",
          first_time_homeless_client | return_from_perm_client | reengaged_from_temp_client | unknown_at_start_client, "Inflow",
          continuous_at_start_client, "Continuous at Start",
          non_res_excluded_client, "Excluded",
          first_of_the_month_exit_client, "First-of-Month Exit",
          default = "something's wrong"
        ), levels = inflow_summary_levels
      ),
      
      InflowTypeDetail = factor(
        fcase(
          active_at_start_homeless_client, "Homeless",
          active_at_start_housed_client, "Housed",
          first_time_homeless_client, "First-Time \nHomeless",
          return_from_perm_client, "Returned from \nPermanent",
          reengaged_from_temp_client, "Re-engaged from \nNon-Permanent",
          continuous_at_start_client, "Continuous at Start",
          unknown_at_start_client, "Unknown",
          non_res_excluded_client, "Excluded",
          first_of_the_month_exit_client, "First-of-Month Exit",
          default = "something's wrong"
        ), levels = c(active_at_levels, inflow_detail_levels)
      ),
      
      OutflowTypeSummary = factor(
        fcase(
          perm_dest_client | temp_dest_client | unknown_at_end_client, "Outflow",
          homeless_at_end_client | housed_at_end_client, "Active at End",
          continuous_at_end_client, "Continuous at End",
          last_of_the_month_entry_client, "Last-of-Month Entry",
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
          last_of_the_month_entry_client, "Last-of-Month Entry",
          default = "something's wrong"
        ), levels = c(outflow_detail_levels, rev(active_at_levels))
      )
    )

  ####
  # Dropping first period Unknowns + multiple Inactives in a row ----------------
  ####
  enrollments_to_remove <- universe_w_ppl_flags %>%
    fsubset(period != "Full", PersonalID, period, InflowTypeDetail, OutflowTypeDetail) %>%
    funique() %>%
    setorder(PersonalID, period) %>%
    fgroup_by(PersonalID) %>%
    fmutate(first_inflow = ffirst(InflowTypeDetail)) %>%
    fungroup() %>%
    fmutate(prev_period_outflow = flag(OutflowTypeDetail, g = PersonalID)) %>%
    fsubset(
      # Remove first Unknown
      first_inflow == "Unknown" |
      # Remove multiple inactives
      (OutflowTypeDetail == "Inactive" & prev_period_outflow == "Inactive")
    ) %>%
    fselect(PersonalID, period)
  
  universe_w_ppl_flags_clean <- universe_w_ppl_flags %>%
    join(enrollments_to_remove, on = c("PersonalID", "period"), how="anti")

  rm(enrollments_to_remove, universe_w_ppl_flags)
  
  if(!in_dev_mode) {
    universe_w_ppl_flags_clean <- universe_w_ppl_flags_clean %>%
      fselect(
        PersonalID,
        InflowTypeSummary,
        InflowTypeDetail,
        OutflowTypeSummary,
        OutflowTypeDetail,
        ProjectType,
        period,
        EnrollmentID, 
        eecr,
        lecr,
        EntryDate,
        days_since_lookback,
        MoveInDateAdjust,
        HouseholdType, 
        CorrectedHoH, 
        LivingSituation, 
        ExitAdjust, 
        Destination
      ) %>%
      funique()
  }
  
  # universe_w_ppl_flags_clean[PersonalID == 576213, .(PersonalID, period, EnrollmentID, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, eecr, lecr, InflowTypeDetail, OutflowTypeDetail, keep_flag)]
  ####
  # Error Checking ------------------
  ####
  ## Random 10 people from each group
  # sampled <- universe_w_ppl_flags_clean[period == "Full"][
  #   , .SD[sample(.N, min(10, .N))],
  #   by = .(InflowTypeDetail, OutflowTypeDetail)
  # ]
  # 
  # sampled_final <- sampled %>%
  #   fselect(PersonalID) %>%
  #   funique() %>%
  #   join(enrollment_categories_all, on = "PersonalID", multiple=T) %>%
  #   join(universe_w_ppl_flags_clean, on = "EnrollmentID", multiple=TRUE) %>%
  #   fselect(PersonalID, period, eecr, lecr, EnrollmentID, EntryDate, MoveInDateAdjust, ExitAdjust, ProjectType, lh_prior_livingsituation, InformationDate, DateProvided, InflowTypeDetail, OutflowTypeDetail) %>%
  #   setorder(PersonalID, period, eecr, lecr)
  # 
  # fwrite(sampled_final, "/media/sdrive/projects/CE_Data_Toolkit/QC_Inflow_Outflow_Statuses_8.20.25_v2.csv")

  ## Inflow Unknown in Full Period -------
  bad_records <- universe_w_ppl_flags_clean %>%
    fsubset(InflowTypeDetail == "Unknown" & period == "Full")
  if(nrow(bad_records) > 0) {
    logToConsole(session, "ERROR: There's an Inflow-Unknown in the Full Annual data")
    if(in_dev_mode & !isTRUE(getOption("shiny.testmode"))) {
      bad_records <- get_all_enrollments_for_debugging(bad_records, universe_w_ppl_flags_clean) %>% 
        fselect(inflow_debug_cols)
      view(bad_records)
      browser()
    }
  }
  
  ## Something's Wrong -------
  bad_records <- universe_w_ppl_flags_clean %>%
    fsubset(
      InflowTypeSummary == "something's wrong" | 
      OutflowTypeSummary == "something's wrong"
    )
  if(nrow(bad_records) > 0) {
    logToConsole(session, "ERROR: There are clients whose Inflow or Outflow is 'something's wrong'")
    if(in_dev_mode & !isTRUE(getOption("shiny.testmode"))) {
      somethings_wrongs <- get_all_enrollments_for_debugging(bad_records, universe_w_ppl_flags_clean, multiple=TRUE) %>%
        fgroup_by(PersonalID) %>%
        fmutate(
          has_inflow_wrong = anyv(InflowTypeDetail, "something's wrong"),
          has_outflow_wrong = anyv(OutflowTypeDetail, "something's wrong"),
          has_continuous_at_start = anyv(InflowTypeDetail, "Continuous at Start"),
          has_continuous_at_end = anyv(OutflowTypeDetail, "Continuous at End")
        ) %>%
        fungroup()
      
      if(nrow(somethings_wrongs[has_inflow_wrong == TRUE]) > 0) view(somethings_wrongs[has_inflow_wrong == TRUE] %>% fselect(inflow_debug_cols, "has_continuous_at_start"))
      if(nrow(somethings_wrongs[has_outflow_wrong == TRUE]) > 0) view(somethings_wrongs[has_outflow_wrong == TRUE] %>% fselect(outflow_debug_cols, "has_continuous_at_end"))
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
    
    logToConsole(session, "ERROR: There are something's wrong records in the universe_ppl_flags data")
  }

  ## First/Last Month Inflow/Outflow != Full Inflow/Outflow-------
  bad_records <- universe_w_ppl_flags_clean %>%
    fgroup_by(PersonalID) %>%
    fsummarize(
      first_enrl_month_inflow = ffirst(fifelse(eecr & period != "Full", InflowTypeDetail, NA)),
      full_period_inflow = ffirst(fifelse(eecr & period == "Full", InflowTypeDetail, NA)),
      
      last_enrl_month_outflow = flast(fifelse(lecr & period != "Full", OutflowTypeDetail, NA)),
      last_enrl_month_outflow_noninactive = flast(fifelse(lecr & period != "Full" & OutflowTypeDetail != "Inactive", OutflowTypeDetail, NA)),
      full_period_outflow = flast(fifelse(lecr & period == "Full", OutflowTypeDetail, NA))
    ) %>%
    fungroup() %>%
    fsubset(
      first_enrl_month_inflow != full_period_inflow |
      (last_enrl_month_outflow != full_period_outflow & full_period_outflow == "Inactive") |
      (last_enrl_month_outflow_noninactive != full_period_outflow & full_period_outflow != "Inactive")
    )
  if(nrow(bad_records) > 0)  {
    logToConsole(session, "ERROR: There are clients whose first-month Inflow != Full Period Inflow and/or last-month Outflow != Full Period outflow")
    if(in_dev_mode & !isTRUE(getOption("shiny.testmode"))) {
      bad_first_inflow_records <- get_all_enrollments_for_debugging(
        bad_records[first_enrl_month_inflow != full_period_inflow],
        universe_w_ppl_flags_clean,
        multiple = TRUE
      )
      if(nrow(bad_first_inflow_records) > 0) {
        bad_first_inflow_records <- bad_first_inflow_records %>%
          fgroup_by(PersonalID) %>%
          fmutate(
            has_something_wrong = anyv(InflowTypeDetail, "something's wrong") | 
                                  anyv(OutflowTypeDetail, "something's wrong"),
            has_continuous_at_start = anyv(InflowTypeDetail, "Continuous at Start")
          ) %>%
          fungroup() %>%
          fsubset(!has_something_wrong)
        
        if(nrow(bad_first_inflow_records) > 0)
          view(bad_first_inflow_records %>% fselect(c(inflow_debug_cols, "has_continuous_at_start")))
      }
      
      bad_last_outflow_records <- get_all_enrollments_for_debugging(
        bad_records[
          (last_enrl_month_outflow != full_period_outflow & full_period_outflow == "Inactive") |
          (last_enrl_month_outflow_noninactive != full_period_outflow & full_period_outflow != "Inactive")
        ],
        universe_w_ppl_flags_clean,
        multiple = TRUE
      )
      if(nrow(bad_last_outflow_records) > 0) {
        bad_last_outflow_records <- bad_last_outflow_records %>%
          fgroup_by(PersonalID) %>%
          fmutate(
            has_something_wrong = anyv(InflowTypeDetail, "something's wrong") | 
                                  anyv(OutflowTypeDetail, "something's wrong"),
            has_continuous_at_end = anyv(OutflowTypeDetail, "Continuous at End")
          ) %>%
          fungroup() %>%
          fsubset(!has_something_wrong)
        
        if(nrow(bad_last_outflow_records) > 0)
          view(bad_last_outflow_records %>% fselect(c(outflow_debug_cols, "has_continuous_at_end")))
      }
      browser()
      # universe_w_ppl_flags_clean[PersonalID == 565354, .(PersonalID, period, EnrollmentID, ProjectType, EntryDate, ExitAdjust, InflowTypeDetail, OutflowTypeDetail)]
    }
  }

  ## ASHomeless and EntryDate on first of month with no recent days_since_last_lh -------
  bad_records <- universe_w_ppl_flags_clean %>%
    fsubset(period != "Full") %>%
    fsubset(
      eecr & 
      InflowTypeDetail == "Homeless" & 
      EntryDate == as.Date(period) &
      EntryDate != session$userData$ReportStart &
      (days_since_last_lh > 14 | is.na(days_since_last_lh))
    )
  if(nrow(bad_records) > 0) {
    if(in_dev_mode & !isTRUE(getOption("shiny.testmode"))) {
      bad_ashomeless <- get_all_enrollments_for_debugging(
        bad_records,
        universe_w_ppl_flags_clean,
        multiple = TRUE
      )
      view(bad_ashomeless)
      browser()
    }
  }
  
  ## Re-Engaged/Return after Exit ---
  bad_records <- universe_w_ppl_flags_clean %>%
    fsubset(period != "Full", PersonalID, period, InflowTypeDetail, OutflowTypeDetail) %>%
    funique() %>%
    setorder(PersonalID, period) %>%
    fmutate(
      inflow_flag = grepl("Return|Re-engaged", InflowTypeDetail),
      prev_outflow = flag(OutflowTypeDetail, g=PersonalID), 
      prev_outflow_flag = !grepl("Exited|Inactive", prev_outflow) & !is.na(prev_outflow)
    ) %>%
    fgroup_by(PersonalID) %>%
    fsummarize(has_issue = any(inflow_flag & prev_outflow_flag, na.rm=TRUE)) %>%
    fungroup() %>%
    fsubset(has_issue)
  if(nrow(bad_records) > 0) {
    if(in_dev_mode & !isTRUE(getOption("shiny.testmode"))) {
      bad_return_after_nonexit <- get_all_enrollments_for_debugging(
        bad_records,
        universe_w_ppl_flags_clean,
        extra_cols = "days_since_lookback",
        multiple = TRUE
      ) %>%
        fselect(
          PersonalID, period, EnrollmentID, eecr, lecr, ProjectType, EntryDate, MoveInDateAdjust, ExitAdjust, lh_prior_livingsituation, lh_dates, InflowTypeDetail, OutflowTypeDetail, days_since_lookback
        )
      view(bad_return_after_nonexit)
      browser()
    }
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
  universe_w_ppl_flags_clean
}

# Inflow/Outflow Client-Level Data ---------------------------
## Summary (Annual) ----------------------------
# This also gets used by the Status chart
get_inflow_outflow_full <- reactive({
  logToConsole(session, "In get_inflow_outflow_full")

  full_data <- period_specific_data()[["Full"]]
  if(nrow(full_data) == 0) return(full_data)
  logToConsole(session, paste0("In get_inflow_outflow_full, num full_data records: ", nrow(full_data)))
  
  full_data <- full_data %>% fsubset(InflowTypeDetail != "Excluded")
  
  logToConsole(session, paste0("In get_inflow_outflow_full, num non-Excluded full_data records: ", nrow(full_data)))
  
  if(nrow(full_data) == 0) return(full_data)
  
  # AS 6/8/25: Do we want to remove *people* that are Continuous? Or just exclude from those Inflow/Outflow bars?
  # ditto for Inflow = Unknown
  # 637203 is an example of someone with Inflow = Unknown but has a regular Outflow
  data <- full_data %>%
    fselect(PersonalID,
            InflowTypeSummary,
            InflowTypeDetail,
            OutflowTypeSummary,
            OutflowTypeDetail
    ) %>%
    funique()
  
  data
})

## Monthly ---------------------------------
# combine individual month datasets
get_inflow_outflow_monthly <- reactive({
  logToConsole(session, paste0("In get_inflow_outflow_monthly"))
  months_data <- period_specific_data()[["Months"]] %>% fsubset(InflowTypeDetail !=" Excluded")
  
  logToConsole(session, paste0("In get_inflow_outflow_monthly, num months_data records: ", nrow(months_data)))
  
  if(nrow(months_data) == 0) return(months_data)
  
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

   rowbind(
    inflow_outflow_full_data %>%
      fcompute(
        Detail = InflowTypeDetail,
        Summary = fct_collapse(InflowTypeDetail, `Active at Start` = active_at_levels),
        InflowOutflow = factor("Inflow", levels = inflow_outflow_levels),
        PlotFillGroups = fct_collapse(InflowTypeDetail, Inflow = inflow_chart_detail_levels)
      ),

    inflow_outflow_full_data %>%
      fcompute(
        Detail = OutflowTypeDetail,
        Summary = fct_collapse(OutflowTypeDetail, `Active at End` = active_at_levels),
        InflowOutflow = factor("Outflow", levels = inflow_outflow_levels),
        PlotFillGroups = fct_collapse(OutflowTypeDetail, Outflow = outflow_chart_detail_levels)
      )
  ) %>% 
  fsubset(!Detail %in% c(inflow_statuses_to_exclude_from_chart, outflow_statuses_to_exclude_from_chart)) %>%
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
    fsubset(!Detail %in% c(inflow_statuses_to_exclude_from_chart, outflow_statuses_to_exclude_from_chart)) %>%
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
sys_inflow_outflow_monthly_single_status_chart_data <- function(monthly_status_data) {
  logToConsole(session, "In sys_inflow_outflow_monthly_single_status_chart_data")
  
  monthly_status_data %>%
    fgroup_by(month) %>%
    fsummarise(Count = GRPN()) %>%
    roworder(month) %>%
    join(
      data.table(month = unique(monthly_status_data$month)),
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
        Inflow = inflow_summary_chart_levels,
        Outflow = outflow_summary_chart_levels
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
    rownames = FALSE,
    selection = "none"
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
sys_monthly_single_status_ui_chart <- function(varname, status) {
  logToConsole(session, "In sys_monthly_single_status_ui_chart")

  monthly_status_data <- get_inflow_outflow_monthly() %>%
    fsubset(.[[varname]] == status)
  
  if(nrow(monthly_status_data) == 0) 
    return(
      ggplot() + 
        labs(title = no_data_msg) + 
        theme_minimal()
    )
  
  if(fndistinct(monthly_status_data$PersonalID) <= 10) 
    return(
      ggplot() + 
        labs(title = suppression_msg) + 
        theme_minimal()
    )
  
  plot_data <- sys_inflow_outflow_monthly_single_status_chart_data(monthly_status_data)

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
      axis.text.x = element_text(size = sys_axis_text_font, face = "bold"),
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
  
  monthly_counts <- rowbind(monthly_counts_detail, monthly_totals) %>%
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
    exportTestValues(sys_inflow_outflow_report = df)
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
        "System Inflow/Outflow Monthly – First-Time Homeless" = sys_monthly_single_status_ui_chart("InflowTypeDetail", "First-Time \nHomeless"),
        "System Inflow/Outflow Monthly – Inactive" = sys_monthly_single_status_ui_chart("OutflowTypeDetail", "Inactive")
      ),
      summary_font_size = 19
    )
  }
)
