# when user changes chart tabs
# hide demographic filters for Composition chart
# hide other stuff if valid file is not uploaded
# move chart download button to be inline with subtabs
observeEvent(input$syso_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  toggleClass(
    id = "syso_inflowoutflow_filters",
    condition = input$syso_tabbox == "System Demographics",
    class = "filter-hidden"
  )
}, ignoreNULL = TRUE, ignoreInit = TRUE) #confirm if need to have ignore init?


observeEvent(input$sys_inflow_outflow_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_inflow_outflow_subtabs,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$sys_status_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_status_subtabs,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$sys_comp_subtabs, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syso_tabbox, " - ", input$sys_comp_subtabs,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$methodology_type, {
  
  updatePickerInput(
    session, 
    "syso_race_ethnicity", 
    choices = syso_race_ethnicity_cats(input$methodology_type)
  )

  # update System Composition Grouped Races/Ethnicities label
  grouped_re_lbl_new <- ifelse(input$methodology_type == 1, "Grouped", "Hispanic-Focused")
  shinyjs::runjs(
    glue("
      $('#system_composition_selections input[value=\"Grouped Races/Ethnicities\"] + span').text('{grouped_re_lbl_new} Races/Ethnicities');
    ")
  )
},
ignoreInit = TRUE)

observeEvent(input$syso_level_of_detail, {
  updatePickerInput(session, "syso_spec_pops",
                    # label = "Special Populations",
                    choices = syso_spec_pops_people)
})

#### DISPLAY FILTER SELECTIONS ###
syso_detailBox <- reactive({
  # remove group names from race/ethnicity filter
  # so we can use getNameByValue() to grab the selected option label
  detail_line <- function(detail_label, val_list, inputVal) {
    return(
      HTML(glue(
        "<b>{detail_label}:</b> {getNameByValue(val_list, inputVal)} <br>"
      ))
    )
  }
  
  selected_race <- getNameByValue(
    syso_race_ethnicity_cats(input$methodology_type), 
    input$syso_race_ethnicity
  )
  
  race_ethnicity_line <- HTML(glue(
    "<b>Race/Ethnicity:</b> {selected_race} <br>"
  ))
  
  list(
    br(),
    strong("Date Range: "),
    
    format(session$userData$ReportStart, "%m-%d-%Y"), " to ", format(session$userData$ReportEnd, "%m-%d-%Y"), br(),
    
    if (input$syso_project_type != "All")
      chart_selection_detail_line("Project Type Group", syso_project_types, input$syso_project_type),
    
    #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
    HTML(glue(
      "<b>Methodology Type:</b> {str_sub(getNameByValue(syso_methodology_types, input$methodology_type), start = 1, end = 8)} <br>"
    )),
    
    if (length(input$syso_age) != length(syso_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syso_age, collapse = ', ')} <br>"
      )),
    
    if (selected_race != "All Races/Ethnicities")
      race_ethnicity_line,
    
    if(getNameByValue(syso_spec_pops_people, input$syso_spec_pops) != "All Statuses")
      HTML(glue(
        "<b>Veteran Status:</b> {paste(getNameByValue(syso_spec_pops_people, input$syso_spec_pops), '(Adult Only)')} <br>"
      ))
    
  )
})

toggle_sys_components <- function(cond, init=FALSE) {
  # 1. toggles the filters (disabled for Composition)
  # 2. toggles subtabs and download button based if valid file has been uploaded
  # 3. moves download button to be in line with subtabs
  tabs <- c(
    "System Flow" = "inflow_outflow",
    "Client System Status" = "status",
    "System Demographics" = "comp"
  )
  
  for (tab in tabs) {
    shinyjs::toggle(glue('sys_{tab}_subtabs'), condition = cond)
    shinyjs::toggle(selector = glue('#sys_{tab}_subtabs + div.tab-content'), condition = cond)
    shinyjs::toggle(glue('sys_{tab}_download_btn'), condition = cond)
    shinyjs::toggle(glue('sys_{tab}_download_btn_ppt'), condition = cond)
    
    # move download button to subtab row and only show if there's data
    if(init) {
      shinyjs::runjs(
        glue("
            document.getElementById('sys_{tab}_subtabs')
              .insertAdjacentHTML('beforeEnd', '<li class=\"syso_download_tab\" id=\"sys_{tab}_download_tab\"></li>');
            $('#sys_{tab}_download_btn').appendTo('#sys_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
            $('#sys_{tab}_download_btn_ppt').appendTo('#sys_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
          ")
      )
    }
  }
  
  shinyjs::toggle('client_level_download_btn', condition = cond)
  if(init) {
    shinyjs::runjs("
      document.getElementById('syso_tabbox')
        .insertAdjacentHTML('beforeEnd', '<li class=\"client_level_download_tab\" id=\"client_level_download_tab\"></li>');
      $('#client_level_download_btn').appendTo('#client_level_download_tab')
        .toggle('{cond}' == 'TRUE');
    ")
  }
  
}
toggle_sys_components(FALSE, init=TRUE) # initially hide them

sys_export_summary_initial_df <- function() {
  
  logMetadata(session, paste0("Downloaded System Overview Tabular Data: ", input$syso_tabbox,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  return(data.frame(
    Chart = c(
      "Start Date",
      "End Date",
      "Methodology Type",
      "Household Type",
      "Level of Detail",
      "Project Type Group"
    ),
    Value = c(
      strftime(session$userData$ReportStart, "%m/%d/%y"),
      strftime(session$userData$ReportEnd, "%m/%d/%y"),
      getNameByValue(syso_methodology_types, input$methodology_type),
      getNameByValue(syso_hh_types, input$syso_hh_type),
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail),
      getNameByValue(syso_project_types, input$syso_project_type)
    )
  ))
}

sys_export_filter_selections <- function() {
  return(tibble(
    Chart = c(
      "Age",
      "Veteran Status",
      "Race/Ethnicity"
    ),
    Value = c(
      if(identical(syso_age_cats, input$syso_age)) {"All Ages"} else {paste(input$syso_age, collapse=", ")},
      getNameByValue(syso_spec_pops_people, input$syso_spec_pops),
      getNameByValue(syso_race_ethnicity_cats(input$methodology_type), input$syso_race_ethnicity)
    )
  ))
}

#### FILTERS ###

# Population reactives ----------------------------------------------------

# Set race/ethnicity filter options based on methodology type selection
# Set special populations options based on level of detail selection
syso_race_ethnicity_cats <- function(methodology = 1){
  ifelse(
    methodology == 1,
    list(syso_race_ethnicity_method1),
    list(syso_race_ethnicity_method2)
  )[[1]]
}

# PowerPoint Export -------------------------------------------------------
sys_overview_ppt_export <- function(file,
                                    title_slide_title,
                                    summary_items,
                                    plot_slide_title,
                                    plot1,
                                    plot2 = NULL,
                                    summary_font_size) {
  
  logMetadata(session, paste0("Downloaded System Overview Powerpoint: ", title_slide_title,
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  #NEED TO UPDATE - if want to get more granular, need to detect with title slide
  
  report_period <- paste0("Report Period: ", 
                          format(session$userData$ReportStart, "%m/%d/%Y"),
                          " - ",
                          format(session$userData$ReportEnd, "%m/%d/%Y")
  )
  loc_title <- ph_location_type(type = "title")
  loc_footer <- ph_location_type(type = "ftr")
  loc_dt <- ph_location_type(type = "dt")
  loc_slidenum <- ph_location_type(type = "sldNum")
  loc_body <- ph_location_type(type = "body")
  loc_subtitle <- ph_location_type(type = "subTitle")
  loc_ctrtitle <- ph_location_type(type = "ctrTitle")
  
  fp_normal <- fp_text(font.size = summary_font_size)
  fp_title <- fp_text(font.size = ppt_chart_title_font_size)
  fp_bold <- update(fp_normal, bold = TRUE)
  fp_red <- update(fp_normal, color = "red")
  
  ppt <- read_pptx(here("system_pptx_template.pptx"))
  
  add_footer <- function(.ppt) {
    return(
      .ppt %>%
        ph_with(value = paste0("CoC Code: ", session$userData$Export$SourceID), location = loc_footer) %>%
        ph_with(value = report_period, location = loc_dt) %>%
        ph_with(
          value = paste0(
            "Export Generated: ",
            format(Sys.Date()),
            "\n",
            "https://hmis.abtsites.com/eva/"
          ),
          location = loc_slidenum
        )
    )
  }
  
  # title Slide
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme") %>%
    ph_with(value = title_slide_title, location = loc_ctrtitle) %>%
    ph_with(value = "Eva Image Export", location = loc_subtitle) %>%
    add_footer()
  
  # Summary
  s_items <- do.call(block_list, lapply(1:nrow(summary_items), function(i) {
    fpar(
      ftext(paste0(summary_items$Chart[i], ": ", summary_items$Value[i]), fp_normal)
    )
  }))
  
  ppt <- add_slide(ppt, layout = "Title and Content") %>%
    ph_with(value = "Summary", location = loc_title) %>%
    ph_with(
      value = s_items,
      level_list = c(rep(1L, length(s_items))),
      location = loc_body
    ) %>% 
    add_footer()
  
  # Chart
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = fpar(ftext(plot_slide_title, fp_title)), location = loc_title) %>%
    ph_with(value = plot1, location = loc_body) %>%
    add_footer()
  
  if(!is.null(plot2)) {
    ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = fpar(ftext(plot_slide_title, fp_title)), location = loc_title) %>%
      ph_with(value = plot2, location = loc_body) %>%
      add_footer()
  }
  
  # Export the PowerPoint
  return(print(ppt, target = file))
}

suppression_msg <- "The chart cannot be displayed because there are fewer than 11 clients."
no_data_msg <- "No data to show."

# Display Filter Selection in Detail Box ----------------------------------

chart_selection_detail_line <- function(detail_label, val_list, inputVal) {
  return(
    HTML(glue(
      "<strong>{detail_label}:</strong> {getNameByValue(val_list, inputVal)} <br>"
    ))
  )
}


# Total Count Above Chart -------------------------------------------------

sys_total_count_display <- function(total_count) {
  return(paste0(
    str_wrap(
      paste0(
        "Total ",
        case_when(
          input$syso_level_of_detail == "All" ~ "People",
          input$syso_level_of_detail == "HoHsOnly" ~ "Heads of Household",
          TRUE ~
            getNameByValue(syso_level_of_detail, input$syso_level_of_detail)
        ),
        if_else(
          input$syso_hh_type == "All",
          "",
          paste0(" in ",
                 str_remove(getNameByValue(syso_hh_types, input$syso_hh_type), "- "),
                 " Households")
        ),       ": ",
        scales::comma(total_count)
      ),
      width = 40
    ),
    "\n")
  )
}

get_adj_font_size <- function(font_size, isExport) {
  return(
    font_size*ifelse(isExport, sys_chart_export_font_reduction, 1)
  )
}

observe({
  windowSize(input$dimension)
})

# if user changes filters, update the reactive vals
# which get used for the various System Overview charts
observeEvent({
  input$syso_hh_type
  input$syso_level_of_detail
  input$syso_project_type
  input$methodology_type
  input$syso_age
  input$syso_spec_pops
  input$syso_race_ethnicity
}, {
  # hide download buttons if < 11 records
  # All Served is handled in system_composition_server.R
  # for that chart, we also hide if all *cells* are < 11
  shinyjs::toggle(
    "sys_inflow_outflow_download_btn sys_inflow_outflow_download_btn_ppt", 
    condition = nrow(get_inflow_outflow_full()) > 10
  )
  shinyjs::toggle(
    "sys_status_download_btn sys_status_download_btn_ppt", 
    condition = sum(get_sankey_data()$freq) > 10
  )
}, ignoreInit = TRUE)

source("client_level_export_server.R", local=TRUE)

# Get period report_dates --------------------------------------------
get_months_in_report_period <- function() {
  seq.Date(from = session$userData$ReportStart, to = session$userData$ReportEnd, by = "months")
} 
get_report_dates <- function() {
  months_in_report_period <- get_months_in_report_period()
  c(
    list("Full" = c(session$userData$ReportStart, session$userData$ReportEnd)),
    setNames(
      lapply(months_in_report_period, function(d) {
        c(d, ceiling_date(d, "month") - days(1))
      }),
      months_in_report_period
    )
  )
}

# Cache management for period-specific universe_ppl_flag datasets --------------
# Check cache size - this keeps the cache manageable
check_cache_size <- function(cache, max_size_mb = 100) {
  cache_size <- utils::object.size(cache) / 1024^2  # Convert to MB
  if (cache_size > max_size_mb) {
    rm(list = ls(cache), envir = cache)
    gc()
    return(TRUE)
  }
  FALSE
}

# Get period-specific universe_ppl_flag datasets ---------------------------
period_specific_data <- reactive({
  if(is.null(session$userData$period_cache)) {
    session$userData$period_cache <- new.env()
  }
  cache <- session$userData$period_cache
  
  check_cache_size(cache)
  
  cache_key <- digest::digest(list(
    if(isTruthy(input$in_demo_mode)) "demo" else input$imported$name,
    
    # Client-level filters
    input$syso_age,
    input$syso_race_ethnicity,
    input$syso_spec_pops,
    
    # Enrollment-level filters
    input$syso_hh_type,
    input$syso_level_of_detail,
    input$syso_project_type
  ))
  
  cached_result <- cache[[cache_key]]
  if (!is.null(cached_result)) {
    return(cached_result)
  }
  
  upload_name <- ifelse(input$in_demo_mode, "DEMO", input$imported$name)
  
  results <- lapply(
    session$userData$report_dates,
    function(period) {
      # custom_rprof({
      enrollment_categories <- session$userData$get_period_specific_enrollment_categories(period, upload_name)
        
      all_filtered <- universe_filtered(enrollment_categories)
      all_filtered_w_lh <- add_lh_info(all_filtered, period)
      universe_w_enrl_flags <- universe_enrl_flags(all_filtered_w_lh, period)
      universe_w_ppl_flags <- universe_ppl_flags(universe_w_enrl_flags, period)
      
      # Add month flag for month-periods
      if(!identical(period, session$userData$report_dates[["Full"]])) {
        universe_w_ppl_flags[, month := as.Date(period[1])]
      }
      # }, "system_overview_server.R")
      universe_w_ppl_flags
    }
  )
  cache[[cache_key]] <- results
  session$userData$period_cache <- cache
  results # %>% 
    # Commenting out for testing purposes, so can view fuller intermediate datasets
    # but this will remove all the lookbacks, which are no longer needed at this point
    # fsubset(eecr | lecr)
})

# Client-level flags, filtered ----------------------------------------------------
client_categories_filtered <- reactive({
  req(nrow(session$userData$client_categories) > 0)
  
  session$userData$client_categories[
    AgeCategory %in% input$syso_age &
      input$syso_race_ethnicity == "All" &
      (
        input$syso_spec_pops == "None" |
          (input$syso_spec_pops == "Veteran" &
             VeteranStatus == 1 & !(AgeCategory %in% c("0 to 12", "13 to 17"))) |
          (input$syso_spec_pops == "NonVeteran" &
             VeteranStatus == 0 & !(AgeCategory %in% c("0 to 12", "13 to 17")))
      )
  ]
})

# Period-specific, user-filtered, enrollment-level universe applied ------------------
universe_filtered <- function(enrollment_categories) {
  
enrollment_categories %>%
    join( # Inner Join with client categories
      # This is necessary for bringing in Veteran Status, but will also make the rest faster
      client_categories_filtered(),
      # client_categories_filt, # this is used for mirai
      on = "PersonalID",
      how = "inner"
    ) %>%
    fsubset(
      # Household type filter
      (input$syso_hh_type == "All" |
         (input$syso_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
         (input$syso_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
         (input$syso_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
         (input$syso_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
         input$syso_hh_type == HouseholdType
      ) &
        # Level of detail filter
        (input$syso_level_of_detail == "All" |
           (input$syso_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (input$syso_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        (input$syso_project_type == "All" |
           input$syso_project_type == eecr_project_type
        )
    )
}


## LH info for non-res enrollments -----------
# continuing the work of the base lh_non_res dataset from 07_system_overview.R 
# we now make it period-specific, and collapse it down to the enrollment-level
# so this contains enrollments with LH CLS and an indicator as to 
# whether InformationDate is within to 60 or 90 days 
# (depending on project type, but only limited to Non-Res Project Types) 
# from the period start/end
# we then merge this with enrollment_categories to fully replace the homeless_cls_finder function
# this avoids having to re-filter and do the check for each enrollment
lh_non_res_period <- function(startDate, endDate) {
  lh_non_res <- session$userData$lh_non_res %>%
    # Initial filtering
    fsubset(EntryDate <= endDate & ExitAdjust >= (startDate %m-% years(2))) %>%
    
    # Calculate time windows
    ftransform(
      start_window = startDate - fifelse(ProjectType == ce_project_type, 90, 60),
      end_window = endDate - fifelse(ProjectType == ce_project_type, 90, 60)
    ) %>%
    ftransform(
      lh_cls_in_start_window = between(InformationDate, start_window, startDate + 15),
      lh_cls_in_end_window = between(InformationDate, end_window, endDate + 15),
      entry_in_start_window = between(EntryDate, start_window, startDate + 15),
      entry_in_end_window = between(EntryDate, end_window, endDate),
      lh_cls_during_period = between(InformationDate, start_window, endDate + 15)
    )  %>%
    fselect(
      EnrollmentID, ProjectType, lh_prior_livingsituation,
      lh_cls_in_start_window,
      lh_cls_in_end_window,
      entry_in_start_window,
      entry_in_end_window,
      lh_cls_during_period
    ) %>%
    fsubset(
      lh_cls_in_start_window |
        lh_cls_in_end_window |
        entry_in_start_window |
        entry_in_end_window |
        lh_cls_during_period
    )
  
  if(nrow(lh_non_res) == 0 ) return(lh_non_res)
  
  lh_non_res %>%
    # Group by EnrollmentID and calculate window flags
    fgroup_by(EnrollmentID) %>%
    fmutate(
      lh_cls_in_start_window = anyv(lh_cls_in_start_window, TRUE),
      lh_cls_in_end_window = anyv(lh_cls_in_end_window, TRUE),
      entry_in_start_window = entry_in_start_window,
      entry_in_end_window = entry_in_end_window,
      lh_cls_during_period = anyv(lh_cls_during_period, TRUE)
    ) %>%
    fungroup()
}

## LH info for NbN enrollments--------------
lh_nbn_period <- function(startDate, endDate) {
  lh_nbn <- session$userData$lh_nbn %>%
    # Initial filtering
    fsubset(EntryDate <= endDate & ExitAdjust >= (startDate %m-% years(2))) %>%
    ftransform(
      nbn_in_start_window = between(DateProvided, startDate - 15, startDate + 15),
      nbn_in_end_window = between(DateProvided, endDate - 15, endDate + 15),
      entry_in_start_window = between(EntryDate, startDate - 15, startDate + 15),
      entry_in_end_window = between(EntryDate, endDate - 15, endDate),
      nbn_during_period = between(DateProvided, startDate - 15, endDate + 15)
    ) %>%
    fselect(
      EnrollmentID, ProjectType, 
      nbn_in_start_window,
      nbn_in_end_window,
      entry_in_start_window,
      entry_in_end_window,
      nbn_during_period
    ) %>%
    fsubset(
      nbn_in_start_window |
        nbn_in_end_window |
        entry_in_start_window |
        entry_in_end_window |
        nbn_during_period
    )
  
  if(nrow(lh_nbn) == 0) return(lh_nbn)
  
  lh_nbn %>%
    fgroup_by(EnrollmentID) %>%
    fmutate(
      nbn_in_start_window = anyv(nbn_in_start_window, TRUE),
      nbn_in_end_window = anyv(nbn_in_end_window, TRUE),
      entry_in_start_window = entry_in_start_window,
      entry_in_end_window = entry_in_end_window,
      nbn_during_period = anyv(nbn_during_period, TRUE)
    ) %>%
    fungroup()
}

## LH info for Other enrollments --------------
lh_other_period <- function(all_filtered, startDate, endDate) {
  all_filtered %>%
    fsubset(
      EntryDate <= endDate & ExitAdjust >= (startDate %m-% years(2)) &
        (
          ProjectType %in% lh_project_types_nonbn | 
          (ProjectType %in% ph_project_types & (is.na(MoveInDateAdjust) | MoveInDateAdjust >= startDate))
        )
    ) %>%
    ftransform(
      entry_in_start_window = between(EntryDate, startDate, startDate + 15)
    ) %>%
    fselect(
      EnrollmentID, ProjectType, MoveInDateAdjust,
      straddles_start, straddles_end,
      entry_in_start_window,
      days_since_lookback,
      days_to_lookahead
    )
}

# Combine lh_infos and add to filtered universe dataset-------------------
add_lh_info <- function(all_filtered, period) {
  startDate <- period[1]
  endDate <- period[2]
  
  lh_info <- unique(
    # Capture LH info for both non-residential projects AND ES NbN projects
    rbindlist(
      list(
        lh_non_res_period(startDate, endDate),
        lh_nbn_period(startDate, endDate),
        lh_other_period(all_filtered, startDate, endDate)
      ),
      fill = TRUE
    )[, 
      .(
        EnrollmentID,
        
        was_lh_at_start = (
          # Non-Res and LH CLS in 60/90-day window OR 
          # Entry in 60/90 day window and lh_prior_livingsituation
          (ProjectType %in% non_res_project_types & (
            lh_cls_in_start_window | (entry_in_start_window & lh_prior_livingsituation)
          )) |
          # ES NbN and Bed Night in 15-day window
          # we don't need lh_prior_livingsituation here 
          # because ES NbN enrollment implies homelessness
          (ProjectType == es_nbn_project_type & (
            nbn_in_start_window | entry_in_start_window
          )) | 
          # All Other projects (lh_project_types 0,2,8 and ph_project_types 3,9,10,13)
          # must either straddle or otherwise be close to (i.e. 14 days from) 
          # start so we can make claims about status at start
          # and must be within 14 days of previous enrollment, otherwise it would be an exit
          ((straddles_start | (entry_in_start_window & days_since_lookback <= 14)) & (
            ProjectType %in% lh_project_types_nonbn | 
            (ProjectType %in% ph_project_types & (is.na(MoveInDateAdjust) | MoveInDateAdjust >= startDate))
          ))
        ),
        
        was_lh_during_period =
          (ProjectType == es_nbn_project_type & nbn_during_period) |
          (ProjectType %in% non_res_project_types & lh_cls_during_period),
        
        was_lh_at_end = (
          (ProjectType %in% non_res_project_types & (
            lh_cls_in_start_window | (entry_in_end_window & lh_prior_livingsituation)
          )) |
          (ProjectType == es_nbn_project_type & (
            nbn_in_end_window | entry_in_end_window
          )) | 
          ((straddles_end | days_to_lookahead <= 14) & (
            ProjectType %in% lh_project_types_nonbn | 
            (ProjectType %in% ph_project_types & (is.na(MoveInDateAdjust) | MoveInDateAdjust >= endDate))
          ))
        )
      )
    ]
  )
  join(all_filtered, lh_info, on = "EnrollmentID", how = "left")
}

