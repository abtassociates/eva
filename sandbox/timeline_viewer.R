raw_enrollments_dt <- reactive({
  # browser()
  upload_name <- ifelse(input$in_demo_mode, "DEMO", input$imported$name)
  
  session$userData$get_period_specific_enrollment_categories(
    session$userData$report_dates[["Full"]], 
    ifelse(input$in_demo_mode, "DEMO", input$imported$name),
    enrollments_filtered()
  ) %>%
    fselect(PersonalID, EnrollmentID, EntryDate, ExitAdjust, ProjectType, lh_prior_livingsituation, LivingSituation, MoveInDateAdjust) %>%
    join(
      session$userData$lh_nbn %>% fselect(EnrollmentID, DateProvided),
      on = "EnrollmentID",
      how = "left"
    ) %>%
    join(
      session$userData$lh_non_res %>% fselect(EnrollmentID, InformationDate),
      on = "EnrollmentID",
      how = "left"
    ) %>%
    fmutate(
      LivingSituationCategory = as.factor(
        fifelse(LivingSituation %in% perm_livingsituation, "permanent",
                fifelse(LivingSituation %in% homeless_livingsituation_incl_TH, "homeless",
                      fifelse(LivingSituation %in% temp_livingsituation, "temporary",
                              fifelse(LivingSituation %in% institutional_livingsituation, "institutional", "other")
                      )))
      )
    )
})

enrollments_dt <- reactive({
  data <- raw_enrollments_dt()[
    (EnrollmentID %in% input$enrollmentIDFilter | is.null(input$enrollmentIDFilter)) &
    (PersonalID %in% input$personalIDFilter | is.null(input$personalIDFilter))
  ]
})

enrl_month_categories <- function() {
  period_specific_data()[["Months"]][, .(
    PersonalID, 
    InflowTypeSummary, 
    OutflowTypeSummary, 
    month, 
    InflowTypeDetail, 
    OutflowTypeDetail, 
    EnrollmentID
  )] %>% funique()
}

observeEvent(session$userData$valid_file(), {
  req(FALSE) #making sure this doesn't run, since timeline_viewer is a helper
  req(session$userData$valid_file() == 1)
  
  # Only run this observer once when the file is first loaded
  # or when the file changes
  isolate({
    if (!is.null(input$personalIDFilter) || !is.null(input$enrollmentIDFilter)) {
      return()
    }
  })
  
  data <- raw_enrollments_dt()
  
  # Initial setup with all options
  enrollment_ids <- sort(unique(data$EnrollmentID))
  updateSelectizeInput(
    session,
    "enrollmentIDFilter",
    choices = c("All" = "", enrollment_ids),
    server = TRUE
  )
  
  personal_ids <- sort(unique(data$PersonalID))
  updateSelectizeInput(
    session,
    "personalIDFilter",
    choices = c("All" = "", personal_ids),
    server = TRUE
  )
}, ignoreNULL = FALSE, ignoreInit = FALSE)

# Update EnrollmentID based on PersonalID selection
observeEvent(input$personalIDFilter, {
  req(session$userData$valid_file() == 1)
  
  data <- raw_enrollments_dt()
  
  # If "All" is selected, show all EnrollmentIDs
  if (input$personalIDFilter == "") {
    enrollment_ids <- sort(unique(data$EnrollmentID))
  } else {
    # Filter EnrollmentIDs for the selected PersonalID
    enrollment_ids <- sort(unique(data[PersonalID %in% input$personalIDFilter, EnrollmentID]))
  }

  updateSelectizeInput(
    session,
    "enrollmentIDFilter",
    choices = c("All" = "", enrollment_ids),
    selected = if (isTRUE(is.null(input$enrollmentIDFilter) | input$enrollmentIDFilter %in% enrollment_ids)) input$enrollmentIDFilter else NULL,
    server = TRUE
  )
}, ignoreInit = TRUE)

# Update PersonalID based on EnrollmentID selection
observeEvent(input$enrollmentIDFilter, {
  req(session$userData$valid_file() == 1)
  
  data <- raw_enrollments_dt()
  
  # If "All" is selected, show all PersonalIDs
  if (input$enrollmentIDFilter == "") {
    personal_ids <- sort(unique(data$PersonalID))
  } else {
    # Filter PersonalIDs for the selected EnrollmentID
    personal_ids <- sort(unique(data[EnrollmentID %in% input$enrollmentIDFilter, PersonalID]))
  }
  
  updateSelectizeInput(
    session,
    "personalIDFilter",
    choices = c("All" = "", personal_ids),
    selected = if (isTRUE(is.null(input$personalIDFilter) | input$personalIDFilter %in% personal_ids)) input$personalIDFilter else NULL,
    server = TRUE
  )
}, ignoreInit = TRUE)

output$timelinePlot <- renderPlotly({
  filtered_data <- enrollments_dt()
  # browser()
  # Calculate the full date range (12 months + 4 months prior + 1 month after)
  date_range_start <- session$userData$ReportStart %m-% months(4)
  date_range_end <- session$userData$ReportEnd %m+% months(1)
  
  # Create month tick marks
  month_ticks <- seq.Date(from = date_range_start, to = date_range_end, by = "month")
  month_labels <- format(month_ticks, "%b %Y")
  
  updateSliderInput(
    session,
    "monthFilter",
    label = month_labels
  )

  # Get unique personal IDs for y-axis positioning
  person_ids <- unique(filtered_data[, PersonalID])
  person_positions <- data.table(PersonalID = person_ids, 
                                 Position = 1:length(person_ids))
  filtered_data <- merge(filtered_data, person_positions, by = "PersonalID")
  
  # Create a mapping of enrollment to initial inflow category
  # filtered_data[, entry_month := floor_date(EntryDate, "month")]
  filtered_data[, entry_month := floor_date(pmax(EntryDate, session$userData$ReportStart), "month"), by=PersonalID]
  
  # Merge inflow data with filtered data
  filtered_data <- enrl_month_categories()[
    filtered_data, 
    on = .(EnrollmentID)
  ][, month := format(month, "%b %y")]

  filtered_data[is.na(InflowTypeSummary) & ExitAdjust < session$userData$ReportStart, InflowTypeSummary := "Lookback"]

  filtered_data[, segment_start := pmax(as.Date(EntryDate), date_range_start)]
  filtered_data[, segment_end := pmin(as.Date(ExitAdjust), as.Date(date_range_end) %m-% months(1))]
  # filtered_data <- filtered_data[!is.na(InformationDate)]
  filtered_data <- unique(filtered_data)
  
  # Jitter enrollment lines away from each other
  jittered_data <- unique(filtered_data[, .(
    EnrollmentID, 
    PersonalID, 
    EntryDate, 
    ExitAdjust, 
    segment_start, 
    segment_end, 
    Position, 
    ProjectType, 
    lh_prior_livingsituation, 
    LivingSituationCategory, 
    InflowTypeDetail,
    InflowTypeSummary,
    OutflowTypeDetail
  )])
  jittered_data[, Position_jittered := Position + (seq_len(.N) - 1) * 0.05, by = Position]
  
  export_random_100(filtered_data)
  
  p <- ggplot() +
    # Add vertical month lines
    geom_vline(xintercept = as.numeric(month_ticks), linetype = "dotted", color = "gray80") +
    
    # Add enrollment lines
    geom_segment(data = jittered_data,
                 aes(x = segment_start, 
                     xend = segment_end,
                     y = Position_jittered, 
                     yend = Position_jittered,
                     color = as.factor(InflowTypeSummary),
                     # group = EnrollmentID,
                     text = paste("PersonalID:", PersonalID, 
                                  "<br>EnrollmentID:", EnrollmentID,
                                  "<br>EntryDate:", format(EntryDate, "%Y-%m-%d"),
                                  "<br>ExitAdjust:", format(ExitAdjust, "%Y-%m-%d"),
                                  "<br>ProjectType:", ProjectType,
                                  "<br>LivingSituation:", LivingSituationCategory,
                                  "<br>lh_prior_livingsituation:", lh_prior_livingsituation,
                                  "<br>Inflow Detail:", InflowTypeDetail, # For click events
                                  "<br>Outflow Detail:", OutflowTypeDetail)), # For click events
                 linewidth = 1)
  
  if(nrow(filtered_data[!is.na(InformationDate)]) > 0) {
    # Add information date markers
    p <- p + 
      geom_point(data = unique(filtered_data[, .(EnrollmentID, Position, InformationDate)]),
               aes(x = as.Date(InformationDate), 
                   y = Position),
               shape = 3,
               color = "orange",
               size = 2)
  }
  
  if(nrow(filtered_data[!is.na(DateProvided)]) > 0) {
    # Add information date markers
    p <- p + 
      geom_point(data = unique(filtered_data[, .(EnrollmentID, Position, DateProvided)]),
                 aes(x = as.Date(DateProvided), 
                     y = Position),
                 shape = 3,
                 color = "pink",
                 size = 2)
  }
  p <- p +
    # Add living situation markers at exit date
    geom_point(data = unique(jittered_data[ExitAdjust < as.Date("2099-01-01"), .(EnrollmentID, PersonalID, ExitAdjust, Position, 
                                                                                 LivingSituationCategory, Position_jittered)]),
               aes(x = as.Date(ExitAdjust), 
                   y = Position_jittered,
                   shape = LivingSituationCategory),
               size = 2) +
    # Customize the plot
    scale_x_date(limits = c(date_range_start, date_range_end),
                 breaks = as.Date(month_ticks),
                 labels = month_labels) +
    scale_y_continuous(breaks = person_positions$Position,
                       labels = NULL) +
    scale_color_manual(values = c("Active at Start" = "blue", "Inflow" = "green", "Unknown" = "gray")) +
    scale_shape_manual(values = c("permanent" = 16, "homeless" = 17, "temporary" = 15, 
                                  "institutional" = 18, "other" = 8)) +
    labs(x = "Date", 
         y = "Personal ID",
         color = "Inflow Summary",
         shape = "Living Situation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank())
  
  # Convert to plotly for interactivity
  ggplotly(p, tooltip = "text", source="timelines") %>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      margin = list(l = 50, r = 50, b = 100, t = 50),
      legend = list(orientation = "h", y = -0.2)
    )
})
# 
# # Store clicked enrollment information
# selectedEnrollment <- reactiveVal(NULL)
# 
# # Handle plot click events
output$personDetails <- renderPrint({
  req(input$personalIDFilter)
  
  info <- enrl_month_categories()[
    PersonalID == input$personalIDFilter
  ]
  
  cat("Personal ID:", unique(info$PersonalID), "\n")
  cat("Monthly Status:\n")
  
  # Apply approach - more efficient for data.table
  info[, {
    month_formatted <- format(as.Date(month), "%b %Y")
    cat(month_formatted, ": (", as.character(InflowTypeDetail), ", ",  as.character(OutflowTypeDetail), ")\n")
  }, by = seq_len(nrow(info))]
  
  # Return invisible to avoid extra output
  invisible()
})

export_random_100 <- function(filtered_data) {
  set.seed(100)
  people_sample <- sample(filtered_data[, unique(PersonalID)], 100)
  
  sampled_data <- filtered_data %>%
    fselect(
      EnrollmentID, 
      PersonalID, 
      EntryDate, 
      MoveInDateAdjust,
      ExitAdjust,
      ProjectType, 
      InformationDate,
      DateProvided,
      lh_prior_livingsituation,
      month,
      InflowTypeDetail,
      InflowTypeSummary,
      OutflowTypeDetail
    ) %>%
    fsubset(PersonalID %in% people_sample)

  write_xlsx(sampled_data, here("/media/sdrive/projects/CE_DatA_Toolkit/Data Sets/sample-timeline-data2.xlsx"))
}