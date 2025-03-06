raw_enrollments_dt <- reactive({
  unique(as.data.table(CurrentLivingSituation)[
    CurrentLivingSituation %in% homeless_livingsituation_incl_TH
  ][
    , .(EnrollmentID, InformationDate)
  ])[
    period_specific_data()[["Full"]][
      , .(PersonalID, EnrollmentID, EntryDate, ExitAdjust, ProjectType, lh_prior_livingsituation, LivingSituation)
    ],
    on = .(EnrollmentID)
  ][# Assign living situation category
    , LivingSituationCategory := as.factor(
      fifelse(LivingSituation %in% 400:499, "permanent",
              fifelse(LivingSituation %in% 100:199 | LivingSituation == 302, "homeless",
                      fifelse(LivingSituation %in% 300:399, "temporary",
                              fifelse(LivingSituation %in% 200:299, "institutional", "other")
                      ))))
  ]
})

enrollments_dt <- reactive({
  data <- raw_enrollments_dt()[
    (EnrollmentID == input$enrollmentIDFilter | input$enrollmentIDFilter == "") &
    (PersonalID == input$personalIDFilter | input$personalIDFilter == "")
  ]
})

enrl_month_categories <- function() {
  unique(rbindlist(period_specific_data()[-1])[
    , .(PersonalID, InflowTypeSummary, OutflowTypeSummary, month)
  ])
}

observeEvent(session$userData$valid_file(), {
  req(session$userData$valid_file() == 1)
  
  # Only run this observer once when the file is first loaded
  # or when the file changes
  isolate({
    if (input$personalIDFilter != "" || input$enrollmentIDFilter != "") {
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
    enrollment_ids <- sort(unique(data[PersonalID == input$personalIDFilter, EnrollmentID]))
  }
  
  updateSelectizeInput(
    session,
    "enrollmentIDFilter",
    choices = c("All" = "", enrollment_ids),
    selected = if (input$enrollmentIDFilter %in% c("", enrollment_ids)) input$enrollmentIDFilter else "",
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
    personal_ids <- sort(unique(data[EnrollmentID == input$enrollmentIDFilter, PersonalID]))
  }
  
  updateSelectizeInput(
    session,
    "personalIDFilter",
    choices = c("All" = "", personal_ids),
    selected = if (input$personalIDFilter %in% c("", personal_ids)) input$personalIDFilter else "",
    server = TRUE
  )
}, ignoreInit = TRUE)

output$timelinePlot <- renderPlotly({
  filtered_data <- enrollments_dt()
  
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
  
  # Add inflow data for coloring
  inflow_data <- enrl_month_categories()
   
  # Create a mapping of enrollment to initial inflow category
  # filtered_data[, entry_month := floor_date(EntryDate, "month")]
  filtered_data[, entry_month := floor_date(pmax(EntryDate, session$userData$ReportStart), "month"), by=PersonalID]
  
 
  # Merge inflow data with filtered data
  filtered_data <- inflow_data[
    filtered_data, 
    on = .(PersonalID, month = entry_month)
  ]
  filtered_data[is.na(InflowTypeSummary) & ExitAdjust < session$userData$ReportStart, InflowTypeSummary := "Lookback1"]

  filtered_data[, segment_start := pmax(as.Date(EntryDate), date_range_start)]
  filtered_data[, segment_end := pmin(as.Date(ExitAdjust), as.Date(date_range_end) %m-% months(1))]
  filtered_data <- filtered_data[!is.na(InformationDate)]
  filtered_data <- unique(filtered_data)
  
  # Create the base plot
  p <- ggplot() +
    # Add vertical month lines
    geom_vline(xintercept = as.numeric(month_ticks), linetype = "dotted", color = "gray80") +
    
    # Add enrollment lines
    geom_segment(data = unique(filtered_data[, .(EnrollmentID, PersonalID, EntryDate, ExitAdjust, segment_start, segment_end, Position, ProjectType, lh_prior_livingsituation, LivingSituationCategory, InflowTypeSummary)]),
                 aes(x = segment_start, 
                     xend = segment_end,
                     y = Position, 
                     yend = Position,
                     color = as.factor(InflowTypeSummary),
                     # group = EnrollmentID,
                     text = paste("PersonalID:", PersonalID, 
                                  "<br>EnrollmentID:", EnrollmentID,
                                  "<br>EntryDate:", format(EntryDate, "%Y-%m-%d"),
                                  "<br>ExitAdjust:", format(ExitAdjust, "%Y-%m-%d"),
                                  "<br>ProjectType:", ProjectType,
                                  "<br>LivingSituation:", LivingSituationCategory,
                                  "<br>lh_prior_livingsituation:", lh_prior_livingsituation,
                                  "<br>Inflow Type:", InflowTypeSummary)), # For click events
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
  p <- p +
    # Add living situation markers at exit date
    geom_point(data = unique(filtered_data[ExitAdjust < as.Date("2099-01-01"), .(EnrollmentID, PersonalID, ExitAdjust, Position, 
                                                                                 LivingSituation, LivingSituationCategory)]),
               aes(x = as.Date(ExitAdjust), 
                   y = Position,
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
  info <- enrl_month_categories()[
    PersonalID == input$personalIDFilter
  ]
  
  cat("Personal ID:", unique(info$PersonalID), "\n")
  cat("Monthly Status:\n")
  
  # Apply approach - more efficient for data.table
  info[, {
    month_formatted <- format(as.Date(month), "%b %Y")
    cat(month_formatted, ": (", InflowTypeSummary, ", ",  OutflowTypeSummary, "\n")
  }, by = seq_len(nrow(info))]
  
  # Return invisible to avoid extra output
  invisible()
})
