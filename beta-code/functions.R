# Chronic logic -----------------------------------------------------------

chronic_determination <- function(.data, aged_in = FALSE) { 
  
  needed_cols <- c("PersonalID", "EntryDate",
                   "AgeAtEntry", "DisablingCondition",
                   "DateToStreetESSH", "TimesHomelessPastThreeYears",
                   "MonthsHomelessPastThreeYears", "ExitAdjust", "ProjectType")
  
  chronicity_levels <- if(aged_in) {
    c("Chronic", "Aged In", "Nearly Chronic", "Not Chronic")}
  else {c("Chronic", "Nearly Chronic", "Not Chronic")}
  
  if (all((needed_cols) %in% colnames(.data))) {
    return(
      .data %>%
        mutate(DaysHomelessInProject = difftime(ymd(ExitAdjust),
                                                ymd(EntryDate),
                                                units = "days"),
               DaysHomelessBeforeEntry = difftime(ymd(EntryDate),
                                                  if_else(
                                                    is.na(ymd(DateToStreetESSH)),
                                                    ymd(EntryDate),
                                                    ymd(DateToStreetESSH)
                                                  ),
                                                  units = "days"),
               ChronicStatus =
                 case_when(
                   ((ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
                       !is.na(DateToStreetESSH)) |
                      (
                        MonthsHomelessPastThreeYears %in% c(112, 113) &
                          TimesHomelessPastThreeYears == 4 &
                          !is.na(MonthsHomelessPastThreeYears) &
                          !is.na(TimesHomelessPastThreeYears)
                      )
                   ) &
                     DisablingCondition == 1 &
                     !is.na(DisablingCondition) ~ "Chronic",
                   ProjectType %in% c(1, 8) &
                     ymd(DateToStreetESSH) + days(365) > ymd(EntryDate) &
                     !is.na(DateToStreetESSH) &
                     DaysHomelessBeforeEntry + DaysHomelessInProject >= 365 ~ "Aged In",
                   ((
                     ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
                       !is.na(DateToStreetESSH)
                   ) |
                     (
                       MonthsHomelessPastThreeYears %in% c(110:113) &
                         TimesHomelessPastThreeYears%in% c(3, 4) &
                         !is.na(MonthsHomelessPastThreeYears) &
                         !is.na(TimesHomelessPastThreeYears)
                     )
                   ) &
                     DisablingCondition == 1 &
                     !is.na(DisablingCondition) ~ "Nearly Chronic",
                   TRUE ~ "Not Chronic"),
               ChronicStatus = case_when(aged_in ~ ChronicStatus,
                                         TRUE ~ if_else(ChronicStatus == "Aged In",
                                                        "Chronic",
                                                        ChronicStatus)),
               ChronicStatus = factor(
                 ChronicStatus,
                 ordered = TRUE,
                 levels = chronicity_levels)))
  }
  
  else {
    stop(paste0(
      "\nYou need to include the column \"",
      needed_cols[needed_cols %in% colnames(.data) == FALSE],
      "\" to use the chronic_determination() function"
    ))
  }
}

# Import Helper -----------------------------------------------------------

parseDate <- function(datevar) {
  newDatevar <- parse_date_time(datevar,
                                orders = c("Ymd", "mdY"))
  return(newDatevar)
}

importFile <- function(csvFile, col_types = NULL, guess_max = 1000) {
  if (is.null(input$imported)) {return()}
  filename = glue::glue("{csvFile}.csv")
  data <- read_csv(unzip(zipfile = input$imported$datapath, files = filename)
                   ,col_types = col_types
  )
  file.remove(filename)
  return(data)
}


getDQReportDataList <- function(dqData, dqOverlaps) {
  select_list = c("Project Name" = "ProjectName",
                  "Issue" = "Issue",
                  "Personal ID" = "PersonalID",
                  "Household ID" = "HouseholdID",
                  "Entry Date"= "EntryDate",
                  "Organization Name" = "OrganizationName",
                  "Project ID" = "ProjectID")
  
  high_priority <- dqData %>% 
    filter(Type == "High Priority") %>% 
    select(all_of(select_list))
  
  errors <- dqData %>%
    filter(Type == "Error") %>% 
    select(all_of(select_list))
  
  warnings <- dqData %>%
    filter(Type == "Warning" & Issue != "Overlapping Project Stays") %>% 
    select(all_of(select_list))
  
  summary <- rbind(
    dqData %>% select(ProjectName, Type, Issue, PersonalID),
    dqOverlaps %>% select("ProjectName" = "ProjectName.x", Type, Issue, PersonalID)
  ) %>%
    group_by(ProjectName, Type, Issue) %>%
    summarise(Clients = n()) %>%
    select(Type, Clients, ProjectName, Issue) %>%
    arrange(Type, desc(Clients))
  
  guidance <- dqData %>%
    select(Type, Issue, Guidance) %>%
    unique() %>%
    mutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning"))) %>%
    arrange(Type)
  
  exportDetail <- data.frame(c("Export Start", "Export End", "Export Date"),
                             c(meta_HUDCSV_Export_Start, meta_HUDCSV_Export_End, meta_HUDCSV_Export_Date))
  colnames(exportDetail) = c("Export Field", "Value")
  
  exportDFList <- list(
    exportDetail = exportDetail,
    summary = summary,
    guidance = guidance,
    high_priority = high_priority,
    errors = errors,
    warnings = warnings,
    overlaps = dqOverlaps
  )
  
  names(exportDFList) = c(
    "Export Detail",
    "Summary",
    "Guidance",
    "High Priority",
    "Errors", 
    "Warnings", 
    "Overlaps"
  )
  
  exportDFList <- exportDFList[sapply(exportDFList, 
                                      function(x) dim(x)[1]) > 0]
  
  return(exportDFList)
}