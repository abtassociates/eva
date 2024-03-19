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
                   ProjectType %in% c(
                     es_nbn_project_type,
                     es_ee_project_type,
                     sh_project_type
                   ) &
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

