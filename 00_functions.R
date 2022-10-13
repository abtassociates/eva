

# Age ---------------------------------------------------------------------

age_years <- function(earlier, later)
{
  floor(decimal_date(later) - decimal_date(earlier))
  
}

served_between <- function(., start, end) {
  served <- .$EntryDate <= end  &
    (is.na(.$ExitDate) | .$ExitDate >= start)
  served
}

living_situation <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 1 ~ "Emergency shelter/ h/motel paid for by a third party/Host Home shelter",
    ReferenceNo == 2 ~ "Transitional housing",
    ReferenceNo == 3 ~ "Permanent housing (other than RRH) for formerly homeless persons",
    ReferenceNo == 4 ~ "Psychiatric hospital/ other psychiatric facility",
    ReferenceNo == 5 ~ "Substance abuse treatment facility or detox center",
    ReferenceNo == 6 ~ "Hospital or other residential non-psychiatric medical facility",
    ReferenceNo == 7 ~ "Jail/prison/juvenile detention",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 32 ~ "Host Home (non-crisis)",
    ReferenceNo == 13 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 36 ~ "Staying or living in a friend's room, apartment or house",
    ReferenceNo == 18 ~ "Safe Haven",
    ReferenceNo == 15 ~ "Foster care home of foster care group home",
    ReferenceNo == 12 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 25 ~ "Long-term care facility or nursing home",
    ReferenceNo == 22 ~ "Staying or living with family, permanent tenure",
    ReferenceNo == 35 ~ "Staying or living in a family member's room, apartment, or house",
    ReferenceNo == 16 ~ "Place not meant for habitation",
    ReferenceNo == 23 ~ "Staying or living with friends, permanent tenure",
    ReferenceNo == 29 ~ "Residential project or halfway house with no homeless criteria",
    ReferenceNo == 14 ~ "H/Motel paid for by household",
    ReferenceNo == 26 ~ "Moved from one HOPWA funded project to HOPWA PH",
    ReferenceNo == 27 ~ "Moved from HOPWA funded project to HOPWA TH",
    ReferenceNo == 28 ~ "Rental by client, with GPD TIP housing subsidy",
    ReferenceNo == 19 ~ "Rental by client, with VASH housing subsidy",
    ReferenceNo == 31 ~ "Rental by client, with RRH or equivalent subsidy",
    ReferenceNo == 33 ~ "Rental by client, with HCV voucher",
    ReferenceNo == 34 ~ "Rental by client in a public housing unit",
    ReferenceNo == 10 ~ "Rental by client, no ongoing housing subsidy",
    ReferenceNo == 20 ~ "Rental by client, with other ongoing housing subsidy",
    ReferenceNo == 21 ~ "Owned by client, with ongoing housing subsidy",
    ReferenceNo == 11 ~ "Owned by client, no ongoing housing subsidy",
    ReferenceNo == 30 ~ "No exit interview completed",
    ReferenceNo == 17 ~ "Other",
    ReferenceNo == 24 ~ "Deceased",
    ReferenceNo == 37 ~ "Worker unable to determine",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

project_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Emergency Shelter",
    ReferenceNo == 2 ~ "Transitional Housing",
    ReferenceNo == 3 ~ "Permanent Supportive Housing",
    ReferenceNo == 4 ~ "Street Outreach",
    ReferenceNo == 6 ~ "Services Only",
    ReferenceNo == 8 ~ "Safe Haven",
    ReferenceNo == 12 ~ "Prevention",
    ReferenceNo == 13 ~ "Rapid Rehousing",
    ReferenceNo == 14 ~ "Coordinated Entry"
  )
}

rel_to_hoh <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "HoH",
    ReferenceNo == 2 ~ "HoHs child",
    ReferenceNo == 3 ~ "HoHs partner/spouse",
    ReferenceNo == 4 ~ "HoHs other relation",
    ReferenceNo == 5 ~ "Non-relation member",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

replace_yes_no <- function(column_name) {
  if_else(column_name == "No" | is.na(column_name), 0, 1)
}

enhanced_yes_no_translator <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 0 ~ "No",
    ReferenceNo == 1 ~ "Yes",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

translate_HUD_yes_no <- function(column_name){
  case_when(
    column_name == 1 ~ "Yes", 
    column_name == 0 ~ "No",
    column_name %in% c(8, 9, 99) ~ "Unknown"
  )
}

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



