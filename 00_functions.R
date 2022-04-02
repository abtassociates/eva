# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.


# Age Function ------------------------------------------------------------

age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(
    format(lt[, 1], format = "%m-%d") != "02-29",
    as.Date(paste(
      format(lt[, 2], format = "%Y"), "-",
      format(lt[, 1], format = "%m-%d"),
      sep = ""
    )),
    ifelse(
      as.numeric(format(later, format = "%Y")) %%
        400 == 0 |
        as.numeric(format(later,
                          format =
                            "%Y")) %%
        100 != 0 &
        as.numeric(format(later, format = "%Y")) %%
        4 == 0,
      as.Date(paste(
        format(lt[, 2], format = "%Y"),
        "-",
        format(lt[, 1], format =
                 "%m-%d"),
        sep = ""
      )),
      as.Date(paste(
        format(lt[, 2], format = "%Y"),
        "-",
        "02-28",
        sep = ""
      ))
    )
  )
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age
}

# Client Entry Exits Between Date Range Functions -------------------------------------
# 
# served_between <- function(table, start, end){
#   served <- ymd(table$EntryDate) <= mdy(end) &
#     (is.na(table$ExitDate) | ymd(table$ExitDate) >= mdy(start))
#   served
# }

# should move to this but will require a LOT of edits!

# served_between <- function(., start, end) {
#   . %>% filter(ymd(EntryDate) <= mdy(end) &
#                  (is.na(ExitDate) | ymd(ExitDate) >= mdy(start)))
# }
# 
# entered_between <- function(table, start, end){
#   entered <- between(ymd(table$EntryDate), mdy(start), mdy(end))
#   entered
# }
# 
# exited_between <- function(table, start, end){
#   exited <- between(ymd(table$ExitDate), mdy(start), mdy(end))
#   exited
# }
# 
# stayed_between <- function(table, start, end){
#   stayed <- ymd(table$EntryAdjust) <= mdy(end) &
#     (is.na(table$ExitDate) | ymd(table$ExitDate) > mdy(start))
#   stayed
# }

# Projects Operating Between Date Range Function --------------------------
# 
# operating_between <- function(table, start, end) {
#   operating <-  if_else(
#     is.na(table$OperatingStartDate) |
#       ymd(table$OperatingStartDate) > mdy(end) |
#       (!is.na(table$OperatingEndDate) &
#          ymd(table$OperatingEndDate) < mdy(start)),
#     FALSE,
#     TRUE
#   )
#   operating
# }

# Beds Available Between --------------------------------------------------
# 
# beds_available_between <- function(table, start, end) {
#   available <-  if_else(
#     is.na(table$InventoryStartDate) |
#       ymd(table$InventoryStartDate) > mdy(end) |
#       (!is.na(table$InventoryEndDate) &
#          ymd(table$InventoryEndDate) < mdy(start)),
#     FALSE,
#     TRUE
#   )
#   available
# }

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

# this function translates the HUD .csv 1.7 and 1.8 lists 
# and returns yes, no, or unknown as appropriate
translate_HUD_yes_no <- function(column_name){
  case_when(
    column_name == 1 ~ "Yes", 
    column_name == 0 ~ "No",
    column_name %in% c(8, 9, 99) ~ "Unknown"
  )
}

copy_lgl <- function(files, dir, overwrite) {
  purrr::map_lgl(files, ~{
    .c <- file.copy(.x, to = dir, overwrite = overwrite)
    if (.c) message(.x, " copied to ", file.path(dir, basename(.x)))
    else 
      message(.x, " did not copy. Perhaps it already exists? Set overwrite = TRUE to overwrite.")
    .c
  })
}

freeze_pe <- function(dir, overwrite = FALSE) {
  # if dir doesn't exist create it
  dirs <- c(dir, file.path(dir, "images"))
  if (any(!purrr::map_lgl(dirs, dir.exists))) purrr::walk(dirs, dir.create)
  
  files <- paste0(c("COHHIOHMIS", "Data_Quality", "cohorts"), ".Rdata")
  .a <- utils::askYesNo(paste0("Have ", paste0(files, collapse = ", ")," been created with today's data?"))
  if (.a) {
    .d_files <- list.files("data", full.names = TRUE, pattern = "csv$|xlsx$")
    .d_copied <- copy_lgl(.d_files, dirs[1], overwrite)
    .rd_files <- grep(paste0(paste0("(?:",files,"$)"), collapse = "|"), list.files("images", full.names = TRUE), value = TRUE, ignore.case = TRUE, perl = TRUE)
    .rd_copied <- copy_lgl(.rd_files, dirs[2], overwrite)
    out <- list(data = file.path(dirs[1], basename(.d_files[.d_copied])),
                rdata = file.path(dirs[2], basename(.rd_files[.rd_copied])))
  } else {
    out <- "No files copied. Ensure Rdata files have been created with today's data."
  }
  return(out)
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

# Experimental ------------------------------------------------------------

# HUD_value_to_description <-
#   function(table, element_name, element_column) {
#     element_name <- sym(element_name)
#     element_column <- enquo(element_column)
#     
#     a <- HUD_specs %>%
#       filter(DataElement == element_name) %>%
#       select("ReferenceNo", "Description")
#     
#     table$element_column <- with(a,
#                                  Description[match(table$element_column,
#                                                    HUD_specs$ReferenceNo)])
#   }
# 
# a <- subset(HUD_specs,
#             DataElement == "HouseholdType",
#             select = c("ReferenceNo", "Description"))
# Inventory$HouseholdType <- with(a,
#                                 Description[match(Inventory$HouseholdType,
#                                                   ReferenceNo)])


 
# HMIS_participating_between <- function(table, start, end) {
#   HMISParticipating <-  if_else(
#     (table$HMISParticipatingBeds == 0 | is.na(table$HMISParticipatingBeds)) |
#     (is.na(table$InventoryStartDate) |
#       ymd(table$InventoryStartDate) > mdy(end)) |
#       (!is.na(table$InventoryEndDate) &
#          ymd(table$InventoryEndDate) < mdy(start)),
#     FALSE,
#     TRUE
#   )
#   HMISParticipating
# }
# not sure what the heck to do about this. :( will have to pull based
# on UsesSP which is super clunky and will leave out providers
