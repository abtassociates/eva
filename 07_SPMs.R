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

library(tidyverse)
library(lubridate)
library(readxl)
library(HMIS)
library(writexl)

what_sheet_is_701_data_on <- 2

rename_file <-
  function(CoC = "OH-507",
           subdirectory = "Current",
           pattern = "(0700 -)") {
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    # create dirs if not created
    if (!dir.exists(directory)) dir.create(directory)
    if (!dir.exists(file.path(directory, subdirectory))) dir.create(file.path(directory, subdirectory))
    # substitute the pattern to make the filename with regex
    filename <- case_when(
      pattern == "(0700 -)" ~ "0700a",
      pattern == "(0700.1b)" ~ "0700b",
      pattern == "(0701 -)" ~ "0701",
      pattern == "(0702 -)" ~ "0702",
      pattern == "(0703 -)" ~ "0703",
      pattern == "(0704 -)" ~ "0704",
      pattern == "(0706 -)" ~ "0706"
    )
    # make the filename with the system-specified separator using file.path and use full.names to get the full path to the file
    .file <- list.files(file.path(directory, subdirectory),
               pattern = pattern, full.names = TRUE)
    # list.files will only list existing files. If there are no files listed - file.exists will error. rlang::is_empty will check if the .file vector of paths from list.files is empty, if not, proceed to rename
    if (!rlang::is_empty(.file)) {
      file.rename(
        .file,
        file.path(dirname(.file), paste0(filename, ".xls"))
      )
    }
  }

# Renaming all the files to reasonable things -----------------------------
expand.grid(
  CoC = c("OH-507", "OH-504"),
  subdirectory = c("Current", "Prior"),
  pattern = c(
    "(0700 -)",
    "(0700.1b)",
    "(0701 -)",
    "(0702 -)",
    "(0703 -)",
    "(0704 -)",
    "(0706 -)"
  )
, stringsAsFactors = FALSE) %>% 
  purrr::pwalk(rename_file)


# OPEN ALL YOUR EXCEL FILES AND PRESS ENABLE EDITING BEFORE PROCEEDING
# I think {openxlsx} has a means of doing this from R

# 0700a - Length of Time Homeless (using EEs) -----------------------------

check_loth_a <- function(CoC = "OH-507", subdirectory = "Current") {
  
  directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                         CoC == "OH-504" ~ "SPM_data_YO")
  
  loth_a <-
    read_xls(
      paste0(directory, "/", subdirectory, "/0700a.xls"),
      sheet = 3,
      range = cell_cols("B2:C9")
    ) %>%
    select("Prompt" = 2, "Selection" = 3) %>%
    head(n = 7L) %>%
    spread(Prompt, Selection) %>%
    select(
      "EDA" = 1,
      "EffectiveDate" = 2,
      "ReportEnd" = 3,
      "PriorYear" = 4,
      "ReportStart" = 5,
      "CoCInfo" = 6,
      "Providers" = 7
    ) %>%
    mutate(
      ReportStart = as.integer(ReportStart),
      ReportEnd = as.integer(ReportEnd),
      PriorYear = as.integer(PriorYear),
      EffectiveDate = as.integer(EffectiveDate),
      ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
      ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
      EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
      PriorYear = as.Date(PriorYear, origin = "1899-12-30")
    )
  
  rpt_0700a <- loth_a %>%
    select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
    mutate(CoCName = CoC,
           CurrentOrPrior = subdirectory,
           ReportName = "0700a")
  
  assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0700a"), 
         rpt_0700a, 
         envir = .GlobalEnv)
  
  loth_ees <- read_xls(
    paste0(directory,
           "/",
           subdirectory,
           "/0700a.xls"),
    sheet = 1,
    range = cell_cols("B2:E4")
  ) %>%
    select("Metric1a" = 1, "ClientCount" = 2, "AvgLoT" = 3, "MedLoT" = 4) %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory)
  
  assign(paste0("spm_Metric_1a_", str_remove(CoC, "-"), "_", subdirectory), 
         loth_ees, 
         envir = .GlobalEnv)

  if (nrow(
    loth_a %>%
    filter(
      EDA != "-Default Provider-" |
      EffectiveDate != ReportEnd |
      str_sub(CoCInfo, 1, 6) != CoC |
      ReportEnd - years(1) != ReportStart |
      ReportStart - years(1) != PriorYear
    )
  ) > 0)
    # should this throw an error instead?
    stop(paste("the", CoC, subdirectory, "0700a report was run incorrectly"))
  else{
    print(paste(CoC, subdirectory, "0700a ok"))
  }
}

check_loth_a(CoC = "OH-507", subdirectory = "Current")
check_loth_a(CoC = "OH-507", subdirectory = "Prior")

check_loth_a(CoC = "OH-504", subdirectory = "Current")
check_loth_a(CoC = "OH-504", subdirectory = "Prior")



# 0700b - Length of Time Homeless (self-report) ---------------------------


check_loth_b <- function(CoC = "OH-507", subdirectory = "Current"){
  
  directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                         CoC == "OH-504" ~ "SPM_data_YO")
  
  loth_b <- read_xls(paste0(directory,
                            "/",
                            subdirectory,
                            "/0700b.xls"),
                         sheet = 3,
                         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "EffectiveDate" = 2,
         "ReportEnd" = 3,
         "PriorYear" = 4,
         "ReportStart" = 5,
         "CoCInfo" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

  rpt_0700b <- loth_b %>%
    select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
    mutate(CoCName = CoC,
           CurrentOrPrior = subdirectory,
           ReportName = "0700b")
  
  assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0700b"), 
         rpt_0700b, 
         envir = .GlobalEnv)
  
  loth_self_report <- read_xls(
    paste0(directory,
           "/",
           subdirectory,
           "/0700b.xls"),
    sheet = 1,
    range = cell_cols("B2:E4")
  ) %>%
    select(
      "Metric1b" = 1,
      "ClientCount" = 2,
      "AvgLoT" = 3,
      "MedLoT" = 4
    ) %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory)  
  
  assign(paste0("spm_Metric_1b_", str_remove(CoC, "-"), "_", subdirectory), 
         loth_self_report, 
         envir = .GlobalEnv)
  
if(nrow(
  loth_b %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    str_sub(CoCInfo, 1, 6) != CoC |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  stop(paste("the", CoC, subdirectory,"0700b report was run incorrectly"))
  else{
    print(paste(CoC, subdirectory, "0700b ok"))
  }
  
  }

check_loth_b(CoC = "OH-507", subdirectory = "Current")
check_loth_b(CoC = "OH-507", subdirectory = "Prior")

check_loth_b(CoC = "OH-504", subdirectory = "Current")
check_loth_b(CoC = "OH-504", subdirectory = "Prior")


# 0701 - Returns to Homelessness ------------------------------------------

check_recurrence <-
  function(CoC = "OH-507", subdirectory = "Current") {
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    
    worksheet_count <- length(excel_sheets(paste0(
      directory,
      "/",
      subdirectory,
      "/0701.xls"
    )))
    
    recurrence <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0701.xls"),
      sheet = worksheet_count,
      range = cell_cols("B2:C9")
    ) %>%
      select("Prompt" = 2, "Selection" = 3) %>%
      head(n = 7L) %>%
      spread(Prompt, Selection) %>%
      select(
        "EDA" = 1,
        "ReportEnd" = 2,
        "EffectiveDate" = 3,
        "PriorYear" = 4,
        "Prior2Year" = 5,
        "CoCInfo" = 6,
        "Providers" = 7
      ) %>%
      mutate(
        Prior2Year = as.integer(Prior2Year),
        ReportEnd = as.integer(ReportEnd),
        PriorYear = as.integer(PriorYear),
        EffectiveDate = as.integer(EffectiveDate),
        Prior2Year = as.Date(Prior2Year, origin = "1899-12-30"),
        ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
        EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
        PriorYear = as.Date(PriorYear, origin = "1899-12-30")
      )
    
    rpt_0701 <- recurrence %>%
      select(PriorYear, Prior2Year, ReportEnd, EffectiveDate) %>%
      mutate(CoCName = CoC,
             CurrentOrPrior = subdirectory,
             ReportName = "0701")
    
    assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0701"), 
           rpt_0701, 
           envir = .GlobalEnv)
    
    recurrence_data <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0701.xls"),
      sheet = what_sheet_is_701_data_on,
      range = cell_rows(3:10)
    ) %>%
      select(
        "ProjectType" = 1,
        "ExitedToPHPast2Yrs" = 2,
        "LessThan6mo" = 3,
        "SixTo12mo" = 4,
        "ThirteenTo24mo" = 5
      ) %>%
      mutate(CoCName = CoC,
             CurrentPrior = subdirectory)  
    
    assign(paste0("spm_Metric_2_", str_remove(CoC, "-"), "_", subdirectory), 
           recurrence_data, 
           envir = .GlobalEnv)

    if (nrow(
      recurrence %>%
      filter(
        EDA != "-Default Provider-" |
        EffectiveDate != ReportEnd |
        str_sub(CoCInfo, 1, 6) != CoC |
        ReportEnd - years(2) != PriorYear |
        ReportEnd - years(3) != Prior2Year
      )
    ) > 0)
      stop(paste("the", CoC, subdirectory, "0701 report was run incorrectly"))
    else{
      print(paste(CoC, subdirectory, "0701 ok"))
    }
  }

check_recurrence(CoC = "OH-507", subdirectory = "Current")
check_recurrence(CoC = "OH-507", subdirectory = "Prior")

check_recurrence(CoC = "OH-504", subdirectory = "Current")
check_recurrence(CoC = "OH-504", subdirectory = "Prior")


# 0702 - Number of Homeless -----------------------------------------------

check_homeless_count <-
  function(CoC = "OH-507", subdirectory = "Current") {
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    
    homeless_count <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0702.xls"),
      sheet = 3,
      range = cell_cols("B2:C9")
    ) %>%
      select("Prompt" = 2, "Selection" = 3) %>%
      head(n = 7L) %>%
      spread(Prompt, Selection) %>%
      select(
        "EDA" = 1,
        "ReportEnd" = 2,
        "ReportStart" = 3,
        "EffectiveDate" = 4,
        "PriorYear" = 5,
        "CoCInfo" = 6,
        "Providers" = 7
      ) %>%
      mutate(
        ReportStart = as.integer(ReportStart),
        ReportEnd = as.integer(ReportEnd),
        PriorYear = as.integer(PriorYear),
        EffectiveDate = as.integer(EffectiveDate),
        ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
        ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
        EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
        PriorYear = as.Date(PriorYear, origin = "1899-12-30")
      )
    
    rpt_0702 <- homeless_count %>%
      select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
      mutate(CoCName = CoC,
             CurrentOrPrior = subdirectory,
             ReportName = "0702")
    
    assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0702"),
           rpt_0702,
           envir = .GlobalEnv)
    
    homeless_count_data <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0702.xls"),
      sheet = 1,
      range = cell_cols("B2:D6")
    ) %>%
      select("Type" = 1, "Count" = 3) %>%
      mutate(CoCName = CoC,
             CurrentPrior = subdirectory)  
    
    assign(paste0("spm_Metric_3_", str_remove(CoC, "-"), "_", subdirectory),
           homeless_count_data,
           envir = .GlobalEnv)
    
    if (nrow(
      homeless_count %>%
      filter(
        EDA != "-Default Provider-" |
        EffectiveDate != ReportEnd |
        str_sub(CoCInfo, 1, 6) != CoC |
        ReportEnd - years(1) != ReportStart |
        ReportStart - years(1) != PriorYear
      )
    ) > 0)
      stop(paste("the", CoC, subdirectory, "0702 report was run incorrectly"))
    else{
      print(paste(CoC, subdirectory, "0702 ok"))
    }
  }

check_homeless_count(CoC = "OH-507", subdirectory = "Current")
check_homeless_count(CoC = "OH-507", subdirectory = "Prior")

check_homeless_count(CoC = "OH-504", subdirectory = "Current")
check_homeless_count(CoC = "OH-504", subdirectory = "Prior")


# 0703 - Employment Income Growth -----------------------------------------


check_income <- function(CoC = "OH-507", subdirectory = "Current") {
  directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                         CoC == "OH-504" ~ "SPM_data_YO")
  
  worksheet_count <- length(excel_sheets(paste0(
    directory,
    "/",
    subdirectory,
    "/0703.xls"
  )))
  
  income <- read_xls(
    paste0(directory,
           "/",
           subdirectory,
           "/0703.xls"),
    sheet = worksheet_count,
    range = cell_cols("B2:e9")
  ) %>%
    select("Prompt" = 2, "Selection" = 5) %>%
    head(n = 7L) %>%
    spread(Prompt, Selection) %>%
    select(
      "EDA" = 1,
      "ReportEnd" = 2,
      "ReportStart" = 3,
      "EffectiveDate" = 4,
      "PriorYear" = 5,
      "CoCInfo" = 6,
      "Providers" = 7
    ) %>%
    mutate(
      ReportStart = as.integer(ReportStart),
      ReportEnd = as.integer(ReportEnd),
      PriorYear = as.integer(PriorYear),
      EffectiveDate = as.integer(EffectiveDate),
      ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
      ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
      EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
      PriorYear = as.Date(PriorYear, origin = "1899-12-30")
    )
  
  
  rpt_0703 <- income %>%
    select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
    mutate(CoCName = CoC,
           CurrentOrPrior = subdirectory,
           ReportName = "0703")
  
  assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0703"), 
         rpt_0703, 
         envir = .GlobalEnv)
  
  income_data <- read_xls(paste0(directory,
                                 "/",
                                 subdirectory,
                                 "/0703.xls"),
                             sheet = 1) %>%
    select("ClientsCounted" = 1, "Counts" = 3) %>%
    filter(!is.na(ClientsCounted))
  
  income_empl_stayers <- income_data[c(1:3),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.1")  
  
  income_non_empl_stayers <- income_data[c(5:7),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.2") 
  
  income_total_stayers <- income_data[c(9:11),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.3") 
  
  income_empl_leavers <- income_data[c(13:15),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.4") 
  
  income_non_empl_leavers <- income_data[c(17:19),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.5") 
  
  income_total_leavers <- income_data[c(21:23),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.6") 
  
  income_data <- rbind(
    income_empl_leavers,
    income_empl_stayers,
    income_non_empl_leavers,
    income_non_empl_stayers,
    income_total_leavers,
    income_total_stayers
  )
  
  assign(paste0("spm_Metric_4_", str_remove(CoC, "-"), "_", subdirectory),
         income_data,
         envir = .GlobalEnv)
  
  if (nrow(
    income %>%
    filter(
      EDA != "-Default Provider-" |
      EffectiveDate != ReportEnd |
      str_sub(CoCInfo, 1, 6) != CoC |
      ReportEnd - years(1) != ReportStart |
      ReportStart - years(1) != PriorYear
    )
  ) > 0)
    stop(paste("the", CoC, subdirectory, "0703 report was run incorrectly"))
  else{
    print(paste(CoC, subdirectory, "0703 ok"))
  }
}

check_income(CoC = "OH-507", subdirectory = "Current")
check_income(CoC = "OH-507", subdirectory = "Prior")

check_income(CoC = "OH-504", subdirectory = "Current")
check_income(CoC = "OH-504", subdirectory = "Prior")


# 0704 - Number who became homeless for the first time --------------------

check_first_timers <-
  function(CoC = "OH-507", subdirectory = "Current") {
    
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    
    first_timers <- read_xls(paste0(directory,
                                    "/",
                                    subdirectory,
                                    "/0704.xls"),
                             sheet = 4,
                             range = cell_cols("B2:C9")) %>%
      select("Prompt" = 2, "Selection" = 3) %>%
      head(n = 7L) %>%
      spread(Prompt, Selection) %>%
      select(
        "EDA" = 1,
        "ReportEnd" = 2,
        "ReportStart" = 3,
        "EffectiveDate" = 4,
        "PriorYear" = 5,
        "CoCInfo" = 6,
        "Providers" = 7
      ) %>%
      mutate(
        ReportStart = as.integer(ReportStart),
        ReportEnd = as.integer(ReportEnd),
        PriorYear = as.integer(PriorYear),
        EffectiveDate = as.integer(EffectiveDate),
        ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
        ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
        EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
        PriorYear = as.Date(PriorYear, origin = "1899-12-30")
      )
    
    rpt_0704 <- first_timers %>%
      select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
      mutate(CoCName = CoC,
             CurrentOrPrior = subdirectory,
             ReportName = "0704")
    
    assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0704"), 
           rpt_0704, 
           envir = .GlobalEnv)
    
    first_timers_data <- read_xls(paste0(directory,
                                         "/",
                                         subdirectory,
                                         "/0704.xls"),
                                     sheet = 1) %>%
      select("ClientsCounted" = 1, "Counts" = 3) %>%
      filter(!is.na(ClientsCounted))
    
    first_timers_lh <- first_timers_data[c(1:3), ] %>%
      mutate(CoCName = CoC,
             CurrentPrior = subdirectory,
             Metric = "5.1")
    
    first_timers_all <- first_timers_data[c(5:7), ] %>%
      mutate(CoCName = CoC,
             CurrentPrior = subdirectory,
             Metric = "5.2")
    
    first_timers_data <- rbind(first_timers_all, first_timers_lh)
    
    assign(paste0("spm_Metric_5_", str_remove(CoC, "-"), "_", subdirectory),
           first_timers_data,
           envir = .GlobalEnv)
    
    if (nrow(
      first_timers %>%
      filter(
        EDA != "-Default Provider-" |
        EffectiveDate != ReportEnd |
        str_sub(CoCInfo, 1, 6) != CoC |
        ReportEnd - years(1) != ReportStart |
        ReportStart - years(1) != PriorYear
      )
    ) > 0)
      stop(paste("the", CoC, subdirectory, "0704 report was run incorrectly"))
    else{
      print(paste(CoC, subdirectory, "0704 ok"))
    }
  }

check_first_timers(CoC = "OH-507", subdirectory = "Current")
check_first_timers(CoC = "OH-507", subdirectory = "Prior")

check_first_timers(CoC = "OH-504", subdirectory = "Current")
check_first_timers(CoC = "OH-504", subdirectory = "Prior")



# 0706 - Successful Placement ---------------------------------------------

check_exits_to_ph <-
  function(CoC = "OH-507", subdirectory = "Current") {
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    
    exits_to_ph <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0706.xls"),
      sheet = 3,
      range = cell_cols("B2:C9")
    ) %>%
      select("Prompt" = 2, "Selection" = 3) %>%
      head(n = 7L) %>%
      spread(Prompt, Selection) %>%
      select(
        "EDA" = 1,
        "ReportEnd" = 2,
        "ReportStart" = 3,
        "EffectiveDate" = 4,
        "PriorYear" = 5,
        "CoCInfo" = 6,
        "Providers" = 7
      ) %>%
      mutate(
        ReportStart = as.integer(ReportStart),
        ReportEnd = as.integer(ReportEnd),
        PriorYear = as.integer(PriorYear),
        EffectiveDate = as.integer(EffectiveDate),
        ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
        ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
        EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
        PriorYear = as.Date(PriorYear, origin = "1899-12-30")
      )
    
    rpt_0706 <- exits_to_ph %>%
      select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
      mutate(CoCName = CoC,
             CurrentOrPrior = subdirectory,
             ReportName = "0706")
    
    assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0706"), 
           rpt_0706, 
           envir = .GlobalEnv)
    
    exits_to_ph_data <- read_xls(paste0(directory,
                                        "/",
                                        subdirectory,
                                        "/0706.xls"),
                                 sheet = 1) %>%
      select("ClientsCounted" = 1, "Counts" = 3) %>%
      filter(!is.na(ClientsCounted)) %>%
      mutate(CoCName = CoC,
             CurrentPrior = subdirectory)
    
    exits_out_to_ph_current <- exits_to_ph_data[c(1:4), ] %>%
      mutate(Metric = "7a")
    
    exits_lh_current <- exits_to_ph_data[c(6:8), ] %>%
      mutate(Metric = "7b1")
    
    exits_ph_current <- exits_to_ph_data[c(10:12), ] %>%
      mutate(Metric = "7b2")
    
    exits_to_ph_data <- rbind(exits_out_to_ph_current,
                              exits_lh_current,
                              exits_ph_current)
    
    assign(paste0("spm_Metric_7_", str_remove(CoC, "-"), "_", subdirectory),
           exits_to_ph_data,
           envir = .GlobalEnv)
    
    if (nrow(
      exits_to_ph %>%
      filter(
        EDA != "-Default Provider-" |
        EffectiveDate != ReportEnd |
        str_sub(CoCInfo, 1, 6) != CoC |
        ReportEnd - years(1) != ReportStart |
        ReportStart - years(1) != PriorYear
      )
    ) > 0)
      stop(paste("the", CoC, subdirectory, "0706 report was run incorrectly"))
    else{
      print(paste(CoC, subdirectory, "0706 ok"))
    }
  }

check_exits_to_ph(CoC = "OH-507", subdirectory = "Current")
check_exits_to_ph(CoC = "OH-507", subdirectory = "Prior")

check_exits_to_ph(CoC = "OH-504", subdirectory = "Current")
check_exits_to_ph(CoC = "OH-504", subdirectory = "Prior")

# Check that ALL SPMs were run on the same date range ---------------------

current_rpts <- OH507_Current_rpt_0700a %>%
  full_join(OH507_Current_rpt_0700b) %>%
  full_join(OH507_Current_rpt_0701) %>%
  full_join(OH507_Current_rpt_0702) %>%
  full_join(OH507_Current_rpt_0703) %>%
  full_join(OH507_Current_rpt_0704) %>%
  full_join(OH504_Current_rpt_0700a) %>%
  full_join(OH504_Current_rpt_0700b) %>%
  full_join(OH504_Current_rpt_0701) %>%
  full_join(OH504_Current_rpt_0702) %>%
  full_join(OH504_Current_rpt_0703) %>%
  full_join(OH504_Current_rpt_0704)

prior_rpts <- OH507_Prior_rpt_0700a %>%
  full_join(OH507_Prior_rpt_0700b) %>%
  full_join(OH507_Prior_rpt_0701) %>%
  full_join(OH507_Prior_rpt_0702) %>%
  full_join(OH507_Prior_rpt_0703) %>%
  full_join(OH507_Prior_rpt_0704) %>%
  full_join(OH504_Prior_rpt_0700a) %>%
  full_join(OH504_Prior_rpt_0700b) %>%
  full_join(OH504_Prior_rpt_0701) %>%
  full_join(OH504_Prior_rpt_0702) %>%
  full_join(OH504_Prior_rpt_0703) %>%
  full_join(OH504_Prior_rpt_0704)

mahoning_current <- current_rpts %>%
  filter(CoCName == "OH-504") %>%
  mutate(CheckStart = if_else(min(ReportStart, na.rm = TRUE) ==
                                max(ReportStart, na.rm = TRUE),
                              0, 1),
         CheckEnd = if_else(min(ReportEnd, na.rm = TRUE) ==
                              max(ReportEnd, na.rm = TRUE),
                            0, 1),
         CheckPrior = if_else(min(PriorYear, na.rm = TRUE) ==
                              max(PriorYear, na.rm = TRUE),
                              0, 1),
         CheckPrior2 = if_else(is.na(Prior2Year) | 
                                 Prior2Year + years(1) == PriorYear,
                               0, 1))

bos_current <- current_rpts %>%
  filter(CoCName == "OH-507") %>%
  mutate(CheckStart = if_else(min(ReportStart, na.rm = TRUE) ==
                                max(ReportStart, na.rm = TRUE),
                              0, 1),
         CheckEnd = if_else(min(ReportEnd, na.rm = TRUE) ==
                              max(ReportEnd, na.rm = TRUE),
                            0, 1),
         CheckPrior = if_else(min(PriorYear, na.rm = TRUE) ==
                                max(PriorYear, na.rm = TRUE),
                              0, 1),
         CheckPrior2 = if_else(is.na(Prior2Year) | 
                                 Prior2Year + years(1) == PriorYear,
                               0, 1))

mahoning_prior <- prior_rpts %>%
  filter(CoCName == "OH-504") %>%
  mutate(CheckStart = if_else(min(ReportStart, na.rm = TRUE) ==
                                max(ReportStart, na.rm = TRUE),
                              0, 1),
         CheckEnd = if_else(min(ReportEnd, na.rm = TRUE) ==
                              max(ReportEnd, na.rm = TRUE),
                            0, 1),
         CheckPrior = if_else(min(PriorYear, na.rm = TRUE) ==
                                max(PriorYear, na.rm = TRUE),
                              0, 1),
         CheckPrior2 = if_else(is.na(Prior2Year) | 
                                 Prior2Year + years(1) == PriorYear,
                               0, 1))

bos_prior <- prior_rpts %>%
  filter(CoCName == "OH-507") %>%
  mutate(CheckStart = if_else(min(ReportStart, na.rm = TRUE) ==
                                max(ReportStart, na.rm = TRUE),
                              0, 1),
         CheckEnd = if_else(min(ReportEnd, na.rm = TRUE) ==
                              max(ReportEnd, na.rm = TRUE),
                            0, 1),
         CheckPrior = if_else(min(PriorYear, na.rm = TRUE) ==
                                max(PriorYear, na.rm = TRUE),
                              0, 1),
         CheckPrior2 = if_else(is.na(Prior2Year) | 
                                 Prior2Year + years(1) == PriorYear,
                               0, 1))

if_else(
  sum(mahoning_current$CheckStart) == 0,
  paste(
    "Mahoning Current Report Start =", 
    min(mahoning_current$ReportStart, na.rm = TRUE)
  ),
  "Your Mahoning Current Start Dates do not all match."
)

if_else(
  sum(mahoning_current$CheckEnd) == 0,
  paste(
    "Mahoning Current Report End =", 
    min(mahoning_current$ReportEnd, na.rm = TRUE)
  ),
  "Your Mahoning Current End Dates do not all match."
)

if_else(
  sum(mahoning_current$CheckPrior) == 0,
  paste(
    "Mahoning Current Report Prior Start =", 
    min(mahoning_current$PriorYear, na.rm = TRUE)
  ),
  "Your Mahoning Current Prior Start Dates do not all match."
)

if_else(
  sum(bos_current$CheckStart) == 0,
  paste(
    "BoS Current Report Start =", 
    min(bos_current$ReportStart, na.rm = TRUE)
  ),
  "Your BOS Current Start Dates do not all match."
)

if_else(
  sum(bos_current$CheckEnd) == 0,
  paste(
    "BoS Current Report End =", 
    min(bos_current$ReportEnd, na.rm = TRUE)
  ),
  "Your BoS Current End Dates do not all match."
)

if_else(
  sum(bos_current$CheckPrior) == 0,
  paste(
    "BoS Current Report Prior Start =", 
    min(bos_current$PriorYear, na.rm = TRUE)
  ),
  "Your BoS Current Prior Start Dates do not all match."
)

spm_Metric_1a <- rbind(
  spm_Metric_1a_OH504_Current,
  spm_Metric_1a_OH504_Prior,
  spm_Metric_1a_OH507_Current,
  spm_Metric_1a_OH507_Prior
) %>%
  pivot_wider(id_cols = c(Metric1a, CoCName, CurrentPrior),
              names_from = CurrentPrior,
              values_from = c(ClientCount, AvgLoT, MedLoT))

spm_Metric_1b <- rbind(
  spm_Metric_1b_OH504_Current,
  spm_Metric_1b_OH504_Prior,
  spm_Metric_1b_OH507_Current,
  spm_Metric_1b_OH507_Prior
) %>%
  pivot_wider(id_cols = c(Metric1b, CoCName, CurrentPrior),
              names_from = CurrentPrior,
              values_from = c(ClientCount, AvgLoT, MedLoT))

spm_Metric_2 <- rbind(
  spm_Metric_2_OH504_Current,
  spm_Metric_2_OH504_Prior,
  spm_Metric_2_OH507_Current,
  spm_Metric_2_OH507_Prior
) %>%
  filter(ProjectType %in% c(
    "Exit was from SO",
    "Exit was from ES",
    "Exit was from TH",
    "Exit was from PSH",
    "Exits from RRH",
    "Exit was from SH",
    "TOTAL Returns to Homelessness"
  )) %>%
  pivot_wider(id_cols = c(ProjectType, CoCName, CurrentPrior),
              names_from = CurrentPrior,
              values_from = c(ExitedToPHPast2Yrs, 
                              LessThan6mo, 
                              SixTo12mo,
                              ThirteenTo24mo))

spm_Metric_3 <- rbind(
  spm_Metric_3_OH504_Current,
  spm_Metric_3_OH504_Prior,
  spm_Metric_3_OH507_Current,
  spm_Metric_3_OH507_Prior
) %>%
  pivot_wider(id_cols = c(Type, CoCName, CurrentPrior),
              names_from = CurrentPrior,
              values_from = Count)

spm_Metric_4 <- rbind(
  spm_Metric_4_OH504_Current,
  spm_Metric_4_OH504_Prior,
  spm_Metric_4_OH507_Current,
  spm_Metric_4_OH507_Prior
) %>%
  pivot_wider(id_cols = c(ClientsCounted, CoCName, CurrentPrior, Metric),
              names_from = CurrentPrior,
              values_from = Counts)

spm_Metric_5 <- rbind(
  spm_Metric_5_OH504_Current,
  spm_Metric_5_OH504_Prior,
  spm_Metric_5_OH507_Current,
  spm_Metric_5_OH507_Prior
) %>%
  pivot_wider(id_cols = c(ClientsCounted, CoCName, CurrentPrior, Metric),
              names_from = CurrentPrior,
              values_from = Counts)

spm_Metric_7 <- rbind(
  spm_Metric_7_OH504_Current,
  spm_Metric_7_OH504_Prior,
  spm_Metric_7_OH507_Current,
  spm_Metric_7_OH507_Prior
) %>%
  pivot_wider(id_cols = c(ClientsCounted, CoCName, CurrentPrior, Metric),
              names_from = CurrentPrior,
              values_from = Counts)

spm_current_start_date <- min(bos_current$ReportStart, na.rm = TRUE)

spm_current_end_date <- min(bos_current$ReportEnd, na.rm = TRUE) - days(1)

spm_prior_start_date <- min(bos_prior$ReportStart, na.rm = TRUE) 

spm_prior_end_date <- min(bos_prior$ReportEnd, na.rm = TRUE) - days(1)

report_dates <- rbind(current_rpts, prior_rpts) %>%
  select(
    CoCName,
    ReportName,
    CurrentOrPrior,
    Prior2Year,
    PriorYear,
    ReportStart,
    ReportEnd,
    EffectiveDate
  )

write_xlsx(
  x = list(
    measure1a = spm_Metric_1a,
    measure1b = spm_Metric_1b,
    measure2 = spm_Metric_2,
    measure3 = spm_Metric_3,
    measure4 = spm_Metric_4,
    measure5 = spm_Metric_5,
    measure7a = spm_Metric_7,
    dates = report_dates
  ),
  "random_data/all_spms.xlsx"
)

rm(list = setdiff(ls(), ls(pattern = "spm_")))

save(list = ls(), file = "images/SPM_data.RData", compress = FALSE)



