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

library(dplyr)

# creates empty environment called rdata
rdata <- rlang::env(rlang::empty_env())

#loads all images into rdata 
purrr::walk(list.files("images", pattern = ".RData", full.names = TRUE), ~{
  message(paste0("Loading ", .x))
  load(.x, envir = rdata)
})

# Function to copy image files to Rm and Rme ------------------------------

#' @title Send data to the respective app directory
#' @description Saves `data.frame`s to `feather` files in the `data/db` directory 
#' and all other objects as a `list` to an `rds` file in the `data/` directory.
#' @param names \code{(character)} vector of object names
#' @param directory \code{(character)} file path to the application directory
#' @param environment \code{(environment)} in which to search for `names`
#' @param accessor \code{(function)} that uses default parameters for loading a 
#' data object from disk. Overwrites the data object.
#' @importFrom purrr map_lgl map imap walk
#' @importFrom dplyr `%>%`
data_prep <- function(object_names, directory, environment, accessor) {
  .missing <- purrr::map_lgl(setNames(object_names, object_names), ~ 
                               !exists(.x, envir = environment, inherits = FALSE))
  
  if (any(.missing)) {
    stop(paste0(
      "00_copy_images missing objects: ",
      paste0(object_names[.missing], collapse = ",")
    ))
  }
  objects <-
    rlang::env_get_list(environment, 
                        object_names, 
                        default = stop("00_copy_images: object missing"))
  # data directory
  .dir <- file.path(directory, "data")
  # db directory inside data directory
  .db <- file.path(.dir, "db")
  # make if not created
  purrr::walk(c(.dir, .db), ~ {
    if (!dir.exists(.x)) {
      dir.create(.x)
    }
  })
  if (missing(accessor)) {
    accessor <- function(x = as.character(match.call()[[1]]),
                         path = "data/db",
                         ext = ".feather") {
      feather::read_feather(
        file.path(path, 
                  paste0(x, 
                         ifelse(grepl("^\\.", ext), ext, paste0(".", ext))
                  )
        )
      )
    }
  } 
  rlang::fn_env(accessor) <- rlang::env(baseenv())
  .is_df <- purrr::map_lgl(objects, is.data.frame)
  if (any(.is_df)) {
    objects[.is_df] <- objects[.is_df] %>%
      # Write the feather files
      purrr::imap(~ {
        message(paste0("Saving ", .y, ".feather"))
        feather::write_feather(.x, file.path(.db, paste0(.y, ".feather")))
        .x
      }) %>%
      # overwrite the DFs with an accessor function.
      # This reads the feather file with the same name as the function
      purrr::map(~accessor)
    # save a list of the data.frames that were replaced with accessor functions
    # for reference while working on apps
    objects$df_nms <- names(objects)[.is_df]
    # Save the results
  }
  #   .is_gg <- purrr::map_lgl(objects, ~inherits(.x, "ggplot"))
  #   
  # if (any(.is_gg)) {
  #     objects[.is_gg] <- objects[.is_gg] %>%
  #       # Write the images
  #       purrr::imap( ~ {
  #         message(paste0("Saving ", .y, ".jpg"))
  #         .p <-
  #           file.path(directory,
  #                     purrr::when(
  #                       grepl("Rminor$", directory),
  #                       . ~ file.path("inst", "app", "www"),
  #                       ~ "www"
  #                     ),
  #                     paste0(.y, ".jpg"))
  #         ggplot2::ggsave(
  #           .p,
  #           .x,
  #           width = 800 / 72,
  #           height = 500 / 72,
  #           device = "jpeg",
  #           units = "in"
  #         )
  #         file.path("www", basename(.p))
  #       })
  #     objects$gg_nms <- names(objects)[.is_gg]
  #   }
  saveRDS(
    objects,
    file = file.path(.dir, paste0(basename(directory), ".rds"))
  )
}

## to Rm:
.Rm <- c("APs",
         "bos_counties",
         "BoS_PIT",
         "calc_2_yrs_prior_end",                  
         "calc_2_yrs_prior_range",                
         "calc_2_yrs_prior_start",                
         "calc_data_goes_back_to",                
         "calc_full_date_range",                  
         "Client",
         "covid19",
         "covid19_priority_plot",
         "covid19_status_plot",
         "current_tay_hohs",
         # "FileEnd",
         "goals",
         "hc_began_collecting_covid_data",        
         "hc_check_dq_back_to",                   
         "hc_data_goes_back_to",    
         "hc_project_eval_start",
         "hc_project_eval_end",
         "hc_project_eval_docs_due",
         "hc_psh_started_collecting_move_in_date",
         "Mah_PIT",
         "meta_HUDCSV_Export_Date",               
         "meta_HUDCSV_Export_End",                
         "meta_HUDCSV_Export_Start",              
         "meta_Rmisc_last_run_date",  
         "note_bed_utilization",
         "note_calculation_utilization",
         "note_qpr_dq_community_need",
         "note_qpr_housed_county",
         "note_qpr_served_county",
         "note_unit_utilization",
         "Organization",
         "pe_validation_summary",
         "project_type",
         "qpr_benefits",
         "qpr_income",
         "qpr_leavers",
         "qpr_rrh_enterers",
         "qpr_spdats_county",
         "qpr_spdats_project",
         "qpr_spending",
         "regions",
         "Scores",
         "Services",
         "spm_Metric_1b",
         "spm_Metric_2",
         "spm_Metric_7",
         "spm_current_end_date",
         "spm_current_start_date",
         "spm_prior_end_date",
         "spm_prior_start_date",
         "summary_pe_final_scoring",
         "Users",
         "utilization",
         "utilization_bed",
         "utilization_unit",
         "validation",
         "veteran_current_in_project"
) %>% 
  data_prep("../Rminor", rdata)

# to Rme

.Rme <- c(#"active_list",
          # "aps_no_referrals",
          # "Beds",
          "calc_2_yrs_prior_end", # CSVExportDFs                  
          "calc_2_yrs_prior_range",# CSVExportDFs                
          "calc_2_yrs_prior_start",# CSVExportDFs                
          "calc_data_goes_back_to",# CSVExportDFs                
          "calc_full_date_range",# CSVExportDFs                  
          "Client",# CSVExportDFs
          "dq_main",# Data_Quality
          "dq_past_year",# Data_Quality
          "dq_overlaps",# Data_Quality
          "detail_eligibility",# Data_Quality
          "dq_plot_eligibility",# Data_Quality
          "dq_plot_errors",# Data_Quality
          "dq_plot_hh_errors",# Data_Quality
          # "dq_plot_hh_no_spdat",
          # "dq_plot_outstanding_referrals",
          "dq_plot_projects_errors",# Data_Quality
          "dq_plot_projects_warnings",# Data_Quality
          "dq_plot_warnings",# Data_Quality
          "dq_providers",# Data_Quality
          "enhanced_yes_no_translator",# CSVExportDFs
          "hc_began_collecting_covid_data",  # CSVExportDFs
          "hc_bos_start_vaccine_data",# CSVExportDFs
          "hc_check_dq_back_to",# CSVExportDFs                   
          "hc_data_goes_back_to",  # CSVExportDFs
          "hc_project_eval_start",# CSVExportDFs
          "hc_project_eval_end",# CSVExportDFs
          "hc_project_eval_docs_due",# CSVExportDFs
          "hc_psh_started_collecting_move_in_date",# CSVExportDFs
          "HUD_specs",# CSVExportDFs
          "living_situation",# CSVExportDFs
          "meta_HUDCSV_Export_Date",# CSVExportDFs               
          "meta_HUDCSV_Export_End",  # CSVExportDFs              
          "meta_HUDCSV_Export_Start",  # CSVExportDFs            
          "meta_Rmisc_last_run_date", # CSVExportDFs 
          "Organization",# CSVExportDFs
          # "pe_increase_income",
          # "pe_exits_to_ph",
          # "pe_homeless_history_index",
          # "pe_length_of_stay",
          # "pe_benefits_at_exit",
          # "pe_entries_no_income",
          # "pe_long_term_homeless",
          # "pe_res_prior",
          # "pe_own_housing",
          # "pe_validation_summary",
          # "pe_scored_at_ph_entry",
          "project_type",# CSVExportDFs
          # "qpr_income",
          # "qpr_benefits",
          # "qpr_leavers",
          # "qpr_rrh_enterers",
          # "qpr_spending",
          # "qpr_spdats_project",
          # "qpr_spdats_county",
          # "Referrals",
          # "summary_pe_final_scoring",
          "Users",# CSVExportDFs
          # "utilizers_clients",
          # "utilization",
          # "utilization_bed",
          "validation"#, # maybe try to use cohorts more instead of this?? (QPR_EEs.R)
          # "veteran_active_list" 
) %>% 
  data_prep("../Rminor_elevated", rdata)
