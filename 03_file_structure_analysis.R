######################
# PURPOSE: This program runs checks of the upload file's content.
# For example, it checks for incorrect date formats, missing columns,
# unexpected nulls, and more
######################

logToConsole(session, "Running file structure analysis")

# Invalid values -----------------------------
source("detect_invalid_values.R", local=TRUE)
valid_values <- final_summary %>%
  merge_check_info(checkIDs = 50) %>%
  mutate(Detail = paste(name, "has", n, "rows with invalid values")) %>% 
  select(all_of(issue_display_cols))

# Integrity Enrollment ----------------------------------------------------
if (nrow(Enrollment) == 0) {
  no_enrollment_records <- data.table(
    Detail = "There are 0 enrollment records in the Enrollment.csv file"
  ) %>%
  merge_check_info_dt(checkIDs = 101)
} else {
  no_enrollment_records <- data.table()
}

# Count (unique) HouseholdIDs within a Project
duplicate_household_id <- Enrollment %>%
  fselect(HouseholdID, ProjectID) %>%
  funique() %>%
  fcount(HouseholdID) %>%
  fsubset(N > 1 & !is.na(HouseholdID)) %>%
  merge_check_info_dt(checkIDs = 98) %>%
  fmutate(
    Detail = paste("HouseholdID", 
                   HouseholdID,
                   "is reused across",
                   N,
                   "Enrollments into different projects.")
  ) %>%
  fselect(issue_display_cols) %>%
  funique()

session$userData$file_structure_analysis_main(rbind(
  df_incorrect_columns,
  df_unexpected_data_types,
  df_nulls,
  disabling_condition_invalid,
  duplicate_client_id,
  duplicate_enrollment_id,
  duplicate_household_id,
  export_id_client,
  foreign_key_no_primary_personalid_enrollment,
  foreign_key_no_primary_projectid_enrollment,
  living_situation_invalid,
  no_enrollment_records,
  nonstandard_CLS,
  nonstandard_destination,
  rel_to_hoh_invalid,
  valid_values_client,
  files_with_brackets,
  ignore.attr=TRUE
  ) %>%
  fmutate(Type = factor(Type, levels = issue_levels)) %>%
  roworder(Type)
)

if(session$userData$file_structure_analysis_main() %>% 
   fsubset(Type == "High Priority") %>%
   nrow() > 0) {
  session$userData$valid_file(0)
  
  # if structural issues were found, reset gracefully
  showModal(
    modalDialog(
      "Your uploaded HMIS CSV Export has at least one High Priority File 
          Structure Error. To be able to read an uploaded hashed HMIS CSV 
          Export, Eva requires the .zip file to have zero High Priority File 
          Structure Errors. Thus, to use Eva, your upload must have zero High 
          Priority File Structure Errors. Please share the file structure 
          issues, prioritizing the High Priotity File Structure Errrors, 
          with your HMIS vendor to fix.",
      easyClose = TRUE,
      title = "Unsuccessful Upload: Your HMIS CSV Export is not
          structurally valid",
      footer = modalButton("OK")
    )
  )
  
  logMetadata(session, "Unsuccessful upload - not structurally valid")
  
  reset_postvalid_components(session)
} else{
  session$userData$valid_file(1)
}
