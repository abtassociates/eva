######################
# PURPOSE: This program runs checks of the upload file's content.
# For example, it checks for incorrect date formats, missing columns,
# unexpected nulls, and more
######################

logToConsole(session, "Running file structure analysis")

# Invalid values -----------------------------
source("machine_readable_specs_validation.R", local=TRUE)

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
  no_enrollment_records,
  duplicate_household_id,
  invalid_values,
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
