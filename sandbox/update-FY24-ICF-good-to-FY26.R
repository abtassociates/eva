# Unzip zip file to  new sandbox folder
upload_filepath <- "/media/sdrive/projects/CE_Data_Toolkit/Data Sets/FY24-ICF-good.zip"

source(here("hardcodes.R"))
source(here("helper_functions.R"))
library(collapse)
library(data.table)
#' Re-generate Demo Mode updated dataset
#' Updated shinytests (created datasets) as needed
#' 3.06 Gender -- retired -- Client.csv
#' Update columns.csv to remove gender-related columns. Currently, the gender columns in Client.csv are hidden and related FSA and DQ checks have been removed.
c <- importFile(upload_filepath, "Client") %>% 
  fselect(-c(
    Woman,Man,Transgender,NonBinary,CulturallySpecific,Questioning,DifferentIdentity,GenderNone,DifferentIdentityText
  ))

#' 4.21 Sex -- Data element added -- Client CSV.
#' Update columns.csv with new data element. An FSA check will be needed for this data element. Since this field will be funding specific, there are no current plans for a DQ check, but one may be developed at a later time.
#' Column Order: see above HMIS CSV specs
#' Valid values: see above HMIS CSV specs
valid_vals <- c(0, 1, 8, 9, 99)
c <- c %>% fmutate(Sex = sample(valid_vals, size = fnrow(.), replace = TRUE))

#' R3 Sexual Orientation -- retired -- Enrollment.csv
#' Update columns.csv to remove this column. Currently, no DQ checks in Eva for this data element. If there is an FSA check, it also should be removed.
e <- importFile(upload_filepath, "Enrollment") %>%
  fselect(-SexualOrientation, -SexualOrientationOther)


#' R13 Family Critical issues -- Expanded response options to include “Client doesn’t know” (8), “Client prefers not to answer” (9), and “Data not collected” (99) -- Enrollment CSV.
#' Update FSA check to allow these new responses items to this data element.
#' This is made of multiple "checkboxes", each of which has 1=Yes, 0=No, 8=Client doesn't know, etc.
valid_vals <- c(0,1,8,9,99)
e <- e %>%
  fmutate(
    UnemploymentFam = sample(valid_vals, size = fnrow(.), replace = TRUE),
    MentalHealthDisorderFam = sample(valid_vals, size = fnrow(.), replace = TRUE),
    PhysicalDisabilityFam = sample(valid_vals, size = fnrow(.), replace = TRUE),
    AlcoholDrugUseDisorderFam = sample(valid_vals, size = fnrow(.), replace = TRUE),
    InsufficientIncome = sample(valid_vals, size = fnrow(.), replace = TRUE),
    IncarceratedParent = sample(valid_vals, size = fnrow(.), replace = TRUE)
  )

#'  V2 Services Provided - SSVF -- Added response option 10: Healthcare Navigation -- Services CSV.
#' Update FSA check to allow for new response item to this data element.
#' @vlopez0603 - track down where new response option for V2 should go. VL 9/11: This new response is specific to V2.A Services.SubTypeProvided
valid_vals <- c(1,2,3,4,5,10,11,13)
s <- importFile(upload_filepath, "Services") %>%
  fmutate(
    SubTypeProvided = sample(valid_vals, size = fnrow(.), replace = TRUE)
  )


#'  V10 Mental Health Consultation -- data element added -- Enrollment CSV
#' Update columns.csv with new column. An FSA check will be needed for this data element. Since this field will be funding specific, there are no current plans for a DQ check, but one may be developed at a later time.
valid_vals <- c(1,2,3,4)
e <- e %>% 
  fmutate(
    MentalHealthConsultation = sample(valid_vals, size = fnrow(.), replace=TRUE)
  )

# Update Export.csv version to 2026
x <- importFile(upload_filepath, "Export") %>%
  fmutate(
    CSVVersion = "2026 v1"
  )

# Write new files
temp_folder <- here("sandbox/temp_data")
write.csv(
  x,
  paste0(temp_folder,"/Export.csv"), 
  row.names = FALSE, 
  na = ""
)
write.csv(
  e,
  paste0(temp_folder,"/Enrollment.csv"), 
  row.names = FALSE, 
  na = ""
)
write.csv(
  s,
  paste0(temp_folder, "/Services.csv"), 
  row.names = FALSE, 
  na = ""
)
write.csv(
  c,
  paste0(temp_folder, "/Client.csv"), 
  row.names = FALSE, 
  na = ""
)

# Zip em up
system("sync")
zip::zipr(
  zipfile = here("tests/FY26-test-good.zip"), 
  files = list.files(temp_folder, pattern = "*.csv$", full.names = TRUE),
  mode = "cherry-pick" # so the files are at the top directory
)
Sys.sleep(1)