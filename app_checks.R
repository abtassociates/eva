library(tidyverse)
library(openxlsx)
library(here)

#Bring in guidance from separate R script
source("03_guidance.R")

### CTRL + F "new check" TO ADD NEW CHECK ----

#### LIST OF ALL EVA CHECKS #### -------------
check1 = c(
  Source = "dq",
  Type = "High Priority",
  Issue = "Duplicate Entries",
  DataElement = "3.10 - Project Start Date, 3.11 - Project Exit Date",
  Guidance = str_squish(
    "A client cannot have two enrollments with the same entry date into the same
    project. These are duplicate enrollment records. Please address this issue."
  ),
  Notes = ""
)

check2 = c(
  "dq",
  "High Priority",
  "No Head of Household",
  "3.15 - Relationship to Head of Household",
  guidance_hoh_issues,
  ""
)

check3 = c(
  "dq",
  "High Priority",
  "Too Many Heads of Household",
  "3.15 - Relationship to Head of Household",
  guidance_hoh_issues,
  ""
)

check4 = c(
  "dq",
  "High Priority",
  "Missing Relationship to Head of Household",
  "3.15 - Relationship to Head of Household",
  guidance_hoh_issues,
  ""
)


check5 = c(
  "pdde",
  "High Priority",
  "Missing Geography Information",
  "2.03.8 - Continuum of Care Information: Geography Type",
  str_squish("Please ensure geography information for projects is complete."),
  str_squish("Missing geography information that makes this a High Priority Error are
  geocode, CoC code, and geography type.")
)


check6 = c("file structure",
           "High Priority",
           "Nulls or incorrect data type",
           str_squish("Specific data elements will be identified in the Detail
                      column of a CoC's Integrity Checks export."),
           str_squish("Either there is a column with nulls where they are not
                      allowed, or there is a column with an incorrect data type.
                      Please review the HMIS CSV Format Specifications for the
                      data types and null requirements associated with the file
                      and column listed in the detail and make the necessary
                      updates."),
           str_squish("If the data type is considered High Priority, this will
                      be a High Priority issue. Otherwise, this will be
                      considered a General Error."))

check7 = c(
  "file structure",
  "High Priority",
  "Duplicate PersonalIDs found in the Client file",
  "5.08 - Personal Identifier",
  str_squish("PersonalIDs should be unique in the Client file. Each unique
             client should only be listed once in the Client file. Please ensure
             that each unique client gets their own unique PersonalID in this
             file."),
  ""
)
check8 = c(
  "file structure",
  "High Priority",
  "Duplicate EnrollmentIDs found in the Enrollment file",
  "5.06 - Enrollment Identifier",
  str_squish("EnrollmentIDs should be unique in the Enrollment file. There may
             be no more than one record for any PersonalID with the same
             EnrollmentID. If there is more than one record for the same
             PersonalID with the same EnrollmentID, this represents an error in
             the CSV export algorithm."),
  ""
)

check9 = c(
  "file structure",
  "High Priority",
  "Client in the Enrollment file not found in Client file",
  "5.08 - Personal Identifier",
  str_squish("Per the HMIS CSV Format Specifications, all PersonalIDs in the
             Enrollment file should have a record in the Client file."),
  ""
)

check10 = c(
  "file structure",
  "High Priority",
  "ProjectID in the Enrollment file not found in Project file",
  "5.05 - Project Identifier",
  str_squish("Per the HMIS CSV Format Specifications, all ProjectIDs in the
             Enrollment file should have a record in the Project file."),
  ""
)

check11 = c("file structure", 
            "High Priority", 
            "Incorrect Date Format", 
            "Date-Based Data Elements", 
            str_squish(
              "Dates in the HMIS CSV Export should be in yyyy-mm-dd or
              yyyy-mm-dd hh:mm:ss format, in alginment with the HMIS CSV Format
              Specifications. Please check the Specifications for the file and
              column identified in the Detail and ensure the correct date format
              is used in the export."),
            "")

check12 = c("file structure", 
            "High Priority", 
            "Incorrect Columns", 
            str_squish("Specific data elements will be identified in the Detail
                       column of a CoC's Integrity Checks export."),
            str_squish("Your HMIS CSV Export should contain - with identical,
                       case-sensitive spelling - only the columns specified in
                       the columns.csv file. Please remove any extra columns and
                       make sure you have all missing columns."),
            str_squish("If the column is considered High Priority, this will be a 
                       High Priority issue. Otherwise, this will be considered a
                       Warning."))

check14 = c("file structure", 
            "High Priority", 
            "Incorrect Data Type", 
            str_squish("Specific data elements will be identified in the Detail
                       column of a CoC's Integrity Checks export."),
            str_squish("Data types must align with the HMIS CSV Format
                       Specifications. Please review the specifications for the
                       data types associated with the file and column liseted in
                       the Detail and make the necessary updates."),
            "")

check15 = c(
  "dq",
  "Error",
  "Future Exit Date",
  "3.11 - Project Exit Date",
  str_squish(
    "This client's Exit Date is a date in the future. Please enter the exact
    date the client left your program. If this client has not yet exited, delete
    the Exit and then enter the Exit Date once the client is no longer in your
    program."
  ),
  ""
)

check16 = c(
  "dq",
  "Error",
  "Missing Year Entered Service",
  "V1.1 - Veteran's Information: Year Entered Military Service",
  guidance_missing_at_entry,
  ""
)

check17 = c(
  "dq",
  "Error",
  "Incorrect Year Entered Service",
  "V1.1 - Veteran's Information: Year Entered Military Service",
  guidance_missing_at_entry,
  ""
)

check18 = c(
  "dq",
  "Error",
  "Missing Year Separated",
  "V1.1 - Veteran's Information: Year Separated from Military Service",
  guidance_missing_at_entry,
  ""
)

check19 = c(
  "dq",
  "Error",
  "Incorrect Year Separated",
  "V1.1 - Veteran's Information: Year Separated from Military Service",
  guidance_missing_at_entry,
  ""
)

check20 = c(
  "dq",
  "Error",
  "Missing War(s)",
  "V1.3-V1.10 - Veteran's Information: Theaters of Operations",
  guidance_missing_at_entry,
  ""
)

check21 = c(
  "dq",
  "Error",
  "Missing Military Branch",
  "V1.11 - Veteran's Information: Branch of the Military",
  guidance_missing_at_entry,
  ""
)

check22 = c(
  "dq",
  "Error",
  "Missing Discharge Status",
  "V1.12 - Veteran's Information: Discharge Status",
  guidance_missing_at_entry,
  ""
)

check23 = c(
  "dq",
  "Error",
  "Missing Percent AMI",
  "V4 - Percent of AMI (SSVF Eligibility)",
  guidance_missing_at_entry,
  ""
)

check24 = c(
  "dq",
  "Error",
  "Missing VAMC Station Number",
  "V6 - VAMC Station Number",
  guidance_missing_at_entry,
  ""
)

check25 = c(
  "dq",
  "Error",
  "Missing Some or All of Last Permanent Address",
  "V5 - Last Permanent Address",
  guidance_missing_at_entry,
  ""
)

check26 = c(
  "dq",
  "Error",
  "Missing HP Screening or Threshold Score",
  "V7 - HP Targeting Criteria",
  guidance_missing_at_entry,
  ""
)

check27 = c(
  "dq",
  "Error",
  "Missing Length of Stay",
  "3.917.2 - Prior Living Situation: length of Stay",
  guidance_missing_at_entry,
  ""
)

check28 = c(
  "dq",
  "Error",
  "Missing Client Location",
  "3.16 - Client Location",
  guidance_missing_at_entry,
  ""
)

check29 = c(
  "dq",
  "Error",
  "Missing Approximate Date Homeless",
  "3.917.3 - Prior Living Situation: Date Homelessness Started",
  guidance_missing_at_entry,
  ""
)

check30 = c(
  "dq",
  "Error",
  "Missing Previously Unsheltered, ES, SH",
  "3.917.2C - Prior Living Situation: Previous Street, ES, SH",
  guidance_missing_at_entry,
  ""
)

check31 = c(
  "dq",
  "Error",
  "Missing Residence Prior",
  "3.917.1 - Prior Living Situation: Living Situation",
  guidance_missing_at_entry,
  ""
)

check32 = c(
  "dq",
  "Error",
  "Missing Months or Times Homeless",
  str_squish("3.917.4 - Prior Living Situation: Times Homeless in Past 3 Years,
             3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years"),
  guidance_missing_at_entry,
  ""
)

check33 = c(
  "dq",
  "Error",
  "Missing Disabling Condition",
  "3.08 - Disabling Condition",
  guidance_missing_at_entry,
  ""
)

check34 = c(
  "dq",
  "Error",
  "Missing Name Data Quality",
  "3.01.5 - Name: Name Data Quality",
  guidance_missing_pii,
  ""
)

check35 = c(
  "dq",
  "Error",
  "Missing DOB",
  "3.03.1 - Date of Birth",
  str_squish(
    "This data element is required to be collected at Project Start. Please go
    to the client's assessment at Project Start to enter this data to HMIS. If
    this data was not collected, the client declined to provide the information
    or was unable to provide it, please update the DOB Quality field accordingly."
  ),
  ""
)

check36 = c(
  "dq",
  "Error",
  "Missing DOB Data Quality",
  "3.03.2 - Date of Birth: DOB Data Quality",
  str_squish(
    "This data element is required to be collected at Project Start. Please go
    to the client's assessment at Project Start to enter this data to HMIS. If
    this data was not collected, the client declined to provide the information
    or was unable to provide it, please update the DOB Quality field accordingly."
  ),
  ""
)

check37 = c("dq",
            "Error",
            "Missing Race",
            "3.04 - Race",
            guidance_missing_at_entry,
            "")

check38 = c("dq",
            "Error",
            "Missing Ethnicity",
            "3.05 - Ethnicity",
            guidance_missing_at_entry,
            "")

check39 = c("dq",
            "Error",
            "Missing Gender",
            "3.06 - Gender",
            guidance_missing_at_entry,
            "")

check40 = c(
  "dq",
  "Error",
  "Missing Veteran Status",
  "3.07 - Veteran Status",
  guidance_missing_pii,
  ""
)

check41 = c(
  "dq",
  "Error",
  "Invalid Move-In Date",
  "3.20 - Housing Move-In Date",
  str_squish(
    "This move-in date does not fall between the Entry Date
    and the Exit Date or this move-in date is after the date of the export."
  ),
  ""
)

check42 = c(
  "dq",
  "Error",
  "Incomplete Living Situation Data",
  "3.917 - Prior Living Situation",
  str_squish(
    "When responding to the Prior Living Situation questions in
         your assessment at Project Start, users must answer questions about the
         clients' situation prior to the \"Type of Residnce\" question that are
         important to help determine that client's Chronicity. Please answer these
         questions to the best of your knowledge."
  ),
  ""
)


check43 = c(
  "pdde",
  "Error",
  "Missing Geography Information",
  "2.03.1-2.03.7 - Continuum of Care Information",
  str_squish("Projects should not have missing geography information.
         Please ensure geography information for projects is complete."),
  str_squish("Missing geography information that makes this an error is the 
             project address information like city, state, and ZIP code.")
)
check44 = c("pdde",
            "Error",
            "No Inventory Records",
            "2.07 - Bed and Unit Inventory Information",
            str_squish("Residential projects should have inventory data.
                       Please enter inventory in HMIS for the project(s)."),
            "")

check45 = c(
  "pdde",
  "Error",
  "Project Operating End precedes Inventory End",
  str_squish("2.02.3 - Project Information: Operating Start Date, 3.10 - Project
  Information: Project Entry Date"),
  str_squish("Inventory Start and End dates should be within Project Operating
             Start and End dates. Please update either the inventory dates or
             the Project Operating dates."),
  ""
)

check46 = c(
  "pdde",
  "Error",
  "Missing Tracking Method",
  "2.02.C - Project Information: Emergency Shelter Tracking Method",
  Guidance = str_squish("All Emergency Shelters must have a Tracking Method.
                        Please update the Emergency Shelter Tracking Method
                        field at the project-level."),
  ""
)

check47 = c(
  "pdde",
  "Error",
  "Sum of the dedicated beds should equal the Total Beds",
  "2.07 - Bed and Unit Inventory Information",
  str_squish("Total Beds should match the sum of CH Vets, Youth Vets, Vets, 
         CH Youth, Youth, CH, and Other beds. Please review project inventory
         records for the number of dedicated beds and ensure this number equals
         the Total Beds listed within each record."),
  ""
)


# check48 = c("file structure", 
#             "Error", 
#             "Nulls not allowed in this column", 
#             str_squish("Specific data elements will be identified in the Detail column of a CoC's Integrity Checks export."),
#             str_squish("Certain columns cannot contain nulls or incompatible data types.
#                     Please review the HMIS CSV Format Specifications for the data types and null requirements associated 
#                     with the file and column listed in the detail and make the necessary updates."),
#             str_squish("If the data type is considered High Priority, this will be a High Priority issue. 
#                        Otherwise, this will be considered a General Error."))

check49 = c("file structure", 
            "Error", 
            "Incorrect Date Format", 
            "Date-Based Data Elements",
            str_squish("Dates in the HMIS CSV Export should be in yyyy-mm-dd or
      yyyy-mm-dd hh:mm:ss format, in alignment with the HMIS CSV Format
      Specifications. Please check the Specifications for the file and column
      identified in the Detail and ensure the correct date format is used in the
      export."),
            str_squish("If the column is considered High Priority, this will be
                       a High Priority issue. Otherwise, this will be considered
                       a General Error."))

check50 = c("file structure", 
            "Error", 
            "Incorrect Data Type", 
            str_squish("Specific data elements will be identified in the Detail
                       column of a CoC's Integrity Checks export."),
            str_squish("Data types must align with the HMIS CSV Format
                       Specifications. Please review the specifications for the
                       data types associated with the file and column liseted in
                       the Detail and make the necessary updates."),
            "")

check51 = c("file structure", 
            "Error", 
            "ExportID mismatch", 
            "ExportID",
            str_squish("Per the HMIS CSV Formatting Specifications, the
                       ExportID in your Export and Client files must match.
                       There should be one unique ExportID that will be used to
                       identify all CSV files genereated as part of the same
                       export process."),
            "")

check52 = c("file structure", 
            "Error", 
            "Invalid value in Client file", 
            "3.01-3.07, V1.1-V1.11",
            str_squish("All columns in the client file should contain only the
                       values listed in the HMIS CSV Format Specifications for
                       that specific column. Please review the Specifications for
                       the column identified in the Detail and ensure all values
                       in the export align with the associated values list found
                       in 'Appendix B - Lists' of the Specifications."),
            "")

check53 = c("file structure",
            "Error",
            "Invalid Disabling Condition",
            "3.01.5 - Name: Name Data Quality",
            str_squish("DisablingCondition should only have valid values. Please
                       review the HMIS CSV Format Specifications for 
                       DisablingCondition and ensure all values in the export
                       align with the associated values list found in 'Appendix
                       B - Lists' of the Specifications."),
            "")

check54 = c(
  "file structure",
  "Error",
  "Invalid Living Situation value",
  "3.917.1 - Prior Living Situation: Living Situation",
  str_squish("LivingSituation may only contain valid values. Please review the
             HMIS CSV Format Specifications for LivingSituation and ensure all
             values in the export align with the associated values list found in
             'Appendix B - Lists' of the Specifications."),
  ""
)

check55 = c(
  "file structure",
  "Error",
  "Invalid RelationshipToHoH value",
  "3.15 - Relationship to Head of Household",
  str_squish("RelationshipToHoH must be a valid value. Please review the HMIS
             CSV Format Specifications for RelationshipToHoH and ensure all
             values in the export align with the associated values list found in
             'Appendix B - Lists' of the Specifications."),
  ""
)

check56 = c("file structure",
            "Error",
            "Invalid Destination value",
            "3.12 - Destination",
            str_squish("Destination values must be valid. Please review the HMIS
                       CSV Format Specifications for Destination and ensure all
                       values in the export align with the associated values list
                       found in 'Appendix B - Lists' of the Specifications."),
            "")

check57 = c(
  "file structure",
  "Error",
  "Non-standard Current Living Situation",
  "4.12 - Current Living Situation",
  str_squish("This column contains a value that may have been retired from an
             old version of the Data Standards or was miskeyed. Please review the
             HMIS CSV Format Specifications for CurrentLivingSituation and ensure
             all values in the export align with the associated values list found
             in 'Appendix B - Lists' of the Specifications."),
  ""
)


check58 = c(
  "dq",
  "Warning",
  "Don't Know/Refused Discharge Status",
  "V1.12 - Veteran's Information: Discharge Status",
  guidance_dkr_data,
  ""
)

check59 = c(
  "dq",
  "Warning",
  "Don't Know/Refused War(s)",
  "V1.3-V1.10 - Veteran's Information: Theaters of Operations",
  guidance_dkr_data,
  ""
)

check60 = c(
  "dq",
  "Warning",
  "Don't Know/Refused Military Branch",
  "V1.11 - Veteran's Information: Branch of the Military",
  guidance_dkr_data,
  ""
)

check61 = c("dq",
            "Warning",
            "Don't Know/Refused Destination",
            "3.12 - Destination",
            guidance_dkr_data,
            "")

check62 = c("dq",
            "Warning",
            "Don't Know/Refused/Data Not Collected DOB",
            "3.03.1 - Date of Birth",
            guidance_dkr_data,
            "")

check63 = c(
  "dq",
  "Warning",
  "Don't Know/Refused Months or Times Homeless",
  str_squish("3.917.4 - Prior Living Situation: Times Homeless in Past 3 Years,
             3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years"),
  guidance_dkr_data,
  "")

check64 = c("dq",
            "Warning",
            "Don't Know/Refused Ethnicity",
            "3.05 - Ethnicity",
            guidance_dkr_data,
            "")

check65 = c("dq", 
            "Warning", 
            "Don't Know/Refused Race", 
            "3.04 - Race",
            guidance_dkr_data,
            "")

check66 = c(
  "dq",
  "Warning",
  "Don't Know/Refused Residence Prior",
  "3.917.1 - Prior Living Situation: Living Situation",
  guidance_dkr_data,
  ""
)

check67 = c("dq", 
            "Warning", 
            "Don't Know/Refused Gender", 
            "3.06 - Gender",
            guidance_dkr_data,
            "")

check68 = c("dq",
            "Warning",
            "Don't Know/Refused Veteran Status",
            "3.07 - Veteran Status",
            guidance_dkr_data,
            "")

check69 = c("dq",
            "Warning",
            "Don't Know/Refused SSN",
            "3.02 - Social Security Number",
            guidance_dkr_data,
            "")

check70 = c("dq",
            "Warning",
            "Don't Know/Refused Living Situation",
            "3.917 - Prior Living Situation",
            guidance_dkr_data,
            "")

check71 = c(
  "dq",
  "Warning",
  "Homelessness Start Date Later Than Entry",
  str_squish("3.10 - Project Start Date, 3.917.3 - Prior Living Situation: Date
  Homelessness Started"),
  str_squish("This client has an Approximate Date Homelessness Started in their 
        enrollment that is after their Project Start Date. The information 
        at Project Start should reflect the client's situation at the point of 
        Project Start, so this date may have been incorrectly entered."),
  ""
)

check72 = c(
  "dq",
  "Warning",
  "Number of Months Homeless Can Be Determined",
  "3.10 - Project Start Date, 3.917.3 - Prior Living Situation: Date 
  Homelessness Started",
  str_squish("According to this client's assessment at Project Start, they 
        experienced a single episode of homelessness in the three years prior to 
        their Project Start and the approximate date homelessness started is known, 
        but there was no response entered for the total number of months they 
        experienced homelessness prior to this enrollment. It should be possible 
        to determine and enter the total number of months they experienced
        homelessness based on the Approximate Date Homelessness Started and the 
        Project Start Date."),
  ""
)

check73 = c(
  "dq",
  "Warning",
  "Invalid Homelessness Start Date/Number of Months Homeless",
  "3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years",
  str_squish(
    "According to this client's assessment at Project Start, they experienced 
        a single episode of homelessness in the three years prior to their 
        enrollment and the approximate date homelessness started known, but the 
        total number of months they experienced homelessness prior to this 
        enrollment is inconsistent with the given dates. Please double-check this 
        information for consistency and accuracy."),
  ""
)

check74 = c("dq",
            "Warning",
            "Possible Missed Move-In Date",
            "3.20 - Housing Move-In Date",
            str_squish(
              "This enrollment may be missing a Move-In Date. It is being
              flagged because the length of time since the enrollment date is in
              the top 1-2% for this project type. Please be sure this household
              is still awaiting housing in this project and if not, record the
              date they either moved into housing or exited your project. If
              they are still awaiting housing, do not change the data."),
      str_squish("1% if project type in PSH, PH - Housing Only, or PH - Housing
                 with Services and 2% for all other project types."))

check75 = c("dq",
            "Warning",
            "Possible Missed Exit Date",
            "3.11 - Project Exit Date",
            str_squish("This enrollment may be missing an Exit Date. It is being
                       flagged because the length of time since the enrollment
                       date is in the top 1-2% for this project type. Please be
                       sure this household is still active in the project and if
                       not, record the Project Exit Date. If they are still
                       active, do not change the data."),
      str_squish("1% if project type in PSH, PH - Housing Only, or PH - Housing
                 with Services and 2% for all other project types."))

check76 = c("dq", 
            "Warning", 
            "Missing Destination", 
            "3.12 - Destination",
            guidance_dkr_data,
            "")

check77 = c(
  "dq",
  "Warning",
  "Future Entry Date",
  "3.10 - Project Start Date",
  str_squish(
    "Users should not be entering a client into a project on a
    date in the future. If the Project Start Date is correct, there is no action
    needed, but going forward, please be sure that your data entry workflow
    is correct according to your project type."
  ),
  ""
)

check78 = c(
  "dq",
  "Warning",
  "Entry Precedes Project's Operating Start",
  "2.02.3 - Operating Start Date, 3.10 - Project Start Date",
  guidance_enrl_active_outside_op,
  ""
)

check79 = c("dq",
            "Warning",
            "Project Overlaps",
            "3.10 - Project Start Date, 3.11 - Project Exit Date",
            str_squish(
              "This enrollment overlaps with another enrollment that would
              indicate a household spent the same night in different inventory
              beds. Please review the HMIS Dual Enrollments and HIC Duplicate
              Inventory Training Resource for more information."),
            "")

check80 = c(
  "dq",
  "Warning",
  "Incomplete or Don't Know/Refused Name",
  "3.01.5 - Name: Name Data Quality",
  guidance_dkr_data,
  ""
)


check81 = c("pdde",
            "Warning",
            "Inventory Start Precedes Project Operating Start",
            "2.07 - Bed and Unit Inventory Information, 3.10 - Project Start Date",
            str_squish("Please update either the inventory Start and End dates
                       or the Project Operating dates."),
            ""
)

check82 = c(
  "pdde",
  "Warning",
  "Non-HMIS-Participating project has client-level data",
  "2.02.7 - Project Information: HMIS Particpating Project",
  str_squish("Non-HMIS-Participating projects should not have client-level data.
         The HMIS Participating Project field may need to be updated, new
         projects may need to be created based on changing HMIS participation
         status, or client-level data may need to be removed from the
         Non-HMIS-Participating projects."),
  ""
)

check83 = c(
  "pdde",
  "Warning",
  "Potentially Missing Operating End Date",
  "2.02.4 - Project Information: Operating End Date",
  str_squish("Projects no longer in operation must have an Operating End Date.
         Please verify if the project is still in operation and, if not, add in
         the Operating End Date."),
  ""
)


check84 = c("file structure", 
            "Warning", 
            "Incorrect Columns", 
            str_squish("Specific data elements will be identified in the Detail
                       column of a CoC's Integrity Checks export."),
            str_squish("Your HMIS CSV Export should contain - with identical,
                       case-sensitive spelling - only the columns specified in
                       the columns.csv file. Please remove any extra columns and
                       make sure you have all missing columns."),
            str_squish("If the column is considered High Priority, this will be
                       a High Priority issue. Otherwise, this will be considered
                       a Warning."))

check85 = c("file structure",
            "Error",
            "Duplicate HouseholdIDs",
            "5.09 - Household Identifier",
            str_squish("The householdID must be unique to the household stay in
                       a project; reuse of the identification of the same or
                       similar household upon readmission into the project is
                       unacceptable. Please review the HMIS Data Standards for
                       more details."),
            ""
  
)

### ADD NEW CHECKS ABOVE ^^^ ------------------------------------

#New check? Copy the template below and then paste it above the ADD NEW CHECKS line.
#The check number should be one number more than the last check. If the last check is check84,
#the new check should be named check85. You don't need to keep the Source = , Type = , etc. from the template. 
#Then add the check to dqChecks data frame below.

#check00 = c(Source = "", #This should be "file structure," "pdde," or "dq"
          # Type = "",   #This should be "High Priority," "Error," or "Warning"
          # Issue = "", 
          # DataElement = "",
          # Guidance = "", 
          # Note = "")


dqChecks <- data.frame(rbind(check1,
                       check2,
                       check3,
                       check4,
                       check5,
                       check6,
                       check7,
                       check8,
                       check9,
                       check10,
                       check11,
                       check12,
                       #check13,
                       check14,
                       check15,
                       check16,
                       check17,
                       check18,
                       check19,
                       check20,
                       check21,
                       check22,
                       check23,
                       check24,
                       check25,
                       check26,
                       check27,
                       check28,
                       check29,
                       check30,
                       check31,
                       check32,
                       check33,
                       check34,
                       check35,
                       check36,
                       check37,
                       check38,
                       check39,
                       check40,
                       check41,
                       check42,
                       check43,
                       check44,
                       check45,
                       check46,
                       check47,
                       #check48,
                       check49,
                       check50,
                       check51,
                       check52,
                       check53,
                       check54,
                       check55,
                       check56,
                       check57,
                       check58,
                       check59,
                       check60,
                       check61,
                       check62,
                       check63,
                       check64,
                       check65,
                       check66,
                       check67,
                       check68,
                       check69,
                       check70,
                       check71,
                       check72,
                       check73,
                       check74,
                       check75,
                       check76,
                       check77,
                       check78,
                       check79,
                       check80,
                       check81,
                       check82,
                       check83,
                       check84
))

### ADD NEW CHECKS TO DATAFRAME ABOVE ^^^ ------------------
### RUN THE LINES OF CODE BELOW TO PRODUCE EXCEL EXPORT-----

#Data frames separated out by Source
dqChecks_integrity <- dqChecks %>%
  filter(Source == "file structure")

dqChecks_dq <- dqChecks %>%
  filter(Source == "dq")

dqChecks_pdde <- dqChecks %>%
  filter(Source == "pdde")

#Make a list of dfs to create tabs in export
EvaChecks <- list('File Structure' = dqChecks_integrity, 
              'PDDE Checks' = dqChecks_pdde, 
              'DQ Checks' = dqChecks_dq, 
              "All Checks" = dqChecks)

#Export checks to Excel doc
write.xlsx(EvaChecks, file = here("public_data/EvaChecks.xlsx"))
