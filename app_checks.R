#### LIST OF ALL EVA CHECKS #### -------------

#Create empty dataframe for checks

# dqChecks <- data.frame(matrix(ncol = 6, nrow = 0))
# colnames(dqChecks) <- c("Source", "Type", "Issue", "DataElement", "Guidance",  "Notes")

#Will follow a set up like check1 = c(Type = Error, Issue = "something, Guidance = "Do this.", DataElement = ...)

#
            # HIGH PRIORITY ERRORS
            ##DataQuality Checks
check1 = c(
  Source = "dq",
  Type = "High Priority",
  Issue = "Duplicate Entries",
  DataElement = "3.10 - Project Start Date, 3.11 - Project Exit Date",
  Guidance = str_squish(
    "A client cannot have two enrollments with the same entry date
                                             into the same project. These are duplicate enrollment records.
                                             Please address this issue."
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


## PDDE Checks
check5 = c(
  "pdde",
  "High Priority",
  "Missing Geography Information",
  "2.03.8 - Continuum of Care Information: Geography Type",
  str_squish(
    "Projects should not have missing geography information.
                                  Please ensure geography information for projects is complete."),
  ""
)

## Integrity Checker
check6 = c("integrity", "High Priority", "Incorrect Column Name", "")

check7 = c("integrity", "High Priority", "Incorrect Data Type", "")

check8 = c("integrity",
           "High Priority",
           "Nulls not allowed in this column",
           "")

check9 = c(
  "integrity",
  "High Priority",
  "Duplicate PersonalIDs found in the Client file",
  "5.08 - Personal Identifier"
)
check10 = c(
  "integrity",
  "High Priority",
  "Duplicate EnrollmentIDs found in the Enrollment file",
  "5.06 - Enrollment Identifier"
)
check11 = c(
  "integrity",
  "High Priority",
  "Client in the Enrollment file not found in Client file",
  "5.08 - Personal Identifier"
)
check12 = c(
  "integrity",
  "High Priority",
  "ProjectID in the Enrollment file not found in Project file",
  "5.05 - Project Identifier"
)
check13 = c("integrity", "High Priority", "Incorrect Date Format", "")

check14 = c("integrity", "High Priority", "Incorrect Column Count", "")

check15 = c("integrity", "High Priority", "Incorrect Column Name", "")

check16 = c("integrity", "High Priority", "Incorrect Data Type", "")

# GENERAL ERRORS
## Data Quality Checks
check17 = c(
  "dq",
  "Error",
  "Future Exit Date",
  "3.11 - Project Exit Date",
  str_squish(
    "This client's Exit Date is a date in the future. Please
                                   enter the exact date the client left your program. If this client has not
                                   yet exited, delete the Exit and then enter the Exit Date once the client
                                   is no longer in your program."
  ),
  ""
)

check18 = c(
  "dq",
  "Error",
  "Missing Year Entered Service",
  "V1.1 - Veteran's Information: Year Entered Military Service",
  guidance_missing_at_entry,
  ""
)

check19 = c(
  "dq",
  "Error",
  "Incorrect Year Entered Service",
  "V1.1 - Veteran's Information: Year Entered Military Service",
  guidance_missing_at_entry,
  ""
)

check20 = c(
  "dq",
  "Error",
  "Missing Year Separated",
  "V1.1 - Veteran's Information: Year Separated from Military Service",
  guidance_missing_at_entry,
  ""
)

check21 = c(
  "dq",
  "Error",
  "Incorrect Year Separated",
  "V1.1 - Veteran's Information: Year Separated from Military Service",
  guidance_missing_at_entry,
  ""
)

check22 = c(
  "dq",
  "Error",
  "Missing War(s)",
  "V1.3-V1.10 - Veteran's Information: Theaters of Operations",
  guidance_missing_at_entry,
  ""
)

check23 = c(
  "dq",
  "Error",
  "Missing Military Branch",
  "V1.11 - Veteran's Information: Branch of the Military",
  guidance_missing_at_entry,
  ""
)

check24 = c(
  "dq",
  "Error",
  "Missing Discharge Status",
  "V1.12 - Veteran's Information: Discharge Status",
  guidance_missing_at_entry,
  ""
)

check25 = c(
  "dq",
  "Error",
  "Missing Percent AMI",
  "V4 - Percent of AMI (SSVF Eligibility)",
  guidance_missing_at_entry,
  ""
)

check26 = c(
  "dq",
  "Error",
  "Missing VAMC Station Number",
  "V6 - VAMC Station Number",
  guidance_missing_at_entry,
  ""
)

check27 = c(
  "dq",
  "Error",
  "Missing Some or All of Last Permanent Address",
  "V5 - Last Permanent Address",
  guidance_missing_at_entry,
  ""
)

check28 = c(
  "dq",
  "Error",
  "Missing HP Screening or Threshold Score",
  "V7 - HP Targeting Criteria",
  guidance_missing_at_entry,
  ""
)

check29 = c(
  "dq",
  "Error",
  "Missing Length of Stay",
  "3.917.2 - Prior Living Situation: length of Stay",
  guidance_missing_at_entry,
  ""
)

check30 = c(
  "dq",
  "Error",
  "Missing Client Location",
  "3.16 - Client Location",
  guidance_missing_at_entry,
  ""
)

check31 = c(
  "dq",
  "Error",
  "Missing Approximate Date Homeless",
  "3.917.3 - Prior Living Situation: Date Homelessness Started",
  guidance_missing_at_entry,
  ""
)

check32 = c(
  "dq",
  "Error",
  "Missing Previously Unsheltered, ES, SH",
  "3.917.2C - Prior Living Situation: Previous Street, ES, SH",
  guidance_missing_at_entry,
  ""
)

check33 = c(
  "dq",
  "Error",
  "Missing Residence Prior",
  "3.917.1 - Prior Living Situation: Living Situation",
  guidance_missing_at_entry,
  ""
)

check34 = c(
  "dq",
  "Error",
  "Missing Months or Times Homeless",
  "3.917.4 - Prior Living Situation: Times Homeless in Past 3 Years, 3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years",
  guidance_missing_at_entry,
  ""
)

check35 = c(
  "dq",
  "Error",
  "Missing Disabling Condition",
  "3.08 - Disabling Condition",
  guidance_missing_at_entry,
  ""
)

check36 = c(
  "dq",
  "Error",
  "Missing Name Data Quality",
  "3.01.5 - Name: Name Data Quality",
  guidance_missing_pii,
  ""
)

check37 = c(
  "dq",
  "Error",
  "Missing DOB",
  "3.03.1 - Date of Birth",
  str_squish(
    "This data element is required to be collected at Project
                   Start. Please go to the client's assessment at Project Start
                   to enter this data to HMIS. If this data was not collected,
                   the client declined to provide the information or was unable
                   to provide it, please update the DOB Quality field accordingly."
  ),
  ""
)

check38 = c(
  "dq",
  "Error",
  "Missing DOB Data Quality",
  "3.03.2 - Date of Birth: DOB Data Quality",
  str_squish(
    "This data element is required to be collected at Project
                   Start. Please go to the client's assessment at Project Start
                   to enter this data to HMIS. If this data was not collected,
                   the client declined to provide the information or was unable
                   to provide it, please update the DOB Quality field accordingly."
  ),
  ""
)

check39 = c("dq",
            "Error",
            "Missing Race",
            "3.04 - Race",
            guidance_missing_at_entry,
            "")

check40 = c("dq",
            "Error",
            "Missing Ethnicity",
            "3.05 - Ethnicity",
            guidance_missing_at_entry,
            "")

#check41 = c("dq", "Error", "Invalid SSN", "3.02 - Social Security Number")

check42 = c("dq",
            "Error",
            "Missing Gender",
            "3.06 - Gender",
            guidance_missing_at_entry,
            "")

check43 = c(
  "dq",
  "Error",
  "Missing Veteran Status",
  "3.07 - Veteran Status",
  guidance_missing_pii,
  ""
)

check44 = c(
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

check60 = c(
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

##PDDE Checks
check45 = c(
  "pdde",
  "Error",
  "Missing Geography Information",
  "2.03.1-2.03.7 - Continuum of Care Information"
)
check46 = c("pdde",
            "Error",
            "No Inventory Records",
            "2.07 - Bed and Unit Inventory Information")
check47 = c(
  "pdde",
  "Error",
  "Project Operating End precedes Inventory End",
  "2.02.3 - Project Information: Operating Start Date, 3.10 - Project Information: Project Entry Date"
)
check48 = c(
  "pdde",
  "Error",
  "Missing Tracking Method",
  "2.02.C - Project Information: Emergency Shelter Tracking Method"
)
check49 = c(
  "pdde",
  "Error",
  "Sum of the dedicated beds should equal the Total Beds",
  "2.07 - Bed and Unit Inventory Information"
)

##Integrity Checker
check50 = c("integrity", "Error", "Nulls not allowed in this column", "")
check51 = c("integrity", "Error", "Incorrect Date Format", "")
check52 = c("integrity", "Error", "Incorrect Data Type", "")
check53 = c("integrity", "Error", "ExportID mismatch", "")
check54 = c("integrity", "Error", "Invalid value in Client file", "")
check55 = c("integrity",
            "Error",
            "Invalid Disabling Condition",
            "3.01.5 - Name: Name Data Quality")
check56 = c(
  "integrity",
  "Error",
  "Invalid Living Situation value",
  "3.917.1 - Prior Living Situation: Living Situation"
)
check57 = c(
  "integrity",
  "Error",
  "Invalid RelationshipToHoH value",
  "3.15 - Relationship to Head of Household"
)
check58 = c("integrity",
            "Error",
            "Invalid Destination value",
            "3.12 - Destination")
check59 = c(
  "integrity",
  "Error",
  "Non-standard Current Living Situation",
  "4.12 - Current Living Situation"
)

# WARNINGS
##Data Quality Checks
check61 = c(
  "dq",
  "Warning",
  "Don't Know/Refused Discharge Status",
  "V1.12 - Veteran's Information: Discharge Status"
)
check62 = c(
  "dq",
  "Warning",
  "Don't Know/Refused War(s)",
  "V1.3-V1.10 - Veteran's Information: Theaters of Operations"
)
check63 = c(
  "dq",
  "Warning",
  "Don't Know/Refused Military Branch",
  "V1.11 - Veteran's Information: Branch of the Military"
)
check64 = c("dq",
            "Warning",
            "Don't Know/Refused Destination",
            "3.12 - Destination")
check65 = c("dq",
            "Warning",
            "Don't Know/Refused/Data Not Collected DOB",
            "3.03.1 - Date of Birth")
check66 = c(
  "dq",
  "Warning",
  "Don't Know/Refused Months or Times Homeless",
  "3.917.4 - Prior Living Situation: Times Homeless in Past 3 Years, 3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years"
)
check67 = c("dq",
            "Warning",
            "Don't Know/Refused Ethnicity",
            "3.05 - Ethnicity")
check68 = c("dq", "Warning", "Don't Know/Refused Race", "3.04 - Race")
check69 = c(
  "dq",
  "Warning",
  "Don't Know/Refused Residence Prior",
  "3.917.1 - Prior Living Situation: Living Situation"
)
check70 = c("dq", "Warning", "Don't Know/Refused Gender", "3.06 - Gender")
check71 = c("dq",
            "Warning",
            "Don't Know/Refused Veteran Status",
            "3.07 - Veteran Status")
check72 = c("dq",
            "Warning",
            "Don't Know/Refused SSN",
            "3.02 - Social Security Number")
check73 = c("dq",
            "Warning",
            "Don't Know/Refused Living Situation",
            "3.917 - Prior Living Situation")
check74 = c(
  "dq",
  "Warning",
  "Homelessness Start Date Later Than Entry",
  "3.10 - Project Start Date, 3.917.3 - Prior Living Situation: Date Homelessness Started"
)
check75 = c(
  "dq",
  "Warning",
  "Number of Months Homeless Can Be Determined",
  "3.10 - Project Start Date, 3.917.3 - Prior Living Situation: Date Homelessness Started"
)
check76 = c(
  "dq",
  "Warning",
  "Invalid Homelessness Start Date/Number of Months Homeless",
  "3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years"
)
check77 = c("dq",
            "Warning",
            "Possible Missed Move-In Date",
            "3.20 - Housing Move-In Date")
check78 = c("dq",
            "Warning",
            "Possible Missed Exit Date",
            "3.11 - Project Exit Date")
check79 = c("dq", "Warning", "Missing Destination", "3.12 - Destination")
check80 = c("dq", "Warning", "Future Entry Date", "3.10 - Project Start Date")
check81 = c(
  "dq",
  "Warning",
  "Entry Precedes Project's Operating Start",
  "2.02.3 - Operating Start Date, 3.10 - Project Start Date"
)
check82 = c("dq",
            "Warning",
            "Project Overlaps",
            "3.10 - Project Start Date, 3.11 - Project Exit Date")
check83 = c(
  "dq",
  "Warning",
  "Incomplete or Don't Know/Refused Name",
  "3.01.5 - Name: Name Data Quality"
)

##PDDE Checks
check84 = c("pdde",
            "Warning",
            "Inventory Start Precedes Project Operating Start",
)
check85 = c(
  "pdde",
  "Warning",
  "Non-HMIS-Participating project has client-level data",
  "2.02.7 - Project Information: HMIS Particpating Project"
)
check86 = c(
  "pdde",
  "Warning",
  "Potentially Missing Operating End Date",
  "2.02.4 - Project Information: Operating End Date"
)

##Integrity Checker
check87 = c("integrity", "Warning", "Incorrect Column Name", "")

