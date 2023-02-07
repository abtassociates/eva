#### LIST OF ALL EVA CHECKS #### -------------

#Create empty dataframe for checks

# dqChecks <- data.frame(matrix(ncol = 6, nrow = 0))
# colnames(dqChecks) <- c("Source", "Type", "Issue", "DataElement", "Guidance",  "Notes")

#Will follow a set up like check1 = c(Type = Error, Issue = "something, Guidance = "Do this.", DataElement = ...)

#
#             # HIGH PRIORITY ERRORS
#             ##DataQuality Checks
#             check1 = c("dq", "High Priority", "Duplicate Entries", "3.10 - Project Start Date, 3.11 - Project Exit Date",,)
#             check2 = c("dq", "High Priority", "No Head of Household", "3.15 - Relationship to Head of Household")
#             check3 = c("dq", "High Priority", "Too Many Heads of Household", "3.15 - Relationship to Head of Household")
#             check4 = c("dq", "High Priority", "Missing Relationship to Head of Household", "3.15 - Relationship to Head of Household")
#
#             ## PDDE Checks
#             check5 = c("pdde", "High Priority", "Missing Geography Information", "2.03.8 - Continuum of Care Information: Geography Type")
#
#             ## Integrity Checker
#             check6 = c("integrity", "High Priority", "Incorrect Column Name","")
#             check7 = c("integrity", "High Priority", "Incorrect Data Type","")
#             check8 = c("integrity", "High Priority", "Nulls not allowed in this column","")
#             check9 = c("integrity", "High Priority", "Duplicate PersonalIDs found in the Client file", "5.08 - Personal Identifier")
#             check10 = c("integrity", "High Priority", "Duplicate EnrollmentIDs found in the Enrollment file", "5.06 - Enrollment Identifier")
#             check11 = c("integrity", "High Priority", "Client in the Enrollment file not found in Client file","5.08 - Personal Identifier")
#             check12 = c("integrity", "High Priority", "ProjectID in the Enrollment file not found in Project file", "5.05 - Project Identifier")
#             check13 = c("integrity", "High Priority", "Incorrect Date Format","")
#             check14 = c("integrity", "High Priority", "Incorrect Column Count","")
#             check15 = c("integrity", "High Priority", "Incorrect Column Name","")
#             check16 = c("integrity", "High Priority", "Incorrect Data Type","")
#
#             # GENERAL ERRORS
#             ## Data Quality Checks
#             check17 = c("dq", "Error", "Future Exit Date", "3.11 - Project Exit Date")
#             check18 = c("dq", "Error", "Missing Year Entered Service", "V1.1 - Veteran's Information: Year Entered Military Service")
#             check19 = c("dq", "Error", "Incorrect Year Entered Service","V1.1 - Veteran's Information: Year Entered Military Service")
#             check20 = c("dq", "Error", "Missing Year Separated", "V1.1 - Veteran's Information: Year Separated from Military Service")
#             check21 = c("dq", "Error", "Incorrect Year Separated","V1.1 - Veteran's Information: Year Separated from Military Service")
#             check22 = c("dq", "Error", "Missing War(s)", "V1.3-V1.10 - Veteran's Information: Theaters of Operations")
#             check23 = c("dq", "Error", "Missing Military Branch", "V1.11 - Veteran's Information: Branch of the Military")
#             check24 = c("dq", "Error", "Missing Discharge Status","V1.12 - Veteran's Information: Discharge Status")
#             check25 = c("dq", "Error", "Missing Percent AMI","V4 - Percent of AMI (SSVF Eligibility)")
#             check26 = c("dq", "Error", "Missing VAMC Station Number","V6 - VAMC Station Number")
#             check27 = c("dq", "Error", "Missing Some or All of Last Permanent Address", "V5 - Last Permanent Address")
#             check28 = c("dq", "Error", "Missing HP Screening or Threshold Score", "V7 - HP Targeting Criteria")
#             check29 = c("dq", "Error", "Missing Length of Stay","3.917.2 - Prior Living Situation: length of Stay")
#             check30 = c("dq", "Error", "Missing Client Location","3.16 - Client Location")
#             check31 = c("dq", "Error", "Missing Approximate Date Homeless","3.917.3 - Prior Living Situation: Date Homelessness Started")
#             check32 = c("dq", "Error", "Missing Previously Unsheltered, ES, SH","3.917.2C - Prior Living Situation: Previous Street, ES, SH")
#             check33 = c("dq", "Error", "Missing Residence Prior","3.917.1 - Prior Living Situation: Living Situation")
#             check34 = c("dq", "Error", "Missing Months or Times Homeless","3.917.4 - Prior Living Situation: Times Homeless in Past 3 Years, 3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years")
#             check35 = c("dq", "Error", "Missing Disabling Condition","3.08 - Disabling Condition")
#             check36 = c("dq", "Error", "Missing Name Data Quality", "3.01.5 - Name: Name Data Quality")
#             check37 = c("dq", "Error", "Missing DOB","3.03.1 - Date of Birth")
#             check38 = c("dq", "Error", "Missing DOB Data Quality","3.03.2 - Date of Birth: DOB Data Quality")
#             check39 = c("dq", "Error", "Missing Race", "3.04 - Race")
#             check40 = c("dq", "Error", "Missing Ethnicity", "3.05 - Ethnicity")
#             check41 = c("dq", "Error", "Invalid SSN", "3.02 - Social Security Number")
#             check42 = c("dq", "Error", "Missing Gender", "3.06 - Gender")
#             check43 = c("dq", "Error", "Missing Veteran Status", "3.07 - Veteran Status")
#             check44 = c("dq", "Error", "Invalid Move-In Date", "3.20 - Housing Move-In Date")
#             check60 = c("dq", "Error", "Incomplete Living Situation Data","3.917 - Prior Living Situation")
#
#            ##PDDE Checks
#            check45 = c("pdde", "Error", "Missing Geography Information","2.03.1-2.03.7 - Continuum of Care Information")
#            check46 = c("pdde", "Error", "No Inventory Records", "2.07 - Bed and Unit Inventory Information")
#            check47 = c("pdde", "Error", "Project Operating End precedes Inventory End","2.02.3 - Project Information: Operating Start Date, 3.10 - Project Information: Project Entry Date")
#            check48 = c("pdde", "Error", "Missing Tracking Method", "2.02.C - Project Information: Emergency Shelter Tracking Method")
#            check49 = c("pdde", "Error", "Sum of the dedicated beds should equal the Total Beds","2.07 - Bed and Unit Inventory Information")
#
#            ##Integrity Checker
#            check50 = c("integrity", "Error", "Nulls not allowed in this column","")
#            check51 = c("integrity", "Error", "Incorrect Date Format","")
#            check52 = c("integrity", "Error", "Incorrect Data Type","")
#            check53 = c("integrity", "Error", "ExportID mismatch","")
#            check54 = c("integrity", "Error", "Invalid value in Client file","")
#            check55 = c("integrity", "Error", "Invalid Disabling Condition","3.01.5 - Name: Name Data Quality")
#            check56 = c("integrity", "Error", "Invalid Living Situation value","3.917.1 - Prior Living Situation: Living Situation")
#            check57 = c("integrity", "Error", "Invalid RelationshipToHoH value","3.15 - Relationship to Head of Household")
#            check58 = c("integrity", "Error", "Invalid Destination value", "3.12 - Destination")
#            check59 = c("integrity", "Error", "Non-standard Current Living Situation", "4.12 - Current Living Situation")
#
#            # WARNINGS
#            ##Data Quality Checks
#            check61 = c("dq", "Warning", "Don't Know/Refused Discharge Status","V1.12 - Veteran's Information: Discharge Status")
#            check62 = c("dq", "Warning", "Don't Know/Refused War(s)","V1.3-V1.10 - Veteran's Information: Theaters of Operations")
#            check63 = c("dq", "Warning", "Don't Know/Refused Military Branch","V1.11 - Veteran's Information: Branch of the Military")
#            check64 = c("dq", "Warning", "Don't Know/Refused Destination","3.12 - Destination")
#            check65 = c("dq", "Warning", "Don't Know/Refused/Data Not Collected DOB","3.03.1 - Date of Birth")
#            check66 = c("dq", "Warning", "Don't Know/Refused Months or Times Homeless","3.917.4 - Prior Living Situation: Times Homeless in Past 3 Years, 3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years")
#            check67 = c("dq", "Warning", "Don't Know/Refused Ethnicity","3.05 - Ethnicity")
#            check68 = c("dq", "Warning", "Don't Know/Refused Race","3.04 - Race")
#            check69 = c("dq", "Warning", "Don't Know/Refused Residence Prior","3.917.1 - Prior Living Situation: Living Situation")
#            check70 = c("dq", "Warning", "Don't Know/Refused Gender","3.06 - Gender")
#            check71 = c("dq", "Warning", "Don't Know/Refused Veteran Status","3.07 - Veteran Status")
#            check72 = c("dq", "Warning", "Don't Know/Refused SSN","3.02 - Social Security Number")
#            check73 = c("dq", "Warning", "Don't Know/Refused Living Situation","3.917 - Prior Living Situation")
#            check74 = c("dq", "Warning", "Homelessness Start Date Later Than Entry","3.10 - Project Start Date, 3.917.3 - Prior Living Situation: Date Homelessness Started")
#            check75 = c("dq", "Warning", "Number of Months Homeless Can Be Determined","3.10 - Project Start Date, 3.917.3 - Prior Living Situation: Date Homelessness Started")
#            check76 = c("dq", "Warning", "Invalid Homelessness Start Date/Number of Months Homeless","3.917.5 - Prior Living Situation: Months Homeless in Past 3 Years")
#            check77 = c("dq", "Warning", "Possible Missed Move-In Date","3.20 - Housing Move-In Date")
#            check78 = c("dq", "Warning", "Possible Missed Exit Date","3.11 - Project Exit Date")
#            check79 = c("dq", "Warning", "Missing Destination","3.12 - Destination")
#            check80 = c("dq", "Warning", "Future Entry Date","3.10 - Project Start Date")
#            check81 = c("dq", "Warning", "Entry Precedes Project's Operating Start","2.02.3 - Operating Start Date, 3.10 - Project Start Date")
#            check82 = c("dq", "Warning", "Project Overlaps","3.10 - Project Start Date, 3.11 - Project Exit Date")
#            check83 = c("dq", "Warning", "Incomplete or Don't Know/Refused Name","3.01.5 - Name: Name Data Quality")
#
#            ##PDDE Checks
#            check84 = c("pdde", "Warning", "Inventory Start Precedes Project Operating Start",)
#            check85 = c("pdde", "Warning", "Non-HMIS-Participating project has client-level data","2.02.7 - Project Information: HMIS Particpating Project")
#            check86 = c("pdde", "Warning", "Potentially Missing Operating End Date","2.02.4 - Project Information: Operating End Date")
#
#            ##Integrity Checker
#            check87 = c("integrity", "Warning", "Incorrect Column Name","")

# All checks regardless of upload with added column(s)

dqChecks <- dq_main %>%
  select(Type, Issue, Guidance) %>%
  mutate(Type = factor(Type, levels = c("High Priority",
                                        "Error",
                                        "Warning")),

         #Create a new HMIS Data Element column for each check
         DataElement = case_when(
           #UDE Checks
           Issue %in% c("Invalid SSN",
                        "Don't Know/Refused SSN") ~ "3.02 - Social Security Number",
           Issue == "Missing Length of Stay" ~ "3.917.2 - Prior Living Situation",
           Issue %in% c("Incomplete Living Situation Data",
                        "Don't Know/Refused Living Situation") ~ "3.917 - Prior Living Situation",
           Issue == "Missing Client Location" ~ "3.16 - Client Location",
           Issue == "Missing Approximate Date Homeless" ~ "3.917.3 - Prior Living Situation",
           Issue == "Missing Previously Unsheltered, ES, SH" ~ "3.917.2c - Prior Living Situation",
           Issue %in% c("Missing Residence Prior",
                        "Don't Know/Refused Residence Prior") ~ "3.917.1 - Living Situation",
           Issue %in% c("Missing Months or Times Homeless",
                        "Don't Know/Refused Months or Times Homeless") ~ "3.917.4, 3.917.5 - Prior Living Situation",
           Issue == "Missing Disabling Condition" ~ "3.08 - Disabling Condition",
           Issue %in% c("Missing Name Data Quality",
                        "Incomplete or Don't Know/Refused Name") ~ "3.01.5 - Name",
           Issue == "Missing DOB" ~ "3.03.1 - Date of Birth",
           Issue %in% c("Missing DOB Data Quality",
                        "Don't Know/Refused/Data Not Collected DOB") ~ "3.03.2 - Date of Birth",
           Issue %in% c("Missing Race",
                        "Don't Know/Refused Race") ~ "3.04 - Race",
           Issue %in% c("Missing Ethnicity",
                        "Don't Know/Refused Ethnicity") ~ "3.05 - Ethnicity",
           Issue %in% c("Missing Gender",
                        "Don't Know/Refused Gender") ~ "3.06 - Gender",
           Issue %in% c("Missing Veteran Status",
                        "Don't Know/Refused Veteran Status") ~ "3.07 - Veteran Status",
           Issue %in% c("Missing Destination",
                        "Don't Know/Refused Destination") ~ "3.12 - Destination",
           Issue %in% c("No Head of Household",
                        "Too Many Heads of Household",
                        "Missing Relationship to Head of Household") ~ "3.15 - Relationship to Head of Household",
           Issue %in% c("Invalid Move-In Date",
                        "Possible Missed Move-In Date") ~ "3.20 - Housing Move-In Date",

           #SSVF Checks
           Issue %in% c("Missing Year Entered Service",
                        "Incorrect Year Entered Service") ~ "V1.1 - Year Entered Military Service",
           Issue %in% c("Missing Year Separated",
                        "Incorrect Year Separated") ~ "V1.2 - Year Separated from Military Service",
           Issue %in% c("Missing War(s)",
                        "Don't Know/Refused War(s)") ~ "V1.3-V1.10 - Theater of Operations",
           Issue %in% c("Missing Military Branch",
                        "Don't Know/Refused Military Branch") ~ "V1.11 - Branch of the Military",
           Issue %in% c("Missing Discharge Status",
                        "Don't Know/Refused Discharge Status") ~ "V1.12 - Discharge Status",
           Issue == "Missing Percent AMI" ~ "V4 - Percent of AMI (SSVF Eligibility)",
           Issue == "Missing VAMC Station Number" ~ "V6 - VAMC Station Number",
           Issue == "Missing Some or All of Last Permanent Address" ~ "V5 - Last Permanent Address",
           Issue == "Missing HP Screening or Threshold Score" ~ "V7 - HP Targeting Criteria",

           #Other Program Specific Data Element Checks
           Issue %in% c("Income Missing at Entry",
                        "Income Missing at Exit",
                        "Conflicting Income yes/no at Entry",
                        "Conflicting Income yes/no at Exit") ~ "4.02 - Income and Sources",
           Issue %in% c("Health Insurance Missing at Entry",
                        "Health Insurance Missing at Exit",
                        "Conflicting Health Insurance yes/no at Entry",
                        "Conflicting Health Insurance yes/no at Exit") ~ "4.04 - Health Insurance",
           Issue %in% c("Non-cash Benefits Missing at Entry",
                        "Conflicting Non-cash benefits yes/no at Entry") ~ "4.03 - Non-Cash Benefits",

           #Enrollment Checks
           Issue == "Incorrect DOB or Entry Date" ~ "3.03 - Date of Birth, 3.10 - Project Start Date",
           Issue == "Oldest Household Member Under 12" ~ "3.03 - Date of Birth, 5.09 Household Identifier",
           Issue == "Duplicate Entries" ~ "3.10 - Project Start Date, 3.11 - Project Exit Date",
           Issue == "Entry Precedes Project's Operating Start" ~ "2.02.3 - Operating Start Date, 3.10 - Project Entry Date",
           Issue == "Oldest Household Member Under 12" ~ "3.03 Date of Birth, 5.09 - Household Identifier",
           Issue == "Exit After Project's Operating End Date" ~ "2.02.4 - Operating End Date, 3.11 - Project Exit Date",
           Issue == "Homelessness Start Date Later Than Entry" ~ "3.10 - Project Start Date, 3.917.3 - Prior Living Situation",
           Issue == "Invalid Homelessness Start Date/Number of Months Homeless" ~ "3.10 - Project Start Date, 3.917.3 - Date Homelessness Started, 3.917.4 - Times Homeless in Past 3 Years",
           Issue %in% c("Future Project Start Date",
                        "Future Entry Date") ~ "3.10 - Project Start Date",
           Issue %in% c("Future Project Exit Date",
                        "Project Exit Before Start",
                        "Possible Missed Exit Date") ~ "3.11 - Project Exit Date",
           Guidance == "This enrollment overlaps with another enrollment that would indicate a
      household spent the same night in different inventory beds. Please review
      the HMIS Dual Enrollments and HIC Duplicate Inventory Training Resource for
      more information." ~ "3.10 - Project Start Date, 3.11 - Project Exit Date"
         )) %>%
  arrange(Type, Issue) %>%
  unique() %>%
  select(Type, Issue, Guidance, DataElement)
