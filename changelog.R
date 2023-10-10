output$changelog <- renderTable({
  tribble(
    ~Date, ~Change,
    "10-12-2023", "Added 2 new PDDE issues: Overlapping HMIS Participation
    records and Overlapping CE Participation records",
    
    "10-02-2023", "Added a High Priority File Structure issue that requires the
    user to upload an HMIS CSV Export that is compliant with the FY 2024
    specifications",
    
    "10-02-2023", "Added \'Missing Destination Subsidy\' check",
    
    "10-02-2023", "Added \'Enrollment After Participating Period\' check",
    
    "10-02-2023", "Added \'Enrollment Crosses Participating Period\' check",
    
    "10-02-2023", "Added \'Enrollment Before Participating Period\' check",
    
    "10-02-2023", "Added \'Enrollment Crosses Participating End\' check",
    
    "10-02-2023", "Added \'Enrollment Crosses Participating Start\' check",
    
    "10-02-2023", "Added \'Enrollment After Operating Period\' check",
    
    "10-02-2023", "Added \'Enrollment Crosses Operating Period\' check",
    
    "10-02-2023", "Added \'Enrollment Before Operating Period\' check",
    
    "10-02-2023", "Added \'Missing Destination Subsidy\' check",
    
    "10-02-2023", "Added \'RRH No SubType\' PDDE check",
    
    "10-02-2023", "Changed \'Missing Client Location\' to \'Missing Enrollment
    CoC\'",
    
    "10-02-2023", "Changed \'Exit After Project's Operating End Date\' to 
    \'Enrollment Crosses Operating End.\'",
    
    "10-02-2023", "Changed \'Entry Precedes Operating Start Date.\' to
    \'Enrollment Crosses Operating Start.\'",
    
    "10-02-2023", "Removed \'Non-HMIS Participating Discrepancy.\'",
    
    "10-02-2023", "Removed \'Missing Last Permanent Address\' check for SSVF
    enrollments",
    
    "10-02-2023", "Removed \'Missing Tracking Method\' PDDE check",
    
    "10-02-2023", "Added High Priority File Structure issues to the EvaChecks.xlsx
    workbook",
    
    "10-02-2023", "Added initial file check for the CSV Version which separates
    the upload issue \'Missing Files\' into \'You may have uploaded the wrong
    dataset,\' \'Your HMIS CSV Export is out of date,\' and \'Incomplete dataset.\'",
    
    "10-02-2023", "Adjusted data element references to account for new Race/Ethnicity
    and Gender options",
    
    "10-02-2023", "Adjusted HMIS Participating, Tracking Method, and
    EnrollmentCoC logic to fit new structure",
    
    "10-02-2023", "Corrected \'Missing Non-cash Benefits\' and \'Conflicting Non-cash
    Benefits\' logic",
    
    "10-02-2023", "Removed HMIS package from development (no change to user
    experience)",
    
    "10-02-2023", "Updated link to the HMIS CSV specifications on the Home tab in
    the \'Instructions\' box",
    
    "10-02-2023", "Changed the EvaChecks.xlsx document to EvaChecks.csv so that
    the document can be version controlled more transparently",
    
    "09-21-2023", "Added back accidentally removed Incorrect DOB and Missing
    Living Situation checks",
    
    "08-30-2023", "Fixed missed demotion of null CoCCode File Structure Analysis 
    issue in EnrollmentCoC.csv and ProjectCoC.csv to \"Error\" 
    from \"High Priority\"",
    
    "08-30-2023", "Replaced 'Invalid Homelessness Start Date/Number of Months 
    Homeless' data quality check with two checks: 'Homelessness Start Date 
    conflicts with Living Situation Data' and 'Number of Months Homeless conflicts 
    with Living Situation Data' to include cases where Aproximate date homeless 
    is over 3 years ago",
    
    "08-14-2023", "Modified Long Stayer logic so the issue names match the
    terminology in the Local Settings. Prepping the current Long Stayers issues
    to fit with the ones we plan to add in upcoming releases.",
    
    "08-10-2023", "Demoted several null-column File Structure Analysis issues
    to \"Error\" from \"High Priority\"",

    "08-10-2023", "Added missing checks to EvaChecks.xlsx",
    
    "08-10-2023", "Changed to pull issue-related text from EvaChecks.xlsx",
    
    "07-06-2023", "Fix bug when datasets have no projects of a certain status
    (e.g. Active No Move-In)",
    
    "06-28-2023", "Allow nulls in AssessmentLocation, AssessmentType, and
    PrioritizationStatus columns.",

    "06-14-2023", "Added graceful rejection of non-zip files, including 7zip.",

    "05-08-2023", "Fixed typo in DQ High Priority plot display when there were
    no issues of that type.",

    "04-25-2023", "Small updates to the EvaChecks.xlsx file to better capture
    Eva's checks.",
    
    "04-20-2023", "Added Zero Enrollments as a High Priority File Structure issue.",
    
    "04-20-2023", "Upgraded the Duplicate HouseholdIDs File Structure issue from
    Error to High Priority as promised",
    
    "04-06-2023", "Code refactoring, consolidating, reorganizing, and commenting",
    
    "04-06-2023", "Fixed bug where an Org-level DQ file name does not
    necessarilly match the selected organization",
    
    "03-23-2023", "Modified DQ chart colors and unsuccessful upload pop-up text",
    
    "03-23-2023", "Added Client Count Download instructions",
    
    "03-23-2023", "Fixed display of system-level, Top 10 Orgs charts for errors
    and warnings",
    
    "03-23-2023", "Added Zero Utilization to Project Descriptor Data Quality 
    Checker",
    
    "03-09-2023", "Renaming R Scripts for improved organization",
    
    "03-09-2023", "Added console logging to facilitate debugging",
    
    "03-09-2023", "Previously, \"Missing Geography Information\" and 
  \"Missing Address\" had the same Issue Name of \"Missing Geography 
  Information.\" They were split out for clarity.",
  
    "03-02-2023", "Fixed timeout to fully clear data by reloading the session.",
    
    "03-02-2023", "Updated language on home page to match recent update to what
  metadata is being logged by Eva.",
    
    
    "02-23-2023", "Changed Long Stayers (aka Possible Missed Exit) logic so that,
  for Outreach and Coordinated Entry projects, it measures from the last 
  Current Living Situation instead of from the Entry Date.",
    "02-23-2023", "Addresses GitHub issue 152 by adding a Detail column to the
  File Structure Analysis download separate from the more general Guidance. This
  column includes more details about affected rows and column in order to help
  the user identify issues in their data.",
    
    "02-23-2023", "Addresses GitHub issue 154 by checking for missing columns and
  extraneous columns in a similar way. This is a change from the prior issues
  specifying that a column name was misspelled. Now a misspelled column will show
  as one missing column (with the correct column name) and one extraneous column
  (with the actual column name.)",
    
    "02-23-2023", "Addresses GitHub issue 172 by preventing R from counting the
  value of \"NA\" as an actual null.",
    
    "02-09-2023", "Added system-wide download of Client Counts data",
    
    "02-09-2023", "Separated app timeout and crash processing. 
  Timeout triggers a javascript alert and clears the app data. Crashes trigger 
  the gray screen with a message and a Refresh link.",
    
    "02-09-2023", "Added Outstanding Referrals as a Warning. Eva users can set
  what constitutes and outstanding referral for their CoC on the Edit Local
  Settings tab. The issue will show in the download on the Warnings tab and
  on its own tab called Referrals so that end users can see which Referral is
  considered outstanding.",
    
    "01-26-2023", "Addresses GitHub issue 82. Now the app times out after 10
  minutes being idle.",
    
    "01-26-2023", "Addresses GitHub issue 122. Modified tab structure to spread
  things out and simplify the Home tab.",
    
    "01-26-2023", "Addresses GitHub issue 124. Modified plot color for High Priority
  issues.",
    
    "01-23-2023", "Hotfix: Added improved metadata collection for troubleshooting
  purposes.",
    
    "01-13-2023", "Hotfix: Set GrantID field so it is not considered a high
  priority column so that it will no longer cause Eva to reject a file for
  incorrect data type.",
    
    "12-29-2022", "Addresses GitHub issue 118. Eva was not checking that all needed
  csvs were in the export. Now it checks this and rejects the export if they are
  not there.",
    
    "12-29-2022", "Addresses GitHub issue 118. Eva was missing some instances
  where a date variable is of the wrong type (e.g. ymd_hms instead of ymd). Now
  it rejects exports if an important variable has the wrong date type.",  
    
    "12-29-2022", "Client Counts report: if a user makes the Report Date Range so
  that the Start > End, Eva now alerts the user in the data tables to check dates.",
    
    "12-29-2022", "Rewrote PDDE issues' Guidance so that it is general guidance,
  then added Details column to include IDs to help admins find specific issues."
    
  )
  
})
