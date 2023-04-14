output$changelog <- renderTable({
  tribble(
    ~Date, ~Change,
    "04-20-2023", "Upgraded the Duplicate HouseholdIDs File Structure issue from
    Error to High Priority as promised",
    
    "04-06-2023", "Code refactoring, consolidating, reorganizing, and commenting",
    
    "04-06-2023", "Fixed bug where an Org-level DQ file name does not necessarilly match the selected organization",
    
    "03-23-2023", "Modified DQ chart colors and unsuccessful upload pop-up text",
    
    "03-23-2023", "Added Client Count Download instructions",
    
    "03-23-2023", "Fixed display of system-level, Top 10 Orgs charts for errors and warnings",
    
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
    
    "01-26-2023", "Addresses GitHub issue 82. Now the app times out after 10 minutes
  being idle.",
    
    "01-26-2023", "Addresses GitHub issue 122. Modified tab structure to spread things
  out and simplify the Home tab.",
    
    "01-26-2023", "Addresses GitHub issue 124. Modified plot color for High Priority
  issues.",
    
    "01-23-2023", "Hotfix: Added improved metadata collection for troubleshooting
  purposes.",
    
    "01-13-2023", "Hotfix: Set GrantID field so it is not considered a high priority column
  so that it will no longer cause Eva to reject a file for incorrect data type.",
    
    "12-29-2022", "Addresses GitHub issue 118. Eva was not checking that all needed
  csvs were in the export. Now it checks this and rejects the export if they are
  not there.",
    
    "12-29-2022", "Addresses GitHub issue 118. Eva was missing some instances where a date
  variable is of the wrong type (e.g. ymd_hms instead of ymd). Now it rejects
  exports if an important variable has the wrong date type.",  
    
    "12-29-2022", "Client Counts report: if a user makes the Report Date Range so
  that the Start > End, Eva now alerts the user in the data tables to check dates.",
    
    "12-29-2022", "Rewrote PDDE issues' Guidance so that it is general guidance,
  then added Details column to include IDs to help admins find specific issues."
    
  )
  
})
