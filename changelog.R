output$changelog <- renderTable({
  tribble(
    ~Date, ~Change,
    
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