output$changelog <- renderDT({
  tribble(
    ~ Date,
    ~ Change,
    "04-03-2025",
    "<b>New Features</b> <br>
      - Added new PDDE check for active inventory with no enrollments during the inventory dates.<br>
    <b>Bug Fixes</b> <br>
      - Update Zero Utilization check logic to avoid flagging projects with utilization (Issues <a href='https://github.com/abtassociates/eva/issues/677' target='_blank'>#677</a> and <a href='https://github.com/abtassociates/eva/issues/713' target='_blank'>#713</a>).",
    "03-27-2025",
    "<b>Bug Fixes</b> <br>
      - Handle leap years in System Performance charts (Issue <a href='https://github.com/abtassociates/eva/issues/738' target='_blank'>#738</a>)<br>
      - Allow bed type \"Other\" for site based ES projects (Issue <a href='https://github.com/abtassociates/eva/issues/703' target='_blank'>#703</a>)<br>
      - Fixed bug where projects with no inventory records were getting flagged for both the No Inventory Records check and the Project CoC Missing Inventory check. The latter should not be flagged if the former is already flagged.<br>
      - Deduplicate records in the PDDE export.<br>
    <b>Miscellaneous</b> <br>
      - Updated and added application text for clarity on System Performance age.<br>
      - Removed System Performance demographic filter as well as references to filter and related language throughout Eva.",
    "03-06-2025",
    "<b>New Features</b> <br>
      - Added HHType to Overlap export (Issue <a href='https://github.com/abtassociates/eva/issues/697' target='_blank'>#697</a>)<br>
    <b>Bug Fixes</b> <br>
      - Fixed edge case detection of NbN overlaps; i.e., too many duplicates caused a join error and crashed Eva. (Issue <a href='https://github.com/abtassociates/eva/issues/683'>#683</a>)<br>
      - Fixed warning about DateProvided when Services file is empty<br>
      - No longer flagging VSP Projects as having clients when they do not (Issue <a href='https://github.com/abtassociates/eva/issues/678' target='_blank'>#724</a>)<br>
    <b>Miscellaneous</b> <br>
    - Gracefully handle timeouts",
    "02-20-2025",
    "<b>New Features</b> <br>
      - Code speedups particularly around file import and initial processing, as well as Impermissible Character downloads.<br>
     <b>Bug Fixes:</b> <br>
      - Small fix in how FSA issues are reported when an expected column is missing from a file.<br>",
    "02-06-2024",
    "<b>New Features</b> <br>
      - Added client-level export to the System Performance tab to provide transparency
      in the charts<br>
      - Rejecting HMIS zip files containing Export.csv with more than one row. This scenario will be captured as a High-Priority error.<br>
     <b>Bug Fixes:</b> <br>
      - Fixed display of missing geography and address warnings for PDDE.<br>
      - Only flag residential projects for Active Inventory PDDE check. (Issue <a href='https://github.com/abtassociates/eva/issues/678' target='_blank'>#678</a>)<br>
      - Only reference active inventory records to check bed type compatibility with housing type. (Issue <a href='https://github.com/abtassociates/eva/issues/680' target='_blank'>#680</a>)<br>
      - Fixed issue in how the Age and Race filter selections are displayed in the System Exports.<br>
      - Fixed issue in bracket detection and handling of non-UTF8 encodings that could lead to crashing Eva. (Issue <a href='https://github.com/abtassociates/eva/issues/698' target='_blank'>#698</a>)<br>
      - Fixed issue in display of Race filter selections in System Performance exports.<br>
    <b>Miscellanous Changes:</b> <br>
      - Updated System Performance Methodology Type terms to \"Method 1\" and \"Method 2.\" Renamed related demographic columns to align with new Methodology Type terms.",
    "12-31-2024",
    "<b>New features:</b> <br>
      - Updated demo.zip to capture new overlaps.<br>
      - Modified upload-related pop-ups to be more informative. <br>
      - Added Project Type to PDDE export (Issue <a href='https://github.com/abtassociates/eva/issues/641' target='_blank'>#641</a>)<br>
     <b>Bug Fixes:</b> <br>
      - Only flagging Missed Move-In if relationship to HoH is 1 (Issue <a href='https://github.com/abtassociates/eva/issues/650' target='_blank'>#650</a>)<br>
      - Fixed NbN overlap detection. An NbN and an EE overlap if the first and last
      DateProvided for a given enrollment overlaps with an EE enrollment by more 
      than 2 days. Two NbNs overlap if they have duplicate DateProvideds for a given
      PersonalID (Issue <a href='https://github.com/abtassociates/eva/issues/659' target='_blank'>#659</a>)<br>
      - Allow all UTF-8 characters, except for brackets (Issues <a href='https://github.com/abtassociates/eva/issues/649' target='_blank'>#649</a> and <a href='https://github.com/abtassociates/eva/issues/667' target='_blank'>#667</a>)<br>
      - Automatically interpret non-UTF-8 encoded characters to 
      avoid crashing and display correct character (Issues <a href='https://github.com/abtassociates/eva/issues/649' target='_blank'>#649</a> and <a href='https://github.com/abtassociates/eva/issues/667' target='_blank'>#667</a>)<br>
      - Modified Missing Address check to allow for VSPs and tenant-based scattered sites to be missing Address1, City, and State (Issue <a href='https://github.com/abtassociates/eva/issues/631' target='_blank'>#631</a>)",
    "11-19-2024",
    "<b>Bug Fixes:</b> <br>
      - Corrected the Zero Utilization logic to only flag projects that are missing
    enrollments during a project's operating and participating periods. See Issue
    <a href='https://github.com/abtassociates/eva/issues/522' target='_blank'>#522</a>.",
    "11-05-2024",
    "<b>Bug Fixes:</b><br>
      - Fixed handling of '#' symbols in first row of a csv file (
<a href='https://github.com/abtassociates/eva/issues/638' target='_blank'>#638</a>,
<a href='https://github.com/abtassociates/eva/issues/632' target='_blank'>#632</a>,
<a href='https://github.com/abtassociates/eva/issues/535' target='_blank'>#535</a>)<br>
      - Fully reset app after a failed upload",
    "10-23-2024",
    "<b>Bug Fixes:</b> <br>
      - Corrected the Zero Utilization logic to only flag projects that are missing
    enrollments during a project's operating and participating periods. See Issue
    <a href='https://github.com/abtassociates/eva/issues/522' target='_blank'>#522</a>.",
    "10-07-2024",
    "<b>New Features:</b> <br>
      - Launched the System Performance Overview Page. The page contains three new 
    system performance charts: the System Flow Chart, the Client System Status Chart, 
    and the System Demographics Chart. The system performance charts give users flexibility 
    in how they filter and view their community’s homeless response system performance data.<br>
      - Updated how 'Data not collected' is handled when Eva assesses data quality. 
    Eva now classifies data elements marked in the export as 'Data not collected' 
    as a Warning instead of as an Error, to align with Eva's Error and Warning definitions. 
    This approach is aimed at helping HMIS end users focus on correcting fixable 
    issues without changing accurate information. The Warning is meant to remind 
    users to verify if the data might still be collected or if the data exists elsewhere, 
    such as in the client's file. If 'Data not collected' is the correct value, 
    users should leave the value as-is in HMIS. Issues that should be fixed in HMIS 
    are categorized as Errors.<br>
      - Removed logic that looks for nulls in specific data elements when Eva assesses 
    data quality. For the Race/Ethnicity, Veteran Status, Disabling Condition, 
    Name Data Quality, DOB Data Quality, SSN Data Quality, and Destination data 
    elements, the HMIS CSV export does not distinguish between nulls and the value 
    'Data not collected' The issues removed are: Missing Name Data Quality, Missing 
    DOB Data Quality, Missing SSN Data Quality, Missing Veteran 
    Status, Missing Disabling Condition, and Missing Destination. These are now 
    flagged as Warnings and described as 'Unknown Veteran Status,' etc.<br>
    <b>Bug Fixes:</b><br>
      - Improved handling of overlapping enrollments<br>
      - Fixed invalid move-in date (issue <a href='https://github.com/abtassociates/eva/issues/615' target='_blank'>#615</a>)<br>
      - Fixed the way Eva uses Project Participation and groups Projects that change 
    participation across the reporting date range. Addresses issues <a href='https://github.com/abtassociates/eva/issues/521' target='_blank'>#521</a> 
    and <a href='https://github.com/abtassociates/eva/issues/605' target='_blank'>#605</a>.",
    
    "08-01-2024",
    "<b>New Features:</b> <br>
      - Formatted changelog for readability <br>
      - Split processing tasks so that Eva does a quick check for any
      impermissible characters, then if any are found, the user can choose to
      run a more detailed report about the impermissible characters< and their
      precise location in the uploaded file<br>
      <b>Bug Fixes:</b><br>
      - Added Enrollment ID and Project Type to exports (Issues
      <a href='https://github.com/abtassociates/eva/issues/482' target='_blank'>#482</a> and
      <a href='https://github.com/abtassociates/eva/issues/529' target='_blank'>529</a>),<br>
      - Corrected the institutional living situations and temporary living situations
    to align to the HMIS Data Standards. Issue
    <a href='https://github.com/abtassociates/eva/issues/490' target='_blank'>#490</a>",
    
    "05-20-2024",
    "<b>New Features:</b> <br>
      - Added demo mode, where users can try out Eva even if they
      don't have access to an FY 2024 HMIS CSV Export.",
    
    "04-09-2024",
    "<b>Bug Fixes:</b> <br>
      - Moved all variables from global environment to a session
      environment to ensure appropriate visibility. <br>
      - Moved data frames \"Export\" and \"Project0\" from the global
      environment to a session environment to ensure appropriate visibility.",
    
    "04-01-2024",
    "<b>New Features:</b> <br>
      - Added 'Demo Mode', which allows users to play around with a
      test dataset in Eva in order to understand how the tool works without having
      to upload any data. <br>
      - Added loading spinners to make clear when something is loading
       as opposed to frozen.",
    
    "01-24-2024",
    "<b>New Features:</b> <br>
      - If a project has an Operating or Participating end date in
      the future Eva will not flag these enrollments as Enrollment Crosses
      Operating/Participating End errors. <br>
      - Added upload progress display text so it's clear Eva is
      working and not freezing. <br>

      <b>Bug Fixes:</b> <br>
      - Corrected 'No Inventory Records' logic so that it does not flag
      RRH-SSO projects.",
    
    "12-14-2023",
    "<b>New Features:</b> <br>
      - Eva now filters out records in any csv file that has a value
      in the DateDeleted column. <br>

      <b>Bug Fixes:</b> <br>
      - Added check for non-ASCII (i.e. impermissible) characters that
      caused Eva to crash",
    
    "11-30-2023",
    "<b>Bug Fixes:</b> <br>
      - Added filters for income, non-cash, health insurance data
      quality issues so that projects that don't need to collect that, based on
      their funding source and project type are not being false flagged.",
    
    "11-15-2023",
    "<b>Bug Fixes:</b> <br>
      - Corrected date logic in several places.",
    
    "11-09-2023",
    "<b>Bug Fixes:</b> <br>
      - Corrected a few incorrect data types in columns.csv. <br>
      - Fixed EntryDate bug so that EntryDate aligns with the original
      data rather than an adjusted date. <br>
      - Added 'VSP participating in HMIS' as a Data Quality Error. <br>
      - Data Quality Warning 'Zero Utilization' now only looks at
      projects that are set as HMIS Participating. <br>
      - Changed Data Quality Warning 'Future Exit Date' to exclude all
      enrollments without an Exit Date. <br>
      - Corrected language throughout for 'Client Refused' to 'Client
      prefers not to answer' and 'Don't know' to 'Doesn't know.'",
    
    "11-02-2023",
    "<b>Bug Fixes:</b> <br>
      - Fixed issue with overidentifying overlapping CE Participation
      records. <br>
      - Removed slowness warning at the beginning. Slowness seems to
      be limited to Abt machines. <br>
      - Fixed bug where Eva crashed when a demographic column name
      was misspelled in Client.csv. <br>
      - Modified empty File Structure Anaysis results text to be
      more explicit and helpful.",
    
    "10-20-2023",
    "<b>New Features:</b> <br>
      - Added 2 new Data Quality issues: Missing Current Living
      Situation Subsidy Type and Missing Prior Living Situation Subsidy Type and
      updated Missing Destination Subsidy Type to also flag subsidy types that
      are not valid subsidy types, like 99, as an example. <br>
      - Added PDDE issue that flags when an RRH-SO project has active
      inventory. <br>
      - Added 2 new PDDE issues: Overlapping HMIS Participation
      records and Overlapping CE Participation records. <br>
      - Updated columns.csv to the new nullability edits from the
      Data Lab",
    
    "10-02-2023",
    "<b>New Features:</b> <br>
      - Added a High Priority File Structure issue that requires the
      user to upload an HMIS CSV Export that is compliant with the FY 2024
      specifications <br>
      - Added \'Missing Destination Subsidy\' check <br>
      - Added \'Enrollment After Participating Period\' check <br>
      - Added \'Enrollment Crosses Participating Period\' check <br>
      - Added \'Enrollment Before Participating Period\' check <br>
      - Added \'Enrollment Crosses Participating End\' check <br>
      - Added \'Enrollment Crosses Participating Start\' check <br>
      - Added \'Enrollment After Operating Period\' check <br>
      - Added \'Enrollment Crosses Operating Period\' check <br>
      - Added \'Enrollment Before Operating Period\' check <br>
      - Added \'Missing Destination Subsidy\' check <br>
      - Added \'RRH No SubType\' PDDE check <br>
      - Changed \'Missing Client Location\' to \'Missing Enrollment
      CoC\' <br>
      - Changed \'Exit After Project's Operating End Date\' to
      \'Enrollment Crosses Operating End.\' <br>
      - Changed \'Entry Precedes Operating Start Date.\' to
      \'Enrollment Crosses Operating Start.\' <br>
      - Removed \'Non-HMIS Participating Discrepancy.\' <br>
      - Removed \'Missing Last Permanent Address\' check for SSVF
      enrollments <br>
      - Removed \'Missing Tracking Method\' PDDE check <br>
      - Added High Priority File Structure issues to the EvaChecks.xlsx
      workbook <br>
      - Added initial file check for the CSV Version which separates
      the upload issue \'Missing Files\' into \'You may have uploaded the wrong
      dataset,\' \'Your HMIS CSV Export is out of date,\' and \'Incomplete dataset.\' <br>
      - Adjusted data element references to account for new Race/Ethnicity options <br>
      - Adjusted HMIS Participating, Tracking Method, and
      EnrollmentCoC logic to fit new structure <br>
      - Corrected \'Missing Non-cash Benefits\' and \'Conflicting Non-cash
      Benefits\' logic <br>
      - Removed HMIS package from development (no change to user
      experience) <br>
      - Updated link to the HMIS CSV specifications on the Home tab in
      the \'Instructions\' box <br>
      - Changed the EvaChecks.xlsx document to EvaChecks.csv so that
      the document can be version controlled more transparently",
    
    "09-21-2023",
    "<b>Bug Fixes:</b> <br>
      - Added back accidentally removed Incorrect DOB and Missing
      Living Situation checks",
    
    "08-30-2023",
    "<b>Bug Fixes:</b> <br>
      - Fixed missed demotion of null CoCCode File Structure Analysis
      issue in EnrollmentCoC.csv and ProjectCoC.csv to \"Error\"
      from \"High Priority\" <br>

      <b>New Features:</b> <br>
      - Replaced 'Invalid Homelessness Start Date/Number of Months
      Homeless' data quality check with two checks: 'Homelessness Start Date
      conflicts with Living Situation Data' and 'Number of Months Homeless conflicts
      with Living Situation Data' to include cases where Aproximate date homeless
      is over 3 years ago.",
    
    "08-14-2023",
    "<b>Bug Fixes:</b> <br>
      - Modified Long Stayer logic so the issue names match the
      terminology in the Local Settings. Prepping the current Long Stayers issues
      to fit with the ones we plan to add in upcoming releases.",
    
    "08-10-2023",
    "<b>Bug Fixes:</b> <br>
      - Demoted several null-column File Structure Analysis issues
      to \"Error\" from \"High Priority\" <br>
      - Added missing checks to EvaChecks.xlsx <br>

      <b>New Features:</b> <br>
      - Changed to pull issue-related text from EvaChecks.xlsx",
    
    "07-06-2023",
    "<b>Bug Fixes:</b> <br>
      - Fix bug when datasets have no projects of a certain status
      (e.g. Active No Move-In)",
    
    "06-28-2023",
    "<b>New Features:</b> <br>
      - Allow nulls in AssessmentLocation, AssessmentType, and
      PrioritizationStatus columns.",
    
    "06-14-2023",
    "<b>New Features:</b> <br>
      - Added graceful rejection of non-zip files, including 7zip.",
    
    "05-08-2023",
    "<b>Bug Fixes:</b> <br>
      - Fixed typo in DQ High Priority plot display when there were
      no issues of that type.",
    
    "04-25-2023",
    "<b>New Features:</b> <br>
      - Small updates to the EvaChecks.xlsx file to better capture
      Eva's checks.",
    
    "04-20-2023",
    "<b>New Features:</b> <br>
      - Added Zero Enrollments as a High Priority File Structure issue. <br>
      - Upgraded the Duplicate HouseholdIDs File Structure issue from
      Error to High Priority as promised. <br>
      - Code refactoring, consolidating, reorganizing, and commenting",
    
    "04-06-2023",
    "<b>Bug Fixes:</b> <br>
      - Fixed bug where an Org-level DQ file name does not
      necessarilly match the selected organization",
    
    "03-23-2023",
    "<b>Bug Fixes:</b> <br>
      - Modified DQ chart colors and unsuccessful upload pop-up text. <br>
      - Fixed display of system-level, Top 10 Orgs charts for errors
      and warnings. <br>

      <b>New Features:</b> <br>
      - Added Client Count Download instructions. <br>
      - Added Zero Utilization to Project Descriptor Data Quality
      Checker.",
    
    "03-09-2023",
    "<b>New Features:</b> <br>
      - Renaming R Scripts for improved organization <br>
      - Added console logging to facilitate debugging <br>
      - Previously, \"Missing Geography Information\" and
    \"Missing Address\" had the same Issue Name of \"Missing Geography
    Information.\" They were split out for clarity.",
    
    "03-02-2023",
    "<b>Bug Fixes:</b> <br>
      - Fixed timeout to fully clear data by reloading the session. <br>
      - Updated language on home page to match recent update to what
    metadata is being logged by Eva.",
    
    "02-23-2023",
    "<b>New Features:</b> <br>
      - Changed Long Stayers (aka Possible Missed Exit) logic so that,
    for Outreach and Coordinated Entry projects, it measures from the last
    Current Living Situation instead of from the Entry Date. <br>
      - Addresses GitHub issue 152 by adding a Detail column to the
      File Structure Analysis download separate from the more general Guidance. This
      column includes more details about affected rows and column in order to help
      the user identify issues in their data. <br>
      - Addresses GitHub issue 154 by checking for missing columns and
    extraneous columns in a similar way. This is a change from the prior issues
    specifying that a column name was misspelled. Now a misspelled column will show
    as one missing column (with the correct column name) and one extraneous column
    (with the actual column name.) <br>
    - Addresses GitHub issue 172 by preventing R from counting the
    value of \"NA\" as an actual null.",
    
    "02-09-2023",
    "<b>New Features:</b> <br>
      - Added system-wide download of Client Counts data <br>
      - Separated app timeout and crash processing.
    Timeout triggers a javascript alert and clears the app data. Crashes trigger
    the gray screen with a message and a Refresh link. <br>
    - Added Outstanding Referrals as a Warning. Eva users can set
    what constitutes and outstanding referral for their CoC on the Edit Local
    Settings tab. The issue will show in the download on the Warnings tab and
    on its own tab called Referrals so that end users can see which Referral is
    considered outstanding.",
    
    "01-26-2023",
    "<b>New Features:</b> <br>
      - Addresses GitHub issue 82. Now the app times out after 10
    minutes being idle. <br>
    - Addresses GitHub issue 122. Modified tab structure to spread
    things out and simplify the Home tab. <br>
    - Addresses GitHub issue 124. Modified plot color for High Priority
    issues.",
    
    "01-23-2023",
    "<b>Bug Fixes:</b> <br>
      - Hotfix: Added improved metadata collection for troubleshooting
    purposes.",
    
    "01-13-2023",
    "<b>Bug Fixes:</b> <br>
      - Hotfix: Set GrantID field so it is not considered a high
    priority column so that it will no longer cause Eva to reject a file for
    incorrect data type.",
    
    "12-29-2022",
    "<b>Bug Fixes:</b> <br>
      - Addresses GitHub issue 118. Eva was not checking that all needed
    csvs were in the export. Now it checks this and rejects the export if they are
    not there.",
    
    "12-29-2022",
    "<b>Bug Fixes:</b> <br>
      - Addresses GitHub issue 118. Eva was missing some instances
    where a date variable is of the wrong type (e.g. ymd_hms instead of ymd). Now
    it rejects exports if an important variable has the wrong date type.",
    
    "12-29-2022",
    "<b>New Features:</b> <br>
      - Client Counts report: if a user makes the Report Date Range so
    that the Start > End, Eva now alerts the user in the data tables to check dates.",
    
    "12-29-2022",
    "<b>New Features:</b> <br>
      - Rewrote PDDE issues' Guidance so that it is general guidance,
    then added Details column to include IDs to help admins find specific issues."
    
  )
}, rownames = FALSE, escape = FALSE)
