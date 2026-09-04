
# Eva Changelog
## 2026-09-17

### Bug Fixes
 - Fix undercounting of Conflicting Health Insurance DQ checks for records with NA health insurance sources.
 - New comprehensive System Performance export interface.
 - Added new DQ check to flag HoHs that are exited earlier than other household members in an enrollment. (Issue <a href='https://github.com/abtassociates/eva/issues/1061' target ='_blank'>#1061</a>)

## 2026-09-03

### Bug Fixes
 - Remove enrollments with no Bed Nights for 'Bed night entered on Project Exit Date' check (Issue <a href='https://github.com/abtassociates/eva/issues/1031' target='_blank'>#1031</a>)
 - Move HouseholdType filter on Exits by Subpopulation page to the chart selector section (Issue <a href='https://github.com/abtassociates/eva/issues/1023' target='_blank'>#1023</a>)
 - Added new DQ check "Enrollment During Non-Participation Period" with updated code that handles multiple HMIS Participation records with more nuance and retired DQ checks "Enrollment After HMIS Participating Period" and "Enrollment Before HMIS Participating Period"
 
### New Features
 - Added DQ Timeliness metrics for Coordinated Entry Assessment and Event records on the Project Dashboard Report
 
### Miscellaneous
 - Updated language in the System Performance Glossary

## 2026-08-03

### Bug Fixes 
 - Fixed System Exits Client-level Download functionality. It was causing an error when writing the output to an Excel file.
 - Updated Enrollment DQ Checks to only show warnings for actively HMIS participating enrollments. (Issues <a href='https://github.com/abtassociates/eva/issues/958' target='_blank'>#958</a> and <a href='https://github.com/abtassociates/eva/issues/1042' target='_blank'>#1042</a>) 
 - Instead of crashing, hide System Performance tab when: 
     - No Continuum Project records (Issue <a href='https://github.com/abtassociates/eva/issues/1028' target='_blank'>#1028</a>)
     - Only HP projects or enrollments outside reporting period
 - Handle other System Performance empty dataset scenarios by showing 'No valid data message' in place of the charts (Issue <a href='https://github.com/abtassociates/eva/issues/1027' target='_blank'>#1027</a>) 
 - Fix Data Quality and System Performance plots and data tables to always use commas for formating large numbers. 

### Miscellaneous 
 - Handle empty Destinations scenario causing crash in GHA (assumed to be edge case scenario) 
 - Revised Client-level Download Instructions, Data Dictionary, and file formatting.

## 2026-06-12

### New Features 
 - System Exits charts, visualizations, and exports 

### Bug Fixes 
 - Fixed incorrect zero utilization warning for projects with multiple HMIS Participation Records (Issue <a href='https://github.com/abtassociates/eva/issues/994' target='_blank'>#994</a>)
 - Fixed incorrect unhashed file upload error for exports that have the correct HashStatus but no client records (Issue <a href='https://github.com/abtassociates/eva/issues/994' target='_blank'>#998</a>) 
 
### Miscellaneous 
 - Renamed Glossary page to System Performance Glossary 
 - Updated System Performance Glossary to include new entries related to System Exits

## 2026-05-08

### Bug Fixes 
 - Ignore persons over age 100 in <i>Incorrect DOB or Entry Date</i> DQ check. Added a separate DQ Warning for persons over age 100 to encourage double-checking of the data. (Issue <a href='https://github.com/abtassociates/eva/issues/983' target='_blank'>#983</a>)


## 2026-05-08

### Bug Fixes 
 - Ignore non-HoH children in DQ check for Days Since Most Recent CLS for Street Outreach, Services Only, and Coordinated Entry projects. (Issue <a href='https://github.com/abtassociates/eva/issues/940' target='_blank'>#940</a>)

## 2026-02-24

### Bug Fixes 
 - Corrected logic for refreshing DQ and PDDE tables and downloads when multiple datasets are uploaded within a session. (Issue <a href='https://github.com/abtassociates/eva/issues/963' target='_blank'>#963</a>)
 - Added more error-catching logic for upload file processing, which includes popups for easier issue creation.

## 2026-02-05

### Bug Fixes 
 - Corrected exit date adjustments for night-by-night and non-residential project enrollments in System Performance calculations, resulting in some clients to be counted as Exited instead of Inactive in the System Flow charts. For enrollments without exit dates, the adjusted exit dates were based on buffer periods that were double the length they should have been.

## 2026-01-12

### Bug Fixes 
 - Fixed logic for DQ report downloads when Referrals sheet was empty, which was causing downloads to fail.

## 2026-01-09

### Bug Fixes 
 - Added better error-catching to Project Dashboard and Data Quality report download functions
 - Added missing data checks to Timeliness info boxes

### Improvements 
 - Sped up DQ report skipping for organizations without DQ issues
 - Increased report download timeout from 45 seconds to 60 seconds

## 2025-12-17

### Bug Fixes 
 - Added check for catching upload System Overview script failures before crashing. If there are errors, it now hides the System Performance portion of Eva instead of crashing.

### New Features
 - Added progress bars for downloads on DQ Export Interface page.

## 2025-11-26

### Bug Fixes 
 - Allow Homelessness Prevention Only files to bypass System Performance portion of Eva; The System Performance Overview page will be hidden for these types of files.
 - Fixed "No data" validation statements for System DQ and Org DQ charts. Previous fix was causing an error to appear on data quality charts.
 - Allowed skipping of long_stayers DQ check when no data available
 - Added 0-row checks to System-level Project Dashboard report within DQ Export downloads
 - Changed logic used for displaying and enabling the DQ Export download button. It should now display and be clickable if any reports are available.

## 2025-11-25

### Bug Fixes 
 - Allowed skipping overlap DQ check if no overlap data is present
 - Removed additional symbols from DQ Export Org folder names, to reduce chance of unsuccessful downloads
 - Added checks for system-level DQ report downloads, so it does not attempt to download if no DQ data is present
 - Added screen notification if DQ Export download is attempted but 0 valid reports can be generated
 - Add validate statements to show "No data" for Project Dashboard, System DQ, and Organization DQ tabs, when 0 rows of data are found.

## 2025-11-24

### Bug Fixes 
 - Modify DQ code to skip long_stayers dataset if DaysSinceLastKnown field is not available.
 - Added logging to 05_Data_Quality.R for additional DQ troubleshooting

## 2025-11-21

### Bug Fixes 
 - Added temporary bug fix for overlap DQ check when data without overlaps is uploaded

## 2025-11-20

### Bug Fixes 
 - Fixed handling of null rows in Participation datasets for overlaps. (Issue <a href='https://github.com/abtassociates/eva/issues/880' target='_blank'>#880</a>).
 - Standardized DQ Export Download file names using underscores when special characters are present (Issue <a href='https://github.com/abtassociates/eva/issues/890' target='_blank'>#890</a>).

## 2025-11-13

### New Features
 - Month-by-Month System Flow visualization
 - Data quality export interface which allows multi-org DQ exports
 - Data quality timeliness features
 - 4 PDDE checks (IDs 35, 36, 37, 106) and 2 DQ checks (IDs 107, 108)
 - Major UI update, due to upgrade to bootstrap5
 - Performance improvements
 - Fixes to System Inflow and Outflow logic

### Bug Fixes 
 - Fixed underreporting of DQ checks for enrollments outside project operation/participation.
 - Convert columns to numeric if they are expected to be numeric but coming in as character, and there are no non-numeric values (Issue <a href='https://github.com/abtassociates/eva/issues/776' target='_blank'>#776</a>).
 - Fixed DQ Check #41 (Incomplete Living Situation) to include temporary and other living situations when analyzing field 3.917B.2B (Issue <a href='https://github.com/abtassociates/eva/issues/803' target='_blank'>#803</a>).

### Miscellaneous 
 - Renamed Client Counts page to Project Dashboard. 
 - Updated System Overview instruction text. 
 - Updated System Performance chart colors. 
 - Updated System Performance exports. 
 - Updated Glossary to include new entries related to Month-by-Month chart. 
 - Fixed typo in Glossary (Issue <a href='https://github.com/abtassociates/eva/issues/802' target='_blank'>#802</a>).

## 2025-09-29

### FY26 Data Standards 
 - Updated Eva to be compliant with FY26 HMIS Data Standards

## 2025-07-22

### Bug Fixes 
 - Fix SSVF HP Screening DQ check (Issue <a href='https://github.com/abtassociates/eva/issues/782' target='_blank'>#782</a>).

## 2025-04-24

### Bug Fixes 
 - Avoid crash when no available beds for active inventory during PDDE check (Issue <a href='https://github.com/abtassociates/eva/issues/762' target = '_blank'>#762</a>)
 - Adjust how move-in date is computed for household members.
 - Fixed issue with Project Type column in DQ Export.

### Miscellaneous 
 - Update Long Stayers check for ES NbN projects to use Bed Nights, rather than CLS records.


## 2025-04-03

### New Features 
 - Added new PDDE check for active inventory with no enrollments during the inventory dates.

### Bug Fixes 
 - Update Zero Utilization check logic to avoid flagging projects with utilization (Issues <a href='https://github.com/abtassociates/eva/issues/677' target='_blank'>#677</a> and <a href='https://github.com/abtassociates/eva/issues/713' target='_blank'>#713</a>).

## 2025-03-27

### Bug Fixes 
 - Handle leap years in System Performance charts (Issue <a href='https://github.com/abtassociates/eva/issues/738' target='_blank'>#738</a>)
 - Allow bed type "Other" for site based ES projects (Issue <a href='https://github.com/abtassociates/eva/issues/703' target='_blank'>#703</a>)
 - Fixed bug where projects with no inventory records were getting flagged for both the No Inventory Records check and the Project CoC Missing Inventory check. The latter should not be flagged if the former is already flagged.
 - Deduplicate records in the PDDE export.

### Miscellaneous 
 - Updated and added application text for clarity on System Performance age.
 - Removed System Performance demographic filter as well as references to filter and related language throughout Eva.

## 2025-03-06

### New Features 
 - Added HHType to Overlap export (Issue <a href='https://github.com/abtassociates/eva/issues/697' target='_blank'>#697</a>)

### Bug Fixes 
 - Fixed edge case detection of NbN overlaps; i.e., too many duplicates caused a join error and crashed Eva. (Issue <a href='https://github.com/abtassociates/eva/issues/683'>#683</a>)
 - Fixed warning about DateProvided when Services file is empty
 - No longer flagging VSP Projects as having clients when they do not (Issue <a href='https://github.com/abtassociates/eva/issues/678' target='_blank'>#724</a>)

### Miscellaneous 
 - Gracefully handle timeouts

## 2025-02-20

### New Features 
 - Code speedups particularly around file import and initial processing, as well as Impermissible Character downloads.

### Bug Fixes 
 - Small fix in how FSA issues are reported when an expected column is missing from a file.


## 2024-02-06

### New Features 
 - Added client-level export to the System Performance tab to provide transparency in the charts
 - Rejecting HMIS zip files containing Export.csv with more than one row. This scenario will be captured as a High-Priority error.

### Bug Fixes 
 - Fixed display of missing geography and address warnings for PDDE.
 - Only flag residential projects for Active Inventory PDDE check. (Issue <a href='https://github.com/abtassociates/eva/issues/678' target='_blank'>#678</a>)
 - Only reference active inventory records to check bed type compatibility with housing type. (Issue <a href='https://github.com/abtassociates/eva/issues/680' target='_blank'>#680</a>)
 - Fixed issue in how the Age and Race filter selections are displayed in the System Exports.
 - Fixed issue in bracket detection and handling of non-UTF8 encodings that could lead to crashing Eva. (Issue <a href='https://github.com/abtassociates/eva/issues/698' target='_blank'>#698</a>)
 - Fixed issue in display of Race filter selections in System Performance exports.

### Miscellaneous
 - Updated System Performance Methodology Type terms to "Method 1" and "Method 2." Renamed related demographic columns to align with new Methodology Type terms.

## 2024-12-31

### New Features 
 - Updated demo.zip to capture new overlaps.
 - Modified upload-related pop-ups to be more informative. 
 - Added Project Type to PDDE export (Issue <a href='https://github.com/abtassociates/eva/issues/641' target='_blank'>#641</a>)

### Bug Fixes 
 - Only flagging Missed Move-In if relationship to HoH is 1 (Issue <a href='https://github.com/abtassociates/eva/issues/650' target='_blank'>#650</a>)
 - Fixed NbN overlap detection. An NbN and an EE overlap if the first and last DateProvided for a given enrollment overlaps with an EE enrollment by more than 2 days. Two NbNs overlap if they have duplicate DateProvideds for a given PersonalID (Issue <a href='https://github.com/abtassociates/eva/issues/659' target='_blank'>#659</a>)
 - Allow all UTF-8 characters, except for brackets (Issues <a href='https://github.com/abtassociates/eva/issues/649' target='_blank'>#649</a> and <a href='https://github.com/abtassociates/eva/issues/667' target='_blank'>#667</a>)
 - Automatically interpret non-UTF-8 encoded characters to avoid crashing and display correct character (Issues <a href='https://github.com/abtassociates/eva/issues/649' target='_blank'>#649</a> and <a href='https://github.com/abtassociates/eva/issues/667' target='_blank'>#667</a>)
 - Modified Missing Address check to allow for VSPs and tenant-based scattered sites to be missing Address1, City, and State (Issue <a href='https://github.com/abtassociates/eva/issues/631' target='_blank'>#631</a>)

## 2024-11-19

### Bug Fixes 
 - Corrected the Zero Utilization logic to only flag projects that are missing enrollments during a project's operating and participating periods. See Issue <a href='https://github.com/abtassociates/eva/issues/522' target='_blank'>#522</a>.

## 2024-11-05

### Bug Fixes
 - Fixed handling of '#' symbols in first row of a csv file (<a href='https://github.com/abtassociates/eva/issues/638' target='_blank'>#638</a>,<a href='https://github.com/abtassociates/eva/issues/632' target='_blank'>#632</a>,<a href='https://github.com/abtassociates/eva/issues/535' target='_blank'>#535</a>)
 - Fully reset app after a failed upload

## 2024-10-23

### Bug Fixes 
 - Corrected the Zero Utilization logic to only flag projects that are missing enrollments during a project's operating and participating periods. See Issue <a href='https://github.com/abtassociates/eva/issues/522' target='_blank'>#522</a>.

## 2024-10-07

### New Features 
 - Launched the System Performance Overview Page. The page contains three new system performance charts: the System Flow Chart, the Client System Status Chart, and the System Demographics Chart. The system performance charts give users flexibility in how they filter and view their community’s homeless response system performance data.
 - Updated how 'Data not collected' is handled when Eva assesses data quality. Eva now classifies data elements marked in the export as 'Data not collected' as a Warning instead of as an Error, to align with Eva's Error and Warning definitions. This approach is aimed at helping HMIS end users focus on correcting fixable issues without changing accurate information. The Warning is meant to remind users to verify if the data might still be collected or if the data exists elsewhere, such as in the client's file. If 'Data not collected' is the correct value, users should leave the value as-is in HMIS. Issues that should be fixed in HMIS are categorized as Errors.
 - Removed logic that looks for nulls in specific data elements when Eva assesses data quality. For the Race/Ethnicity, Veteran Status, Disabling Condition, Name Data Quality, DOB Data Quality, SSN Data Quality, and Destination data elements, the HMIS CSV export does not distinguish between nulls and the value 'Data not collected' The issues removed are: Missing Name Data Quality, Missing DOB Data Quality, Missing SSN Data Quality, Missing Veteran Status, Missing Disabling Condition, and Missing Destination. These are now flagged as Warnings and described as 'Unknown Veteran Status,' etc.

### Bug Fixes
 - Improved handling of overlapping enrollments
 - Fixed invalid move-in date (issue <a href='https://github.com/abtassociates/eva/issues/615' target='_blank'>#615</a>)
 - Fixed the way Eva uses Project Participation and groups Projects that change participation across the reporting date range. Addresses issues <a href='https://github.com/abtassociates/eva/issues/521' target='_blank'>#521</a> and <a href='https://github.com/abtassociates/eva/issues/605' target='_blank'>#605</a>.

## 2024-08-01

### New Features 
 - Formatted changelog for readability 
 - Split processing tasks so that Eva does a quick check for any impermissible characters, then if any are found, the user can choose to run a more detailed report about the impermissible characters< and their precise location in the uploaded file

### Bug Fixes
 - Added Enrollment ID and Project Type to exports (Issues <a href='https://github.com/abtassociates/eva/issues/482' target='_blank'>#482</a> and <a href='https://github.com/abtassociates/eva/issues/529' target='_blank'>529</a>),
 - Corrected the institutional living situations and temporary living situations to align to the HMIS Data Standards. Issue <a href='https://github.com/abtassociates/eva/issues/490' target='_blank'>#490</a>

## 2024-05-20

### New Features 
 - Added demo mode, where users can try out Eva even if they don't have access to an FY 2024 HMIS CSV Export.

## 2024-04-09

### Bug Fixes 
 - Moved all variables from global environment to a session environment to ensure appropriate visibility. 
 - Moved data frames "Export" and "Project0" from the global environment to a session environment to ensure appropriate visibility.

## 2024-04-01

### New Features 
 - Added 'Demo Mode', which allows users to play around with a test dataset in Eva in order to understand how the tool works without having to upload any data. 
 - Added loading spinners to make clear when something is loading as opposed to frozen.

## 2024-01-24

### New Features 
 - If a project has an Operating or Participating end date in the future Eva will not flag these enrollments as Enrollment Crosses Operating/Participating End errors. 
 - Added upload progress display text so it's clear Eva is working and not freezing. 

### Bug Fixes 
 - Corrected 'No Inventory Records' logic so that it does not flag RRH-SSO projects.

## 2023-12-14

### New Features 
 - Eva now filters out records in any csv file that has a value in the DateDeleted column. 

### Bug Fixes 
 - Added check for non-ASCII (i.e. impermissible) characters that caused Eva to crash

## 2023-11-30

### Bug Fixes 
 - Added filters for income, non-cash, health insurance data quality issues so that projects that don't need to collect that, based on their funding source and project type are not being false flagged.

## 2023-11-15

### Bug Fixes 
 - Corrected date logic in several places.

## 2023-11-09

### Bug Fixes 
 - Corrected a few incorrect data types in columns.csv. 
 - Fixed EntryDate bug so that EntryDate aligns with the original data rather than an adjusted date. 
 - Added 'VSP participating in HMIS' as a Data Quality Error. 
 - Data Quality Warning 'Zero Utilization' now only looks at projects that are set as HMIS Participating. 
 - Changed Data Quality Warning 'Future Exit Date' to exclude all enrollments without an Exit Date. 
 - Corrected language throughout for 'Client Refused' to 'Client prefers not to answer' and 'Don't know' to 'Doesn't know.'

## 2023-11-02

### Bug Fixes 
 - Fixed issue with overidentifying overlapping CE Participation records. 
 - Removed slowness warning at the beginning. Slowness seems to be limited to Abt machines. 
 - Fixed bug where Eva crashed when a demographic column name was misspelled in Client.csv. 
 - Modified empty File Structure Anaysis results text to be more explicit and helpful.

## 2023-10-20

### New Features 
 - Added 2 new Data Quality issues: Missing Current Living Situation Subsidy Type and Missing Prior Living Situation Subsidy Type and updated Missing Destination Subsidy Type to also flag subsidy types that are not valid subsidy types, like 99, as an example. 
 - Added PDDE issue that flags when an RRH-SO project has active inventory. 
 - Added 2 new PDDE issues: Overlapping HMIS Participation records and Overlapping CE Participation records. 
 - Updated columns.csv to the new nullability edits from the Data Lab

## 2023-10-02

### New Features 
 - Added a High Priority File Structure issue that requires the user to upload an HMIS CSV Export that is compliant with the FY 2024 specifications 
 - Added 'Missing Destination Subsidy' check 
 - Added 'Enrollment After Participating Period' check 
 - Added 'Enrollment Crosses Participating Period' check 
 - Added 'Enrollment Before Participating Period' check 
 - Added 'Enrollment Crosses Participating End' check 
 - Added 'Enrollment Crosses Participating Start' check 
 - Added 'Enrollment After Operating Period' check 
 - Added 'Enrollment Crosses Operating Period' check 
 - Added 'Enrollment Before Operating Period' check 
 - Added 'Missing Destination Subsidy' check 
 - Added 'RRH No SubType' PDDE check 
 - Changed 'Missing Client Location' to 'Missing Enrollment CoC' 
 - Changed 'Exit After Project's Operating End Date' to 'Enrollment Crosses Operating End.' 
 - Changed 'Entry Precedes Operating Start Date.' to 'Enrollment Crosses Operating Start.' 
 - Removed 'Non-HMIS Participating Discrepancy.' 
 - Removed 'Missing Last Permanent Address' check for SSVF enrollments 
 - Removed 'Missing Tracking Method' PDDE check 
 - Added High Priority File Structure issues to the EvaChecks.xlsx workbook 
 - Added initial file check for the CSV Version which separates the upload issue 'Missing Files' into 'You may have uploaded the wrong dataset,' 'Your HMIS CSV Export is out of date,' and 'Incomplete dataset.' 
 - Adjusted data element references to account for new Race/Ethnicity options 
 - Adjusted HMIS Participating, Tracking Method, and EnrollmentCoC logic to fit new structure 
 - Corrected 'Missing Non-cash Benefits' and 'Conflicting Non-cash Benefits' logic 
 - Removed HMIS package from development (no change to user experience) 
 - Updated link to the HMIS CSV specifications on the Home tab in the 'Instructions' box 
 - Changed the EvaChecks.xlsx document to EvaChecks.csv so that the document can be version controlled more transparently

## 2023-09-21

### Bug Fixes 
 - Added back accidentally removed Incorrect DOB and Missing Living Situation checks

## 2023-08-30

### Bug Fixes 
 - Fixed missed demotion of null CoCCode File Structure Analysis issue in EnrollmentCoC.csv and ProjectCoC.csv to "Error" from "High Priority" 

### New Features 
 - Replaced 'Invalid Homelessness Start Date/Number of Months Homeless' data quality check with two checks: 'Homelessness Start Date conflicts with Living Situation Data' and 'Number of Months Homeless conflicts with Living Situation Data' to include cases where Aproximate date homeless is over 3 years ago.

## 2023-08-14

### Bug Fixes 
 - Modified Long Stayer logic so the issue names match the terminology in the Local Settings. Prepping the current Long Stayers issues to fit with the ones we plan to add in upcoming releases.

## 2023-08-10

### Bug Fixes 
 - Demoted several null-column File Structure Analysis issues to "Error" from "High Priority" 
 - Added missing checks to EvaChecks.xlsx 

### New Features 
 - Changed to pull issue-related text from EvaChecks.xlsx

## 2023-07-06

### Bug Fixes 
 - Fix bug when datasets have no projects of a certain status (e.g. Active No Move-In)

## 2023-06-28

### New Features 
 - Allow nulls in AssessmentLocation, AssessmentType, and PrioritizationStatus columns.

## 2023-06-14

### New Features 
 - Added graceful rejection of non-zip files, including 7zip.

## 2023-05-08

### Bug Fixes 
 - Fixed typo in DQ High Priority plot display when there were no issues of that type.

## 2023-04-25

### New Features 
 - Small updates to the EvaChecks.xlsx file to better capture Eva's checks.

## 2023-04-20

### New Features 
 - Added Zero Enrollments as a High Priority File Structure issue. 
 - Upgraded the Duplicate HouseholdIDs File Structure issue from Error to High Priority as promised. 
 - Code refactoring, consolidating, reorganizing, and commenting

## 2023-04-06

### Bug Fixes 
 - Fixed bug where an Org-level DQ file name does not necessarilly match the selected organization

## 2023-03-23

### Bug Fixes 
 - Modified DQ chart colors and unsuccessful upload pop-up text. 
 - Fixed display of system-level, Top 10 Orgs charts for errors and warnings. 
 
### New Features 
 - Added Client Count Download instructions. 
 - Added Zero Utilization to Project Descriptor Data Quality Checker.

## 2023-03-09

### New Features 
 - Renaming R Scripts for improved organization 
 - Added console logging to facilitate debugging 
 - Previously, "Missing Geography Information" and "Missing Address" had the same Issue Name of "Missing Geography Information." They were split out for clarity.

## 2023-03-02

### Bug Fixes 
 - Fixed timeout to fully clear data by reloading the session. 
 - Updated language on home page to match recent update to what metadata is being logged by Eva.

## 2023-02-23

### New Features 
 - Changed Long Stayers (aka Possible Missed Exit) logic so that, for Outreach and Coordinated Entry projects, it measures from the last Current Living Situation instead of from the Entry Date. 
 - Addresses GitHub issue 152 by adding a Detail column to the File Structure Analysis download separate from the more general Guidance. This column includes more details about affected rows and column in order to help the user identify issues in their data. 
 - Addresses GitHub issue 154 by checking for missing columns and extraneous columns in a similar way. This is a change from the prior issues specifying that a column name was misspelled. Now a misspelled column will show as one missing column (with the correct column name) and one extraneous column (with the actual column name.) 
 - Addresses GitHub issue 172 by preventing R from counting the value of "NA" as an actual null.

## 2023-02-09

### New Features 
 - Added system-wide download of Client Counts data 
 - Separated app timeout and crash processing. Timeout triggers a javascript alert and clears the app data. Crashes trigger the gray screen with a message and a Refresh link. 
 - Added Outstanding Referrals as a Warning. Eva users can set what constitutes and outstanding referral for their CoC on the Edit Local Settings tab. The issue will show in the download on the Warnings tab and on its own tab called Referrals so that end users can see which Referral is considered outstanding.

## 2023-01-26

### New Features 
 - Addresses GitHub issue 82. Now the app times out after 10 minutes being idle. 
 - Addresses GitHub issue 122. Modified tab structure to spread things out and simplify the Home tab. 
 - Addresses GitHub issue 124. Modified plot color for High Priority issues.

## 2023-01-23

### Bug Fixes 
 - Hotfix: Added improved metadata collection for troubleshooting purposes.

## 2023-01-13

### Bug Fixes 
 - Hotfix: Set GrantID field so it is not considered a high priority column so that it will no longer cause Eva to reject a file for incorrect data type.

## 2022-12-29

### Bug Fixes 
 - Addresses GitHub issue 118. Eva was not checking that all needed csvs were in the export. Now it checks this and rejects the export if they are not there.

## 2022-12-29

### Bug Fixes 
 - Addresses GitHub issue 118. Eva was missing some instances where a date variable is of the wrong type (e.g. ymd_hms instead of ymd). Now it rejects exports if an important variable has the wrong date type.

## 2022-12-29

### New Features 
 - Client Counts report: if a user makes the Report Date Range so that the Start > End, Eva now alerts the user in the data tables to check dates.

## 2022-12-29

### New Features 
 - Rewrote PDDE issues' Guidance so that it is general guidance, then added Details column to include IDs to help admins find specific issues.
