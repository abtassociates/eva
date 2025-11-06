tabHome_welcome <- HTML("<h2>Welcome to Eva!</h2>
              <p><b>Eva</b> is an <a href = 'https://github.com/abtassociates/eva'
              target= '_blank' rel='noopener noreferrer'>open-source</a>
              project intended for local use by HMIS Administrators in Continuums
              of Care (CoCs) around the U.S. and its territories. Eva is designed 
              to help you (1) assess the accuracy and completeness of the data within 
              your HMIS, and (2) understand your homeless response system’s flow 
              and performance. Using Eva does not result in reporting or sharing 
              data with HUD and use of Eva is not required by HUD.</p>
              
              <p>Eva is a web-based tool built with R Shiny. This means:</p>
              <ul>
                <li>Eva will only access your CoC&rsquo;s data during your session, 
                <strong>no CoC data is being retained or viewed by anyone besides 
                you.</strong> Eva does retain metadata about the upload file itself, 
                such as the name of your software vendor, your export dates, hash 
                status, and data source information. This is collected for 
                troubleshooting and tool planning purposes.
                </li>
                <li>You can upload a zipped CSV Export of up to 200 MB. The 
                file must be hashed.</li>
                <li>You can stay up to date with the new features by visiting 
                the Changelog tab.</li>
              </ul> 
            ")

tabHome_home_live_instructions <-  HTML(
  "<p>Eva works by uploading a hashed 
              <a href='https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf'
              target= '_blank' rel='noopener noreferrer'>HMIS CSV Export</a>.
              </p>
              <p>Generate a hashed HMIS CSV Export from your local HMIS and store
              it in a secure location that you can easily find again. It must be
              a .zip file with 23 csv files in it.
              <ul>
              <li>A hashed export means that the personal identifiers are obscured
              when the export is generated.</li>
              <li>The HMIS CSV Export has client-level data in it, so it must be
              stored in a secure location per HUD, state, and local rules and
              regulations.</li>
              <li>If you are unsure how to generate your hashed HMIS CSV Export,
              please contact your vendor.</li>
              </ul>
              
              <p>Once you have exported the correct file from your HMIS, you are
              ready to engage with Eva. Navigate to the \'HMIS CSV Export\' tab
              and follow the instructions there.</p>

              <p>Want to explore Eva without uploading? Use Eva's Demo Mode by clicking the 
              toggle at the top.</p>
              ")

tabHome_home_demo_instructions <- HTML(
  "
              <p>Welcome to Eva’s Demo Mode. In Demo Mode, you can explore the
              functionality of Eva with a pre-uploaded HMIS CSV Export file that
              uses sample HMIS data. When Demo Mode is on, Eva has the same
              functionality but uses the sample HMIS data to provide examples of
              possible File Structure Errors, Data Quality Errors, and Warnings.
              Select any of Eva's pages from the navigation menu to the left to
              explore the application. </p>
              <h4>Turning Demo Mode On & Off</h4>
              <p>To turn Demo Mode on and off, use the yellow Demo Mode toggle
              on the top right of the screen. This toggle will be available from
              every page in Eva. Please note that you can turn Demo Mode off or
              on at any time, the application will just ask you to confirm your
              choice.</p>
              <p>If you uploaded your own dataset to Eva and then decide to turn
              on Demo Mode, Eva will (1) clear the application of your HMIS data,
              ending the session, and (2) replace it with that of the sample
              dataset. <strong>If you wish to see your results again you will need to
              re-upload your hashed HMIS CSV Export file.</strong> To do so, you
              need to turn off Demo Mode. This will clear the sample HMIS data
              from the application so you can operate Eva as normal and upload
              your own HMIS data again.</p>
              ")

tabHome_need_help <-   HTML(
  "<p>Trouble-shooting tips:
              <ul>
              <li>Be sure you have generated the correct export. You cannot use
              an APR or LSA export file for use with this tool.</li>
              <li>Eva is looking for a zip file and will extract the files for you,
              so unzipping your export is not necessary.</li>
              <li>If your export is a .7z file, you must convert it to a .zip file.
              If you are not sure about how to do this, please contact your vendor.
              <li>If something is not working, please go to <a
              href='https://github.com/abtassociates/eva/issues' target= '_blank'
              rel='noopener noreferrer'>GitHub</a> to check for known issues
              and/or enter any new issues or feature enhancement requests. To
              enter an Issue on GitHub, you must have an account. If you do not
              already have a GitHub account, you can sign up for one <a
              href='https://github.com/join' target= '_blank'
              rel='noopener noreferrer'>here</a>.
              </ul>"
)

tabHome_citations <-  HTML("
                 <p>This project would not exist were it not for the existence of
                 other quality, free and open source products. The following are
                 citations for the products this app relies on.
                              
                 <p> The foundational code for the app was shared via AGPL license by 
                 the Coalition on Homelessness and Housing in Ohio (<a href =
                 'https://www.cohhio.org' 
                 target= '_blank' rel='noopener noreferrer'>COHHIO</a>). 
                 
                 <p>R Core Team (2022). R: A language and environment for statistical
                 computing. R Foundation for Statistical Computing. Vienna, Austria. 
                 <a href = 'https://www.r-project.org' 
                 target= '_blank' rel='noopener noreferrer'>R programming language</a>.
                 
                 <p>Wickham et al., (2019). Welcome to the tidyverse. <a href = 
                 'https://doi.org/10.21105/joss.01686'
                 target = '_blank' rel='noopener noreferrer'>Journal of Open Source
                 Software</a>, 4(43), 1686, 
                 <a href = 'https://cran.r-project.org/web/packages/tidyverse/index.html'
                 target= '_blank' rel='noopener noreferrer'>Tidyverse package</a>.
                 
                 <p>Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen
                 J, McPherson J, Dipert A, Borges B (2021). _shiny: Web Application
                 Framework for R_. R package version 1.7.1, 
                 <a href = 'https://cran.r-project.org/web/packages/shiny/index.html'
                 target= '_blank' rel='noopener noreferrer'>R Shiny package</a>.
                 
                 <p>Chang W, Borges Ribeiro B (2021). _shinydashboard: Create
                 Dashboards with 'Shiny'_. R package version 0.7.2, 
                 <a href = 'https://CRAN.R-project.org/package=shinydashboard'
                 target= '_blank' rel='noopener noreferrer'>shinydashboard package</a>.
                 
                 <p> Special thanks to 
                 <a href=\"http://www.squarepegdata.com/\" 
                 target= '_blank' rel='noopener noreferrer'>
                 Square Peg Data</a>, the CoCs who provided us with
                 sample datasets to support programming.")

tabUpload_instructions <-  HTML("
                 <h4>Upload hashed HMIS CSV Export</h4>
                 
              <p>To upload your hashed HMIS CSV Export, click the \'Browse\'
              button. Once you find the zip file on your computer, click on it to select it, 
              and click \'Open\' to begin the upload. Eva might take a few moments 
              to process your selected file. Eva will first check to determine if 
              the export is hashed. If it is not, Eva will reject the file with 
              an error message, and clear Eva's memory. Eva will continue to do 
              this until you upload a hashed HMIS CSV Export.</p>
              
              <h4>HMIS CSV Export File Structure Analysis</h4>
              <p>After confirming your export is hashed, Eva will review and process 
              the file structure of your upload. The File Structure Analysis assesses 
              the structural components of the uploaded .zip file and determines 
              if it meets Eva’s file structure requirements, such as if the files 
              have all the right names, columns, data types, and allowable values etc.</p>
              
              <p>Once your upload is processed and Eva has finished assessing the 
              file structure integrity of your upload, Eva will provide a pop-up 
              message alerting you of your upload status. You can have either a 
              successful upload or an unsuccessful upload based on the structural 
              integrity of your HMIS CSV export. The key difference between a successful 
              upload and an unsuccessful upload is if the upload has any High Priority 
              File Structure Errors.</p>
              
              <p>While any error identified during the File Structure Analysis represent 
              components in the uploaded HMIS CSV export file that do not meet the 
              most recent <a href='https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf'
              target= '_blank' rel='noopener noreferrer'>HMIS CSV Format Specifications</a>, 
              there are some file structural errors that are more relevant to the 
              functionality of Eva.</p>
              
              <ul>
                <li><b>High Priority File Structure Errors</b> are file structure issues 
                that will cause Eva to not work.</li>
                <li><b>General File Structure Errors</b> are file structure issues that 
                will not impact Eva’s ability to work, but do not meet HMIS CSV 
                format specifications.</li>
              </ul>
              
              <p>If Eva identifies any High Priority File Structure Errors during 
              the File Structure Analysis that prevent Eva from functioning, Eva 
              will reject your upload and stop processing the export. You will thus 
              not be able to assess the data quality of your upload or analyze the 
              system performance of your homeless response system. For both successful 
              and unsuccessful uploads, all identified file structure errors will 
              display in the HMIS CSV File Structure Analysis panel, where you can 
              download the details.</p>
              
              <p>It is essential that you contact your HMIS vendor to resolve all 
              High Priority File Structure Errors identified in the HMIS CSV Export 
              File Structure Analysis, as well as any other structural issues which 
              you feel need to be corrected. Not all structural issues found in 
              this analysis will prevent the data from being accepted for analysis, 
              so they may not require immediate attention. Once your vendor has 
              addressed any High Priority File Structure Errors, you can attempt 
              another upload.</p>
              
              <p>Once you have uploaded a hashed and structurally sound .zip file, 
              you will see a confirmation that your upload was successful, the date 
              range of the files you uploaded, plus the date your Export was downloaded 
              from your HMIS. You will then be able to assess the data quality of 
              your upload and analyze the system performance of your homeless response system.</p>
              
              
              ")

tabUpload_in_demo_mode <- HTML(paste0("
                 <p>You’re currently in Demo Mode and viewing sample HMIS data
                 from a curated HMIS CSV Export file. View the File Structure
                 Analysis below to see examples of the File Structure Errors you
                 could get in your own uploads. For a full list of possible
                 errors, see <a href =
                 'https://github.com/abtassociates/eva/blob/main/public-resources/EvaChecks.csv' 
                 target= '_blank' rel='noopener noreferrer'>Eva Checks</a>.</p>
                 <p>To explore your own File Structure Errors, turn off Demo
                 Mode and upload your own hashed HMIS CSV Export file.</p><br>
            "))

tabLocalSettings_instructions <-  HTML("
              <p>To make Eva data quality analysis more useful at the local level, 
              you can adjust the local settings to better analyze your data in a 
              way that is meaningful to your CoC. To edit these, click to expand 
              the relevant box below. If you do not edit them, the Data Quality 
              and Client Counts pages will use the defaults listed. Please 
              note, these local settings do not impact the System Performance Overview page.</p>
              
              <p>These defaults do not imply any HUD recommendations. Please read 
              the description in the Local Settings tab for more information.</p>
              ")

tabLocalSettings_outstanding_referrals <- HTML(
  "<p>This check aims to help communities find Coordinated Entry (CE)
                Event referrals that may be missing a Result Date or may have
                been delayed in getting the client to housing. This check is
                only applied to CE Event referrals which are expected to have an
                associated Result and Result Date (4.20.2 responses 10-15, 17,
                18. Please see the HMIS Data Standards for the complete list
                of CE Events.) 
                
                <p>When a CE Event referral does not have a Result Date at the
                time the export is uploaded, Eva calculates how many days the
                referral has been open by looking at the number of days
                between the Referral Date and the date your upload was exported
                from your HMIS. Then Eva compares the length of each open
                referral with the 'Max Days' assumption entered in the input
                field below. If the referral is open longer than the expected
                timeframe, it is categorized as an 'Outstanding Referral.' This
                check is for all projects that have a relevant CE Event referral.
                
                <p>Data quality flags about Outstanding Referrals are categorized
                as Warnings, indicating that data should be reviewed for accuracy.
                It does not imply that any data should be changed.

                <p>In the field below, specify the maximum number of days a referral
                can stay open according to the CoC's Coordinated Entry Referral
                process. The value defaults to 14 days. (These defaults do not
                imply any HUD recommendations)."
)

tabLocalSettings_long_stayers <-  HTML("
                <p>This check aims to help communities find enrollments that may
                be missing an Exit Date. First, the tool calculates the number of
                days each enrollment has been open (meaning, the number of days
                between the Entry Date and the date your upload was exported
                from your HMIS.) Then the check uses one of two methodologies to
                identify Long Stayers. 
                <p>For select project types, the check identifies the top % of
                longest stayers in each project type. For other project types,
                the check compares the length of each enrollment with assumptions
                entered about the expected maximum period of assistance envisioned
                for the project type. For the latter check, users can set the
                assumptions for each project type. All data quality flags about
                Long Stayers are categorized as Warnings and is a suggestion to
                verify that the identified clients are still active in these
                projects. It does not imply that any data should be changed.
      
                <p><b>Top 2% longest enrollments are flagged for the following
                project types:</b>
                <ul>
                <li>Emergency Shelter - Entry/Exit
                <li>Safe Haven
                <li>Transitional Housing
                <li>Rapid Rehousing
                <li>Homeless Prevention
                </ul>
                
                <p><b>Top 1% longest enrollments are flagged for the following
                project types:</b>
                <ul>
                <li>Permanent Supportive Housing
                <li>Permanent Housing with Services (no disability required for entry)
                <li>Permanent Housing - Housing Only
                </ul>
                
                <p><b>Enrollments active longer than the CoC-specified length of
                assistance targets are flagged for the following project types:</b>
                <ul>
                <li>Emergency Shelter - Night-by-Night
                <li>Street Outreach
                <li>Other
                <li>Services Only
                <li>Day Shelter
                <li>Coordinated Entry
                </ul>
      
                <p>Below, you can specify the expected maximum period of assistance
                envisioned for the project type, meaning the timeframe after which you
                would want an organization to confirm the client is still active in the 
                project. You can set these based on your current data or leave them at
                the defaults (these defaults do not imply any HUD recommendations)."
)

tabClientCount_instructions <- HTML("
               <h4>Project Dashboard Report</h4>
               <p>The Project Dashboard Report provides the number of households/clients 
                who have been served in each project and their enrollment status at the 
                time of CSV Export generation. This report can be used to verify that a project is up to date 
                on their HMIS data entry by comparing counts reported with the number of households/clients that 
                are known to be served in each project. Permanent housing projects can check that the number of 
                households/clients who have not yet moved into housing is correct. The report also contains 
                record entry timeliness metrics that show how long it takes for a project to enter Project 
                Start and Project Exit records into HMIS. Certain project types will also see timeliness 
                metrics for Current Living Situation (CLS) and Bed Night service records.</p>
               
               <h4>Inputs</h4>
               <p>Select a project from the drop list and adjust the the Date
               Range for the cohort of clients you want to see reported. The
               Date Range defaults to the date range covered by the HMIS CSV Export. 
               Users are encouraged to edit the Date Range as desired to see
               metrics for timeframes within the Export period, such as the
               number of households/clients who exited during that timeframe
               with and without a Move-In Date. Note that setting the Start Date
               to the Export End Date will show the current status for all
               enrollments for the project. While on the Timeliness panel, to 
              see the percentage of all records entered within a specific number 
              of days for a project, users can adjust the value in the \'Timeliness: Max Record Entry Days\' box.</p>
               
               <h4>Client Counts Summary</h4>
               <p>The Summary tab of the Client Counts panel provides a count of households/clients who
               have statuses of the following within the selected project:
               <ul>
               <li>Active No Move-In Date</li>
               <li>Currently Moved In</li>
               <li>Exited No Move-In</li>
               <li>Exited With Move-In</li>
               <li>Currently in Project</li>
               <li>Exited Project</li>
               </ul>
               
               <h4>Client Counts Detail</h4>
               <p>The Detail tab of the Client Counts panel you will see the Personal ID, Relationship to HoH,
               Entry Date, Move-In Date, Exit Date, and the Status for each client
               served by the selected project within the Date Range selected. The
               rows are ordered by Entry Date (oldest on top), Household ID (not
               visible), and Personal ID. This enables users to see the oldest
               enrollments first and groups clients in the same household
               together. All columns are searchable. For example, to find all
               enrollments with a Status of \'Active No Move-In Date\', you can
               type \'act\' in the Status search bar and the data table will
               react and filter in that way.</p>

              <h4>Timeliness Record Entry</h4>
               <p>The Record Entry tab of the Timeliness panel provides counts of different 
              record types for a project along with timeliness metrics. Time for Record Entry 
              is calculated by comapring a record's Created Date against the:
               <ul>
               <li>Entry Date for Project Start records</li>
               <li>Exit Date for Project Exit records</li>
               <li>Information Date for Current Living Situation records</li>
               <li>Date Provided for Bed Night service records</li>
               </ul>
               
               <h4>Download System-wide Project Dashboard Report</h4>
               <p>To download client count data for all projects in your HMIS CSV Export, 
               click the System-wide download button. The download contains a Current 
               tab limited to just the current date, a Date Range tab limited to the 
               Date Range set, and a Detail tab with clients' PersonalIDs, Entry Date, 
               Move-In Date (if applicable), Exit Date (if applicable), and
               project status. There are also separate Timeliness tabs for metrics on 
              Project Start, Project Exit, CLS records, and Bed Night service records data entry.</p>")

tabPDDE_instructions <-   HTML("
               <h4>Project Descriptor Data Element (PDDE) Check Summary</h4>
               <p>Once you have successfully uploaded an HMIS CSV Export, you
               will find a summary of each issue that was flagged in your data
               regarding your PDDEs. Please download the details by clicking the
               \'Download\' button.</p>
               
               <h4>Guidance</h4>
               <p>For a description of each issue found, check the Guidance 
               panel.</p>")

tabDQOrg_instructions <-  HTML("
               <h4>Organization-wide HMIS Data Quality</h4>
               <p>Below, select the organization whose data quality you would
               like to check. The data shown will reflect the date range that
               you used to run your HMIS CSV Export. It will show data quality
               metrics from all Projects that are associated with that
               organization.</p>
               <p>You can click the Download button to generate an Excel workbook 
               with the selected organization's data quality errors. You can send 
               these to authorized HMIS users at the selected organization
               so they can work on correcting their data. Feel free to modify, 
               add, or remove anything as you see fit. For example, you may want 
               your users to only address High Priority issues right now. You can
               easily remove any tabs that may distract your users from that goal.
               Please note that Overlaps will be shown in the 'Warnings' tab and
               again in the 'Overlap Detail' tab of the download. This is so
               your users have enough detail to track down each issue.</p>
               <p>Note that protected personal information (PPI), such as Personal
               ID in combination with other data elements, is contained in the
               Excel downloads. Users must follow all applicable HMIS privacy
               and security policies when storing, transmitting, and disclosing
               files with client records.</p>
               
               <h4>Some definitions:</h4>
               <p>This app categorizes every issue it finds in your data set in
               terms of its severity.</p>
               <ul>
               <li>High Priority Errors
                <ul>
                  <li> Always indicates a data quality issue that can and should
                      be fixed in HMIS.</li>
                  <li> Aim for 0 High Priority errors.</li>
                  <li> These errors affect multiple federal reports in a
                      fundamental way and thus should be prioritized.</li>
                </ul></li>
               <li>General Errors
                <ul>
                  <li> Always indicates a data quality issue that can and should
                      be fixed in HMIS.</li>
                  <li> Aim for 0 General Errors.
                </ul></li>
               <li>Warnings
                <ul>
                  <li> May be a data quality issue, but may also be an unexpected
                      situation that reflects reality.</li>
                  <li> Do not aim for 0 Warnings. It is ok and expected to have
                      some warnings.</li>
                  <li> End users should check that any data being flagged as a
                      Warning is accurate in their HMIS. If it is not accurate,
                      then it should be corrected. If it is accurate, it should
                      be left as is.</li>
                </ul></li>
               </ul>
               <p>Regardless of an issue's categorization, <b>users should never
               edit data that accurately reflects reality</b>.
               
               <h4>Organization-wide HMIS Data Quality Plots</h4>
               <p>For each type of issue (High Priority Errors, General Errors,
               and Warnings) you will find two plots: one graphing the number of
               issues by type, and one graphing the number of issues by project.

               <h4>Top 10 Issues</h4>
               <p>Across all the projects within the selected Organization, this
               plot shows the <b>top 10</b> issues identified. This can be
               useful in planning targeted HMIS training efforts.</p>
          
               <h4>Top 10 Projects</h4>
               <p>These plots show the <b>top 10</b> projects within the selected
               organization with the highest number of issues identified. You can
               use this to help determing which projects may need extra assistance
               in addressing their data quality issues.</p>
               
               <h4>Download Organization-wide HMIS Data Quality Data</h4>
               <p>To download all of the client and enrollment related issues
               found in the selected Organization, click the Download button.
               This will give HMIS admins a way of communicating to an Organization
               what kinds of HMIS data quality issues they have.</p>")

tabSystemOverview_instructions <-  HTML("<h4>System Performance Overview</h4>
                <p>The System Performance Overview page in Eva features three system 
                performance panels, each with their own set of charts: the System Flow Panel, the Client System Status 
                Panel, and the System Demographics Panel. The charts on these panels display system 
                performance data, pulled from your uploaded HMIS CSV export, from 
                all HMIS Continuum projects, excluding homeless prevention projects. 
                Eva uses the last 12 full months of data in the upload, which constitutes 
                the report period. <b>Note that some charts on this page may not display 
                if the uploaded HMIS CSV export has less than 12 full months of data.</b></p>
                
                <p>The purpose of the system performance charts is to use your HMIS 
                data to (1) evaluate how effective your homeless system is in moving 
                clients through the system and helping them reach permanent housing, 
                and (2) help you understand the demographic composition of all clients 
                served in your homeless system.</p>
                
                <p>Use the Filters Menu to explore system performance trends of 
                clients in your homeless system with specific characteristics. 
                This has two components: </p>
                
                <ul style='list-style-type:none'>
                  <li>1) <b>The universal filters</b>, the top row of the Filters Menu, 
                  impact the data shown on all three visualizations on this page. 
                  Universal filters include Household Type, Level of Detail, Project 
                  Type Group, and Race/Ethnicity Methodology Type.</li>
                  <li>2) <b>The demographic filters</b>, the bottom row of the Filters Menu, 
                  only impact the data shown in the System Flow and Client System 
                  Status charts. Demographic filters include Age, Veteran Status, 
                  and Race/Ethnicity.</li>
                </ul>
                
                <p>Use the drop-down menus to select the characteristics of the 
                system subpopulation you want to analyze. The default selection 
                is all clients in your homeless system throughout the report period. 
                To see system performance by households, select the \"Head of Households 
                only” level of detail. All filters (except one) are single-select, 
                meaning you can only select one category at a time. For the Age 
                filter, you can select multiple age ranges to explore.</p>
                
                <p>Please note that household type and age group filters use different 
                methods for calculating a client's age. Household type is based on all 
                household members’ ages as of the entry date of their earliest enrollment 
                included in the report period. Age group is determined based on the client’s 
                age as of the entry date of their last enrollment included in the report period. 
                Because of this reporting difference, it is possible for a client that ages from 
                24 to 25 during the report period to be categorized in the Adult Only 18-24 
                household type while also being categorized as in the 25-34 age group.</p>
                
                <p>The Race/Ethnicity Methodology Type selection only impacts the 
                Race/Ethnicity filters. To learn more about methodology and demographic 
                categories, please visit the Glossary accessible on Eva's Navigation Menu.</p>
                
                <p>The system performance panels are beneath the Filters Menu. Under each Panel tab are Chart subtabs and an Information subtab. 
                The Information subtab includes a “Chart Overview” section that provides guidance on how to read the charts. Additionally, some 
                panels have an “Interpretation Tips” section that can help you interpret their output. </p>
                
                <h4>Downloads</h4>
                <p>To support further systems analysis, local reporting, and presentations, 
                Eva includes three System Performance Overview export options. The data in these exports reflect the clients that meet the 
                characteristics of the system subpopulation you selected with the Filters Menu.<p>
                <ul style='list-style-type:none'>
                <li>(1) To generate an Excel workbook with the aggregate data for a specific panel, click the \"Data Download\" button while viewing the chart.  </li>
                <li>(2) To generate a PowerPoint slide deck with the image(s) for a specific panel, click the \"Image Download\" button while viewing the panel. </li>
                </ul>
                
                <p>To generate an Excel workbook with all client data used for all of the System Performance Overview Charts, click the \"Client Level Download\" button. </p>
                
                <h4>Data Suppression and Data Security</h4>
                <p>To ensure the privacy and protection of individuals and small 
                population groups, Eva uses varying levels of data suppression. 
                If the total number of clients within a chart is less than 11, the 
                chart will not display. When this happens, you may need to broaden 
                your filter selections or upload a larger dataset to ensure there 
                is enough data to view the chart. Image and Data Downloads are unavailable for charts that are fully suppressed.</p>
                
                <p>The Client Level Download is always available with unsuppressed data, even when the total number of clients across all charts is less than 11.</p> 

                <p>Use caution when saving and sharing the Excel workbooks. Clients can become more identifiable in the data with smaller numbers, even if the data is in aggregate. 
                Before sharing, feel free to modify, add, or remove anything as you see fit to preserve client anonymity. </p>

                <p>The client data in the Client Level Download is easily identifiable as it contains Personal IDs, demographic information, and enrollment dates. 
                We recommend not sharing this Excel workbook with anyone who does not have permission to view client PII.</p>   
               ")

tab_sys_inflow_outflow_subtabs_information <-          HTML("<h4>Chart Overview</h4>
                                <p>The System Flow panel shows your homeless system's inflow and outflow during the period, 
                                helping you assess the effectiveness of your homeless system. 
                                The client universe for this panel is the number of clients identified as active in your system 
                                at the start of the report period plus the number of clients that inflowed into your system 
                                during the period. The System Flow panel contains a set of three charts: the Summary Chart, 
                                the Detail Chart, and the Month-by-Month Chart.
                                The charts are read from left to right. </p>
                                <br>
                                <h4>Summary and Detail Charts</h4>
                                <p>The Summary and Detail Charts show the total number of clients active in the system at the start and end of the period and whether they are homeless at that time or housed (and still receiving assistance). The Summary Chart shows the inflow and outflow of clients that occurred throughout the period. The Detail Chart breaks down inflow and outflow into several categories. </p>
                                
                                <ul>
                                  <li><b>Inflow</b> is categorized into three groups: 
                                  \"First-Time Homeless,\" \"Returned from Permanent,\" and 
                                  \"Re-engaged from Non-Permanent.\"</li>
                                  <li><b>Outflow</b> is divided into three categories: 
                                  \"Exited, Non-Permanent,\" \"Exited, Permanent,\" and \"Inactive.\"</li>
                                </ul>
                                
                                <p>In the Summary and Detail Charts, the Total Change value: </p><br>
                                <pre><code>Total Change = Inflow value(s) - Outflow value(s)</code></pre><br>

                                <p>represents the Outflow value(s) minus Inflow value(s). A negative Total Change value means more clients left your system than flowed into your system. A positive Total Change value means more clients flowed into your system than left your system.</p>
                                <br>
                                <h4>Month-by-Month Chart</h4>
                                <p>The Month-by-Month Chart, which includes a stacked bar chart and a data table, shows Inflow and Outflow counts by month over a 12-month period. The stacked bar chart visually compares, by month, the total count of people that enter the homeless system <b>(Inflow)</b> or are continuingly experiencing homelessness in the system <b>(Active at Start: Homeless)</b> with the total count of people that exited the system <b>(Outflow)</b> or are permanently housed within the system <b>(Active at End: Housed)</b> all within that given month.</p>
                                
                                <p>The actual counts of each category in a given month are listed in a table below the chart, including the Monthly Change value. The <b>Monthly Change</b> value:</p>
                                <br>
                                <pre><code>Monthly Change = Inflow (for a given month) - Outflow (for a given month)</code></pre>
                                <br>
                                <p>A negative Monthly Change value means more clients left your system than flowed into your system during a given month. A positive Monthly Change value means more clients flowed into your system than left your system during a given month. The months that have the most inflow and the most outflow are colored in the table.</p>
                                
                                <p>The <b>Average Monthly Change, Average Monthly Inflow, and Average Monthly Outflow</b> values have the same underlying calculation represented by:</p>
                                <br>
                                <pre><code>Average Monthly [Value] = (Month Value 1 + Month Value 2 + ...) / 12</code></pre>
                                <br>
                                <p>A negative Average Monthly Change value means, on average, more clients left your system than flowed into your system each month. A positive Average Monthly Change value means, on average, more clients flowed into your system than left your system each month. </p>
                                <br>
                                <p>Note: A client may be counted more than once in the Month-by-Month Chart. For example, a client who outflows in January and inflows again two months later in March would be counted in both January and March.</p>
                                <br>
                                <p>The Month-by-Month Chart also has Flow Type Filters for viewing \"First-Time Homeless\" and \"Inactive\" client counts. The default Flow Type Filter is \"All\" which shows the Inflow, Outflow, and baseline numbers together.</p>
                                <br>
                                
                                <h4>Interpretation Tips</h4>
                                <p>This section provides general tips on how to 
                                interpret the chart. Depending on the data you uploaded, 
                                some of the items below may not apply.</p>
                                <br>
                                <table class='sys_info_table' id='sys_flow_info_table'>
                                  <tr>
                                    <th>Scenario</th>
                                    <th>What You See</th>
                                    <th>What It Means</th>
                                  </tr>
                                  <tr>
                                    <td>Less than 36 months of data are uploaded</td>
                                    <td>In the Detail chart, \"Inflow Unspecified\" 
                                    displays instead of \"First-Time Homeless.”</td>
                                    <td>The \"First-Time Homeless” category refers 
                                    to someone who has not been served in the system 
                                    within the 24 months prior to their entry. Therefore, 
                                    it is not possible to assess if people are newly 
                                    homeless or returners/re-engagers without a 36-month 
                                    dataset. Thus, because of the shorter timeframe 
                                    of your export, the number of returners/re-engagers 
                                    may be an undercount.</td>
                                  </tr>
                                  <tr>
                                    <td>Less than 12 months of data are uploaded</td>
                                    <td>In the Detail chart, \"Inflow Unspecified\" 
                                    displays instead of \"First-Time Homeless.”</td>
                                    <td>The \"First-Time Homeless” category refers 
                                    to someone who has not been served in the system 
                                    within the 24 months prior to their entry.
                                    Therefore, it will be difficult to draw conclusions about 
                                    whether changes in inflow/outflow are meaningful. 
                                    For instance, change in inflow/outflow over a 
                                    4-month period may reflect expected seasonal shifts 
                                    instead of a difference in system performance. 
                                    For a fuller and more complete picture of your 
                                    system, please use a file that has at least 36 
                                    months of data.</td>
                                  </tr>
                                  <tr>
                                    <td>Total Inflow is greater than total Outflow</td>
                                    <td>In the Summary chart, the bar for Inflow 
                                    is larger than the bar for Outflow. The Total 
                                    Change value is a positive number, representing an increase.</td>
                                    <td>This means there were more clients that came into your system 
                                    than left your system during the reporting period. 
                                    Compare with results from prior years to see 
                                    if more clients are coming into the system than 
                                    in prior years, or if the change is because 
                                    fewer clients are exiting. Use the Detail Chart 
                                    to explore if a majority of the clients flowing 
                                    in were first-time homeless, returning to homelessness 
                                    after previously exiting to a permanent destination, 
                                    or re-engaging with the system after previously 
                                    exiting to a non-permanent destination.</td>
                                  </tr>
                                  <tr>
                                    <td>Total Outflow is greater than total Inflow</td>
                                    <td>In the Summary chart, the bar for Outflow
                                    is larger than the bar for Inflow. The Total 
                                    Change value is a negative number, representing a reduction.</td>
                                    <td>This means there were more clients that left your system than 
                                    came into your system during the reporting period.</td>
                                  </tr>
                                  <tr>
                                    <td>The largest Outflow category is \"Non-Permanent Destination”</td>
                                    <td>In the Detail chart, the bar for \"Non-Permanent 
                                    Destination” is larger than the bar for \"Permanent 
                                    Destination” and the bar for \"Inactive.”</td>
                                    <td>This means most clients leaving your system 
                                    are exiting to temporary or unknown destinations. 
                                    Check your completion rate for exit destination 
                                    to see if any corrections to unknown destinations 
                                    are possible. To inform strategies for improving 
                                    performance, filter to look at results for more 
                                    specific groups, to see if there are differences 
                                    in the rate of exits to temporary destinations.</td>
                                  </tr>
                                  <tr>
                                    <td>The largest Outflow category \"Inactive”</td>
                                    <td>In the Detail chart, the bar for \"Inactive”
                                    is larger than the bar for \"Permanent 
                                    Destination” and the bar for \"Non-Permanent Destination.”</td>
                                    <td>This means many ended the report period 
                                    with (1) an open enrollment in an Emergency 
                                    Shelter – Night-by-Night project that has not 
                                    had a bed night recorded within the last 15 
                                    days of the report period, (2) an open enrollment 
                                    in Street Outreach, Day Shelter, Supportive 
                                    Services, and Other project type enrollments 
                                    without a Current Living Situation (CLS) record 
                                    within the last 60 days of the report period, 
                                    or (3) an open enrollment in Coordinated Entry 
                                    without a CLS record within the last 90 days 
                                    of the report period.</td>
                                  </tr>
                                </table>")

tab_sys_status_subtabs_information <-    HTML("<h4>Chart Overview</h4>
                                <p>The Client System Status Chart shows the end-of-year 
                                housing status of the clients that were active in 
                                your homeless response system at the start of the 
                                period. This chart helps you identify the proportion 
                                of clients that ended the period as (1) homeless 
                                or (2) housed or in permanent housing. The client 
                                universe for this chart is the number of clients 
                                active in your system at the start of the report 
                                period. This chart does not include clients that 
                                inflowed into your system after the start of the 
                                report period.</p>
                                <br>
                                <p>The left-hand bar labeled \"Period Start” in the 
                                chart shows the status of clients active/enrolled 
                                in your system at the start of the period; clients 
                                are identified as either \"Homeless” or \"Housed.” 
                                The right-hand bar labeled \"Period End” in the chart 
                                shows the status of these clients at the end of 
                                the period. Clients are categorized into five system 
                                statuses at the end of the period: \"Exited, Non-Permanent,” 
                                \"Enrolled, Homeless,” \"Inactive,” \"Exited, Permanent,” 
                                and \"Enrolled, Housed.”</p>
                                <br>
                                <p>In the area of the figure between the two bars, 
                                the Client System Status Chart depicts the change 
                                of these clients from their status at the start 
                                of the period to their status at the end of the 
                                period through visible linkages that connect the 
                                two bars. The width of each linkage represents the 
                                proportion of clients that make up that linkage. 
                                Meaning, the thicker the linkage, the larger proportion 
                                of clients it represents.</p>
                                <br>
                                <h4>Interpretation Tips</h4>
                                <p>This section provides general tips on how to 
                                interpret the chart. Depending on the data you 
                                uploaded, some of the items below may not apply.</p>
                                <br>
                                <table class='sys_info_table' id='sys_status_info_table'>
                                  <tr>
                                    <th>Scenario</th>
                                    <th>What You See</th>
                                    <th>What It Means</th>
                                  </tr>
                                  <tr>
                                    <td>The sum of \"Enrolled, Housed” and \"Exited, 
                                    Permanent” is greater than the sum of the 
                                    remaining categories at Period End</td>
                                    <td>The bars for \"Enrolled, Housed” and \"Exited, 
                                    Permanent” combined look larger than the bars 
                                    for the remaining categories in the chart.</td>
                                    <td>This means the majority of clients who were 
                                    active in your system at the start of the report 
                                    period exited to or retained permanent housing 
                                    by the end of the report period.</td>
                                  </tr>
                                  <tr>
                                    <td>The sum of \"Enrolled, Homeless” and 
                                    \"Exited, Non-Permanent” is greater than the 
                                    sum of the remaining categories at Period End</td>
                                    <td>The bars for \"Enrolled, Homeless” and 
                                    \"Exited, Non-Permanent” combined look larger than the bars 
                                    for the remaining categories in the chart.</td>
                                    <td>This means the majority of clients who were 
                                    active in your system at the start of the report 
                                    period either exited to homeless, temporary, 
                                    or unknown destinations or remained homeless 
                                    by the end of the report period. Check your 
                                    completion rate for exit destination to see 
                                    if any corrections to unknown destinations 
                                    are possible.</td>
                                  </tr>
                                  <tr>
                                    <td>Clients who were active in the system at
                                    Period Start are inactive at Period End</td>
                                    <td>The category \"Inactive” is display in the 
                                    chart at Period End.</td>
                                    <td>This means some clients ended the report 
                                    period with (1) an open enrollment in an Emergency 
                                    Shelter – Night-by-Night project that has not 
                                    had a bed night recorded within the last 15 
                                    days of the report period, (2) an open enrollment 
                                    in Street Outreach, Day Shelter, Supportive 
                                    Services, and Other project type enrollments 
                                    without a Current Living Situation (CLS) record 
                                    within the last 60 days of the report period, 
                                    or (3) an open enrollment in Coordinated Entry 
                                    without a CLS record within the last 90 days 
                                    of the report period.</td>
                                  </tr>
                                </table>")

tab_sys_comp_subtabs_information <-   HTML("<h4>Chart Overview</h4>
                                <p>The System Demographics Chart shows the demographic 
                                make-up of your homeless system and highlights 
                                important trends among various demographic groups. 
                                The client universe for this chart is the number 
                                of clients identified as active in your system at 
                                the start of the report period plus the number of 
                                clients that inflowed into your system.</p>
                                <br>
                                <p>Under the chart tab are five demographic categories 
                                you can choose from: Age, 
                                All Races/Ethnicities, a second race/ethnicity 
                                option, and Veteran Status. Please note, the second 
                                race/ethnicity option differs for each
                                Race/Ethnicity Methodology Type selection you made 
                                earlier on the Filter Menu.</p>
                                <br>
                                <p>For a simple count of totals within a demographic 
                                category, select only one category. To see the 
                                intersection of two demographic categories, select 
                                both categories to create a crosstab chart. To change 
                                your crosstab selection, uncheck at least one of 
                                your previous selections before selecting a new 
                                category. Please note that you can only select one 
                                race/ethnicity category to display in the chart 
                                at a time.</p>
                                <br>
                                <p>Each cell in the chart is a unique combination 
                                of demographic characteristics. For example, if 
                                you selected Age and Race/Ethnicity, a unique demographic 
                                combination would be \"25 to 34” and \"Black alone.” Any cell with a count is shaded. 
                                The darker the color in a cell, the greater the 
                                value of that cell.</p>
                                <br>
                                <p>Please note that household type and age group filters use different 
                                methods for calculating a client's age. Household type is based on all 
                                household members’ ages as of the entry date of their earliest enrollment 
                                included in the report period. Age group is determined based on the client’s 
                                age as of the entry date of their last enrollment included in the report period. 
                                Because of this reporting difference, it is possible for a client that ages from 
                                24 to 25 during the report period to be categorized in the Adult Only 18-24 
                                household type while also being categorized as in the 25-34 age group.</p>
                                <br>
                                <h4>Data Suppression</h4>
                                <p>Additional levels of data suppression apply to 
                                the System Demographics Chart.</p>
                                <br>
                                <ul>
                                  <li>Any value less than 11 is suppressed, including totals.</li>
                                  <li>If there is only one suppressed value within 
                                  a row or column, the next highest value is also suppressed.</li>
                                  <li>If all individual cells in the chart have 
                                  values of less than eleven (11), the chart will not display.</li>
                                </ul>
                                
                                <p>All suppressed values are represented by *** in the chart.</p>
                                <br>
                                <p>Please note that while data can be suppressed 
                                in the System Demographics chart in Eva and in its 
                                image download, the data in the chart’s data download 
                                will not be suppressed. Be careful how you save 
                                and share the data download, which is an Excel export. With smaller numbers, 
                                clients can become more identifiable in the data. 
                                Before you share the Excel export, feel free to modify, 
                                add, or remove anything as you see fit to preserve 
                                client anonymity.</p>
                                
                                ")

tabDQSystem_instructions <-  HTML("
               <h4>System-wide HMIS Data Quality</h4>
               <p>Use your System-wide Data Quality data to evaluate which
               organizations may benefit from additional assistance and where
               extra training may be needed. You can download this data to use
               for reporting to interested entities about your overall HMIS
               system data quality.</p>
               <p>Click the Download button to generate an Excel workbook with
               the your entire system's Data Quality data. Feel free to modify,
               add, or remove anything as you see fit. For example, if you are
               sending this workbook to your CoC management, you may want to 
               remove the tabs that have client-level data on them. 
               <p>Review the plots below to identify the organizations that
               you want to examine more closely in the <b>Data Quality > 
               Organization-level</b> tab. </p>
               <h4>Some definitions:</h4>
               <p>This app categorizes every issue it finds in your data set in
               terms of its severity.</p>
               <ul>
               <li>High Priority Errors
                <ul>
                  <li> Always indicates a data quality issue that can and should
                      be fixed in HMIS.</li>
                  <li> Aim for 0 High Priority errors.</li>
                  <li> These errors affect multiple federal reports in a
                      fundamental way and thus should be prioritized.</li>
                </ul></li>
               <li>General Errors
                <ul>
                  <li> Always indicates a data quality issue that can and should
                      be fixed in HMIS.</li>
                  <li> Aim for 0 General Errors.
                </ul></li>
               <li>Warnings
                <ul>
                  <li> May be a data quality issue, but may also be an unexpected
                      situation that reflects reality.</li>
                  <li> Do not aim for 0 Warnings. It is ok and expected to have
                      some warnings.</li>
                  <li> End users should check that any data being flagged as a
                      Warning is accurate in their HMIS. If it is not accurate,
                      then it should be corrected. If it is accurate, it should
                      be left as is.</li>
                </ul></li>
               </ul>
               <p>Regardless of an issue's categorization, <b>users should never
               edit data that accurately reflects reality</b>.
               
               <h4>System-wide HMIS Data Quality Plots</h4>
               <p>For each type of issue (High Priority Errors, General Errors,
               and Warnings) you will find two plots: one plots the counts of
               issues and one plots the number of issues by organization.</p>
               
               <h5><b>Top 10 Issues</b></h5>
               <p>Across all of the organizations in your upload, this plot shows
               the <b>top 10</b> issues identified in the data quality scan. This
               result can help to focus future end-user trainings and bring to
               light any potential considerations in your federal or local
               reporting and analysis.</p>
               
               <h5><b>Top 10 Organizations</b></h5>
               <p>These plots show the <b>top 10</b> organizations across your
               system with the highest number of issues identified. You can use
               these plots to help determine which organizations may need extra
               assistance in getting their HMIS Errors/Warnings resolved.</p>
               
               <h4>Download System-wide HMIS Data Quality Data</h4>
               <p>To download all of the client and enrollment related issues
               found in your system, click the Download button. This will give
               HMIS admins a way of reporting to interested entities, such as
               your CoC leadership, a broader view of the state of your HMIS
               data quality.</p>")

tabDQExport_instructions <- HTML("<h4>Data Quality Export Interface</h4>
                                 <p>The Data Quality Export Interface provides a centralized location to download all data quality reports generated by Eva, eliminating the need to visit individual pages. Data quality reports available for export include those from the Project Dashboard, Project Descriptor Data, System-level Data Quality, and Organization-level Data Quality pages. These reports help verify the accuracy, completeness, and timeliness of HMIS data entry across projects and organizations. 
                                 On this page, users can customize their export by selecting the scope (organization-level or system-level), choosing specific reports to download, and applying date filters where applicable. </p>
                                 <h5>Select Dates</h5> 
                                 <p>Specify a date range or single date you want to see reported. This filter applies only to the Project Dashboard Report. All other reports will use the full date range of the HMIS CSV Export. </p>
                                 <ul>
                                  <li>Date Range: Allows users to specify a start and end date to focus on a particular timeframe within the export period. </li>
                                  <li>Single Date: Displays data as of a specific date. </li>
                                 </ul>
                                 <p>Use this option to review project status or timeliness metrics for a defined period rather than the entire export. </p>
                                 
                                 <h5>Select Export Type </h5>
                                 <p>Choose whether to generate reports at the organization level, the system level, or both: </p>
                                 <ul>
                                  <li>Organization-level: Select one or more organizations to view data quality metrics specific to those entities. </li>
                                  <li>System-level: Includes all projects in the HMIS CSV Export for a comprehensive view. </li>
                                 </ul>
                                 <p>This flexibility allows users to analyze data quality for individual organizations or across the entire system. </p>
                                 
                                 <h5>Select Data Quality Exports</h5> 
                                 <p>Users can select one or more of the following reports: </p>
                                 <ul>
                                  <li>Project Dashboard Report: Provides client counts, enrollment statuses, and timeliness metrics for selected projects. </li>
                                  <li>PDDE Report: Displays data quality checks related to project descriptor data elements. </li>
                                  <li>Data Quality Report: Summarizes missing or invalid data elements across client records. </li>
                                 </ul>
                                 <p>Selecting All Data Quality Reports will include all available report types in the export. </p>
                                 
                                 <h5>Download Reports</h5>
                                 <p>After setting your export parameters, click the “Download” button to export the chosen reports. Selected reports will be delivered in a .zip file. Depending on the selected export types, the .zip file may contain: 
                                 <ul>
                                  <li>A folder for each selected organization with organization-level data quality reports </li>
                                  <li>A system-level folder with system-wide data quality reports </li>
                                 </ul>
                                 <p>The reports are in Excel format for easy review and analysis. You can send an organization folder to authorized HMIS users at that organization so they can work on correcting their data. 
                                 
                                 Note that protected personal information (PPI), such as Personal ID in combination with other data elements, is contained in the Project Dashboard and Data Quality Reports. Users must follow all applicable HMIS privacy and security policies when storing, transmitting, and disclosing files with client records. </p>")

tabGlossary_instructions <- HTML("
               <p>This glossary provides definitions for the terms used throughout 
              Eva's System Performance Overview page. You can review definitions 
              of the terms by their focus, including:</p>
              
              <ul>
                <li>System Performance Filters</li>
                <li>System Flow Chart</li>
                <li>Client System Status Chart</li>
              </ul>
              
              <p>You can also search for a specific term using the search bar.</p>")

tabChangelog_instructions <-  HTML("
              <p>This tab will list the most recent technical updates and
              changes to Eva. For more in-depth information on current and past
              issues, please go to <a
              href='https://github.com/abtassociates/eva/issues' target= '_blank'
              rel='noopener noreferrer'>GitHub</a>.</p>
            ")
