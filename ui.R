dashboardPage(
  skin = "black",
  dashboardHeader(title = "Eva"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Home",
               tabName = "tabHome"),
      menuItem("Upload HMIS CSV Export",
               tabName = "tabUpload"),
      menuItem("Edit Local Settings",
               tabName = "tabLocalSettings"),
      menuItem("View Client Counts",
                  tabName = "tabClientCount"),
      menuItem("Assess Data Quality",
               menuSubItem("Check PDDEs",
                           tabName = "tabPDDE"),
               menuSubItem("System-level",
                           tabName = "tabDQSystem"),
               menuSubItem("Organization-level",
                           tabName = "tabDQOrg")),
      menuItem("View Changelog",
               tabName = "tabChangelog")
        # menuSubItem("Data Entry Timeliness", 
        #             tabName = "tabDeskTime")
      # ),
      # menuItem("System Analysis",
      #          menuSubItem("System Flow",
      #                      tabName = "tabSystemFlow"),
      #          menuSubItem("Outcomes",
      #                      tabName = "tabOutcomes"),
      #          menuSubItem("Referrals",
      #                      tabName = "tabReferrals"),
      #          menuSubItem("Assessments",
      #                      tabName = "tabAssessments"),
      #          menuSubItem("Diversion",
      #                      tabName = "tabDiversion"),
      #          menuSubItem("Prioritization",
      #                      tabName = "tabPrioritized"))
    )
  ), 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$html(lang="en"), #Added as WAVE fix but not considered ideal
      tags$script(HTML("function idleTimer() {
          var timeoutTime = 600000;
          var t = setTimeout(logout, timeoutTime);
          window.onmousemove = resetTimer; // catches mouse movements
          window.onmousedown = resetTimer; // catches mouse movements
          window.onclick = resetTimer;     // catches mouse clicks
          window.onscroll = resetTimer;    // catches scrolling
          window.onkeypress = resetTimer;  //catches keyboard actions
    
          function logout() {
            alert('Your session timed out. Your data has been cleared, please re-upload.');
            Shiny.setInputValue('timeOut', 1)
          }
    
          function resetTimer() {
            clearTimeout(t);
            t = setTimeout(logout, timeoutTime); 
          }
        }
        idleTimer();"
      ))
    ),
    useShinyjs(),
    disconnectMessage(
      text = str_squish(
        "Eva has crashed. Please submit an issue on GitHub and note the
          date and time in order to help the team diagnose the issue."
      )), 
    tabItems(
      tabItem(
        tabName = "tabHome",
        fluidRow(
          box(
            width = 12,
            HTML("<h2>Welcome to Eva!</h2>
              <p><b>Eva</b> is an <a href = 'https://github.com/abtassociates/eva'
              target= '_blank' rel='noopener noreferrer'>open-source</a>
              project intended for local use by HMIS Administrators in Continuums
              of Care (CoCs) around the U.S. and its territories. Eva is designed
              to help you assess the accuracy and completeness of the data within
              your HMIS. In future iterations it will also assist communities in
              analyzing HMIS performance data, including coordinated entry, if 
              your community utilizes HMIS for this purpose. Use of this tool is
              not required by HUD.</p>"
            ),
            actionButton("Go_to_upload","Click here to get started")
          ),
          box(
            title = "Instructions",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            HTML(
              "<p>Eva works by uploading a hashed 
              <a href='https://www.hudhdx.info/Resources/Vendors/HMIS_CSV_Specifications_FY2022_v1.3.pdf'
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
              ready to engage with Eva. Navigate to the \'Upload HMIS CSV Export\' tab
              and follow the instructions there.</p>
              
              ")
          ),
          box(
            title = "Need help?",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            HTML(
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
          ), 
          box(
            title = "Citations and Special Thanks",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            HTML("
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
                 Square Peg Data</a>, the San Diego City and County CoC (CA-601),
                 and the Minneapolis/Hennepin County CoC (MN-500) for providing
                 sample datasets to support programming.")
          )
        )
      ), 
      tabItem(
        tabName = "tabUpload",
        fluidRow(box(htmlOutput("headerUpload"), width = 12)),
        fluidRow(
          box(
            title = "Instructions",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            HTML("
                 <h4>Upload hashed HMIS CSV Export</h4>
                 
              <p>To upload your hashed HMIS CSV Export, click the \'Browse\'
              button. Once you find the zip file on your computer, select it 
              and click \'Open\'. Your file will begin uploading. Eva will check 
              to determine if the export is hashed. If it is not, Eva will reject 
              the file with an error message, and clear Eva's memory until you 
              upload a hashed HMIS CSV Export.</p>
              
              <h4>HMIS CSV Export File Structure Analysis</h4>
              <p>Once Eva verifies that your export is hashed, it will check
              that the files have all the right names, columns, data types, and
              allowable values. Eva will generate data quality issues that are
              categorized as high priority errors, general errors, and warnings.
              If there are any high priority errors that prevent Eva from
              functioning, Eva will reject your upload, stop processing the export,
              and clear Eva's memory.</p>
              <p>All issues will display in the HMIS CSV File Structure Analysis
              panel, where you can download the details, even if the file
              was rejected. Users should contact their vendor to resolve high 
              priority errors identified in the HMIS CSV Export File Structure
              Analysis, as well as any other structural issues which you feel need
              to be corrected. Not all structural issues found in this analysis will 
              prevent the data from being accepted for analysis, so they may not 
              require immediate attention. Once your vendor has addressed any
              high priority structural errors, you can attempt another upload.</p>
              
              <p>Once you have uploaded a hashed and structurally sound zip file,
              you will see a confirmation that your upload was successful, the date
              range of the files you uploaded, plus the date your Export was 
              downloaded from your HMIS.
              ")
          ),
          box(
            fileInput("imported",
                      label = NULL,
                      multiple = FALSE,
                      accept = ".zip"),
            uiOutput("fileInfo"),
            width = 12
          ),
          #,
          # HTML(
          #   "<h4>Referrals</h4>
          #   <p>Please enter the number of days your CoC would consider a Referral
          #     to be \"outstanding\"."
          # ),
          # numericInput(inputId = "OutstandingReferrals",
          #              label = "Outstanding Referral Days:",
          #              value = 7)
          
          box(
            title = "HMIS CSV Export File Structure Analysis",
            width = 12,
            DT::dataTableOutput("integrityChecker"),
            p(),
            HTML("<p>Users should contact their vendor to resolve high priority 
            errors identified in the HMIS CSV Export File Structure Analysis, as
            well as any other structural issues which you feel need to be corrected.
            </p>"),
            p(),
            uiOutput('downloadIntegrityBtn')
          )
        )
      ),
      tabItem(
        tabName = "tabLocalSettings",
        fluidRow(box(htmlOutput("headerLocalSettings"), width = 12)),
        fluidRow(
          box(
            title = "Instructions",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            HTML("
              <p>To make Eva reporting more useful at the local level, you can
              adjust the local settings to better analyze your data in a
              way that is meaningful to the CoC. To edit these, click on the 
              Edit Local Settings tab. If you do not edit them, the reporting will 
              use the defaults listed. These defaults do not imply any HUD 
              recommendations. Please read the description in the
              Edit Local Settings tab for more information.</p>
              ")
          ),
          box(
            title = "Outstanding Referrals",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            fluidRow(box(
              width = 12,
              HTML(
                "<p>This check aims to help communities find Coordinated Entry
                Event referrals that may be outstanding and missing a Result
                Date. (Note that not all CE Events have an associated Result and
                Result Date.) First, the tool calculates the number of days each
                referral has been open (the number of days between the Referral
                Date and the date your upload was exported from your HMIS). Then
                the check compares the length of each referral with assumptions
                entered about the expected maximum period of assistance envisioned
                for the CoC's Coordinated Entry Referral process. This check is
                for all project types that may have Coordinated Entry Event
                referrals.

                <p>Any data quality flags about Outstanding Referrals is
                categorized as a Warning and is a suggestion to verify that the
                identified referrals are still active or in progress. It does
                not imply that any data should be changed.

                <p>Below, you can specify the expected maximum period of
                assistance envisioned for the CoC's Coordinated Entry Referral
                process, meaning the timeframe after which you would want an
                organization to confirm the referral is still active. You can
                set these based on your current data or leave them at the
                defaults (these defaults do not imply any HUD recommendations)."
              ),
              numericInput(
                inputId = "CEOutstandingReferrals",
                label = "All Projects:",
                value = 30,
                min = 0,
                max = 3650,
                step = 5,
                width = "200px"
              )
            ))
          ), 
          box(
            title = "Long Stayers",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            fluidRow(
              box(
                HTML("
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
                assumptions for each project type. Any data quality flags about
                Long Stayers is categorized as a Warning and is a suggestion to
                verify that the identified clients are still active in these
                projects. It does not imply that any data should be changed.
      
                <p><b>Top 2% longest enrollments are flagged for the following
                project types:</b>
                <ul>
                <li>Coordinated Entry
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
                <li>Street Outreach
                <li>Services Only
                <li>Other
                <li>Day Shelter
                <li>Emergency Shelter - Night-by-Night
                </ul>
      
                <p>Below, you can specify the expected maximum period of assistance
                envisioned for the project type, meaning the timeframe after which you
                would want an organization to confirm the client is still active in the 
                project. You can set these based on your current data or leave them at
                the defaults (these defaults do not imply any HUD recommendations)."
                ),
                column(
                  numericInput(
                    inputId = "ESNbNLongStayers",
                    label = "Emergency Shelter (NbN only!):",
                    value = 90,
                    min = 0,
                    max = 3650,
                    step = 5,
                    width = "200px"
                  ),
                  numericInput(
                    inputId = "OtherLongStayers",
                    label = "Other:",
                    value = 90,
                    min = 0,
                    max = 3650,
                    step = 5,
                    width = "200px"
                  ),
                  numericInput(
                    inputId = "DayShelterLongStayers",
                    label = "Day Shelter:",
                    value = 90,
                    min = 0,
                    max = 3650,
                    step = 5,
                    width = "200px"
                  ),
                  width = 6
                ),
                column(
                  numericInput(
                    inputId = "OUTLongStayers",
                    label = "Street Outreach:",
                    value = 90,
                    min = 0,
                    max = 3650,
                    step = 5,
                    width = "200px"
                  ),
                  numericInput(
                    inputId = "ServicesOnlyLongStayers",
                    label = "Services Only:",
                    value = 90,
                    min = 0,
                    max = 3650,
                    step = 5,
                    width = "200px"
                  ),
                  width = 6
                ),
                width = 12
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tabClientCount",
        fluidRow(box(htmlOutput("headerClientCounts"), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("
               <h4>Client Counts Report</h4>
               <p>The Client Counts Report provides the number of households/clients
               who have been served in each project and their enrollment status
               at the time of CSV Export generation. This report can be used to
               verify that a project is up to date on their HMIS data entry by
               comparing counts reported with the number of households/clients
               that are known to be served in each project. Permanent housing
               projects can check that the number of households/clients who have
               not yet moved into housing is correct.</p>
               
               <h4>Inputs</h4>
               <p>Select a project from the drop list and adjust the the Date
               Range for the cohort of clients you want to see reported. The
               Date Range defaults to the date range covered by the HMIS CSV Export. 
               Users are encouraged to edit the Date Range as desired to see
               metrics for timeframes within the Export period, such as the
               number of households/clients who exited during that timeframe
               with and without a Move-In Date. Note that setting the Start Date
               to the Export End Date will show the current status for all
               enrollments for the project.</p>
               
               <h4>Summary</h4>
               <p>The Summary panel provides a count of households/clients who
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
               <p>In this panel you will see the Personal ID, Relationship to HoH,
               Entry Date, Move-In Date, Exit Date, and the Status for each client
               served by the selected project within the Date Range selected. The
               rows are ordered by Entry Date (oldest on top), Household ID (not
               visible), and Personal ID. This enables users to see the oldest
               enrollments first and groups clients in the same household
               together. All columns are searchable. For example, to find all
               enrollments with a Status of \'Active No Move-In Date\', you can
               type \'act\' in the Status search bar and the data table will
               react and filter in that way.")
        )), 
        fluidRow(box(
          dateRangeInput(
            "dateRangeCount",
            "Date Range",
            format = "mm/dd/yyyy",
            width = 300
          ),
          uiOutput("downloadClientCountsReportButton"),
          width = 12
        )),
        fluidRow(box(
          pickerInput(
            label = "",
            inputId = "currentProviderList",
            choices = NULL,
            width = "600px",
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = 'contains')
          ),
          width = 12,
          title = "Select Project",
          status = "info",
          solidHeader = TRUE
        )),
        fluidRow(box(
          title = "Client Counts Summary",
          status = "info",
          solidHeader = TRUE,
          DT::dataTableOutput("clientCountSummary"),
          width = 12
        )),
        fluidRow(box(
          title = "Client Counts Detail",
          status = "info",
          solidHeader = TRUE,
          DT::dataTableOutput("clientCountData"),
          width = 12
        ))
      ),
      tabItem(
        tabName = "tabPDDE",
        fluidRow(box(htmlOutput(
          "headerPDDE"
        ), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("
               <h4>Project Descriptor Data Element (PDDE) Check Summary</h4>
               <p>Once you have successfully uploaded an HMIS CSV Export, you
               will find a summary of each issue that was flagged in your data
               regarding your PDDEs. Please download the details by clicking the
               \'Download\' button.</p>
               
               <h4>Guidance</h4>
               <p>For a description of each issue found, check the Guidance 
               panel.</p>")
        )),
        fluidRow(
          box(
            id = "PDDESummaryOrganization",
            title = paste("PDDE Check Summary"),
            status = "info",
            solidHeader = TRUE,
            DT::dataTableOutput("pdde_summary_table"),
            width = 12,
            br(),
            uiOutput("downloadPDDEReportButton")
          ),
          box(id = "PDDEGuidance",
              DT::dataTableOutput("pdde_guidance_summary"),
              title = "Guidance",
              width = 12,
              status = "info",
              solidHeader = TRUE)
        )
      ),
      tabItem(
        tabName = "tabDQOrg",
        fluidRow(box(htmlOutput("headerDataQuality"), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("
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
        )), 
        fluidRow(box(
          pickerInput(
            label = "Select Organization",
            inputId = "orgList",
            choices = NULL,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = 'contains'),
            width = "100%",
            selected = "none"
          ),
          uiOutput("downloadOrgDQReportButton"),
          width = 12
        )), 
        
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "High Priority Errors",
            tabPanel("Top 10 Projects", uiOutput("orgDQHighPriorityErrorsByProject_ui")),
            tabPanel("Top 10 Issues", uiOutput("orgDQHighPriorityErrorByIssue_ui")),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "General Errors",
            tabPanel("Top 10 Projects", uiOutput("orgDQErrorsByProject_ui")),
            tabPanel("Top 10 Issues", uiOutput("orgDQErrorByIssue_ui")),
            width =12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "Warnings",
            tabPanel("Top 10 Projects", uiOutput("orgDQWarningsByProject_ui")),
            tabPanel("Top 10 Issues", uiOutput("orgDQWarningsByIssue_ui")),
            width = 12
          )
        ),
       
        fluidRow(
          box(
            id = "DQSummaryOrganization",
            title = paste("Data Quality Summary"),
            status = "info",
            solidHeader = TRUE,
            DT::dataTableOutput("dq_organization_summary_table"),
            width = 12
          )
        ),
        
        fluidRow(
          box(
            id = "DQSummaryProvider",
            DT::dataTableOutput("dq_org_guidance_summary"),
            title = "Data Quality Guidance",
            width = 12,
            status = "info",
            solidHeader = TRUE
          )
        )
      ),
      tabItem(
        tabName = "tabChangelog",
        fluidRow(box(HTML("<h2>Changelog</h2>"), width = 12)),
        fluidRow(
          box(
            width = 12,
            HTML("
              <p>This tab will list the most recent technical updates and changes to Eva.
              For more in-depth information on current and past issues, please go to <a
              href='https://github.com/abtassociates/eva/issues' target= '_blank'
              rel='noopener noreferrer'>GitHub</a>.</p>
            ")
          ),
          box(
            # collapsible = TRUE,
            # collapsed = TRUE,
            width = 12,
            tableOutput("changelog")
          )
        )
      ),
      tabItem(
        tabName = "tabDeskTime",
        fluidRow(box(htmlOutput("headerDeskTime"),
                     width = 12)),
          fluidRow(box(
            title = "Instructions",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            HTML("")
          )), 
        fluidRow(box(
          pickerInput(
            label = "Select Provider",
            inputId = "providerDeskTime",
            choices = NULL, #desk_time_providers,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = 'contains'),
            width = "100%",
            selected = NULL # desk_time_providers[1]
          ),
          width = 12
        )),
        fluidRow(box(plotOutput("DeskTimePlotDetail"), width = 12)), 
        fluidRow(
          box(
            uiOutput("deskTimeNote"),
            title = "More Information",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
          )
        )
      ),
      tabItem(
        tabName = "tabDQSystem",
        fluidRow(box(
          htmlOutput("headerSystemDQ"), width = 12, 
          uiOutput("downloadFullDQReportButton"))),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("
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
        )), 

        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "High Priority Errors",
            tabPanel("Top 10 Organizations", uiOutput("systemDQHighPriorityErrorsByOrg_ui")),
            tabPanel("Top 10 Issues", uiOutput("systemDQHighPriorityErrorsByIssue_ui")),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "General Errors",
            tabPanel("Top 10 Organizations", uiOutput("systemDQErrorsByOrg_ui")),
            tabPanel("Top 10 Issues", uiOutput("systemDQErrorByIssue_ui")),
            width =12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "Warnings",
            tabPanel("Top 10 Organizations", uiOutput("systemDQWarningsByOrg_ui")),
            tabPanel("Top 10 Issues", uiOutput("systemDQWarningByIssue_ui")),
            width = 12
          )
        )
      )
    )
  )
)


# tabItem(
#   tabName = "utilizationTab",
#   fluidRow(box(htmlOutput(
#     "headerUtilization"
#   ), width = 12)),
#   fluidRow(box(
#     pickerInput(
#       label = "Select Project",
#       inputId = "providerListUtilization",
#       choices = NULL, 
#       options = pickerOptions(liveSearch = TRUE,
#                               liveSearchStyle = 'contains'),
#       width = "100%"
#     ),
#     airDatepickerInput(
#       inputId = "utilizationDate",
#       label = "Report End Month for Annual Plot",
#       max = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
#       min = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date - days(335), unit = "month")),
#       dateFormat = "MM yyyy",
#       view = "month",
#       value = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
#       minView = "months",
#       addon = "none",
#       autoClose = TRUE,
#       width = '25%'
#     ),
#     width = 12
#   )),
#   plotlyOutput("bedPlot"),
#   br(),
#   fluidRow(
#     box(
#       uiOutput("bedNote"),
#       title = "What is Bed Utilization?",
#       collapsible = TRUE,
#       collapsed = TRUE
#     ),
#     box(
#       uiOutput("unitNote"),
#       title = "What is Unit Utilization?",
#       collapsible = TRUE,
#       collapsed = TRUE
#     ),
#     box(
#       uiOutput("utilizationNote"),
#       title = "Methodology",
#       collapsible = TRUE,
#       collapsed = TRUE
#     )
#   ),
#   fluidRow(box(
#     airDatepickerInput(
#       inputId = "utilizationDetailDate",
#       label = "Choose Month for Detail Data",
#       max = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
#       min = NULL, # ymd(floor_date(ymd(meta_HUDCSV_Export_End), "month") - years(2) + days(1)),
#       dateFormat = "MM yyyy",
#       view = "month",
#       value = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
#       minView = "months",
#       addon = "none",
#       autoClose = TRUE,
#       width = '50%'
#     ),
#     width = 12
#   )),
#   fluidRow(box(
#     infoBoxOutput("utilizationSummary0", width = '100%'),
#     infoBoxOutput("utilizationSummary1", width = '100%'),
#     infoBoxOutput("utilizationSummary2", width = '100%'),
#     width = 12
#   )),
#   fluidRow(box(
#     DT::dataTableOutput("utilizationDetail"), width = 12
#   ))
# ),
#)
