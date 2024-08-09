dashboardPage(
  title = "Eva",
  skin = "black",
  dashboardHeader(
    title = span(img(src = "Eva_logo_horizontal_white.png",
                                   height = 45)),
    # https://alvarotrigo.com/blog/toggle-switch-css/
    tags$li(class="dropdown",
            HTML('<div class="toggle-button-cover">
      <div class="button-cover">
        <div id="demo-label">
          <span>DEMO MODE </span>
          <i class="fas fa-circle-info demo-tooltip">
          <span class="demo-tooltiptext">
            <strong>Off</strong>: Upload your own HMIS CSV Export.
            <br>
            <br>
            <strong>On</strong>: Uses a demo HMIS CSV Export.
          </span>
          </i>
        </div>
        <div class="button r" id="button-1">
          <input id="isdemo" type="checkbox" class="checkbox" />
          <div class="knobs"></div>
          <div class="layer"></div>
        </div>
      </div>
    </div>
    <script>
    document.getElementById("isdemo").onchange = function() {
        Shiny.setInputValue("in_demo_mode", this.checked, {priority: "event"});
    }
    </script>'))
  ),
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
      menuItem("System Performance",
              menuSubItem("System Overview",
                           tabName = "systemOverview"),
              menuSubItem("System Exit Detail",
                           tabName = "systemExitDetail")),
      menuItem("Assess Data Quality",
               menuSubItem("Check PDDEs",
                           tabName = "tabPDDE"),
               menuSubItem("System-level",
                           tabName = "tabDQSystem"),
               menuSubItem("Organization-level",
                           tabName = "tabDQOrg")),
      menuItem("View Changelog",
               tabName = "tabChangelog")
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
              not required by HUD.</p>
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
            "),
            actionButton("Go_to_upload", "Click here to get started")
          ),
          box(
            id = "home_live_instructions",
            title = "Instructions",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            HTML(
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
              ready to engage with Eva. Navigate to the \'Upload HMIS CSV Export\' tab
              and follow the instructions there.</p>

              <p>Want to explore Eva without uploading? Use Eva's Demo Mode by clicking the 
              toggle at the top.</p>
              ")
          ),
          box(
            id = 'home_demo_instructions',
            title = "Demo Instructions",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            HTML(
              "<div class = 'in_demo_mode' style='display:none'>
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
              your own HMIS data again.</p></div>
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
                 Square Peg Data</a>, the CoCs who provided us with
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
          )),
          fluidRow(box(
            HTML(paste0("<div class='in_demo_mode' style='display:none'>
                 <p>You’re currently in Demo Mode and viewing sample HMIS data
                 from a curated HMIS CSV Export file. View the File Structure
                 Analysis below to see examples of the File Structure Errors you
                 could get in your own uploads. For a full list of possible
                 errors, see ",
                 a('Eva Checks', href=here("https://github.com/abtassociates/eva/blob/main/public-resources/EvaChecks.csv")),
                 ".</p>
                 <p>To explore your own File Structure Errors, turn off Demo
                 Mode and upload your own hashed HMIS CSV Export file.</p></div>
            ")),
            fileInput("imported",
                      label = NULL,
                      multiple = FALSE,
                      accept = ".zip"),
            uiOutput("fileInfo") %>% withSpinner(),
            width = 12
          )),
          fluidRow(box(
            title = "HMIS CSV Export File Structure Analysis",
            width = 12,
            DTOutput("fileStructureAnalysis"),
            p(),
            HTML("<p>Users should contact their vendor to resolve high priority 
            errors identified in the HMIS CSV Export File Structure Analysis, as
            well as any other structural issues which you feel need to be corrected.
            </p>"),
            p(),
            uiOutput('downloadFileStructureAnalysisBtn'),
            p(),
            uiOutput('downloadImpermissibleCharacterDetailBtn')
          ))
          # fluidRow(box(
          #   title = "System Data Quality Overview",
          #   width = 12,
          #   column(12, plotOutput("validate_plot")),
          #   column(6, plotOutput("dq_overview_plot")),
          #   column(6, plotOutput("dq_orgs_overview_plot"))
          # )
        # )
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
              way that is meaningful to your CoC. To edit these, click to expand the 
              relevant box below. If you do not edit them, the reporting will 
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
              ),
              numericInput(
                inputId = "CEOutstandingReferrals",
                label = "Max Days:",
                value = 14,
                min = 0,
                max = 365,
                step = 7,
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
                  numericInput(
                    inputId = "CELongStayers",
                    label = "Coordinated Entry:",
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
               react and filter in that way.
               
               <h4>Download System-wide Client Counts</h4>
               To download client count data for all projects in your HMIS CSV Export, 
               click the System-wide download button. The download contains a Current 
               tab limited to just the current date, a Date Range tab limited to the 
               Date Range set, and a Detail tab with clients' PersonalIDs, Entry Date, 
               Move-In Date (if applicable), Exit Date (if applicable), and
               project status.")
        )), 
        fluidRow(box(
          dateRangeInput(
            "dateRangeCount",
            "Date Range",
            format = "mm/dd/yyyy",
            start = if_else(isTRUE(getOption("shiny.testmode")), ymd("20231005"), ymd(today())),
            end = if_else(isTRUE(getOption("shiny.testmode")), ymd("20231005"), ymd(today())),
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
          DTOutput("clientCountSummary"),
          width = 12
        )),
        fluidRow(box(
          title = "Client Counts Detail",
          status = "info",
          solidHeader = TRUE,
          DTOutput("clientCountData"),
          width = 12
        ))
      ),
      tabItem(
        tabName = "systemOverview",
        fluidRow(box(htmlOutput("headerSystemOverview"), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("<h4>Placeholder</h4>")
        )),
        fluidRow(
          box(
            title = "Universe Selectors",
            width = 6,
            id = "universe_selectors",
            column(
              4,
              pickerInput(
                label = "Household Type",
                inputId = "syso_hh_type",
                choices = syso_hh_types,
                selected = syso_hh_types[1]
              )
            ),
            column(
              4,
              pickerInput(
                label = "Level of Detail",
                inputId = "syso_level_of_detail",
                choices = syso_level_of_detail,
                selected = syso_level_of_detail[1]
              )
            ),
            column(
              4,
              pickerInput(
                label = "Project Type",
                inputId = "syso_project_type",
                choices = syso_project_types,
                selected = syso_project_types[1]
              )
            )
          ),
          box(
            title = "Methodology Type",
            width = 6,
            radioButtons(
              "methodology_type",
              label = NULL,
              choices = syso_methodology_types,
              width = "100%"
            ),
            h4("Download Tabular View of System Overview Charts"),
            uiOutput("downloadSysOverviewTabBtn")
          )
        ), 
        # fluidRow(column(4, 
        #       br(),
        #       pickerInput(
        #         label = "Level of Detail",
        #         inputId = "syso_level_of_detail",
        #         choices = syso_level_of_detail,
        #         selected = syso_level_of_detail[1],
        #         width = "90%"
        #       )
        #     )),
        # fluidRow(
        #       br(),
        #       column(4,
        #       # a(href="www.google.com", "Click for Project Type Information"),
        #       pickerInput(
        #         label = "Project Type",
        #         inputId = "syso_project_type",
        #         choices = syso_project_types,
        #         selected = syso_project_types[1],
        #         width = "90%"
        #       )
        #     )),
          # box(
            # div(class="box-header", h3("Advanced Settings", class="box-title")),
            # div(class="box-body",
            #   radioButtons("methodology_type",
            #     HTML("Methdology Type <br/> <a href='www.google.com'>Click for Methodology Type Information</a>"),
            #     choices = syso_methodology_type,
            #     width = "100%"
            #   )),
            # hr(),
            # div(class="box-header", h3("Download Tabular View of System Overview Charts", class="box-title")),
            # width = 6
        #     box(
        #       title = "Advanced Settings",
        #       radioButtons("methodology_type",
        #         HTML("Methdology Type <br/> <a href='www.google.com'>Click for Methodology Type Information</a>"),
        #         choices = syso_methodology_types,
        #         width = "100%"
        #       ),
        #       width = 12
        #     ),
        #     box(
        #       title = "Download Tabular View of System Overview Charts",
        #       uiOutput("downloadSysOverviewTabBtn"),
        #       width = 12
        # ),
        fluidRow(
          box(
            id = "syso_header",
            "System Inflow and Outflow",
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Age and Special Population Filters",
            width = 6,
            column(
              6,
              pickerInput(
                inputId = "syso_age",
                label = "Age",
                selected = syso_age_cats,
                choices = syso_age_cats,
                multiple = TRUE,
                width = "100%",
                options = pickerOptions(
                  actionsBox = TRUE,
                  selectedTextFormat = paste("count >", length(syso_age_cats)-1),
                  countSelectedText = "All ages",
                  noneSelectedText = "All ages" 
                )
              )
            ),
            column(
              6,
              pickerInput(
                label = "Special Populations",
                inputId = "syso_spec_pops",
                choices = syso_spec_pops_people,
                width = "100%",
                selected = syso_spec_pops_people[1]
              )
            )
          ),
          box(
            title = "Gender and Race/Ethnicity Filters",
            width = 6,
            column(
              6,
              pickerInput(
                label = "Gender",
                inputId = "syso_gender",
                choices = syso_gender_excl,
                width = "100%",
                selected = syso_gender_excl,
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  selectedTextFormat = paste("count >", length(syso_gender_excl)-1),
                  countSelectedText = "All",
                  noneSelectedText = "All"
                )
              )
            ),
            column(
              6,
              pickerInput(
                label = "Race/Ethnicity",
                inputId = "syso_race_ethnicity",
                choices = syso_race_ethnicity_excl,
                width = "100%",
                selected = syso_race_ethnicity_excl
              )
            )
          )
        ),         fluidRow(
          tabBox(
            side = "right",
            selected = "Summary",
            title = "System Activity",
            tabPanel("Instructions", 
              uiOutput("system_activity_instructions_ui")
            ),
            tabPanel("Detail", 
              uiOutput("sys_act_detail_filter_selections"),
              uiOutput("sys_act_detail_chart_subheader"),
              plotOutput("sys_act_detail_ui_chart")
            ),
            tabPanel("Summary", 
              uiOutput("sys_act_summary_filter_selections"),
              uiOutput("sys_act_summary_chart_subheader"),
              plotOutput("sys_act_summary_ui_chart")
            ),
            width = 12
          )
        ),
        # fluidRow(
        #   box(
        #     id = "syso_header",
        #     "System Composition",
        #     width = 12
        #   )
        # ),
        fluidRow(
          box(
            checkboxGroupInput(
              "system_composition_filter",
              label = paste0(
                "Gender, Race/Ethnicity, and Special Populations",
                "(select up to 2)"
              ),
              choices = sys_comp_filter_choices1,
              inline = TRUE
            ),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Summary",
            title = "Composition of All Served in Period",
            tabPanel("Instructions", 
                     p("Some instructions")
            ),
            tabPanel("Summary", 
                     uiOutput("sys_comp_summary_filter_selections"),
                     plotOutput("sys_comp_summary_ui_chart")
            ),
            width = 12
          )
        ),
      ),
      tabItem(
        tabName = "systemExitDetail",
        fluidRow(box(htmlOutput("headerSystemExit"), width = 12)),
        fluidRow(
          box(
            width = 12,
            HTML("<h2>Placeholder</h2>")
          )
        )
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
            DTOutput("pdde_summary_table"),
            width = 12,
            br(),
            uiOutput("downloadPDDEReportButton") %>% withSpinner()
          ),
          box(id = "PDDEGuidance",
              DTOutput("pdde_guidance_summary"),
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
            tabPanel("Top 10 Projects",
                     uiOutput("orgDQHighPriorityErrorsByProject_ui")  %>% withSpinner()),
            tabPanel("Top 10 Issues",
                     uiOutput("orgDQHighPriorityErrorByIssue_ui") %>% withSpinner()),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "General Errors",
            tabPanel("Top 10 Projects", uiOutput("orgDQErrorsByProject_ui") %>% withSpinner()),
            tabPanel("Top 10 Issues", uiOutput("orgDQErrorByIssue_ui") %>% withSpinner()),
            width =12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "Warnings",
            tabPanel("Top 10 Projects", uiOutput("orgDQWarningsByProject_ui") %>% withSpinner()),
            tabPanel("Top 10 Issues", uiOutput("orgDQWarningsByIssue_ui") %>% withSpinner()),
            width = 12
          )
        ),
       
        fluidRow(
          box(
            id = "DQSummaryOrganization",
            title = paste("Data Quality Summary"),
            status = "info",
            solidHeader = TRUE,
            DTOutput("dq_organization_summary_table"),
            width = 12
          )
        ),
        
        fluidRow(
          box(
            id = "DQSummaryProvider",
            DTOutput("dq_org_guidance_summary"),
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
              <p>This tab will list the most recent technical updates and
              changes to Eva. For more in-depth information on current and past
              issues, please go to <a
              href='https://github.com/abtassociates/eva/issues' target= '_blank'
              rel='noopener noreferrer'>GitHub</a>.</p>
            ")
          ),
          box(
            # collapsible = TRUE,
            # collapsed = TRUE,
            width = 12,
            htmlTableWidgetOutput("changelog")
          )
        )
      ),
      tabItem(
        tabName = "tabDQSystem",
        fluidRow(box(
          htmlOutput("headerSystemDQ"), width = 12, 
          uiOutput("downloadSystemDQReportButton"))),
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
            tabPanel("Top 10 Organizations",
                     uiOutput("systemDQHighPriorityErrorsByOrg_ui")),
            tabPanel("Top 10 Issues",
                     uiOutput("systemDQHighPriorityErrorsByIssue_ui")),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "General Errors",
            tabPanel("Top 10 Organizations", uiOutput("systemDQErrorsByOrg_ui")),
            tabPanel("Top 10 Issues", uiOutput("systemDQErrorsByIssue_ui")),
            width =12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "Warnings",
            tabPanel("Top 10 Organizations", uiOutput("systemDQWarningsByOrg_ui")),
            tabPanel("Top 10 Issues", uiOutput("systemDQWarningsByIssue_ui")),
            width = 12
          )
        )
      )
    )
  )
)

