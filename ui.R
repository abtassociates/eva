dashboardPage(
  title = "Eva",
  skin = "black",
  dashboardHeader(
    title = span(img(src = "Eva_logo_horizontal_white.png",
                     alt = "Eva logo",
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
      menuItem("Assess Data Quality",
               menuSubItem("Check Project Data",
                           tabName = "tabPDDE"),
               menuSubItem("System-level",
                           tabName = "tabDQSystem"),
               menuSubItem("Organization-level",
                           tabName = "tabDQOrg")),
      menuItem("System Performance Overview",
               tabName = "tabSystemOverview"),
               #menuSubItem("System Exit Detail",
                #           tabName = "systemExitDetail")
      menuItem("Glossary",
               tabName = "tabGlossary"),
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
      tags$script(HTML(
        "function idleTimer() {
          var timeoutTime = 900000; //15 mins
          var t = setTimeout(showTimeoutDialog, timeoutTime);
          window.onmousemove = resetTimer;
          window.onmousedown = resetTimer;
          window.onclick = resetTimer;
          window.onscroll = resetTimer;
          window.onkeypress = resetTimer;
      
          function showTimeoutDialog() {
            document.querySelector('a[data-value=\"tabHome\"]').click();
            
            // If user clicks Cancel, disable all interactivity
            document.body.style.opacity = '0.5';
            document.body.style.pointerEvents = 'none';
            
            // Add a banner at the top of the page
            var banner = document.createElement('div');
            banner.style.position = 'fixed';
            banner.style.top = '40%';
            banner.style.left = '0';
            banner.style.width = '100%';
            banner.style.padding = '10px';
            banner.style.backgroundColor = 'lightgray';
            banner.style.color = 'black';
            banner.style.fontSize = '2em';
            banner.style.textAlign = 'center';
            banner.style.zIndex = '9999';
            banner.innerHTML = 'Session ended due to inactivity. Please refresh the page to continue.';
            document.body.appendChild(banner);
          }
      
          function resetTimer() {
              clearTimeout(t);
              t = setTimeout(showTimeoutDialog, timeoutTime);
          }
        }
        idleTimer();
                       
        var dimension = [0, 0];
        $(document).on('shiny:connected', function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange('dimension', dimension);
        });
        $(window).resize(function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange('dimension', dimension);
        });
        "
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
          )),
          fluidRow(box(
            HTML(paste0("<div class='in_demo_mode' style='display:none'>
                 <p>You’re currently in Demo Mode and viewing sample HMIS data
                 from a curated HMIS CSV Export file. View the File Structure
                 Analysis below to see examples of the File Structure Errors you
                 could get in your own uploads. For a full list of possible
                 errors, see <a href =
                 'https://github.com/abtassociates/eva/blob/main/public-resources/EvaChecks.csv' 
                 target= '_blank' rel='noopener noreferrer'>Eva Checks</a>.</p>
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
              <p>To make Eva data quality analysis more useful at the local level, 
              you can adjust the local settings to better analyze your data in a 
              way that is meaningful to your CoC. To edit these, click to expand 
              the relevant box below. If you do not edit them, the Assess Data Quality 
              and View Client Counts pages will use the defaults listed. Please 
              note, these local settings do not impact the System Performance Overview page.</p>
              
              <p>These defaults do not imply any HUD recommendations. Please read 
              the description in the Edit Local Settings tab for more information.</p>
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
            selected = "Issues",
            title = "High Priority Errors",
            tabPanel("Top 10 Projects",
                     uiOutput("orgDQHighPriorityErrorsByProject_ui")  %>% withSpinner()),
            tabPanel("Issues",
                     uiOutput("orgDQHighPriorityErrorByIssue_ui") %>% withSpinner()),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "General Errors",
            tabPanel("Top 10 Projects",
                     uiOutput("orgDQErrorsByProject_ui") %>% withSpinner()),
            tabPanel("Top 10 Issues",
                     uiOutput("orgDQErrorByIssue_ui") %>% withSpinner()),
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
        tabName = "tabSystemOverview",
        fluidRow(box(htmlOutput("headerSystemOverview"), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("<h4>System Performance Overview</h4>
                <p>The System Performance Overview page in Eva features three system 
                performance charts: the System Flow Chart, the Client System Status 
                Chart, and the System Demographics Chart. The charts display system 
                performance data, pulled from your uploaded HMIS CSV export, from 
                all HMIS Continuum projects, excluding homeless prevention projects. 
                Eva uses the last 12 full months of data in the upload, which constitutes 
                the report period. <b>Note that some charts on this page may not display 
                if the uploaded HMIS CSV export has less than 12 full months of data.</b></p>
                
                <p>The purpose of the system performance charts is to use your HMIS 
                data to (1) evaluate how effective your homeless system is in moving 
                clients through the system and helping them reach permanent housing, 
                and (2) help you understand the demographic composition of all clients 
                served in your homeless system. Explore each of the charts using 
                chart tabs beneath the Filters Menu. Under each chart tab is a Chart 
                subtab for viewing the chart itself and an Information subtab. The 
                Information subtab includes a \"Chart Overview” section that provides 
                guidance on how to read the chart, and some charts additionally have 
                an \"Interpretation Tips” section that can help you interpret their output.</p>
                
                <p>Use the Filters Menu to explore system performance trends of 
                clients in your homeless system with specific characteristics. 
                This has two components: </p>
                
                <ol>
                  <li><b>The universal filters</b>, the top row of the Filters Menu, 
                  impact the data shown on all three visualizations on this page. 
                  Universal filters include Household Type, Level of Detail, Project 
                  Type Group, and Race/Ethnicity Methodology Type.</li>
                  <li><b>The demographic filters</b>, the bottom row of the Filters Menu, 
                  only impact the data shown in the System Flow and Client System 
                  Status charts. Demographic filters include Age, Veteran Status, 
                  and Race/Ethnicity.</li>
                </ol>
                
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
                
                <h4>Downloads</h4>
                <p>To support further systems analysis, local reporting, and presentations, 
                Eva includes two download options. To generate an Excel workbook 
                with the data for a specific chart, click the \"Data Download\" button 
                while viewing the chart. To generate a PowerPoint slide deck with 
                the chart image, click the \"Image Download\" button while viewing the chart. </p>
                
                <h4>Data Suppression and Data Security</h4>
                <p>To ensure the privacy and protection of individuals and small 
                population groups, Eva uses varying levels of data suppression. 
                If the total number of clients within a chart is less than 11, the 
                chart will not display. When this happens, you may need to broaden 
                your filter selections or upload a larger dataset to ensure there 
                is enough data to view the chart. A chart that is not displayed 
                cannot be exported in Excel or PowerPoint.</p>
                
                <p>The data in the data download of a chart’s export <b>will not be 
                suppressed</b>. Be careful how you save and share the tabular export. 
                With smaller numbers, clients can become more identifiable in the 
                data. Before you share the Excel export, feel free to modify, add, 
                or remove anything as you see fit to preserve client anonymity. </p>
               ")
        )),
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            id = "syso_filters",
            fluidRow(
              id="universe_filters",
              column(
                2,
                pickerInput(
                  label = "Household Type",
                  inputId = "syso_hh_type",
                  choices = syso_hh_types,
                  selected = syso_hh_types[1]
                )
              ),
              column(
                2,
                pickerInput(
                  label = "Level of Detail",
                  inputId = "syso_level_of_detail",
                  choices = syso_level_of_detail,
                  selected = syso_level_of_detail[1]
                ),
              ),
              column(
                2,
                pickerInput(
                  label = "Project Type Group",
                  inputId = "syso_project_type",
                  choices = syso_project_types,
                  selected = syso_project_types[1]
                ),
              ),
              column(
                6,
                pickerInput(
                  label = "Race/Ethnicity Methodology Type",
                  inputId = "methodology_type",
                  multiple = FALSE,
                  selected = syso_methodology_types[1],
                  choices = syso_methodology_types,
                  width = "100%"
                ),
              ) %>% tagAppendAttributes(class="light-left-border"),
            ),
            fluidRow(
              id="syso_inflowoutflow_filters",
              column(
                3,
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
                    countSelectedText = "All Ages",
                    noneSelectedText = "All Ages" 
                  )
                )
              ),
              column(
                3,
                pickerInput(
                  label = "Veteran Status",
                  inputId = "syso_spec_pops",
                  choices = syso_spec_pops_people,
                  width = "100%",
                  selected = syso_spec_pops_people[1]
                )
              ),
              column(
                6,
                pickerInput(
                  label = "Race/Ethnicity",
                  inputId = "syso_race_ethnicity",
                  choices = syso_race_ethnicity_method1,
                  width = "100%",
                  selected = syso_race_ethnicity_method1,
                  options = list(
                    `dropdown-align-right` = TRUE, 
                    `dropup-auto` = FALSE)
                )
              ) %>% tagAppendAttributes(class="light-left-border")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            tabBox(
              width = 12,
              id = "syso_tabbox",
              type = "tabs",
              tabPanel(
                id = "syso_inflowoutflow", 
                title = "System Flow",
                tabBox(
                  width = 12,
                  id = "sys_inflow_outflow_subtabs",
                  selected = "Summary Chart",
                  tabPanel("Summary Chart", 
                           uiOutput("sys_act_summary_filter_selections") %>%
                             withSpinner(),
                           plotOutput("sys_act_summary_ui_chart",
                                      width = "70%",
                                      height = "500") %>%
                             withSpinner()
                  ),
                  tabPanel("Detail Chart", 
                           uiOutput("sys_act_detail_filter_selections") %>%
                             withSpinner(),
                           plotOutput("sys_act_detail_ui_chart",
                                      width = "100%",
                                      height = "500") %>%
                             withSpinner()
                  ),
                  tabPanel("Information", 
                           HTML("<h4>Chart Overview</h4>
                                <p>The System Flow chart shows your homeless system's 
                                inflow and outflow during the period, helping you 
                                assess the effectiveness of your homeless system. 
                                The client universe for this chart is the number 
                                of clients identified as active in your system at 
                                the start of the report period plus the number of 
                                clients that inflowed into your system during the 
                                period. There are two views for this chart: the 
                                Summary Chart view and the Detail Chart view. Both 
                                views show the total number of clients active in 
                                the system at the start and end of the period and 
                                whether they are homeless at that time or housed 
                                (and still receiving assistance).</p>
                                
                                <p>The Summary Chart shows the inflow and outflow 
                                of clients that occurred throughout the period. 
                                The Detail Chart breaks down inflow and outflow 
                                into several categories.</p>
                                
                                <ul>
                                  <li><b>Inflow</b> is categorized into three groups: 
                                  \"First-Time Homeless,\" \"Returned from Permanent,\" and 
                                  \"Re-engaged from Non-Permanent.\"</li>
                                  <li><b>Outflow</b> is divided into three categories: 
                                  \"Exited, Non-Permanent,\" \"Exited, Permanent,\" and \"Inactive.\"</li>
                                </ul>
                                
                                <p>The System Flow chart is read from 
                                left to right. The Total Change value represents 
                                the Outflow value(s) minus Inflow value(s). The 
                                Total Change value can be positive or negative. 
                                A negative change value means more clients left 
                                your system than flowed into your system. A positive 
                                change value means more clients flowed into your 
                                system than left your system.</p>
                                
                                <h4>Interpretation Tips</h4>
                                <p>This section provides general tips on how to 
                                interpret the chart. Depending on the data you uploaded, 
                                some of the items below may not apply.</p>
                                
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
                                    is larger than the bard for Inflow. The Total 
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
                  )
                ),
                downloadButton("sys_inflow_outflow_download_btn", "Data Download"),
                downloadButton("sys_inflow_outflow_download_btn_ppt", "Image Download")
              ),
              tabPanel(
                id = "syso_systemstatus",
                side = "left",
                selected = "Chart",
                title = "Client System Status",
                tabBox(
                  width = 12,
                  id = "sys_status_subtabs",
                  tabPanel("Chart", 
                           uiOutput("sankey_filter_selections") %>% withSpinner(),
                           plotOutput("sankey_ui_chart", width="70%") %>% withSpinner()
                  ),
                  tabPanel("Information", 
                           HTML("<h4>Chart Overview</h4>
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
                                
                                <p>In the area of the figure between the two bars, 
                                the Client System Status Chart depicts the change 
                                of these clients from their status at the start 
                                of the period to their status at the end of the 
                                period through visible linkages that connect the 
                                two bars. The width of each linkage represents the 
                                proportion of clients that make up that linkage. 
                                Meaning, the thicker the linkage, the larger proportion 
                                of clients it represents.</p>
                                
                                <h4>Interpretation Tips</h4>
                                <p>This section provides general tips on how to 
                                interpret the chart. Depending on the data you 
                                uploaded, some of the items below may not apply.</p>
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
                  )
                ),
                downloadButton("sys_status_download_btn", "Data Download"),
                downloadButton("sys_status_download_btn_ppt", "Image Download"),
                width = 12
              ),
              tabPanel(
                id = "syso_composition",
                side = "left",
                selected = "Chart",
                title = "System Demographics",
                tabBox(
                  width = 12,
                  id = "sys_comp_subtabs",
                  tabPanel("Chart", 
                           fluidRow(
                             box(
                               strong("Select Demographic Crosstab Categories (up to 2)"),
                               p(str_glue(
                                 "For a simple count of totals within a demographic 
                                 category, select only one category. To see the 
                                 intersection of two demographic categories, select 
                                 both categories to create a crosstab chart. To 
                                 change your crosstab selection, uncheck at least 
                                 one of your previous selections before selecting 
                                 new categories. Note that you can only select one Race/Ethnicity 
                                 category to display in the chart at a time."
                               )),
                               checkboxGroupInput(
                                 "system_composition_selections",
                                 label = "",
                                 choices = sys_comp_selection_choices,
                                 selected = c("All Races/Ethnicities", "Age"),
                                 inline = TRUE
                               ),
                               width = 12
                             )
                          ),
                          uiOutput("sys_comp_summary_selections"),
                          plotOutput("sys_comp_summary_ui_chart") %>% withSpinner()
                  ),
                  tabPanel("Information", 
                           HTML("<h4>Chart Overview</h4>
                                <p>The System Demographics Chart shows the demographic 
                                make-up of your homeless system and highlights 
                                important trends among various demographic groups. 
                                The client universe for this chart is the number 
                                of clients identified as active in your system at 
                                the start of the report period plus the number of 
                                clients that inflowed into your system.</p>
                                
                                <p>Under the chart tab are five demographic categories 
                                you can choose from: Age, 
                                All Races/Ethnicities, a second race/ethnicity 
                                option, and Veteran Status. Please note, the second 
                                race/ethnicity option differs for each
                                Race/Ethnicity Methodology Type selection you made 
                                earlier on the Filter Menu.</p>
                                
                                <p>For a simple count of totals within a demographic 
                                category, select only one category. To see the 
                                intersection of two demographic categories, select 
                                both categories to create a crosstab chart. To change 
                                your crosstab selection, uncheck at least one of 
                                your previous selections before selecting a new 
                                category. Please note that you can only select one 
                                race/ethnicity category to display in the chart 
                                at a time.</p>
                                
                                <p>Each cell in the chart is a unique combination 
                                of demographic characteristics. For example, if 
                                you selected Age and Race/Ethnicity, a unique demographic 
                                combination would be \"25 to 34” and \"Black alone.” Any cell with a count is shaded. 
                                The darker the color in a cell, the greater the 
                                value of that cell.</p>
                                
                                <p>Please note that household type and age group filters use different 
                                methods for calculating a client's age. Household type is based on all 
                                household members’ ages as of the entry date of their earliest enrollment 
                                included in the report period. Age group is determined based on the client’s 
                                age as of the entry date of their last enrollment included in the report period. 
                                Because of this reporting difference, it is possible for a client that ages from 
                                24 to 25 during the report period to be categorized in the Adult Only 18-24 
                                household type while also being categorized as in the 25-34 age group.</p>
                                
                                <h4>Data Suppression</h4>
                                <p>Additional levels of data suppression apply to 
                                the System Demographics Chart.</p>
                                
                                <ul>
                                  <li>Any value less than 11 is suppressed, including totals.</li>
                                  <li>If there is only one suppressed value within 
                                  a row or column, the next highest value is also suppressed.</li>
                                  <li>If all individual cells in the chart have 
                                  values of less than eleven (11), the chart will not display.</li>
                                </ul>
                                
                                <p>All suppressed values are represented by *** in the chart.</p>
                                
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
                  )
                ),  # end syso_comp_subtab tabbox
                downloadButton("sys_comp_download_btn", "Data Download"),
                downloadButton("sys_comp_download_btn_ppt", "Image Download"),
                width = 12
              ) #end syso_comp tabPanel
            ), # end syso_tabbox
            downloadButton("client_level_download_btn", "Client Level Download")
          ) # end box
        ) #end fluid row
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
        tabName = "tabGlossary",
        fluidRow(box(HTML("<h2>Glossary</h2>"), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("
               <p>This glossary provides definitions for the terms used throughout 
              Eva's System Performance Overview page. You can review definitions 
              of the terms by their focus, including:</p>
              
              <ul>
                <li>System Performance Filters</li>
                <li>System Flow Chart</li>
                <li>Client System Status Chart</li>
              </ul>
              
              <p>You can also search for a specific term using the search bar.</p>")
        )),
        fluidRow(
          box(
            # collapsible = TRUE,
            # collapsed = TRUE,
            width = 12,
            DTOutput("glossary")
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
            dataTableOutput("changelog")
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
            selected = "Issues",
            title = "High Priority Errors",
            tabPanel("Top 10 Organizations",
                     uiOutput("systemDQHighPriorityErrorsByOrg_ui")),
            tabPanel("Issues",
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

