
dashboardPage(
  skin = "black",
  dashboardHeader(title = "Eva"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Home",
               tabName = "tabHome"),
      menuItem("View Client Counts",
                  tabName = "tabClientCount"),
      menuItem("Assess Data Quality",
               menuSubItem("Check PDDEs",
                           tabName = "tabPDDE"),
               menuSubItem("System-level",
                           tabName = "tabDQSystem"),
               menuSubItem("Organization-level",
                           tabName = "tabDQOrg"))
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
      tags$html(lang="en") #Added as WAVE fix but not considered ideal
    ),
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "tabHome",
        fluidRow(
          box(
            title = "Welcome to Eva!",
            width = 12,
            HTML(
              "<div>Eva is intended for local use by HMIS Administrators in Continuums of Care (CoCs) around the U.S. and its territories. 
              Eva is designed to help you assess the accuracy and completeness of the data within your HMIS. 
              In future iterations it will also assist communities in analyzing your HMIS performance data, 
              including coordinated entry, if your community utilizes HMIS for this purpose. Use of this tool is not required by HUD.</div>
              <br/>
              <div>This app works by using an uploaded 
              <a href='https://www.hudhdx.info/VendorResources.aspx' target= '_blank' rel='noopener noreferrer'>HMIS CSV</a> 
              file.
              </div>
              <br/>"
            )
          ),
          box(
            title = "Instructions",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            HTML("
                 <h4>Upload Hashed CSV zip file</h4>
                 <p>To upload your hashed HUD CSV Export zip file, click the \'Browse\'
                 button in the \'Upload Hashed CSV zip file\' panel. Once you find
                 the zip file on your computer, select it and click \'Open\'. Your
                 file will begin uploading. It will then check only the files
                 needed to determine if it is hashed. If it is not, the app will
                 reject your file with an error message, not process any further
                 data, and clear the app's memory until you upload another (hashed)
                 file. If it is hashed, it will then check for structural issues
                 in your HUD CSV Export. Some structural issues will affect the
                 functioning of this app and some will not. If your file has any
                 structural issues that would prevent this app from functioning,
                 your file will be rejected with an error message, it will stop
                 processing your data further, and it will clear the app's memory
                 until you upload another (structurally sound) file. Once you have 
                 uploaded a hashed and structurally sound
                 zip file, you will see a confirmation that your upload was 
                 successful, the date range of the file you uploaded, plus the
                 date your file was downloaded from your HMIS.</p>
                 
                 <h4>Edit CoC-specific Settings</h4>
                 <p>To make Eva reporting more useful at the local level,
                 you will find the CoC-specific settings that HMIS Leads can edit
                 to better analyse their data in a way that is meaningful to the
                 CoC. To edit these, click on the \'+\'. If you do not edit them,
                 the reporting will use the defaults listed. These defaults do
                 not imply any HUD recommendations. Please read the description
                 in the settings panel for more information.</p>
                 
                 <h4>File Structure Analysis</h4>
                 <p>Once the app verifies that your file is hashed, it will then
                 check that your upload has all the right tables, columns, data
                 types, and allowable values. If there are any issues
                 that will prevent the app from functioning, the app will reject
                 your file and not process any further. All issues will display
                 in the panel and you can download the details, even if the file
                 was rejected. Please ontact your vendor if there's a High
                 Priority issue found or if your file shows an Error or Warning
                 that you feel needs to be corrected. Not all issues found in
                 this analysis will need immediate attention.</p>
                 
                 <h4>Citations</h4>
                 <p>This panel will credit the people who wrote the open-source code
                 used to build this app, plus the community on whose code this app
                 was built.</p>")
          ),  
          box(
            title = "Upload Hashed CSV zip file",
            HTML('<i class="fa fa-info-circle" 
                title = "Use the Browse function to direct the app to the file folder containing your zipped CSV.">
                 </i>'),
            fileInput("imported",
                      label = NULL,
                      multiple = FALSE,
                      accept = ".zip"),
            uiOutput("headerFileInfo"),
            width = 12
          ), 
          box(
            title = "Edit CoC-specific Settings",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            fluidRow(
              box(
                HTML(
                  "<h4>Long Stayers</h4>
                <p>This check aims to help communities find enrollments that may be
                missing an Exit Date. It does this by looking at the number of days an
                enrollment has been open (or, the number of days between the Entry Date
                and the date your upload was exported from your HMIS.) The projects in
                your CoC have lengths of stay that will vary for different project
                types. Any data quality flags about Long Stayers should be considered a
                Warning, or, something to be checked for accuracy, and does not imply
                that any data should be changed.
      
                <p><b>Methodology by Project Type:</b>
                <ul>
                <li><b>Emergency Shelter, Entry Exit Method:</b> enrollment's Length
                of Stay in days is in the top 2% of all other ES project enrollments in
                your system
                <li><b>Transitional Housing:</b> enrollment's Length of Stay in days
                is in the top 2% of all other TH project enrollments in your system
                <li><b>Permanent Supportive Housing (Project Types 3, 9, and 10):</b> 
                enrollment's Length of Stay in days is in the top 1% of all PSH project
                enrollments in your system
                <li><b>Coordinated Entry:</b> open enrollments with a Length of
                Stay (in days) that are in the top 2% project enrollments in
                your system
                <li><b>Safe Haven:</b> enrollment's Length of Stay in days is in the 
                top 2% of all other SH project enrollments in your system
                <li><b>Homelessness Prevention:</b> open enrollments with a Length of
                Stay (in days) that are in the top 2% project enrollments in
                your system
                <li><b>Rapid Rehousing:</b> enrollment's Length
                of Stay in days is in the top 2% of all other RRH project enrollments in
                your system
                <br>
                <li><b>Street Outreach:</b> open enrollments with a Length of Stay in
                days that are equal to or greater than the user-input available below
                <li><b>Services Only:</b> open enrollments with a Length of Stay in
                days that are equal to or greater than the user-input available below
                <li><b>Other:</b> open enrollments with a Length of Stay in days that
                are equal to or greater than the user-input available below
                <li><b>Day Shelter:</b> open enrollments with a Length of Stay in days
                that are equal to or greater than the user-input available below
                <li><b>Emergency Shelter, Night-by-Night:</b> open enrollments with a
                Length of Stay in days that are equal to or greater than the user-input
                available below
                </ul>
      
                <p>Below, you can set the number of days your CoC would consider an
                enrollment to be well beyond the expected number of days in the Project
                Type. You can set these based on your current data or leave them at the
                defaults."
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
            title = "HUD CSV Export File Structure Analysis",
            width = 12,
            DT::dataTableOutput("integrityChecker"),
            p(),
            HTML("<p>Please contact your vendor if there's a High Priority issue
                 found or if your file shows an Error or Warning that you feel
                 needs to be corrected. Not all issues found in this analysis
                 will need immediate attention.</p>"),
            p(),
            uiOutput('downloadIntegrityBtn')
          ),
      box(
        title = "Citations and Special Thanks",
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        HTML("
             <p> This project would not exist were it not for the existence of other quality, 
             free and open source products. The following are citations for the products this
             app relies on.
             
             <p> R Core Team (2019). R: A language and environment for statistical computing.
             R Foundation for Statistical Computing. Vienna, Austria. 
             <a href = 'https://www.r-project.org' target= '_blank' rel='noopener noreferrer'>R programming language</a>.
             
             <p> Hadley Wickham (2017). tidyverse: Easily Install and Load the 'Tidyverse.'
             R package version 1.2.1. 
             <a href = 'https://cran.r-project.org/web/packages/tidyverse/index.html' target= '_blank' rel='noopener noreferrer'>Tidyverse package</a>.
             
             <p>Winston Chang, Joe Cheng, JJ Allaire, Yihue Xie and Jonathan McPherson (2019).
             shiny: Web Application Framework for R. R package version 1.3.2. 
             <a href = 'https://cran.r-project.org/web/packages/shiny/index.html' target= '_blank' rel='noopener noreferrer'>R Shiny package</a>
             and shinydashboard; Create Dashboards with 'Shiny.' R package version 0.7.1.
             <a href = 'https://cran.r-project.org/web/packages/shinydashboard/index.html' target= '_blank' rel='noopener noreferrer'>shinydashboard package</a>.
             
             <p> The foundational code for the app was shared by 
             <a href = 'https://www.cohhio.org' target= '_blank' rel='noopener noreferrer'>COHHIO</a>, 
             Coalition on Homelessness and Housing in Ohio.
             
             <p> Special thanks to 
             <a href=\"http://www.squarepegdata.com/\" target= '_blank' rel='noopener noreferrer'>
             Square Peg Data</a>,
             San Diego City and County CoC (CA-601) and Minneapolis/Hennepin County CoC
             (MN-500) for providing sample datasets to support programming.")
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
               <p>Once you have successfully uploaded a zip file, you will find
               a summary of each issue that was flagged in your data regarding
               your PDDEs. Please download the details by clicking the \'Download\'
               button.</p>
               
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
          box(title = "Guidance",
              width = 12,
              HTML("coming soon"),
              status = "info",
              solidHeader = TRUE)
        )
      ),
      tabItem(
        tabName = "tabClientCount",
        fluidRow(box(htmlOutput("headerCurrent"), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("
               <h4>Client Counts Report</h4>
               <p>The Client Counts Report serves as a way to check that a
               project is up to date on their HMIS data entry. Projects can
               check that the number of households/clients who are currently in
               the project aligns with what they know about who they are serving.
               Permanent housing projects can check that the number of households/
               clients who have not yet moved into housing is correct. It can
               be run to show the current status of their data entry or the
               HMIS Lead may wish to edit the Date Range to cover a larger
               time frame.</p>
               
               <h4>Inputs</h4>
               <p>HMIS Leads may select a single project from the drop list. The
               Date Range defaults to the export's start and end date, but
               users are encouraged to edit the Date Range as desired to see
               metrics such as how many clients/households exited with and 
               without a Move-In Date, how many exited during a specific time period,
               or \'current\' enrollments only.</p>
               
               <h4>Summary</h4>
               <p>Check here for a count of households or clients who have statuses
               of the following:
               <ul>
               <li>Currently Awaiting Housing</li>
               <li>Currently Moved In</li>
               <li>Exited No Move-In</li>
               <li>Exited With Move-In</li>
               <li>Currently in Project</li>
               <li>Exited Project</li>
               </ul>
               
               <h4>Client Counts Detail</h4>
               <p>In this panel, HMIS Leads can search in any column. You will see
               the Personal ID, Relationship to HoH, Entry Date, Move-In Date, 
               Exit Date, and the Status for each client. The rows are ordered by
               Entry Date (oldest on top), Household ID (not visible), and
               Personal ID. This enables users to see the oldest enrollments first.
               To find all enrollments with a Status of \'Currently Awaiting
               Housing\' for example, you can type \'wait\' in the Status search bar
               and the data table will react and filter in that way.")
        )), 
        fluidRow(box(
          pickerInput(
            label = "Select Project",
            inputId = "currentProviderList",
            choices = NULL,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = 'contains')
          ),
          dateRangeInput(
            "dateRangeCount",
            "Date Range",
            format = "mm/dd/yyyy",
            width = 300
          ),
          width = 12
        )),
        fluidRow(box(
          DT::dataTableOutput("clientCountSummary"),
          width = 12
        )),
        fluidRow(box(
          DT::dataTableOutput("clientCountData"),
          width = 12
        ))
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
               <p>Below, select the Organization whose data quality you would
               like to check. The data shown will reflect the date range that
               you used to run your HUD CSV Export. It will show data quality
               metrics from all Projects that are associated with that
               Organization.</p>
               <p>HMIS Leads can click the Download button in order to get an
               Excel workbook with the selected Organization's data quality
               errors. You can send these to any users at the selected Organization
               so they can work on correcting their data. Feel free to modify, 
               add, or remove anything as you see fit. For example, you may want 
               your users to only address High Priority issues right now. You can
               easily remove any tabs that may distract your users from that goal.
               Please note that Overlaps will be shown in the 'Warnings' tab and
               again in the 'Overlap Detail' tab of the download. This is so
               that your users have enough detail to track down that issue.</p>
               
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

               <h4>Most Common High Priority Errors Plot</h4>
               <p>Which High Priority Errors are most common across all
               the projects under the selected Organization? This plot will only
               show the top 10 High Priority Errors. This can be useful in
               planning targeted HMIS training efforts.</p>
               
               <h4>Projects with the Most High Priority Errors</h4>
               <p>Within the selected Organization, which Projects are making the
               highest number of High Priority Errors? This plot will only show the
               top 10 projects' High Priority Error counts.</p>
               
               <h4>Most Common General Errors</h4>
               <p>Which General Errors are most common across all the projects
               under the selected Organization? This plot will only show the top
               10 General Errors. This can be useful in planning targeted HMIS
               training efforts.</p>
               
               <h4>Projects with the Most General Errors</h4>
               <p>Within the selected Organization, which Projects are making the
               highest number of General Errors? This plot will only show the
               top 10 projects' General Error counts.</p>
               
               <h4>Most Common Warnings</h4>
               <p>Which Warnings are most common across all the
               projects under the selected Organization? This plot will only show
               the top 10 Warnings. This can be useful in planning targeted HMIS
               training efforts.</p>
               
               <h4>Projects with the Most Warnings</h4>
               <p>Within the selected Organization, which Projects are flagged
               with the highest number of Warnings? This plot will only show the
               top 10 projects' Warning counts.</p>
               
               <h4>Data Quality Summary</h4>
               <p>In this panel, Leads can see which projects within the selected
               Organization have how many of what types of issues. Where the plots
               only list the top 10 for each error type, this data displays ALL
               Warnings, General Errors, and High Priority Errors for all Projects
               within the Organization. At the top of each column the Lead may
               type in any part of a word and the data will filter for you. Some
               examples of how this might be used:
               <ul><li> Filter all projects in the Organization that have household
               issues. [Type 'hou' in the Issue column's search box.]</li>
               <li> Find all Warnings for Project 'Homeward Bound - ES'. [Type
               'homew' in the Project Name column and 'war' in the Type column.]
               </li></ul>
               </p>
               
               <h4>Data Quality Guidance</h4>
               <p>This panel lists each High Priority Error, General Error, and
               Warning represented in the selected Organization's data along with
               descriptions for each issue that include why the data is being
               flagged. It is ordered by the Type of issue and the name of the
               Issue. Leads may also search this data by column.</p>")
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
            selected = "Most Common Errors",
            title = "High Priority Errors",
            tabPanel("Top Projects", uiOutput("orgDQHighPriorityErrors_ui")),
            tabPanel("Most Common Errors", uiOutput("orgDQHighPriorityErrorTypes_ui")),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Most Common Errors",
            title = "General Errors",
            tabPanel("Top Projects", uiOutput("orgDQErrors_ui")),
            tabPanel("Most Common Errors", uiOutput("orgDQErrorTypes_ui")),
            width =12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Most Common Warnings",
            title = "Warnings",
            tabPanel("Top Projects", uiOutput("orgDQWarnings_ui")),
            tabPanel("Most Common Warnings", uiOutput("orgDQWarningTypes_ui")),
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
        fluidRow(box(htmlOutput("headerSystemDQ"), width = 12, uiOutput("downloadFullDQReportButton"))),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("<h4>System-wide HMIS Data Quality Plots</h4>
               <p>The plots here will help HMIS admins to get a sense for which
               Organizations may be in need of extra assistance and where extra
               training may be needed. HMIS admins can also download this data to
               aid in reporting to interested entities about your overall HMIS
               system data quality. HMIS admins can check this report to help
               guide which Organizations may need a closer look in the next tab,
               called Data Quality > Organization-level. For each type of error
               (High Priority Errors, General Errors, and Warnings) you will
               find 2 plots.</p>
               
               <h5><b>Most Common of a Type of Error</b></h5>
               <p>Across all of the Organizations in your upload, this plot shows
               the top 10 Errors/Warnings of the type indicated. This can help
               to focus future end user trainings and bring to light any
               potential considerations in your federal or local reporting.</p>
               
               <h5><b>Organizations with the Most of a Type of Error</b></h5>
               <p>This plot shows which Organizations across your system have
               the highest number of issues of the type indicated. HMIS admins
               can use these plots to help determine which organizations may
               need extra assistance in getting their HMIS Errors/Warnings
               resolved.</p>
               
               <h4>Download System-wide HMIS Data Quality Data</h4>
               <p>To download all of the client and enrollment related issues
               found in your system, click the Download button. This will give
               HMIS admins a way of reporting to interested entities, such as
               your CoC leadership, a broader view of the state of your HMIS
               data quality.</p>
               
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
               edit data that accurately reflects reality</b>.")
        )), 

        fluidRow(
          tabBox(
            side = "right",
            selected = "Most Common Errors",
            title = "High Priority Errors",
            tabPanel("Top Organizations", uiOutput("systemDQHighPriorityErrors_ui")),
            tabPanel("Most Common Errors", uiOutput("systemDQHighPriorityErrorTypes_ui")),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Most Common Errors",
            title = "General Errors",
            tabPanel("Top Organizations", uiOutput("systemDQErrors_ui")),
            tabPanel("Most Common Errors", uiOutput("systemDQErrorTypes_ui")),
            width =12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Most Common Warnings",
            title = "Warnings",
            tabPanel("Top Organizations", uiOutput("systemDQWarnings_ui")),
            tabPanel("Most Common Warnings", uiOutput("systemDQWarningTypes_ui")),
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


