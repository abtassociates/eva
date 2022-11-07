# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

dashboardPage(
  skin = "black",
  dashboardHeader(title = "StellaR"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Home",
               tabName = "tabHome"),
      menuItem("Upload Hashed CSV",
               tabName = "tabUploadCSV"),
      menuItem("Check PDDEs",
               tabName = "tabPDDE"),
      menuItem("Client Counts",
                  tabName = "tabClientCount"),
      menuItem("Data Quality",
               menuSubItem("System-level",
                           tabName = "tabDQSystem"),
               menuSubItem("Organization-level",
                           tabName = "tabDQOrg")
        # menuSubItem("Data Entry Timeliness", 
        #             tabName = "tabDeskTime")
      ),
      menuItem("System Analysis",
               menuSubItem("System Flow",
                           tabName = "tabSystemFlow"),
               menuSubItem("Outcomes",
                           tabName = "tabOutcomes"),
               menuSubItem("Referrals",
                           tabName = "tabReferrals"),
               menuSubItem("Assessments",
                           tabName = "tabAssessments"),
               menuSubItem("Diversion",
                           tabName = "tabDiversion"),
               menuSubItem("Prioritization",
                           tabName = "tabPrioritized"))
    )), 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
    tabItem(
      tabName = "tabHome",
      box(
        title = "Welcome to StellaR!",
        width = 12,
        HTML(
          "<div>StellaR is intended for local use by HMIS Administrators in Continuums of Care (CoCs) around the U.S. and its territories. 
          StellaR is designed to help you assess the accuracy and completeness of the data within your HMIS. 
          In future iterations it will also assist communities in analyzing your HMIS performance data, 
          including coordinated entry, if your community utilizes HMIS for this purpose. Use of this tool is not required by HUD.</div>
          <br/>
          <div>This app works by using an uploaded 
          <a href='https://www.hudhdx.info/VendorResources.aspx'>HMIS CSV</a> 
          file.
          </div>
          <br/>"
        ),
        htmlOutput("goToUpload_text"), # fixme- is this needed anymore?
        uiOutput("goToUpload_btn")
      ),
      width = 12
    ),
    tabItem(
      tabName = "tabUploadCSV",
      box(
        title = "Edit CoC-specific Settings",
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(box(
          HTML(
            "<h3>Long Stayers</h3>
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
          <li><b>Permanent Supportive Housing:</b> enrollment's Length of Stay
          in days is in the top 1% of all PSH project enrollments in your system
          <li><b>Safe Haven:</b> enrollment's Length
          of Stay in days is in the top 2% of all other SH project enrollments in
          your system
          <li><b>Homelessness Prevention:</b> open enrollments with a Length of
          Stay (in days) that are in the top 2% project enrollments in
          your system
          <li><b>Rapid Rehousing:</b> enrollment's Length
          of Stay in days is in the top 2% of all other RRH project enrollments in
          your system
          <li><b>Street Outreach:</b> open enrollments with a Length of Stay in
          days that are equal to or greater than the user-input available below
          <li><b>Services Only:</b> open enrollments with a Length of Stay in
          days that are equal to or greater than the user-input available below
          <li><b>Other:</b> open enrollments with a Length of Stay in days that
          are equal to or greater than the user-input available below
          <li><b>Other Permanent Housing:</b> ???
          <li><b>Day Shelter:</b> open enrollments with a Length of Stay in days
          that are equal to or greater than the user-input available below
          <li><b>Emergency Shelter, Night-by-Night:</b> open enrollments with a
          Length of Stay in days that are equal to or greater than the user-input
          available below
          <li><b>Coordinated Entry:</b> open enrollments with a
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
        ),
      HTML(
        "<h3>Referrals</h3>
        <p>Please enter the number of days your CoC would consider a Referral
          to be \"outstanding\"."
      ),
      numericInput(inputId = "OutstandingReferrals",
                   label = "Outstanding Referral Days:",
                   value = 7)
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
        width = 12
      ), 
      box(
        title = "HUD CSV Export Integrity Checker",
        width = 12,
        DT::dataTableOutput("integrityChecker"),
        p(),
        downloadButton(outputId = "downloadIntegrityCheck",
                       label = "Download Integrity Check Detail")
      ), 
      box(title = "Status",
          uiOutput("headerFileInfo"),
          uiOutput("headerNoFileYet"),
          width = 12)
    ), 
    tabItem(
      tabName = "tabPDDE",
      fluidRow(box(htmlOutput(
        "headerPDDE"
      ), width = 12)),
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
        )
      )
    ),
    tabItem(
      tabName = "tabClientCount",
      fluidRow(box(htmlOutput("headerCurrent"), width = 12)),
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
          min = NULL, 
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
      tabName = "utilizationTab",
      fluidRow(box(htmlOutput(
        "headerUtilization"
      ), width = 12)),
      fluidRow(box(
        pickerInput(
          label = "Select Project",
          inputId = "providerListUtilization",
          choices = NULL, 
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = 'contains'),
          width = "100%"
        ),
        airDatepickerInput(
          inputId = "utilizationDate",
          label = "Report End Month for Annual Plot",
          max = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
          min = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date - days(335), unit = "month")),
          dateFormat = "MM yyyy",
          view = "month",
          value = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
          minView = "months",
          addon = "none",
          autoClose = TRUE,
          width = '25%'
        ),
        width = 12
      )),
      plotlyOutput("bedPlot"),
      br(),
      fluidRow(
        box(
          uiOutput("bedNote"),
          title = "What is Bed Utilization?",
          collapsible = TRUE,
          collapsed = TRUE
        ),
        box(
          uiOutput("unitNote"),
          title = "What is Unit Utilization?",
          collapsible = TRUE,
          collapsed = TRUE
        ),
        box(
          uiOutput("utilizationNote"),
          title = "Methodology",
          collapsible = TRUE,
          collapsed = TRUE
        )
      ),
      fluidRow(box(
        airDatepickerInput(
          inputId = "utilizationDetailDate",
          label = "Choose Month for Detail Data",
          max = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
          min = NULL, # ymd(floor_date(ymd(meta_HUDCSV_Export_End), "month") - years(2) + days(1)),
          dateFormat = "MM yyyy",
          view = "month",
          value = NULL, # ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
          minView = "months",
          addon = "none",
          autoClose = TRUE,
          width = '50%'
        ),
        width = 12
      )),
      fluidRow(box(
        infoBoxOutput("utilizationSummary0", width = '100%'),
        infoBoxOutput("utilizationSummary1", width = '100%'),
        infoBoxOutput("utilizationSummary2", width = '100%'),
        width = 12
      )),
      fluidRow(box(
        DT::dataTableOutput("utilizationDetail"), width = 12
      ))
    ),
    tabItem(
      tabName = "tabDQOrg",
      fluidRow(box(htmlOutput("headerDataQuality"), width = 12)),
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
        # dateInput(
        #   inputId = "dq_startdate",
        #   label = "Report Start Date",
        #   format = "mm/dd/yyyy",
        #   value = NULL, # ymd(meta_HUDCSV_Export_Start),
        #   min = NULL,
        #   width = "25%"
        # ),
        uiOutput("downloadOrgDQReportButton"),
        width = 12
      )), 
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
          plotOutput("orgDQHighPriorityErrorTypes"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Most Common High Priority Errors"
        ),
        box(
          plotOutput("orgDQHighPriorityErrors"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Projects with the Most High Priority Errors"
        )
      ),
      fluidRow(
        box(
          plotOutput("orgDQErrorTypes"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Most Common General Errors"
        ),
        box(
          plotOutput("orgDQErrors"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Projects with the Most General Errors"
        )
      ),
      fluidRow(
        box(
          plotOutput("orgDQWarningTypes"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Most Common Warnings Errors"
        ),
        box(
          plotOutput("orgDQWarnings"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Projects with the Most Warnings Errors"
        )
      ),
      fluidRow(box(DTOutput("DQHighPriority"),
                   title = "High Priority Issues",
                   width = 12)),
      fluidRow(box(DTOutput("DQErrors"),
                   title = "Data Quality Errors",
                   width = 12)),
      fluidRow(uiOutput("DQOverlappingEEs")), 
      fluidRow(box(
        id = "warnings",
        DT::dataTableOutput("DQWarnings"),
        title = "Data Quality Warnings",
        width = 12
      )),
      #fluidRow(
        #uiOutput("DQIneligible"),
        #uiOutput("DQOverlappingEEs")),
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
      fluidRow(box(htmlOutput("headerSystemDQ"), width = 12)),
      fluidRow(
        box(
          plotOutput("systemDQHighPriorityErrorTypes"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Most Common High Priority Errors"
        ),
        box(
          plotOutput("systemDQHighPriorityErrors"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Organizations with the Most High Priority Errors"
        )
      ),
      fluidRow(
        box(
          plotOutput("systemDQErrorTypes"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Most Common General Errors"
        ),
        box(
          plotOutput("systemDQErrors"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Organizations with the Most General Errors"
        )
      ),
        # box(plotOutput("systemHHErrors"), width = 12,
        #     solidHeader = TRUE,
        #     status = "danger",
        #     title = "Projects with the Most Household Errors")),
      fluidRow(
        box(
          plotOutput("systemDQWarningTypes"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Most Common Warnings"
        ),
        box(
          plotOutput("systemDQWarnings"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Organizations with the Most Warnings"
        )
      )
      )
    )
  )
)
