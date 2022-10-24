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
  dashboardHeader(title = "Stella HMIS"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Home",
               tabName = "homeTab"),
      menuItem("Upload Hashed CSV",
               tabName = "uploadCSV"),
      menuItem("Check PDDEs",
               tabName = "tabPDDE"),
      menuItem("Client Counts",
                  tabName = "currentProviderLevel"),
      menuItem("Data Quality",
        menuSubItem("System-level",
                    tabName = "dqSystem"),
               menuSubItem("Organization-level", 
                    tabName = "dqTab")
        # menuSubItem("Organization-level", 
        #             tabName = "dqOrganization"),
        #menuSubItem("Data Entry Timeliness", 
        #            tabName = "deskTime")
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
      tabName = "homeTab",
      box(
        title = "Welcome to Stella HMIS!",
        width = 12,
        HTML(
          "<div>Stella HMIS (Stella H) is intended for local use by HMIS Administrators in Continuums of Care (CoCs) around the U.S. and its territories. 
          Stella H is designed to help you assess the accuracy and completeness of the data within your HMIS. 
          In future iterations it will also assist communities in analyzing your HMIS performance data, 
          including coordinated entry, if your community utilizes HMIS for this purpose. Use of this tool is not required by HUD.</div>
          <br/>
          <div>This app works by using an uploaded 
          <a href='https://www.hudhdx.info/VendorResources.aspx'>HMIS CSV</a> 
          file.
          </div>
          <br/>"
        ),
        htmlOutput("goToUpload_text"),
        uiOutput("goToUpload_btn")
      ),
      width = 12
    ),
    tabItem(
      tabName = "uploadCSV",
      box(
        title = "Edit CoC-specific Settings",
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(
          column(
            width = 12,
            HTML(
              "<h3>Long Stayers</h3>
          <p>The projects in your CoC have a baseline average length of stay that
          will vary for different project types. Below, you can set the number
          of days your CoC would consider an enrollment to be well beyond the
          expected number of days in the project. You can set these based on your
          current data or leave them at the defaults."
            ),
            numericInput(
              inputId = "ESLongStayers",
              label = "Emergency Shelters:",
              value = 120,
              min = 0,
              max = 3650,
              step = 5
            ),
            numericInput(
              inputId = "SHLongStayers",
              label = "Safe Havens:",
              value = 120,
              min = 0,
              max = 3650,
              step = 5
            ),
            numericInput(
              inputId = "THLongStayers",
              label = "Transitional Housing:",
              value = 120,
              min = 0,
              max = 3650,
              step = 5
            ),
            numericInput(
              inputId = "OutLongStayers",
              label = "Street Outreach:",
              value = 120,
              min = 1,
              max = 3652
            ),
            numericInput(
              inputId = "ServicesOnlyLongStayers",
              label = "Services Only:",
              value = 120,
              min = 0,
              max = 3650,
              step = 5
            ),
            numericInput(
              inputId = "RRHLongStayers",
              label = "Rapid Rehousing:",
              value = 120,
              min = 0,
              max = 3650,
              step = 5
            ),
            numericInput(
              inputId = "HPLongStayers",
              label = "Prevention:",
              value = 120,
              min = 0,
              max = 3650,
              step = 5
            )
          )
        ),
        HTML(
          "<h3>Referrals</h3>
             <p>Please enter the number of days your CoC would consider a Referral
          to be \"outstanding\"."
        ),
        numericInput(
          inputId = "OutstandingReferrals",
          label = "Outstanding Referral Days:",
          value = 7
        )
      ),
      box(
        title = "Upload Hashed CSV zip file",
        HTML('<i class="fa fa-info-circle" 
            title="Use the Browse function to direct the app to the file folder containing your zipped CSV.">
             </i>'),
        fileInput("imported",
                  "",
                  multiple = FALSE,
                  accept = ".zip"),
        width = 12
      ), 
      uiOutput("integrityCheckerPanel"),
      box(title = "Status",
          uiOutput("headerFileInfo"),
          uiOutput("headerNoFileYet"),
          width = 12)
    ), 
    tabItem(
      tabName = "currentProviderLevel",
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
      tabName = "dqTab",
      fluidRow(box(htmlOutput("headerDataQuality"), width = 12)),
      fluidRow(box(
        pickerInput(
          label = "Select Organization",
          inputId = "orgList",
          choices = NULL, #c(unique(Organization$OrganizationName)),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = 'contains'),
          width = "100%",
          selected = "none"
        ),
        dateInput(
          inputId = "dq_startdate",
          label = "Report Start Date",
          format = "mm/dd/yyyy",
          value = NULL, # ymd(meta_HUDCSV_Export_Start),
          min = NULL,
          width = "25%"
        ),
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
        #uiOutput("DQHHIssues"),
        #uiOutput("DQDuplicateEEs"),
        #uiOutput("DQMissingLocation"),
        # uiOutput("DQPATHMissingContact")
      ),
      #fluidRow(uiOutput("DQIneligible")),
      #fluidRow(uiOutput("DQOverlappingEEs")),
      fluidRow(box(DTOutput("DQErrors"),
                   title = "Data Quality Errors",
                   width = 12)),
      fluidRow(uiOutput("DQIneligible"),
               uiOutput("DQOverlappingEEs")), 
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
          id = "warnings",
          DT::dataTableOutput("DQWarnings"),
          title = "Data Quality Warnings",
          width = 12
        )
      ),
      fluidRow(
        box(
          id = "DQSummaryProvider",
          DT::dataTableOutput("dq_provider_summary_table"),
          title = "Data Quality Guidance",
          width = 12,
          status = "info",
          solidHeader = TRUE
        )
      )
    ),
    tabItem(
      tabName = "deskTime",
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
      tabName = "dqSystem",
      fluidRow(box(htmlOutput("headerSystemDQ"), width = 12)),
      fluidRow(
        box(
          plotOutput("systemDQErrorTypes"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Most Common High Priority Issues and Errors"
        ),
        box(
          plotOutput("systemDQErrors"),
          width = 6,
          solidHeader = TRUE,
          status = NULL,
          title = "Organizations with the Most High Priority Issues and Errors"
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
