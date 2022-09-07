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
  dashboardHeader(title = "Needs New Name"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Home",
               tabName = "homeTab"),
      menuItem("Upload Hashed CSV",
               tabName = "uploadCSV"),
      menuItem("PDDE Checker",
               tabName = "tabPDDE"),
      menuItem("Client Counts",
                  tabName = "currentProviderLevel"),
      menuItem("Data Quality",
        menuSubItem("Project-level", 
                    tabName = "dqTab"),
        menuSubItem("Organization-level", 
                    tabName = "dqOrganization"),
        menuSubItem("System-level",
                    tabName = "dqSystem"),
        menuSubItem("Data Entry Timeliness", 
                    tabName = "deskTime")
      ),
      menuItem("Coordinated Entry Analyses",
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
               menuSubItem("Prioritized",
                           tabName = "tabPrioritized"))
    )), 
  dashboardBody(tabItems(
    tabItem(
      tabName = "homeTab",
      htmlOutput("headerHome"),
      width = 12
    ),
    tabItem(
      tabName = "uploadCSV",
      box(title = "Status",
          uiOutput("headerFileInfo"),
          width = 12),
      box(
        title = "Upload Hashed CSV zip file",
        fileInput("imported",
                  "",
                  multiple = FALSE,
                  accept = ".zip"),
        tableOutput("files"),
        tableOutput("test"),
        width = 12
      ), 
      box(
        title = "HUD CSV Export Integrity Checker",
        width = 12,
        downloadButton(outputId = "downloadIntegrityCheck",
                       label = "Download Integrity Checker")
      )
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
          min = 0,
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
          choices = NULL, #c(sort(utilization_bed$ProjectName)),
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
      fluidRow(box(htmlOutput(
        "headerDataQuality"
      ), width = 12)),
      fluidRow(box(
        pickerInput(
          label = "Select Project",
          inputId = "providerListDQ",
          choices = NULL, # dq_providers,
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = 'contains',
            actionsBox = TRUE
          ),
          multiple = TRUE,
          width = "100%",
          selected = "none"
        ),
        dateInput(
          inputId = "dq_startdate",
          label = "Report Start Date",
          format = "mm/dd/yyyy",
          value = NULL, # ymd(hc_check_dq_back_to),
          min = 0,
          width = "25%"
        ),
        width = 12
      )),
      fluidRow(
        uiOutput("DQHHIssues"),
        uiOutput("DQDuplicateEEs"),
        uiOutput("DQMissingLocation")#,
        # uiOutput("DQPATHMissingContact")
      ),
      fluidRow(uiOutput("DQIneligible")),
      fluidRow(uiOutput("DQOverlappingEEs")),
      fluidRow(box(
        DTOutput("DQErrors"),
        title = "Data Quality Errors",
        width = 12
      )),
      fluidRow(
        box(
          id = "warnings",
          DT::dataTableOutput("DQWarnings"),
          title = "Data Quality Warnings",
          width = 12
        )
      )
      ,
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
      fluidRow(box(
        plotOutput("DeskTimePlotDetail"), width = 12
      )),
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
    tabItem(tabName = "dqOrganization",
      fluidRow(box(
        htmlOutput("headerOrganizationDQ"), width = 12
      )),
      fluidRow(box(
        pickerInput(
          inputId = "orgList",
          choices = NULL, #c(unique(Organization$OrganizationName)),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = 'contains'),
          width = "70%"
        ),
        dateInput(
          inputId = "dq_org_startdate",
          label = "Report Start Date",
          format = "mm/dd/yyyy",
          value = NULL, # ymd(hc_check_dq_back_to),
          width = "25%"
        ),
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
      )
    ),
    tabItem(
      tabName = "dqSystem",
      fluidRow(
        box(htmlOutput("headerSystemDQ"), width = 12),
        box(plotOutput("systemDQErrors"), width = 12,
            solidHeader = TRUE,
            status = "danger",
            title = "Projects with the Most High Priority Issues and Errors"),
        box(plotOutput("systemHHErrors"), width = 12,
            solidHeader = TRUE,
            status = "danger",
            title = "Projects with the Most Household Errors"),
        box(plotOutput("systemDQEligibility"), width = 12,
            solidHeader = TRUE,
            status = "warning",
            title = "Projects with the Most Eligibility Warnings"),
        box(plotOutput("systemDQWarnings"), width = 12,
            solidHeader = TRUE,
            status = "warning",
            title = "Projects with the Most Warnings"),
        box(plotOutput("systemDQErrorTypes"), width = 12,
            solidHeader = TRUE,
            status = "danger",
            title = "Most Common High Priority Issues and Errors"),
            solidHeader = TRUE,
            status = "warning",
            title = "Most Common Warnings")
      )
    )
  )
)
