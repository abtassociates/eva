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
  dashboardHeader(title = "Data Quality"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Home",
               tabName = "homeTab"),
      menuItem("Upload Hashed CSV",
               tabName = "uploadCSV"),
      menuItem("Export Validator",
               tabName = "tabValidate"),
      menuItem(
        "Reality Check",
        tabName = "realityCheck",
        menuSubItem("Client Counts",
                    tabName = "currentProviderLevel"),
        menuSubItem("Bed and Unit Utilization",
                    tabName = "utilizationTab")
      ), 
      menuItem("PDDE Checker",
               tabName = "tabPDDE"),
      menuItem("Data Quality",
        menuSubItem("Project-level", 
                    tabName = "dqTab"),
        menuSubItem("Organization-level", 
                    tabName = "dqOrganization"),
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
    ),
    HTML(paste0(
      "<br>&emsp;Data last refreshed:&emsp;<br>&emsp;",
      format(meta_HUDCSV_Export_Date, "%m-%d-%Y %I:%M %p")
      ,
      "<p><p>&emsp;" # add short message here if you want <-
    )),
    
    br(),
    br(),
    br(),
    br(),
    actionButton(inputId = "logOutButton",
                 label = "Log Out",
                 onclick =
                   "window.open('https://ohiobalanceofstatecoc.shinyapps.io/Rminor_elevated/__logout__/')")
  ), 
  dashboardBody(tabItems(
    tabItem(
      tabName = "homeTab",
      htmlOutput("headerHome"),
      width = 12
    ),
    tabItem(tabName = "uploadCSV",
            fileInput(
              "file1",
              "Upload Hashed CSV file",
              multiple = TRUE,
              accept = ".zip"
            )), 
    tabItem(
      tabName = "currentProviderLevel",
      fluidRow(box(htmlOutput("headerCurrent"), width = 12)),
      fluidRow(box(
        pickerInput(
          label = "Select Project",
          inputId = "currentProviderList",
          choices = projects,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = 'contains')
        ),
        dateRangeInput(
          "dateRangeCount",
          "Date Range",
          min = meta_HUDCSV_Export_Start,
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
          choices = c(sort(utilization_bed$ProjectName)),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = 'contains'),
          width = "100%"
        ),
        airDatepickerInput(
          inputId = "utilizationDate",
          label = "Report End Month for Annual Plot",
          max =
            ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
          min =
            ymd(floor_date(
              meta_HUDCSV_Export_Date - days(335), unit = "month"
            )),
          dateFormat = "MM yyyy",
          view = "month",
          value =
            ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
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
          max = ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
          min = ymd(floor_date(ymd(
            meta_HUDCSV_Export_End
          ), "month") - years(2) + days(1)),
          dateFormat = "MM yyyy",
          view = "month",
          value =
            ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
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
          choices = dq_providers,
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
          value = ymd(hc_check_dq_back_to),
          min = ymd(meta_HUDCSV_Export_Start),
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
          choices = desk_time_providers,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = 'contains'),
          width = "100%",
          selected = desk_time_providers[1]
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
                choices = c(unique(Organization$OrganizationName)),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains'),
                width = "70%"
              ),
              dateInput(
                inputId = "dq_org_startdate",
                label = "Report Start Date",
                format = "mm/dd/yyyy",
                value = ymd(hc_check_dq_back_to),
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
            ))
  ))
)
