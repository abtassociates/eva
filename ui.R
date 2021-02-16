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
  dashboardHeader(title = "R minor elevated"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Home",
               tabName = "homeTab"),
      menuItem("Prioritization",
               tabName = "prioritizationListTab"),
      menuItem("Client Counts",
               tabName = "currentProviderLevel"),
      # menuItem("Ending Veteran Homelessness",
      #          menuSubItem("Active List", tabName = "vetActiveList"),
      #          menuSubItem("USICH Benchmarks", tabName = "dashUSICH"),
      #          menuSubItem("Inflow Outflow", tabName = "flow")
      #          ),
      menuItem("Bed and Unit Utilization",
               tabName = "utilizationTab"),
      menuItem(
        "Data Quality",
        menuSubItem("Provider-level", tabName = "dqTab"),
        menuSubItem("Data Entry Timeliness", tabName = "deskTime"),
        menuSubItem("Unsheltered", tabName = "unsheltered"),
        menuSubItem("Region-level", tabName = "dqRegion"),
        menuSubItem("System-wide", tabName = "dqCoC"),
        menuSubItem("CE Summary", tabName = "ceCoC")
      ),
      # menuItem("CoC Competition",
      #          tabName = "cocCompetitionTab"),
      menuItem(
        "Quarterly Performance Report",
        menuItem(
          "Community Need",
          tabName = "spdatTab",
          menuSubItem("PSH/RRH Detail",
                      tabName = "spdat1-Tab"),
          menuSubItem("County Detail",
                      tabName = "spdat2-Tab")
        ),
        menuSubItem("Length of Stay",
                    tabName = "LoS-Tab"),
        menuSubItem("Exits to Permanent Housing",
                    tabName = "PHTab"),
        menuSubItem("Non-Cash Benefits at Exit",
                    tabName = "NCB-Tab"),
        menuSubItem("Health Insurance at Exit",
                    tabName = "HI-Tab"),
        menuSubItem("Income Growth",
                    tabName = "income-Tab"),
        menuSubItem("Rapid Placement for RRH",
                    tabName = "rapid-Tab"),
        menuSubItem("RRH Spending",
                    tabName = "spendingTab")
      )
    ),
    HTML(
      paste0(
        "<br>&emsp;Data last refreshed:&emsp;<br>&emsp;",
        format(meta_HUDCSV_Export_Date, "%m-%d-%Y %I:%M %p")
        ,
        "<p><p>&emsp;Wear your mask! Be well."
      )
    ),
    br(),
    br(),
    br(),
    br(),
    actionButton(
      inputId = "logOutButton",
      label = "Log Out",
      onclick =
        "window.open('https://ohiobalanceofstatecoc.shinyapps.io/Rminor_elevated/__logout__/')"
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "homeTab",
        htmlOutput("headerHome"),
        width = 12
      ),
      tabItem(tabName = "prioritizationListTab",
              fluidRow(box(
                htmlOutput("headerPrioritization"), width = 12
              )),
              fluidRow(
                box(
                  pickerInput(
                    label = "Select County/-ies",
                    inputId = "prioritizationCounty",
                    multiple = TRUE,
                    choices = regions() %>% 
                      filter(County != "Mahoning") %>%
                      arrange(County) %>% pull(County),
                    options = list('live-search' = TRUE)
                  ),
                  downloadButton("downloadActiveList", "Download")
                ),
                width = 12
              ),
              fluidRow(box(
                DT::dataTableOutput("prioritizationData"), 
                width = 12,
                footer = "Dark gray cells mean the client has a Data Quality issue that may be causing incorrect information to show."
              ))), 
      tabItem(tabName = "currentProviderLevel",
              fluidRow(box(
                htmlOutput("headerCurrent"), width = 12
              )),
              fluidRow(box(
                pickerInput(
                  label = "Select Provider",
                  inputId = "currentProviderList",
                  choices = providers,
                  options = list('live-search' = TRUE)
                ),
                dateRangeInput(
                  "dateRangeCount",
                  "Date Range",
                  min = meta_HUDCSV_Export_Start,
                  format = "mm/dd/yyyy",
                  width = '50%'
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
              ))),
      tabItem(
        tabName = "utilizationTab",
        fluidPage(
          fluidRow(box(htmlOutput(
            "headerUtilization"
          ), width = 12)),
          fluidRow(
            box(
              title = "NOTICE",
              status = "warning",
              solidHeader = TRUE,
              "During this time, congregate facilities should be aiming to deconcentrate. If this causes fluctuations in Utilization, that is okay. Please continue to keep your clients safe."
              ,
              width = 6
            )
          ), 
          fluidRow(box(
            pickerInput(
              label = "Select Provider",
              inputId = "providerListUtilization",
              choices = c(sort(utilization_bed()$ProjectName)),
              options = list(`live-search` = TRUE),
              width = "100%"
            ),
            airDatepickerInput(
              inputId = "utilizationDate",
              label = "Click to Choose a Month",
              max = ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
              min = ymd(floor_date(ymd(meta_HUDCSV_Export_End), "month") - years(2) + days(1)), 
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
          fluidRow(
            box(
              infoBoxOutput("utilizationSummary0", width = '100%'),
              infoBoxOutput("utilizationSummary1", width = '100%'),
              infoBoxOutput("utilizationSummary2", width = '100%'),
              width = 12
            )
          ),
          fluidRow(box(
            DT::dataTableOutput("utilizationDetail"), width = 12
          ))
        )
      ),
      tabItem(
        tabName = "vetActiveList",
        fluidRow(box(
          DT::dataTableOutput("VeteranActiveList"),
          title = "Veteran Active List",
          width = 12
        ))
      ),
      tabItem(
        tabName = "dqTab",
        fluidRow(box(htmlOutput(
          "headerDataQuality"
        ), width = 12)),
        fluidRow(box(
          pickerInput(
            label = "Select Provider",
            inputId = "providerListDQ",
            choices = dq_providers,
            options = list('live-search' = TRUE),
            width = "100%",
            selected = dq_providers[1]
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
        fluidRow(uiOutput("DQ_APs_w_EEs")),
        fluidRow(uiOutput("DQAPsNoReferrals")),
        fluidRow(uiOutput("DQHHIssues"),
                 uiOutput("DQDuplicateEEs"),
                 uiOutput("DQIncorrectEEType"),
                 uiOutput("DQMissingLocation"),
                 uiOutput("DQPATHMissingContact")),
        fluidRow(uiOutput("DQIneligible")),
        fluidRow(uiOutput("DQOverlappingEEs")), 
        fluidRow(
          box(
            DT::dataTableOutput("DQErrors"),
            title = "Data Quality Errors",
            width = 12
          )
        ),
        fluidRow(
          box(
            DT::dataTableOutput("DQWarnings"),
            title = "Data Quality Warnings",
            width = 12
          )
        )
        ,
        fluidRow(box(
          id = "DQSummaryProvider",
          DT::dataTableOutput("dq_provider_summary_table"),
          title = "Data Quality Guidance",
          width = 12,
          status = "info",
          solidHeader = TRUE))
      ),
      tabItem(tabName = "deskTime",
              fluidRow(box(htmlOutput("headerDeskTime"),
                           width = 12)),
              fluidRow(box(
                pickerInput(
                  label = "Select Provider",
                  inputId = "providerDeskTime",
                  choices = dtproviders,
                  options = list('live-search' = TRUE),
                  width = "100%",
                  selected = dtproviders[1]
                ),
                width = 12
              )),
              fluidRow(box(plotOutput(
                "DeskTimePlotDetail"
              ), width = 12)),
              fluidRow(box(
                uiOutput("deskTimeNote"),
                title = "More Information",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12
              ))), 
      tabItem(
        tabName = "dqRegion",
        fluidRow(box(htmlOutput(
          "headerRegionDataQuality"
        ), width = 12)),
        fluidRow(box(
          pickerInput(
            inputId = "regionList3",
            choices = c(unique(regions()$RegionName)),
            options = list(`live-search` = TRUE),
            width = "70%"
          ),
          dateInput(
            inputId = "dq_region_startdate",
            label = "Report Start Date",
            format = "mm/dd/yyyy",
            value = ymd(hc_check_dq_back_to),
            width = "25%"
          ),
          width = 12
        )),
        fluidRow(box(
          id = "DQSummaryRegion",
          title = paste("Data Quality Summary"),
          status = "info",
          solidHeader = TRUE,
          DT::dataTableOutput("dq_region_summary_table"),
          width = 12))
      ),
      tabItem(
        tabName = "unsheltered",
        fluidRow(box(
          htmlOutput("headerUnshDataQuality"), width = 12
        )),
        fluidRow(box(
          pickerInput(
            inputId = "unshDefaultProvidersList",
            label = "Select your DEFAULT Provider",
            choices = sort(dq_unsheltered()$DefaultProvider) %>%
              unique(),
            options = list('live-search' = TRUE),
            width = "100%"
          ),
          dateInput(
            inputId = "unsh_dq_startdate",
            label = "Report Start Date",
            format = "mm/dd/yyyy",
            value = ymd(hc_check_dq_back_to),
            width = "25%"
          ),
          width = 12
        )),
        fluidRow(
          uiOutput("unshIncorrectEEType"),
          uiOutput("unshIncorrectResPrior"),
          uiOutput("unshMissingCounty"),
          uiOutput("unshOverlaps"),
          uiOutput("unshHHIssues"),
          uiOutput("unshDuplicateEEs")
        ),
        fluidRow(
          box(
            DT::dataTableOutput("unshDQErrorsTable"),
            title = "Unsheltered Data Quality Errors",
            width = 12
          )
        ),
        fluidRow(
          box(
            DT::dataTableOutput("unshDQWarningsTable"),
            title = "Unsheltered Data Quality Warnings",
            width = 12
          )
        )
        ,
        fluidRow(uiOutput("dq_unsheltered_summary_box"))
      ),
      # tabItem(tabName = "diversion"),
      tabItem(
        tabName = "dqCoC",
        fluidRow(box(htmlOutput("headerCocDQ"), width = 12)),
        fluidRow(
          list(dq_plot_projects_errors = list(status = "danger",
                                  title = "Providers with the Most High Priority Issues and Errors"),
               
               dq_plot_hh_errors = list(status = "danger",
                                  title = "Providers with the Most Household Errors"),
               dq_plot_unsheltered_high = list(status = "danger",
                                         title = "Unsheltered High Priority Issues (User's Default Provider)"),
               dq_plot_projects_warnings = list(status = "warning",
                                    title = "Providers with the Most Data Quality Warnings"),
               DeskTimePlotCoC = list(width = 12,
                                      status = "warning",
                                      title = "Longest Data Entry Delay Medians (in the past 365 days)"),
               dq_plot_errors = list(status = "primary",
                                      title = "Top 10 Error Types"),
               dq_plot_warnings = list(status = "primary",
                                        title = "Top 10 Warning Types"),
               dq_plot_eligibility = list(status = "warning",
                                     title = "Providers with Potential Eligibility Issues")
          ) %>% 
            purrr::imap(~{
              do.call(shinydashboard::box, purrr::list_modify(
                list(imageOutput(.y),
                     solidHeader = TRUE,
                     status = "danger",
                     title = NULL),
                !!!.x))
            }) 
        ),
        fluidRow(
          box(
            DT::dataTableOutput("cocOverlap"),
            title = "Top 20 Providers with Overlapping Entry Exits",
            solidHeader = TRUE,
            status = "warning"
          ),
          box(DT::dataTableOutput("cocLongStayers"),
              title = "Extremely Long Stayers",
              solidHeader = TRUE,
              status = "warning"),
          box(DT::dataTableOutput("cocRRHDestination"),
              title = "Destinations & Rapid Rehousing",
              solidHeader = TRUE,
              status = "warning"),
          box(
            DT::dataTableOutput("cocWidespreadIssues"),
            title = "Widespread Issues (Training focus)",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(tabName = "ceCoC",
                box(
                  plotOutput("cocAPsNoReferrals"),
                  width = 6,
                  title = "Access Points Creating Referrals"
                ),
                box(
                  DT::dataTableOutput("cocAPsNoReferralsList"),
                  width = 6,
                  title = "APs Not Creating Referrals"
                ),
                box(
                  imageOutput("dq_plot_hh_no_spdat"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "warning",
                  title = "Current Households Without SPDAT (minus Veterans)"
                ),
                box(
                  imageOutput("dq_plot_outstanding_referrals"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "warning",
                  title = "Top 20 Providers with Old Outstanding Referrals"
                ),
                fluidRow(box(
                  pickerInput(
                    inputId = "unshEntriesByMonth_County",
                    label = "Select County/-ies",
                    choices = sort(unsheltered_by_month()$County) %>%
                      unique(),
                    selected = c("Lake", 
                                 "Ashtabula", 
                                 "Trumbull", 
                                 "Geauga", 
                                 "Portage"),
                    multiple = TRUE,
                    options = pickerOptions(
                      liveSearch = TRUE,
                      actionsBox = TRUE
                      ),
                    width = "100%"
                  ),
                  airDatepickerInput(
                    inputId = "unshEntriesByMonth_ReportStart",
                    label = "Report Start Month",
                    dateFormat = "MM yyyy",
                    max =
                      ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
                    min =
                      ymd(floor_date(meta_HUDCSV_Export_Start, unit = "month")),
                    view = "month",
                    value =
                      ymd(floor_date(meta_HUDCSV_Export_Date - days(182), unit = "month")),
                    minView = "months",
                    addon = "none",
                    autoClose = TRUE,                    
                    width = "25%"
                  ),
                  plotlyOutput("cocUnshelteredEntriesByMonth"),
                  width = 12,
                  title = "Unsheltered Entries by Month",
                  footer = "Where the CountyServed data was not answered, the 
                  County where the user who created the project stay is based was 
                  substituted.",
                  status = "info",
                  solidHeader = TRUE
                )
              )),
      tabItem(tabName = "cocCompetitionTab",
              fluidRow(box(
                htmlOutput("headerCoCCompetitionProjectLevel"),
                width = 12
              )),
              fluidRow(box(
                pickerInput(
                  inputId = "pe_provider",
                  label = "Select your CoC-funded Provider",
                  choices = sort(pe_validation_summary()$AltProjectName) %>%
                    unique(),
                  selected = pe_validation_summary()$AltProjectName[1],
                  options = list('live-search' = TRUE),
                  width = "100%"
                ),
                width = 12
              )),
              fluidRow(box(
                DT::dataTableOutput("pe_ProjectSummary"), 
                width = 12,
                title = "Score Summary",
                status = "info",
                solidHeader = TRUE, 
                collapsible = TRUE
              )),
              fluidRow(tabBox(
                id = "tabs",
                # title = "Client Detail",
                tabPanel("Exits to Permanent Housing",
                         DT::dataTableOutput("pe_ExitsToPH")),
                tabPanel("Moved into Own Housing",
                         DT::dataTableOutput("pe_OwnHousing")),
                tabPanel("Increased Income",
                         DT::dataTableOutput("pe_IncreasedIncome")), 
                tabPanel("Benefits & Health Insurance at Exit",
                         DT::dataTableOutput("pe_BenefitsAtExit")),
                tabPanel("Living Situation at Entry",
                         DT::dataTableOutput("pe_LivingSituationAtEntry")),
                tabPanel("No Income at Entry",
                         DT::dataTableOutput("pe_NoIncomeAtEntry")),
                tabPanel("Length of Stay",
                         DT::dataTableOutput("pe_LengthOfStay")),
                tabPanel("Median Homeless History Index",
                         DT::dataTableOutput("pe_MedianHHI")),
                tabPanel("Long Term Homeless",
                         DT::dataTableOutput("pe_LongTermHomeless")),
                tabPanel("VISPDAT Score Completion",
                         DT::dataTableOutput("pe_ScoredAtPHEntry")),
                width = 12
              ))), 
      mod_QPR_tabItem_ui("spdat1"),
      mod_QPR_tabItem_ui("spdat2"),
      mod_QPR_tabItem_ui("LoS"),
      tabItem(
        tabName = "PHTab",
        fluidRow(box(htmlOutput("headerExitsToPH"), width = 12)),
        fluidRow(
          box(
            pickerInput(
              inputId = "ExitsToPHProjectList",
              choices = c(unique(qpr_leavers()$ProjectName[
                qpr_leavers()$ProjectType %in% c(1:4, 8:9, 12:13)])),
              options = list(`live-search` = TRUE),
              width = "70%"
            ),
            
            dateRangeInput(
              "ExitsToPHDateRange",
              "Date Range",
              start = floor_date(today() - days(31), "year"),
              end = today(),
              min = meta_HUDCSV_Export_Start,
              format = "mm/dd/yyyy"
            )
          )
        ),
        fluidRow(infoBoxOutput("ExitsToPHSummary", width = 12)),
        fluidRow(box(
          DT::dataTableOutput("ExitsToPH"), width = 12
        )),
        br(),
        br(),
        fluidRow(box(
          DT::dataTableOutput("ExitsToPHOutreach"),
          width = 12
        ))
      ),
      mod_QPR_tabItem_ui("NCB"),
      mod_QPR_tabItem_ui("HI"),
      mod_QPR_tabItem_ui("income"),
      mod_QPR_tabItem_ui("rapid"),
      tabItem(
        tabName = "spendingTab",
        fluidRow(box(htmlOutput(
          "headerRRHSpending"
        ), width = 12)),
        fluidRow(
          box(
            setSliderColor("#56B4E9", 1),
            pickerInput(
              inputId = "RRHSpendingOrganizationList",
              label = "Select Organization",
              choices = c(unique(sort(
                qpr_spending()$OrganizationName
              ))),
              options = list(`live-search` = TRUE),
              width = "100%"
            ),
            dateRangeInput(
              "RRHSpendingDateRange",
              "Date Range",
              start = floor_date(today() - days(31), "year"),
              end = today(),
              min = meta_HUDCSV_Export_Start,
              format = "mm/dd/yyyy"
            )
          )
        ),
        # fluidRow(infoBoxOutput("notCreatedYet"), width = 3),
        fluidRow(
          box(
            DT::dataTableOutput("RRHSpending"),
            title = "Rapid Rehousing Spending",
            width = 12
          )
        ),
        fluidRow(
          box(
            DT::dataTableOutput("HPSpending"),
            title = "Prevention Spending",
            width = 12
          )
        )
      )
      
    )
  )
)
