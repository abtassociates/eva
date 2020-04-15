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
      # menuItem("Prioritization",
      #          tabName = "prioritizationListTab"),
      menuItem("Current Clients",
               tabName = "currentProviderLevel"),
      menuItem("Bed and Unit Utilization",
               tabName = "utilizationTab"),
      menuItem(
        "Data Quality",
        menuSubItem("Provider-level", tabName = "dqTab"),
        menuSubItem("Data Entry Delay", tabName = "deskTime"),
        menuSubItem("Unsheltered", tabName = "unsheltered"),
        menuSubItem("Region-level", tabName = "dqRegion"),
        menuSubItem("CoC-wide", tabName = "dqCoC"),
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
                      tabName = "spdatTab1"),
          menuSubItem("County Detail",
                      tabName = "spdatTab2")
        ),
        menuSubItem("Length of Stay",
                    tabName = "LoSTab"),
        menuSubItem("Exits to Permanent Housing",
                    tabName = "PHTab"),
        menuSubItem("Non-Cash Benefits at Exit",
                    tabName = "NCBTab"),
        menuSubItem("Health Insurance at Exit",
                    tabName = "HITab"),
        menuSubItem("Income Growth",
                    tabName = "incomeTab"),
        menuSubItem("Rapid Placement for RRH",
                    tabName = "rapidTab"),
        menuSubItem("RRH Spending",
                    tabName = "spendingTab")
      )
    ),
    HTML(
      paste0(
        "<br>&emsp;Data last refreshed:&emsp;<br>&emsp;",
        format(update_date, "%m-%d-%Y %I:%M %p", tz = "US/Eastern")
        ,
        "<p><p>&emsp;Wash your hands!"
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
                    choices = regions %>% arrange(County) %>% pull(County),
                    options = list('live-search' = TRUE),
                    selected = regions %>% arrange(County) %>% pull(County) %>%
                      sample(1)
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
                width = 12
              )),
              fluidRow(box(
                DT::dataTableOutput("currentClients"),
                width = 12
              ))),
      tabItem(
        tabName = "utilizationTab",
        fluidPage(
          fluidRow(box(htmlOutput(
            "headerUtilization"
          ), width = 12)),
          fluidRow(box(
            pickerInput(
              label = "Select Provider",
              inputId = "providerListUtilization",
              choices = c(sort(utilization_bed$ProjectName)),
              options = list(`live-search` = TRUE),
              width = "100%"
            ),
            airDatepickerInput(
              inputId = "utilizationDate",
              label = "Click to Choose a Month",
              max = ymd(floor_date(update_date, unit = "month") - days(1)),
              min = ymd(floor_date(mdy(FileEnd), "month") - years(2) + days(1)), 
              dateFormat = "MM yyyy",
              view = "month",
              value =
                ymd(floor_date(update_date, unit = "month") - days(1)),
              minView = "months",
              addon = "none",
              autoClose = TRUE,
              width = '50%'
            ),
            width = '100%'
          )),
          fluidRow(
            box(
              infoBoxOutput("utilizationSummary0", width = '100%'),
              infoBoxOutput("utilizationSummary1", width = '100%'),
              infoBoxOutput("utilizationSummary2", width = '100%'),
              width = '100%'
            )
          ),
          fluidRow(box(
            DT::dataTableOutput("utilizationDetail"), width = '100%'
          ))
        )
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
            selected = sample(dq_providers, 1)
          ),
          dateInput(
            inputId = "dq_startdate",
            label = "Report Start Date",
            format = "mm/dd/yyyy",
            value = mdy("10012018"),
            min = ymd(floor_date(mdy(FileEnd), "year") - years(2)),
            width = "25%"
          ),
          width = 12
        )),
        fluidRow(uiOutput("DQ_APs_w_EEs")),
        fluidRow(uiOutput("DQAPsNoReferrals")),
        fluidRow(uiOutput("DQDuplicateEEs"),
                 uiOutput("DQHHIssues"),
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
        fluidRow(uiOutput("dq_provider_summary_box"))
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
                  selected = sample(dtproviders, 1)
                ),
                width = 12
              )),
              fluidRow(box(plotOutput(
                "DeskTimePlotDetail"
              ))),
              box(
                uiOutput("deskTimeNote"),
                title = "More Information",
                collapsible = TRUE,
                collapsed = TRUE
              )), 
      tabItem(
        tabName = "dqRegion",
        fluidRow(box(htmlOutput(
          "headerRegionDataQuality"
        ), width = 12)),
        fluidRow(box(
          pickerInput(
            inputId = "regionList3",
            choices = c(unique(regions$RegionName)),
            options = list(`live-search` = TRUE),
            width = "70%"
          ),
          dateInput(
            inputId = "dq_region_startdate",
            label = "Report Start Date",
            format = "mm/dd/yyyy",
            value = mdy("10012018"),
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
            choices = sort(dq_unsheltered$DefaultProvider) %>%
              unique(),
            options = list('live-search' = TRUE),
            width = "100%"
          ),
          dateInput(
            inputId = "unsh_dq_startdate",
            label = "Report Start Date",
            format = "mm/dd/yyyy",
            value = mdy("01012019"),
            width = "25%"
          ),
          width = 12
        )),
        fluidRow(
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
          box(
            plotOutput("cocDQErrors"),
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            title = "Providers with the Most Data Quality Errors"
          ),
          box(
            plotOutput("cocHHErrors"),
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            title = "Providers with the Most Household Errors"
          ),
          box(
            plotOutput("cocUnshelteredHigh"),
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            title = "Unsheltered High Priority Issues (User's Default Provider)"
          ),
          box(
            plotOutput("DeskTimePlotCoC"),
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            title = "Longest Data Entry Delay Medians (in the past 365 days)"
          ),
          box(
            plotOutput("cocDQWarnings"),
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            title = "Providers with the Most Data Quality Warnings"
          ),
          box(
            plotOutput("cocDQErrorTypes"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            title = "Top 10 Error Types"
          ),
          box(
            plotOutput("cocDQWarningTypes"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            title = "Top 10 Warning Types"
          ),
          box(
            plotOutput("cocEligibility"),
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            title = "Providers with Potential Eligibility Issues"
          )
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
          box(
            DT::dataTableOutput("cocWidespreadIssues"),
            title = "Widespread Issues (Training focus)",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(tabName = "ceCoC",
              fluidPage(
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
                  plotOutput("cocSPDAT"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "warning",
                  title = "Current Households Without SPDAT (minus Veterans)"
                ),
                box(
                  plotOutput("cocOutstandingReferrals"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "warning",
                  title = "Top 20 Providers with Old Outstanding Referrals"
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
                  choices = sort(pe_validation_summary$AltProjectName) %>%
                    unique(),
                  selected = sample(pe_validation_summary$AltProjectName, 1),
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
      tabItem(
        tabName = "spdatTab1",
        fluidRow(box(
          htmlOutput("headerCommunityNeedPH"), width = 12
        )),
        fluidRow(
          box(
            pickerInput(
              inputId = "regionList1",
              choices = c(unique(regions$RegionName)),
              options = list(`live-search` = TRUE),
              width = "70%"
            ),
            dateRangeInput(
              "spdatDateRange",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
            )
          )
        ),
        fluidRow(infoBoxOutput("ScoredHousedSummary", width = 12)),
        fluidRow(box(
          DT::dataTableOutput("SPDATScoresHoused"), width = 12
        ))
      ),
      tabItem(
        tabName = "spdatTab2",
        fluidRow(box(
          htmlOutput("headerCommunityNeedCounty"), width = 12
        )),
        fluidRow(
          box(
            pickerInput(
              inputId = "regionList2",
              choices = c(unique(regions$RegionName)),
              options = list(`live-search` = TRUE),
              width = "70%"
            ),
            dateRangeInput(
              "spdatDateRange2",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
            )
          )
        ),
        fluidRow(infoBoxOutput("ScoredInRegionSummary", width = 12)),
        fluidRow(box(
          DT::dataTableOutput("SPDATScoresServedInCounty"), width = 12
        ))
      ),
      tabItem(
        tabName = "LoSTab",
        fluidRow(box(htmlOutput("headerLoS"), width = 12)),
        fluidRow(
          box(
            pickerInput(
              inputId = "LoSProjectList",
              choices = c(unique(qpr_leavers$ProjectName[qpr_leavers$ProjectType %in% c(1, 2, 8, 13)])),
              options = list(`live-search` = TRUE),
              width = "70%"
            ),
            dateRangeInput(
              "LoSDateRange",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
            ),
            width = 12
          )
        ),
        fluidRow(infoBoxOutput("LoSSummary", width = 12)),
        fluidRow(box(
          DT::dataTableOutput("LoSDetail"), width = 12
        ))
      ),
      tabItem(
        tabName = "PHTab",
        fluidRow(box(htmlOutput("headerExitsToPH"), width = 12)),
        fluidRow(
          box(
            pickerInput(
              inputId = "ExitsToPHProjectList",
              choices = c(unique(qpr_leavers$ProjectName[qpr_leavers$ProjectType %in% c(1:4, 8:9, 12:13)])),
              options = list(`live-search` = TRUE),
              width = "70%"
            ),
            dateRangeInput(
              "ExitsToPHDateRange",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
            ),
            width = 12
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
      tabItem(
        tabName = "NCBTab",
        fluidRow(box(htmlOutput("headerNCBs"), width = 12)),
        fluidRow(
          box(
            pickerInput(
              inputId = "MBProjectListNC",
              choices = c(unique(qpr_benefits$ProjectName)),
              options = list(`live-search` = TRUE),
              width = "70%"
            ),
            dateRangeInput(
              "NCBDateRange",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
            ),
            width = 12
          )
        ),
        fluidRow(infoBoxOutput("qprNCBSummary", width = 12)),
        fluidRow(box(
          DT::dataTableOutput("ExitedWithNCBs"), width = 12
        ))
      ),
      tabItem(
        tabName = "HITab",
        fluidRow(box(
          htmlOutput("headerHealthInsurance"), width = 12
        )),
        fluidRow(
          box(
            pickerInput(
              inputId = "MBProjectListHI",
              choices = c(unique(qpr_benefits$ProjectName)),
              options = list(`live-search` = TRUE),
              width = "70%"
            ),
            dateRangeInput(
              "HIDateRange",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
            )
          )
        ),
        fluidRow(infoBoxOutput("healthInsuranceSummary", width = 12)),
        fluidRow(box(
          DT::dataTableOutput("ExitedWithInsurance"),
          width = 12
        ))
      ),
      tabItem(
        tabName = "incomeTab",
        fluidRow(box(
          htmlOutput("headerIncomeIncrease"), width = 12
        )),
        fluidRow(
          box(
            pickerInput(
              inputId = "incomeProjectList",
              choices = c(unique(qpr_income$ProjectName)),
              options = list(`live-search` = TRUE),
              width = "70%"
            ),
            dateRangeInput(
              "IncomeDateRange",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
            )
          )
        ),
        fluidRow(infoBoxOutput("qprIncomeSummary", width = 12)),
        fluidRow(box(
          DT::dataTableOutput("IncomeIncrease"), width = 12
        ))
      ),
      tabItem(
        tabName = "rapidTab",
        fluidRow(box(htmlOutput(
          "headerDaysToHouse"
        ), width = 12)),
        fluidRow(
          box(
            setSliderColor("#56B4E9", 1),
            pickerInput(
              inputId = "RapidRRHProviderList",
              choices = c(unique(sort(
                qpr_rrh_enterers$ProjectName
              ))),
              options = list(`live-search` = TRUE),
              width = "100%"
            ),
            dateRangeInput(
              "DaysToHouseDateRange",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
            )
          )
        ),
        fluidRow(infoBoxOutput("daysToHouseSummary", width = 12)),
        fluidRow(box(
          DT::dataTableOutput("daysToHouseRRH"), width = 12
        ))
      ),
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
                qpr_spending$OrganizationName
              ))),
              options = list(`live-search` = TRUE),
              width = "100%"
            ),
            dateRangeInput(
              "RRHSpendingDateRange",
              "Date Range",
              start = floor_date(today() - months(1), "year"),
              end = today(),
              min = FileStart,
              format = "mm-dd-yyyy"
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
