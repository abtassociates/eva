# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
#<https://www.gnu.org/licenses/>.

dashboardPage(
  skin = "black",
  dashboardHeader(title = "R minor elevated"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Home",
               tabName = "homeTab"),
      # menuItem(
      #   "Prioritization",
      #   menuSubItem("Prioritization List",
      #               tabName = "prioritizationListTab"),
      #   menuSubItem("Contact",
      #               tabName = "contactTab"),
      #   menuSubItem("Veteran Active List",
      #               tabName = "vetActiveListTab")
      # ),
      menuItem("Current Clients",
               tabName = "currentProviderLevel"),
      menuItem("Bed and Unit Utilization",
               tabName = "utilizationTab"),
      menuItem(
        "Data Quality",
        menuSubItem("Provider-level", tabName = "dqTab"),
        menuSubItem("Unsheltered", tabName = "unsheltered"),
        # menuSubItem("Diversion", tabName = "diversion"),
        menuSubItem("CoC-wide", tabName = "dqCoC"),
        menuSubItem("CE Summary", tabName = "ceCoC")
      ),
      # menuItem("CoC Competition",
      #          tabName = "cocCompetitionTab"),
      menuItem(
        "Performance and Outcomes",
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
    HTML(paste0(
      "<br>&emsp;Data last refreshed:&emsp;<br>&emsp;",
      format(update_date, "%m-%d-%Y %I:%M %p", tz = "US/Eastern"),
            "<p><p>&emsp;Happy Holidays!"
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
  dashboardBody(
    tabItems(
      tabItem(tabName = "homeTab",
              htmlOutput("headerHome"), width = 12),
      tabItem(
        tabName = "currentProviderLevel",
        fluidRow(box(htmlOutput("headerCurrent"), width = 12)),
        fluidRow(box(pickerInput(
          label = "Select Provider",
          inputId = "currentProviderList",
          choices = providers,
          options = list('live-search' = TRUE)
        ), width = 12)),
        fluidRow(box(DT::dataTableOutput("currentClients"),
                     width = 12))),
      tabItem(
        tabName = "utilizationTab",
        fluidPage(fluidRow(box(htmlOutput("headerUtilization"), width = 12)),
        fluidRow(box(pickerInput(
          label = "Select Provider",
          inputId = "providerListUtilization",
          choices = c(sort(BedUtilization$ProjectName)),
          options = list(`live-search` = TRUE),
          width = "100%"
        ),
        airDatepickerInput(inputId = "utilizationDate",
                           label = "Click to Choose a Month",
                           max = 
                             ymd(floor_date(update_date, unit = "month") - days(1)),
                           dateFormat = "MM yyyy",
                           view = "month",
                           value = 
                             ymd(floor_date(update_date, unit = "month") - days(1)),
                           minView = "months",
                           addon = "none",
                           autoClose = TRUE,
                           width = '50%'
        ), width = '100%')),
        fluidRow(box(
          infoBoxOutput("utilizationSummary0", width = '100%'),
          infoBoxOutput("utilizationSummary1", width = '100%'),
          infoBoxOutput("utilizationSummary2", width = '100%'), width = '100%')
        ), 
        fluidRow(box(DT::dataTableOutput("utilizationDetail"), width = '100%')))),     
      tabItem(
        tabName = "dqTab",
        fluidRow(box(htmlOutput("headerDataQuality"), width = 12)),
        fluidRow(box(
          pickerInput(
            label = "Select Provider",
            inputId = "providerListDQ",
            choices = dqProviders,
            options = list('live-search' = TRUE),
            width = "100%",
            selected = sample(dqProviders, 1)
          ),
          dateInput(
            inputId = "dq_startdate",
            label = "Report Start Date",
            format = "mm/dd/yyyy",
            value = mdy("10012018"),
            width = "25%"
          ), width = 12
        )),
        fluidRow(
          uiOutput("DQDuplicateEEs"),
          uiOutput("DQHHIssues")
        ),
        uiOutput("DQIneligible"),
        uiOutput("DQAPsNoReferrals"),
        uiOutput("DQOverlappingEEs"), 
        fluidRow(box(
          DT::dataTableOutput("DQErrors"),
          title = "Data Quality Errors",
          width = 12
        )), 
        fluidRow(box(
          DT::dataTableOutput("DQWarnings"),
          title = "Data Quality Warnings",
          width = 12
        ))
      ), 
      tabItem(tabName = "unsheltered", 
              fluidRow(box(htmlOutput("headerUnshDataQuality"), width = 12)),
              fluidRow(box(
                pickerInput(
                  inputId = "unshDefaultProvidersList",
                  label = "Select your DEFAULT Provider",
                  choices = sort(unshelteredDataQuality$DefaultProvider) %>% 
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
                ), width = 12
              )),
              fluidRow(
                uiOutput("unshIncorrectResPrior"),
                uiOutput("unshMissingCounty"),
                uiOutput("unshOverlaps"),
                uiOutput("unshHHIssues"),
                uiOutput("unshDuplicateEEs")
              ), 
                fluidRow(box(DT::dataTableOutput("unshDQErrorsTable"),
                             title = "Unsheltered Data Quality Errors",
                             width = 12)),
                fluidRow(box(DT::dataTableOutput("unshDQWarningsTable"),
                             title = "Unsheltered Data Quality Warnings",
                             width = 12))
              ),
      # tabItem(tabName = "diversion"),
      tabItem(tabName = "dqCoC",
              fluidRow(box(htmlOutput("headerCocDQ"), width = 12)),
              fluidRow(
                box(plotOutput("cocDQErrors"), 
                    width = 12,
                    solidHeader = TRUE,
                    status = "danger",
                    title = "Providers with the Most Data Quality Errors"),
                box(plotOutput("cocHHErrors"), 
                    width = 12,
                    solidHeader = TRUE,
                    status = "danger",
                    title = "Providers with the Most Household Errors"),
                box(plotOutput("cocDQWarnings"), 
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    title = "Providers with the Most Data Quality Warnings"),
                box(plotOutput("cocDQErrorTypes"), 
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    title = "Top 10 Error Types"),
                box(plotOutput("cocDQWarningTypes"), 
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    title = "Top 10 Warning Types"),
                box(plotOutput("cocEligibility"),
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    title = "Providers with Potential Eligibility Issues")
              ), 
              fluidRow(
                box(DT::dataTableOutput("cocOverlap"),
                    title = "Top 20 Providers with Overlapping Entry Exits",
                    solidHeader = TRUE,
                    status = "warning"),
                box(DT::dataTableOutput("cocWidespreadIssues"),
                    title = "Widespread Issues (Training focus)",
                    solidHeader = TRUE,
                    status = "primary")
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
                box(plotOutput("cocOutstandingReferrals"), 
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    title = "Top 20 Providers with Old Outstanding Referrals")
              )), 
      tabItem(
        tabName = "spdatTab1",
        fluidRow(box(htmlOutput("headerCommunityNeedPH"), width = 12)),
        fluidRow(box(pickerInput(
          inputId = "regionList1",
          choices = c(unique(Regions$RegionName)),
          options = list(`live-search` = TRUE),
          width = "70%"
        ),
        chooseSliderSkin("Round"),
        setSliderColor("#56B4E9", c(1, 2)),
        sliderTextInput("spdatSlider1",
                        "",
                        c(
                          unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                        ),
                        selected = Sys.yearqtr() - 1 / 4))),
        fluidRow(infoBoxOutput("ScoredHousedSummary")),
        fluidRow(box(DT::dataTableOutput("SPDATScoresHoused")))
      ),
      tabItem(
        tabName = "spdatTab2",
        fluidRow(box(htmlOutput("headerCommunityNeedCounty"), width = 12)),
        fluidRow(box(pickerInput(
          inputId = "regionList2",
          choices = c(unique(Regions$RegionName)),
          options = list(`live-search` = TRUE),
          width = "70%"
        ),
        chooseSliderSkin("Round"),
        setSliderColor("#56B4E9", c(1, 2)),
        sliderTextInput("spdatSlider2",
                        "",
                        c(
                          unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                        ),
                        selected = Sys.yearqtr() - 1 / 4))),
        fluidRow(infoBoxOutput("ScoredInRegionSummary")),
        fluidRow(box(DT::dataTableOutput("SPDATScoresServedInCounty")))
      ),      
      tabItem(tabName = "LoSTab",
              fluidRow(box(htmlOutput("headerLoS"), width = 12)),
              fluidRow(box(pickerInput(
                inputId = "LoSProjectList",
                choices = c(unique(
                  qpr_leavers$ProjectName[qpr_leavers$ProjectType %in% c(1, 2, 8, 13)])),
                options = list(`live-search` = TRUE),
                width = "70%"
              ),
              chooseSliderSkin("Round"),
              setSliderColor("#56B4E9", c(1, 2)),
              sliderTextInput("LoSSlider1",
                              "",
                              c(
                                unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                              ),
                              selected = Sys.yearqtr() - 1 / 4))),
              fluidRow(infoBoxOutput("LoSSummary")),
              fluidRow(box(DT::dataTableOutput("LoSDetail")))
              ),
      tabItem(tabName = "PHTab",
              fluidRow(box(htmlOutput("headerExitsToPH"), width = 12)),
              fluidRow(box(pickerInput(
                inputId = "ExitsToPHProjectList",
                choices = c(unique(
                  qpr_leavers$ProjectName[qpr_leavers$ProjectType %in% c(1:4, 8:9, 12:13)])),
                options = list(`live-search` = TRUE),
                width = "70%"
              ),
              setSliderColor("#56B4E9", 1),
              sliderTextInput("ExitsToPHSlider",
                              "",
                              c(
                                unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                              ),
                              selected = Sys.yearqtr() - 1 / 4))),
              fluidRow(infoBoxOutput("ExitsToPHSummary")),
              fluidRow(box(DT::dataTableOutput("ExitsToPH"))),
              br(),
              br(),
              fluidRow(box(DT::dataTableOutput("ExitsToPHOutreach")))),

      tabItem(tabName = "NCBTab",
              fluidRow(box(htmlOutput("headerNCBs"), width = 12)),
              fluidRow(box(
                pickerInput(
                  inputId = "MBProjectListNC",
                  choices = c(unique(QPR_MainstreamBenefits$ProjectName)),
                  options = list(`live-search` = TRUE),
                  width = "70%"
                ),
                setSliderColor("#56B4E9", 1),
                sliderTextInput("dateNCBSlider",
                                "",
                                c(
                                  unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                ),
                                selected = Sys.yearqtr() - 1 / 4)
              )), 
              fluidRow(infoBoxOutput("qprNCBSummary")),
              fluidRow(box(DT::dataTableOutput("ExitedWithNCBs")))),
      tabItem(tabName = "HITab",
              fluidRow(box(htmlOutput("headerHealthInsurance"), width = 12)),
              fluidRow(box(
                pickerInput(
                  inputId = "MBProjectListHI",
                  choices = c(unique(QPR_MainstreamBenefits$ProjectName)),
                  options = list(`live-search` = TRUE),
                  width = "70%"
                ),
                setSliderColor("#56B4E9", 1),
                sliderTextInput("dateHealthInsuranceSlider",
                                "",
                                c(
                                  unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                ),
                                selected = Sys.yearqtr() - 1 / 4)
              )), 
              fluidRow(infoBoxOutput("healthInsuranceSummary")),
              fluidRow(box(DT::dataTableOutput("ExitedWithInsurance")))),
      tabItem(tabName = "incomeTab",
              fluidRow(box(htmlOutput("headerIncomeIncrease"), width = 12)),
              fluidRow(box(pickerInput(
                inputId = "incomeProjectList",
                choices = c(unique(QPR_Income$ProjectName)),
                options = list(`live-search` = TRUE),
                width = "70%"
              ),
              setSliderColor("#56B4E9", 1),
              sliderTextInput("dateIncomeSlider",
                              "",
                              c(
                                unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                              ),
                              selected = Sys.yearqtr() - 1 / 4))),
              fluidRow(infoBoxOutput("qprIncomeSummary")),
              fluidRow(box(DT::dataTableOutput("IncomeIncrease")))),
      tabItem(tabName = "recurrenceTab",
              HTML("<h1>Under Construction</h1>")),
      tabItem(tabName = "rapidTab",
              fluidRow(box(htmlOutput("headerDaysToHouse"), width = 12)),
              fluidRow(box(setSliderColor("#56B4E9", 1),
              pickerInput(
                inputId = "RapidRRHProviderList",
                choices = c(unique(
                  sort(RRHEnterers$ProjectName))),
                options = list(`live-search` = TRUE),
                width = "70%"
              ),
              sliderTextInput("RapidRRHDateSlider",
                              "",
                              c(
                                unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                              ),
                              selected = Sys.yearqtr() - 1 / 4))),
              fluidRow(infoBoxOutput("daysToHouseSummary"), width = 3),
              fluidRow(box(DT::dataTableOutput("daysToHouseRRH")))),
      tabItem(tabName = "spendingTab",
              fluidRow(box(htmlOutput("headerRRHSpending"), width = 12)),
              fluidRow(box(setSliderColor("#56B4E9", 1),
                           pickerInput(
                             inputId = "RRHSpendingOrganizationList",
                             label = "Select Organization",
                             choices = c(unique(
                               sort(QPR_RRH_HP_Spending$OrganizationName))),
                             options = list(`live-search` = TRUE),
                             width = "70%"
                           ),
                           sliderTextInput("RRHSpendingDateSlider",
                                           "",
                                           c(
                                             unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                           ),
                                           selected = Sys.yearqtr() - 1 / 4))),
              # fluidRow(infoBoxOutput("notCreatedYet"), width = 3),
              fluidRow(box(DT::dataTableOutput("RRHSpending"),
                           title = "Rapid Rehousing Spending"),
                       box(DT::dataTableOutput("HPSpending"),
                           title = "Prevention Spending")))

    )
  )
)

