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
  dashboardHeader(title = "R minor _elevated_"),
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
      # menuItem("Data Quality",
      #          tabName = "dqTab"),
      # menuItem("CoC Competition",
      #          tabName = "cocCompetitionTab"),
      menuItem(
        "Performance and Outcomes",
        menuSubItem("Bed and Unit Utilization",
                    tabName = "utilizationTab"),
        menuItem("Community Need",
                    tabName = "spdatTab",
                 menuSubItem("PSH/RRH Detail",
                             tabName = "spdatTab1"),
                 menuSubItem("County Detail",
                             tabName = "spdatTab2")),
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
        menuSubItem("Recurrence",
                    tabName = "recurrenceTab"),
        menuSubItem("Rapid Placement for RRH",
                    tabName = "rapidTab"),
        menuSubItem("RRH HP Spending",
                    tabName = "spendingTab")
      )
    ),
    HTML(paste0(
      "<br>&emsp;Last update:&emsp;",
      format(updatedate, "%m-%d-%Y %I:%M %p", tz = "US/Eastern")#,
      #      "<br>&emsp;Happy Passover and Easter and Spring Equinox!"
    )),
    actionButton(inputId = "logOutButton", 
                 label = "Log Out",
                 onclick = 
                   "window.open('https://ohiobalanceofstatecoc.shinyapps.io/Rminor_elevated/__logout__/')")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "homeTab",
              htmlOutput("headerHome"), width = 9),
      # tabItem(tabName = "prioritizationListTab"),
      # tabItem(tabName = "contactTab"),
      # tabItem(tabName = "vetActiveListTab"),
      # tabItem(tabName = "dqTab"),
      # tabItem(tabName = "cocCompetitionTab"),
      tabItem(
        tabName = "utilizationTab",
        box(htmlOutput("headerUtilization"), width = 12),
        pickerInput(
          inputId = "providerListUtilization",
          choices = c(sort(BedUtilization$ProjectName)),
          options = list(`live-search` = TRUE),
          width = "100%"
        ),
       airDatepickerInput(inputId = "utilizationDate",
                  label = "Click to Choose a Month",
                  max = floor_date(today(), unit = "month") - days(1),
                  dateFormat = "MM yyyy",
                  view = "month",
                  value = floor_date(today(), unit = "month") - days(1),
                  minView = "months",
                  addon = "none"
        ),
      fluidRow(infoBoxOutput("utilizationSummary0",
                      width = 6),
               infoBoxOutput("utilizationSummary1",
                             width = 6)),
      fluidRow(infoBoxOutput("utilizationSummary2",
                             width = 12)),
        dataTableOutput("utilizationDetail")),      
      tabItem(
        tabName = "spdatTab1",
        box(htmlOutput("headerCommunityNeedPH"), width = 12),
        pickerInput(
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
                        selected = Sys.yearqtr() - 1 / 4),
        dataTableOutput("SPDATScoresHoused")
      ),
      tabItem(
        tabName = "spdatTab2",
        box(htmlOutput("headerCommunityNeedCounty"), width = 12),
        pickerInput(
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
                        selected = Sys.yearqtr() - 1 / 4),
        dataTableOutput("SPDATScoresServedInCounty")
      ),      
      tabItem(tabName = "LoSTab",
              box(htmlOutput("headerLoS"), width = 12),
              fluidRow(pickerInput(
                inputId = "LoSProjectList",
                choices = c(unique(
                  QPR_EEs$ProjectName[QPR_EEs$ProjectType %in% c(1, 2, 8, 13)])),
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
                              selected = Sys.yearqtr() - 1 / 4)),
              dataTableOutput("LoSDetail")
              ),
      tabItem(tabName = "PHTab",
              fluidRow(box(htmlOutput("headerExitsToPH"), width = 12)),
              pickerInput(
                inputId = "ExitsToPHProjectList",
                choices = c(unique(
                  QPR_EEs$ProjectName[QPR_EEs$ProjectType %in% c(1:4, 8:9, 12:13)])),
                options = list(`live-search` = TRUE),
                width = "70%"
              ),
              setSliderColor("#56B4E9", 1),
              sliderTextInput("ExitsToPHSlider",
                              "",
                              c(
                                unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                              ),
                              selected = Sys.yearqtr() - 1 / 4),
              dataTableOutput("ExitsToPH"),
              br(),
              br(),
              dataTableOutput("ExitsToPHOutreach")),

      tabItem(tabName = "NCBTab",
              HTML("<h1>Under Construction</h1>")),
      tabItem(tabName = "HITab",
              HTML("<h1>Under Construction</h1>")),
      tabItem(tabName = "incomeTab",
              HTML("<h1>Under Construction</h1>")),
      tabItem(tabName = "recurrenceTab",
              HTML("<h1>Under Construction</h1>")),
      tabItem(tabName = "rapidTab",
              fluidRow(box(htmlOutput("headerDaysToHouse"), width = 12)),
              setSliderColor("#56B4E9", 1),
              sliderTextInput("RapidRRHDateSlider",
                              "",
                              c(
                                unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                              ),
                              selected = Sys.yearqtr() - 1 / 4),
              pickerInput(
                inputId = "RapidRRHProviderList",
                choices = c(unique(
                  QPR_EEs$ProjectName[QPR_EEs$ProjectType == 13])),
                options = list(`live-search` = TRUE),
                width = "70%"
              ),
              fluidRow(infoBoxOutput("daysToHouseSummary"), width = 3),
              dataTableOutput("daysToHouseRRH")),
      tabItem(tabName = "spendingTab",
              HTML("<h1>Under Construction</h1>"))

    )
  )
)

