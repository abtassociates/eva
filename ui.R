dashboardPage(
  skin = "black",
  dashboardHeader(title = "R minor _elevated_"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem(
        "Prioritization",
        menuSubItem("Prioritization List",
                    tabName = "prioritizationListTab"),
        menuSubItem("Contact",
                    tabName = "contactTab"),
        menuSubItem("Veteran Active List",
                    tabName = "vetActiveListTab")
      ),
      menuItem("Data Quality",
               tabName = "dqTab"),
      menuItem("CoC Competition",
               tabName = "cocCompetitionTab"),
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
    ))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "prioritizationListTab"),
      tabItem(tabName = "contactTab"),
      tabItem(tabName = "vetActiveListTab"),
      tabItem(tabName = "dqTab"),
      tabItem(tabName = "cocCompetitionTab"),
      tabItem(tabName = "LoSTab"),
      tabItem(tabName = "PHTab"),
      tabItem(tabName = "NCBTab"),
      tabItem(tabName = "HITab"),
      tabItem(tabName = "incomeTab"),
      tabItem(tabName = "recurrenceTab"),
      tabItem(tabName = "rapidTab"),
      tabItem(tabName = "spendingTab"),
      tabItem(
        tabName = "utilizationTab",
        pickerInput(
          inputId = "providerListUtilization",
          choices = c(sort(BedUtilization$ProjectName)),
          options = list(`live-search` = TRUE),
          width = "100%"
        ),
        setSliderColor("#56B4E9", 1),
        sliderTextInput(
          "utilizationSlider",
          label = "Select END DATE",
          choices = choices_month,
          selected = choices_month[24]
        ),
        dataTableOutput("utilizationDetail")
      ),
      tabItem(
        tabName = "spdatTab1",
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
      )
    )
  )
)
