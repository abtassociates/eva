dashboardPage(
  title = "Eva",
  skin = "black",
  dashboardHeader(
    title = span(img(src = "Eva_logo_horizontal_white.png",
                     alt = "Eva logo",
                                   height = 45))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("System Overview",
               tabName = "tabSystemOverview")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "tabSystemOverview",
        fluidRow(box(
            fileInput("imported",
                      label = NULL,
                      multiple = FALSE,
                      accept = ".zip"),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Filters",
            width = 12,
            id = "syso_filters",
            fluidRow(
              id="universe_filters",
              column(
                2,
                pickerInput(
                  label = "Household Type",
                  inputId = "syso_hh_type",
                  choices = syso_hh_types,
                  selected = syso_hh_types[1]
                )
              ),
              column(
                2,
                pickerInput(
                  label = "Level of Detail",
                  inputId = "syso_level_of_detail",
                  choices = syso_level_of_detail,
                  selected = syso_level_of_detail[1]
                )
              ),
              column(
                2,
                pickerInput(
                  label = "Project Type Group",
                  inputId = "syso_project_type",
                  choices = syso_project_types,
                  selected = syso_project_types[1]
                )
              ),
              column(
                6,
                pickerInput(
                  label = "Gender and Race/Ethnicity Methodology Type",
                  inputId = "methodology_type",
                  multiple = FALSE,
                  selected = syso_methodology_types[1],
                  choices = syso_methodology_types,
                  width = "100%"
                )
              )
            ),
            fluidRow(
              id="syso_inflowoutflow_filters",
              column(
                3,
                pickerInput(
                  inputId = "syso_age",
                  label = "Age",
                  selected = syso_age_cats,
                  choices = syso_age_cats,
                  multiple = TRUE,
                  width = "100%",
                  options = pickerOptions(
                    actionsBox = TRUE,
                    selectedTextFormat = paste("count >", length(syso_age_cats)-1),
                    countSelectedText = "All Ages",
                    noneSelectedText = "All Ages" 
                  )
                )
              ),
              column(
                3,
                pickerInput(
                  label = "Veteran Status",
                  inputId = "syso_spec_pops",
                  choices = syso_spec_pops_people,
                  width = "100%",
                  selected = syso_spec_pops_people[1]
                )
              ),
              column(
                2,
                pickerInput(
                  label = "Gender",
                  inputId = "syso_gender",
                  choices = syso_gender_excl,
                  width = "100%",
                  selected = "All",
                  options = pickerOptions(actionsBox = TRUE)
                )
              ) %>% tagAppendAttributes(class="light-left-border"),
              column(
                4,
                pickerInput(
                  label = "Race/Ethnicity",
                  inputId = "syso_race_ethnicity",
                  choices = syso_race_ethnicity_excl,
                  width = "100%",
                  selected = syso_race_ethnicity_excl,
                  options = list(
                    `dropdown-align-right` = TRUE, 
                    `dropup-auto` = FALSE)
                )
              )
            )
          )
        ),
        fluidRow(
          uiOutput("sys_act_summary_filter_selections") %>% withSpinner(),
          plotOutput("sys_act_summary_ui_chart", width="70%", height="500px") %>% withSpinner()
        )
      )
    )
  )
)