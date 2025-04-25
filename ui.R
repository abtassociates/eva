page_navbar(
  id = 'pageid',
  window_title = 'Eva',
  theme = bs_theme(
    version = 5,
    brand = TRUE,
    navbar_bg = "#16697A" # dark_blue
  ),
  title = span(img(src = "Eva_logo_horizontal_white.png",
                 alt = "Eva logo",
                               height = 45)),
    # https://alvarotrigo.com/blog/toggle-switch-css/
    tags$li(class="dropdown",
            HTML('<div class="toggle-button-cover">
      <div class="button-cover">
        <div id="demo-label">
          <span>DEMO MODE </span>
          <i class="fas fa-circle-info demo-tooltip">
          <span class="demo-tooltiptext">
            <strong>Off</strong>: Upload your own HMIS CSV Export.
            <br>
            <br>
            <strong>On</strong>: Uses a demo HMIS CSV Export.
          </span>
          </i>
        </div>
        <div class="button r" id="button-1">
          <input id="isdemo" type="checkbox" class="checkbox" />
          <div class="knobs"></div>
          <div class="layer"></div>
        </div>
      </div>
    </div>
    <script>
    document.getElementById("isdemo").onchange = function() {
        Shiny.setInputValue("in_demo_mode", this.checked, {priority: "event"});
    }
    </script>')),
   header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$html(lang="en"), #Added as WAVE fix but not considered ideal
      tags$script(src = "idle_timeout.js"), # Reference the JS file
      tags$script(HTML(
        "var dimension = [0, 0];
        $(document).on('shiny:connected', function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange('dimension', dimension);
        });
        $(window).resize(function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange('dimension', dimension);
        });
        "
      ))
    ),
    useShinyjs(),
    disconnectMessage(
      text = str_squish(
        "Eva has crashed. Please submit an issue on GitHub and note the
          date and time in order to help the team diagnose the issue."
      ),
      overlayColour = '#F5F5F5',
      refresh = ""
    )), 
    nav_panel(
        title = "Home",
        value = "tabHome", 
        icon = icon("home"),
          card(
            tabHome_welcome,
            actionButton("Go_to_upload", "Click here to get started"),
            fill = FALSE
          ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Instructions",
            value = "home_live_instructions",
            tabHome_home_live_instructions
          ),
          accordion_panel(
            title = "Demo Instructions",
            value = 'home_demo_instructions',
            tabHome_home_demo_instructions
          ),
          accordion_panel(
            title = "Need help?",
            tabHome_need_help
          ), 
          accordion_panel(
            title = "Citations and Special Thanks",
            tabHome_citations
          )
        )
      ), 
      nav_panel(
        title = "Upload HMIS CSV Export",
        value = "tabUpload",
        icon = icon("upload"),
        card(
          card_header(htmlOutput("headerUpload")),
          card_body(
            accordion(
              id = 'accordion_upload',
              open = FALSE,
              accordion_panel(
                  title = "Instructions",
                  tabUpload_instructions
              )
            )
          )
        ),
         
          card(
            tabUpload_in_demo_mode,
            
            fileInput("imported",
                      label = NULL,
                      multiple = FALSE,
                      accept = ".zip"),
            uiOutput("fileInfo") %>% withSpinner()
          ),
          card(
            title = "HMIS CSV Export File Structure Analysis",

            DTOutput("fileStructureAnalysis"),
            p(),
            HTML("<p>Users should contact their vendor to resolve high priority 
            errors identified in the HMIS CSV Export File Structure Analysis, as
            well as any other structural issues which you feel need to be corrected.
            </p>"),
            p(),
            uiOutput('downloadFileStructureAnalysisBtn'),
            p(),
            uiOutput('downloadImpermissibleCharacterDetailBtn')
          )
          # fluidRow(box(
          #   title = "System Data Quality Overview",
          #   width = 12,
          #   column(12, plotOutput("validate_plot")),
          #   column(6, plotOutput("dq_overview_plot")),
          #   column(6, plotOutput("dq_orgs_overview_plot"))
          # )
        # )
      ),
      nav_panel(
        title = "Edit Local Settings",
        value = "tabLocalSettings",
        icon = icon("gear"),
        card(
          card_header(htmlOutput("headerLocalSettings")),
          card_body(
            
          )
        ),
        accordion(
          id = 'accordion_local_settings',
          open = FALSE,
          accordion_panel(
            title = "Instructions",
            tabLocalSettings_instructions
          ),
          accordion_panel(
            title = "Outstanding Referrals",
            
              tabLocalSettings_outstanding_referrals,
              numericInput(
                inputId = "CEOutstandingReferrals",
                label = "Max Days:",
                value = 14,
                min = 0,
                max = 365,
                step = 7,
                width = "200px"
              )
            ),
          accordion_panel(
            title = "Long Stayers",
            
            tabLocalSettings_long_stayers,
            
           layout_column_wrap(
                width = 1/2,
                card(
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
                    inputId = "OtherLongStayers",
                    label = "Other:",
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
                  )
                ),
                card(
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
                    inputId = "ServicesOnlyLongStayers",
                    label = "Services Only:",
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
                  )
                )
            )
          )
        )
      ),
      nav_panel(
        title = "View Client Counts",
        value = "tabClientCount",
        icon = icon("people-group"),
        card(
          card_header(
            htmlOutput("headerClientCounts")
          ),
          card_body(
            accordion(
              id = 'accordion_client_count',
              accordion_panel(
                title = "Instructions",
                tabClientCount_instructions,
              )
            ),
            
            box(
              dateRangeInput(
                "dateRangeCount",
                "Date Range",
                format = "mm/dd/yyyy",
                start = if_else(isTRUE(getOption("shiny.testmode")), ymd("20231005"), ymd(today())),
                end = if_else(isTRUE(getOption("shiny.testmode")), ymd("20231005"), ymd(today())),
                width = 300
              ),
              uiOutput("downloadClientCountsReportButton"),
             ),
            
            box(
              pickerInput(
                label = "",
                inputId = "currentProviderList",
                choices = NULL,
                width = "600px",
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains')
              ),
              width = 12,
              title = "Select Project",
              status = "info",
              solidHeader = TRUE
            ),
            box(
              title = "Client Counts Summary",
              status = "info",
              solidHeader = TRUE,
              DTOutput("clientCountSummary"),
              width = 12
            ),
            box(
              title = "Client Counts Detail",
              status = "info",
              solidHeader = TRUE,
              DTOutput("clientCountData"),
              width = 12
            )
          )
        ),
      
        
       
      ),
  
      tabItem(
        tabName = "tabPDDE",
        fluidRow(box(htmlOutput(
          "headerPDDE"
        ), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("
               <h4>Project Descriptor Data Element (PDDE) Check Summary</h4>
               <p>Once you have successfully uploaded an HMIS CSV Export, you
               will find a summary of each issue that was flagged in your data
               regarding your PDDEs. Please download the details by clicking the
               \'Download\' button.</p>
               
               <h4>Guidance</h4>
               <p>For a description of each issue found, check the Guidance 
               panel.</p>")
        )),
        fluidRow(
          box(
            id = "PDDESummaryOrganization",
            title = paste("PDDE Check Summary"),
            status = "info",
            solidHeader = TRUE,
            DTOutput("pdde_summary_table"),
            width = 12,
            br(),
            uiOutput("downloadPDDEReportButton") %>% withSpinner()
          ),
          box(id = "PDDEGuidance",
              DTOutput("pdde_guidance_summary"),
              title = "Guidance",
              width = 12,
              status = "info",
              solidHeader = TRUE)
        )
      ),
      tabItem(
        tabName = "tabDQOrg",
        fluidRow(box(htmlOutput("headerDataQuality"), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
        )), 
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
          uiOutput("downloadOrgDQReportButton"),
          width = 12
        )), 
        fluidRow(
          tabBox(
            side = "right",
            selected = "Issues",
            title = "High Priority Errors",
            tabPanel("Top 10 Projects",
                     uiOutput("orgDQHighPriorityErrorsByProject_ui")  %>% withSpinner()),
            tabPanel("Issues",
                     uiOutput("orgDQHighPriorityErrorByIssue_ui") %>% withSpinner()),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "General Errors",
            tabPanel("Top 10 Projects",
                     uiOutput("orgDQErrorsByProject_ui") %>% withSpinner()),
            tabPanel("Top 10 Issues",
                     uiOutput("orgDQErrorByIssue_ui") %>% withSpinner()),
            width =12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "Warnings",
            tabPanel("Top 10 Projects", uiOutput("orgDQWarningsByProject_ui") %>% withSpinner()),
            tabPanel("Top 10 Issues", uiOutput("orgDQWarningsByIssue_ui") %>% withSpinner()),
            width = 12
          )
        ),
        
        fluidRow(
          box(
            id = "DQSummaryOrganization",
            title = paste("Data Quality Summary"),
            status = "info",
            solidHeader = TRUE,
            DTOutput("dq_organization_summary_table"),
            width = 12
          )
        ),
        
        fluidRow(
          box(
            id = "DQSummaryProvider",
            DTOutput("dq_org_guidance_summary"),
            title = "Data Quality Guidance",
            width = 12,
            status = "info",
            solidHeader = TRUE
          )
        )
      ),
      tabItem(
        tabName = "tabSystemOverview",
        fluidRow(box(htmlOutput("headerSystemOverview"), width = 12)),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          tabSystemOverview_instructions
        )),
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
                ),
              ),
              column(
                2,
                pickerInput(
                  label = "Project Type Group",
                  inputId = "syso_project_type",
                  choices = syso_project_types,
                  selected = syso_project_types[1]
                ),
              ),
              column(
                6,
                pickerInput(
                  label = "Race/Ethnicity Methodology Type",
                  inputId = "methodology_type",
                  multiple = FALSE,
                  selected = syso_methodology_types[1],
                  choices = syso_methodology_types,
                  width = "100%"
                ),
              ) %>% tagAppendAttributes(class="light-left-border"),
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
                6,
                pickerInput(
                  label = "Race/Ethnicity",
                  inputId = "syso_race_ethnicity",
                  choices = syso_race_ethnicity_method1,
                  width = "100%",
                  selected = syso_race_ethnicity_method1,
                  options = list(
                    `dropdown-align-right` = TRUE, 
                    `dropup-auto` = FALSE)
                )
              ) %>% tagAppendAttributes(class="light-left-border")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            tabBox(
              width = 12,
              id = "syso_tabbox",
              type = "tabs",
              tabPanel(
                id = "syso_inflowoutflow", 
                title = "System Flow",
                tabBox(
                  width = 12,
                  id = "sys_inflow_outflow_subtabs",
                  selected = "Summary Chart",
                  tabPanel("Summary Chart", 
                           uiOutput("sys_act_summary_filter_selections") %>%
                             withSpinner(),
                           plotOutput("sys_act_summary_ui_chart",
                                      width = "70%",
                                      height = "500") %>%
                             withSpinner()
                  ),
                  tabPanel("Detail Chart", 
                           uiOutput("sys_act_detail_filter_selections") %>%
                             withSpinner(),
                           plotOutput("sys_act_detail_ui_chart",
                                      width = "100%",
                                      height = "500") %>%
                             withSpinner()
                  ),
                  tabPanel("Information", 
                    tab_sys_inflow_outflow_subtabs_information
                  )
                ),
                downloadButton("sys_inflow_outflow_download_btn", "Data Download"),
                downloadButton("sys_inflow_outflow_download_btn_ppt", "Image Download")
              ),
              tabPanel(
                id = "syso_systemstatus",
                side = "left",
                selected = "Chart",
                title = "Client System Status",
                tabBox(
                  width = 12,
                  id = "sys_status_subtabs",
                  tabPanel("Chart", 
                           uiOutput("sankey_filter_selections") %>% withSpinner(),
                           plotOutput("sankey_ui_chart", width="70%") %>% withSpinner()
                  ),
                  tabPanel("Information", 
                        tab_sys_status_subtabs_information
                  )
                ),
                downloadButton("sys_status_download_btn", "Data Download"),
                downloadButton("sys_status_download_btn_ppt", "Image Download"),
                width = 12
              ),
              tabPanel(
                id = "syso_composition",
                side = "left",
                selected = "Chart",
                title = "System Demographics",
                tabBox(
                  width = 12,
                  id = "sys_comp_subtabs",
                  tabPanel("Chart", 
                           fluidRow(
                             box(
                               strong("Select Demographic Crosstab Categories (up to 2)"),
                               p(str_glue(
                                 "For a simple count of totals within a demographic 
                                 category, select only one category. To see the 
                                 intersection of two demographic categories, select 
                                 both categories to create a crosstab chart. To 
                                 change your crosstab selection, uncheck at least 
                                 one of your previous selections before selecting 
                                 new categories. Note that you can only select one Race/Ethnicity 
                                 category to display in the chart at a time."
                               )),
                               checkboxGroupInput(
                                 "system_composition_selections",
                                 label = "",
                                 choices = sys_comp_selection_choices,
                                 selected = c("All Races/Ethnicities", "Age"),
                                 inline = TRUE
                               ),
                               width = 12
                             )
                          ),
                          uiOutput("sys_comp_summary_selections"),
                          plotOutput("sys_comp_summary_ui_chart") %>% withSpinner()
                  ),
                  tabPanel("Information", 
                         tab_sys_comp_subtabs_information
                  )
                ),  # end syso_comp_subtab tabbox
                downloadButton("sys_comp_download_btn", "Data Download"),
                downloadButton("sys_comp_download_btn_ppt", "Image Download"),
                width = 12
              ) #end syso_comp tabPanel
            ), # end syso_tabbox
            downloadButton("client_level_download_btn", "Client Level Download")
          ) # end box
        ) #end fluid row
      ),
      tabItem(
        tabName = "systemExitDetail",
        fluidRow(box(htmlOutput("headerSystemExit"), width = 12)),
        fluidRow(
          box(
            width = 12,
            HTML("<h2>Placeholder</h2>")
          )
        )
      ),
      nav_panel(
        title = "Glossary",
        value = "tabGlossary",
        icon = icon("book"),
        card(
            card_header(HTML("<h2>Glossary</h2>")),
            card_body(
#          title = "Instructions",
        
          HTML("
               <p>This glossary provides definitions for the terms used throughout 
              Eva's System Performance Overview page. You can review definitions 
              of the terms by their focus, including:</p>
              
              <ul>
                <li>System Performance Filters</li>
                <li>System Flow Chart</li>
                <li>Client System Status Chart</li>
              </ul>
              
              <p>You can also search for a specific term using the search bar.</p>"),
          
            DTOutput("glossary")
            )
          )
      ),
      nav_panel(
        title = "View Changelog",
        value = "tabChangelog",
        icon = icon("clipboard"),
        card(
          card_header(HTML("<h2>Changelog</h2>")),
          card_body(
            HTML("
              <p>This tab will list the most recent technical updates and
              changes to Eva. For more in-depth information on current and past
              issues, please go to <a
              href='https://github.com/abtassociates/eva/issues' target= '_blank'
              rel='noopener noreferrer'>GitHub</a>.</p>
            "),
            dataTableOutput("changelog")
          )
        )
      ),
      tabItem(
        tabName = "tabDQSystem",
        fluidRow(box(
          htmlOutput("headerSystemDQ"), width = 12, 
          uiOutput("downloadSystemDQReportButton"))),
        fluidRow(box(
          title = "Instructions",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          tabDQSystem_instructions
        )), 

        fluidRow(
          tabBox(
            side = "right",
            selected = "Issues",
            title = "High Priority Errors",
            tabPanel("Top 10 Organizations",
                     uiOutput("systemDQHighPriorityErrorsByOrg_ui")),
            tabPanel("Issues",
                     uiOutput("systemDQHighPriorityErrorsByIssue_ui")),
            width = 12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "General Errors",
            tabPanel("Top 10 Organizations", uiOutput("systemDQErrorsByOrg_ui")),
            tabPanel("Top 10 Issues", uiOutput("systemDQErrorsByIssue_ui")),
            width =12
          )
        ),
        fluidRow(
          tabBox(
            side = "right",
            selected = "Top 10 Issues",
            title = "Warnings",
            tabPanel("Top 10 Organizations", uiOutput("systemDQWarningsByOrg_ui")),
            tabPanel("Top 10 Issues", uiOutput("systemDQWarningsByIssue_ui")),
            width = 12
          )
        )
      )
    )

