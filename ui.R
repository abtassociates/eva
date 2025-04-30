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
   
   header = tagList(
     input_switch(id = 'in_demo_mode', 
                  label = tooltip(trigger = list('DEMO MODE', bs_icon('info-circle')), 
                            HTML('<strong>Off</strong>: Upload your own HMIS CSV Export.<br><br><strong>On</strong>: Uses a demo HMIS CSV Export.')
                          ), 
                  value=FALSE),
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
      ),
    ), 
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
            card(
              title = "Client Counts Summary",
              status = "info",
              solidHeader = TRUE,
              DTOutput("clientCountSummary"),
              width = 12
            ),
            card(
              title = "Client Counts Detail",
              status = "info",
              solidHeader = TRUE,
              DTOutput("clientCountData"),
              width = 12
            )
          )
        ),
      
        
       
      ),
  
    nav_menu(
      title = "Assess Data Quality",
      icon = icon("square-check"),
      nav_panel(
        title = "Check Project Data",
        value = "tabPDDE",
        card(
          card_header(htmlOutput("headerPDDE")),
          card_body(
            accordion(
              id = 'accordion_pdde',
              accordion_panel(
                title = "Instructions",
                tabPDDE_instructions
              )
            ),  
           
            card(
              id = "PDDESummaryOrganization",
              title = paste("PDDE Check Summary"),
              status = "info",
              solidHeader = TRUE,
              DTOutput("pdde_summary_table"),
              width = 12,
              br(),
              uiOutput("downloadPDDEReportButton") %>% withSpinner()
            ),
            card(id = "PDDEGuidance",
                DTOutput("pdde_guidance_summary"),
                title = "Guidance",
                width = 12,
                status = "info",
                solidHeader = TRUE)
            
            )
        )
      ),
      nav_panel(
        title = "System-level",
        value = "tabDQSystem",
        
        card(
          card_header(htmlOutput("headerSystemDQ")),
          card_body(
            uiOutput("downloadSystemDQReportButton"),
            accordion(
              id = 'accordion_dqsystem',
              open = FALSE,
              accordion_panel(
                title = 'Instructions',
                tabDQSystem_instructions
              )
            ),
            
            
            navset_card_tab(
              id = 'tabDQSystem_subtabs',
              
              nav_panel(
                id = 'hp_errors_dqsystem',
                title = 'High Priority Errors',
                
                navset_tab(
                  selected = "Issues",
                  nav_panel(
                    title = 'Top 10 Organizations',
                    uiOutput("systemDQHighPriorityErrorsByOrg_ui"),
                  ),
                  nav_panel(
                    title = 'Issues',
                    uiOutput("systemDQHighPriorityErrorsByIssue_ui"),
                  )
                ),
                
                
              ),
              nav_panel(
                id = 'g_errors_dqsystem',
                title = 'General Errors',
                
                navset_tab(
                  selected = "Top 10 Issues",
                  nav_panel(
                    title = 'Top 10 Organizations',
                    uiOutput("systemDQErrorsByOrg_ui"),
                  ),
                  nav_panel(
                    title = 'Top 10 Issues',
                    uiOutput("systemDQErrorsByIssue_ui"),
                  )
                ),
                
              ),
              nav_panel(
                id = 'warnings_dqsystem',
                title = 'Warnings',
                navset_tab(
                  selected = "Top 10 Issues",
                  nav_panel(
                    title = "Top 10 Organizations", 
                    uiOutput("systemDQWarningsByOrg_ui")
                  ),
                  nav_panel(
                    title = "Top 10 Issues", 
                    uiOutput("systemDQWarningsByIssue_ui")
                  )
                )
              )
             
              )
            )
          )
      ),
      nav_panel(
        title = "Organization-level",
        value = "tabDQOrg",
        
        card(
          card_header(htmlOutput("headerDataQuality")),
          card_body(
            accordion(
              id = 'accordion_dqorg',
              open = FALSE,
              accordion_panel(
                title = "Instructions",
                tabDQOrg_instructions
              )
            ), 
            card(
              pickerInput(
                label = "Select Organization",
                inputId = "orgList",
                choices = NULL,
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains'),
                width = "100%",
                selected = "none"
              ),
              uiOutput("downloadOrgDQReportButton")#,
              #width = 12
            ),
            
            navset_card_tab(
              id = 'tabDQOrg_subtabs',
              
              nav_panel(
                id = 'hp_errors',
                title = 'High Priority Errors',
               
                navset_tab(
                  selected = "Issues",
                  nav_panel(
                    title = 'Top 10 Projects',
                    uiOutput("orgDQHighPriorityErrorsByProject_ui")  %>% withSpinner(),
                  ),
                  nav_panel(
                    title = 'Issues',
                    uiOutput("orgDQHighPriorityErrorByIssue_ui") %>% withSpinner(),
                  )
                ),
                
                
              ),
              nav_panel(
                id = 'g_errors',
                title = 'General Errors',
                
               navset_tab(
                 selected = "Top 10 Issues",
                 nav_panel(
                   title = 'Top 10 Projects',
                   uiOutput("orgDQErrorsByProject_ui")  %>% withSpinner(),
                 ),
                 nav_panel(
                   title = 'Top 10 Issues',
                   uiOutput("orgDQErrorByIssue_ui") %>% withSpinner(),
                 )
               ),
               
              ),
              nav_panel(
                id = 'warnings',
                title = 'Warnings',
                navset_tab(
                  selected = "Top 10 Issues",
                  nav_panel(
                    title = "Top 10 Projects", 
                    uiOutput("orgDQWarningsByProject_ui") %>% withSpinner()
                  ),
                  nav_panel(
                    title = "Top 10 Issues", 
                    uiOutput("orgDQWarningsByIssue_ui") %>% withSpinner()
                  )
                )
              )
             
            ),
            
            card(
              id = "DQSummaryOrganization",
              title = paste("Data Quality Summary"),
              status = "info",
              solidHeader = TRUE,
              DTOutput("dq_organization_summary_table"),
              #width = 12
            ),
            card(
              id = "DQSummaryProvider",
              DTOutput("dq_org_guidance_summary"),
              title = "Data Quality Guidance",
              #width = 12,
              status = "info",
              solidHeader = TRUE
            )
          )
        )
      )
    ),
  
 
  
      nav_panel(
        title = "System Performance Overview",
        value = "tabSystemOverview",
        icon = icon("chart-simple"),
        
        card(
          card_header(
            htmlOutput("headerSystemOverview")
          ),
          card_body(
            accordion(
              id = 'accordion_systemoverview',
              open = FALSE,
              accordion_panel(
                title = 'Instructions',
                tabSystemOverview_instructions
              )
            ),
            
            card(
              card_header('Filters'),
              layout_column_wrap(
                width = 1/2,
                card(
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
                  )
                ),
                card(
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
              )
            ),
            navset_card_tab(
              id = 'syso_tabbox',
              
              nav_panel(
                id = 'syso_inflowoutflow',
                title = 'System Flow',
                
                navset_tab(
                  id = "sys_inflow_outflow_subtabs",
                  selected = "Summary Chart",
                  nav_panel(
                    title = 'Summary Chart',
                    uiOutput("sys_act_summary_filter_selections") %>%
                      withSpinner(),
                    plotOutput("sys_act_summary_ui_chart",
                               width = "70%",
                               height = "500") %>%
                      withSpinner()
                  ),
                  nav_panel(
                    title = 'Detail Chart',
                    uiOutput("sys_act_detail_filter_selections") %>%
                      withSpinner(),
                    plotOutput("sys_act_detail_ui_chart",
                               width = "100%",
                               height = "500") %>%
                      withSpinner()
                  ),
                  nav_panel(
                    title = "Information",
                    tab_sys_inflow_outflow_subtabs_information
                  )
                ),
                downloadButton("sys_inflow_outflow_download_btn", "Data Download"),
                downloadButton("sys_inflow_outflow_download_btn_ppt", "Image Download")
              ),
              
              nav_panel(
                id = 'syso_systemstatus',
                title = "Client System Status",
                navset_tab(
                  id = 'sys_status_subtabs',
                  selected = "Chart",
                  
                  nav_panel(
                    title = "Chart",   
                    uiOutput("sankey_filter_selections") %>% withSpinner(),
                    plotOutput("sankey_ui_chart", width="70%") %>% withSpinner()
                  ),
                  nav_panel(
                    title = "Information",
                    tab_sys_status_subtabs_information
                  )
                ),
                downloadButton("sys_status_download_btn", "Data Download"),
                downloadButton("sys_status_download_btn_ppt", "Image Download"),
              ),
              
              nav_panel(
                id = 'syso_composition',
                title = "System Demographics",
                
                navset_tab(
                  id = 'sys_comp_subtabs',
                  selected = "Chart",
                  nav_panel(
                    title = "Chart",
                    card(
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
                    ),
                    uiOutput("sys_comp_summary_selections"),
                    plotOutput("sys_comp_summary_ui_chart") %>% withSpinner()
                  ),
                  nav_panel(
                    title = "Information",
                    tab_sys_comp_subtabs_information
                    
                  ),
                  downloadButton("sys_comp_download_btn", "Data Download"),
                  downloadButton("sys_comp_download_btn_ppt", "Image Download"),
                )
              )
              
            ),
            downloadButton("client_level_download_btn", "Client Level Download")
            
            ) 
          ) 
            
        ),
        
      nav_panel(
        title = "System Exits",
        value = "systemExitDetail",
        card(
          card_header(htmlOutput("headerSystemExit")),
          card_body(
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
          tabGlossary_instructions,
          
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
            tabChangelog_instructions,
            dataTableOutput("changelog")
          )
        )
      ),
    )

