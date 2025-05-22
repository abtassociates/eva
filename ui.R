page_navbar(
  # options, theme, and title ----------------
  id = 'pageid',
  # navbar_options = navbar_options(
  #   bg = "#16697A"
  # ),
  window_title = 'Eva',
  fillable = FALSE,
  theme = bslib_eva_theme,
  
  title = span(
    img(src = "Eva_logo_horizontal_white.png", alt = "Eva logo", height = 45)
  ),
  # Header ------
  header = tagList(
    ## css, idle management, and dimension management --------
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
    ## Enable shinyjs -----
    useShinyjs(),
    ## Demo banner -----------------
    shinyjs::hidden(div(id = "demo_banner","DEMO")),
    # The switch itself - it controls the input value
    div(
      id = "demo_mode_switch_wrapper",
      input_switch(
        id = 'in_demo_mode',
        label = tooltip(
          id = "demo_mode_tooltip",
          trigger = list('DEMO MODE', bs_icon('info-circle')),
          HTML('
               <strong>Off</strong>: Upload your own HMIS CSV Export.<br><br>
               <strong>On</strong>: Uses a demo HMIS CSV Export.'
          )
        ),
        value=FALSE
      )
    ),
    ## Eva has crashed ------
    disconnectMessage(
      text = str_squish(
        "Eva has crashed. Please submit an issue on GitHub and note the
          date and time in order to help the team diagnose the issue."
      ),
      overlayColour = '#F5F5F5',
      refresh = ""
    ),
  ), 
  
  # Sidebar --------
  # sidebar = sidebar(open = 'closed',title = 'More Info',bg = 'white',fg='black',
  #                 
  #                   input_switch(id = 'in_demo_mode',
  #                                label = tooltip(trigger = list('DEMO MODE', bs_icon('info-circle')),
  #                                                HTML('<strong>Off</strong>: Upload your own HMIS CSV Export.<br><br><strong>On</strong>: Uses a demo HMIS CSV Export.')
  #                                ),
  #                                value=FALSE)
  # ),
  # Home tab ------------------
  nav_panel(
    title = "Home",
    value = "tabHome", 
    icon = icon("home"),
    card(
      card_body(
        tabHome_welcome,
        actionButton("Go_to_upload", "Click here to get started",class = 'btn-secondary'),
        fillable = FALSE
      )
    ),
    accordion(
      id = 'accordion_home',
      open = FALSE,
      
      shinyjs::hidden(accordion_panel(
        title = "Demo Instructions",
        value = "home_demo_instructions",
        tabHome_home_demo_instructions
      )),
      accordion_panel(
        title = "Instructions",
        value = "home_live_instructions",
        tabHome_home_live_instructions
      ),
      accordion_panel(
        title = "Need help?",
        tabHome_need_help,
        value = 'home_need_help'
      ),
      accordion_panel(
        title = "Citations and Special Thanks",
        tabHome_citations
      )
    )
  ), 
  # Upload CSV Export tab --------------
  nav_panel(
    title = "HMIS CSV Export",
    value = "tabUpload",
    icon = icon("upload"),
    card(htmlOutput("headerUpload")),
    
    accordion(
      id = 'accordion_upload',
      open = FALSE,
      accordion_panel(
        title = "Instructions",
        tabUpload_instructions
      )
    ),
    br(),
    card(
      tabUpload_in_demo_mode,
      
      fileInput("imported",
                label = NULL,
                multiple = FALSE,
                accept = ".zip"),
      uiOutput("fileInfo") %>% withSpinner()
    ),
    card(
      card_title("HMIS CSV Export File Structure Analysis"),
      
      DTOutput("fileStructureAnalysis"),
      p(),
      HTML("<p>Users should contact their vendor to resolve high priority 
            errors identified in the HMIS CSV Export File Structure Analysis, as
            well as any other structural issues which you feel need to be corrected.
            </p>"),
      p(),
      uiOutput('downloadFileStructureAnalysisBtn', fill = FALSE, inline = TRUE),
      p(),
      uiOutput('downloadImpermissibleCharacterDetailBtn', fill = FALSE),
      fillable = FALSE
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
  # Local settings tab ----------------
  nav_panel(
    title = "Local Settings",
    value = "tabLocalSettings",
    icon = icon("gear"),
    card(
        htmlOutput("headerLocalSettings")
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
  # Client counts tab ----------------
  nav_panel(
    title = "Client Counts",
    value = "tabClientCount",
    icon = icon("people-group"),
    card(
      card_title(
        htmlOutput("headerClientCounts")
      ),
    ),
    accordion(
      id = 'accordion_client_count',
      open = FALSE,
      accordion_panel(
        title = "Instructions",
        tabClientCount_instructions,
      )
    ),
    br(),
    card(
      card_header("Date Range"),
      dateRangeInput(
        "dateRangeCount",
        labe = NULL,
        format = "mm/dd/yyyy",
        start = if_else(isTRUE(getOption("shiny.testmode")), ymd("20231005"), ymd(today())),
        end = if_else(isTRUE(getOption("shiny.testmode")), ymd("20231005"), ymd(today())),
        width = 300
      ),
      br(),
      uiOutput("downloadClientCountsReportButton", inline = TRUE),
    ),
    
    card(
      card_header("Select Project"),
      pickerInput(
        label = NULL,
        inputId = "currentProviderList",
        choices = NULL,
        options = pickerOptions(liveSearch = TRUE,
                                liveSearchStyle = 'contains', 
                                container = 'body')
      )
    ),
    
    navset_card_underline(
      id = 'client_count_subtabs',
      
      nav_panel(
        title = "Client Counts Summary",
        DTOutput("clientCountSummary")
      ),
      nav_panel(
        title = "Client Counts Detail",
        DTOutput("clientCountData")
      )
    )
    # card(
    #   card_header("Client Counts Summary"),
    #   DTOutput("clientCountSummary"),
    #   width = 12
    # ),
    # card(
    #   card_header("Client Counts Detail"),
    #   DTOutput("clientCountData"),
    #   width = 12
    # )
  ),
  # Data Quality tab ----------------------
  nav_menu(
    title = "Data Quality",
    icon = icon("square-check"),
    ## PDDE -------------
    nav_panel(
      title = "Project Data",
      value = "tabPDDE",
      card(
        card_title(htmlOutput("headerPDDE"))#,
      ),
      accordion(
        id = 'accordion_pdde',
        open = FALSE,
        accordion_panel(
          title = "Instructions",
          tabPDDE_instructions
        )
      ),  
      br(),
    
      navset_card_underline(
        id = "pdde_subtabs",
        ### PDDE Summary table-------
        nav_panel(
          title = 'PDDE Check Summary',
          DTOutput("pdde_summary_table"),
          br(),
          uiOutput("downloadPDDEReportButton", inline = TRUE) %>% withSpinner()
        ),
        ### PDDE Guidance table ---------
        nav_panel(
          title = 'Guidance',
          DTOutput("pdde_guidance_summary")
        )
      )
    ),
    ## DQ System --------------
    nav_panel(
      title = "System-level",
      value = "tabDQSystem",
      
      card(
        htmlOutput("headerSystemDQ")
      ),
      accordion(
        id = 'accordion_dqsystem',
        open = FALSE,
        accordion_panel(
          title = 'Instructions',
          tabDQSystem_instructions
        )
      ),
      br(),
      
      navset_card_underline(
        id = 'tabDQSystem_subtabs',
        footer = uiOutput("downloadSystemDQReportButton", inline = TRUE),
        ### HP Errors - System ----
        nav_panel(
          id = 'hp_errors_dqsystem',
          title = 'High Priority Errors',
          
          navset_underline(
            selected = "Issues",
            nav_panel(
              title = 'Top 10 Organizations',
              uiOutput("systemDQHighPriorityErrorsByOrg_ui"),
            ),
            nav_panel(
              title = 'Issues',
              uiOutput("systemDQHighPriorityErrorsByIssue_ui"),
            )
          )
        ),
        ### General Errors - System -----
        nav_panel(
          id = 'g_errors_dqsystem',
          title = 'General Errors',
          
          navset_underline(
            selected = "Top 10 Issues",
            nav_panel(
              title = 'Top 10 Organizations',
              uiOutput("systemDQErrorsByOrg_ui"),
            ),
            nav_panel(
              title = 'Top 10 Issues',
              uiOutput("systemDQErrorsByIssue_ui"),
            )
          )
        ),
        ### Warnings - System -------
        nav_panel(
          id = 'warnings_dqsystem',
          title = 'Warnings',
          navset_underline(
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
    ),
    ## DQ Org -------------
    nav_panel(
      title = "Organization-level",
      value = "tabDQOrg",
      
      card(htmlOutput("headerDataQuality")),
      accordion(
        id = 'accordion_dqorg',
        open = FALSE,
        accordion_panel(
          title = "Instructions",
          tabDQOrg_instructions
        )
      ), 
      br(),
      card(
        card_header("Select Organization"),
        pickerInput(
          inputId = "orgList",
          choices = NULL,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = 'contains',
                                  container = 'body'),
          width = "100%",
          selected = "none"
        )
      ),
      
      navset_card_underline(
        id = 'tabDQOrg_subtabs',
        footer = uiOutput("downloadOrgDQReportButton", inline = TRUE),
        ### HP Errors - Org ------------
        nav_panel(
          id = 'hp_errors',
          title = 'High Priority Errors',
          
          navset_underline(
            selected = "Issues",
            nav_panel(
              title = 'Top 10 Projects',
              uiOutput("orgDQHighPriorityErrorsByProject_ui")  %>% withSpinner(),
            ),
            nav_panel(
              title = 'Issues',
              uiOutput("orgDQHighPriorityErrorByIssue_ui") %>% withSpinner(),
            )
          )
        ),
        ### General Errors - Org ---------------
        nav_panel(
          id = 'g_errors',
          title = 'General Errors',
          
          navset_underline(
            selected = "Top 10 Issues",
            nav_panel(
              title = 'Top 10 Projects',
              uiOutput("orgDQErrorsByProject_ui")  %>% withSpinner(),
            ),
            nav_panel(
              title = 'Top 10 Issues',
              uiOutput("orgDQErrorByIssue_ui") %>% withSpinner(),
            )
          )
        ),
        ### Warnings - Org --------------
        nav_panel(
          id = 'warnings',
          title = 'Warnings',
          navset_underline(
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
      ### DQ Summary and Guidance ------------
      navset_card_underline(
        id = 'dq_summary_subtabs',
        
        nav_panel(
          title = "Data Quality Summary",
          DTOutput("dq_organization_summary_table")
        ),
        nav_panel(
          title = "Data Quality Guidance",
          DTOutput("dq_org_guidance_summary")
        )
      )
      # card(
      #   id = "DQSummaryOrganization",
      #   card_header(paste("Data Quality Summary")),
      #   
      #   DTOutput("dq_organization_summary_table"),
      #   #width = 12
      # ),
      # card(
      #   id = "DQSummaryProvider",
      #   card_header("Data Quality Guidance"),
      #   DTOutput("dq_org_guidance_summary")
      #   
      # )
    )
),

# System Performance Overview tab -------------------
nav_panel(
  title = "System Performance Overview",
  value = "tabSystemOverview",
  icon = icon("chart-simple"),
  
  card(
    htmlOutput("headerSystemOverview")
  ),
  accordion(
    id = 'accordion_systemoverview',
    open = FALSE,
    accordion_panel(
      title = 'Instructions',
      tabSystemOverview_instructions
    )
  ),
  br(),
  ## Filters --------------
  card(
    card_header('Filters'),
    layout_columns(
      col_widths = c(2,2,2,6, 3,3,6),
      pickerInput(
        label = "Household Type",
        inputId = "syso_hh_type",
        choices = syso_hh_types,
        selected = syso_hh_types[1],
        options = pickerOptions(container = "body")
      ),
      pickerInput(
        label = "Level of Detail",
        inputId = "syso_level_of_detail",
        choices = syso_level_of_detail,
        selected = syso_level_of_detail[1],
        options = pickerOptions(container = "body")
      ),
      pickerInput(
        label = "Project Type Group",
        inputId = "syso_project_type",
        choices = syso_project_types,
        selected = syso_project_types[1],
        options = pickerOptions(container = "body")
      ),
      pickerInput(
        label = "Race/Ethnicity Methodology Type",
        inputId = "methodology_type",
        multiple = FALSE,
        selected = syso_methodology_types[1],
        choices = syso_methodology_types,
        options = pickerOptions(container = "body")
      ),
      pickerInput(
        inputId = "syso_age",
        label = "Age",
        selected = syso_age_cats,
        choices = syso_age_cats,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          selectedTextFormat = paste("count >", length(syso_age_cats)-1),
          countSelectedText = "All Ages",
          noneSelectedText = "All Ages",
          container = "body"
        )
      ),
      pickerInput(
        label = "Veteran Status",
        inputId = "syso_spec_pops",
        choices = syso_spec_pops_people,
        selected = syso_spec_pops_people[1],
        options = pickerOptions(container = "body")
      ),
      pickerInput(
        label = "Race/Ethnicity",
        inputId = "syso_race_ethnicity",
        choices = syso_race_ethnicity_method1,
        selected = syso_race_ethnicity_method1,
        options = list(
          `dropdown-align-right` = TRUE,
          `dropup-auto` = FALSE,
          container = "body"
        )
      )
    )
    
  ),
  
  navset_card_underline(
    id = 'syso_tabbox',
    
    ## System Flow -------------
    nav_panel(
      id = 'syso_inflowoutflow',
      title = 'System Flow',
      
      navset_underline(
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
          br(),
          tab_sys_inflow_outflow_subtabs_information
        )
      ),
      downloadButton("sys_inflow_outflow_download_btn", "Data Download"),
      downloadButton("sys_inflow_outflow_download_btn_ppt", "Image Download")
    ),
    
    ## System Status/Sankey ----------------
    nav_panel(
      id = 'syso_systemstatus',
      title = "Client System Status",
      navset_underline(
        id = 'sys_status_subtabs',
        selected = "Chart",
        
        nav_panel(
          title = "Chart",   
          uiOutput("sankey_filter_selections") %>% withSpinner(),
          plotOutput("sankey_ui_chart", width="70%") %>% withSpinner()
        ),
        nav_panel(
          title = "Information",
          br(),
          tab_sys_status_subtabs_information
        )
      ),
      downloadButton("sys_status_download_btn", "Data Download"),
      downloadButton("sys_status_download_btn_ppt", "Image Download"),
    ),
    
    ## System Demographics/Composition --------------
    nav_panel(
      id = 'syso_composition',
      title = "System Demographics",
      
      navset_underline(
        id = 'sys_comp_subtabs',
        selected = "Chart",
        nav_panel(
          title = "Chart",
          card(
            br(),
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
          br(),
          uiOutput("sys_comp_summary_selections"),
          plotOutput("sys_comp_summary_ui_chart") %>% withSpinner()
        ),
        nav_panel(
          title = "Information",
          br(),
          tab_sys_comp_subtabs_information
          
        ),
        downloadButton("sys_comp_download_btn", "Data Download"),
        downloadButton("sys_comp_download_btn_ppt", "Image Download"),
      )
    )
    
    ),
    downloadButton("client_level_download_btn", "Client Level Download")
  
  
  ),

# nav_panel(
#   title = "System Exits",
#   value = "systemExitDetail",
#   card(
#     card_header(htmlOutput("headerSystemExit")),
#     card_body(
#       HTML("<h2>Placeholder</h2>")
#     )
#   )
# ),
# Glossary tab -------------
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
  # Changelog tab --------------
  nav_panel(
    title = "Changelog",
    value = "tabChangelog",
    icon = icon("clipboard"),
    card(
      card_header(HTML("<h2>Changelog</h2>"),class = 'cardhdr'),
      card_body(
        tabChangelog_instructions,
        dataTableOutput("changelog")
      ), min_height = 1000, fill = FALSE
      
    )
  )
)
