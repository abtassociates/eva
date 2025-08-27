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
      tags$script(HTML(
        "function idleTimer() {
          var timeoutTime = 900000; //15 mins
          var t = setTimeout(showTimeoutDialog, timeoutTime);
          window.onmousemove = resetTimer;
          window.onmousedown = resetTimer;
          window.onclick = resetTimer;
          window.onscroll = resetTimer;
          window.onkeypress = resetTimer;
      
          function showTimeoutDialog() {
            document.querySelector('a[data-value=\"tabHome\"]').click();
            
            // If user clicks Cancel, disable all interactivity
            document.body.style.opacity = '0.5';
            document.body.style.pointerEvents = 'none';
            
            // Add a banner at the top of the page
            var banner = document.createElement('div');
            banner.style.position = 'fixed';
            banner.style.top = '40%';
            banner.style.left = '0';
            banner.style.width = '100%';
            banner.style.padding = '10px';
            banner.style.backgroundColor = 'lightgray';
            banner.style.color = 'black';
            banner.style.fontSize = '2em';
            banner.style.textAlign = 'center';
            banner.style.zIndex = '9999';
            banner.innerHTML = 'Session ended due to inactivity. Please refresh the page to continue.';
            document.body.appendChild(banner);
          }
      
          function resetTimer() {
              clearTimeout(t);
              t = setTimeout(showTimeoutDialog, timeoutTime);
          }
        }
        idleTimer();
                       
        var dimension = [0, 0];
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
    ## Eva has crashed ------
    disconnectMessage(
      text = str_squish(
        "Eva has crashed. Please submit an issue on GitHub and note the
          date and time in order to help the team diagnose the issue."
      ),
      overlayColour = '#F5F5F5',
      refresh = ""
    )
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
        actionButton("Go_to_upload", "Click here to get started"),
        fillable = FALSE
      )
    ),
    accordion(
      id = 'accordion_home',
      open = FALSE,
      
      accordion_panel(
        title = "Instructions",
        value = "home_live_instructions",
        tabHome_home_live_instructions
      ),
      shinyjs::hidden(accordion_panel(
        title = "Demo Instructions",
        value = "home_demo_instructions",
        tabHome_home_demo_instructions
      )),
      accordion_panel(
        title = "Need help?",
        tabHome_need_help,
        value = 'home_need_help'
      ),
      accordion_panel(
        title = "Citations and Special Thanks",
        tabHome_citations
      )
    ),
    br()
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
      shinyjs::hidden(
        uiOutput("demo_text")
      ),
      
      fileInput("imported",
                label = NULL,
                multiple = FALSE,
                accept = ".zip",width = '100%'),
      br(),
      uiOutput("fileInfo") %>% withSpinner()
    ),
    card(
      card_title(headerCard("HMIS CSV Export File Structure Analysis")),
      
      DTOutput("fileStructureAnalysis"),
      br(),
      HTML("<p>Users should contact their vendor to resolve high priority 
            errors identified in the HMIS CSV Export File Structure Analysis, as
            well as any other structural issues which you feel need to be corrected.
            </p>"),
      br(),
      uiOutput('downloadFileStructureAnalysisBtn', fill = FALSE, inline = TRUE,style='margin-bottom:10px'),
      uiOutput('downloadImpermissibleCharacterDetailBtn', fill = FALSE, inline = TRUE),
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
        
        card(class = "border border-0 shadow-none",
          layout_columns(
            col_widths = c(6,6,6,6,6,6),
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
              inputId = "OUTLongStayers",
              label = "Street Outreach:",
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
              inputId = "ServicesOnlyLongStayers",
              label = "Services Only:",
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
      )
    ),
    accordion(
      id = 'accordion_client_count',
      open = FALSE,
      accordion_panel(
        title = "Instructions",
        tabClientCount_instructions
      )
    ),
    br(),
    card(
      card_header(headerCard("Date Range")),
      dateRangeInput(
        "dateRangeCount",
        labe = NULL,
        format = "mm/dd/yyyy",
        start = if_else(isTRUE(getOption("shiny.testmode")), ymd("20231005"), ymd(today())),
        end = if_else(isTRUE(getOption("shiny.testmode")), ymd("20231005"), ymd(today())),
        width = 300
      )
    ),
    
    card(
      card_header(headerCard("Select Project")),
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
        title = headerTab("Client Counts"),
        
        navset_card_underline(
          id = "client_count_cc_subtabs",
          nav_panel(
            title = headerSubTab("Client Counts Summary"),
            DTOutput("clientCountSummary")
          ),
          nav_panel(
            title = headerSubTab("Client Counts Detail"),
            DTOutput("clientCountData")
          ),
        )
      ),
      
      nav_panel(
        title = headerTab("Timeliness"),
        navset_card_underline(
          id = "client_count_ti_subtabs",
          nav_panel(
            title = headerSubTab("Record Entry"),
            layout_column_wrap(
              width = "250px",
              fill = FALSE,

              value_box(
                title = "Median Days to Project Start Data Entry",
                value = textOutput("timeliness_vb1_val"),
                showcase = bs_icon("calendar-plus"),
                theme = "text-primary",
                class = "border-primary"
              ),
              value_box(
                title = "Median Days to Project Exit Data Entry",
                value = textOutput("timeliness_vb2_val"),
                showcase = bs_icon("calendar-minus"),
                theme = "text-primary",
                class = "border-primary"
              ),
              uiOutput("timeliness_vb3", fill = TRUE)
              
            ),
            br(),
            DTOutput("timelinessTable")
            
          )
        )
        
      ),
      
      nav_spacer(),
      nav_item(
        uiOutput("downloadClientCountsReportButton", inline = TRUE)
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
      title = "Project Descriptor Data",
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
          title = headerTab('PDDE Check Summary'),
          DTOutput("pdde_summary_table"),
          br()
        ),
        ### PDDE Guidance table ---------
        nav_panel(
          title = headerTab('Guidance'),
          DTOutput("pdde_guidance_summary")
        ),
        nav_spacer(),
        nav_item(
          uiOutput("downloadPDDEReportButton", inline = TRUE) %>% withSpinner()
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
        ### HP Errors - System ----
        nav_panel(
          id = 'hp_errors_dqsystem',
          title = headerTab('High Priority Errors'),
          
          navset_underline(
            id = 'hp_errors_dqsystem_subtabs',
            selected = headerSubTab("Issues"),
            nav_panel(
              title = headerSubTab('Issues'),
              uiOutput("sysDQHighPriorityByIssue_ui")
            ),
            nav_panel(
              title = headerSubTab('Top 10 Organizations'),
              uiOutput("sysDQHighPriorityByOrg_ui")
            )
          )
        ),
        ### General Errors - System -----
        nav_panel(
          id = 'g_errors_dqsystem',
          title = headerTab('General Errors'),
          
          navset_underline(
            id = 'g_errors_dqsystem_subtabs',
            selected = headerSubTab("Top 10 Issues"),
            nav_panel(
              title = headerSubTab('Top 10 Issues'),
              uiOutput("sysDQErrorByIssue_ui")
            ),
            nav_panel(
              title = headerSubTab('Top 10 Organizations'),
              uiOutput("sysDQErrorByOrg_ui")
            )
          )
        ),
        ### Warnings - System -------
        nav_panel(
          id = 'warnings_dqsystem',
          title = headerTab('Warnings'),
          navset_underline(
            id = 'warnings_dqsystem_subtabs',
            selected = headerSubTab("Top 10 Issues"),
            nav_panel(
              title = headerSubTab("Top 10 Issues"), 
              uiOutput("sysDQWarningByIssue_ui")
            ),
            nav_panel(
              title = headerSubTab("Top 10 Organizations"), 
              uiOutput("sysDQWarningByOrg_ui")
            )
          )
        ),
        nav_spacer(),
        nav_item(
          uiOutput("downloadSystemDQReportButton", inline = TRUE)
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
        card_header(headerCard("Select Organization")),
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
        ### HP Errors - Org ------------
        nav_panel(
          id = 'hp_errors',
          title = headerTab('High Priority Errors'),
          
          navset_underline(
            id = 'hp_errors_dqorg_subtabs',
            selected = headerSubTab("Issues"),
            nav_panel(
              title = headerSubTab('Issues'),
              uiOutput("orgDQHighPriorityByIssue_ui") %>% withSpinner()
            ),
            nav_panel(
              title = headerSubTab('Top 10 Projects'),
              uiOutput("orgDQHighPriorityByProject_ui")  %>% withSpinner()
            )
          )
        ),
        ### General Errors - Org ---------------
        nav_panel(
          id = 'g_errors',
          title = headerTab('General Errors'),
          
          navset_underline(
            id = 'g_errors_dqorg_subtabs',
            selected = headerSubTab("Top 10 Issues"),
            nav_panel(
              title = headerSubTab('Top 10 Issues'),
              uiOutput("orgDQErrorByIssue_ui") %>% withSpinner()
            ),
            nav_panel(
              title = headerSubTab('Top 10 Projects'),
              uiOutput("orgDQErrorByProject_ui")  %>% withSpinner()
            )
          )
        ),
        ### Warnings - Org --------------
        nav_panel(
          id = 'warnings',
          title = headerTab('Warnings'),
          navset_underline(
            id = 'warnings_dqorg_subtabs',
            selected = headerSubTab("Top 10 Issues"),
            nav_panel(
              title = headerSubTab("Top 10 Issues"), 
              uiOutput("orgDQWarningByIssue_ui") %>% withSpinner()
            ),
            nav_panel(
              title = headerSubTab("Top 10 Projects"), 
              uiOutput("orgDQWarningByProject_ui") %>% withSpinner()
            )
          )
        ),
        nav_spacer(),
        nav_item(
          uiOutput("downloadOrgDQReportButton", inline = TRUE)
        )
      ),
      ### DQ Summary and Guidance ------------
      navset_card_underline(
        id = 'dq_summary_subtabs',
        
        nav_panel(
          title = headerTab("Data Quality Summary"),
          DTOutput("dq_organization_summary_table")
        ),
        nav_panel(
          title = headerTab("Data Quality Guidance"),
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
    card_header(headerCard('Filters')),
    layout_columns(
      col_widths=c(6,6),
      gap = 0,
      card(
        id = 'card_filters1',
        style='border-width:0;border-radius:0',
        layout_columns(
          col_widths = c(4,4,4,6,6),
          fill = T,
          
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
          )
        )
      ),
      card(
        id = 'card_filters2',
        style="border-width:0;border-left-width: 1px; border-radius:0",
        layout_columns(
          col_widths = c(12,12),
          pickerInput(
            label = "Race/Ethnicity Methodology Type",
            inputId = "methodology_type",
            multiple = FALSE,
            selected = syso_methodology_types[1],
            choices = syso_methodology_types,
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
      )
    )
    
  ),
  
  navset_card_underline(
    id = 'syso_tabbox',
    
    ## System Flow -------------
    nav_panel(
      id = 'syso_inflowoutflow',
      title = headerTab('System Flow'),
      
      navset_underline(
        id = "sys_inflow_outflow_subtabs",
        selected = headerSubTab("Summary Chart"),
        nav_panel(
          title = headerSubTab('Summary Chart'),
          uiOutput("sys_inflow_outflow_summary_filter_selections") %>%
            withSpinner(),
          plotOutput("sys_inflow_outflow_summary_ui_chart",
                     width = "70%",
                     height = "500") %>%
            withSpinner()
        ),
        nav_panel(
          title = headerSubTab('Detail Chart'),
          uiOutput("sys_inflow_outflow_detail_filter_selections") %>%
            withSpinner(),
          plotOutput("sys_inflow_outflow_detail_ui_chart",
                     width = "100%",
                     height = "500") %>%
            withSpinner()
        ),
        nav_panel(
          title = headerSubTab("Month-by-Month Chart"), 
          uiOutput("sys_inflow_outflow_monthly_filter_selections") %>%
           withSpinner(),
          radioGroupButtons(
            inputId = "mbm_status_filter",
            label = "Flow Type Filters",
            choices = c("All", "First-Time Homeless", "Inactive"),
            #Inactive
            selected = "All",
            individual = TRUE,
            checkIcon = list(yes = icon("check"))
          ), 
          conditionalPanel(
            condition = "input.mbm_status_filter == 'Inactive'",
            plotOutput("sys_inactive_monthly_ui_chart", width = "100%", height = "500")
          ), 
          conditionalPanel(
            condition = "input.mbm_status_filter == 'First-Time Homeless'",
            plotOutput("sys_fth_monthly_ui_chart", width = "100%", height = "500")
          ),
          conditionalPanel(
            condition = "input.mbm_status_filter == 'All'",
            plotOutput("sys_inflow_outflow_monthly_ui_chart", width = "100%", height = "500") %>%
              withSpinner()
          ),
          conditionalPanel(
            condition = "input.mbm_status_filter == 'All'",
            DTOutput("sys_inflow_outflow_monthly_table") %>%
              withSpinner()
          )
        ),
        # nav_panel(
        #   title = "Timeline Chart",
        #          plotlyOutput("timelinePlot", height = "600px"),
        #          selectizeInput(
        #            inputId = "enrollmentIDFilter",
        #            label = "Search by Enrollment ID",
        #            choices = NULL, # We'll populate this dynamically
        #            options = list(
        #              placeholder = "Type to search for Enrollment ID",
        #              closeAfterSelect = TRUE
        #            ),
        #            multiple = TRUE
        #          ),
        #          
        #          # PersonalID Filter
        #          selectizeInput(
        #            inputId = "personalIDFilter",
        #            label = "Search by Personal ID",
        #            choices = NULL, # We'll populate this dynamically
        #            options = list(
        #              placeholder = "Type to search for Personal ID",
        #              closeAfterSelect = TRUE
        #            ),
        #            multiple = TRUE
        #          ),
        #          conditionalPanel(
        #            condition = "len(input.personalIDFilter)",
        #            h4("Person's Monthly Inflow/Outflow"),
        #            verbatimTextOutput("personDetails")
        #          )
        #          
        # ),
        nav_panel(
          title = headerSubTab("Information"),
          br(),
          tab_sys_inflow_outflow_subtabs_information
        )
      ),
      downloadButton("sys_inflow_outflow_download_btn", "Data Download", style='margin-right:2px'),
      downloadButton("sys_inflow_outflow_download_btn_ppt", "Image Download")
    ),
    
    ## System Status/Sankey ----------------
    nav_panel(
      id = 'syso_systemstatus',
      title = headerTab("Client System Status"),
      navset_underline(
        id = 'sys_status_subtabs',
        selected = headerSubTab("Chart"),
        
        nav_panel(
          title = headerSubTab("Chart"),   
          uiOutput("sankey_filter_selections") %>% withSpinner(),
          plotOutput("sankey_ui_chart", width="70%") %>% withSpinner()
        ),
        nav_panel(
          title = headerSubTab("Information"),
          br(),
          tab_sys_status_subtabs_information
        )
      ),
      downloadButton("sys_status_download_btn", "Data Download", style='margin-right:2px'),
      downloadButton("sys_status_download_btn_ppt", "Image Download"),
    ),
    
    ## System Demographics/Composition --------------
    nav_panel(
      id = 'syso_composition',
      title = headerTab("System Demographics"),
      
      navset_underline(
        id = 'sys_comp_subtabs',
        selected = headerSubTab("Chart"),
        nav_panel(
          title = headerSubTab("Chart"),
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
          uiOutput("sys_comp_summary_selections",inline = TRUE),
          plotOutput("sys_comp_summary_ui_chart") %>% withSpinner()
        ),
        nav_panel(
          title = headerSubTab("Information"),
          br(),
          tab_sys_comp_subtabs_information
          
        )
      ),
      downloadButton("sys_comp_download_btn", "Data Download", style='margin-right:2px'),
      downloadButton("sys_comp_download_btn_ppt", "Image Download")
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
  ),
  nav_spacer(),
  nav_item(
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
    ),
    id="demo_wrapper",
    style="text-align: right;"
  )
)
