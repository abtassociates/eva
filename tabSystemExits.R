#mod_system_exits_ui <- function(id) {
#  ns <- NS(id)
tabSystemExits <- nav_panel(
    title = "System Exits",
    value = "tabSystemExits",
    icon = icon('door-open'),
    
    card(
      htmlOutput("headerSystemExit")
    ),
    accordion(
      id = 'accordion_systemexits',
      open = FALSE,
      accordion_panel(
        title = 'Instructions',
        tabSystemExits_instructions
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
          id = 'syse_filters_left',
          style='border-width:0;border-radius:0',
          layout_columns(
            col_widths = c(4,4,4,6,6),
            fill = T,
            
            pickerInput(
              label = "Household Type",
              inputId = "syse_hh_type",
              choices = sys_hh_types,
              selected = sys_hh_types[1],
              options = pickerOptions(container = "body")
            ),
            pickerInput(
              label = "Level of Detail",
              inputId = "syse_level_of_detail",
              choices = sys_level_of_detail,
              selected = sys_level_of_detail[1],
              options = pickerOptions(container = "body")
            ),
            pickerInput(
              label = "Project Type Group",
              inputId = "syse_project_type",
              choices = sys_project_types,
              selected = sys_project_types[1],
              options = pickerOptions(container = "body")
            ),
            pickerInput(
              inputId = "syse_age",
              label = "Age",
              selected = sys_age_cats,
              choices = sys_age_cats,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = paste("count >", length(sys_age_cats)-1),
                countSelectedText = "All Ages",
                noneSelectedText = "All Ages",
                container = "body"
              )
            ),
            pickerInput(
              label = "Veteran Status",
              inputId = "syse_spec_pops",
              choices = sys_spec_pops_people,
              selected = sys_spec_pops_people[1],
              options = pickerOptions(container = "body")
            )
          )
        ),
        card(
          id = 'syse_filters_right',
          style="border-width:0;border-left-width: 1px; border-radius:0",
          layout_columns(
            col_widths = c(12,12),
            pickerInput(
              label = "Race/Ethnicity Methodology Type",
              inputId = "syse_methodology_type",
              multiple = FALSE,
              selected = sys_methodology_types[1],
              choices = sys_methodology_types,
              options = pickerOptions(container = "body")
            ),
            pickerInput(
              label = "Race/Ethnicity",
              inputId = "syse_race_ethnicity",
              choices = sys_race_ethnicity_method1,
              selected = sys_race_ethnicity_method1,
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
      id = 'syse_tabbox',
      
      nav_panel(
        title = headerTab('Exits by Type'),
        
        navset_underline(
          id = "syse_types_subtabs",
          selected = headerSubTab("Chart"),
          
          nav_panel(
            title = headerSubTab("Chart"),
            uiOutput("syse_types_filter_selections") %>%
              withSpinner(),
            
            plotOutput("syse_types_ui_chart",
                       #width = "75%"
                       height = "700px"
            ) %>%
              withSpinner()
          ),
          nav_panel(
            title = headerSubTab("Information"),
            br(),
            tab_syse_types_subtabs_information
          )
        ),
        downloadButton("syse_types_download_btn", "Data Download", style='margin-right:2px'),
        downloadButton("syse_types_download_btn_ppt", "Image Download")
      ),
      nav_panel(
        title = headerTab('Exits by Year'),
        navset_underline(
          id = "syse_time_subtabs",
          selected = headerSubTab('Chart'),
          nav_panel(
            title = headerSubTab('Chart'),
            uiOutput("syse_compare_time_filter_selections") %>%
              withSpinner(),
            div(
              style='margin-left:17px;',
              plotOutput("syse_compare_time_chart",
                         width = "92%",
                         height = "500"
              ) %>% withSpinner()
            ),
            
            DTOutput("syse_compare_time_table") %>%
              withSpinner()
          ),
          nav_panel(
            title = headerSubTab('Information'),
            br(),
            tab_syse_time_chart_information
          )
        ),
        downloadButton("syse_time_download_btn", "Data Download", style='margin-right:2px'),
        downloadButton("syse_time_download_btn_ppt", "Image Download")
      ),
      
      nav_panel(
        title = headerTab('Exits by Subpopulation'),
        navset_underline(
          id = "syse_subpop_subtabs",
          selected = headerSubTab('Chart'),
          nav_panel(
            title = headerSubTab('Chart'),
            card(
              br(),
              strong("Select Demographic Crosstab Categories (up to 2) and a Destination Type"),
              HTML("<p>Select one demographic category to view totals within that group, or two categories to create a crosstab showing intersections between groups. To change your selection, uncheck a category before selecting a new one. You may also apply a Household Type filter, which functions as an additional grouping.</p>
                        <br>
                        <p>Select a destination type to determine which exit outcomes are displayed.</p>"
              ),
              br(),
              layout_columns(
                col_widths = c(3,3,6),fill=T,
                tagList(
                  checkboxInput('syse_subpop_age_selection', 'Age'),
                  div(id ='age_picker',style='margin-top:0px; padding-top:0px;',
                      pickerInput(
                        inputId = "syse_subpop_age",
                        label=NULL,#label = "Age",
                        selected = sys_age_cats,
                        choices = sys_age_cats,
                        multiple = TRUE,
                        options = pickerOptions(
                          actionsBox = TRUE,
                          selectedTextFormat = paste("count >", length(sys_age_cats)-1),
                          countSelectedText = "",
                          noneSelectedText = "None Selected",
                          container = "body",
                        )
                      )
                  )
                ),
                tagList(
                  checkboxInput('syse_subpop_vet_selection', 'Veteran Status (Adult Only)'),
                  div(id = 'vet_picker',
                      pickerInput(
                        label = NULL,#label = "Veteran Status",
                        inputId = "syse_subpop_spec_pops",
                        #choices = sys_spec_pops_people,
                        choices = setNames(sys_spec_pops_people,
                                           nm = c("None Selected", names(sys_spec_pops_people[-1]))
                        ),
                        selected = "None Selected",
                        options = pickerOptions(container = "body")
                      )
                  )
                ),
                tagList(
                  checkboxInput('syse_subpop_race_eth_selection', 'Race/Ethnicity'),
                  div(id='race_eth_picker',
                      conditionalPanel(condition = 'input.syse_methodology_type == 1',
                                       pickerInput(
                                         label = NULL,#"Race/Ethnicity",
                                         inputId = "syse_subpop_race_ethnicity1",
                                         choices = setNames(sys_race_ethnicity_method1,
                                                            c("None Selected", names(sys_race_ethnicity_method1)[-1])
                                         ),
                                         selected = "None Selected",
                                         options = list(
                                           `dropdown-align-right` = TRUE,
                                           `dropup-auto` = FALSE,
                                           container = "body",
                                           noneSelectedText = "-"
                                         )
                                       )
                      ),
                      conditionalPanel(condition = 'input.syse_methodology_type == 2',
                                       pickerInput(
                                         label = NULL,#"Race/Ethnicity",
                                         inputId = "syse_subpop_race_ethnicity2",
                                         choices = setNames(sys_race_ethnicity_method2,
                                                            c("None Selected",names(sys_race_ethnicity_method2)[-1])),
                                         selected = "None Selected",
                                         options = list(
                                           `dropdown-align-right` = TRUE,
                                           `dropup-auto` = FALSE,
                                           container = "body",
                                           noneSelectedText = "-"
                                         )
                                       )
                      )
                  )
                )
              ),
              radioGroupButtons(
                inputId = "subpop_dest_type",
                label = "Destination Type",
                choices = c("Permanent", "Homeless", "Institutional","Temporary","Other/Unknown"),
                #Inactive
                selected = "Permanent",
                individual = TRUE
              ), 
              width = 12
            ),
            br(),
            uiOutput("syse_compare_subpop_filter_selections") %>%
                           withSpinner(),
            div(
              style='margin-left:17px;',
              plotOutput("syse_compare_subpop_chart",
                         width = "92%",
                         height = "500")
            ),
            
          ),
          nav_panel(
            title = headerSubTab('Information'),
            br(),
            tab_syse_subpop_chart_information
          )
        ),
        downloadButton("syse_subpop_download_btn", "Data Download", style='margin-right:2px'),
        downloadButton("syse_subpop_download_btn_ppt", "Image Download")
      ),
      
      nav_panel(
        title = headerTab('Exits to PH Demographics'),
        navset_underline(
          id = "syse_phd_subtabs",
          selected = headerSubTab("Chart"),
          
          nav_panel(
            title = headerSubTab("Chart"),
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
                "syse_phd_selections",
                label = "",
                choices = sys_heatmap_selection_choices,
                selected = c("All Races/Ethnicities", "Age"),
                inline = TRUE
              ),
              width = 12
            ),
            br(),
            
            uiOutput("syse_phd_summary_selections",inline = TRUE),
            #plotOutput("syse_phd_chart", width="100%") %>% withSpinner()
            conditionalPanel(
              condition = 'input.syse_phd_selections.length == 1',
              plotOutput("syse_phd_chart_1d",height=700,width=500) %>% withSpinner(),
            ),
            conditionalPanel(
              condition = 'input.syse_phd_selections.length == 2',
              plotOutput("syse_phd_chart_2d", height=700,width="auto") %>% withSpinner()
              
            )
            
          ),
          nav_panel(
            title = headerSubTab("Information"),
            br(),
            tab_syse_phd_subtabs_information
          )
        ),
        downloadButton("syse_phd_download_btn", "Data Download", style='margin-right:2px'),
        downloadButton("syse_phd_download_btn_ppt", "Image Download")
      )
    ),
    downloadButton("syse_client_level_download_btn", "Client Level Download")
  )
#}

# mod_system_exits_server <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       
#     }
#   )
# }