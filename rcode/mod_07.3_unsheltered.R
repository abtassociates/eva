mod_unsheltered_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Unsheltered Homelessness",
    value = "tabUnsheltered",
    icon = icon('flag'),
    ## Tab Header -----
    card(
      htmlOutput("headerUnsheltered")
    ),
    ## Tab Instructions ------
    accordion(
      id = 'accordion_unsheltered',
      open = FALSE,
      accordion_panel(
        title = 'Instructions',
        tabUnsheltered_instructions
      )
    ),
    br(),
    
    ## Filter Panel submodule ---------------
    mod_filter_panel_ui(id=ns('filt'),
                        flabels = set_filter_labels('project_type'= "Unsheltered Project Type"),
                        fchoices = set_filter_choices("project_type" = unsh_project_types)
                        ),
    
    ## Unsheltered Subtabs -------------
    navset_card_underline(
      id = ns('unsh_tabbox'),
      
      ### Distribution ----
      nav_panel(
          title = headerTab("Unsheltered Distribution"),
          navset_underline(
            id = ns("dist_subtabs"),
            selected = headerSubTab("Chart"),
            nav_panel(
              title = headerSubTab("Chart"),
              uiOutput(outputId = ns("dist_filter_selections")) %>%
                withSpinner(),
              plotOutput(outputId = ns('dist_chart')) %>% withSpinner()
            ),
            nav_panel(
              title = headerSubTab("Information"),
              br(),
              tab_unsh_dist_subtabs_information
            )
          )
      ),
      
      ### PIT -----
      nav_panel(
        title = headerTab("Unsheltered PIT"),
        navset_underline(
          id = ns("pit_subtabs"),
          selected = headerSubTab("Chart"),
          nav_panel(
            title = headerSubTab("Chart"),
            uiOutput(outputId = ns("pit_filter_selections")) %>%
              withSpinner(),
            div(id=ns('pit_vertical_div'),
                verticalLayout(
                  plotOutput(outputId = ns("pit_chart")) %>% withSpinner(),
                  br(),
                  DTOutput(outputId = ns("pit_table")) %>% withSpinner()
                )
            )
          ),
          nav_panel(
            title = headerSubTab("Information"),
            br(),
            tab_unsh_pit_subtabs_information
          )
        )
      ),
      
      ### Flow -----
      nav_panel(
        title = headerTab("Unsheltered Flow"),
        navset_underline(
          id = ns("flow_subtabs"),
          selected = headerSubTab("Chart"),
          nav_panel(
            title = headerSubTab("Chart"),
            uiOutput(outputId = ns("flow_filter_selections")) %>%
              withSpinner(),
            div(id=ns('flow_vertical_div'),
                
                  plotOutput(outputId = ns("flow_chart")) %>% withSpinner(),
                  br(),
                  DTOutput(outputId = ns("flow_table")) %>% withSpinner()
                )
            )
          ),
          nav_panel(
            title = headerSubTab("Information"),
            br(),
            tab_unsh_flow_subtabs_information
          )
        ),
      nav_panel(
        title = headerTab("Unsheltered Demographics"),
        navset_underline(
          id = ns("demog_subtabs"),
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
                ns("demog_selections"),
                label = "",
                choices = sys_heatmap_selection_choices,
                selected = c("All Races/Ethnicities", "Age"),
                inline = TRUE
              ),
              width = 12
            ),
            # uiOutput(outputId = glue("demog_filter_selections")) %>%
            #   withSpinner(),
            plotOutput(outputId = ns('demog_chart')) %>% withSpinner()
          ),
          nav_panel(
            title = headerSubTab("Information"),
            br(),
            tab_unsh_demog_subtabs_information
          )
        )
      ),
    )
  )
}

mod_unsheltered_server <- function(id, global_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      unsh_colors <- c(
        "Sheltered" = get_brand_color('dark_grey'),
        "Unsheltered" = get_brand_color('coral'),
        'Both' = get_brand_color('light_grey')
      )
      
      ## Run and retrieve output from filter panel submodule -----
      unsh_filtered_data <- mod_filter_panel_server(id='filt', global_vars)
      
      # Client-level flags, filtered 
      unsh_client_categories_filtered <- unsh_filtered_data$client
      unsh_enrollments_filtered <- unsh_filtered_data$enrl
      filter_vals <- unsh_filtered_data$filter_vals

      ## Reactives -----
      unsh_pit_dates <- reactive({
        get_pits(session) %>% sort()
      })
      
      unsh_pit_df <- reactive({
        
        joined <- unsh_client_enrl_filt() %>% #enrl_sub %>% 
          fsubset(!is.na(homeless_type_full_period)) %>% 
          fmutate(active_at_pit1 = unsh_pit_dates()[1] %between% list(active_start, active_end),
                  active_at_pit2 = unsh_pit_dates()[2] %between% list(active_start, active_end),
                  active_at_pit3 = unsh_pit_dates()[3] %between% list(active_start, active_end),
                  active_at_pit4 = unsh_pit_dates()[4] %between% list(active_start, active_end)) %>% 
          fgroup_by(PersonalID) %>%
          fsummarize( 
            homeless_at_pit1 = calc_homeless_type2(unsheltered, sheltered, permanent_housing, cond=active_at_pit1),
            homeless_at_pit2 = calc_homeless_type2(unsheltered, sheltered, permanent_housing, cond=active_at_pit2),
            homeless_at_pit3 = calc_homeless_type2(unsheltered, sheltered, permanent_housing, cond=active_at_pit3),
            homeless_at_pit4 = calc_homeless_type2(unsheltered, sheltered, permanent_housing, cond=active_at_pit4)
          ) 
        
        joined %>% 
          fselect(PersonalID, #homeless_type_full_period, 
                  homeless_at_pit1:homeless_at_pit4) %>% 
          pivot(ids=1,how='longer', values=2:5, names=list('period','homeless_type'))
        
      })
      
      unsh_level_of_detail_text <- reactive({
        
        case_when(
          filter_vals$level_of_detail() == "All" ~ "People",
          filter_vals$level_of_detail() == "HoHsOnly" ~ "Heads of Household",
          TRUE ~
            getNameByValue(sys_level_of_detail, filter_vals$level_of_detail())
        )
      })
      
      unsh_full_unit_of_analysis_display <- reactive({
        paste0(
          "Total ", 
          unsh_level_of_detail_text(),
          if_else(
            filter_vals$hh_type() == "All",
            "",
            paste0(" in ",
                   str_remove(getNameByValue(sys_hh_types, filter_vals$hh_type()), "- "),
                   " Households")
          )
        )
      })
      
      unsh_client_enrl_filt <- reactive({
        
        enrl <- unsh_enrollments_filtered()  
        
        active_enrl <- get_active_spans(enrl,enrl, 
                                        session$userData$lh_info, session$userData$ReportStart, session$userData$ReportEnd)
        enrl_filt <- join(enrl, active_enrl, how='left') %>% 
          fsubset(active_in_full_period) %>% 
          fsubset(
            ProjectType != hp_project_type &
              EntryDate <= session$userData$ReportEnd & ExitAdjust >= session$userData$ReportStart
          ) %>% 
          fgroup_by(PersonalID) %>%
          fmutate( 
            homeless_type_full_period = calc_homeless_type2(unsheltered, sheltered, permanent_housing)
          ) %>% 
          fungroup()
        
        join(unsh_client_categories_filtered(),
             enrl_filt, on = 'PersonalID', how='full'
        )
        
      })
      
      output$dist_filter_selections <-renderUI({ 
        
        req(session$userData$valid_file() == 1 )
        
        sys_detailBox(
          detail_type = 'unsh',
          methodology_type = filter_vals$methodology_type(),
          cur_project_types = filter_vals$project_type(),
          startDate = session$userData$ReportStart,
          endDate = session$userData$ReportEnd,
          age = filter_vals$age(),
          spec_pops = filter_vals$spec_pops(),
          race_eth = filter_vals$race_ethnicity()
        )
        
      })
      
      
      output$dist_chart <- renderPlot({
        
        dist_enrl <- unsh_client_enrl_filt() %>% 
          fsubset(!is.na(homeless_type_full_period) & homeless_type_full_period != 'PH Only')
        nr <- nrow(dist_enrl)
        
        validate(need(nr > 0, no_data_msg))
        validate(need(nr > 10, suppression_msg))
        
        border_color <- 'black'
        
        ## client level counts and %ages of HomelessnessType
        tree_unsh_data <-  dist_enrl %>% 
          fcount(homeless_type_full_period, name='Count') %>% 
          fmutate(Percent = Count/fsum(Count),
                  label = str_c(homeless_type_full_period, ': ', scales::label_comma()(Count),
                                ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
                  ))
        
        #if(show_legend == FALSE){
        ggplot(tree_unsh_data, aes(area = Count, fill = homeless_type_full_period,
                                   label = label, subgroup = border_color) )+
          labs(title = paste0("Total ",
                              unsh_level_of_detail_text(), " in ",
                              str_remove(getNameByValue(sys_hh_types, filter_vals$hh_type()), "- "),
                              if_else(getNameByValue(sys_hh_types, filter_vals$hh_type()) == "All Household Types", "", " Households"), 
                              " who Experienced Unsheltered Homelessness: ", scales::label_comma()(nr),"\n")
          ) +
          geom_treemap(layout='squarified', start='bottomright',color = "black", size = 2, show.legend = FALSE) +
          geom_treemap_text(layout='squarified', start='bottomright',color = "black",  place = "bottomleft", grow = FALSE, reflow = TRUE) +
          #geom_treemap_subgroup_border(layout='squarified',start='bottomright',color = "black", size = 4, show.legend = FALSE) +
          scale_color_identity() +
          scale_fill_manual(values = unsh_colors) +
          theme_minimal() +
          coord_fixed(ratio =0.8) +
          theme(
            plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
          )
        
        # } else if (show_legend == TRUE){
        # }
        
      })
      sys_comp_plot_df <- reactiveVal()
      
      output$demog_chart <- renderPlot({
       
        req(
          !is.null(input$demog_selections) &
            session$userData$valid_file() == 1 &
            between(length(input$demog_selections), 1, 2)
        )
        
        validate(
          need(
            fnrow(session$userData$enrollment_categories) > 0,
            no_valid_data_msg
          )
        )
        demog_unsh_data <-unsh_client_enrl_filt() %>% #unsh_client_categories_filtered() %>% 
          ## universe for this chart is clients with Unsheltered or Both enrollments
          fsubset(!is.na(homeless_type_full_period) & !(homeless_type_full_period %in% c('Sheltered','PH Only'))) #%>% 
        
        if(length(input$demog_selections) == 1) {
          sys_comp_plot_1var(subtab = 'unsh', 
                             methodology_type = filter_vals$methodology_type(), 
                             selection = input$demog_selections, 
                             people_univ = demog_unsh_data,
                             isExport = FALSE, display_var = unsh_full_unit_of_analysis_display())
        } else {
          sys_comp_plot_2vars(subtab = 'unsh', 
                              methodology_type = filter_vals$methodology_type(), 
                              selections = input$demog_selections, 
                              people_univ = demog_unsh_data,
                              isExport = FALSE, session = session, display_var = unsh_full_unit_of_analysis_display())
          
        }
      }, height = function() {
        ifelse(!is.null(input$demog_selections), 700, 100)
      }, width = function() {
        input$demog_subtabs
        input$unsh_tabbox
        input$pageid
        if (length(input$demog_selections) == 1 |
            isTRUE(getOption("shiny.testmode"))) {
          500
        } else {
          "auto"
        }
      }, alt = "A crosstab data table of the demographic make-up of the homeless system.")
      
      output$demog_filter_selections <-renderUI({ 
        
        req(session$userData$valid_file() == 1 )
        
        sys_detailBox(
          detail_type = 'unsh',
          methodology_type = filter_vals$methodology_type(),
          cur_project_types = filter_vals$project_type(),
          startDate = session$userData$ReportStart,
          endDate = session$userData$ReportEnd,
          age = filter_vals$age(),
          spec_pops = filter_vals$spec_pops(),
          race_eth = filter_vals$race_ethnicity()
        )
        
      })
      
      output$flow_chart <- renderPlot({
        req(session$userData$valid_file() == 1 )
        
        unsh_client_enrl_filt() %>% 
          ## universe for this chart is clients with Unsheltered or Both enrollments
          fsubset(!is.na(homeless_type_full_period) & !(homeless_type_full_period %in% c('Sheltered','PH Only'))) 
      })
      
      unsh_pit_counts <- reactive({
        req(session$userData$valid_file() == 1 )
        
        # If the client is active in both a sheltered and unsheltered 
        # enrollment on a given PIT date, precedence is given to the active 
        # sheltered enrollment (i.e., the client is counted as "Sheltered")
        
        # If a client is active in a sheltered enrollment and inactive in an 
        # unsheltered enrollment on a given PIT date, the client should be 
        # counted under "Sheltered"
        
        # If a client is active in an unsheltered enrollment and inactive in an 
        # ES – NbN enrollment on a given PIT date, the client should be 
        # counted under "Unsheltered"
        
        ## need to account for homelessnesstype changing at each pit date before counting
        counts_df <- unsh_pit_df() %>% 
          fsubset(!is.na(homeless_type) & homeless_type != 'PH Only' & homeless_type != 'Other') %>% 
          fmutate(homeless_type = ifelse(homeless_type == 'Both', 'Sheltered',homeless_type)) %>% 
          fgroup_by(period, homeless_type) %>%  
          fsummarize(n = GRPN()) %>% 
          fungroup() 
        counts_df  
      })
      
      output$pit_table <- renderDT({
        req(session$userData$valid_file() == 1)
        
        pit_dt_df <- unsh_pit_counts() %>% 
          pivot_wider(id_cols='homeless_type',names_from='period', values_from='n') %>% 
          setNames(c(' ',format(unname(unsh_pit_dates()), '%m/%d/%Y') ))
        
        datatable(pit_dt_df, 
                  options = list(dom='t', ordering = FALSE), style='default',
                  filter = 'none', selection='none', rownames = FALSE)
        
      })
      
      
      output$pit_chart <- renderPlot({
        req(session$userData$valid_file() == 1)
        
        ggplot(unsh_pit_counts(), aes(x=period, fill=factor(homeless_type), y=n)) +
          geom_bar(stat='identity', position='dodge') +
          scale_fill_manual(values=unsh_colors) +
          scale_y_continuous(limits=c(0, NA), expand = expansion(mult=c(0,0.1),add=0)) +
          labs(y = '', 
               title = ) +
          theme_minimal() +
          theme(legend.position = 'none', 
                axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank(),
                axis.text.y = element_text(size = sys_axis_text_font))
        
      })
      
      
      output$pit_filter_selections <-renderUI({ 
        
        req(session$userData$valid_file() == 1 )
        
        sys_detailBox(
          detail_type = 'unsh',
          methodology_type = filter_vals$methodology_type(),
          cur_project_types = filter_vals$project_type(),
          startDate = session$userData$ReportStart,
          endDate = session$userData$ReportEnd,
          age = filter_vals$age(),
          spec_pops = filter_vals$spec_pops(),
          race_eth = filter_vals$race_ethnicity()
        )
        
      })
      
      observeEvent(input$demog_selections, {
        # they can select up to 2
        #disable all unchecked boxes if they've already selected 2
        shinyjs::runjs(str_glue("
    var numSelected = {length(input$`unsh-demog_selections`)};
    $('input[name=`unsh-demog_selections`]:not(\":checked\")')
      .attr('disabled', numSelected == 2);

    var reSelected = \"{
      \"All Races/Ethnicities\" %in% input$`unsh-demog_selections` |
      \"Grouped Races/Ethnicities\" %in% input$`unsh-demog_selections`
    }\";

    if(numSelected == 1)
      $('input[name=`unsh-demog_selections`][value*=\"Races/Ethnicities\"]:not(\":checked\")')
        .attr('disabled', reSelected == 'TRUE');
  "))
      }, ignoreNULL = FALSE)
    }
  )
}

calc_homeless_type2 <- function(unsheltered, sheltered, permanent_housing, cond=NULL){
  if(!is.null(cond)){
    fcase(
      (fsum(cond & unsheltered) > 0) & (fsum(cond & sheltered) > 0), 'Both',
      fsum(cond & unsheltered) > 0, 'Unsheltered',
      fsum(cond & sheltered) > 0, 'Sheltered',
      (fsum(cond & permanent_housing) > 0) & (fsum(cond & unsheltered) == 0) & (fsum(cond & sheltered) == 0), 'PH Only',
      default='Other'
    )
  } else {
    fcase(
      (fsum(unsheltered) > 0) & (fsum(sheltered) > 0), 'Both',
      fsum(unsheltered) > 0, 'Unsheltered',
      fsum(sheltered) > 0, 'Sheltered',
      (fsum(permanent_housing) > 0) & (fsum(unsheltered) == 0) & (fsum(sheltered) == 0), 'PH Only',
      default='Other'
    )
  }
  
}