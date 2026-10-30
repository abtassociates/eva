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
      
      ## Demographics subtab - using mod_system_demog -----
      mod_system_demog_ui(id = ns('demog'), title_prefix = 'Unsheltered', instructions = tab_unsh_demog_subtabs_information)
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
        get_unsheltered_universe(enrl = unsh_enrollments_filtered(), 
                                 client = unsh_client_categories_filtered(), session) 
      })
      
      unsh_people_universe_demog <- reactive({
        get_unsheltered_universe(enrl = unsh_enrollments_filtered(), 
                                 client = session$userData$client_categories, session) %>% 
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
      
      ## Distribution subtab outputs -----
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
          fsubset(!is.na(homeless_type_full_period) & homeless_type_full_period != 'PH Only') %>% 
          fselect('PersonalID', 'homeless_type_full_period') %>% 
          funique()
        
        build_dist_chart(dist_enrl, filter_vals = filter_vals, level_of_detail = unsh_level_of_detail_text(),
                         chart_colors = unsh_colors)
      })
      
      output$flow_chart <- renderPlot({
        req(session$userData$valid_file() == 1 )
        
        unsh_client_enrl_filt() %>% 
          ## universe for this chart is clients with Unsheltered or Both enrollments
          fsubset(!is.na(homeless_type_full_period) & !(homeless_type_full_period %in% c('Sheltered','PH Only'))) 
      })
      
      ## PIT subtab outputs -----
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
      
     ## Demographics subtab outputs - using submodule -----
     unsh_demog_out <- mod_system_demog_server(id = 'demog', type = 'unsheltered',
                                               people_univ = unsh_people_universe_demog(),
                                               display_var = unsh_full_unit_of_analysis_display(), 
                                               filter_vals, global_vars)
      
     # Toggle Age, Vet Status, and Race filters based on System OVerview subtab (i.e. hide for System Demographics/Composition)
     observeEvent(input$unsh_tabbox, {
        req(session$userData$valid_file() == 1)
        logMetadata(session, paste0("Clicked on ", input$unsh_tabbox,
                                    if_else(isTruthy(global_vars$in_demo_mode), " - DEMO MODE", "")))
       
        shinyjs::runjs(str_glue("
    $('#{id}-filt-spec_pops, #{id}-filt-age, #{id}-filt-race_ethnicity')
      .closest('.bslib-grid-item')
      .toggle({ifelse(input$unsh_tabbox != '<h4>Unsheltered Demographics</h4>', 'true','false')});
  "))
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
 
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

get_unsheltered_universe <- function(enrl, client, session){
  
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
  
  join(client,
       enrl_filt, on = 'PersonalID', how='left'
  )
  
}

build_dist_chart <- function(dist_enrl, filter_vals, level_of_detail, chart_colors = unsh_colors){
  
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
  
  chart_title <- paste0("Total ",
                        level_of_detail, " in ",
                        str_remove(getNameByValue(sys_hh_types, filter_vals$hh_type()), "- "),
                        if_else(getNameByValue(sys_hh_types, filter_vals$hh_type()) == "All Household Types", "", " Households"), 
                        " who Experienced Unsheltered Homelessness: ", scales::label_comma()(nr),"\n")
  #if(show_legend == FALSE){
  ggplot(tree_unsh_data, aes(area = Count, fill = homeless_type_full_period,
                             label = label, subgroup = border_color) )+
    labs(title = chart_title) +
    geom_treemap(layout='squarified', start='bottomright',color = "black", size = 2, show.legend = FALSE) +
    geom_treemap_text(layout='squarified', start='bottomright',color = "black",  place = "bottomleft", grow = FALSE, reflow = TRUE) +
    #geom_treemap_subgroup_border(layout='squarified',start='bottomright',color = "black", size = 4, show.legend = FALSE) +
    scale_color_identity() +
    scale_fill_manual(values = chart_colors) +
    theme_minimal() +
    coord_fixed(ratio =0.8) +
    theme(
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
  
  # } else if (show_legend == TRUE){
  # }
  
}