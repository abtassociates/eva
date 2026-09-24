unsh_colors <- c(
  "Sheltered" = get_brand_color('dark_grey'),
  "Unsheltered" = get_brand_color('coral'),
  'Both' = get_brand_color('light_grey')
)

# Client-level flags, filtered ----------------------------------------------------
unsh_client_categories_filtered <- reactive({
  
  logToConsole(session, "In unsh_client_categories_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  req(nrow(session$userData$client_categories) > 0)
  
  session$userData$client_categories[
    AgeCategory %in% input$unsh_age &
      (if(input$unsh_race_ethnicity == "All") rep(TRUE, .N) else get(input$unsh_race_ethnicity) == 1) & 
      (
        input$unsh_spec_pops == "None" |
          (input$unsh_spec_pops == "Veteran" &
             VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
          (input$unsh_spec_pops == "NonVeteran" &
             VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17"))
      )
  ]
})

unsh_enrollments_filtered <- reactive({
  
  hh_val <- input$unsh_hh_type
  lod_val <- input$unsh_level_of_detail
  pt_val <- input$unsh_project_type
  vet_val <- input$unsh_spec_pops
  join(session$userData$enrollment_categories,
       session$userData$client_categories |> fselect(PersonalID, VeteranStatus),
       on='PersonalID', how='left'
      ) |> 
    fsubset(
      # Household type filter
        (hh_val == "All" |
           (hh_val == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (hh_val == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (hh_val == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (hh_val == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           hh_val == HouseholdType)
        &
        
        # Level of detail filter
        (lod_val == "All" |
           (lod_val == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (lod_val == "HoHsOnly" &
              CorrectedHoH == 1)) #&
        
        # Project type filter (wrapped in parentheses to preserve logical order of operations)
        # (pt_val == "All" |
        #    (pt_val %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
        #    (pt_val %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
        #    (pt_val == "SO" & ProjectType == sso_project_type) |
        #    (pt_val == "AllNonRes" & ProjectType %in% non_res_project_types))
    ) |>
    fselect(-VeteranStatus)
})
# Create passes-enrollment-filter flag to exclude enrollments from heatmap -------


unsh_level_of_detail_text <- reactive({
  case_when(
    input$unsh_level_of_detail == "All" ~ "People",
    input$unsh_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(sys_level_of_detail, input$unsh_level_of_detail)
  )
})

unsh_client_enrl_filt <- reactive({
  join( 
    unsh_client_categories_filtered(),
    unsh_enrollments_filtered(),
    on = "PersonalID",
    how = "inner"
  )
})

output$unsh_dist_filter_selections <-renderUI({ 
  
  req(session$userData$valid_file() == 1 )
  
  sys_detailBox(
    detail_type = 'unsh',
    methodology_type = input$unsh_methodology_type,
    cur_project_types = input$unsh_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$unsh_age,
    spec_pops = input$unsh_spec_pops,
    race_eth = input$unsh_race_ethnicity
  )
 
})


output$unsh_dist_chart <- renderPlot({
  
  nr <- nrow(unsh_client_enrl_filt())
  
  validate(need(nr > 0, no_data_msg))
  validate(need(nr > 10, suppression_msg))
  
  border_color <- 'black'
  
  ## client level counts and %ages of HomelessnessType
  tree_unsh_data <-  unsh_client_enrl_filt() %>% 
    fsubset(!is.na(HomelessnessType) & HomelessnessType != 'PH Only') %>% 
    fcount(HomelessnessType, name='Count') %>% 
    fmutate(Percent = Count/fsum(Count),
          label = str_c(HomelessnessType, ': ', scales::label_comma()(Count),
                        ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
          ))
  
  #if(show_legend == FALSE){
    ggplot(tree_unsh_data, aes(area = Count, fill = HomelessnessType,
                                label = label, subgroup = border_color) )+
      labs(title = paste0("Total ",
                          unsh_level_of_detail_text(), " in ",
                          str_remove(getNameByValue(sys_hh_types, input$unsh_hh_type), "- "),
                          if_else(getNameByValue(sys_hh_types, input$unsh_hh_type) == "All Household Types", "", " Households"), 
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

output$unsh_demog_chart <- renderPlot({
  
  req(
    !is.null(input$unsh_demog_selections) &
      session$userData$valid_file() == 1 &
      between(length(input$unsh_demog_selections), 1, 2)
  )
  
  validate(
    need(
      fnrow(session$userData$enrollment_categories) > 0,
      no_valid_data_msg
    )
  )
  
  demog_unsh_data <- unsh_client_categories_filtered() %>% 
    ## universe for this chart is clients with Unsheltered or Both enrollments
    fsubset(!is.na(HomelessnessType) & !(HomelessnessType %in% c('Sheltered','PH Only'))) #%>% 
   
  if(length(input$unsh_demog_selections) == 1) {
    sys_comp_plot_1var(subtab = 'unsh', 
                       methodology_type = input$unsh_methodology_type, 
                       selection = input$unsh_demog_selections, 
                       people_univ = demog_unsh_data,
                       isExport = FALSE)
  } else {
    sys_comp_plot_2vars(subtab = 'unsh', 
                        methodology_type = input$unsh_methodology_type, 
                        selections = input$unsh_demog_selections, 
                        people_univ = demog_unsh_data,
                        isExport = FALSE)
    
  }
}, height = function() {
  ifelse(!is.null(input$unsh_demog_selections), 700, 100)
}, width = function() {
  input$unsh_demog_subtabs
  input$unsh_tabbox
  input$pageid
  if (length(input$unsh_demog_selections) == 1 |
      isTRUE(getOption("shiny.testmode"))) {
    500
  } else {
    "auto"
  }
}, alt = "A crosstab data table of the demographic make-up of the homeless system.")

output$unsh_demog_filter_selections <-renderUI({ 
  
  req(session$userData$valid_file() == 1 )
  
  sys_detailBox(
    detail_type = 'unsh',
    methodology_type = input$unsh_methodology_type,
    cur_project_types = input$unsh_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$unsh_age,
    spec_pops = input$unsh_spec_pops,
    race_eth = input$unsh_race_ethnicity
  )
  
})
