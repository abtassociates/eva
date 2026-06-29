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

# Create passes-enrollment-filter flag to exclude enrollments from heatmap -------
enrollments_filtered_unsh <- reactive({
  logToConsole(session, "in enrollments_filtered_unsh")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  en_unfilt <-  join(
    session$userData$enrollment_categories,
    session$userData$client_categories %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  )
  
  en_filt <- en_unfilt %>%
    fmutate(
      passes_enrollment_filters =
        # Household type filter
        (input$unsh_hh_type == "All" |
           (input$unsh_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (input$unsh_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (input$unsh_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (input$unsh_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           input$unsh_hh_type == HouseholdType
        ) &
        # Level of detail filter
        (input$unsh_level_of_detail == "All" |
           (input$unsh_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (input$unsh_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        (input$unsh_project_type == 'AllNonRes' & ProjectType %in% non_res_project_types | 
            input$unsh_project_type == 'SO' & ProjectType == out_project_type)
    ) %>%
    fselect(-VeteranStatus)
  
  en_filt %>% 
    fsubset(passes_enrollment_filters)
  
})

unsh_level_of_detail_text <- reactive({
  case_when(
    input$unsh_level_of_detail == "All" ~ "People",
    input$unsh_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(sys_level_of_detail, input$unsh_level_of_detail)
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
  #browser()
  
  nr <- nrow(enrollments_filtered_unsh())
  
  validate(need(nr > 0, no_data_msg))
  validate(need(nr > 10, suppression_msg))
  
  tree_colors <- c(
    "Sheltered" = get_brand_color('dark_grey'),
    "Unsheltered" = get_brand_color('coral'),
    'Both' = get_brand_color('light_grey')
  )
  border_color <- 'black'
  browser()
  tree_unsh_data <- unsh_client_categories_filtered() %>% 
    fsubset(!is.na(HomelessnessType) & HomelessnessType != 'PH Only') %>% 
    fcount(HomelessnessType, name='Count') %>% 
    fmutate(Percent = Count/fsum(Count),
          #text_color = fifelse(`Destination Type` %in% c('Temporary','Institutional','Other/Unknown'), 'black', 'white'),
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
      scale_fill_manual(values = tree_colors) +
      theme_minimal() +
      coord_fixed(ratio =0.8) +
      theme(
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
      )
    
  # } else if (show_legend == TRUE){
  # }
  # enrollments_filtered_unsh() %>% 
  #   fmutate(AccessType = ifelse(unsheltered, 'Unsheltered', ifelse(sheltered, 'Sheltered', 'Permanent Housing'))) %>% 
  #   fcount(AccessType) %>% 
  
    # fsummarize(unsheltered = fsum(unsheltered, na.rm=T), 
    #            sheltered = fsum(sheltered, na.rm=T), 
    #            permanent_housing = fsum(permanent_housing, na.rm=T))
})
