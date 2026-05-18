
get_syse_types_chart <- function(varname, status, show_legend = FALSE){
  
  tree_colors <- c(
    "Permanent" = get_brand_color('dark_blue'),
    "Homeless" = get_brand_color('coral'),
    "Institutional" = get_brand_color('med_grey'),
    "Temporary" = get_brand_color('med_grey'),
    "Other/Unknown" = get_brand_color('med_grey')
  )
  nr <- nrow(tree_exits_data())
  
  validate(need(nr > 0, no_data_msg))
  validate(need(nr > 10, suppression_msg))
  
  tree_exits_summ <- tree_exits_data() %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(Count = GRPN(), 
               Percent = GRPN() / nr) %>% 
    fungroup() %>% 
    fmutate(Percent = Count/fsum(Count),
            text_color = fifelse(`Destination Type` %in% c('Temporary','Institutional','Other/Unknown'), 'black', 'white'),
            label = str_c(`Destination Type`, ': ', scales::label_comma()(Count),
                          ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
            )) %>% 
    fmutate(border_color = "black") %>% 
    fmutate(subgroup2 = factor(
      fifelse(`Destination Type` %in% c('Permanent','Homeless'), 'group1', 'group2'),
      levels = c('group1','group2'))
    ) %>%
    roworder('subgroup2', 'Destination Type') 
  
  if(show_legend == FALSE){
    ggplot(tree_exits_summ, aes(area = Count, fill = `Destination Type`,
                                label = label, subgroup = border_color, subgroup2 = subgroup2 ) )+
      labs(title = paste0(scales::label_comma()(nr), " System Exits for ",
                          syse_level_of_detail_text(), " in ",
                          str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
                          if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"),"\n")
      ) +
      geom_treemap(layout='squarified', start='bottomright',color = "black", size = 2, show.legend = FALSE) +
      geom_treemap_text(layout='squarified', start='bottomright',aes(color = text_color),  place = "center", grow = FALSE, reflow = TRUE) +
      geom_treemap_subgroup_border(layout='squarified',start='bottomright',color = "black", size = 4, show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_manual(values = tree_colors) +
      theme_minimal() +
      coord_fixed(ratio =0.8) +
      theme(
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
      )
    
  } else if (show_legend == TRUE){
    ggplot(tree_exits_summ, aes(area = Count, fill = `Destination Type`,
                                label = label, subgroup = border_color, subgroup2 = subgroup2 ) )+
      labs(title = paste0(scales::label_comma()(nr), " System Exits for ",
                          syse_level_of_detail_text(), " in ",
                          str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
                          if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"),"\n")
      ) +
      geom_treemap(layout='squarified', start='bottomright',color = "black", size = 2, show.legend = FALSE,) +
      geom_treemap_text(layout='squarified', start='bottomright',aes(color = text_color),  place = "center", grow = FALSE, reflow = TRUE) +
      geom_treemap_subgroup_border(layout='squarified',start='bottomright',color = "black", size = 4, show.legend = FALSE)
    scale_color_identity() +
      scale_fill_manual("",breaks = tree_exits_summ$label, values = setNames(tree_colors, tree_exits_summ$label)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
        legend.text = element_text(size = sys_chart_text_font),
        legend.position = 'bottom'
      )
  }
  
}


output$syse_types_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  
  sys_detailBox(
    detail_type = 'types',
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
  )
})

output$syse_types_ui_chart <- renderPlot({
  
  get_syse_types_chart("Destination Type", input$syse_dest_type_filter)
})

output$syse_types_download_btn <- downloadHandler( filename = date_stamped_filename("System Exits by Type Report - "), content = function(file) {
   logToConsole(session, "System Exits by Type data download")
   
   write_xlsx(
     list(
       "SystemExitsByType Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
         rowbind(
           sys_export_filter_selections(type = 'exits'),
           data.table(Chart = 'Total System Exits', Value = scales::label_comma()(nrow(tree_exits_data())))              
         ) %>% 
         frename('System Exits by Type' = Value),
       
       "SystemExitTypesData" = tree_exits_data() %>% 
         fmutate(`Destination Type Detail` = living_situation(Destination)) %>% 
         fgroup_by(`Destination Type`,`Destination Type Detail`, sort = TRUE) %>% 
         fsummarize(Count = GRPN()) %>% 
         fungroup() %>% 
         list_all_destinations(fill_zero = TRUE) %>% 
         fmutate(Percent = scales::label_percent(accuracy = 0.1,scale=100)(Count / fsum(Count)))
     ),
     path = file,
     format_headers = FALSE,
     col_names = TRUE
   )  
   
   logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
                               if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
   
})

output$syse_types_download_btn_ppt <- downloadHandler(filename = function() {
  paste("System Exits by Type_", Sys.Date(), ".pptx", sep = "")
},
content = function(file) {
  logToConsole(session, "In syse_types_download_btn_ppt")
  
  sys_perf_ppt_export(file = file, 
                      type = 'exits',
                      title_slide_title = "System Exits by Type",
                      summary_items = sys_export_summary_initial_df(type = 'exits') %>%
                        fsubset(Chart != "Start Date" & Chart != "End Date") %>% 
                        rowbind(sys_export_filter_selections(type = 'exits'),
                                data.table(Chart="Total System Exits", Value = scales::label_comma()(nrow(tree_exits_data())))),
                      plots = list("System Exits by Type" = get_syse_types_chart("Destination Type", input$syse_dest_type_filter)),
                      summary_font_size = 19,
                      startDate = session$userData$ReportStart, 
                      endDate = session$userData$ReportEnd, 
                      sourceID = session$userData$Export$SourceID,
                      in_demo_mode = input$in_demo_mode
  )
})
