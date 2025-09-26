

#### DISPLAY FILTER SELECTIONS ###
syse_detailBox <- reactive({
  
  sys_detailBox(
    all_filters = TRUE,
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
    )
})




output$syse_compare_subpop_filter_selections <- 
  output$syse_types_filter_selections <- renderUI({ 
    req(session$userData$valid_file() == 1)
    syse_detailBox()
  })

## separate info for time chart tab since report period covers 2 years before ReportEnd
output$syse_compare_time_filter_selections <- renderUI({
  req(session$userData$valid_file() == 1)
 
  syse_time_detailBox(
    all_filters = TRUE,
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
  )
})

sys_phd_selections_info <- reactive({
  sys_perf_selection_info(type = 'exits', selection = input$syse_phd_selections)
})

output$syse_phd_summary_selections <- renderUI({
  req(!is.null(input$syse_phd_selections) & session$userData$valid_file() == 1)
  sys_detailBox(selection = input$syse_phd_selections,
                all_filters = FALSE,
                methodology_type = input$syse_methodology_type,
                cur_project_types = input$syse_project_type,
                startDate = session$userData$ReportStart,
                endDate = session$userData$ReportEnd)
})
  

sys_phd_selections_summary <- function() {
  return(
    sys_export_summary_initial_df(type = 'exits') %>%
      bind_rows(sys_perf_selection_info(type = 'exits', selection = input$syse_phd_selections)) %>%
      rename("System Demographics" = Value)
  )
}

syse_level_of_detail_text <- reactive({
  case_when(
    input$syse_level_of_detail == "All" ~ "People",
    input$syse_level_of_detail == "HoHsOnly" ~ "Heads of Household",
    TRUE ~
      getNameByValue(sys_level_of_detail, input$syse_level_of_detail)
  )
})

output$syse_types_ui_chart <- renderPlot({
  
  syse_types_chart("Destination Type", input$syse_dest_type_filter)
})

tree_exits_data <- reactive({
  all_filtered_syse()  %>% 
    fselect( Destination, PersonalID, EnrollmentID) %>% 
    fmutate(`Destination Type` = fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    )) 
})

syse_types_chart <- function(varname, status, show_legend = FALSE){
  
  tree_colors <- c(
    "Permanent" = "#16697A",
    "Homeless" = "#C2462E",
    "Institutional" = "#CCC7C4",
    "Temporary" = "#CCC7C4",
    "Other/Unknown" = "#CCC7C4"
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
           label = str_c(`Destination Type`, ':\n', scales::label_comma()(Count),
                         ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
           )) %>% 
    fmutate(border_color = "black")
  
    if(show_legend == FALSE){
      ggplot(tree_exits_summ, aes(area = Count, fill = `Destination Type`,
                                  label = label, subgroup = border_color)) +
        labs(title = paste0(scales::label_comma()(nr), " System Exits for ",
                            syse_level_of_detail_text(), " in ",
                            str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
                            if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"),"\n")
        ) +
        geom_treemap(start = "left",color = "black", size = 2, show.legend = FALSE) +
        geom_treemap_text(aes(color = text_color), start = "left", place = "center", grow = FALSE) +
        geom_treemap_subgroup_border(color = "black", size = 4, show.legend = FALSE) +
        scale_color_identity() +
        scale_fill_manual(values = tree_colors) +
        theme_minimal() +
        coord_fixed(ratio =0.8) +
        theme(
          plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
        )
  
    } else if (show_legend == TRUE){
    ggplot(tree_exits_summ, aes(area = Count, fill = label,
                                label = label, subgroup = border_color)) +
      labs(title = paste0(scales::label_comma()(nr), " System Exits for ", 
                          syse_level_of_detail_text(), " in ", 
                          str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "), 
                          if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"))
      ) +
      geom_treemap(start = "left", color = "black", size = 2, show.legend = TRUE) +
      geom_treemap_text(aes(color = text_color), start = "left", place = "center", grow = FALSE) +
      geom_treemap_subgroup_border(color = "black", size = 4, show.legend = TRUE) +
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


output$syse_types_download_btn <- downloadHandler( filename = date_stamped_filename("System Exit Types Report - "),
                                                   content = function(file) 
    {
     logToConsole(session, "System Exit Types data download")
     
     write_xlsx(
       list(
         "ExitsByType Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
           bind_rows(
             sys_export_filter_selections(type = 'exits'),
              data.frame(Chart = 'Total System Exits', Value = scales::label_comma()(nrow(tree_exits_data())))              
           ) %>% 
           rename('System Exits by Type' = Value),
         
         "SystemExitData" = tree_exits_data() %>% 
           mutate(`Destination Type Detail` = living_situation(Destination)) %>% 
           group_by(`Destination Type`,`Destination Type Detail`) %>% 
           summarize(Count = n()) %>% 
           ungroup() %>% 
           list_all_destinations(fill_zero = TRUE) %>% 
           mutate(Percent = scales::label_percent(accuracy = 0.1,scale=100)(Count / sum(Count)))
       ),
       path = file,
       format_headers = FALSE,
       col_names = TRUE
     )        
     
})

output$syse_types_download_btn_ppt <- downloadHandler(filename = function() {
  paste("System Exit Types_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    logToConsole(session, "In syse_types_download_btn_ppt")
  
    sys_perf_ppt_export(file = file, 
                         type = 'exits',
                       title_slide_title = "System Exits by Type",
                       summary_items = sys_export_summary_initial_df(type = 'exits') %>%
                         filter(Chart != "Start Date" & Chart != "End Date") %>% 
                         bind_rows(sys_export_filter_selections(type = 'exits'),
                                   data.frame(Chart="Total System Exits", Value = scales::label_comma()(nrow(tree_exits_data())))),
                       plots = list("System Exits by Type" = syse_types_chart("Destination Type", input$syse_dest_type_filter)),
                       summary_font_size = 19,
                       startDate = session$userData$ReportStart, 
                       endDate = session$userData$ReportEnd, 
                       sourceID = session$userData$Export$SourceID,
                       in_demo_mode = input$in_demo_mode
                       )
})


# System Exit Comparisons  ------------------------------------------------

subpop_chart_validation <- function(raceeth, vetstatus, age, show = TRUE, req = FALSE) {
  logToConsole(session, "In subpop_chart_validation")
 
  cond <- raceeth != "All" | vetstatus != "None" | length(age) != length(sys_age_cats)
  
  ## whether to show validate message or not
  if(show){
    validate(
      need(
        cond,#"All Ages",
        message = "Please select one or more demographic filters to generate the subpopulation chart and table."
      )
    )
  } else if (req){
    ##  just hide but do not show a duplicate validate message
    req(cond)
  } else {
    ## otherwise, just return TRUE/VALSE of condition
    return(cond)
  }
}
 
time_chart_validation <- function(startDate, endDate, raceeth, vetstatus, age, show = TRUE) {
  logToConsole(session, "In time_chart_validation")
  
  cond <- interval(startDate, endDate) > years(2)
  #cond <- raceeth != "All" | vetstatus != "None" | length(age) != length(sys_age_cats)
  
  ## whether to show validate message or not
  if(show){
    validate(
      need(
        cond,
        message = "Data will not be shown for reporting periods of less than 2 years."
      )
    )
  } else {
    ## otherwise, just hide but do not show a duplicate validate message
    req(cond)
  }
  
}

syse_subpop_export <- reactive({
  ## compute subcategories of destination types for data export - not shown in chart or table
  pct_subpop_sub <- tree_exits_data() %>% 
    mutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    group_by(`Destination Type`, `Destination Type Detail`) %>% 
    summarize(count_subpop = n()) %>% 
    ungroup() %>% 
    mutate(pct_subpop = count_subpop /sum(count_subpop, na.rm=T))
  
  pct_comparison_sub <- everyone_else() %>% 
    mutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    group_by(`Destination Type`, `Destination Type Detail`) %>% 
    summarize(count_comparison = n()) %>% 
    ungroup() %>% 
    mutate(pct_comparison = count_comparison/sum(count_comparison, na.rm=T))
 
  pct_subpop_totals <- pct_subpop_sub %>% 
    group_by(`Destination Type`) %>% 
    summarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
              count_subpop = sum(count_subpop), 
              pct_subpop = sum(pct_subpop)) 
  
  pct_comparison_totals <- pct_comparison_sub %>% 
    group_by(`Destination Type`) %>% 
    summarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
              count_comparison = sum(count_comparison), 
              pct_comparison = sum(pct_comparison)) 

  #full_join(pct_prev_year_sub, pct_current_year_sub) %>% 
  full_join(pct_subpop_sub %>% 
              bind_rows(pct_subpop_totals), 
            pct_comparison_sub %>% 
              bind_rows(pct_comparison_totals), 
            by=c('Destination Type','Destination Type Detail')) %>%    
    list_all_destinations(fill_zero = TRUE, add_totals = TRUE) %>% 
      mutate(pct_diff =  scales::percent(pct_subpop - pct_comparison,accuracy = 0.1, scale = 100),
             total_count = count_subpop + count_comparison,
             pct_comparison = scales::percent(pct_comparison, accuracy = 0.1,scale=100),
             pct_subpop = scales::percent(pct_subpop, accuracy = 0.1,scale=100)) %>% 
      select(`Destination Type`, `Destination Type Detail`, 'Subpopulation %' = pct_subpop, 'Comparison Group %' = pct_comparison, 
             'Percent Difference' = pct_diff, 'Subpopulation Count' = count_subpop, 'Comparison Group Count' = count_comparison, 'Total Count' = total_count)
})

everyone_else <- reactive({
  all_unfiltered_syse() %>% 
    roworder(PersonalID, EntryDate, ExitAdjust) %>%
    fgroup_by(PersonalID) %>%
    fmutate(
      # Days_to_lookahead is simpler because if they have ANY enrollment <= 14 days ahead
      # then it was clearly not a system exit
      days_to_lookahead = L(EntryDate, n=-1) - ExitAdjust
    ) %>%
    fungroup() %>% 
    fsubset(days_to_lookahead > 14 ) %>% 
    ## drop rows that are in the filtered version - (everyone minus subpop)
    fsubset(!(EnrollmentID %in% all_filtered_syse()$EnrollmentID)) %>% 
    fmutate(`Destination Type` = fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    )) %>% 
    fmutate(
      `Destination Type` = factor(`Destination Type`, levels = c('Permanent','Homeless','Institutional','Temporary','Other/Unknown'))
    )
  
})

get_syse_compare_subpop_data <- reactive({
  
  validate(need(nrow(all_filtered_syse()) > 0, no_data_msg))
  validate(need(nrow(all_filtered_syse()) > 10, suppression_msg))
  
  
  pct_subpop <- tree_exits_data() %>% summarize(
    'Permanent' = mean(`Destination Type` == 'Permanent'),
    'Homeless'= mean(`Destination Type` == 'Homeless'),
    'Institutional' = mean(`Destination Type` == 'Institutional'),
    'Temporary' = mean(`Destination Type` == 'Temporary'),
    'Other/Unknown' = mean(`Destination Type` == 'Other/Unknown')
  )
  
  pct_everyone_else <- everyone_else() %>% summarize(
    'Permanent' = mean(`Destination Type` == 'Permanent'),
    'Homeless' = mean(`Destination Type` == 'Homeless'),
    'Institutional' = mean(`Destination Type` == 'Institutional'),
    'Temporary' = mean(`Destination Type` == 'Temporary'),
    'Other/Unknown' = mean(`Destination Type` == 'Other/Unknown')
  )
  
  tibble(
    subpop_summ = c("Subpopulation","Comparison Group","Percent Difference"),
  round(
    rowbind(
      pct_subpop,
      pct_everyone_else,
      pct_subpop - pct_everyone_else
    ),
  2)
  )

})

syse_time_export <- reactive({
  
  prev_year <- everyone() %>% 
    fsubset(period == 'Previous Year')
  
  current_year <- everyone() %>% 
    fsubset(period == 'Current Year')
  
  ## compute subcategories of destination types for data export - not shown in chart or table
  pct_prev_year_sub <- prev_year %>% 
    mutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    group_by(`Destination Type`, `Destination Type Detail`) %>% 
    summarize(count_prev_year = n()) %>% 
    ungroup() %>% 
    mutate(pct_prev_year = count_prev_year/sum(count_prev_year, na.rm=T))
  
  pct_current_year_sub <- current_year %>% 
    mutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    group_by(`Destination Type`, `Destination Type Detail`) %>% 
    summarize(count_cur_year = n()) %>% 
    ungroup() %>% 
    mutate(pct_cur_year = count_cur_year/sum(count_cur_year, na.rm=T))

 
 pct_prev_year_totals <- pct_prev_year_sub %>% 
   group_by(`Destination Type`) %>% 
   summarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
             count_prev_year = sum(count_prev_year), 
             pct_prev_year = sum(pct_prev_year)) 
 
 pct_current_year_totals <- pct_current_year_sub %>% 
   group_by(`Destination Type`) %>% 
   summarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
             count_cur_year = sum(count_cur_year), 
             pct_cur_year = sum(pct_cur_year)) 
 
    #full_join(pct_prev_year_sub, pct_current_year_sub) %>% 
 full_join(pct_prev_year_sub %>% 
             bind_rows(pct_prev_year_totals), 
           pct_current_year_sub %>% 
             bind_rows(pct_current_year_totals), 
           by=c('Destination Type','Destination Type Detail')) %>%    
      list_all_destinations(fill_zero = TRUE, add_totals = TRUE) %>% 
      mutate(pct_change = scales::percent(pct_cur_year - pct_prev_year, accuracy=0.1, scale=100),
             pct_cur_year = scales::percent(pct_cur_year, accuracy=0.1, scale=100), 
             pct_prev_year = scales::percent(pct_prev_year, accuracy=0.1, scale=100)) %>% 
      select(`Destination Type`, `Destination Type Detail`, 'Previous Year %' = pct_prev_year, 'Current Year %' = pct_cur_year, 
             'Percent Change' = pct_change, 'Previous Year Count' = count_prev_year, 'Current Year Count' = count_cur_year)
  
})

everyone <- reactive({
  all_filtered_syse_time() %>% 
    fmutate(`Destination Type` = fcase(
      Destination %in% perm_livingsituation, 'Permanent',
      Destination %in% 100:199, 'Homeless',
      Destination %in% temp_livingsituation, 'Temporary',
      Destination %in% institutional_livingsituation, 'Institutional',
      Destination %in% other_livingsituation, 'Other/Unknown',
      default = 'Other/Unknown'
    )) %>% 
    fmutate(
      `Destination Type` = factor(`Destination Type`, levels = c('Permanent','Homeless','Institutional','Temporary','Other/Unknown'))
    )
})

get_syse_compare_time_data <- reactive({
  
  validate(need(nrow(all_filtered_syse_time()) > 0, no_data_msg))
  validate(need(nrow(all_filtered_syse_time()) > 10, suppression_msg))
  
  prev_year <- everyone() %>% 
    fsubset(period == 'Previous Year')

  current_year <- everyone() %>% 
    fsubset(period == 'Current Year')
    
  pct_prev_year <- prev_year %>% summarize(
    'Permanent' = mean(`Destination Type` == 'Permanent'),
    'Homeless'= mean(`Destination Type` == 'Homeless'),
    'Institutional' = mean(`Destination Type` == 'Institutional'),
    'Temporary' = mean(`Destination Type` == 'Temporary'),
    'Other/Unknown' = mean(`Destination Type` == 'Other/Unknown')
  )
  
  pct_current_year <- current_year %>% summarize(
    'Permanent' = mean(`Destination Type` == 'Permanent'),
    'Homeless' = mean(`Destination Type` == 'Homeless'),
    'Institutional' = mean(`Destination Type` == 'Institutional'),
    'Temporary' = mean(`Destination Type` == 'Temporary'),
    'Other/Unknown' = mean(`Destination Type` == 'Other/Unknown')
  )
  
  tibble(
    time_summ = c("Current Year","Previous Year","Percent Change"),
    round(rowbind(
      pct_current_year,
      pct_prev_year,
      pct_current_year - pct_prev_year
    ),2)
  )
  
})

## function to make System Exits comparison subpopulation chart
syse_compare_subpop_chart <- function(subpop, isExport = FALSE){
  
  subgroup_colors <- c(
   "Subpopulation" = "#FCB248",
   "Comparison Group" = "#9E958F"
  )
  
  ## long format needed for plotting points
  subpop_chart_df <- get_syse_compare_subpop_data() %>% 
    filter(subpop_summ != "Percent Difference") %>% 
    pivot_longer(cols = -1, names_to = 'dest_type', values_to = 'subpop_pct') %>% 
    mutate(dest_type = factor(dest_type, levels = c("Permanent","Homeless","Institutional","Temporary","Other/Unknown")) )
  
  ## wide format needed for plotting arrows between points
  subpop_segment_df <- subpop_chart_df %>% 
    pivot_wider(names_from = 'subpop_summ', values_from = 'subpop_pct')
 
  g <- ggplot(subpop_chart_df, aes(x = dest_type, y = subpop_pct)) +
    geom_point(aes(fill = subpop_summ), size = 10, shape = 21, color = 'black') +
    geom_segment(data=subpop_segment_df,
                 aes(x = dest_type, xend = dest_type, y = `Comparison Group`, yend = Subpopulation),
                 arrow = arrow(length = unit(0.125, "inches")), color = '#948A84', linewidth = 1.5) +
    scale_fill_manual(values=subgroup_colors, guide = guide_legend(ncol = 2)) +
    scale_y_continuous(limits=c(0,NA), labels = scales::label_percent()) +
    scale_x_discrete(expand = expansion(mult = 0.03, add = 0)) +
    labs(x = '', y = '') +
    theme_minimal() +
    theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill=NA, colour = 'black'),
          legend.title = element_blank(),
          legend.justification = 'left',
          legend.position = 'top',
          legend.text = element_text(size = get_adj_font_size(sys_legend_text_font, isExport)),
          axis.text.y = element_text(size = sys_axis_text_font)
        )
  if(isExport){
    g + theme(
      axis.text.x = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
    )
  } else {
    g + theme(axis.text.x = element_blank())
  }
}
  
## function for System Exits Comparison subpopulation table (below chart)
get_syse_compare_subpop_table <- function(tab){
  
  subgroup_colors <- c(
    "Subpopulation" = "#FCB248",
    "Comparison Group" = "#9E958F"
  )
  
  datatable(tab, 
            colnames = c(' ' = 'subpop_summ',
                         "<b>Permanent</b>" = "Permanent","<b>Homeless</b>" = "Homeless",
                         "<b>Institutional</b>" = "Institutional","<b>Temporary</b>" = "Temporary",
                        "<b>Other/Unknown</b>" = "Other/Unknown"),
            options = list(
    dom = 't',
    ordering = FALSE,
    columnDefs = list(
      list(width = "48px", targets = 0), # Set first column width
      list(className = 'dt-center', targets = '_all') # Center text
    )
  ),
  escape = FALSE,
  style = "default",
  rownames = FALSE) %>% DT::formatPercentage(
    columns = -1 
  ) %>% 
    # Highlight only the first column of "Subpopulation" and "Comparison Group" rows
    formatStyle(
      columns = 1,  # First column
      target = "cell",
      backgroundColor = styleEqual(
        names(subgroup_colors), unname(subgroup_colors)
      ),
      border = styleEqual(
        names(subgroup_colors),
        c(rep("2px solid black", 2))
      )
    ) %>% 
    # Contrast font and background colors
    formatStyle(
      columns = 1,
      target = "cell",
      color = styleEqual(
        names(subgroup_colors), 
        rep("black", length(subgroup_colors))
      )
    )
}

get_syse_compare_subpop_flextable <- function(tab) {
  logToConsole(session, "In get_syse_compare_subpop_flextable")
 
  
  ft <- flextable(tab %>%
                    frename("subpop_summ" = " ")) %>%
    width(j = 1, width = 0.9) %>% # make first col narrower
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    border(border.top = fp_border(), part = "header") %>%
    border_inner_h(border = fp_border(color = "grey", width = 0.5), part = "body")
  
  ## formatting function for percentages with 0 decimal places and % sign
  fmt_func_pct <- function(x){sprintf("%.0f%%", x*100)}
  
  ft <- set_formatter(
    x = ft,
    Permanent = fmt_func_pct,
    Homeless = fmt_func_pct,
    Institutional = fmt_func_pct,
    Temporary = fmt_func_pct,
    `Other/Unknown` = fmt_func_pct
  )
  
  row_labels <- tab[[1]]
  
  # Formatting the subpopulation row labels
  subgroup_colors <- c(
    "Subpopulation" = "#FCB248",
    "Comparison Group" = "#9E958F"
  )
  
  ft <- ft %>%
    # Background colors from datatable's formatStyle
    bg(i = 1:2, j = 1, bg = subgroup_colors) %>%
    # thick borders for the first column
    border(i = 1:3, j = 1, border = fp_border(color = "black", width = 2)) %>% 
    # expand to better fit slide width
    autofit()
  
  ft
  
}

get_syse_compare_time_flextable <- function(tab) {
  logToConsole(session, "In get_syse_compare_time_flextable")
  
  
  ft <- flextable(tab%>%
                    frename("time_summ" = " ")) %>%
    width(j = 1, width = 0.9) %>% # make first col narrower
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    border(border.top = fp_border(), part = "header") %>%
    border_inner_h(border = fp_border(color = "grey", width = 0.5), part = "body")
  
  ## formatting function for percentages with 0 decimal places and % sign
  fmt_func_pct <- function(x) sprintf("%.0f%%", x*100)
  
  ft <- set_formatter(
    x = ft,
    Permanent = fmt_func_pct,
    Homeless = fmt_func_pct,
    Institutional = fmt_func_pct,
    Temporary = fmt_func_pct,
    `Other/Unknown` = fmt_func_pct
  )
  
  row_labels <- tab[[1]]
  
  ## formatting the time row labels
  time_colors <- c(
    "Current Year" = "#72B4CD",
    "Previous Year" = "#16697A"
  )
  
  ft <- ft %>%
    # Background colors from datatable's formatStyle
    bg(i = 1:2, j = 1, bg = time_colors) %>%
    # thick borders for the first column
    border(i = 1:3, j = 1, border = fp_border(color = "black", width = 2)) %>% 
    # expand to better fit slide width
    autofit()
    
  
  ft
  
}

output$syse_compare_subpop_chart <- renderPlot({
  ## check if filters have been changed from defaults before showing 
  subpop_chart_validation(input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age, show=TRUE, req=FALSE)
  syse_compare_subpop_chart(subpop = input$syse_race_ethnicity)
})

output$syse_compare_subpop_table <- renderDT({
  ## check if filters have been changed from defaults before showing 
  subpop_chart_validation(input$syse_race_ethnicity,input$syse_spec_pops,input$syse_age, show = FALSE, req=TRUE)
  get_syse_compare_subpop_table(
    get_syse_compare_subpop_data()
  )
})


## function to make System Exits comparison subpopulation chart
syse_compare_time_chart <- function( isExport = FALSE){
  
  time_colors <- c(
    "Current Year" = "#72B4CD",
    "Previous Year" = "#16697A"
  )
  
  ## long format needed for plotting points
  time_chart_df <- get_syse_compare_time_data() %>% 
    filter(time_summ != "Percent Change") %>% 
    pivot_longer(cols = -1, names_to = 'dest_type', values_to = 'time_pct') %>% 
    mutate(dest_type = factor(dest_type, levels = c("Permanent","Homeless","Institutional","Temporary","Other/Unknown")) )
  
  ## wide format needed for plotting arrows between points
  time_segment_df <- time_chart_df %>% 
    pivot_wider(names_from = 'time_summ', values_from = 'time_pct')
  
  g <- ggplot(time_chart_df, aes(x = dest_type, y = time_pct, color = time_summ)) +
    geom_point(size = 5) +
    geom_segment(data=time_segment_df,
                 aes(x = dest_type, xend = dest_type, y = `Previous Year`, yend = `Current Year`),
                 arrow = arrow(length = unit(0.125, "inches")), color = '#948A84') +
    scale_color_manual(values=time_colors,guide =  guide_legend(ncol = 2)) +
    scale_y_continuous(limits=c(0,NA), labels = scales::label_percent()) +
    scale_x_discrete(expand = expansion(mult = 0.03, add = 0)) +
    labs(x = '', y = '') +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill=NA, colour = 'black'),
      legend.title = element_blank(),
      legend.justification = 'left',
      legend.position = 'top',
      legend.text = element_text(size = get_adj_font_size(sys_legend_text_font, isExport)),
      axis.text.y = element_text(size = sys_axis_text_font),
    )
  if(isExport){
    g + theme(
          axis.text.x = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
        )
    } else {
      g + theme(axis.text.x = element_blank())
    }
}

## function for System Exits Comparison subpopulation table (below chart)
get_syse_compare_time_table <- function(tab){
  
  time_colors <- c(
    "Current Year" = "#72B4CD",
    "Previous Year" = "#16697A"
  )
  
  datatable(tab, 
            colnames = c(' ' = 'time_summ',
                         "<b>Permanent</b>" = "Permanent","<b>Homeless</b>" = "Homeless",
                         "<b>Institutional</b>" = "Institutional","<b>Temporary</b>" = "Temporary",
                         "<b>Other/Unknown</b>" = "Other/Unknown"),
            options = list(
              dom = 't',
              ordering = FALSE,
              columnDefs = list(
                list(width = "90px", targets = 0), # Set first column width
                list(className = 'dt-center', targets = '_all') # Center text
              )
            ),
            escape = FALSE,
            style = "default",
            rownames = FALSE) %>% DT::formatPercentage(
              columns = -1 
            ) %>% 
    # Highlight only the first column of "Current Year" and "Previous Year" rows
    formatStyle(
      columns = 1,  # First column
      target = "cell",
      backgroundColor = styleEqual(
        names(time_colors), unname(time_colors)
      ),
      border = styleEqual(
        names(time_colors),
        c(rep("2px solid black", 2))
      )
    ) %>% 
    # Contrast font and background colors
    formatStyle(
      columns = 1,
      target = "cell",
      color = styleEqual(
        names(time_colors), 
        rep("white", length(time_colors))
      )
    )
  
  
}

output$syse_compare_time_chart <- renderPlot({
  time_chart_validation(startDate = session$userData$meta_HUDCSV_Export_Start, endDate = session$userData$meta_HUDCSV_Export_End,
                        input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age,
                        show = TRUE)
  syse_compare_time_chart()
})

output$syse_compare_time_table <- renderDT({
  # time_chart_validation(startDate = session$userData$ReportStart, endDate = session$userData$ReportEnd,
  #                       input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age,
  #                       show = FALSE)
  get_syse_compare_time_table(
    get_syse_compare_time_data()
  )
})

output$syse_compare_download_btn <- downloadHandler(filename = date_stamped_filename("System Exit Comparisons Report - "),
                                                    content = function(file) {
      logToConsole(session, "System Exit Comparisons data download")
  
    if(subpop_chart_validation(input$syse_race_ethnicity,input$syse_spec_pops,input$syse_age, show = FALSE, req = FALSE)){
      sheets <- list(
        "SystemExitsTimeMetadata" = sys_export_summary_initial_df(type = 'exits_time') %>%
          bind_rows(
            sys_export_filter_selections(type = 'exits')
          ) %>% 
          bind_rows(
            data.frame(Chart = c('Total Current Year System Exits', 'Total Previous Year System Exits'),
                       Value = scales::label_comma()(c(nrow(everyone() %>% fsubset(period == 'Current Year')),
                                              nrow(everyone() %>% fsubset(period == 'Previous Year')))
                       )
            )
          ) %>% 
          rename("System Exit Comparisons: Time" = Value),
        "Time" = syse_time_export(),
        "SystemExitsSubpopMetadata" = sys_export_summary_initial_df(type = 'exits') %>%
          bind_rows(
            sys_export_filter_selections(type = 'exits_subpop'),
            data.frame(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Comparison Group'),
                       Value = scales::label_comma()(c(nrow(tree_exits_data()),nrow(everyone_else())))
            )
          ) %>% 
          rename("System Exit Comparisons: Subpopulation" = Value),
        "Subpopulation" = syse_subpop_export()
      )
    } else {
      sheets <- list(
        "SystemExitsTimeMetadata" = sys_export_summary_initial_df(type = 'exits_time') %>%
          bind_rows(
            sys_export_filter_selections(type = 'exits')
          ) %>% 
          bind_rows(
            data.frame(Chart = c('Total Current Year System Exits', 'Total Previous Year System Exits'),
                       Value = scales::label_comma()(c(nrow(everyone() %>% fsubset(period == 'Current Year')),
                                                       nrow(everyone() %>% fsubset(period == 'Previous Year')))
                       )
            )
          ) %>% 
          rename("System Exit Comparisons: Time" = Value),
        "Time" = syse_time_export()
      )
    }
                                                    
    write_xlsx(
        sheets,     
        path = file,
        format_headers = FALSE,
        col_names = TRUE
      )        
})

## hide demographic filters when on PHD subtab
observeEvent(input$syse_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  if(input$syse_tabbox == '<h4>Permanent Housing Demographics</h4>'){
    shinyjs::hide('syse_spec_pops')
    shinyjs::hide('syse_age')
    shinyjs::hide('syse_race_ethnicity')
  } else {
    shinyjs::show('syse_spec_pops')
    shinyjs::show('syse_age')
    shinyjs::show('syse_race_ethnicity')
  }
 
})

# Client-level flags, filtered ----------------------------------------------------
syse_client_categories_filtered <- reactive({
  
  logToConsole(session, "In syse_client_categories_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  req(nrow(session$userData$client_categories) > 0)
  
  session$userData$client_categories[
    AgeCategory %in% input$syse_age &
      (if(input$syse_race_ethnicity == "All") rep(TRUE, .N) else get(input$syse_race_ethnicity) == 1) & 
      (
        input$syse_spec_pops == "None" |
          (input$syse_spec_pops == "Veteran" &
             VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
          (input$syse_spec_pops == "NonVeteran" &
             VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17"))
      )
  ]
})

all_unfiltered_syse <- reactiveVal(NULL)

# Create passes-enrollment-filter flag to exclude enrollments from heatmap -------
enrollments_filtered_syse <- reactive({
  logToConsole(session, "in enrollments_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  en_unfilt <-  join(
    session$userData$enrollment_categories,
    session$userData$client_categories %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  )
  all_unfiltered_syse(en_unfilt)
  
  en_filt <- en_unfilt %>%
    fmutate(
      passes_enrollment_filters =
        # Household type filter
        (input$syse_hh_type == "All" |
           (input$syse_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (input$syse_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (input$syse_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (input$syse_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           input$syse_hh_type == HouseholdType
        ) &
        # Level of detail filter
        (input$syse_level_of_detail == "All" |
           (input$syse_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (input$syse_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        # Project type filter
        (input$syse_project_type == "All" |
           (input$syse_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
           (input$syse_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
           (input$syse_project_type == "SO" & ProjectType == out_project_type) |
           (input$syse_project_type == "AllNonRes" & ProjectType %in% non_res_project_types)
        )
    ) %>%
    fselect(-VeteranStatus)
  
  en_filt %>% 
    fsubset(passes_enrollment_filters)
  #   roworder(PersonalID, EntryDate, ExitAdjust) %>%
  #   fgroup_by(PersonalID) %>%
  #   fmutate(
  #     # Days_to_lookahead is simpler because if they have ANY enrollment <= 14 days ahead
  #     # then it was clearly not a system exit
  #     days_to_lookahead = L(EntryDate, n=-1) - ExitAdjust
  #   ) %>%
  #   flast() %>% 
  #   fungroup() %>% 
  #   fsubset(is.na(days_to_lookahead ) | days_to_lookahead > 14) %>% 
   
  
})

all_filtered_syse <- reactive({
  logToConsole(session, "in all_filtered_syse")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  
  tmp <- enrollments_filtered_syse() %>%
    get_lookbacks() %>% 
    get_days_since_last_lh()
 
   eecr_and_lecr <- tmp %>% 
    expand_by_periods_syse(time_chart = FALSE) %>% 
    get_was_lh_info(tmp) %>%
    get_syse_eecr_and_lecr()
    
  join( 
    eecr_and_lecr,
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  ) %>% 
    fmutate(
      continuous_at_end = lecr & 
        endDate < session$userData$ReportEnd &
        ExitAdjust <= endDate & days_to_lookahead %between% c(0, 14),
      exited_system = lecr &
        ExitAdjust %between% list(startDate, endDate) &
        (!continuous_at_end | is.na(continuous_at_end))
    ) %>% 
    fsubset(period == 'Full' & exited_system) 
  
})

all_filtered_syse_time <- reactive({
  logToConsole(session, "in all_filtered_syse_time")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode)) 
  
  tmp <- enrollments_filtered_syse() %>%
    get_lookbacks() %>% 
    get_days_since_last_lh()
  
  eecr_and_lecr <- tmp %>% 
    expand_by_periods_syse(time_chart = TRUE) %>% 
    get_was_lh_info_time(tmp) %>%
    get_syse_eecr_and_lecr()
  
  join( 
    eecr_and_lecr,
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  ) %>% 
    fmutate(
      continuous_at_end = lecr & 
        endDate < session$userData$ReportEnd &
        ExitAdjust <= endDate & days_to_lookahead %between% c(0, 14),
      exited_system = lecr &
        ExitAdjust %between% list(startDate - years(1), endDate) &
        (!continuous_at_end | is.na(continuous_at_end))
    ) %>% 
    fsubset(exited_system) 
 
})

all_filtered_syse_demog <- reactive({
  logToConsole(session, "in all_filtered_syse")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  tmp <- enrollments_filtered_syse() %>%
    get_lookbacks() %>% 
    get_days_since_last_lh()
  
  eecr_and_lecr <- tmp %>% 
    expand_by_periods_syse(time_chart = FALSE) %>% 
    get_was_lh_info(tmp) %>%
    get_syse_eecr_and_lecr()
  
  join( 
    eecr_and_lecr,
    session$userData$client_categories,
    on = "PersonalID",
    how = "inner"
  ) %>% 
    fmutate(
      continuous_at_end = lecr & 
        endDate < session$userData$ReportEnd &
        ExitAdjust <= endDate & days_to_lookahead %between% c(0, 14),
      exited_system = lecr &
        ExitAdjust %between% list(startDate, endDate) &
        (!continuous_at_end | is.na(continuous_at_end))
    ) %>% 
    fsubset(period == 'Full' & exited_system) 
  
})

output$syse_compare_download_btn_ppt <- downloadHandler(filename = function(){
  paste("System Exit Comparisons_", Sys.Date(), ".pptx", sep = "")
},
content = function(file) {
  logToConsole(session, "In syse_compare_download_btn_ppt")
  
  if(subpop_chart_validation(input$syse_race_ethnicity,input$syse_spec_pops,input$syse_age, show = FALSE, req = FALSE)){
    sys_perf_ppt_export(file = file, 
                        type = 'exits_comparison',
                        title_slide_title = "System Exits Comparisons",
                        summary_items = list(
                          "Summary - Time" = sys_export_summary_initial_df(type = 'exits_time') %>%
                            bind_rows(
                              sys_export_filter_selections(type = 'exits')
                            ) %>% 
                            bind_rows(
                              data.frame(Chart = c('Total Current Year System Exits', 'Total Previous Year System Exits'),
                                         Value = scales::label_comma()(c(nrow(everyone() %>% fsubset(period == 'Current Year')),
                                                                         nrow(everyone() %>% fsubset(period == 'Previous Year')))
                                         )
                              )
                            ),
                          "Summary - Subpopulation" = sys_export_summary_initial_df(type = 'exits') %>%
                            bind_rows(
                              sys_export_filter_selections(type = 'exits_subpop'),
                              data.frame(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Comparison Group'),
                                         Value = scales::label_comma()(c(nrow(tree_exits_data()),nrow(everyone_else())))
                              )
                            ) 
                        ),
                        plots = list(
                          "System Exits - Time Chart" = syse_compare_time_chart(isExport = TRUE),
                          "System Exits - Time Table" = get_syse_compare_time_flextable(
                            get_syse_compare_time_data()
                            ),
                            "System Exits - Subpopulation Chart" =  syse_compare_subpop_chart(isExport = TRUE),
                            "System Exits - Subpopulation Table" = get_syse_compare_subpop_flextable(
                              get_syse_compare_subpop_data()
                            )
                        ),
                        summary_font_size = 19,
                        startDate = session$userData$ReportStart, 
                        endDate = session$userData$ReportEnd, 
                        sourceID = session$userData$Export$SourceID,
                        in_demo_mode = input$in_demo_mode
    )
  } else {
    sys_perf_ppt_export(file = file, 
                        type = 'exits_comparison',
                        title_slide_title = "System Exits Comparisons",
                        summary_items = list(
                          "Summary - Time" = sys_export_summary_initial_df(type = 'exits_time') %>%
                            bind_rows(
                              sys_export_filter_selections(type = 'exits')
                            ) %>% 
                            bind_rows(
                              data.frame(Chart = c('Total Current Year System Exits', 'Total Previous Year System Exits'),
                                         Value = scales::label_comma()(c(nrow(everyone() %>% fsubset(period == 'Current Year')),
                                                                         nrow(everyone() %>% fsubset(period == 'Previous Year')))
                                         )
                              )
                            ) 
                          ),
                        plots = list(
                          "System Exits - Time Chart" = syse_compare_time_chart(isExport = TRUE),
                          "System Exits - Time Table" = get_syse_compare_time_flextable(
                            get_syse_compare_time_data()
                          )
                        ),
                        summary_font_size = 19,
                        startDate = session$userData$ReportStart, 
                        endDate = session$userData$ReportEnd, 
                        sourceID = session$userData$Export$SourceID,
                        in_demo_mode = input$in_demo_mode
    )
  }
 
})


# System Exits Permanent Housing Demographics (PHD) -----------------------
sys_phd_plot_df <- reactiveVal()

# output$syse_phd_chart <- renderPlot({
#   # req(
#   #   !is.null(input$syse_phd_selections) &
#   #     session$userData$valid_file() == 1 &
#   #     between(length(input$syse_phd_selections), 1, 2)
#   # )
# 
#    if( is.null(input$syse_phd_selections) |
#         session$userData$valid_file() != 1 |
#         !between(length(input$syse_phd_selections), 1, 2)){
#      validate("Please select one or more options to display the demographic chart.")
#    }
# 
#   if(length(input$syse_phd_selections) == 1) {
#     sys_phd_plot_1var(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
#   } else if(length(input$syse_phd_selections) == 2){
#     sys_phd_plot_2vars(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
#   }
# }, height = function() {
#   ifelse(!is.null(input$syse_phd_selections), 700, 100)
# }, width = function() {
#   input$syse_phd_subtabs
#   input$syse_tabbox
#   input$pageid
# 
#   if (num_selections() %in% c(0,1) |
#       isTRUE(getOption("shiny.testmode"))) {
#     500
#   } else {
#     "auto"
#   }
# 
# },
# alt = "A crosstab data table of the demographic make-up of the homeless system.")

full_unit_of_analysis_display_syse <- reactive({
  c(
  paste0(
    "Total ", 
    syse_level_of_detail_text(),
    " with System Exits",
    if_else(
      input$syse_hh_type == "All",
      "",
      paste0(" in ",
             str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
             " Households")
    )
  ),
  paste0(
    "Total ", 
    syse_level_of_detail_text(),
    " with PH System Exits",
    if_else(
      input$syse_hh_type == "All",
      "",
      paste0(" in ",
             str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
             " Households")
    )
  )
  )
})

syse_total_count_display <- function(total_count, total_ph_count) {
  #browser()
  return(paste0(
    str_wrap(
      paste0(
        full_unit_of_analysis_display_syse(),
        ": ",
        scales::comma(c(total_count, total_ph_count))
      ),
      width = 40
    ),collapse='',
    "\n")
  )
}

output$syse_phd_chart_1d <- renderPlot({

  req(session$userData$valid_file() == 1 &
        !is.null(input$syse_phd_selections) &
        length(input$syse_phd_selections) == 1)
  
    sys_phd_plot_1var(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)

}, height = 700, width = 500,
alt = "A crosstab data table of the demographic make-up of the homeless system.")

output$syse_phd_chart_2d <- renderCachedPlot({
  
  req(session$userData$valid_file() == 1 &
        !is.null(input$syse_phd_selections) &
        length(input$syse_phd_selections) == 2)
  
  sys_phd_plot_2vars(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
  
}, cacheKeyExpr = {
  list(
    input$syse_phd_selections,
    input$syse_hh_type,
    input$syse_level_of_detail,
    input$syse_project_type,
    input$syse_methodology_type
  )
}, alt = "A crosstab data table of the demographic make-up of the homeless system.")


observeEvent(input$syse_phd_selections, {
  # they can select up to 2
  #disable all unchecked boxes if they've already selected 2
  shinyjs::runjs(str_glue("
    var numSelected = {length(input$syse_phd_selections)};
    $('input[name=syse_phd_selections]:not(\":checked\")')
      .attr('disabled', numSelected == 2);

    var reSelected = \"{
      \"All Races/Ethnicities\" %in% input$syse_phd_selections |
      \"Grouped Races/Ethnicities\" %in% input$syse_phd_selections
    }\";
    
    if(numSelected == 1)
      $('input[name=syse_phd_selections][value*=\"Races/Ethnicities\"]:not(\":checked\")')
        .attr('disabled', reSelected == 'TRUE');
    
  "))
}, ignoreNULL = FALSE)

output$syse_phd_download_btn_ppt <- downloadHandler(
  filename = function() {
    paste("System Exit Demographics_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    sys_perf_ppt_export(
      file = file,
      type = 'exits',
      title_slide_title = "System Exits Permanent Housing (PH) Demographics",
      summary_items = sys_export_summary_initial_df(type = 'exits') %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(sys_phd_selections_info()),
      plots = setNames(
        list(
          if (length(input$syse_phd_selections) == 1) {
            sys_phd_plot_1var(subtab = 'phd', 
                                  methodology_type = input$syse_methodology_type, 
                                  selection = input$syse_phd_selections, 
                                  isExport = TRUE)
          } else {
              sys_phd_plot_2vars(subtab = 'phd', 
                                   methodology_type = input$syse_methodology_type, 
                                   selection = input$syse_phd_selections, 
                                   isExport = TRUE)
          }
        ),
        ifelse(length(input$syse_phd_selections) == 1, 
               paste0(
                 "System Exits PH Destinations: ",
                 input$syse_phd_selections[1]
               ),
              paste0(
                "System Exits PH Destinations: ",
                input$syse_phd_selections[1],
                " by ",
                input$syse_phd_selections[2]
              )
          )
        ),
      summary_font_size = 28,
      startDate = session$userData$ReportStart, 
      endDate = session$userData$ReportEnd, 
      sourceID = session$userData$Export$SourceID,
      in_demo_mode = input$in_demo_mode
    )
  }
)

output$syse_phd_download_btn <- downloadHandler(
  filename = date_stamped_filename("System Exit Demographics Report - "),
  content = function(file) {
    sys_heatmap_xl_export(file, 
                          type = 'exits',
                          methodology_type = input$syse_methodology_type,
                          selections = input$syse_phd_selections,
                          plot_df = sys_phd_plot_df,
                          in_demo_mode = input$in_demo_mode)
  }
)

observeEvent(input$syse_methodology_type, {
  
  updatePickerInput(
    session, 
    "syse_race_ethnicity", 
    choices = sys_race_ethnicity_cats(input$syse_methodology_type)
  )

},
ignoreInit = TRUE)

toggle_sys_components(prefix='syse', FALSE, init=TRUE) # initially hide them
