
time_chart_validation <- function(startDate, endDate, raceeth, vetstatus, age, show = TRUE) {
  logToConsole(session, "In time_chart_validation")
  
  cond <- interval(startDate, endDate) >= days(729)
  
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

get_syse_compare_time_data <- function(output_type = 'table'){
  
  validate(need(nrow(all_filtered_syse_time()) > 0, no_data_msg))
  validate(need(nrow(all_filtered_syse_time()) > 10, suppression_msg))
  
  prev_year <- everyone() %>% 
    fsubset(period == 'Previous Year')
  
  current_year <- everyone() %>% 
    fsubset(period == 'Current Year')
  
  count_prev_year <- prev_year %>% fsummarize(
    'Permanent' = fsum(`Destination Type` == 'Permanent'),
    'Homeless'= fsum(`Destination Type` == 'Homeless'),
    'Institutional' = fsum(`Destination Type` == 'Institutional'),
    'Temporary' = fsum(`Destination Type` == 'Temporary'),
    'Other/Unknown' = fsum(`Destination Type` == 'Other/Unknown')
  )
  
  count_current_year <- current_year %>% fsummarize(
    'Permanent' = fsum(`Destination Type` == 'Permanent'),
    'Homeless'= fsum(`Destination Type` == 'Homeless'),
    'Institutional' = fsum(`Destination Type` == 'Institutional'),
    'Temporary' = fsum(`Destination Type` == 'Temporary'),
    'Other/Unknown' = fsum(`Destination Type` == 'Other/Unknown')
  )
  
  .total_p <- fnrow(prev_year)
  
  .total_c <- fnrow(current_year)
  
  if(output_type == 'chart'){
    pct_prev_year <- count_prev_year / .total_p
    
    pct_current_year <- count_current_year / .total_c
    
    pct_change <- map2_dfr(count_prev_year, count_current_year, .f = calc_pct_change, format='num')
    
    data.table(
      time_summ = c("Current Year","Previous Year","Percent Change"),
      round(rowbind(
        pct_current_year,
        pct_prev_year,
        pct_change
      ), 2)
    )
    
  } else if(output_type == 'table'){
    
    .total_p <- fnrow(prev_year)
    
    pct_prev_year <- count_prev_year %>% fsummarize(
      'Permanent' = format_compare_value(Permanent, .total_p),
      'Homeless'= format_compare_value(Homeless, .total_p),
      'Institutional' = format_compare_value(Institutional, .total_p),
      'Temporary' = format_compare_value(Temporary, .total_p),
      'Other/Unknown' = format_compare_value(`Other/Unknown`, .total_p)
    )
    
    .total_c <- fnrow(current_year)
    
    pct_current_year <- count_current_year %>% fsummarize(
      'Permanent' = format_compare_value(Permanent, .total_c),
      'Homeless'= format_compare_value(Homeless, .total_c),
      'Institutional' = format_compare_value(Institutional, .total_c),
      'Temporary' = format_compare_value(Temporary, .total_c),
      'Other/Unknown' = format_compare_value(`Other/Unknown`, .total_c)
    )
    
    pct_change <- map2_dfr(count_prev_year, count_current_year, .f = calc_pct_change)
    
    data.table(
      time_summ = c("Current Year","Previous Year","Percent Change"),
      rowbind(
        pct_current_year,
        pct_prev_year,
        pct_change
      )
    )
  }
  
}

syse_time_export <- reactive({
  
  prev_year <- everyone() %>% 
    fsubset(period == 'Previous Year')
  
  current_year <- everyone() %>% 
    fsubset(period == 'Current Year')
  
  ## compute subcategories of destination types for data export - not shown in chart or table
  pct_prev_year_sub <- prev_year %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_prev_year = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_prev_year = count_prev_year/fsum(count_prev_year, na.rm=T))
  
  pct_current_year_sub <- current_year %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_cur_year = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_cur_year = count_cur_year/fsum(count_cur_year, na.rm=T))
  
  
  pct_prev_year_totals <- pct_prev_year_sub %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
               count_prev_year = fsum(count_prev_year), 
               pct_prev_year = fsum(pct_prev_year)) 
  
  pct_current_year_totals <- pct_current_year_sub %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
               count_cur_year = fsum(count_cur_year), 
               pct_cur_year = fsum(pct_cur_year)) 
  
  #full_join(pct_prev_year_sub, pct_current_year_sub) %>% 
  full_join(pct_prev_year_sub %>% 
              rowbind(pct_prev_year_totals), 
            pct_current_year_sub %>% 
              rowbind(pct_current_year_totals), 
            by=c('Destination Type','Destination Type Detail')) %>%    
    list_all_destinations(fill_zero = TRUE, add_totals = TRUE) %>% 
    fmutate(pct_change = map2_chr(count_prev_year, count_cur_year, .f = calc_pct_change, accuracy = 0.1),
            pct_cur_year = scales::percent(pct_cur_year, accuracy=0.1, scale=100), 
            pct_prev_year = scales::percent(pct_prev_year, accuracy=0.1, scale=100)) %>% 
    fselect(`Destination Type`, `Destination Type Detail`, 'Previous Year %' = pct_prev_year, 'Current Year %' = pct_cur_year, 
            'Percent Change' = pct_change, 'Previous Year Count' = count_prev_year, 'Current Year Count' = count_cur_year)
  
})

## function to make System Exits time chart
get_syse_compare_time_chart <- function( isExport = FALSE){
  
  time_colors <- c(
    "Current Year" = get_brand_color('med_purple'),
    "Previous Year" = get_brand_color('med_grey2')
  )
  
  ## use adjusted locations for point placement 
  adj_x_vals <- c(0.85, 1.83, 2.87, 3.94, 5.15)
  ## long format needed for plotting points
  time_chart_df <- get_syse_compare_time_data(output_type = 'chart') %>% 
    fsubset(time_summ != "Percent Change") %>% 
    pivot_longer(cols = -1, names_to = 'dest_type', values_to = 'time_pct') %>% 
    fmutate(dest_type = factor(dest_type, levels = c("Permanent","Homeless","Institutional","Temporary","Other/Unknown")) ) %>% 
    add_column(dest_type_adj = rep(adj_x_vals, times = 2))
  
  ## wide format needed for plotting arrows between points
  time_segment_df <- time_chart_df %>% 
    pivot_wider(names_from = 'time_summ', values_from = 'time_pct')
  
  compare_export_bar_width <- 0.4
  compare_bar_width <- 0.4
  
  ## add x-axis labels for PPT download only
  if(isExport){
    dest_type_labels <- time_segment_df$dest_type
    bar_width <- compare_export_bar_width
  } else {
    dest_type_labels <- rep(NA,5)    
    bar_width <- compare_bar_width
  }
  
  title_start <- paste0(c("Total Current Year System Exits for ", "Total Previous Year System Exits for "),
                        syse_level_of_detail_text(), " in ",
                        str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "),
                        if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"))
  
  title <- paste0(title_start, 
                  c(paste0(': ', scales::label_comma()(nrow(everyone() %>% fsubset(period == 'Current Year')))),
                    paste0(': ', scales::label_comma()(nrow(everyone() %>% fsubset(period == 'Previous Year'))))),
                  collapse='\n'
  )
  
  g <- ggplot(time_chart_df, aes(x = dest_type_adj, y = time_pct )) +
    geom_bar(aes(fill = factor(time_summ, levels=c('Previous Year', 'Current Year'))), color = 'black', width = bar_width, stat = "identity", position = 'dodge') +
    scale_fill_manual(values=rev(time_colors),guide =  guide_legend(ncol = 2)) +
    scale_y_continuous(limits=c(0,NA), labels = scales::label_percent(), expand = expansion(add=0.001, mult=c(0, 0.1))) +
    scale_x_continuous(labels=dest_type_labels, breaks=adj_x_vals, limits = c(min(adj_x_vals) - 0.2, max(adj_x_vals) + 0.2)) +
    labs(x = '', y = 'Percentage of System Exits', title = title) +
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
      axis.title.y = element_text(size = sys_axis_text_font),
      plot.title = element_text(size = sys_chart_title_font, hjust =0.5)
    )
  if(isExport){
    g + theme(
      axis.text.x = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
    )
  } else {
    g + theme(axis.text.x = element_blank())
  }
}

## function for System Exits time table (below chart)
get_syse_compare_time_table <- function(tab){
  
  time_colors <- c(
    "Current Year" = get_brand_color('med_purple'),
    "Previous Year" = get_brand_color('med_grey2')
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
            selection = 'none',
            escape = FALSE,
            style = "default",
            rownames = FALSE) %>% 
    # Highlight only the first column of "Current Year" and "Previous Year" rows
    formatStyle(
      columns = 1,  # First column
      target = "cell",
      backgroundColor = styleEqual(
        names(time_colors), unname(time_colors)
      ),
      borderTop = styleEqual(
        names(time_colors),
        c("2px solid black", "1px solid black")
      ),
      borderLeft = styleEqual(
        names(time_colors),
        c(rep("2px solid black", 2))
      ),
      borderRight = styleEqual(
        names(time_colors),
        c(rep("2px solid black", 2))
      ),
      borderBottom = styleEqual(
        names(time_colors),
        c("1px solid black", "2px solid black")
      )
    ) %>% 
    # Contrast font and background colors
    formatStyle(
      columns = 1,
      target = "cell",
      color = styleEqual(
        names(time_colors), 
        rep("black", length(time_colors))
      )
    )
  
}

## function for System Exits time flextable (in PPT download)
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
  # fmt_func_pct <- function(x) sprintf("%.0f%%", x*100)
  # 
  # ft <- set_formatter(
  #   x = ft,
  #   Permanent = fmt_func_pct,
  #   Homeless = fmt_func_pct,
  #   Institutional = fmt_func_pct,
  #   Temporary = fmt_func_pct,
  #   `Other/Unknown` = fmt_func_pct
  # )
  
  row_labels <- tab[[1]]
  
  ## formatting the time row labels
  time_colors <- c(
    "Current Year" = get_brand_color('med_purple'),
    "Previous Year" = get_brand_color('med_grey2')
  )
  
  ft <- ft %>%
    # Background colors from datatable's formatStyle
    bg(i = 1:2, j = 1, bg = time_colors) %>%
    # thick borders for the first column - adjust adjacent ones to match same total width
    border(i = 1, j = 1, 
           border.top = fp_border(color = "black", width = 2),
           border.left = fp_border(color = "black", width = 2),
           border.right = fp_border(color = "black", width = 2),
           border.bottom = fp_border(color = "black", width = 1)) %>% 
    border(i = 2, j = 1, 
           border.top = fp_border(color = "black", width = 1),
           border.left = fp_border(color = "black", width = 2),
           border.right = fp_border(color = "black", width = 2),
           border.bottom = fp_border(color = "black", width = 1)) %>% 
    border(i = 3, j = 1, 
           border.top = fp_border(color = "black", width = 1),
           border.left = fp_border(color = "black", width = 2),
           border.right = fp_border(color = "black", width = 2),
           border.bottom = fp_border(color = "black", width = 2)) %>% 
    # expand to better fit slide width
    autofit()
  
  
  ft
  
}

output$syse_compare_time_filter_selections <- renderUI({
  req(session$userData$valid_file() == 1)
  
  sys_detailBox(
    detail_type = 'time',
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
  )
})

output$syse_compare_time_chart <- renderPlot({
  time_chart_validation(startDate = session$userData$meta_HUDCSV_Export_Start, endDate = session$userData$meta_HUDCSV_Export_End,
                        input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age,
                        show = TRUE)
  get_syse_compare_time_chart()
})

output$syse_compare_time_table <- renderDT({
  # time_chart_validation(startDate = session$userData$ReportStart, endDate = session$userData$ReportEnd,
  #                       input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age,
  #                       show = FALSE)
  get_syse_compare_time_table(
    get_syse_compare_time_data(output_type = 'table')
  )
})

output$syse_time_download_btn <- downloadHandler(filename = date_stamped_filename("System Exits by Year Report - "),
                                                 content = function(file) {
                                                   logToConsole(session, "System Exits by Year data download")
                                                   
                                                   sheets <- list(
                                                     "SystemExitsByYear Metadata" = sys_export_summary_initial_df(type = 'exits_time') %>%
                                                       rowbind(
                                                         sys_export_filter_selections(type = 'exits')
                                                       ) %>% 
                                                       rowbind(
                                                         data.table(Chart = c('Total Current Year System Exits', 'Total Previous Year System Exits'),
                                                                    Value = scales::label_comma()(c(nrow(everyone() %>% fsubset(period == 'Current Year')),
                                                                                                    nrow(everyone() %>% fsubset(period == 'Previous Year')))
                                                                    )
                                                         )
                                                       ) %>% 
                                                       frename("System Exits by Year" = Value),
                                                     "YearComparisonData" = syse_time_export()
                                                     
                                                   )
                                                   
                                                   write_xlsx(
                                                     sheets,     
                                                     path = file,
                                                     format_headers = FALSE,
                                                     col_names = TRUE
                                                   )       
                                                   
                                                   logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
                                                                               if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
                                                 })


output$syse_time_download_btn_ppt <- downloadHandler(filename = function(){
  paste("System Exits by Year_", Sys.Date(), ".pptx", sep = "")
},
content = function(file) {
  logToConsole(session, "In syse_time_download_btn_ppt")
  
  sys_perf_ppt_export(file = file, 
                      type = 'exits_comparison',
                      title_slide_title = "System Exits by Year",
                      summary_items = list(
                        "Summary" = sys_export_summary_initial_df(type = 'exits_time') %>%
                          rowbind(
                            sys_export_filter_selections(type = 'exits')
                          ) %>% 
                          rowbind(
                            data.table(Chart = c('Total Current Year System Exits', 'Total Previous Year System Exits'),
                                       Value = scales::label_comma()(c(nrow(everyone() %>% fsubset(period == 'Current Year')),
                                                                       nrow(everyone() %>% fsubset(period == 'Previous Year')))
                                       )
                            )
                          ) 
                      ),
                      plots = list(
                        "System Exits by Year - Chart" = get_syse_compare_time_chart(isExport = TRUE),
                        "System Exits by Year - Table" = get_syse_compare_time_flextable(
                          get_syse_compare_time_data(output_type = 'table')
                        )
                      ),
                      summary_font_size = 19,
                      startDate = session$userData$ReportStart, 
                      endDate = session$userData$ReportEnd, 
                      sourceID = session$userData$Export$SourceID,
                      in_demo_mode = input$in_demo_mode
  )
  
})
