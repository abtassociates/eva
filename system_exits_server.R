

#### DISPLAY FILTER SELECTIONS ###
syse_detailBox <- reactive({
  list(
    br(),
    strong("Date Range: "),
    
    format(session$userData$ReportStart, "%m-%d-%Y"), " to ", format(session$userData$ReportEnd, "%m-%d-%Y"), br(),
    
    if (input$syse_project_type != "All")
      chart_selection_detail_line("Project Type ", sys_project_types, str_remove(input$syse_project_type, "- ")),
    
    #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
    HTML(glue(
      "<b>Methodology Type:</b> {str_sub(getNameByValue(sys_methodology_types, input$syse_methodology_type), start = 1, end = 8)} <br>"
    )),
    
    if (length(input$syse_age) != length(sys_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syse_age, collapse = ', ')} <br>"
      )),
    
    if (input$syse_race_ethnicity != "All")
      chart_selection_detail_line("Race/Ethnicity", sys_race_ethnicity_cats(input$syse_methodology_type), input$syse_race_ethnicity),
    
    if(getNameByValue(sys_spec_pops_people, input$syse_spec_pops) != "All Statuses")
      HTML(glue(
        "<b>Veteran Status:</b> {paste(getNameByValue(sys_spec_pops_people, input$syse_spec_pops), '(Adult Only)')} <br>"
      ))
    
  )
})




output$syse_compare_subpop_filter_selections <- 
  output$syse_compare_time_filter_selections <- 
  output$syse_types_filter_selections <- renderUI({ 
    req(session$userData$valid_file() == 1)
    syse_detailBox() 
  })
sys_phd_selections_info <- reactive({
  data.frame(
    Chart = c(
      "Demographic Selection 1",
      "Demographic Selection 2",
      "Total Served People"
    ),
    Value = c(
      input$system_phd_selections[1],
      input$system_phd_selections[2],
      nrow(get_people_universe_filtered() %>% remove_non_applicables(selection = input$system_phd_selections))
    )
  )
})
syse_phd_detailBox <- function() {
  return(
    list(
      strong("Date Range: "),
      
      format(session$userData$ReportStart, "%m-%d-%Y"),
      " to ",
      format(session$userData$ReportEnd, "%m-%d-%Y"),
      br(),
      
      if (input$syse_project_type != "All")
        chart_selection_detail_line("Project Type Group", sys_project_types, str_remove(input$syse_project_type, "- ")),
      
      #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
      HTML(glue(
        "<b>Methodology Type:</b> {str_sub(getNameByValue(sys_methodology_types, input$syse_methodology_type), start = 1, end = 8)} <br>"
      )),
      
      HTML(
        glue(
          "<strong>Selections</strong>: {paste(input$system_phd_selections, collapse=' and ')} <br>"
        )
      )
    )
  )
}

output$syse_phd_summary_selections <- renderUI({
  req(!is.null(input$system_phd_selections) & session$userData$valid_file() == 1)
  syse_phd_detailBox()
})
  
sys_comp_selections_summary <- function() {
  return(
    sys_export_summary_initial_df(type = 'exits') %>%
      bind_rows(sys_comp_selections_info()) %>%
      rename("System Demographics" = Value)
  )
}

level_of_detail_text_syse <- reactive({
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

syse_types_chart <- function(varname, status){
  
  tree_colors <- c(
    "Permanent" = "#16697A",
    "Homeless" = "#C2462E",
    "Institutional" = "#C1DDD7",
    "Temporary" = "#71B4CB",
    "Other/Unknown" = "#73655E"
  )
  
  
  
  ggplot(test_exits_summ, aes(area = Count, fill = `Destination Type`, 
                              label = str_c(`Destination Type`, ':\n', scales::label_comma()(Count),
                                            ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
                                                                                     ))) +
    labs(title = paste0(scales::label_comma()(sum(test_exits_data$Count)), " System Exits for ", 
                        level_of_detail_text_syse(), " in ", 
                        str_remove(getNameByValue(sys_hh_types, input$syse_hh_type), "- "), 
                        if_else(getNameByValue(sys_hh_types, input$syse_hh_type) == "All Household Types", "", " Households"))
         ) +
    geom_treemap(start = "left", show.legend = FALSE) +
    geom_treemap_text(aes(color = text_color), fontface = 'bold',start = "left", place = "center", grow = FALSE) +
    scale_color_identity() +
    scale_fill_manual(values = tree_colors) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5)
    )
}


# PowerPoint Export -------------------------------------------------------
sys_exits_ppt_export <- function(file,
                                    title_slide_title,
                                    summary_items,
                                    plots,
                                    summary_font_size) {
  logMetadata(session, paste0("Downloaded System Exits Powerpoint: ", title_slide_title,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  loc_title <- ph_location_type(type = "title")
  loc_footer <- ph_location_type(type = "ftr")
  loc_dt <- ph_location_type(type = "dt")
  loc_slidenum <- ph_location_type(type = "sldNum")
  loc_body <- ph_location_type(type = "body")
  loc_subtitle <- ph_location_type(type = "subTitle")
  loc_ctrtitle <- ph_location_type(type = "ctrTitle")
  
  fp_normal <- fp_text(font.size = summary_font_size)
  fp_title <- fp_text(font.size = ppt_chart_title_font_size)
  fp_bold <- update(fp_normal, bold = TRUE)
  fp_red <- update(fp_normal, color = "red")
  
  ppt <- read_pptx(here("system_pptx_template.pptx"))
  
  report_period <- paste0("Report Period: ", 
                          format(session$userData$ReportStart, "%m/%d/%Y"),
                          " - ",
                          format(session$userData$ReportEnd, "%m/%d/%Y")
  )
  
  add_footer <- function(.ppt) {
    return(
      .ppt %>%
        ph_with(value = paste0("CoC Code: ", session$userData$Export$SourceID), location = loc_footer) %>%
        ph_with(value = report_period, location = loc_dt) %>%
        ph_with(
          value = paste0(
            "Export Generated: ",
            format(Sys.Date()),
            "\n",
            "https://hmis.abtsites.com/eva/"
          ),
          location = loc_slidenum
        )
    )
  }
  
  # title Slide
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme") %>%
    ph_with(value = title_slide_title, location = loc_ctrtitle) %>%
    ph_with(value = "Eva Image Export", location = loc_subtitle) %>%
    add_footer()
  
  # Summary
  s_items <- do.call(block_list, lapply(1:nrow(summary_items), function(i) {
    fpar(
      ftext(paste0(summary_items$Chart[i], ": ", summary_items$Value[i]), fp_normal)
    )
  }))
  
  ppt <- add_slide(ppt, layout = "Title and Content") %>%
    ph_with(value = "Summary", location = loc_title) %>%
    ph_with(
      value = s_items,
      level_list = c(rep(1L, length(s_items))),
      location = loc_body
    ) %>% 
    add_footer()
  
  # Chart
  for(plot_slide_title in names(plots)) {
    p <- plots[[plot_slide_title]]
    if(!is.null(p)) {
      ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext(plot_slide_title, fp_title)), location = loc_title) %>%
        ph_with(value = p, location = loc_body) %>%
        add_footer()
    }
  }
  
  # Export the PowerPoint
  return(print(ppt, target = file))
}


output$syse_types_download_btn <- downloadHandler( filename = date_stamped_filename("System Exits Report - "),
                                                   content = function(file) 
    {
     logToConsole(session, "System Exit Types data download")
     
     write_xlsx(
       list(
         "ExitsByType Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
           bind_rows(
             sys_export_filter_selections(type = 'exits')
           ),
         ## dummy dataset read-in from global.R for now
         "SystemExitData" = test_exits_data
       ),
       path = file,
       format_headers = FALSE,
       col_names = TRUE
     )        
     
})

output$syse_types_download_btn_ppt <- downloadHandler(filename = function() {
  paste("System Exits_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    logToConsole(session, "In syse_types_download_btn_ppt")
  
    sys_exits_ppt_export(file = file, 
                       title_slide_title = "System Exits by Type",
                       summary_items = sys_export_summary_initial_df(type = 'exits') %>%
                         filter(Chart != "Start Date" & Chart != "End Date") %>% 
                         bind_rows(sys_export_filter_selections(type = 'exits'),
                                   data.frame(Chart="Total System Exits", Value = scales::label_comma()(sum(test_exits_data$Count)))),
                       plots = list("System Exits by Type" = syse_types_chart("Destination Type", input$syse_dest_type_filter)),
                       summary_font_size = 19
                       )
})


# System Exit Comparisons  ------------------------------------------------

subpop_chart_validation <- function(raceeth, vetstatus, age, show = TRUE) {
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
  } else {
    ## otherwise, just hide but do not show a duplicate validate message
    req(cond)
  }
 
}

get_syse_compare_subpop_data <- reactive({
  ## create placeholder data for chart and table creation
  tribble(~subpop_summ,~Permanent,~Homeless,~Institutional,~Temporary,~"Other/Unknown",
         "Subpopulation", 0.15, 0.31, 0.04, 0.10, 0.40,
         "Everyone Else", 0.25, 0.17, 0.11, 0.14, 0.33,
         "Percent Difference", -0.1, 0.14, -0.07, -0.04, 0.07
  )

})

get_syse_compare_time_data <- reactive({
  ## create placeholder data for chart and table creation
  tribble(~time_summ,~Permanent,~Homeless,~Institutional,~Temporary,~"Other/Unknown",
          "Current Year", 0.17, 0.20, 0.08, 0.26, 0.29,
          "Previous Year", 0.25, 0.17, 0.11, 0.14, 0.33,
          "Percent Change", -0.08, 0.03, -0.03, 0.12, -0.04
  )
  
})

## function to make System Exits comparison subpopulation chart
syse_compare_subpop_chart <- function(subpop, isExport = FALSE){
  
  subgroup_colors <- c(
   "Subpopulation" = "#136779",
   "Everyone Else" = "#C1432B"
  )
  
  ## long format needed for plotting points
  subpop_chart_df <- get_syse_compare_subpop_data() %>% 
    filter(subpop_summ != "Percent Difference") %>% 
    pivot_longer(cols = -1, names_to = 'dest_type', values_to = 'subpop_pct') %>% 
    mutate(dest_type = factor(dest_type, levels = c("Permanent","Homeless","Institutional","Temporary","Other/Unknown")) )
  
  ## wide format needed for plotting arrows between points
  subpop_segment_df <- subpop_chart_df %>% 
    pivot_wider(names_from = 'subpop_summ', values_from = 'subpop_pct')
  
  ggplot(subpop_chart_df, aes(x = dest_type, y = subpop_pct, color = subpop_summ)) +
    geom_point(size = 5) +
    geom_segment(data=subpop_segment_df,
                 aes(x = dest_type, xend = dest_type, y = Subpopulation, yend = `Everyone Else`),
                 arrow = arrow(length = unit(0.125, "inches")), color = '#948A84') +
    scale_color_manual(values=subgroup_colors,guide =  guide_legend(ncol = 2)) +
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
          axis.text.x = element_blank()
          )
}
  
## function for System Exits Comparison subpopulation table (below chart)
get_syse_compare_subpop_table <- function(tab, subpop){
  
  subgroup_colors <- c(
    "Subpopulation" = "#136779",
    "Everyone Else" = "#C1432B"
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
    # Highlight only the first column of "Subpopulation" and "Everyone Else" rows
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
        rep("white", length(subgroup_colors))
      )
    )
  
  
}

output$syse_compare_subpop_chart <- renderPlot({
  ## check if filters have been changed from defaults before showing 
  subpop_chart_validation(input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age)
  syse_compare_subpop_chart(subpop = input$syse_race_ethnicity)
})

output$syse_compare_subpop_table <- renderDT({
  ## check if filters have been changed from defaults before showing 
  subpop_chart_validation(input$syse_race_ethnicity,input$syse_spec_pops,input$syse_age, show = FALSE)
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
  
  ggplot(time_chart_df, aes(x = dest_type, y = time_pct, color = time_summ)) +
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
      axis.text.x = element_blank()
    )
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
  syse_compare_time_chart()
})

output$syse_compare_time_table <- renderDT({
  
  get_syse_compare_time_table(
    get_syse_compare_time_data()
  )
})

output$syse_compare_download_btn <- downloadHandler(filename = 'tmp',{
## hide demographic filters when on PHD subtab
observeEvent(input$syse_tabbox, {
  req(session$userData$valid_file() == 1)
  logMetadata(session, paste0("Clicked on ", input$syse_tabbox,
                              if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
  if(input$syse_tabbox == 'Permanent Housing Demographics'){
    shinyjs::hide('syse_spec_pops')
    shinyjs::hide('syse_age')
    shinyjs::hide('syse_race_ethnicity')
  } else {
    shinyjs::show('syse_spec_pops')
    shinyjs::show('syse_age')
    shinyjs::show('syse_race_ethnicity')
  }
 
})

output$syse_compare_download_btn_ppt <- downloadHandler(filename = 'tmp',{
  
})


# System Exits Permanent Housing Demographics (PHD) -----------------------

output$syse_phd_chart <- renderPlot({
  req(
    !is.null(input$syse_phd_selections) &
      session$userData$valid_file() == 1 &
      between(length(input$syse_phd_selections), 1, 2)
  )
  
  if(length(input$syse_phd_selections) == 1) {
    sys_comp_plot_1var()
  } else {
    sys_comp_plot_2vars()
  }
}, height = function() {
  ifelse(!is.null(input$syse_phd_selections), 700, 100)
}, width = function() {
  if (length(input$syse_phd_selections) == 1 |
      isTRUE(getOption("shiny.testmode"))) {
    500
  } else {
    "auto"
  }
}, alt = "A crosstab data table of the demographic make-up of the homeless system.")

output$syse_phd_download_btn <- downloadHandler(filename = 'tmp',{
  
})

output$syse_phd_download_btn_ppt <- downloadHandler(filename = 'tmp',{
  
})

observeEvent(input$syse_methodology_type, {
  
  updatePickerInput(
    session, 
    "syse_race_ethnicity", 
    choices = sys_race_ethnicity_cats(input$syse_methodology_type)
  )

},
ignoreInit = TRUE)

toggle_sys_components(prefix='syse', FALSE, init=TRUE) # initially hide them