

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
  output$syse_compare_time_filter_selections <- 
  output$syse_types_filter_selections <- renderUI({ 
    req(session$userData$valid_file() == 1)
    syse_detailBox()
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

syse_types_chart <- function(varname, status){
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
  
  tree_colors <- c(
    "Permanent" = "#16697A",
    "Homeless" = "#C2462E",
    "Institutional" = "#C1DDD7",
    "Temporary" = "#71B4CB",
    "Other/Unknown" = "#73655E"
  )
  
  
  tree_exits_summ <- tree_exits_data() %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(Count = GRPN(), 
              Percent = GRPN() / nr) %>% 
    fungroup() %>% 
    fmutate(Percent = Count/fsum(Count),
           text_color = fifelse(`Destination Type` == 'Institutional', 'black', 'white'),
           label = str_c(`Destination Type`, ':\n', scales::label_comma()(Count),
                         ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
           ))
  
  ggplot(test_exits_summ, aes(area = Count, fill = `Destination Type`, 
                              label = str_c(`Destination Type`, ':\n', scales::label_comma()(Count),
                                            ' (', scales::label_percent(accuracy = 0.1)(Percent),')'
                                                                                     ))) +
    labs(title = paste0(scales::label_comma()(sum(test_exits_data$Count)), " System Exits for ", 
                        syse_level_of_detail_text(), " in ", 
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
         "SystemExitData" = tree_exits_data()
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
  
    sys_perf_ppt_export(file = file, 
                         type = 'exits',
                       title_slide_title = "System Exits by Type",
                       summary_items = sys_export_summary_initial_df(type = 'exits') %>%
                         filter(Chart != "Start Date" & Chart != "End Date") %>% 
                         bind_rows(sys_export_filter_selections(type = 'exits'),
                                   data.frame(Chart="Total System Exits", Value = scales::label_comma()(sum(tree_exits_data()$Count)))),
                       plots = list("System Exits by Type" = syse_types_chart("Destination Type", input$syse_dest_type_filter)),
                       summary_font_size = 19,
                       startDate = session$userData$ReportStart, 
                       endDate = session$userData$ReportEnd, 
                       sourceID = session$userData$Export$SourceID,
                       in_demo_mode = input$in_demo_mode
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
  time_chart_validation(startDate = session$userData$meta_HUDCSV_Export_Start, endDate = session$userData$meta_HUDCSV_Export_End,
                        input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age,
                        show = TRUE)
  syse_compare_time_chart()
})

output$syse_compare_time_table <- renderDT({
  time_chart_validation(startDate = session$userData$ReportStart, endDate = session$userData$ReportEnd,
                        input$syse_race_ethnicity, input$syse_spec_pops, input$syse_age,
                        show = FALSE)
  get_syse_compare_time_table(
    get_syse_compare_time_data()
  )
})

output$syse_compare_download_btn <- downloadHandler(filename = date_stamped_filename("System Exits Report - "),
                                                    content = function(file) {
      logToConsole(session, "System Exit Types data download")
                                                      
      write_xlsx(
        list(
          "ExitsComparison Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
            bind_rows(
              sys_export_filter_selections(type = 'exits')
            ),
          ## dummy dataset read-in from global.R for now
          "SystemExitData" = tree_exits_data()
        ),
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


# Create passes-enrollment-filter flag to exclude enrollments from heatmap -------
enrollments_filtered_syse <- reactive({
  logToConsole(session, "in enrollments_filtered")
  req(!is.null(input$imported$name) | isTRUE(input$in_demo_mode))
  
  en_filt <- join(
    session$userData$enrollment_categories,
    session$userData$client_categories %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  ) %>%
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
    roworder(PersonalID, EntryDate, ExitAdjust) %>%
    fgroup_by(PersonalID) %>%
    fmutate(
      # Days_to_lookahead is simpler because if they have ANY enrollment <= 14 days ahead
      # then it was clearly not a system exit
      days_to_lookahead = L(EntryDate, n=-1) - ExitAdjust
    ) %>%
    fungroup() %>% 
    fsubset(!is.na(days_to_lookahead ) & days_to_lookahead > 14 & passes_enrollment_filters)
  
})

all_filtered_syse <- reactive({
  join(
  ## filter in same way as enrollments_filtered
    enrollments_filtered_syse(),
    syse_client_categories_filtered(),
    on = "PersonalID",
    how = "inner"
  )
})

output$syse_compare_download_btn_ppt <- downloadHandler(filename = 'tmp',{
  
})


# System Exits Permanent Housing Demographics (PHD) -----------------------
sys_phd_plot_df <- reactiveVal()

output$syse_phd_chart <- renderPlot({
  req(
    !is.null(input$syse_phd_selections) &
      session$userData$valid_file() == 1 &
      between(length(input$syse_phd_selections), 1, 2)
  )
  
  if(length(input$syse_phd_selections) == 1) {
    sys_phd_plot_1var(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
  } else {
    sys_phd_plot_2vars(subtab = 'phd', input$syse_methodology_type, input$syse_phd_selections, isExport = FALSE)
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
    paste("System Exits PHD_", Sys.Date(), ".pptx", sep = "")
  },
  content = function(file) {
    sys_perf_ppt_export(
      file = file,
      type = 'exits',
      title_slide_title = "System Exits PHD",
      summary_items = sys_export_summary_initial_df(type = 'exits') %>%
        filter(Chart != "Start Date" & Chart != "End Date") %>% 
        bind_rows(sys_phd_selections_info()),
      plots = setNames(
        list(
          if (length(input$system_phd_selections) == 1) {
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
        paste0(
          "System Exits PHD: ",
          input$syse_phd_selections[1],
          " by ",
          input$syse_phd_selections[2]
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
  filename = date_stamped_filename("System Exits PHD Report - "),
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
