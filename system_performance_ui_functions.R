

#' Title
#'
#' @param tab_prefix 
#' @param filter_labels 
#' @param filter_choices list
#' @param widths list of two vectors, left column widths and right column widths
#'
#' @returns
#' @export
#'
#' @examples
evaFilterPanel <- function(fprefix, flabels, fchoices, fwidths = c(6,6)){
  
  left_card <- card(
    id = paste0(fprefix, '_filters_left'),
    style='border-width:0;border-radius:0',
    layout_columns(
      col_widths = c(4,4,4,6,6),
      fill = T,
      evaPickerInput(id = paste0(fprefix, '_hh_type'), label = flabels["hh_type"], choices = fchoices[["hh_type"]]),
      evaPickerInput(id = paste0(fprefix, '_level_of_detail'), label = flabels["level_of_detail"], choices = fchoices[["level_of_detail"]]),
      evaPickerInput(id = paste0(fprefix, '_project_type'), label = flabels["project_type"], choices = fchoices[["project_type"]]),
      
      evaPickerInput(
        id = paste0(fprefix, '_age'),
        label = flabels["age"],
        choices = fchoices[["age"]],
        selected = fchoices[["age"]],
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          selectedTextFormat = paste("count >", length(fchoices[["age"]])-1),
          countSelectedText = "All Ages",
          noneSelectedText = "All Ages",
          container = "body"
        )
      ),
      evaPickerInput(id = paste0(fprefix, '_spec_pops'), label = flabels["spec_pops"], choices = fchoices[["spec_pops"]])
    )
  )
  
  right_card <- card(
    id = paste0(fprefix, '_filters_right'),
    style="border-width:0;border-left-width: 1px; border-radius:0",
    layout_columns(
      col_widths = c(12,12),
      evaPickerInput(id = paste0(fprefix, '_methodology_type'), label = flabels["methodology_type"], choices = fchoices[["methodology_type"]]),
      evaPickerInput(
        id = paste0(fprefix, '_race_ethnicity'),
        label = flabels["race_ethnicity"],
        choices = fchoices[["race_ethnicity"]],
        selected = fchoices[["race_ethnicity"]],
        options = list(
          `dropdown-align-right` = TRUE,
          `dropup-auto` = FALSE,
          container = "body"
        )
      )
    )
  )
  
  card(
    card_header(headerCard('Filters')),
    layout_columns(
      col_widths=fwidths,
      gap = 0,
      left_card,
      right_card
    )
  )
}

evaPickerInput <- function(id, label, choices, options= pickerOptions(container = "body"), multiple = FALSE, selected = choices[1]){
  
  pickerInput(
    inputId = id,
    label = label,
    multiple = multiple,
    choices = choices,
    selected = selected,
    options = options
  )
}

default_filter_labels <- c("hh_type" = "Household Type", 
                           "level_of_detail" = "Level of Detail",
                           "project_type" = "Project Type Group",
                           "age" = "Age", "spec_pops" = "Veteran Status",
                           "methodology_type" = "Race/Ethnicity Methodology Type",
                           "race_ethnicity" = "Race/Ethnicity"
                          )
default_filter_choices <- list(
  "hh_type" = sys_hh_types,
  "level_of_detail" = sys_level_of_detail,
  "project_type" = sys_project_types,
  "age" = sys_age_cats,
  "spec_pops" = sys_spec_pops_people,
  "methodology_type" = sys_methodology_types,
  "race_ethnicity" = sys_race_ethnicity_method1
)

set_filter_labels <- function(hh_type = NULL,
                              level_of_detail = NULL,
                              project_type = NULL,
                              age = NULL,
                              spec_pops = NULL,
                              methodology_type = NULL,
                              race_ethnicity = NULL, ...){
  
  c("hh_type" = ifelse(is.null(hh_type), "Household Type", hh_type),
    "level_of_detail" = ifelse(is.null(level_of_detail), "Level of Detail", level_of_detail),
    "project_type" = ifelse(is.null(project_type), "Project Type Group", project_type),
    "age" = ifelse(is.null(age), "Age", age), 
    "spec_pops" = ifelse(is.null(spec_pops), "Veteran Status", spec_pops),
    "methodology_type" = ifelse(is.null(methodology_type), "Race/Ethnicity Methodology Type", methodology_type),
    "race_ethnicity" = ifelse(is.null(race_ethnicity), "Race/Ethnicity", race_ethnicity),
    ...
  )
}

set_filter_choices <- function(hh_type = sys_hh_types, 
                               level_of_detail = sys_level_of_detail,
                               project_type = sys_project_types,
                               age = sys_age_cats,
                               spec_pops = sys_spec_pops_people,
                               methodology_type = sys_methodology_types,
                               race_ethnicity = sys_race_ethnicity_method1,
                               ...
){
  
  list(
    hh_type = if(is.null(hh_type)){ sys_hh_types} else { hh_type},
    level_of_detail = if(is.null(level_of_detail)){ sys_level_of_detail} else { level_of_detail},
    project_type = if(is.null(project_type)){ sys_project_types} else { project_type},
    age = if(is.null(age)){ sys_age_cats} else { age},
    spec_pops = if(is.null(spec_pops)){ sys_spec_pops_people} else { spec_pops},
    methodology_type = if(is.null(methodology_type)){ sys_methodology_types} else { methodology_type},
    race_ethnicity = if(is.null(race_ethnicity)){ sys_race_ethnicity_method1} else { race_ethnicity},
    ...
  )
  
}



chart_selection_detail_line <- function(detail_label, val_list, inputVal) {
  return(
    HTML(glue(
      "<strong>{detail_label}:</strong> {getNameByValue(val_list, inputVal)} <br>"
    ))
  )
}
# tabBox ------------------------------------------------------------------
sys_detailBox <- function(
    selection = NULL,
    detail_type = 'overview',
    methodology_type = input$syse_methodology_type,
    cur_project_types = input$syse_project_type,
    startDate = session$userData$ReportStart,
    endDate = session$userData$ReportEnd,
    age = input$syse_age,
    spec_pops = input$syse_spec_pops,
    race_eth = input$syse_race_ethnicity
) {
  
  # Date Range
  date_range <- list(
    strong(fifelse(detail_type == 'time', "Current Year Date Range: ","Date Range: ")),
    format(startDate, "%m-%d-%Y"), " to ", format(endDate, "%m-%d-%Y"), br()
  )
  
  if(detail_type == 'time') { 
    date_range <- c(
      date_range, 
      list(
        strong("Previous Year Date Range: "),
        format(startDate - years(1) , "%m-%d-%Y"), " to ", format(endDate - years(1), "%m-%d-%Y"), br()
      )
    )
  } else if (detail_type == 'subpop') {
    subpop_mini_header <- list(HTML("<b>Subpopulation Selections</b> <br>") )
  }
  
  if(!is.null(race_eth) && (race_eth != "All" | (!is.null(selection) & !is.na(methodology_type)))){
    
    race_eth_methodology_type <- list(
      #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
      HTML(glue(
        "<b>Race/Ethnicity Methodology Type:</b> {str_sub(getNameByValue(sys_methodology_types , methodology_type), start = 1, end = 8)} <br>"
      ))
    )
    
  } else {
    race_eth_methodology_type <- NULL
  }
  
  if (cur_project_types != "All"){
    
    project_type <- c(
      list(chart_selection_detail_line("Project Type Group", sys_project_types, str_remove(cur_project_types, "- ")))
    )
  } else {
    project_type <- NULL
  }
  
  # For System Comp/Demographics and System Exits, the demographic items to display 
  # are which checkboxes user selected for the chart. Otherwise, they're the selected filter values
  demographics <- if(!is.null(selection) & detail_type != 'subpop') {
    list(
      HTML(glue("<strong>Selections</strong>: {paste(selection, collapse=' and ')} <br>"))
    )
  } else if (detail_type == 'subpop'){
    list(
      if (length(age) != length(sys_age_cats))
        HTML(glue(
          "<div style='text-indent: 20px;'><b>Age:</b> {paste(age, collapse = ', ')}</div>"
        )),
      
      if (!is.null(race_eth) && race_eth != "All")
        div(style='text-indent: 20px;',
            chart_selection_detail_line("Race/Ethnicity", sys_race_ethnicity_cats(methodology_type), race_eth)),
      
      if(getNameByValue(sys_spec_pops_people, spec_pops) != "All Statuses")
        HTML(glue(
          "<div style='text-indent: 20px;'><b>Veteran Status:</b> {paste(getNameByValue(sys_spec_pops_people, spec_pops), '(Adult Only)')}</div>"
        ))
    )
  } else {
    list(
      if (length(age) != length(sys_age_cats))
        HTML(glue(
          "<b>Age:</b> {paste(age, collapse = ', ')} <br>"
        )),
      
      if (!is.null(race_eth) && race_eth != "All")
        chart_selection_detail_line("Race/Ethnicity", sys_race_ethnicity_cats(methodology_type), race_eth),
      
      if(getNameByValue(sys_spec_pops_people, spec_pops) != "All Statuses")
        HTML(glue(
          "<b>Veteran Status:</b> {paste(getNameByValue(sys_spec_pops_people, spec_pops), '(Adult Only)')} <br>"
        ))
    )
  }
  
  if(detail_type == 'subpop'){
    if (!is.null(race_eth) && race_eth != "All"){
      return(c(date_range, race_eth_methodology_type, subpop_mini_header, demographics))
    } else {
      return(c(date_range, subpop_mini_header, demographics))
    }
    
  }  else {
    return(c(date_range, race_eth_methodology_type, project_type, demographics))
  }
}

# evaTabBox <- function(prefix, headers, subtabids, contentList = vector('list', length(subtabids))){
#   
#   
#   content <- lapply(seq_along(subtabids), function(i){
#      
#     tl <- tagList()
#     if('header' %in% contentList[[i]] | subtabids[i] != 'demog'){
#       tl <- tagAppendChild(tl,
#                            uiOutput(outputId = glue("{subtabids[i]}_filter_selections")) %>%
#                              withSpinner())
#     }
#    
#     if('plot' %in% contentList[[i]] & 'table' %in% contentList[[i]]){
#       tl <- tagAppendChild(
#         tl,
#         div(id=glue('{subtabids[i]}_vertical_div'),
#         verticalLayout(
#           plotOutput(outputId = glue("{subtabids[i]}_chart")) %>% withSpinner(),
#           br(),
#           DTOutput(outputId = glue("{subtabids[i]}_table")) %>% withSpinner()
#         )
#       )
#       )
#     } else if('plot' %in% contentList[[i]]){
#       
#       if('demog' %in% subtabids[i]){
#         tl <- tagAppendChildren(
#           tl,
#           card(
#             strong("Select Demographic Crosstab Categories (up to 2)"),
#             p(str_glue(
#               "For a simple count of totals within a demographic 
#                                    category, select only one category. To see the 
#                                    intersection of two demographic categories, select 
#                                    both categories to create a crosstab chart. To 
#                                    change your crosstab selection, uncheck at least 
#                                    one of your previous selections before selecting 
#                                    new categories. Note that you can only select one Race/Ethnicity 
#                                    category to display in the chart at a time."
#             )),
#             checkboxGroupInput(
#               glue("{prefix}_{subtabids[i]}_selections"),
#               label = "",
#               choices = sys_heatmap_selection_choices,
#               selected = c("All Races/Ethnicities", "Age"),
#               inline = TRUE
#             ),
#             width = 12
#           ),
#           br(),
#           plotOutput(outputId = glue("{subtabids[i]}_chart")) %>%
#             withSpinner()
#         )
#        
#       } else {
#         tl <- tagAppendChild(
#           tl,
#           plotOutput(outputId = glue("{subtabids[i]}_chart")
#           ) %>%
#             withSpinner()
#         )
#       }
#       
#    
#     } else if('table' %in% contentList[[i]]){
#       tl <- tagAppendChild(
#         tl,
#         tagList(
#         br(),
#         DTOutput(outputId = glue("{subtabids[i]}_table"))
#         )
#       )
#     }
#     tl
#   })
#   
#   
#   navset_card_underline(
#     id = glue('{prefix}_tabbox'),
#     
#     !!!lapply(seq_along(subtabids), function(i){
#               nav_panel(
#                 title = headerTab(headers[i]),
#                 navset_underline(
#                   id = glue("{subtabids[i]}_subtabs"),
#                   selected = headerSubTab("Chart"),
#                   nav_panel(
#                     title = headerSubTab("Chart"),
#                     content[[i]]
#                   ),
#                   nav_panel(
#                     title = headerSubTab("Information"),
#                     br(),
#                     get(glue("tab_{prefix}_{str_remove(str_remove(subtabids[i],prefix),'-')}_subtabs_information"))
#                   )
#                 )
#               )
#     }
#     )
#   )
# }

# evaTabBox(prefix = 'unsh', 
#           headers = c('Unsheltered Distribution',
#                       'Unsheltered PIT',
#                       'Unsheltered Inflow-Outflow',
#                       'Unsheltered CLS/DQ',
#                       'Unsheltered Demographics'),
#           subtabids = c('dist', 'pit','flow','clsdq','demog'),
#           contentList = list(
#             c('header' ),
#             c('header', 'plot'),
#             c('header', 'plot','table'),
#             c('header', 'table'),
#             c('plot', 'table')
#           ))
toggle_download_buttons <- function(prefix='sys', subtab = 'comp',plot_df) {
  shinyjs::toggle(glue("{prefix}_{subtab}_download_btn"), condition = sum(plot_df$n > 10, na.rm = TRUE) > 0)
  shinyjs::toggle(glue("{prefix}_{subtab}_download_btn_ppt"), condition = sum(plot_df$n > 10, na.rm = TRUE) > 0)
}

sys_comp_plot_1var <- function(subtab = 'comp', methodology_type, selection, people_univ = NULL, isExport = FALSE, session = session, display_var) {
  var_cols <- get_var_cols(methodology_type)
  browser()
  if(is.null(people_univ)){
    peopl_univ <- get_people_universe_filtered() 
  } 
  comp_df <- people_univ %>%
    remove_non_applicables(selection = selection) %>%
    fselect("PersonalID", unname(var_cols[[selection]]))
  
  validate(
    need(
      nrow(comp_df) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      nrow(comp_df) > 10,
      message = suppression_msg
    )
  )
  
  sys_comp_plot_df <- reactiveVal()
  
  
  plot_df <- get_sys_plot_df_1var(comp_df, var_cols[[selection]], selection = selection, session = session)
  
  # hide download buttons if not enough data
  toggle_download_buttons(subtab,plot_df)
  
  if(subtab == 'comp' | subtab == 'unsh'){
    type <- 'overview'
  } else if (subtab == 'phd'){
    type <- 'exits'
  }
  
  if(subtab == 'comp'){
    light_col <- get_brand_color('very_light_purple') 
    med_col <- get_brand_color('light_purple')
    dark_col <- get_brand_color('dark_purple')
  } else if(subtab == 'unsh'){
    light_col <- get_brand_color('light_coral')
    med_col <- get_brand_color('coral')
    dark_col <- get_brand_color('coral2')
  }
  
  selection_cats1 <- get_selection_cats(selection, methodology_type = methodology_type)
  if(is.null(names(selection_cats1))){
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  plot_df[[selection]] <- factor(
    plot_df[[selection]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels,
    ordered = TRUE)
  
  if(subtab == 'comp' | subtab == 'unsh'){
    sys_comp_plot_df(plot_df)
  } else if(subtab == 'phd'){
    sys_phd_plot_df(plot_df)
  }
  
  
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(selection, "n")
  
  validate(
    need(
      fsum(plot_df$n) > 0,
      message = all_data_suppressed_msg
    )
  )
  
  return(
    ggplot(plot_df  %>% fmutate(n= ifelse(is.na(n) & wasRedacted, 0, n)), aes("", .data[[selection]])) +
      # main data into cells for each cross-combination
      geom_tile(
        color = '#f0f0f0',
        lwd = 0.5,
        linetype = 1,
        aes(fill = n)
      ) +
      # scale_fill_gradient(
      #   low = get_brand_color('light_purple'),
      #   high = get_brand_color('dark_purple'),
      #   na.value = ifelse(
      #     is.na(plot_df$wasRedacted) | !plot_df$wasRedacted,
      #     "white",
      #     get_brand_color('very_light_purple')
      #   )
      # ) +
      scale_fill_gradient2(
        low = light_col,#get_brand_color('very_light_purple'),
        mid= med_col, #get_brand_color('light_purple'),
        high = dark_col, #get_brand_color('dark_purple'),
        #midpoint=0,
        #breaks = #c(0,seq(0,45,by=5)),
        na.value = 'white'
      ) +
      # set text color to be 508 compliant contrasting
      geom_text(
        aes(label = ifelse(wasRedacted, "***", format(n, big.mark = ',', scientific = FALSE, trim = TRUE))),
        size = sys_chart_text_font,
        color = ifelse(
          plot_df$n > mean(plot_df$n, na.rm = TRUE) & !plot_df$wasRedacted,
          'white',
          'black'
        )
      ) +
      scale_y_discrete(
        labels = label_wrap(30),
        limits = rev(levels(plot_df[[selection]])),
      ) +
      # other stuff
      theme_bw() +
      ggtitle(sys_total_count_display(
        nrow(comp_df),display_var = display_var
      )) +
      labs(caption = "*** indicates the value is suppressed") +
      theme(
        text = element_text(size = sys_chart_text_font_pts),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = get_adj_font_size(sys_axis_text_font, isExport))
      )
  )
}

sys_comp_plot_2vars <- function(subtab = 'comp', methodology_type, selections, people_univ = NULL, isExport = FALSE, session = session, display_var) {
  # race/ethnicity, if selected, should always be on the row
  var_cols <- get_var_cols(methodology_type)
  
  if (selections[1] %in% c("All Races/Ethnicities", "Grouped Races/Ethnicities")) {
    selections <- c(selections[2], selections[1])
  }
  
  # get dataset underlying the freqs we will produce below
  comp_df <- people_univ %>%
    remove_non_applicables(selection = selections) %>%
    fselect(
      "PersonalID", 
      unname(var_cols[[selections[1]]]), 
      unname(var_cols[[selections[2]]])
    ) %>%
    funique()
  
  shinyjs::toggle(
    "sys_comp_download_btn",
    condition = nrow(comp_df) > 10
  )
  shinyjs::toggle(
    "sys_comp_download_btn_ppt",
    condition = nrow(comp_df) > 10
  )
  
  validate(
    need(
      nrow(comp_df) > 0,
      message = no_data_msg
    )
  )
  validate(
    need(
      nrow(comp_df) > 10,
      message = suppression_msg
    )
  )
  
  if(subtab == 'comp'){
    light_col <- get_brand_color('very_light_purple')
    med_col <- get_brand_color('light_purple')
    dark_col <- get_brand_color('dark_purple')
  } else if(subtab == 'unsh'){
    light_col <- get_brand_color('light_coral')
    med_col <- get_brand_color('coral')
    dark_col <- get_brand_color('coral2')
  }
  
  plot_df <- get_sys_plot_df_2vars(comp_df, var_cols, selections = selections, session = session)
  
  toggle_download_buttons(subtab, plot_df)
  
  if(subtab == 'comp' | subtab == 'unsh'){
    type <- 'overview'
  } else if (subtab == 'phd'){
    type <- 'exits'
  }
  selection_cats1 <- get_selection_cats(selections[1], methodology_type = methodology_type)
  
  if (is.null(names(selection_cats1))) {
    selection_cats1_labels <- selection_cats1
  } else {
    selection_cats1_labels <- names(selection_cats1)
  }
  
  selection_cats2 <- get_selection_cats(selections[2], methodology_type = methodology_type)
  if (is.null(names(selection_cats2))) {
    selection_cats2_labels <- selection_cats2
  } else {
    selection_cats2_labels <- names(selection_cats2)
  }
  
  plot_df[selections[1]] <- factor(
    plot_df[[selections[1]]], 
    levels = selection_cats1, 
    labels = selection_cats1_labels)
  
  plot_df[selections[2]] <- factor(
    plot_df[[selections[2]]], 
    levels = selection_cats2, 
    labels = selection_cats2_labels)
  
  plot_df <- plot_df %>%
    complete(
      !!sym(selections[1]),
      !!sym(selections[2])
    ) %>%
    replace(is.na(.), 0)
  
  if(methodology_type == 1) {
    
    h_total_tmp <- plot_df %>%
      fgroup_by(selections[2]) %>%
      fsummarise(N = fsum(n))
    
    h_total_tmp[[selections[1]]] <- "Total"
    
    h_total <- h_total_tmp %>% 
      suppress_values("N") %>%
      suppress_next_val_if_one_suppressed_in_group(selections[1], "N")
    
    v_total <- plot_df %>%
      group_by(!!!syms(selections[[1]])) %>%
      summarise(N = ifelse(all(is.na(n)), NA, sum(n, na.rm = TRUE))) %>%
      mutate(!!selections[[2]] := 'Total') %>%
      suppress_values("N") %>%
      suppress_next_val_if_one_suppressed_in_group(selections[2], "N")
  }
  
  sys_comp_plot_df <- reactiveVal()
  
  # save before supressing the values
  # this will be used for the download/export
  if(subtab == 'comp' | subtab == 'unsh'){
    sys_comp_plot_df(plot_df)
  } else {
    sys_phd_plot_df(plot_df)
  }
  
  # Suppress values <= 10
  plot_df <- plot_df %>%
    suppress_values("n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[1], "n") %>%
    suppress_next_val_if_one_suppressed_in_group(selections[2], "n")
  
  validate(
    need(
      fsum(plot_df$n) > 0,
      message = all_data_suppressed_msg
    )
  )
  
  g <- ggplot(plot_df %>% fmutate(n = ifelse(is.na(n) & wasRedacted, 0, n)), aes(.data[[selections[1]]], .data[[selections[2]]])) +
    # main data into cells for each cross-combination
    geom_tile(
      color = '#f0f0f0',
      lwd = 0.5,
      linetype = 1,
      aes(fill = n)
    ) +
    scale_fill_gradient2(
      low = light_col,#get_brand_color('light_purple'),
      mid = med_col,
      high = dark_col,#get_brand_color('dark_purple'),
      #midpoint = 0,
      na.value = "white"
    ) + # na.value makes 0s invisible
    # scale_fill_gradient2(
    #   low = get_brand_color('very_light_purple'),
    #   mid=get_brand_color('light_purple'),
    #   high = get_brand_color('dark_purple'),
    #   midpoint=0,
    #   na.value = 'white'
    # ) +
    # set text color to be 508 compliant contrasting
    geom_text(
      # aes(label = paste0(scales::comma(n), "\n", "(",scales::percent(pct, accuracy = 0.1),")")),
      aes(label = ifelse(wasRedacted, "***", format(n, big.mark = ',', scientific = FALSE, trim = TRUE))),
      size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction, 1),
      color = ifelse(
        plot_df$n > mean(plot_df$n, na.rm = TRUE) & !plot_df$wasRedacted,
        'white',
        'black'
      )
    )
  
  
  x_labels <- selection_cats1_labels
  x_limits <- levels(plot_df[[selections[1]]])
  y_labels <- rev(selection_cats2_labels)
  y_limits <- rev(levels(plot_df[[selections[2]]]))
  
  if(methodology_type == 1) {
    x_labels <- c(x_labels, "Total")
    x_limits <- c(x_limits, "Total")
    y_labels <- c("Total", y_labels)
    y_limits <- c("Total", y_limits)
    g <- g + 
      ggnewscale::new_scale("fill") +
      # Row totals
      geom_tile(
        data = h_total %>% fmutate(N= ifelse(is.na(N) & wasRedacted, 0, N)),
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = N)
      ) +
      
      scale_fill_gradient(
        low = get_brand_color('light_grey'),
        high = get_brand_color('dark_grey'),
        na.value = 'white'#ifelse(h_total$wasRedacted, get_brand_color('light_grey'), 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, '***', format(N, big.mark = ',', scientific = FALSE, trim = TRUE))),
        size = sys_chart_text_font * ifelse(isExport, sys_chart_export_font_reduction, 1),
        color = ifelse(
          h_total$N > mean(h_total$N, na.rm = TRUE) & !h_total$wasRedacted,
          'white',
          'black'
        ),
        data = h_total
      ) +
      
      # column totals
      ggnewscale::new_scale("fill") +
      geom_tile(
        data = v_total,
        color = "white",
        lwd = 0.5,
        linetype = 1,
        aes(fill = N)
      ) +
      scale_fill_gradient(
        low = get_brand_color('light_grey'),
        high = get_brand_color('dark_grey'),
        na.value = 'white'#ifelse(v_total$wasRedacted, get_brand_color('light_grey'), 'white')
      ) +
      
      geom_text(
        aes(label = ifelse(wasRedacted, "***",
                           format(N, big.mark = ',', scientific = FALSE, trim = TRUE))),
        size = sys_chart_text_font * ifelse(isExport, 0.7, 1),
        color = ifelse(
          v_total$N > mean(v_total$N, na.rm = TRUE) & !v_total$wasRedacted,
          'white',
          'black'
        ),
        data = v_total
      )
  }
  g + 
    # axis labels
    scale_x_discrete(
      labels = str_wrap(x_labels, width = 20),
      limits = x_limits,
      position = "top"
    ) +
    scale_y_discrete(
      labels = str_wrap(y_labels, width = 30),
      limits = y_limits
    ) +
    
    # other stuff
    theme_bw() +
    
    ggtitle(sys_total_count_display(
      nrow(comp_df), display_var=display_var
    )) +
    labs(caption = "*** indicates the value is suppressed") +
    
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = sys_chart_title_font, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # axis.title.x.top = element_text(margin = margin(0, 0, 15, 0)),
      axis.text = element_text(size = sys_comp_axis_text_font * 1 * ifelse(isExport, 0.6, 1))
    )
}

sys_total_count_display <- function(total_count, display_var ) {
  return(paste0(
    str_wrap(
      paste0(
        display_var,#full_unit_of_analysis_display(),
        ": ",
        format(total_count, big.mark = ',', scientific = FALSE, trim = TRUE)
      ),
      width = 40
    ),
    "\n")
  )
}

get_race_ethnicity_vars <- function(v, methodology_type, race_ethnicity_func) {
  #browser()
  if (v == "All") {
    sys_race_ethnicities_all <- unlist(c(race_ethnicity_func(methodology_type)["Detailed"],"Unknown" = "RaceEthnicityUnknown"))
    names(sys_race_ethnicities_all) <- gsub("Detailed.", "", names(sys_race_ethnicities_all))
    return(sys_race_ethnicities_all)
  } else if (v %in% c("Grouped")) {
    sys_race_ethnicities_grouped <- unlist(c(race_ethnicity_func(methodology_type)["Summarized"], "Unknown" = "RaceEthnicityUnknown"))
    names(sys_race_ethnicities_grouped) <- gsub("Summarized.", "", names(sys_race_ethnicities_grouped))
    return(sys_race_ethnicities_grouped)
  }
}

get_var_cols <- function(methodology_type, race_ethnicity_func = sys_race_ethnicity_cats) {
  #browser()
  return(
    list(
      "Age" = "AgeCategory",
      "All Races/Ethnicities" = get_race_ethnicity_vars("All", methodology_type = methodology_type, 
                                                        race_ethnicity_func = race_ethnicity_func),
      "Grouped Races/Ethnicities" = get_race_ethnicity_vars("Grouped", methodology_type = methodology_type, 
                                                            race_ethnicity_func = race_ethnicity_func),
      #"Domestic Violence" = "DomesticViolenceCategory", #VL 9/20/24: Not including for launch
      # "Homelessness Type" =  "HomelessnessType",# Victoria, 8/15/24: Not including this for Launch
      "Veteran Status (Adult Only)" =  "VeteranStatus"
    )
  )
}

get_sys_plot_df_1var <- function(comp_df, var_col, selection = input$system_composition_selections, session = session) {
  # if number of variables associated with selection > 1, then they're dummies
  
  if (length(var_col) > 1) {
    #browser()
    plot_df <- comp_df %>%
      fselect(PersonalID:LatinoAloneMethod2Summarized) %>% 
      pivot(
        how="longer",
        ids = "PersonalID",
        names = list(selection, "n")
      ) %>%
      fgroup_by(selection) %>%
      fsummarize(n = fsum(n))
    
  } else {
    plot_df <- as.data.frame(table(comp_df[[var_col]]))
    names(plot_df) <- c(selection, "n")
    
    if(selection == "Domestic Violence Status") {
      plot_df <- plot_df %>% rowbind(tibble(
        `Domestic Violence Status` = "DVTotal",
        n = sum(plot_df %>% 
                  fsubset(`Domestic Violence Status` != "NotDV") %>%
                  pull(n), na.rm = TRUE)))
    }
  }
  return(plot_df)
}


get_sys_plot_df_2vars <- function(comp_df, var_cols, selections = input$system_composition_selections, session = session, input=input) {
  
  # Get a dataframe of the freqs of all combinations
  # along with percents
  freqs <- expand_grid(v1 = var_cols[[selections[1]]], v2 = var_cols[[selections[2]]]) %>%
    pmap_dfr(~ process_combination(..1, ..2, comp_df, var_cols = var_cols, selections = selections, session = session)) # %>%
  # mutate(pct = (n / sum(n, na.rm = TRUE)))
  
  # Handle DV, since the "Total" is not an actual value of DomesticViolenceCategory.
  if ("Domestic Violence Status" %in% selections) {
    dv_totals <- freqs %>%
      fsubset(`Domestic Violence Status` %in% c("DVFleeing", "DVNotFleeing")) %>%
      group_by(!!sym(
        ifelse(
          selections[1] == "Domestic Violence Status",
          selections[2],
          selections[1]
        )
      )) %>%
      summarize(`Domestic Violence Status` = "DVTotal",
                n = sum(n, na.rm = TRUE)) #,
    # pct = sum(pct, na.rm = TRUE))
    freqs <- rowbind(freqs, dv_totals)
  }
  
  return(freqs)
}

# Function to process each combination of the variables underlying the all-served
# selections E.g. if Age and Race (and Method 1),
# then we'd combine 0 to 12 with White, 0 to 12 with Black,
# 13 to 24 with White, etc.
process_combination <- function(v1, v2, comp_df, var_cols, selections, session = session) {
  logToConsole(session, glue("processing combination of {v1} and {v2}"))
  freq_df <- as.data.frame(table(comp_df[[v1]], comp_df[[v2]]))
  names(freq_df) <- c(
    selections[1],
    selections[2],
    "n"
  )
  
  # for selections comprised of multiple (binary/dummy) vars (e.g. Race), 
  # filter to the 1s and change the 1 to the variable name
  for (i in seq_along(selections)) {
    v <- get(paste0("v", i))
    vname <- sym(selections[i])
    var_cats <- var_cols[[vname]]
    if (length(var_cats) > 1) {
      freq_df <- freq_df %>%
        filter(!!vname == 1) %>%
        mutate(!!vname := v)
    }
  }
  return(freq_df)
}