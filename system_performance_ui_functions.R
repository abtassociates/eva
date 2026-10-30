

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


