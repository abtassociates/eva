

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


evaFilterPanel(
  fprefix = 'unsh',
  flabels = set_filter_labels('project_type'= "Unsheltered Project Type"), 
  fchoices = set_filter_choices("project_type" = unsh_project_types)
)


# tabBox ------------------------------------------------------------------

evaTabBox <- function(prefix, headers, subtabids, contentList = vector('list', length(subtabids))){
  
  #browser()
  
  content <- lapply(seq_along(subtabids), function(i){
     
    tl <- tagList()
    #browser()
    if('header' %in% contentList[[i]]){
      tl <- tagAppendChild(tl,
                           uiOutput("{prefix}_{subtabsids[i]}_filter_selections") %>%
                             withSpinner())
    }
   
    if('plot' %in% contentList[[i]]){
      tl <- tagAppendChild(
        tl,
        plotOutput("{prefix}_{subtabsids[i]}_chart"
        ) %>%
          withSpinner()
      )
    } 
    
    if('table' %in% contentList[[i]]){
      tl <- tagAppendChild(
        tl,
        tagList(
        br(),
        DTOutput("{prefix}_{subtabsids[i]}_table")
        )
      )
    }
    tl
  })
  
  
  navset_card_underline(
    id = glue('{prefix}_tabbox'),
    
    !!!lapply(seq_along(subtabids), function(i){
              nav_panel(
                title = headerTab(headers[i]),
                navset_underline(
                  id = glue("{prefix}_{subtabids[i]}_subtabs"),
                  selected = headerSubTab("Chart"),
                  nav_panel(
                    title = headerSubTab("Chart"),
                    content[[i]]
                  ),
                  nav_panel(
                    title = headerSubTab("Information"),
                    br(),
                    get(glue("tab_{prefix}_{subtabids[i]}_subtabs_information"))
                  )
                )
              )
    }
    )
  )
}

evaTabBox(prefix = 'unsh', 
          headers = c('Unsheltered Distribution',
                      'Unsheltered PIT',
                      'Unsheltered Inflow-Outflow',
                      'Unsheltered CLS/DQ',
                      'Unsheltered Demographics'),
          subtabids = c('dist', 'pit','flow','clsdq','demog'),
          contentList = list(
            c('header' ),
            c('header', 'plot'),
            c('header', 'plot','table'),
            c('header', 'table'),
            c('plot', 'table')
          ))


