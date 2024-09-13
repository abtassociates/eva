# when user changes chart tabs
# hide demographic filters for Composition chart
# hide other stuff if valid file is not uploaded
# move chart download button to be inline with subtabs
observeEvent(input$syso_tabsetpanel, {
  req(valid_file() == 1)
  toggleClass(
    id = "syso_inflowoutflow_filters",
    condition = input$syso_tabsetpanel == "Composition of All Served in Period",
    class = "filter-hidden"
  )
}, ignoreNULL = TRUE)

observeEvent(input$methodology_type, {
  
  updatePickerInput(
    session = session,
    "syso_gender", 
    choices = syso_gender_cats(input$methodology_type),
    selected = "All Genders"
  )
  
  updatePickerInput(
    session, 
    "syso_race_ethnicity", 
    choices = syso_race_ethnicity_cats(input$methodology_type)
  )
  
  updateCheckboxGroupInput(
    session, 
    "system_composition_selections", 
    choices = sys_comp_selection_choices(),
    inline = TRUE
  )
  
},
ignoreInit = TRUE)

observeEvent(input$syso_level_of_detail, {
  updatePickerInput(session, "syso_spec_pops",
                    # label = "Special Populations",
                    choices = syso_spec_pops_people)
})

#### DISPLAY FILTER SELECTIONS ###
syso_detailBox <- reactive({
  # remove group names from race/ethnicity filter
  # so we can use getNameByValue() to grab the selected option label
  # if (input$methodology_type == 2) {
  # browser()
  # }
  detail_line <- function(detail_label, val_list, inputVal) {
    return(
      HTML(glue(
        "<b>{detail_label}:</b> {getNameByValue(val_list, inputVal)} <br>"
      ))
    )
  }
  
  selected_race <- getNameByValue(
    unlist(syso_race_ethnicity_cats(input$methodology_type)),
    input$syso_race_ethnicity
  )
  
  race_ethnicity_line <- HTML(glue(
    "<b>Race/Ethnicity:</b> {
          str_sub(
            selected_race, 
            start = str_locate(
              selected_race,
              '\\\\.'
            )[, 1] + 1,
            end = -1L
          )
        } <br>"
  ))
  
  list(
    br(),
    strong("Date Range: "),
    
    format(ReportStart(), "%m-%d-%Y"), " to ", format(ReportEnd(), "%m-%d-%Y"), br(),
    
    if (getNameByValue(syso_project_types, input$syso_project_type) != "All")
      detail_line("Project Type", syso_project_types, input$syso_project_type),
    
    detail_line("Methodology Type", syso_methodology_types, input$methodology_type),
    
    if (length(input$syso_age) != length(syso_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syso_age, collapse = ', ')} <br>"
      )),
    
    if (getNameByValue(syso_gender_cats(), input$syso_gender) != "All Genders")
      detail_line("Gender", syso_gender_cats(input$methodology_type), input$syso_gender),
    
    if (selected_race != "All Races/Ethnicities")
      race_ethnicity_line,
    
    if(getNameByValue(syso_spec_pops_people, input$syso_spec_pops) != "None")
      detail_line("Special Populations", syso_spec_pops_people, input$syso_spec_pops)
    
  )
})

output$sys_act_detail_filter_selections <- renderUI({ syso_detailBox() })
output$sys_act_summary_filter_selections <- renderUI({
  req(valid_file() == 1)
  syso_detailBox() 
})

toggle_sys_components <- function(cond) {
  # 1. toggles the filters (disabled for Composition)
  # 2. toggles subtabs and download button based if valid file has been uploaded
  # 3. moves download button to be in line with subtabs
  tabs <- c(
    "System Inflow/Outflow" = "inflow_outflow",
    "Client System Status" = "status",
    "Composition of All Served in Period" = "comp"
  )
  
  for (tab in tabs) {
    shinyjs::toggle(glue('sys_{tab}_subtabs'), condition = cond)
    shinyjs::toggle(selector = glue('#sys_{tab}_subtabs + div.tab-content'), condition = cond)
    shinyjs::toggle(glue('sys_{tab}_download_btn'), condition = cond)
    shinyjs::toggle(glue('sys_{tab}_download_btn_ppt'), condition = cond)
    
    # move download button to subtab row and only show if there's data
    shinyjs::runjs(
      glue("
          document.getElementById('sys_{tab}_subtabs')
            .insertAdjacentHTML('beforeEnd', '<li id=\"sys_{tab}_download_tab\"></li>');
          $('#sys_{tab}_download_btn').appendTo('#sys_{tab}_download_tab')
            .toggle('{cond}' == 'TRUE');
          $('#sys_{tab}_download_btn_ppt').appendTo('#sys_{tab}_download_tab')
            .toggle('{cond}' == 'TRUE');
        ")
    )
  }
}
toggle_sys_components(FALSE) # initially hide them

sys_export_summary_initial_df <- function() {
  return(data.frame(
    Chart = c(
      "Start Date",
      "End Date",
      "Methodology Type",
      "Household Type",
      "Level of Detail",
      "Project Type"
    ),
    Value = c(
      strftime(ReportStart(), "%m/%d/%y"),
      strftime(ReportEnd(), "%m/%d/%y"),
      getNameByValue(syso_methodology_types, input$methodology_type),
      getNameByValue(syso_hh_types, input$syso_hh_type),
      getNameByValue(syso_level_of_detail, input$syso_level_of_detail),
      getNameByValue(syso_project_types, input$syso_project_type)
    )
  ))
}

sys_export_filter_selections <- function() {
  return(tibble(
    Chart = c(
      "Age",
      "Special Populations",
      "Gender",
      "Race/Ethnicity"
    ),
    Value = c(
      ifelse(identical(syso_age_cats, input$syso_age), "All", input$syso_age),
      getNameByValue(syso_spec_pops_people, input$syso_spec_pops),
      getNameByValue(syso_gender_cats(input$methodology_type), input$syso_gender),
      getNameByValue(syso_race_ethnicity_cats(input$methodology_type), input$syso_race_ethnicity)
    )
  ))
}

#### FILTERS ###

# Population reactives ----------------------------------------------------

# Set race/ethnicity + gender filter options based on methodology type selection
# Set special populations options based on level of detail selection
syso_race_ethnicity_cats <- function(methodology = 1){
  ifelse(
    methodology == 1,
    list(syso_race_ethnicity_excl),
    list(syso_race_ethnicity_incl)
  )[[1]]
}

syso_gender_cats <- function(methodology = 1){
  ifelse(methodology == 1,
         list(syso_gender_excl),
         list(syso_gender_incl))[[1]]
}

font_size <- 14 / .pt