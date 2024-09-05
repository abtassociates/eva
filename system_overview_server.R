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
    
    # move download button to subtab row and only show if there's data
    shinyjs::runjs(
      glue("
          document.getElementById('sys_{tab}_subtabs')
            .insertAdjacentHTML('beforeEnd', '<li id=\"sys_{tab}_download_tab\"></li>');
          $('#sys_{tab}_download_btn').appendTo('#sys_{tab}_download_tab')
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