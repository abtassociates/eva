toggle_sys_components <- function(toggleDownloadCond) {
  # 1. toggles the filters (disabled for Composition)
  # 2. toggles subtabs and download button based if valid file has been uploaded
  # 3. moves download button to be in line with subtabs
  tab <- switch(input$syso_tabsetpanel,
                "System Inflow/Outflow" = "inflow_outflow",
                "Client System Status" = "status",
                "Composition of All Served in Period" = "comp"
  )
  
  toggleClass(
    id = "syso_inflowoutflow_filters",
    condition = tab == "comp",
    class = "filter-disabled"
  )
  
  shinyjs::toggle(glue('sys_{tab}_subtabs'), condition = valid_file() == 1)
  shinyjs::toggle(glue('sys_{tab}_summary'), condition = valid_file() == 1)
  shinyjs::toggle(glue('sys_{tab}_insights'), condition = valid_file() == 1)
  shinyjs::toggle(glue('sys_{tab}_download_btn'), condition = valid_file() == 1)
  
  # move download button to subtab row and only show if there's data
  shinyjs::runjs(
    glue("
        document.getElementById('sys_{tab}_subtabs')
          .insertAdjacentHTML('beforeEnd', '<li id=\"sys_{tab}_download_tab\"></li>');
        $('#sys_{tab}_download_btn').appendTo('#sys_{tab}_download_tab')
          .toggle('{toggleDownloadCond}' == 'TRUE');
      ")
  )
}

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
sys_comp_selection_choices <- reactive({
  ifelse(
    input$methodology_type == 1,
    list(sys_comp_selection_choices1),
    list(sys_comp_selection_choices2)
  )[[1]]
})

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