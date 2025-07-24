
#### DISPLAY FILTER SELECTIONS ###
syse_detailBox <- reactive({
  list(
    br(),
    strong("Date Range: "),
    
    format(session$userData$ReportStart, "%m-%d-%Y"), " to ", format(session$userData$ReportEnd, "%m-%d-%Y"), br(),
    
    if (input$syse_project_type != "All")
      chart_selection_detail_line("Project Type Group", syse_project_types, str_remove(input$syse_project_type, "- ")),
    
    #detail_line for "Methodology Type" where only the first part of the label before the : is pulled in
    HTML(glue(
      "<b>Methodology Type:</b> {str_sub(getNameByValue(syse_methodology_types, input$syse_methodology_type), start = 1, end = 8)} <br>"
    )),
    
    if (length(input$syse_age) != length(syse_age_cats))
      HTML(glue(
        "<b>Age:</b> {paste(input$syse_age, collapse = ', ')} <br>"
      )),
    
    if (input$syse_race_ethnicity != "All")
      chart_selection_detail_line("Race/Ethnicity", syse_race_ethnicity_cats(input$syse_methodology_type), input$syse_race_ethnicity),
    
    if(getNameByValue(syse_spec_pops_people, input$syse_spec_pops) != "All Statuses")
      HTML(glue(
        "<b>Veteran Status:</b> {paste(getNameByValue(syse_spec_pops_people, input$syse_spec_pops), '(Adult Only)')} <br>"
      ))
    
  )
})

output$syse_types_filter_selections <- renderUI({ 
  req(session$userData$valid_file() == 1)
  syse_detailBox() 
})


output$syse_types_download_btn <- downloadHandler(filename = 'tmp',{

  })

output$syse_types_download_btn_ppt <- downloadHandler(filename = 'tmp', {
  
})

output$syse_compare_download_btn <- downloadHandler(filename = 'tmp',{
  
})

output$syse_compare_download_btn_ppt <- downloadHandler(filename = 'tmp',{
  
})

output$syse_phd_download_btn <- downloadHandler(filename = 'tmp',{
  
})

output$syse_phd_download_btn_ppt <- downloadHandler(filename = 'tmp',{
  
})

toggle_syse_components <- function(cond, init=FALSE) {
  # 1. toggles the filters (disabled for Composition)
  # 2. toggles subtabs and download button based if valid file has been uploaded
  # 3. moves download button to be in line with subtabs
  tabs <- c(
    "System Exit Types" = "types",
    "System Exit Comparisons" = "compare",
    "Permanent Housing Demographics" = "phd"
  )
  
  for (tab in tabs) {
    shinyjs::toggle(glue('syse_{tab}_subtabs'), condition = cond)
    shinyjs::toggle(selector = glue('#syse_{tab}_subtabs + div.tab-content'), condition = cond)
    shinyjs::toggle(glue('syse_{tab}_download_btn'), condition = cond)
    shinyjs::toggle(glue('syse_{tab}_download_btn_ppt'), condition = cond)
    
    # move download button to subtab row and only show if there's data
    if(init) {
      shinyjs::runjs(
        glue("
            document.getElementById('syse_{tab}_subtabs')
              .insertAdjacentHTML('beforeEnd', '<li class=\"syse_download_tab\" id=\"syse_{tab}_download_tab\"></li>');
            $('#syse_{tab}_download_btn').appendTo('#syse_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
            $('#syse_{tab}_download_btn_ppt').appendTo('#syse_{tab}_download_tab')
              .toggle('{cond}' == 'TRUE');
          ")
      )
    }
  }
  
  shinyjs::toggle('syse_client_level_download_btn', condition = cond)
  if(init) {
    shinyjs::runjs("
      document.getElementById('syse_tabbox')
        .insertAdjacentHTML('beforeEnd', '<li class=\"syse_download_tab\" id=\"syse_client_level_download_tab\"></li>');
      $('#syse_client_level_download_btn').appendTo('#syse_client_level_download_tab')
        .toggle('{cond}' == 'TRUE');
    ")
  }
  
}
toggle_syse_components(FALSE, init=TRUE) # initially hide them