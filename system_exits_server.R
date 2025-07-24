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