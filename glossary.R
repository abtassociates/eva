

gloss <- readxl::read_xlsx('public-resources/system performance glossary.xlsx') %>% as.tibble()

# uncomment when changes are made to the csv and we need to create a new pdf for the download
#saveRDS(gloss, file = 'sandbox/glossary.rds')

output$glossary <- renderDT({
  
  datatable(
    gloss,
    rownames = FALSE,
    options = list(
      searchHighlight = TRUE
    ),
    style = "default"
  )
  
})

output$glossary_download_btn <- downloadHandler(filename = date_stamped_filename('System Performance Glossary', ext = '.pdf'),
                                         content = function(file){
     file.copy('www/eva_glossary.pdf', file)
})
