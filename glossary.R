

gloss <- readxl::read_xlsx('public-resources/system performance glossary.xlsx') %>% as_tibble()
gloss <- gloss |> mutate(Definition = str_replace_all(Definition,c('<br>'= '<br><br>','\\?' = '-', "“" = '"', '”' = '"')))
gloss <- gloss |> dplyr::filter(Term != 'Percent Difference')

# uncomment when changes are made to the csv and we need to create a new pdf for the download
#saveRDS(gloss, file = 'sandbox/glossary.rds')

output$glossary <- renderDT({
  
  datatable(
    gloss,
    rownames = FALSE,
    options = list(
      searchHighlight = TRUE
    ),
    style = "default", escape=F
  )
  
})

output$glossary_download_btn <- downloadHandler(filename = date_stamped_filename('Eva System Performance Glossary', ext = '.pdf'),
                                         content = function(file){
     file.copy('www/eva_glossary.pdf', file)
})
