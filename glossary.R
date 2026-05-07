
gloss <- readxl::read_xlsx('public-resources/system performance glossary.xlsx') %>% as.tibble()

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