convert_changelog_to_tbl <- function(rmd_fname){
  ## convert md headings to tibble
  rmd <- as_tibble(parsermd::parse_rmd(rmd_fname))
  
  ## extract bullet points from list columns into text strings
  rmd$value <- sapply(1:nrow(rmd), function(x) {pluck(rmd$ast[x], 1, 'lines',.default = NA_character_)})
  
  ## collapse entries with multiple bullet points into one string, with line breaks
  rmd2 <- rmd %>% 
    rowwise() %>% 
    mutate(value = paste0(value, collapse='<br>')) %>% 
    ungroup()
  
  ## filter out non-relevant headings and clean up final columns
  rmd3 <- rmd2 %>% 
    filter(!is.na(sec_h2), !is.na(value),!is.na(sec_h3), value!="NA") %>% 
    select(Date = sec_h2, Category = sec_h3, Changes = value) %>%
    mutate(Date = as.Date(Date), 
           Category = as.factor(str_trim(Category)),
           Changes = str_squish(Changes)) 
  
  return(rmd3)  
}

output$changelog <- renderDT({
  
  changelog_dt <- convert_changelog_to_tbl('changelog.md') %>%
    fmutate(Date = format(Date, '%m-%d-%Y'))

  datatable(changelog_dt, rownames = F, escape = F, 
            options = list(dom = 'tip', 
                           # group table by date of changes
                           rowGroup = list(dataSrc=0)
                           ), 
            style = 'default',selection='none', filter='top', extensions = 'RowGroup')
  
})
