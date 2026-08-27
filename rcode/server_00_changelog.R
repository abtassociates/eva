convert_changelog_to_tbl <- function(md_fname){
  
  chg_text <- readLines(md_fname)
  
  ## h3 = Category (Bug Fix, Feature Enhancement, Miscellaneous, etc.)
  h3 <- stringr::str_starts(chg_text,'###')
  ## h2 = date
  h2 <- stringr::str_starts(chg_text,'##') & !h3
  ## h1 = title (should just be one value, "Changelog")
  h1 <- stringr::str_starts(chg_text,'#') & !h3 & !h2
  
  ## items are bullet points
  item <- stringr::str_starts(chg_text, ' - ')
  ## sub-items are sub-bullet points
  sub_item <- stringr::str_starts(chg_text, '     - ')
  ## empty rows will be ignored
  blank <- chg_text %in% c('',' ')

  chg_df <- data.frame(line_num = 1:length(chg_text),
                       type = case_when(
                         h3 ~ 'h3',
                         h2 ~ 'h2',
                         h1 ~ 'h1',
                         item ~ 'item',
                         sub_item ~ 'sub_item',
                         blank ~ 'blank'
                       ),
                       text = chg_text)
  
  chg_df2 <- chg_df |> 
    filter(type != 'blank') |> 
    mutate(sec_h2 = ifelse(type == 'h2', str_remove(text,'## ') , NA), 
           sec_h3 = ifelse(type == 'h3', str_remove(text,'### '), NA)) |> 
    fill(sec_h2,sec_h3, .direction = 'down') |> 
    mutate(sec_h3 = ifelse(type == 'h2', NA, sec_h3)) |> 
    ## collapse entries with multiple bullet points into one string, with line breaks
    group_by(sec_h2,sec_h3, type) |> 
    mutate(value = ifelse(type %in% c('item','sub_item'), paste0(text, collapse='<br>'), NA)) |> 
    ungroup()
  
  chg_df3 <- chg_df2 %>% 
    ## filter out non-relevant headings and clean up final columns
    filter(!is.na(sec_h2), !is.na(value),!is.na(sec_h3), value!="NA") %>% 
    group_by(sec_h2,sec_h3,type) |> 
    summarize(first_text=first(value)) |> 
    ungroup() |> 
    select(-type) |> 
    select(Date = sec_h2, Category = sec_h3, Changes = first_text) |> 
    mutate(Date = as.Date(Date), 
           Category = as.factor(str_trim(Category)),
           Changes = str_squish(Changes)) |> 
    arrange(desc(Date))
  
  return(chg_df3)  
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
