

subpop_chart_validation <- function(show = TRUE, req = FALSE) {
  logToConsole(session, "In subpop_chart_validation")
  
  
  cond <- any(did_factors_change()) 
  
  ## whether to show validate message or not
  if(show){
    validate(
      need(
        cond,#"All Ages",
        message = "Please select a household type or one or more demographic filters to generate the subpopulation chart."
      )
    )
  } else if (req){
    ##  just hide but do not show a duplicate validate message
    req(cond)
  } else {
    ## otherwise, just return TRUE/VALSE of condition
    return(cond)
  }
}

syse_subpop_export_summary <- reactive({
  
  subpop_table_df <- get_syse_compare_subpop_data(output_type = 'table') 
  
  which_factors_changed <- names(which(did_factors_change() == 1))
  
  labels_factors_changed <- c(
    meets_hh_type = ifelse('meets_hh_type' %in% which_factors_changed && input$syse_hh_type != 'All', getNameByValue(sys_hh_types,input$syse_hh_type), NA_character_),
    meets_age_filter = ifelse('meets_age_filter' %in% which_factors_changed && length(input$syse_subpop_age) < length(sys_age_cats), paste0(input$syse_subpop_age, collapse=', '), NA_character_),
    meets_race_eth_filter = ifelse('meets_race_eth_filter' %in% which_factors_changed && input$syse_subpop_race_ethnicity1 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(1), input$syse_subpop_race_ethnicity1), collapse=','), 
                                   ifelse(input$syse_subpop_race_ethnicity2 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(2), input$syse_subpop_race_ethnicity2)) , NA_character_)),
    meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed && input$syse_subpop_spec_pops != 'None', input$syse_subpop_spec_pops, NA_character_)
  )
  labels_all_other <- c(
    meets_hh_type = 'All Other Household Types',
    meets_age_filter = 'All Other Ages',
    meets_race_eth_filter = 'All Other Races/Ethnicities',
    meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed, paste0(setdiff(c('Veteran','Non-Veteran'), input$syse_subpop_spec_pops), 's'), NA_character_)
  )
  
  if('meets_hh_type' %in% which_factors_changed){
    levels(subpop_table_df[['meets_hh_type']]) <- c(labels_factors_changed['meets_hh_type'],labels_all_other['meets_hh_type'])
  } else {
    subpop_table_df <- subpop_table_df %>% 
      fmutate(meets_hh_type = getNameByValue(sys_hh_types, input$syse_hh_type))
  }
  
  if('meets_age_filter' %in% which_factors_changed){
    levels(subpop_table_df[['meets_age_filter']]) <- c(labels_factors_changed['meets_age_filter'],labels_all_other['meets_age_filter'])
  } else {
    subpop_table_df <- subpop_table_df %>% 
      fmutate(meets_age_filter = "All Ages")
  }
  
  if('meets_race_eth_filter' %in% which_factors_changed){
    levels(subpop_table_df[['meets_race_eth_filter']]) <- c(labels_factors_changed['meets_race_eth_filter'],labels_all_other['meets_race_eth_filter'])
  } else {
    subpop_table_df <- subpop_table_df %>% 
      fmutate(meets_race_eth_filter = getNameByValue(sys_race_ethnicity_cats(input$syse_methodology_type), 
                                                     switch(input$syse_methodology_type, '1'=input$syse_subpop_race_ethnicity1,
                                                            '2' = input$syse_subpop_race_ethnicity2))
      )
  }
  
  if('meets_vet_filter' %in% which_factors_changed){
    levels(subpop_table_df[['meets_vet_filter']]) <- c(labels_factors_changed['meets_vet_filter'],labels_all_other['meets_vet_filter'])
  } else {
    subpop_table_df <- subpop_table_df %>% 
      fmutate(meets_vet_filter = getNameByValue(sys_spec_pops_people, input$syse_subpop_spec_pops))
  }
  
  export_names <- c('Household Type' = 'meets_hh_type', 'Race/Ethnicity' = 'meets_race_eth_filter',
                    'Age' = 'meets_age_filter', 'Veteran Status' = 'meets_vet_filter',
                    'Suppression Flag' = 'wasRedacted','Count' = 'N','Total System Exits' = 'total','Percent of Total System Exits' = 'pct')
  
  subpop_table_df %>% 
    fmutate(pct = ifelse(is.nan(pct), 0, pct),
            pct = scales::percent(pct, accuracy=0.1)) %>% 
    get_vars( vars=c(which_factors_changed, 'Destination Type','N','total','pct','wasRedacted')) %>% 
    arrange(pick(which_factors_changed), 'Destination Type') %>% 
    rename(any_of(export_names))
  
})

syse_subpop_export_detail <- reactive({
  
  
  ## compute subcategories of destination types for data export - not shown in chart or table
  pct_subpop_sub <- subpop() %>% 
    fselect( Destination, PersonalID, EnrollmentID) %>% 
    add_destination_type() %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_subpop = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_subpop = count_subpop /sum(count_subpop, na.rm=T))
  
  pct_comparison_sub <- everyone_else() %>% 
    fmutate(`Destination Type Detail` = living_situation(Destination)) %>%     
    fgroup_by(`Destination Type`, `Destination Type Detail`, sort = TRUE) %>% 
    fsummarize(count_comparison = GRPN()) %>% 
    fungroup() %>% 
    fmutate(pct_comparison = count_comparison/sum(count_comparison, na.rm=T))
  
  pct_subpop_totals <- pct_subpop_sub %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
               count_subpop = sum(count_subpop), 
               pct_subpop = sum(pct_subpop)) 
  
  pct_comparison_totals <- pct_comparison_sub %>% 
    fgroup_by(`Destination Type`) %>% 
    fsummarize(`Destination Type Detail` = paste0('Total ', ffirst(`Destination Type`)),
               count_comparison = sum(count_comparison), 
               pct_comparison = sum(pct_comparison)) 
  
  #full_join(pct_prev_year_sub, pct_current_year_sub) %>% 
  full_join(pct_subpop_sub %>% 
              rowbind(pct_subpop_totals), 
            pct_comparison_sub %>% 
              rowbind(pct_comparison_totals), 
            by=c('Destination Type','Destination Type Detail')) %>%    
    list_all_destinations(fill_zero = TRUE, add_totals = TRUE) %>% 
    fmutate(#pct_diff = map2_chr(count_subpop, count_comparison, .f = calc_pct_diff),
      total_count = count_subpop + count_comparison,
      pct_comparison = scales::percent(pct_comparison, accuracy = 0.1,scale=100),
      pct_subpop = scales::percent(pct_subpop, accuracy = 0.1,scale=100)) %>% 
    fselect(`Destination Type`, `Destination Type Detail`, 'Subpopulation %' = pct_subpop, 'Subpopulation Count' = count_subpop, 
            #'Percent Difference' = pct_diff, 
            'Everyone Else %' = pct_comparison, 'Everyone Else Count' = count_comparison)#, 'Total Count' = total_count)
})

syse_subpop_selections <- reactive({
  possible <- c("Age","Race/Ethnicity","Veteran Status (Adult Only)")
  selected <- which(c(input$syse_subpop_age_selection, input$syse_subpop_race_eth_selection, input$syse_subpop_vet_selection))
  
  vals <- possible[selected]
  if("Race/Ethnicity" %in% vals){
    vals[vals == "Race/Ethnicity"] <- c("All Races/Ethnicities","Grouped Races/Ethnicities")[as.numeric(input$syse_methodology_type)]
  }
  
  vals
})

did_factors_change <- reactive({
  c(
    meets_hh_type = (input$syse_hh_type != 'All'),
    meets_age_filter = ('Age' %in% syse_subpop_selections() && length(input$syse_subpop_age) < length(sys_age_cats)),
    meets_race_eth_filter = ('All Races/Ethnicities' %in% syse_subpop_selections() && input$syse_subpop_race_ethnicity1 != 'All') +
      ('Grouped Races/Ethnicities' %in% syse_subpop_selections() && input$syse_subpop_race_ethnicity2 != 'All'),
    meets_vet_filter = ('Veteran Status (Adult Only)' %in% syse_subpop_selections() && input$syse_subpop_spec_pops != 'None')
  )
})

compute_subpop_and_everyone_else <- function(input_df){
  
  subpop_w_client_filters <- input_df
  
  if(input$syse_hh_type != 'All'){
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_hh_type = HouseholdType %in% input$syse_hh_type)
  } else {
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_hh_type = TRUE)
  }
  
  if('Age' %in% syse_subpop_selections()){
    req(input$syse_subpop_age)
    
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_age_filter = AgeCategory %in% input$syse_subpop_age)
    
  } else {
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_age_filter = TRUE)
  }
  
  if('All Races/Ethnicities' %in% syse_subpop_selections()){
    req(input$syse_subpop_race_ethnicity1)
    
    subpop_race_eth <- subpop_w_client_filters[ (if(input$syse_subpop_race_ethnicity1 == "All") rep(TRUE, .N) else get(input$syse_subpop_race_ethnicity1) == 1)]
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_race_eth_filter = PersonalID %in% subpop_race_eth$PersonalID)
  } else if('Grouped Races/Ethnicities' %in% syse_subpop_selections()){
    req(input$syse_subpop_race_ethnicity2)
    subpop_race_eth <- subpop_w_client_filters[ (if(input$syse_subpop_race_ethnicity2 == "All") rep(TRUE, .N) else get(input$syse_subpop_race_ethnicity2) == 1)]
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_race_eth_filter = PersonalID %in% subpop_race_eth$PersonalID)
  } else {
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_race_eth_filter = TRUE)
  }
  
  if('Veteran Status (Adult Only)' %in% syse_subpop_selections()){
    req(input$syse_subpop_spec_pops)
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_vet_filter =input$syse_subpop_spec_pops == "None" |
                (input$syse_subpop_spec_pops == "Veteran" &
                   VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")) |
                (input$syse_subpop_spec_pops == "NonVeteran" &
                   VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17")))
  } else {
    subpop_w_client_filters <- subpop_w_client_filters %>% 
      fmutate(meets_vet_filter = TRUE)
  }
  
  
  
  subpop_out <- subpop_w_client_filters[meets_hh_type & meets_age_filter & meets_race_eth_filter & meets_vet_filter]
  
  rest_of_pop <- subpop_w_client_filters[!meets_hh_type | !meets_age_filter | !meets_race_eth_filter | !meets_vet_filter] %>% 
    fmutate(meets_hh_type = factor(meets_hh_type, levels=c(T,F)),
            meets_age_filter = factor(meets_age_filter, levels=c(T,F)),
            meets_race_eth_filter = factor(meets_race_eth_filter, levels=c(T,F)),
            meets_vet_filter = factor(meets_vet_filter, levels=c(T,F)))
  return(list(subpop = subpop_out, everyone_else = rest_of_pop))
  
}

comps <- reactive({
  compute_subpop_and_everyone_else(all_filtered_syse_subpop())
})

subpop <- reactive({comps()$subpop})

everyone_else <- reactive({
  comps()$everyone_else %>% 
    add_destination_type()
})

get_syse_compare_subpop_data <- function(output_type = 'table'){
  
  validate(need(nrow(subpop()) > 0, no_data_msg))
  validate(need(nrow(subpop()) > 10, suppression_msg))
  
  validate(need(nrow(everyone_else()) > 0, no_data_msg))
  validate(need(nrow(everyone_else()) > 10, suppression_msg))
  
  
  df_subpop <- subpop() %>% 
    add_destination_type(as_factor = TRUE)
  
  .total_s <- fnrow(df_subpop)
  
  count_subpop <- df_subpop %>%
    count(`Destination Type`, .drop=FALSE, name='N') %>% 
    fmutate(total = fsum(N), wasRedacted = total < 10, pct = ifelse(wasRedacted * (output_type == "chart"), NA, N / total))
  
  .total_e <- fnrow(everyone_else())
  
  count_df <- expand.grid(
    meets_hh_type = c(TRUE, FALSE),
    meets_age_filter = c(TRUE, FALSE),
    meets_race_eth_filter = c(TRUE, FALSE),
    meets_vet_filter = c(TRUE, FALSE)
  )
  
  filt_vars <- c('meets_hh_type','meets_age_filter','meets_race_eth_filter','meets_vet_filter')
  which_factors_changed <- names(which(did_factors_change() == 1))
  
  count_everyone_else <- everyone_else() %>%
    count(`Destination Type`, meets_hh_type, meets_age_filter, meets_race_eth_filter, meets_vet_filter,.drop=F,name='N') %>%
    fgroup_by(meets_hh_type, meets_age_filter, meets_race_eth_filter, meets_vet_filter) %>% 
    #fmutate(pct = N / fsum(N)) %>% 
    fmutate(total = fsum(N), wasRedacted = total < 10, pct = ifelse(wasRedacted * (output_type == "chart"), NA, N / total)) %>% 
    fungroup() %>%
    dplyr::filter(if_all(setdiff(filt_vars, which_factors_changed), ~ .x == TRUE)) %>% 
    fsubset(((meets_hh_type==T) + (meets_age_filter==T) + (meets_race_eth_filter==T) + (meets_vet_filter==T)) < 4)
  
  # which_factors_changed <- names(which(did_factors_change() == 1))
  # labels_factors_changed <- c(
  #   meets_hh_type = ifelse('meets_hh_type' %in% which_factors_changed && input$syse_hh_type != 'All', getNameByValue(sys_hh_types,input$syse_hh_type), NA_character_),
  #   meets_age_filter = ifelse('meets_age_filter' %in% which_factors_changed && length(input$syse_subpop_age) < length(sys_age_cats), paste0(input$syse_subpop_age, collapse=', '), NA_character_),
  #   meets_race_eth_filter = ifelse('meets_race_eth_filter' %in% which_factors_changed && input$syse_subpop_race_ethnicity1 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(1), input$syse_subpop_race_ethnicity1), collapse=','), 
  #                                  ifelse(input$syse_subpop_race_ethnicity2 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(2), input$syse_subpop_race_ethnicity2)) , NA_character_)),
  #   meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed && input$syse_subpop_spec_pops != 'None', input$syse_subpop_spec_pops, NA_character_)
  # )
  # labels_all_other <- c(
  #   meets_hh_type = 'All Other Household Types',
  #   meets_age_filter = 'All Other Ages',
  #   meets_race_eth_filter = 'All Other Races/Ethnicities',
  #   meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed, paste0(setdiff(c('Veteran','Non-Veteran'), input$syse_subpop_spec_pops), 's'), NA_character_)
  # )
  
  if(output_type == 'chart'){
    
    rowbind(
      count_subpop %>% fmutate(meets_hh_type = TRUE, meets_age_filter = TRUE, 
                               meets_race_eth_filter = TRUE, meets_vet_filter = TRUE, group = 'subpop'),
      count_everyone_else %>% fmutate(group = 'everyone_else')
    )
    
    
  } else if (output_type == 'table'){
    
    rowbind(
      count_subpop %>% fmutate(meets_hh_type = TRUE, meets_age_filter = TRUE, 
                               meets_race_eth_filter = TRUE, meets_vet_filter = TRUE, group = 'subpop'),
      count_everyone_else %>% fmutate(group = 'everyone_else')
    )
  }
}

observeEvent(input$syse_subpop_age_selection,
             {
               if(isTruthy(input$syse_subpop_age_selection)){
                 shinyjs::enable(id = 'age_picker')
               } else {
                 shinyjs::disable(id = 'age_picker')
               }                 
             })

observeEvent(input$syse_subpop_race_eth_selection,
             {
               if(isTruthy(input$syse_subpop_race_eth_selection)){
                 shinyjs::enable(id = 'race_eth_picker')
               } else {
                 shinyjs::disable(id = 'race_eth_picker')
               }                 
             }, ignoreInit=F)

observeEvent(input$syse_subpop_vet_selection,
             {
               if(isTruthy(input$syse_subpop_vet_selection)){
                 shinyjs::enable(id = 'vet_picker')
               } else {
                 shinyjs::disable(id = 'vet_picker')
               }                 
             })


observeEvent(syse_subpop_selections(),{
  
  str_vec <- c('Age','Races/Ethnicities','Veteran Status')
  excl_vec <- c('age','race_eth','vet')
  if(length(syse_subpop_selections()) == 2){
    
    excl <- which(!sapply(str_vec, \(x) any(str_detect(syse_subpop_selections(), x)) ,USE.NAMES = F))
    shinyjs::disable(id = paste0('syse_subpop_',excl_vec[excl],'_selection'))
  } else {
    shinyjs::enable(id='syse_subpop_age_selection')
    shinyjs::enable(id='syse_subpop_race_eth_selection')
    shinyjs::enable(id='syse_subpop_vet_selection')
  }
})

output$syse_compare_subpop_filter_selections <-renderUI({ 
  
  req(session$userData$valid_file() == 1 & did_factors_change())
  
  sys_detailBox(selection = syse_subpop_selections(),
                detail_type = 'subpop',
                methodology_type = input$syse_methodology_type,
                startDate = session$userData$ReportStart,
                endDate = session$userData$ReportEnd,
                age = input$syse_subpop_age,
                spec_pops = input$syse_subpop_spec_pops,
                race_eth = input[[glue('syse_subpop_race_ethnicity{input$syse_methodology_type}')]])
})

syse_compare_subpop_chart <- function(subpop_data = get_syse_compare_subpop_data(output_type = 'chart'),
                                      dest_type = input$subpop_dest_type, isExport = FALSE){
  req(all_filtered_syse_subpop())
  
  subgroup_colors <- c(
    'subpop' = get_brand_color('med_purple'),
    'everyone_else' = get_brand_color('med_grey2')
  )
  ## long format needed for plotting points
  subpop_chart_df <- subpop_data %>% 
    fsubset(`Destination Type` == dest_type)
  
  title <- paste0("Total System Exits for ", 
                  c(paste0('Subpopulation: ', scales::label_comma()(nrow(subpop()))),
                    paste0('Everyone Else: ', scales::label_comma()(nrow(everyone_else())))),
                  collapse='\n'
  )
  
  num_factors_changed <- sum(did_factors_change())
  which_factors_changed <- names(which(did_factors_change() == 1))
  
  labels_factors_changed <- c(
    meets_hh_type = ifelse('meets_hh_type' %in% which_factors_changed && input$syse_hh_type != 'All', getNameByValue(sys_hh_types,input$syse_hh_type), NA_character_),
    meets_age_filter = ifelse('meets_age_filter' %in% which_factors_changed && length(input$syse_subpop_age) < length(sys_age_cats), paste0(input$syse_subpop_age, collapse=', '), NA_character_),
    meets_race_eth_filter = ifelse('meets_race_eth_filter' %in% which_factors_changed && input$syse_subpop_race_ethnicity1 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(1), input$syse_subpop_race_ethnicity1), collapse=','), 
                                   ifelse(input$syse_subpop_race_ethnicity2 != 'All', paste0(getNameByValue(sys_race_ethnicity_cats(2), input$syse_subpop_race_ethnicity2)) , NA_character_)),
    meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed && input$syse_subpop_spec_pops != 'None', input$syse_subpop_spec_pops, NA_character_)
  )
  labels_all_other <- c(
    meets_hh_type = 'All Other Household Types',
    meets_age_filter = 'All Other Ages',
    meets_race_eth_filter = 'All Other Races/Ethnicities',
    meets_vet_filter = ifelse('meets_vet_filter' %in% which_factors_changed, paste0(setdiff(c('Veteran','Non-Veteran'), input$syse_subpop_spec_pops), 's'), NA_character_)
  )
  
  
  
  if(num_factors_changed == 1){
    
    levels(subpop_chart_df[[which_factors_changed]]) <- c(labels_factors_changed[which_factors_changed],labels_all_other[which_factors_changed])
    
    g <- ggplot(subpop_chart_df, aes(x=!!sym(which_factors_changed), y=1))
    
  } else if(num_factors_changed == 2){
    
    if('meets_hh_type' %in% which_factors_changed){
      
      other_factor <- setdiff(which_factors_changed, 'meets_hh_type')
      
      ## horizontal variable
      levels(subpop_chart_df[['meets_hh_type']]) <- c(labels_factors_changed['meets_hh_type'],labels_all_other['meets_hh_type'])
      
      ## horizontal variable
      levels(subpop_chart_df[[other_factor]]) <- c(labels_factors_changed[other_factor],labels_all_other[other_factor])
      
      g <- ggplot(subpop_chart_df, aes(x=meets_hh_type, y=fct_rev(!!sym(other_factor))))
      
    } else if ('meets_race_eth_filter' %in% which_factors_changed) {
      other_factor <- setdiff(which_factors_changed, 'meets_race_eth_filter')
      
      ## horizontal variable
      levels(subpop_chart_df[[other_factor]]) <- c(labels_factors_changed[other_factor],labels_all_other[other_factor])
      
      ## vertical variable
      levels(subpop_chart_df[['meets_race_eth_filter']]) <- c(labels_factors_changed['meets_race_eth_filter'],labels_all_other['meets_race_eth_filter'])
      
      g <- ggplot(subpop_chart_df, aes(x=!!sym(other_factor), y=fct_rev(meets_race_eth_filter))
      )        
    } else {
      
      ## horizontal variable
      levels(subpop_chart_df[[which_factors_changed[1]]]) <- c(labels_factors_changed[which_factors_changed[1]],labels_all_other[which_factors_changed[1]])
      
      ## vertical variable
      levels(subpop_chart_df[[which_factors_changed[2]]]) <- c(labels_factors_changed[which_factors_changed[2]],labels_all_other[which_factors_changed[2]])
      
      g <- ggplot(subpop_chart_df, aes(x=!!sym(which_factors_changed[1]), y=fct_rev(!!sym(which_factors_changed[2])))) 
      
    }
    
  } else if(num_factors_changed == 3){
    
    if('meets_race_eth_filter' %in% which_factors_changed){
      vert_var <- 'meets_race_eth_filter'
      if('meets_hh_type' %in% which_factors_changed){
        horiz_var_outer <- 'meets_hh_type'
        horiz_var_inner <- setdiff(which_factors_changed, c('meets_race_eth_filter','meets_hh_type'))
      } else {
        other_factors <- setdiff(which_factors_changed, vert_var)
        horiz_var_inner <- other_factors[1]#setdiff(which_factors_changed, c('meets_race_eth_filter','meets_hh_type'))
        horiz_var_outer <- other_factors[2]
      }
    } else {
      horiz_var_inner <- which_factors_changed[1]
      horiz_var_outer <- which_factors_changed[2]
      vert_var <- which_factors_changed[3]
      #other_factors <- which_factors_changed[1:2]
    }
    
    ## horizontal variable
    levels(subpop_chart_df[[horiz_var_inner]]) <- c(labels_factors_changed[horiz_var_inner],labels_all_other[horiz_var_inner])
    
    ## vertical variable
    levels(subpop_chart_df[[vert_var]]) <- c(labels_factors_changed[vert_var],labels_all_other[vert_var])
    
    ## facet variable
    levels(subpop_chart_df[[horiz_var_outer]]) <- c(labels_factors_changed[horiz_var_outer],labels_all_other[horiz_var_outer])
    
    g <- ggplot(subpop_chart_df, aes(x=!!sym(horiz_var_inner), y=fct_rev(!!sym(vert_var))))
    
    g <- g + facet_wrap(as.formula(paste0('~ ', horiz_var_outer)), strip.position='top',scales='free_x', ncol=2,labeller = label_wrap_gen(18))
  }
  
  g <- g + 
    geom_tile(color='#f0f0f0', lwd=0.5, linetype=1, aes(fill = group)) +
    scale_fill_manual(
      values = subgroup_colors
    ) +
    geom_text(
      aes(label = ifelse(wasRedacted, "***", paste0(scales::percent(pct, accuracy = 1), '\n', '(',N,' of ',total,')'))),#scales::comma(n))),
      size = sys_chart_text_font,
      color = "black"
    ) +
    scale_x_discrete(position='top', labels = label_wrap(25), expand = c(0,0)) +
    scale_y_discrete(labels = label_wrap(25), expand = c(0,0)) +
    labs(x='', y='', title = title) +
    theme(panel.spacing = unit(0, "lines"),
          strip.background = element_blank(),
          plot.title = element_text(size=sys_chart_title_font, hjust = 0.5),
          axis.line = element_blank(),
          panel.grid.major.y =element_blank(),
          strip.placement = "outside",
          strip.text.x.top = element_text(size=sys_axis_text_font, angle=0),
          axis.ticks = element_blank(),
          legend.position = 'none',
          axis.text.x = element_text(size=get_adj_font_size(sys_axis_text_font, isExport)),
          axis.text.y = element_text(size=sys_axis_text_font, hjust=1),#, hjust = 1),
          panel.background = element_rect(fill = 'white', colour = 'white')
    )
  
  g
  
}


output$syse_compare_subpop_chart <- renderPlot({
  ## check if filters have been changed from defaults before showing 
  subpop_chart_validation(show=TRUE, req=FALSE)
  syse_compare_subpop_chart(get_syse_compare_subpop_data(output_type = 'chart'),
                            dest_type = input$subpop_dest_type)
})

output$syse_subpop_download_btn <- downloadHandler(filename = date_stamped_filename("System Exits by Subpopulation Report - "),
                                                   content = function(file) {
                                                     logToConsole(session, "System Exits by Subpopulation data download")
                                                     
                                                     sheets <- list(
                                                       "SystemExitsBySubpop Metadata" = sys_export_summary_initial_df(type = 'exits') %>%
                                                         rowbind(
                                                           sys_export_filter_selections(type = 'exits_subpop'),
                                                           data.table(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
                                                                      Value = scales::label_comma()(c(nrow(subpop()),nrow(everyone_else())))
                                                           )
                                                         ) %>% 
                                                         frename("System Exits by Subpopulation" = Value),
                                                       "SubpopulationComparisonSummary" = syse_subpop_export_summary(),
                                                       "SubpopulationExitDetail" = syse_subpop_export_detail()
                                                     )
                                                     
                                                     write_xlsx(
                                                       sheets,     
                                                       path = file,
                                                       format_headers = FALSE,
                                                       col_names = TRUE
                                                     )   
                                                     
                                                     logMetadata(session, paste0("Downloaded System Exits Tabular Data: ", input$syse_tabbox,
                                                                                 if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
                                                   })

output$syse_subpop_download_btn_ppt <- downloadHandler(filename = function(){
  paste("System Exits by Subpopulation_", Sys.Date(), ".pptx", sep = "")
},
content = function(file) {
  sys_perf_ppt_export(file = file,
                      type = 'exits_comparison',
                      title_slide_title = "System Exits by Subpopulation",
                      summary_items = list(
                        "Summary" = sys_export_summary_initial_df(type = 'exits') %>%
                          rowbind(
                            sys_export_filter_selections(type = 'exits_subpop'),
                            data.table(Chart = c('Total System Exits for Subpopulation', 'Total System Exits for Everyone Else'),
                                       Value = scales::label_comma()(c(nrow(subpop()),nrow(everyone_else())))
                            )
                          )
                      ),
                      plots = list(
                        "System Exits by Subpopulation - Permanent" =  syse_compare_subpop_chart(dest_type = 'Permanent',isExport = TRUE),
                        "System Exits by Subpopulation - Homeless" =  syse_compare_subpop_chart(dest_type = 'Homeless', isExport = TRUE),
                        "System Exits by Subpopulation - Institutional" =  syse_compare_subpop_chart(dest_type = 'Institutional',isExport = TRUE),
                        "System Exits by Subpopulation - Temporary" =  syse_compare_subpop_chart(dest_type = 'Temporary',isExport = TRUE),
                        "System Exits by Subpopulation - Other/Unknown" =  syse_compare_subpop_chart(dest_type = 'Other/Unknown',isExport = TRUE)
                      ),
                      summary_font_size = 19,
                      startDate = session$userData$ReportStart,
                      endDate = session$userData$ReportEnd,
                      sourceID = session$userData$Export$SourceID,
                      in_demo_mode = input$in_demo_mode
  )
  
})