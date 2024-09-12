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
    shinyjs::toggle(glue('sys_{tab}_download_btn_ppt'), condition = cond)
    
    # move download button to subtab row and only show if there's data
    shinyjs::runjs(
      glue("
          document.getElementById('sys_{tab}_subtabs')
            .insertAdjacentHTML('beforeEnd', '<li id=\"sys_{tab}_download_tab\"></li>');
          $('#sys_{tab}_download_btn').appendTo('#sys_{tab}_download_tab')
            .toggle('{cond}' == 'TRUE');
          $('#sys_{tab}_download_btn_ppt').appendTo('#sys_{tab}_download_tab')
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

sys_export_filter_selections <- function() {
  return(tibble(
    Chart = c(
      "Age",
      "Special Populations",
      "Gender",
      "Race/Ethnicity"
    ),
    Value = c(
      ifelse(identical(syso_age_cats, input$syso_age), "All", input$syso_age),
      getNameByValue(syso_spec_pops_people, input$syso_spec_pops),
      getNameByValue(syso_gender_cats(input$methodology_type), input$syso_gender),
      getNameByValue(syso_race_ethnicity_cats(input$methodology_type), input$syso_race_ethnicity)
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

font_size <- 14 / .pt


# PowerPoint Export -------------------------------------------------------
sys_overview_ppt_export <- function(file, title_slide_title, summary_items, plot_slide_title, plot1, plot2 = NULL, summary_font_size) {
  report_period <- paste0("Report Period: ", 
                          format(meta_HUDCSV_Export_Start(), "%m/%d/%Y"),
                          " - ",
                          format(meta_HUDCSV_Export_End(), "%m/%d/%Y")
  )
  loc_title <- ph_location_type(type = "title")
  loc_footer <- ph_location_type(type = "ftr")
  loc_dt <- ph_location_type(type = "dt")
  loc_slidenum <- ph_location_type(type = "sldNum")
  loc_body <- ph_location_type(type = "body")
  loc_subtitle <- ph_location_type(type = "subTitle")
  loc_ctrtitle <- ph_location_type(type = "ctrTitle")
  
  fp_normal <- fp_text(font.size = summary_font_size)
  fp_bold <- update(fp_normal, bold = TRUE)
  fp_red <- update(fp_normal, color = "red")
  
  ppt <- read_pptx()
  
  add_footer <- function(.ppt) {
    return(
      .ppt %>%
        ph_with(value = paste0("CoC Code: ", Export()$SourceID), location = loc_footer) %>%
        ph_with(value = report_period, location = loc_dt) %>%
        ph_with(
          value = paste0(
            "Export Generated: ",
            format(Sys.Date()),
            "\n",
            "https://hmis.abtsites.com/eva/"
          ),
          location = loc_slidenum
        )
    )
  }
  browser()
  # title Slide
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme") %>%
    ph_with(value = title_slide_title, location = loc_ctrtitle) %>%
    ph_with(value = "Eva Image Export", location = loc_subtitle) %>%
    add_footer()
  
  # Summary
  s_items <- do.call(block_list, lapply(1:nrow(summary_items), function(i) {
    fpar(
      ftext(paste0(summary_items$Chart[i], ": ", summary_items$Value[i]), fp_normal)
    )
  }))
  
  ppt <- add_slide(ppt, layout = "Title and Content") %>%
    ph_with(value = "Summary", location = loc_title) %>%
    ph_with(
      value = s_items,
      level_list = c(rep(1L, length(s_items))),
      location = loc_body
    ) %>% 
    add_footer()
  
  # Chart
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = plot_slide_title, location = loc_title) %>%
    ph_with(value = plot1, location = loc_body) %>%
    add_footer()
  
  if(is.null(plot2)) {
    ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = plot_slide_title, location = loc_title) %>%
      ph_with(value = plot2, location = loc_body) %>%
      add_footer()
  }
  
  # Export the PowerPoint
  return(print(ppt, target = file))
}

suppression_msg <- "The chart cannot be displayed because there are fewer than 11 clients."
no_data_msg <- "No data to show."