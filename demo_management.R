# This helps manage "canceling" a switch to/from demo mode
in_demo_mode_compare <- reactiveVal(FALSE)

# Tab-description message when in demo mode ----------------------------------
observeEvent(input$pageid, {
  req(input$in_demo_mode)
  req(input$continue_demo_btn)
  selectedTabId <- input$pageid
  msg <- 
    switch(selectedTabId,
           "tabUpload" = "Welcome to the Upload HMIS CSV Export page. This
             page is where users upload their hashed HMIS CSV Export and review
             any File Structure Errors in their export. In Demo Mode, you can
             see an example of the types of issues the File Structure Analysis
             identifies. ",
           
           "tabLocalSettings" = "Welcome to the Local Settings page. This
             page is where users can adjust the local settings of their uploaded
             dataset so Eva can better analyze their data in a way that is
             meaningful to their CoC. In Demo Mode, changing these settings will
             cause Eva to recalculate the data quality metrics with the selected
             parameters.",
           
           "tabClientCount" = "Welcome to the Client Counts page. This
             page helps users review the counts of households/clients served in
             each project and verify that a project is up to date on their HMIS
             data entry. In Demo Mode, you can see an example Client Counts
             report.",
           
           "tabPDDE" = "Welcome to the Project Data page. This page
             helps users review the Project Descriptor Data Element (PDDE) data
             quality issues in their HMIS data. Users can use this information
             to identify where corrections should be made in their HMIS. In Demo
             Mode, you can see an example of the types of issues the PDDE Check
             Summary identifies.",
           
           "tabDQSystem" = "Welcome to the System-wide HMIS Data Quality page.
             This page helps users review the system-level data quality issues
             in their HMIS data. Users can use this information to identify which
             organizations may benefit from additional assistance and training on
             HMIS entry. In Demo Mode, you can see an example of the types of
             data quality errors and warnings identified by Eva organized either
             by the most common issues in the overall system, or by the
             organizations with the most issues overall.",
           
           "tabDQOrg" = "Welcome to the Organization-wide HMIS Data Quality
             page. This page helps users review the organization-level data
             quality issues in their HMIS data. Users can use this information
             to identify where corrections should be made in their HMIS. In Demo
             Mode, you can see an example of the types of data quality errors
             and warnings identified by Eva organized either by the most common
             issues overall in the selected organization, or by the projects
             with the most issues overall.",
           
           "tabSystemOverview" = "Welcome to the System Performance Overview
             page. This page helps users evaluate how effective their homeless 
           system is in moving clients through the system and helping them reach 
           permanent housing. Users can also see the detailed composition 
           of all clients served their homeless system. In Demo Mode, you can see 
           examples of the System Performance charts, including data and image
           downloads."
    )
  req(msg)
  req(!isTruthy(seen_message[[selectedTabId]]))
  seen_message[[selectedTabId]] <- TRUE
  showModal(modalDialog(msg))
}) 


observeEvent(input$in_demo_mode,{
  req(in_demo_mode_compare() != input$in_demo_mode)
  show_warning_popup(input$in_demo_mode)
}, ignoreInit = TRUE)

show_warning_popup <- function(in_demo_mode) {
  if(in_demo_mode == TRUE) {
    msg <- "<p>You're currently requesting to turn on Demo Mode. Demo Mode
      allows you to explore Eva using sample HMIS data, rather than having to
      use your own HMIS CSV Export file."
    
    if(length(input$imported) > 0) {
      msg <- paste(msg, "<p>If you turn on Demo Mode now, your uploaded HMIS
        CSV Export data will be erased from Eva and replaced with the sample
        HMIS data. You will be able to re-upload your HMIS CSV
        Export file if you switch out of Demo Mode.</p>")
    } else {
      msg <- paste(msg, "You will still be able to upload your own HMIS CSV
                     Export file when you turn off Demo Mode. ")
    }
    msg <- paste0(msg,
                  "<p>Please select \"Continue\" to switch to Demo Mode</p>")
    
    showModal(
      modalDialog(
        HTML(msg),
        title = "Turn on Demo Mode?",
        footer = tagList(actionButton("continue_demo_btn", "Continue", class='btn-secondary'),
                         actionButton("stay_in_live", "Cancel"))
      )
    )
    
  } else {
    showModal(
      modalDialog(
        HTML("<p>You're currently requesting to turn off Demo Mode. When Demo Mode
          is off, the sample HMIS data will clear, and you will be able to
          explore Eva by uploading your own hashed HMIS CSV Export file.
          <p>Please select \"Continue\" to turn off Demo Mode."),
        title = "Turn off Demo Mode?",
        footer = tagList(actionButton("continue_live_btn", "Continue", class='btn-secondary'),
                         actionButton("stay_in_demo", "Cancel"))
      )
    )
  }
}

observeEvent(input$continue_demo_btn, {
  removeModal()
  toggle_demo(TRUE)
})

observeEvent(input$stay_in_demo, {

  toggle_switch(id = 'in_demo_mode', value = TRUE)
  removeModal()
  logMetadata("Chose to stay in demo mode")
})

observeEvent(input$stay_in_live, {

  toggle_switch(id = 'in_demo_mode', value = FALSE)  
  removeModal()
  logMetadata("Chose to stay in live mode")
})

observeEvent(input$continue_live_btn, {
  removeModal()
  toggle_demo(FALSE)
})

toggle_demo <- function(in_demo_mode) {
  
  if(in_demo_mode == TRUE){
    process_upload("demo.zip", here("demo.zip"))
    removeModal()
    accordion_panel_open(id = 'accordion_home', values = 'home_demo_instructions')
  } else {
    reset_app()
  }
  mode <- ifelse(in_demo_mode, 'demo', 'live')
  print(glue("Switched to {mode} mode!"))
  capture.output(glue("Switched into {mode} mode"))
  logMetadata(glue("Switched to {mode} mode"))
  
  nav_select(id = 'pageid', selected = ifelse(in_demo_mode, 'tabHome', 'tabUpload'), session = session)
  shinyjs::toggle("fileStructureAnalysis", condition = in_demo_mode)
  shinyjs::toggleState("imported", condition = !in_demo_mode)
  shinyjs::toggle('demo_banner', condition = in_demo_mode)
  shinyjs::toggle(selector = '#accordion_home [data-value=home_live_instructions]', condition = !in_demo_mode)
  shinyjs::toggle(selector = '#accordion_home [data-value=home_demo_instructions]', condition = in_demo_mode)
  in_demo_mode_compare(in_demo_mode)
}
