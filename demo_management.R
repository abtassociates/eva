# Tab-description message when in demo mode ----------------------------------
observeEvent(input$sidebarmenuid, {
  req(input$in_demo_mode)
  req(input$continue_demo_btn)
  selectedTabId <- input$sidebarmenuid
  msg <- 
    switch(selectedTabId,
           "tabUpload" = "Welcome to the Upload HMIS CSV Export page. This
             page is where users upload their hashed HMIS CSV Export and review
             any File Structure Errors in their export. In Demo Mode, you can
             see an example of the types of issues the File Structure Analysis
             identifies. ",
           
           "tabLocalSettings" = "Welcome to the Edit Local Settings page. This
             page is where users can adjust the local settings of their uploaded
             dataset so Eva can better analyze their data in a way that is
             meaningful to their CoC. In Demo Mode, changing these settings will
             cause Eva to recalculate the data quality metrics with the selected
             parameters.",
           
           "tabClientCount" = "Welcome to the View Client Counts page. This
             page helps users review the counts of households/clients served in
             each project and verify that a project is up to date on their HMIS
             data entry. In Demo Mode, you can see an example Client Counts
             report.",
           
           "tabPDDE" = "Welcome to the Check Project Data page. This page
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

toggleDemoJs <- function(t) {
  js_t <- ifelse(t, 'true','false')
  shinyjs::runjs(str_glue("
      $('#home_demo_instructions').parent().parent().toggle({js_t});
      $('#home_demo_instructions').parent().css('border', '2px solid #FCB248');
      
      $('.in_demo_mode').toggle({js_t});
      
      $('#home_live_instructions').parent().parent().toggle(!{js_t});
      
      document.getElementById('isdemo').checked = {js_t};
      
      $('#imported').closest('.btn').attr('disabled',{js_t});
      
      $('#demo_banner').remove();
    "))
  
  if(t) {
    capture.output("Switching to demo mode!")
    
    # let user know things take a min to load then load the demo data
    showModal(
      modalDialog(
        "Activating demo mode...",
        title = NULL,
        footer = NULL
      )
    )
    
    process_upload("demo.zip", here("demo.zip"))
    
    valid_file(1)
    
    removeModal()
    
    nav_select(id = 'pageid', selected = 'tabHome', session = session)
    
    shinyjs::runjs(paste0(
      "var demoBannerHTML = \"<div id='demo_banner' class='in_demo_mode'>",
      "DEMO",
      "</div>\";",
      "$('header.main-header').append(demoBannerHTML);"
    ))
    
    shinyjs::runjs("$('#sidebarItemExpanded').css({
                     'top': '1.5em',
                     'position':'relative'})")
    shinyjs::hide(id = "successful_upload")
    shinyjs::disable("imported")
    
    print("Switched to demo mode!")
    logMetadata("Switched to demo mode")
    
  } else {
    capture.output("Switching to live mode")
    
    nav_select(id = 'pageid', selected = 'tabUpload', session = session)

    shinyjs::runjs("
          $('#imported').closest('.btn').removeAttr('disabled');
      ")
    shinyjs::runjs("$('#sidebarItemExpanded').css({
                     'top': '',
                     'position':''})")
    shinyjs::enable("imported")
    
    shinyjs::runjs("
      $('#imported').closest('.input-group-btn').next().val('');
      ")
    
    shinyjs::hide("fileStructureAnalysis")
    
    reset_app()
    
    session$sendInputMessage('currentProviderList', list(
      choices = NULL
      # selected = "none"
    ))
    session$sendInputMessage('orgList', list(
      choices = NULL
      # selected = "none"
    ))
    session$sendCustomMessage('dateRangeCount', list(
      min = NULL,
      start = ymd(today()),
      max = NULL,
      end = ymd(today())
    ))
    
    print("Switched into live mode!")
    capture.output("Switched into live mode")
    logMetadata("Switched into live mode")
  }
}

observeEvent(input$continue_demo_btn, {
  removeModal()
  toggleDemoJs(TRUE)
})

observeEvent(input$stay_in_demo, {
  demo_modal_closed(1)
  removeModal()
  toggle_switch(id = 'in_demo_mode', value = TRUE, session = session)
  logMetadata("Chose to stay in demo mode")
})

observeEvent(input$stay_in_live, {
  demo_modal_closed(1)
  
  removeModal()
  toggle_switch(id = 'in_demo_mode', value = FALSE, session = session)
  logMetadata("Chose to stay in live mode")
})

observeEvent(input$continue_live_btn, {
  removeModal()
  toggleDemoJs(FALSE)
})

observeEvent(input$in_demo_mode, {
  if(input$in_demo_mode == TRUE) {
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
        footer = tagList(actionButton("continue_demo_btn", "Continue"),
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
        footer = tagList(actionButton("continue_live_btn", "Continue"),
                         actionButton("stay_in_demo", "Cancel"))
      )
    )
  }
}, ignoreInit = TRUE)

shinyjs::runjs("$('#home_demo_instructions').parent().parent().hide()")
