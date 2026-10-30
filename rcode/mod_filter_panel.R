
mod_filter_panel_ui <- function(id, flabels = set_filter_labels(), 
                          fchoices = set_filter_choices(), fwidths=c(6,6)) {
  ns <- NS(id)
  
  left_card <- card(
    id = ns('filters_left'),
    style='border-width:0;border-radius:0',
    layout_columns(
      col_widths = c(4,4,4,6,6),
      fill = T,
      evaPickerInput(id = ns('hh_type'), label = flabels["hh_type"], choices = fchoices[["hh_type"]]),
      evaPickerInput(id = ns('level_of_detail'), label = flabels["level_of_detail"], choices = fchoices[["level_of_detail"]]),
      evaPickerInput(id = ns('project_type'), label = flabels["project_type"], choices = fchoices[["project_type"]]),
      
      evaPickerInput(
        id = ns('age'),
        label = flabels["age"],
        choices = fchoices[["age"]],
        selected = fchoices[["age"]],
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          selectedTextFormat = paste("count >", length(fchoices[["age"]])-1),
          countSelectedText = "All Ages",
          noneSelectedText = "All Ages",
          container = "body"
        )
      ),
      evaPickerInput(id = ns('spec_pops'), label = flabels["spec_pops"], choices = fchoices[["spec_pops"]])
    )
  )
  
  right_card <- card(
    id = ns('filters_right'),
    style="border-width:0;border-left-width: 1px; border-radius:0",
    layout_columns(
      col_widths = c(12,12),
      evaPickerInput(id = ns('methodology_type'), label = flabels["methodology_type"], choices = fchoices[["methodology_type"]]),
      evaPickerInput(
        id = ns('race_ethnicity'),
        label = flabels["race_ethnicity"],
        choices = fchoices[["race_ethnicity"]],
        selected = fchoices[["race_ethnicity"]],
        options = list(
          `dropdown-align-right` = TRUE,
          `dropup-auto` = FALSE,
          container = "body"
        )
      )
    )
  )
  
  card(
    card_header(headerCard('Filters')),
    layout_columns(
      col_widths=fwidths,
      gap = 0,
      left_card,
      right_card
    )
  )
  
}

mod_filter_panel_server <- function(id, global_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      #ns <- session$ns
      
      filtered_clients <- reactive({
        logToConsole(session, glue("In {id}_client_categories_filtered"))
        req(!is.null(global_vars$imported_name) | isTRUE(global_vars$in_demo_mode))
        req(nrow(session$userData$client_categories) > 0)
        
        get_clients_filtered(input, session)
      })
      
      filtered_enrollments <- reactive({
        logToConsole(session, glue("In {id}_enrollment_categories_filtered"))
        req(!is.null(global_vars$imported_name) | isTRUE(global_vars$in_demo_mode))
        req(nrow(session$userData$enrollment_categories) > 0)
        
        get_enrollments_filtered(
          enrollment_cats      = session$userData$enrollment_categories,
          client_cats          = session$userData$client_categories,
          syse_hh_type         = input$hh_type, # Dynamic input access
          syse_level_of_detail = input$level_of_detail,
          syse_project_type    = input$project_type,
          filter_hh_type       = TRUE,
          filter_pt            = FALSE 
        )
    
        # hh_val <- input$hh_type
        # lod_val <- input$level_of_detail
        # pt_val <- input$project_type
        # vet_val <- input$spec_pops
        # 
        # join(session$userData$enrollment_categories,
        #      session$userData$client_categories |> fselect(PersonalID, VeteranStatus),
        #      on='PersonalID', how='left'
        # ) |> 
        #   fsubset(
        #     # Household type filter
        #     (hh_val == "All" |
        #        (hh_val == "YYA" & HouseholdType %in% c("PY", "UY")) |
        #        (hh_val == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
        #        (hh_val == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
        #        (hh_val == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
        #        hh_val == HouseholdType)
        #     &
        #       
        #       # Level of detail filter
        #       (lod_val == "All" |
        #          (lod_val == "HoHsAndAdults" &
        #             (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
        #          (lod_val == "HoHsOnly" &
        #             CorrectedHoH == 1)) #&
        #     
        #     # Project type filter (wrapped in parentheses to preserve logical order of operations)
        #     # (pt_val == "All" |
        #     #    (pt_val %in% c("LHRes", "AllRes") & ProjectType %in% lh_residential_project_types) |
        #     #    (pt_val %in% c("PHRes", "AllRes") & ProjectType %in% ph_project_types) |
        #     #    (pt_val == "SO" & ProjectType == sso_project_type) |
        #     #    (pt_val == "AllNonRes" & ProjectType %in% non_res_project_types))
        #   ) |>
        #   fselect(-VeteranStatus)
      })
      
      return(
        list(
          client = filtered_clients, 
          enrl = filtered_enrollments,
          filter_vals = list(age = reactive(input$age), hh_type = reactive(input$hh_type), level_of_detail = reactive(input$level_of_detail),
                        project_type = reactive(input$project_type), spec_pops = reactive(input$spec_pops), 
                        race_ethnicity = reactive(input$race_ethnicity), methodology_type = reactive(input$methodology_type)
                        )
        )
      )
    }
  )
}

## function to initialize pickerInput to construct Sys Perf filters
evaPickerInput <- function(id, label, choices, options= pickerOptions(container = "body"), multiple = FALSE, selected = choices[1]){
  
  pickerInput(
    inputId = id,
    label = label,
    multiple = multiple,
    choices = choices,
    selected = selected,
    options = options
  )
}

get_clients_filtered <- function(input, session){
  
  filtered_clients <- session$userData$client_categories %>%
    fsubset(
      AgeCategory %in% input$age &
        (if(input$race_ethnicity == "All") rep(TRUE, fnrow(session$userData$client_categories)) else get(input$race_ethnicity) == 1) & 
        (
          input$spec_pops == "None" | (
            input$spec_pops == "Veteran" &
              VeteranStatus == 1 & !AgeCategory %in% c("0 to 12", "13 to 17")
          ) | (
            input$spec_pops == "NonVeteran" &
              VeteranStatus == 0 & !AgeCategory %in% c("0 to 12", "13 to 17")
          )
        )
    )
  filtered_clients
}

get_enrollments_filtered <- function(
    enrollment_cats,
    client_cats,
    syse_hh_type,
    syse_level_of_detail,
    syse_project_type,
    lh_res_types = lh_residential_project_types,
    ph_types = ph_project_types,
    out_type = out_project_type,
    non_res_types = non_res_project_types,
    filter_hh_type = TRUE,
    filter_pt = TRUE
) {
  
  join(
    enrollment_cats,
    client_cats %>% fselect(PersonalID, VeteranStatus),
    on = "PersonalID", 
    how = "inner"
  ) %>%
    fsubset(
      # Household type filter
      (if (filter_hh_type) {
        (syse_hh_type == "All" |
           (syse_hh_type == "YYA" & HouseholdType %in% c("PY", "UY")) |
           (syse_hh_type == "YYA" & HouseholdType == "CO" & VeteranStatus != 1) | 
           (syse_hh_type == "AO" & HouseholdType %in% c("AOminusUY","UY")) | 
           (syse_hh_type == "AC" & HouseholdType %in% c("ACminusPY","PY")) | 
           syse_hh_type == HouseholdType)
      } else {
        TRUE
      }) &
        
        # Level of detail filter
        (syse_level_of_detail == "All" |
           (syse_level_of_detail == "HoHsAndAdults" &
              (MostRecentAgeAtEntry >= 18 | CorrectedHoH == 1)) |
           (syse_level_of_detail == "HoHsOnly" &
              CorrectedHoH == 1)) &
        
        (if (filter_pt) {
        # Project type filter (wrapped in parentheses to preserve logical order of operations)
        (syse_project_type == "All" |
           (syse_project_type %in% c("LHRes", "AllRes") & ProjectType %in% lh_res_types) |
           (syse_project_type %in% c("PHRes", "AllRes") & ProjectType %in% ph_types) |
           (syse_project_type == "SO" & ProjectType == out_type) |
           (syse_project_type == "AllNonRes" & ProjectType %in% non_res_types))
        } else {
          TRUE
        })
    ) |>
    fselect(-VeteranStatus)
}

# A function that returns a reactive expression
create_filtered_enrollments_reactive <- function(prefix, prev_yr = FALSE, filter_hh_type = TRUE) {
  reactive({
    enrollment_cats <- if (prev_yr) session$userData$enrollment_categories_prev else session$userData$enrollment_categories
    
    get_enrollments_filtered(
      enrollment_cats      = enrollment_cats,
      client_cats          = session$userData$client_categories,
      syse_hh_type         = input[[paste0(prefix, "_hh_type")]], # Dynamic input access
      syse_level_of_detail = input[[paste0(prefix, "_level_of_detail")]],
      syse_project_type    = input[[paste0(prefix, "_project_type")]],
      filter_hh_type       = filter_hh_type
    )
  })
}