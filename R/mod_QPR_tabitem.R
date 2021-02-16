#' @family QPR
#' @title QPR_tabItem UI Function
#' @description A shiny Module to generate the QPR tabitems.
#' @param id Internal parameters for {shiny}.
#' @param choices \code{(named list)} of arguments to \link[shinyWidgets]{pickerInput}. `FALSE` to omit the choice drop-down selector. - choices must be provided for the picker to show options. Defaults will be used for omitted arguments unless arguments are explicitly set to `NULL`. Defaults are as follows:
#' `list(
#' inputId = ns("region"),
#' label = "Select Region(s)",
#' choices = choices$choices, # 
#' options = shinyWidgets::pickerOptions(                                                  liveSearch = TRUE),
#' selected = NULL,
#' width = "70%"
#' )`
#' Default value for choices is found in *global.R* for each tabItem namespace.
#QUESTION Should there be defaults for options?
#' @param date_choices \code{(logical)} whether to show a date range picker
#' @importFrom shiny NS tagList 
#' @importFrom rlang parse_expr
#' @importFrom purrr compact
#' @importFrom lubridate floor_date today
#' @importFrom shiny

mod_QPR_tabItem_ui <- function(id, choices = NULL, date_choices = NULL) {
  ns <- NS(id)
  # Create labeled Quarter List
  # .quarter_labels <- rev(unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() + 1 / 4))
  # slider_choices <- rev(purrr::map(.qs, ~lubridate::yq(.x) - lubridate::days(1)))
  # names(slider_choices) <- .quarter_labels
  .defaults <- purrr::compact(list(
  Dates = if (!isFALSE(date_choices)) list(
    inputId = ns("date_range"),
    label = "Date Range",
    start = lubridate::floor_date(lubridate::today() - lubridate::days(31), "year"),
    end = lubridate::today(),
    min = get0("FileStart"),
    format = "mm/dd/yyyy"
  ),
  Regions = if (!isFALSE(choices))
    list(
      inputId = ns("region"),
      label = "Select Region(s)",
      choices = tab_choices[[id]]$choices,
      options = shinyWidgets::pickerOptions(liveSearch = TRUE),
      selected = NULL,
      width = "70%"
    )
  ))
  .user <- purrr::compact(list(
    Dates = date_choices,
    Regions = choices
  ))
  if (length(.user) > 0) {
    # if there are 
    .defaults[names(.user)] <- purrr::map2(.defaults[names(.user)], .user, ~{
      # replace default params with those supplied by user on a param by param basis, retaining defaults.
      purrr::list_modify(.x, !!!.y)
    })
  }
  # tabItem Output ----

  shinydashboard::tabItem(
    tabName = ns("Tab"),
    shiny::fluidRow(
      shinydashboard::box(
        shiny::htmlOutput(ns("header")), width = 12)
      ),
    shiny::fluidRow(
      shinydashboard::box(
        if (shiny::isTruthy(.defaults$Regions)) {
          do.call(shinyWidgets::pickerInput, .defaults$Regions)
          }
        ,
        if (shiny::isTruthy(.defaults$Dates)) {
          do.call(shiny::dateRangeInput, .defaults$Dates)
        }
      )
    ),
    shiny::fluidRow(
      shinydashboard::infoBoxOutput(ns("ib_summary"), width = 12)
      ),
    shiny::fluidRow(
      shinydashboard::box(
      DT::dataTableOutput(ns("dt_detail")), width = 12
      )
    )
  )
    
}

#' @family QPR
#' @title QPR Server Functions
#' @description A shiny server Module to generate the header, slider, pickers and plot for each tabitem.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param header \code{(character)} The header text passed to the initial \link[shiny]{h2} tag in the header.
#' @param ... Additional \code{(list/shiny.tag.list/shiny.tag)}s  to be appended to the header after the \link[shiny]{h2} tag with `header`. Defaults to \code{list(h4(input$region), h4(paste(ReportStart, "to", ReportEnd)))} if unspecified. 
#' @importFrom shiny NS tagList 
#' @importFrom rlang parse_expr eval_bare
#' @importFrom purrr keep


mod_QPR_server <- function(id, header, input, output, session, ...){
  if (missing(header)) {
    rlang::abort("Must provide header for mod_QPR_server(",id,")")
  }
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Header
    output$header <- shiny::renderUI({
      shiny::tagList(
          shiny::h2(header),
          shiny::h4(input$region),
          shiny::h4(format.Date(input$date_range[1], "%B %d, %Y"), "-", format.Date(input$date_range[2], "%B %d, %Y"))
        )
    })
    # Gather Objects
    
    # Process Data
    data_env <- shiny::reactive(qpr_expr[[id]]$expr, quoted = TRUE)
    
    output$ib_summary <- shinydashboard::renderInfoBox(qpr_expr[[id]]$infobox, quoted = TRUE)
    
    output$dt_detail <- DT::renderDT(qpr_expr[[id]]$datatable, quoted = TRUE)
  })
  
}

