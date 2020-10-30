#' @family QPR
#' @title QPR_tabItem UI Function
#' @description A shiny Module to generate the QPR tabitems.
#' @param id Internal parameters for {shiny}.
#' @param project_choices \code{(named list)} of choices of program types for the radio picker. Default does not show the UI item - choices must be provided for the picker to show.
#' @param region_choices \code{(named list)} of choices of regions for the region drop-down selector. Default does not show the UI item - choices must be provided for the picker to show options
#QUESTION Should there be defaults for options?
#' @param radio_mean \code{(logical)} whether to show the Mean/Median based average radio UI
#' @importFrom shiny NS tagList 
#' @importFrom rlang parse_expr
#' @importFrom purrr compact
#' @importFrom lubridate floor_date today
#' @importFrom shiny

mod_QPR_tabItem_ui <- function(id, region_choices = list(choices = c(unique(regions$RegionName[regions$County != "Mahoning"]))), date_choices = NULL) {
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
   Regions = if (!isFALSE(region_choices)) list(
    inputId = ns("region"),
    label = "Select Region(s)",
    choices = region_choices$choices,
    options = shinyWidgets::pickerOptions(                                                  liveSearch = TRUE),
    selected = NULL,
    width = "70%"
  )
  ))
  .user <- purrr::compact(list(
    Dates = if (!is.null(date_choices)) date_choices,
    Regions = if (!identical(region_choices, list(choices = c(unique(regions$RegionName[regions$County != "Mahoning"]))))) {
      # if the argument to region_choices differs from the default then add region_choices to the list of user supplied arguments to modify in the default args
       region_choices
    }
  ))
  if (length(.user) > 0) {
    # if there are 
    .defaults[names(.user)] <- purrr::map2(.defaults[names(.user)], .user, ~{
      # check if there are any arguments supplied by user not in default args
      .out <- purrr::list_modify(.x, !!!.y)
      .out
    })
  }
  
  # tabItem Output ----
  # Mon Oct 26 15:09:32 2020
  shinydashboard::tabItem(
    tabName = ns("Tab"),
    shiny::fluidRow(
      shinydashboard::box(
        shiny::htmlOutput(ns("header")), width = 12)
      ),
    shiny::fluidRow(
      box(
        if (shiny::isTruthy(.defaults$Regions)) {
          do.call(shinyWidgets::pickerInput, .defaults$Region)
          }
        ,
        if (shiny::isTruthy(.defaults$Dates)) {
          do.call(shiny::dateRangeInput, .defaults$Dates)
        }
      )
    ),
    ,
    shiny::fluidRow(
      shinydashboard::infoBoxOutput("ib_summary", width = 12)
      ),
    shiny::fluidRow(
      shiny::box(
      DT::dataTableOutput("dt_detail"), width = 12
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
    # Process Slider Inputs
    Report <- eventReactive(input$date, {
      .dates <- lubridate::parse_date_time2(input$date_range, c("mdY", "Ymd"))
      list(
        Start = .dates[1],
        End = .dates[2]
      )
    })
    
    # Header
    output$header <- shiny::renderUI({
      .dots <- rlang::dots_list(...)
      if (length(.dots) > 0) {
        .header <- do.call(shiny::tagList, append(list(shiny::h2(header)), .dots))
      } else {
        .header <- shiny::tagList(
          shiny::h2(header),
          shiny::h4(input$region),
          shiny::h4(format.Date(Report()$Start, "%m-%d-%Y"), "-", format.Date(Report()$End, "%m-%d-%Y"))
        )
      }
      .header
    })
    # Gather Objects
    
    # Process Data
    data_env <- reactive(qpr_expr[[id]]$expr, quoted = TRUE)
    
    output$ib_summary <- shinydashboard::renderInfoBox({
      rlang::eval_bare(qpr_expr[[id]]$ib)
    })
    output$dt_detail <- DT::renderDataTable({
      rlang::eval_bare(qpr_expr[[id]]$dt)
    })
  })
}

