#' @title qpr_datatable
#' @description Function to render datatable from default template for QPR tabitems
#' @param .data \code{(any)} Data to be passed in and used subsequent arguments
#' @param .replace \code{(logical)} whether to replace the default arguments with those supplied and eliminate the default arguments, or to replace existing defaults & and add additional args specified
#' @inheritParams DT datatable
#' @param ... \code{(named arguments)} passed on to \link[DT]{datatable}
#' @export
#' @importFrom rlang fn_fmls call_args dots_list exec `!!!`
#' @importFrom tidyselect starts_with
#' @importFrom purrr list_modify
#' @importFrom DT datatable
 
qpr_datatable <- function(.data,
                          .replace = FALSE,
                          caption = NULL,
                          rownames = FALSE,
                          filter = 'top',
                          options = list(dom = 'ltpi'),
                          ...
) {
  # Get default args
  .dt_opts <- as.list(rlang::fn_fmls())
  # Get user supplied args
  .user <- rlang::call_args(match.call())
  # Get dots
  .dots <- rlang::dots_list(...)
  # Remove . args
  .dt_opts <- .dt_opts[-tidyselect::starts_with(".", vars = names(.dt_opts))]
  .user <- .user[-tidyselect::starts_with(".", vars = names(.user))]
  
  if (!identical(.dt_opts, .user)) {
    .dt_opts <- purrr::list_modify(.dt_opts, !!!.user)
  } else if (.replace) {
    .dt_opts <- .user
  }
  rlang::exec(DT::datatable, !!!.dt_opts, !!!.dots)
}


#' @title qpr_infobox
#' @description Function to render infobox from default template for QPR tabitems
#' @param .data \code{(any)} Data to be passed in and used subsequent arguments
#' @param .replace \code{(logical)} whether to replace the default arguments with those supplied and eliminate the default arguments, or to replace existing defaults & and add additional args specified
#' @inheritParams shinydashboard infoBox
#' @param ... \code{(named arguments)} passed on to \link[shinydashboard]{infoBox}
#' @export
#' @importFrom rlang fn_fmls call_args dots_list exec `!!!`
#' @importFrom tidyselect starts_with
#' @importFrom purrr list_modify
#' @importFrom shinydashboard infoBox

qpr_infobox <- function(.data,
                        .replace = FALSE,
                        title = "Average Score",
                        color = "purple",
                        icon = shiny::icon("shopping-cart"),
                        value = scales::percent(.data$Percent),
                        subtitle = paste(.data$Increased, "out of", .data$TotalHHs, "households served"),
                        ...
) {
  .ib_opts <- as.list(rlang::fn_fmls())
  .user <- rlang::call_args(match.call())
  .ib_opts <- .ib_opts[-tidyselect::starts_with(".", vars = names(.ib_opts))]
  .user <- .user[-tidyselect::starts_with(".", vars = names(.user))]
  .dots <- rlang::dots_list(...)
  if (inherits(.user$icon, "character")) .user$icon <- shiny::icon(.user$icon)
  if (!identical(.ib_opts, .user)) {
    .ib_opts <- purrr::list_modify(.ib_opts, !!!.user)
  } else if (.replace) {
    .ib_opts <- .user
  }
  rlang::exec(shinydashboard::infoBox, !!!.ib_opts, !!!.dots)
}

