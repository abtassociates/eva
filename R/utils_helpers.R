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
  .dt_opts <- rlang::fn_fmls()
  # Remove . args
  .dt_opts <- .dt_opts[!grepl("^\\.", names(.dt_opts))]
  # Get user supplied args
  .user <- rlang::call_args(match.call())
  .user <- append(.user[!grepl("^\\.", names(.user))], rlang::dots_list(...))
  
  if (!identical(.dt_opts, .user) && !rlang::is_empty(.user)) {
    .dt_opts <- purrr::list_modify(.dt_opts, !!!.user)
  } else if (.replace) {
    .dt_opts <- .user
  }
  # replace data call with actual data
  .dt_opts$data <- .data
  #evaluate each item so calls are not passed in.
  for (i in which(purrr::map_lgl(.dt_opts, is.call))) {
    .dt_opts[[i]] <- eval(.dt_opts[[i]])
  }
  rlang::exec(DT::datatable, !!!.dt_opts)
}


#' @title qpr_infobox
#' @description Function to render infobox from default template for QPR tabitems
#' @inheritParams shinydashboard::infoBox
#' @inheritDotParams shinydashboard::infoBox
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
                        value = .data$AvgScore,
                        icon = shiny::icon("shopping-cart"),
                        subtitle = paste(.data$Increased, "out of", .data$TotalHHs, "households served"),
                        ...
) {
  
  .ib_opts <- rlang::fn_fmls()
  .ib_opts <- .ib_opts[!grepl("^\\.", names(.ib_opts))]
  .user <- rlang::call_args(match.call())
  .user <- append(.user[!grepl("^\\.", names(.user))], rlang::dots_list(...))
  if (inherits(.user$icon, "character")) .user$icon <- shiny::icon(.user$icon)
  if (!identical(.ib_opts, .user) && !rlang::is_empty(.user)) {
    .ib_opts <- purrr::list_modify(.ib_opts, !!!.user)
  } else if (.replace) {
    .ib_opts <- .user
  }
  # Evaluate all calls in this environment (purrr::map does not work)
  for (i in which(purrr::map_lgl(.ib_opts, is.call))) {
    .ib_opts[[i]] <- eval(.ib_opts[[i]])
  }
  
  rlang::exec(shinydashboard::infoBox, !!!.ib_opts)
}



