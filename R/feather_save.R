data2feather <- function(path = "data/db", data = "data/Rme_old.RData", new_data = "data/Rminor_elevated.rds") {
  objects <- new.env()
  if (stringr::str_detect(stringr::regex("RData$", ignore_case = TRUE))) {
    load(data, envir = objects)
  } else {
    list2env(readRDS(data), envir = objects)
  }
  #file.copy("data/Rminor_elevated.RData", "data/Rme_old.RData")
  # This is the previous
  
  nms <- ls(objects, all.names = TRUE) %>%
    {`[`(., !. %in% c("Exit", "summary", "Project", "tay", "Enrollment"))} %>%
    stringr::str_subset("\\_between$", negate = TRUE)
  obj_list <- rlang::env_get_list(objects, nms, NULL)
  .is_df <- purrr::map_lgl(obj_list, is.data.frame)
  all_df_nms <- names(obj_list)[.is_df]
  rm(list = all_df_nms, envir = objects)
  objects$all_df_nms <- all_df_nms
  purrr::iwalk(obj_list[.is_df], ~{
    message(.y)
    feather::write_feather(.x, path = file.path(path, paste0(.y, ".feather")))
  })
  purrr::iwalk(all_df_nms, ~{
    assign(.x, function() get_feather(as.character(match.call()[[1]])), objects)
  })
  save(list = ls(envir = objects), file = new_data, envir = objects)
}




compare_image_sizes <- function(new = "data/Rminor_elevated.RData", old = "data/Rme_old.RData") {
  old_objects <- new.env()
  load(new, envir = old_objects)
  nms <- ls(old_objects, all.names = TRUE)
  old_list <- rlang::env_get_list(old_objects, nms, NULL)
  new_objects <- new.env()
  load(old, envir = new_objects)
  nms <- ls(new_objects, all.names = TRUE)
  new_list <- rlang::env_get_list(new_objects, nms, NULL)
  out <- object.size(old_list) - object.size(new_list) 
  message(paste0(as.numeric(out) / as.numeric(object.size(old_list)), "% reduction"))
  message(format(out,"GB", standard = "SI"), " removed")
  list(difference = out,
       new = new_list,
       old = old_list)
}

# compare_image_sizes()
#99% reduction in size

#' @title Convert the occurrence of a symbol in an R document to a function call by the same name
#' @description Useful to replace calls to an object by symbol name with an accessor function or reactive with the same name
#' @param x \code{(character)} All symbols to change
#' @param file \code{(character)} Path to file in which to change them
#' @return \code{(character)} vector of text in the file with the new function calls
sym2function <- function(x, file) {
  .parse <- parse(file)
  .server_data <- utils::getParseData(.parse)
  .to_change <- which(.server_data$token %in% "SYMBOL" & grepl(paste0(paste0("(?:(?<!\\w)",x,"(?!\\w))"), collapse = "|"), .server_data$text, perl = TRUE))
  out <- readLines(file)
  
  for (i in .to_change) {
    .ln_num <- .server_data$line2[i]
    
    if (interactive()) {
      rstudioapi::navigateToFile(file, line = .ln_num, column = .server_data$col1[i])
      Sys.sleep(1)
      .answer <- utils::askYesNo("Change this symbol instance to a function?")
      if (isFALSE(.answer)) next
      else if (is.na(.answer)) stop("A choice must be selected")
        
    }
    # if a line must be altered more than once, adjust the column numbers based on the number of reoccurences
    .add <- (which(.to_change[.server_data$line2[.to_change] %in% .ln_num] %in% .to_change[.to_change %in% i]) - 1) * 2
    
    .line <- out[.ln_num]
    .col <- .server_data$col2[i]
    out[.ln_num] <- stringr::str_c(stringr::str_sub(.line, end = .col + .add),"()", stringr::str_sub(.line, .col + .add + 1, nchar(.line)))
  }
  out
}

# purrr::walk(c(list.files("R", pattern = "\\.R$", full.names = T), "server.R", "ui.R", "global.R")[ - c(1:3)], ~{
#   .lines <- object2function(all_df_nms, .x)
#   skip = FALSE
#   browser()
#   if (!skip) write(.lines, file = .x)
# })



 






