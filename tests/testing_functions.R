customDownload <- function(app, downloadHandler, fname) {
  print(paste("downloading",downloadHandler))
  app$get_download(downloadHandler, fname)
  file.remove(fname)
}

handle_helper_data <- function(app, test_script_name, datasetname) {
  snapshot_folder <- glue::glue(
    "tests/testthat/_snaps/{platform_variant(os_name = FALSE)}/{gsub('test-','',test_script_name)}"
  )
  
  old_path <- here(glue::glue("{snapshot_folder}/{datasetname}.csv"))
  new_path <- gsub(".csv", ".new.csv", old_path)
  
  new_df <- app$get_value(export=datasetname)
  
  if(is.null(new_df) & !file.exists(old_path)) return(NULL)
  
  new_df <- as.data.table(new_df)
  fwrite(new_df, file = new_path)
  
  # if no csv yet, create one at the original filepath
  if(!file.exists(old_path)) {
    message(glue::glue("A csv for {datasetname} does not yet exist. Creating one now!"))
    fwrite(new_df, file = old_path)
  } 
  else {
    # otherwise, if new and old are different warn user
    if(!identical(fread(old_path), fread(new_path))) {
      warning(
        paste0(
          "Difference detected in ",
          datasetname,
          ". Use snapshot_review to visualize diff."
        )
      )
      fwrite(new_df, file = new_path)
    } 
    # otherwise, remove the new one
    else {
      file.remove(new_path)
    }
  }
}