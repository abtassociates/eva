
logToConsole("Running get export")

# build a list of problems() with each data frame
# problems() is a built-in function that collects the problems reported in the 
# console from a read_csv() call.
list_of_problems <- list()

# for each file in the csv, loop through the file names in the csv
for (file in unique(cols_and_data_types$File)) {
  #import the csv and save it as a data frame
  print(paste0("importing ", file))
  assign(file, importFile(upload_filepath, csvFile=file))
  # add the problems() to the list
  list_of_problems[[file]] <- problems(get(file))
  
  file.remove(paste0(file, ".csv"))
}

problems <- do.call(rbind, list_of_problems)