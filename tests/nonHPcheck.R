capture.output("Alyssa Test", file = stderr())
#Libraries & Datasets
library(readr)
library(fs)
library(dplyr)
library(here)
columns <- read_csv("public-resources/columns.csv")

#isolate the non-HP column names
nonHPcolumns <- subset(columns, columns$DataTypeHighPriority!=1)
#Add space after file value
nonHPcolumns$File <- sub("$", " ", nonHPcolumns$File)
#identify all File options
file_options <- sort(unique(nonHPcolumns$File))

#isolate file path to r scripts we want to investigate
directory <- here()

rscripts <- list("/00_initially_valid_import.R","/01_get_Export.R",
                 "/02_export_dates.R","/03_file_structure_analysis.R",
                 "/04_initial_data_prep.R","/05_DataQuality_functions.R",
                 "/05_DataQuality.R","/06_PDDE_Checker.R",
                 "/client_counts_functions.R","/global.R","/hardcodes.R",
                 "/helper_functions.R","/server.R")
             

#create empty lists for variables we want included in final dataframe
rscript_list <- list()
file_option_list <- list()
line_num_list <- list()
line_segment_list <- list()
column_list <- list()

#loop through all nonHPcolnames in all r script files
for (o in file_options){
  #identify file option
  file <- o
  specfilecolumns <- subset(nonHPcolumns, nonHPcolumns$File==file)
  for (j in rscripts){
    #identify rscript
    fname <- j
    file_path <- paste0(directory, fname)
    lines <- readLines(file_path)
    for (i in specfilecolumns$Column){
      #for each non-HP column in the file option, 
      variable <- i
      line_num <- 0
      yes <- 0
      for (p in lines){
        line_segment <- p
        if(grepl(file, line_segment) == TRUE){
          #if file option is mentioned in rscript, move forward
          yes <- yes+1
          for (k in lines){
            line_segment_ <- k
            line_num <- line_num + 1
            if(grepl(variable, line_segment_) == TRUE){
              #if column is mentioned in rscript, add to dataframe
              ##identify length of lists
              len_rscript_list <- length(rscript_list)
              len_file_option_list <- length(file_option_list)
              len_line_segment_list <- length(line_segment_list)
              len_line_num_list <- length(line_num_list)
              len_column_list <- length(column_list)
              ##add components to lists
              rscript_list[len_rscript_list+1] <- fname
              file_option_list[len_file_option_list+1] <- file
              column_list[len_column_list+1] <- variable
              line_num_list[len_line_num_list+1] <- line_num
              line_segment_list[len_line_segment_list+1] <- line_segment_
            }
          }
        if (yes==1) break
        #if file option already found in rscript, don't run for loop again
        ##this is necessary to not have duplicates
        }
      }
    }  
  }
}
#make a list of all the component lists
hpcollist <- list(file_option_list, column_list, rscript_list, line_num_list, 
                  line_segment_list)

#make a dataframe
dataframe <- as.data.frame(do.call(cbind,hpcollist))
possibleHPcolumns <- dataframe %>%
  rename(
    UploadFileName = V1,
    Column = V2,
    RScript = V3,
    LineNum = V4,
    LineSegment = V5
    )
