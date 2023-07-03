# meta = result comes directly from imported meta data
# calc = result is calculated

logToConsole("Running dates")

# Dates from Metadata -----------------------------------------------------
meta_HUDCSV_Export_Start <<- Export %>% pull(ExportStartDate)

meta_HUDCSV_Export_End <<- Export %>% pull(ExportEndDate)

meta_HUDCSV_Export_Date <<- Export %>% pull(ExportDate)

file_date_range <<- interval(meta_HUDCSV_Export_Start, meta_HUDCSV_Export_End)
