# meta = result comes directly from imported meta data
# calc = result is calculated

# Dates from Metadata -----------------------------------------------------
meta_HUDCSV_Export_Start <<- Export %>% pull(ExportStartDate)

meta_HUDCSV_Export_End <<- Export %>% pull(ExportEndDate)

meta_HUDCSV_Export_Date <<- Export %>% pull(ExportDate)
