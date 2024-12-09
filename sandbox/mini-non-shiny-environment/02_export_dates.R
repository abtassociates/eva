# meta = result comes directly from imported meta data
# calc = result is calculated

# Dates from Metadata -----------------------------------------------------
meta_HUDCSV_Export_Start(Export() %>% pull(ExportStartDate))

meta_HUDCSV_Export_End(Export() %>% pull(ExportEndDate))

meta_HUDCSV_Export_Date(Export() %>% pull(ExportDate))

# Build report dates ------------------------------------------------------
# if the start date's day of the month = 1, then that's the start date
# otherwise go forward a month and use the 1st of that month.
ExportStartAdjusted <- if_else(
  day(meta_HUDCSV_Export_Start()) == 1,
  meta_HUDCSV_Export_Start(),
  floor_date(meta_HUDCSV_Export_Start() %m+% months(1), unit = "month"))

# if you go forward to the first day of the next month and then subtract a day,
# and that equals the raw ExportEndDate, that means it is already a last day of
# the month so we just return the raw ExportEndDate. If the date is something
# other than that, then we want to get the first day of the month and go back 
# a day so that it cuts off on the last day of the month previous to the raw
# ExportEndDate
ExportEndAdjusted <- if_else(
  floor_date(meta_HUDCSV_Export_End() %m+% months(1), unit = "month") - days(1) ==
    meta_HUDCSV_Export_End(),
  meta_HUDCSV_Export_End(),
  floor_date(meta_HUDCSV_Export_End(), unit = "month") - days(1))

ReportEnd(as.Date(ExportEndAdjusted))
ReportStart(as.Date(ReportEnd() - years(1) + days(1)))

