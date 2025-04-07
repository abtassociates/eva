# meta = result comes directly from imported meta data
# calc = result is calculated

logToConsole(session, "Running dates")
# Dates from Metadata -----------------------------------------------------
session$userData$meta_HUDCSV_Export_Start <- session$userData$Export$ExportStartDate

session$userData$meta_HUDCSV_Export_End <- session$userData$Export$ExportEndDate

session$userData$meta_HUDCSV_Export_Date <- session$userData$Export$ExportDate

# Build report dates ------------------------------------------------------
# if the start date's day of the month = 1, then that's the start date
# otherwise go forward a month and use the 1st of that month.

ExportStartAdjusted <- if_else(
  day(session$userData$meta_HUDCSV_Export_Start) == 1,
  session$userData$meta_HUDCSV_Export_Start,
  floor_date(session$userData$meta_HUDCSV_Export_Start %m+% months(1), unit = "month"))

# if you go forward to the first day of the next month and then subtract a day,
# and that equals the raw ExportEndDate, that means it is already a last day of
# the month so we just return the raw ExportEndDate. If the date is something
# other than that, then we want to get the first day of the month and go back 
# a day so that it cuts off on the last day of the month previous to the raw
# ExportEndDate
session$userData$ReportEnd <- if_else(
  floor_date(session$userData$meta_HUDCSV_Export_End %m+% months(1), unit = "month") - days(1) ==
    session$userData$meta_HUDCSV_Export_End,
  session$userData$meta_HUDCSV_Export_End,
  floor_date(session$userData$meta_HUDCSV_Export_End, unit = "month") - days(1))

session$userData$ReportStart <- session$userData$ReportEnd - years(1) + days(1)
# browser()
session$userData$days_of_data <- session$userData$ReportEnd - ExportStartAdjusted

