
x <- "2019-03-02"

load("data/Utilization.RData")

ReportStart <- 
  floor_date(ymd(x), 
             unit = "month")
ReportStart <- format(ymd(ReportStart), "%m-%d-%Y")

ReportEnd <- 
  floor_date(ymd(x) + months(1), 
             unit = "month") - days(1)
ReportEnd <- format(ymd(ReportEnd), "%m-%d-%Y")

y <- paste0(substr(x, 6, 7), "01", substr(x, 1, 4))

ClientUtilizers %>% 
  filter(ProjectName == "Allen - Lima Samaritan House - ESap",
         served_between(., ReportStart, ReportEnd)) %>%
  mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                            MoveInDate, EntryDate)) %>%
  select("Client ID" = PersonalID, 
         "Bed Start" = BedStart,
         "Exit Date" = ExitDate, 
         "Bed Nights in Month" = y) %>% view()


