#COHHIO_HMIS
#Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published
#by the Free Software Foundation, either version 3 of the License, or
#any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

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

a <- ClientUtilizers %>% 
  filter(ProjectName == "Allen - Lima Samaritan House - ESap",
         served_between(., ReportStart, ReportEnd)) %>%
  mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                            MoveInDate, EntryDate)) %>%
  select("Client ID" = PersonalID, 
         "Bed Start" = BedStart,
         "Exit Date" = ExitDate, 
         y) 

z <- paste("Bed Nights in", format(ymd(x), "%B %Y")) 

colnames(a) <- c("Client ID", "Bed Start", "Exit Date", z)

a %>% view()

