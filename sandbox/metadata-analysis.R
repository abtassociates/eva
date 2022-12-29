library(here)
library(tidyverse)
library(lubridate)

folder_date <- "12-29-2022"

visits <-
  read_lines(here(paste0(
    "sandbox/",
    folder_date,
    "/site_visits.txt"
  )))

visits_df <- data.frame(everything = visits) %>%
  mutate(
    SessionID = str_squish(str_sub(everything, 1L, 33L)),
    Date = ymd_hms(str_squish(str_sub(everything, 35L, 54L))),
    WeekStarting = floor_date(Date, unit = "week"),
    Event = str_squish(str_sub(everything, 57L, nchar(everything))),
    everything = NULL
  )

uploads <-
  read_lines(here(paste0(
    "sandbox/",
    folder_date,
    "/upload_metadata.txt"
  )))

uploads_df <- data.frame(everything = uploads) %>%
  mutate(
    SessionID = str_squish(str_sub(everything, 1L, 33L)),
    Date = ymd_hms(str_squish(str_sub(everything, 35L, 54L))),
    WeekStarting = floor_date(Date, unit = "week"),
    Event = str_squish(str_sub(everything, 57L, nchar(everything))),
    everything = NULL
  )

count(uploads_df, Event, WeekStarting) %>% arrange(WeekStarting, desc(n))
count(visits_df %>% 
        select(SessionID, WeekStarting) %>% 
        unique(), WeekStarting,
      name = "Hits") %>% 
  arrange(WeekStarting)
count(uploads_df, SessionID, Event) %>% arrange(SessionID)
