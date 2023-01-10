library(here)
library(tidyverse)
library(lubridate)
library(patchwork)

folder_date <- "01-09-2023"

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
    WeekStarting = floor_date(Date, unit = "week", week_start = 1),
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
    WeekStarting = floor_date(Date, unit = "week", week_start = 1),
    Event = str_squish(str_sub(everything, 57L, nchar(everything))),
    everything = NULL
  )

old_way <- rbind(uploads_df, visits_df) %>%
  mutate(
    Event = case_when(
      Event == "Unsuccessful - not structurally valid" ~
        "Unsuccessful upload - not structurally valid",
      Event == "Unsuccessful - not hashed" ~
        "Unsuccessful upload - not hashed",
      Event == "Unsuccessful - wrong dataset" ~
        "Unsuccessful upload - wrong/incomplete dataset",
      Event == "Unsuccessful - file was mistructured" ~
        "Unsuccessful upload - file was mistructured",
      TRUE ~ Event
    )
  )

new_way <-
  read_csv(here(paste0(
    "sandbox/",
    folder_date,
    "/metadata.csv"
  ))) %>%
  select("SessionID" = SessionToken,
         "Date" = Datestamp,
         "Event" = Details) %>%
  mutate(WeekStarting = floor_date(Date, unit = "week", week_start = 1))

staging <- rbind(new_way, old_way)

all <- staging %>%
  filter(WeekStarting < floor_date(max(ymd_hms(staging$Date)),
                                   unit = "week")) %>%
  mutate(Event = if_else(Event == "Session started", "Site hit", Event),
         WeekStarting = as.Date(WeekStarting))

counts <- count(all, WeekStarting, Event)

a <- counts %>%
  filter(Event == "Site hit") %>%
  ggplot(aes(x = WeekStarting, y = n, group = Event, color = Event)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d-%Y") +
  labs(
    title = "Eva Site Hits",
    x = "Week Starting",
    y = ""
  ) +
  theme_minimal()

b <- counts %>%
  filter(str_detect(Event, "upload")) %>%
  ggplot(aes(x = WeekStarting, y = n, group = Event, color = Event)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d-%Y") +
  labs(title = "Eva Uploads",
       x = "Week Starting",
       y = "") +
  theme_minimal()

c <- counts %>%
  filter(str_detect(Event, "Download")) %>%
  ggplot(aes(x = WeekStarting, y = n, group = Event, color = Event)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d-%Y") +
  labs(title = "Eva Downloads",
       x = "Week Starting",
       y = "",
       caption = "Download data not collected until Dec 29, 2022.") +
  theme_minimal()

a / b / c
