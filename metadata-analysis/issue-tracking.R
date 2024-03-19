x <- read_log("sandbox/2023-08-15_logs/eva-shiny-20230815-122443-40367.log")
y <- read_csv("sandbox/2023-08-15_logs/metadata.csv")
z <- read_csv("sandbox/2023-08-15_logs/sessiondata.csv")

what_happened <- y %>%
  left_join(z, by = c("SessionToken", "Datestamp"))

what_happened

y %>% # <- don't change this
  # ^ change where your file is in your project directory
  group_by(SessionToken) %>%
  mutate(sessionTime = difftime(max(Datestamp), min(Datestamp), units = "mins"),
         is_timeout = sessionTime >= 10 & sessionTime < 11,
         Details = if_else(Details == "Session started" & is_timeout == TRUE , 
                           "Session restarted after timeout", 
                           Details
         )
  ) %>%
  ungroup() %>%
  select("SessionID" = SessionToken,
         "Date" = Datestamp,
         "Event" = Details) %>%
  mutate(WeekEnding = ceiling_date(Date, unit = "week", week_start = 1),
         MonthEnding = ceiling_date(Date, unit = "month"),
         EventCategory = case_when(
           Event %in% c(
             "Unsuccessful upload - not structurally valid",
             "Unsuccessful upload - not hashed",
             "Unsuccessful upload - wrong dataset",
             "Unsuccessful upload - wrong/incomplete dataset",
             "Unsuccessful upload - file was mistructured",
             "Unsuccessful upload - zip file was misstructured"
           )
           ~ "Unsuccessful upload",
           Event %in% c("Successful upload") ~ "Successful upload",
           TRUE ~ "Other"
         ))

what <- y %>% # <- don't change this
  # ^ change where your file is in your project directory
  group_by(SessionToken) %>%
  mutate(sessionTime = difftime(max(Datestamp), min(Datestamp), units = "mins"),
         is_timeout = sessionTime >= 10 & sessionTime < 11,
         Details = if_else(Details == "Session started" & is_timeout == TRUE , 
                           "Session restarted after timeout", 
                           Details
         )
  ) %>%
  ungroup() %>%
  filter(Datestamp > ymd_hms("20230815 17:00:00"))
