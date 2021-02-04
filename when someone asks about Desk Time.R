

provider <- "Ashland - Appleseed CMHC - HCRP RRH"

ReportStart <- format.Date(ymd(today() - years(1)), "%m-%d-%Y")
ReportEnd <- format.Date(ymd(today()), "%m-%d-%Y")

desk_time <- validation %>%
  filter(ProjectName == provider &
           entered_between(., ReportStart, ReportEnd) &
           ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13)) %>%
  select(ProjectName, PersonalID, HouseholdID, EntryDate, DateCreated) %>%
  mutate(
    DeskTime = difftime(floor_date(ymd_hms(DateCreated), unit = "day"),
                        ymd(EntryDate),
                        units = "days"),
    DeskTime = as.integer(floor(DeskTime)),
    GoalMet = if_else(DeskTime > 5 |
                        DeskTime < 0,
                      "chocolate2",
                      "forestgreen")
  ) %>%
  select(HouseholdID,
         PersonalID,
         ProjectName,
         EntryDate,
         DateCreated,
         DeskTime,
         GoalMet) 

desk_time_medians <- desk_time %>%
  group_by(ProjectName) %>%
  summarise(MedianDeskTime = median(DeskTime),
            TotalEntered = n()) %>%
  ungroup()

dq_plot_desk_time <-
  ggplot(
    desk_time,
    aes(x = ymd(EntryDate), y = DeskTime)
  ) +
  geom_point(aes(color = GoalMet, size = 8, alpha = .2),
             show.legend = FALSE)+
  scale_color_identity() +
  geom_hline(yintercept = 5, color = "forestgreen") +
  geom_hline(yintercept = 0, color = "forestgreen") +
  geom_hline(
    data = desk_time_medians,
    aes(yintercept = MedianDeskTime),
    color = "black"
  ) +
  xlim(today() - years(1), today()) +
  geom_label(x = today() - days(180),
             y = desk_time_medians %>%
               pull(MedianDeskTime),
             label = paste("Median:", 
                           desk_time_medians %>%
                             pull(MedianDeskTime),
                           "days | Total Clients:",
                           desk_time_medians %>%
                             pull(TotalEntered))) +
  geom_label(x = today() - days(300),
             y = 5,
             label = "DQ Standards (5 days or less)") +
  labs(x = "Entry Date",
       y = "Data Entry Delay (in days)") +
  theme_minimal(base_size = 18)

dq_plot_desk_time