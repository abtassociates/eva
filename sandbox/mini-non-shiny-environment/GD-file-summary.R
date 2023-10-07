client_count_data_df <- validation %>%
  mutate(
    PersonalID = as.character(PersonalID),
    RelationshipToHoH = case_when(
      RelationshipToHoH == 1 ~ "Head of Household",
      RelationshipToHoH == 2 ~ "Child",
      RelationshipToHoH == 3 ~ "Spouse or Partner",
      RelationshipToHoH == 4 ~ "Other relative",
      RelationshipToHoH == 5 ~ "Unrelated household member",
      RelationshipToHoH == 99 ~ "Data not collected (please correct)"
    ),
    Status = case_when(
      ProjectType %in% c(ph_project_types) &
        is.na(MoveInDateAdjust) &
        is.na(ExitDate) ~ "Active No Move-In",
      ProjectType %in% c(ph_project_types) &
        !is.na(MoveInDateAdjust) &
        is.na(ExitDate) ~ paste0("Currently Moved In (",
                                 today() - MoveInDateAdjust,
                                 " days)"),
      ProjectType %in% c(ph_project_types) &
        is.na(MoveInDateAdjust) &
        !is.na(ExitDate) ~ "Exited No Move-In",
      ProjectType %in% c(ph_project_types) &
        !is.na(MoveInDateAdjust) &
        !is.na(ExitDate) ~ "Exited with Move-In",
      !ProjectType %in% c(ph_project_types) &
        is.na(ExitDate) ~ paste0("Currently in project (",
                                 today() - EntryDate, 
                                 " days)"),
      !ProjectType %in% c(ph_project_types) &
        !is.na(ExitDate) ~ "Exited project"
    ),
    sort = today() - EntryDate
  ) %>%
  arrange(desc(sort), HouseholdID, PersonalID) %>%
  # make sure to include all columns that will be needed for the various uses
  select(
    PersonalID,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    Status,
    ProjectID,
    ProjectName,
    OrganizationName,
    ProjectType
  )

detail <- client_count_data_df %>%
  filter(str_detect(Status, "Exit", negate = TRUE)) %>%
  mutate(Status = factor(
    case_when(
      str_detect(Status, "Currently in") ~ "Currently in project",
      str_detect(Status, "Currently Moved") ~ "Currently Moved In",
      TRUE ~ Status
    ),
    levels = c("Currently in project",
               "Active No Move-In",
               "Currently Moved In")
  )) %>% 
  count(ProjectType, Status, name = "Total")

detail_order <- detail %>%
  group_by(ProjectType) %>%
  summarise(InProject = sum(Total, na.rm = FALSE)) %>%
  ungroup()

plot_data <- detail %>%
  left_join(detail_order, by = "ProjectType") %>%
  group_by(ProjectType) %>%
  arrange(ProjectType, desc(Total)) %>%
  mutate(
    movedin = lag(Total, default = 0),
    text_position = case_when(
      !ProjectType %in% c(ph_project_types) ~ InProject / 2,
      ProjectType %in% c(ph_project_types) ~ 
        Total / 2 + movedin
    )
  )

validate_by_org <-
  ggplot(
    plot_data,
    aes(x = reorder(project_type_abb(ProjectType), InProject),
        y = Total, fill = Status)
  ) +
  geom_col(alpha = .7, position = "stack")  +
  geom_text(aes(label = prettyNum(Total, big.mark = ","),
                y = text_position),
            color = "gray14")+
  scale_y_continuous(label = comma_format()) +
  scale_colour_manual(
    values = c(
      "Currently in project" = "#71B4CB",
      "Active No Move-In" = "#7F5D9D",
      "Currently Moved In" = "#52BFA5"
    ),
    aesthetics = "fill"
  ) +
  labs(
    title = "Current System-wide Counts",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title.position = "plot",
    title = element_text(colour = "#73655E"),
    legend.position = "top"
  )

# pdde plot ---------------------------------------------------------------

# run PDDE checker script minus log to console command
types <- pdde_main %>%
  right_join(data.frame(Type = c("High Priority", "Error", "Warning")),
             by = "Type") %>%
  mutate(CountThis = if_else(is.na(ProjectID), 0, 1)) %>%
  group_by(Type) %>%
  summarise(Total = sum(CountThis, na.rm = TRUE)) %>%
  ungroup()

high_priority_yn <- types %>%
  filter(Type == "High Priority") %>%
  nrow()

errors_yn <- types %>%
  filter(Type == "Error") %>%
  nrow()

warnings_yn <- types %>%
  filter(Type == "Warning") %>%
  nrow()

colors <- c("#71538c", "#b19bc4", "#d0c3db")

detail <- pdde_main %>%
  mutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning"))) %>%
  count(Issue, Type, name = "Total") %>%
  arrange(Type, desc(Total)) %>%
  mutate(Order = 1:n())

# pdde_plot_overview <-
  ggplot(
    detail %>%
      head(5L),
    aes(x = reorder(x = str_wrap(Issue, width = 20),
                    X = Order,
                    decreasing = TRUE),
        y = Total,
        fill = Type)
  ) +
  geom_col(alpha = .7) +
  scale_y_continuous(label = comma_format()) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Top 5 Project Descriptor Data Element Issues",
    subtitle = paste(
      "High Priority:",
      types %>% filter(Type == "High Priority") %>% pull(Total),
      "| Errors:",
      types %>% filter(Type == "Error") %>% pull(Total),
      "| Warnings:",
      types %>% filter(Type == "Warning") %>% pull(Total)
    ),
    x = "",
    y = ""
  ) +
  coord_flip() +
  theme_minimal(base_size = 16) +
  theme(
    plot.title.position = "plot",
    panel.grid = element_blank(),
    title = element_text(colour = "#73655E"),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  geom_text(
    aes(label = prettyNum(Total, big.mark = ",")),
    nudge_y = 2,
    color = "gray14",
    size = 6
  )
