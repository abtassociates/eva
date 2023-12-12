
# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
system_activity_filtered <- system_df %>%
    group_by(HouseholdID) %>%
    mutate(
        Household_Type = case_when(
            all(is.na(AgeAtEntry)) ~ "Unknown Households",
            all(AgeAtEntry >= 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~ "Adult-Only",
            any(AgeAtEntry < 18, na.rm = TRUE) & any(AgeAtEntry >= 18, na.rm = TRUE) ~ "Adult-Child",
            all(AgeAtEntry < 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~ "Child-Only",
            all(AgeAtEntry < 25 & AgeAtEntry >= 18, na.rm = TRUE) & !any(is.na(AgeAtEntry)) ~ "Youth and Young Adult",
            any(is.na(AgeAtEntry)) ~ "Unknown Households"
        )
    ) %>% 
    ungroup() %>%
    filter(
        # Household Type
        Household_Type == input$syso_hh_types &

        # Level of Detail
        (
            (input$syso_level_of_detail == "All Adults and Heads of Households" & (AgeAtEntry >= 18 | RelationshipToHoH == 1)) |
            (input$syso_level_of_detail == "All Heads of Households" & RelationshipToHoH == 1) |
            (input$syso_level_of_detail == "All People")
        ) & 

        # Project Type
        (
            (ProjectType %in% input$syso_project_types) | input$syso_project_types == "All Project Types"
        ) & 

         # Age
        (
            (AgeAtEntry >= 0 & AgeAtEntry <= 12 & syso_age[1] %in% input$syso_age) |
            (AgeAtEntry >= 13 & AgeAtEntry <= 17 & syso_age[2] %in% input$syso_age) |
            (AgeAtEntry >= 18 & AgeAtEntry <= 20 & syso_age[3] %in% input$syso_age) |
            (AgeAtEntry >= 21 & AgeAtEntry <= 24 & syso_age[4] %in% input$syso_age) |
            (AgeAtEntry >= 25 & AgeAtEntry <= 34 & syso_age[5] %in% input$syso_age) |
            (AgeAtEntry >= 35 & AgeAtEntry <= 44 & syso_age[6] %in% input$syso_age) |
            (AgeAtEntry >= 45 & AgeAtEntry <= 54 & syso_age[7] %in% input$syso_age) |
            (AgeAtEntry >= 55 & AgeAtEntry <= 64 & syso_age[8] %in% input$syso_age) |
            (AgeAtEntry >= 65 & AgeAtEntry <= 74 & syso_age[9] %in% input$syso_age) |
            (AgeAtEntry >= 75 ~ syso_age[10] %in% input$syso_age)
        ) &

        # Gender
        (
            input$syso_gender == "All Genders"
        ) & 

        # Race/Ethnicity
        (
            input$syso_race_ethnicity == "All Races/Ethnicities"
        )
    ) %>%
    group_by(PersonalID) %>%
    mutate(
        EarliestEntry = min(EntryDate),
        LatestExit = coalesce(max(ExitDate), no_end_date) 
    ) %>%
    ungroup()

system_activity_prep <- system_activity_filtered %>%
    distinct(PersonalID, EarliestEntry, LatestExit) %>%
    mutate(
        x.axis.Var = case_when(
            EarliestEntry <= input$syso_date_range[1] & 
            LatestExit > input$syso_date_range[1] ~ paste0("Active as of ",input$syso_date_range[1]),
            EarliestEntry > input$syso_date_range[1] ~ "Inflow",
            LatestExit < input$syso_date_range[2] ~ "Outflow",
            EarliestEntry <= input$syso_date_range[2] & 
            LatestExit > input$syso_date_range[2] ~ paste0("Active as of ",input$syso_date_range[2])
        ),
        cat.Var = case_when(
            ProjectType %in% coc_funded_project_types &
            EntryDate <= input$syso_date_range[1] &
            LatestExit > input$syso_date_range[1] &
            PriorLivingSituation = Literally Homeless OR
            ProjectType IN (0,2,8) OR
            ProjectType IN (1, 4, 14) AND Bednight/Contact within __ days of ReportStartDate ~ "Enrolled: Homeless",

            ProjectType %in% coc_funded_project_types &
            EntryDate <= ReportStartDate
            ExitDate is null or > ReportStartDate
            PriorLivingSituation = Literally Homeless
            ProjectType IN (3, 9, 10, 13)
            MoveInDate <= ReportStartDate  ~ "Enrolled: Housed"

        )
    )

system_activity_chart_prep <- system_activity_prep %>%


# x.axis.Var cat.Var   group.id start.Bar values end.Bar total.by.x
system_activity_prep2 <- data.frame(
    x.axis.Var = rep(c(paste0("Active as of ",ymd("20230105")), "Inflow", "Outflow", paste0("Active as of ", ymd("20230210"))), 4),
    cat.Var = rep(c("Enrolled: Homeless", "Enrolled: Housed", "Inflow", "Outflow"), each=4),
    values = c(700, 0, 0, -750, # Enrolled: Homeless
               300, 0, 0, -150, # Enrolled: Housed
               0, 200, 0, 0,   # Inflow
               0, 0, -300, 0   # Outflow
               )
    )

# Get max Y value
sum_active_as_of_start <- system_activity_prep %>%
  filter(x.axis.Var == "Active as of 2023-01-05") %>%
  summarise(sum_values = sum(values))

# Sum of positive values for x.axis.Var = "Inflow"
sum_inflow <- system_activity_prep %>%
  filter(x.axis.Var == "Inflow", values > 0) %>%
  summarise(sum_values = sum(values))

# Total sum
total_sum <- sum_active_as_of_start$sum_values + sum_inflow$sum_values

system_activity_prep2 <- system_activity_prep %>%
    mutate(cat.Var = factor(cat.Var,
                         levels = c("Enrolled: Homeless", "Enrolled: Housed", "Inflow", "Outflow"))) %>%
    group_by(cat.Var) %>%
    mutate(group.id = row_number(cat.Var)) %>%
    ungroup() %>%
    arrange(group.id, desc(cat.Var)) %>%
    mutate(end.Bar = cumsum(values),
        start.Bar = c(0, head(end.Bar, -1))) %>%
    select(names(system_activity_prep),group.id, end.Bar,start.Bar)

ggplot(system_activity_prep2, aes(x = group.id, fill = cat.Var)) + 
    # \_Simple Waterfall Chart ----
    geom_rect(aes(xmin = group.id - 0.25, # control bar gap width
                    xmax = group.id + 0.25, 
                    ymin = end.Bar,
                    ymax = start.Bar),
                color="black", 
                alpha=0.95
    ) + 
    # \_Lines Between Bars ----
    geom_segment(aes(x=ifelse(group.id == last(group.id),
                                last(group.id),
                                group.id+0.25), 
                    xend=ifelse(group.id == last(group.id),
                                last(group.id),
                                group.id+0.75), 
                    y=ifelse(cat.Var == "Enrolled: Homeless",
                                end.Bar,
                                # these will be removed once we set the y limits
                                max(end.Bar)*2), 
                    yend=ifelse(cat.Var == "Enrolled: Homeless",
                                end.Bar,
                                # these will be removed once we set the y limits
                                max(end.Bar)*2)), 
                colour="black"
    ) +
    # \_Numbers inside bars (each category) ----
    geom_text(
        mapping = 
        aes(
            label = ifelse(abs(values) < 150, "", as.character(values)),
            y = rowSums(cbind(start.Bar,values/2))
            ),
        color = "black",
        fontface = "bold",
        size=10
    ) + 
    # \_Change colors ----
    scale_fill_manual(
        values=c('#73655E','#C6BDB9','#C34931', '#16697A')
    ) +
    # \_Change y axis to same scale as original ----
    scale_y_continuous(
        expand=c(0,0),
        limits = c(0, total_sum),
        breaks = NULL,
        labels = NULL
    ) +
    # \_Add tick marks on x axis to look like the original plot ----
    scale_x_continuous(
        expand=c(0,0),
        limits = c(min(system_activity_prep2$group.id)-0.5,max(system_activity_prep2$group.id)+0.5),
        breaks = c(min(system_activity_prep2$group.id)-0.5,
                unique(system_activity_prep2$group.id), 
                unique(system_activity_prep2$group.id) + 0.5
                ),
        labels = 
        c("", 
            as.character(unique(system_activity_prep2$x.axis.Var)), 
            rep(c(""), length(unique(system_activity_prep2$x.axis.Var)))
        )
    ) +
    # \_Theme options to make it look like the original plot ----
    theme(
        text = element_text(size = 14, color = "#4e4d47"),
        axis.text = element_text(size = 10, color = "#4e4d47", face = "bold"),
        axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm")),
        axis.ticks.x = element_line(
            color =
                c("black",
                rep(NA, length(unique(system_activity_prep2$x.axis.Var))),
                rep("black", length(unique(system_activity_prep2$x.axis.Var))-1)
                )
        ),
        axis.line = element_line(colour = "#4e4d47", linewidth = 0.5),
        axis.ticks.length = unit(.15, "cm"),
        axis.title.x =      element_blank(),
        axis.title.y =      element_blank(),
        panel.background =  element_blank(),
        panel.border    =   element_blank(),
        panel.grid.major=   element_blank(),
        panel.grid.minor=   element_blank(),
        plot.background=    element_blank(),
        plot.margin =        unit(c(1, 1, 1, 1), "lines"),
        legend.text =        element_text(size = 10, 
                                        color = "#4e4d47",
                                        face = "bold",
                                        margin = margin(l = 0.25, unit = "cm")
                                        ),
        legend.title =       element_blank()
    )