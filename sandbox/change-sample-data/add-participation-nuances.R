
colnames(HMISParticipation)

max_id <- max(HMISParticipation$HMISParticipationID)

total_projects <- nrow(Project)

stopped_participating <- HMISParticipation %>%
  filter(HMISParticipationType == 1 & #1==2&
           !is.na(HMISParticipationStatusEndDate)) %>%
  pull(ProjectID)

if(is_empty(stopped_participating)) { # if no projects stopped participating
  random_project_id <-
    Project$ProjectID[runif(n = 1, min = 1, max = total_projects)]
  
  stopped_date <- meta_HUDCSV_Export_End() - weeks(4)
  
  HMISParticipation <- HMISParticipation %>%
    mutate(
      HMISParticipationStatusEndDate = if_else(
        ProjectID == random_project_id,
        stopped_date,
        HMISParticipationStatusEndDate
      )
    )
} else{ # if at least 1 project stopped participating
  random_project_id <- sample(stopped_participating, 1)
  
  stopped_date <-
    HMISParticipation %>% filter(ProjectID == random_project_id) %>%
    pull(HMISParticipationStatusEndDate)
  

}

x <- HMISParticipation %>%
  add_row(
    HMISParticipationID = paste0(max_id, random_project_id),
    ProjectID = random_project_id,
    HMISParticipationType = 1,
    HMISParticipationStatusStartDate = stopped_date + days(6),
    HMISParticipationStatusEndDate = NA,
    DateCreated = now(),
    DateUpdated = now(),
    UserID = "Yugi",
    DateDeleted = NA,
    ExportID = "Sunny"
  )


dir.create("sandbox/mini-non-shiny-environment/zip-output")

file.copy(
  from = "sandbox/mini-non-shiny-environment/data/",
  to = "sandbox/mini-non-shiny-environment/zip-output/",
  overwrite = TRUE,
  recursive = TRUE
)

# rename the folder named "data" to whatever the output is

write_csv(x,
          "sandbox/mini-non-shiny-environment/zip-output/project-w-2-hmisps/HMISParticipation.csv")


