# run 00-Start_here.R EXCEPT for the "Data Prep" section

Project <- Project %>%
  mutate(ProjectName = str_replace(ProjectName, "-", "+"))

# write edits to the csvs -------------------------------------------------

write.csv(
  Project,
  here(paste0(directory, "data/Project.csv")),
  na = "",
  row.names = FALSE
)


