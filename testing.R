active <- active_list %>%
  filter(CountyServed %in% c("Ashland", "Clark", "Ashtabula", "Portage")) %>%
  arrange(HouseholdID)

non_veterans <- active %>%
  filter(VeteranStatus != 1) %>%
  pull(PersonalID)

non_chronic <- active %>%
  filter(ChronicStatus == "Not Chronic") %>%
  pull(PersonalID)

non_TAY <- active %>%
  filter(TAY == 0) %>%
  pull(PersonalID)

no_disability <- active %>%
  filter(DisabilityInHH == 0) %>%
  pull(PersonalID)

prioritizationFilterVeteran <- FALSE
prioritizationFilterChronic <- FALSE
prioritizationFilterTAY <- TRUE
prioritizationFilterDisability <- TRUE

final <- active %>%
  mutate(
    show = case_when(
      (prioritizationFilterVeteran == TRUE &
        PersonalID %in% c(non_veterans)) |
      (prioritizationFilterChronic == TRUE &
        PersonalID %in% c(non_chronic)) |
      (prioritizationFilterTAY == TRUE &
        PersonalID %in% c(non_TAY)) |
      (prioritizationFilterDisability == TRUE &
        PersonalID %in% c(no_disability)) ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  filter(show == 1) %>%
  select("PersonalID", "ProjectName", "EntryDate", "PTCStatus", 
         "CountyServed", "PHTrack", "HouseholdSize", "IncomeFromAnySource", 
         "Score", "ChronicStatus")