# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(janitor)
library(tidyverse)
library(lubridate)
library(HMIS)
library(here)
# library(sf)
# library(urbnmapr)
# library(choroplethrMaps)
# library(plotly)

if (!exists("Enrollment"))
  load("images/COHHIOHMIS.RData")
if(!exists("dq_main"))
  load("images/Data_Quality.RData")
if (!exists("tay")) {
  load("images/cohorts.RData")
  # rlang::env_binding_lock(environment(), ls())
}

# ohio_counties <- st_read("Ohio/counties/REFER_COUNTY.shp")
# 
# counties <- get_urbn_map("counties", sf = TRUE)
# 
# counties <- st_transform(counties, "+init=epsg:3857")
# 
# counties <- counties %>%
#   mutate(county_name = str_remove(county_name, " County"))
# 
# data(county.map)
# 
# oh_counties <- county.map %>% filter(STATE == 39) %>% select(NAME, region)

# Pinpointing where Vaccines are Wanted -----------------------------------

# who's already been vaccinated?

one_dose_and_done <- doses %>%
  filter(COVID19VaccineManufacturer == "Johnson & Johnson") %>%
  select(PersonalID, COVID19DoseDate) %>%
  group_by(PersonalID) %>%
  slice_max(COVID19DoseDate) %>%
  select(PersonalID, "LastDose" = COVID19DoseDate) %>% 
  unique() 

complete <- doses %>%
  group_by(PersonalID) %>%
  summarise(Doses = n()) %>%
  ungroup() %>%
  filter(Doses > 1) %>%
  left_join(doses, by = "PersonalID") %>%
  group_by(PersonalID) %>%
  mutate(LastDose = lag(COVID19DoseDate, order_by = COVID19DoseDate)) %>%
  filter(!is.na(LastDose)) %>%
  mutate(DaysBetweenDoses = difftime(COVID19DoseDate, LastDose, units = "days")) %>%
  filter(DaysBetweenDoses >= 20) %>%
  select(PersonalID, LastDose) %>% 
  unique() %>%
  rbind(one_dose_and_done) %>%
  mutate(HasAllDoses = "Yes") %>%
  unique() %>%
  mutate(
    FullyVaccinated = case_when(
      ymd(LastDose) >= today() - days(14) ~ "No",
      ymd(LastDose) < today() - days(14) ~ "Yes"
    )
  )

# deduping enrollment data taking the most recent open enrollment
most_recent_entries <- co_clients_served %>%
  filter(AgeAtEntry >= 16 &
           is.na(ExitDate) &
           (ProjectType %in% c(lh_project_types) |
              (ProjectType %in% c(ph_project_types) &
                 is.na(MoveInDateAdjust)))
  ) %>%
  group_by(PersonalID) %>%
  slice_max(EntryDate) %>%
  slice_max(EnrollmentID) %>%
  ungroup()

# cohort of clients = current, over 16, and literally homeless in any ptc
current_over16_lh <- most_recent_entries %>%
  select(CountyServed, PersonalID, ProjectName) %>%
  left_join(covid19[c("PersonalID", "ConsentToVaccine", "VaccineConcerns")],
            by = "PersonalID") %>%
  left_join(complete, by = "PersonalID") %>%
  mutate(HasAllDoses = if_else(is.na(HasAllDoses),
                                   "Not acc. to HMIS",
                                   HasAllDoses))

# getting total clients included per county
total_lh_by_county <- current_over16_lh %>%
  count(CountyServed) %>%
  rename("TotalLH" = n) %>%
  arrange(desc(TotalLH))

# getting consent data on everyone, getting data ready to turn
consent_status <- current_over16_lh %>%
  mutate(
    ConsentToVaccine = if_else(is.na(ConsentToVaccine), 
                               "Data not collected (HUD)", 
                               ConsentToVaccine),
    Status = case_when(
      HasAllDoses == "Yes" ~ "Has All Doses",
      ConsentToVaccine == "Yes (HUD)" ~ "Answered Yes to Consent question",
      !ConsentToVaccine %in% c("Yes (HUD)", "No (HUD)") ~ "Consent Unknown",
      ConsentToVaccine == "No (HUD)" ~ "Answered No to Consent question")) 

# turning the data so each Status has its own column and it's by County
consent_status_by_county <- consent_status %>%
  count(CountyServed, Status) %>%
  pivot_wider(names_from = Status,
              values_from = n,
              values_fill = 0)

# putting all the data together
totals_by_county <- total_lh_by_county %>%
  left_join(consent_status_by_county, by = "CountyServed") %>%
  clean_names() %>%
  rename("county_name" = county_served)

# creating sf data object with the pre-shaped data
# vaccine_distribution_county <- counties %>%
#   filter(state_fips == 39, # Ohio
#          !county_fips %in% c(39113, # Montgomery
#                              39035, # Cuyahoga
#                              39049, # Franklin
#                              39153, # Summit
#                              39061, # Hamilton
#                              39095, # Lucas 
#                              39151)) %>% # Stark
#   left_join(totals_by_county, by = "county_name") %>%
#   mutate(across(7:11, ~replace_na(.x, 0)),
#          hover = paste0(county_name, ": \n", 
#                         consent_unknown,
#                         " + | Consent Unknown\n",
#                         answered_no_to_consent_question,
#                         " + | Would Not Consent\n",
#                         answered_yes_to_consent_question,
#                         " + | Would Consent\n",
#                         has_all_doses,
#                         " + | Has All Doses\n= ",
#                         total_lh,
#                         " | Total Over 16 and Literally Homeless")) 
# 
# creating plot
# consent_plot <- ggplot(counties %>% filter(state_fips == 39)) + 
#   geom_sf() +
#   geom_sf(vaccine_distribution_county, 
#           mapping = aes(fill = total_lh)) +  
#   geom_sf_label(vaccine_distribution_county,
#                 mapping = aes(label = hover)) +
#   # geom_sf_text(counties %>% filter(state_fips == 39),
#   #              mapping = aes(label = county_name),
#   #              check_overlap = TRUE,
#   #              size = 3,
#   #              color = "slategray3") +
#   scale_fill_viridis_c(super = ScaleContinuous) +
#   labs(title = "Would Consent to Vaccine") +
#   theme_void()

# # making it usable
# ggplotly(consent_plot,
#          tooltip = "text")


# Trying Leaflet ----------------------------------------------------------


# Connecting Clients to their 2nd Doses -----------------------------------

vaccine_needs_second_dose <- dose_counts %>%
  filter(Doses == 1) %>%
  left_join(doses, by = "PersonalID") %>%
  left_join(most_recent_entries, by = "PersonalID") %>%
  filter(COVID19VaccineManufacturer != "Johnson & Johnson") %>%
  mutate(
    NextDoseNeededDate = case_when(
      COVID19VaccineManufacturer == "Moderna" ~
        ymd(COVID19DoseDate) + days(28),
      COVID19VaccineManufacturer == "Pfizer" ~
        ymd(COVID19DoseDate) + days(21),
      str_starts(COVID19VaccineManufacturer, "Client doesn't know") == TRUE ~
        ymd(COVID19DoseDate) + days(28)
    ),
    CurrentLocation = case_when(
      is.na(EntryDate) ~ if_else(
        is.na(VaccineContactInfo),
        "No contact info and not currently enrolled in any project.",
        VaccineContactInfo
      ),
      today() >= ymd(EntryDate) &
        (ymd(ExitDate) > today()) | is.na(ExitDate) ~ 
        paste(
          "Currently in",
          ProjectName,
          "Contact Info:",
          VaccineContactInfo
        ),
        ExitDate <= today() ~ paste(
          "Exited",
          ProjectName,
          "on",
          ExitDate,
          "to",
          living_situation(Destination),
          "Contact info:",
          VaccineContactInfo
        )
    ),
    DaysUntilNextDose = ymd(NextDoseNeededDate) - today(),
    VeteranStatus = case_when(
      VeteranStatus == 0 ~ "No",
      VeteranStatus == 1 ~ "Yes",
      TRUE ~ "Unknown"
    ),
    AgeAtEntry = case_when(
      AgeAtEntry < 12 ~ "0-11",
      AgeAtEntry < 16 ~ "12-15",
      AgeAtEntry < 25 ~ "16-24",
      AgeAtEntry < 65 ~ "25-59",
      AgeAtEntry < 75 ~ "60-74",
      AgeAtEntry < 85 ~ "75-84",
      AgeAtEntry < 120 ~ "85+",
      TRUE ~ "Unknown"
    ),
    HowSoon = case_when(
      DaysUntilNextDose < 0 ~ "Overdue",
      DaysUntilNextDose > 7 ~ "Next Week",
      DaysUntilNextDose > 3 ~ "7 days",
      DaysUntilNextDose >= 0 ~ "3 days"
    )
  ) %>%
  select(
    PersonalID,
    HouseholdID,
    CountyServed,
    COVID19VaccineManufacturer,
    AgeAtEntry,
    VeteranStatus,
    NextDoseNeededDate,
    HowSoon,
    DaysUntilNextDose,
    CurrentLocation
  )

# Client Statuses ---------------------------------------------------------

get_county_from_provider <- co_clients_served %>%
  left_join(Project %>%
              select(ProjectName, ProjectCounty), by = "ProjectName") %>%
  mutate(
    CountyGuessed = if_else(is.na(CountyServed) |
                              CountyServed == "--Outside of Ohio--", 1, 0),
    CountyServed = case_when(
      CountyGuessed == 1 &
        ProjectName != "Unsheltered Clients - OUTREACH" ~ ProjectCounty,
      CountyGuessed == 0 ~ CountyServed,
      TRUE ~ "Unsheltered in unknown County"
    ),
    ProjectCounty = NULL
  )

co_clients_served_county_guesses <- get_county_from_provider %>%
  left_join(Enrollment %>%
              select(EnrollmentID, UserCreating), by = "EnrollmentID") %>%
  mutate(
    UserID = gsub(pattern = '[^0-9\\.]', '', UserCreating, perl = TRUE)
  ) %>%
  left_join(Users %>%
              mutate(UserID = as.character(UserID)) %>%
              select(UserID, UserCounty), by = "UserID") %>%
  mutate(CountyServed = if_else(CountyServed == "Unsheltered in unknown County",
                                UserCounty,
                                CountyServed)) %>%
  select(-starts_with("User"))

vaccine_status <- co_clients_served_county_guesses %>%
  left_join(complete %>% select(-LastDose), by = "PersonalID") %>%
  mutate(HasAllDoses = if_else(is.na(HasAllDoses), "No", HasAllDoses)) %>%
  left_join(vaccine_needs_second_dose[c("PersonalID", "HouseholdID", "HowSoon")],
            by = c("HouseholdID", "PersonalID")) %>% 
  left_join(covid19[c("PersonalID", "ConsentToVaccine")], 
            by = c("PersonalID")) %>%
  mutate(
    ConsentToVaccine = if_else(is.na(ConsentToVaccine), 
                               "Data not collected", 
                               ConsentToVaccine),
    AgeAtEntry = case_when(
      AgeAtEntry < 12 ~ "0-11",
      AgeAtEntry < 16 ~ "12-15",
      AgeAtEntry < 25 ~ "16-24",
      AgeAtEntry < 65 ~ "25-59",
      AgeAtEntry < 75 ~ "60-74",
      AgeAtEntry < 85 ~ "75-84",
      AgeAtEntry < 120 ~ "85+",
      TRUE ~ "Unknown"
    ),
    VaccineStatus = case_when(
      FullyVaccinated == "Yes" ~ "Fully vaccinated",
      HasAllDoses == "Yes" ~ "Has all doses",
      !is.na(HowSoon) ~ "Needs 2nd dose",
      ConsentToVaccine == "Yes (HUD)" ~ "Not vaccinated, would consent",
      ConsentToVaccine == "No (HUD)" ~ "Not vaccinated, would not consent",
      !ConsentToVaccine %in% c("Yes (HUD)", "No (HUD)", "Data not collected") ~ 
        "Not vaccinated, consent unknown",
      ConsentToVaccine == "Data not collected" ~ "Data not collected"
    ),
    VaccineStatus = factor(VaccineStatus, levels = c(
      "Fully vaccinated",
      "Has all doses",
      "Needs 2nd dose",
      "Not vaccinated, would consent",
      "Not vaccinated, would not consent",
      "Not vaccinated, consent unknown",
      "Data not collected"
    ))
  ) %>%
  select(
    PersonalID,
    HouseholdID,
    CountyServed,
    ProjectName,
    AgeAtEntry,
    RelationshipToHoH,
    VeteranStatus,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    VaccineStatus
  )

 # Concerns ----------------------------------------------------------------
# 
# concerns <- covid19 %>%
#   select(PersonalID, ConsentToVaccine, VaccineConcerns) %>%
#   filter(ConsentToVaccine != "Yes (HUD)" & !is.na(VaccineConcerns))
# 
# text <- concerns$VaccineConcerns
# 
# text <- tolower(text)
# 
# text <- str_replace(text, "side affects", "side effects")
# 
# text <-
#   removeWords(
#     text,
#     c(
#       "am",
#       "is",
#       "are",
#       "was",
#       "been",
#       "did",
#       "want",
#       "will",
#       "would",
#       "doesnt",
#       "dont",
#       "have",
#       "has",
#       "hasn't",
#       "hasnt",
#       "does",
#       "doesn't",
#       "don't",
#       "the",
#       "not",
#       "and",
#       "vaccine",
#       "vaccines",
#       "about",
#       "into",
#       "for",
#       "its",
#       "it's",
#       "that"
#     )
#   )
# 
# cloud <- Corpus(VectorSource(text))
# 
# cloud <- tm_map(cloud, content_transformer(tolower)) %>%
#   tm_map(removeNumbers) %>%
#   tm_map(removePunctuation) %>%
#   tm_map(stripWhitespace)
# 
# vaccine_concerns_cloud <- wordcloud(
#   cloud,
#   colors = brewer.pal(8, "Dark2"),
#   random.order = FALSE,
#   random.color = FALSE,
#   scale = c(3, .2)
# )


# Who needs followup? -----------------------------------------------------


served_since_02052021 <- co_clients_served %>%
  filter(served_between(., hc_bos_start_vaccine_data, meta_HUDCSV_Export_End)) %>%
  count(ProjectName) %>%
  rename("totalserved" = n)

exited <- missing_vaccine_exited %>%
  count(ProjectName) %>%
  rename("missingexited" = n)

current <- missing_vaccine_current %>%
  count(ProjectName) %>%
  rename("missingcurrent" = n)

all <- full_join(served_since_02052021, current, by = "ProjectName") %>%
  full_join(exited, by = "ProjectName") %>%
  mutate(missingcurrent = replace_na(missingcurrent, 0),
         missingexited = replace_na(missingexited, 0),
         allmissing = missingcurrent + missingexited,
         percentmissing = allmissing/totalserved) 

write_csv(all, "random_data/percentmissing.csv")

# to update this, I'm saving this to DB > HMIS > Covid-19 Data Analysis for EM

# cleanup -----------------------------------------------------------------

rm(list = ls()[!(
  ls() %in% c(
    "vaccine_needs_second_dose",
    "vaccine_status"
  )
)])

save(list = ls(),
     file = "images/COVID_vaccine.RData",
     compress = FALSE)

