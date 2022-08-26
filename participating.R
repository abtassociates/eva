library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)

aaqs <-
  tribble(
    ~AAQ, ~Date, ~Who,
    186148, "10/4/2021", "North Carolina Coalition to End Homelessness",
    187950, "11/4/2021", "North Carolina Coalition to End Homelessness",
    185465, "10/4/2021", "North Carolina Coalition to End Homelessness",
    185398, "9/21/2021", "North Carolina Coalition to End Homelessness",
    185040, "9/15/2021", "Regional Task Force on the Homeless",
    180837, "7/2/2021", "Planning Council, The",
    180778, "7/1/2021", "Planning Council, The",
    175230, "3/23/2021", "North Carolina Coalition to End Homelessness",
    174918, "3/18/2021", "Champaign County Regional Planning Commission",
    174647, "4/19/2021", "Gulf Coast Partnership",
    172050, "1/27/2021", "Coalition on Homelessness and Housing in Ohio",
    139254, "6/6/2019", "Coalition on Homelessness and Housing in Ohio",
    138983, "5/31/2019", "Coalition on Homelessness and Housing in Ohio",
    139743, "6/17/2019", "Coalition on Homelessness and Housing in Ohio",
    171543, "1/20/2021", "Appalachian Regional Coalition on Homelessness",
    171120, "1/14/2021", "Appalachian Regional Coalition on Homelessness",
    189532, "12/10/2021", "Appalachian Regional Coalition on Homelessness"
  )

ggplot(aaqs, aes(x = mdy(Date), y = !is.na(AAQ), colour = Who)) +
  geom_point(alpha = .7) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE)
