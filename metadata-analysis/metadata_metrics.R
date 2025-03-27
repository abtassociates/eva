# Load required libraries
library(data.table)
library(ggplot2)
library(lubridate)

# Read metadata
metadata <- fread("/media/sdrive/projects/CE_Data_Toolkit/Eva metadata/metadata.csv", header=TRUE)

# Convert Datestamp to POSIXct (ensure it's in UTC)
metadata[, `:=`(
  Datestamp = as.POSIXct(Datestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
  Date = as.Date(Datestamp), 
  Month = floor_date(Datestamp, "month"),  # Extract the month
  Hour = floor_date(Datestamp, "hour")
)]

# Unwanted session details
unwanted_details <- c("Session started", "User on tabHome", "Timed out", "Session Ended")

# Convert the target event list to a sorted string for comparison
unwanted_details_str <- paste(sort(unwanted_details), collapse = "|")

# Group by SessionToken, collapse details into sorted string
sessions_to_exclude <- metadata[, .(DetailsStr = paste(sort(unique(Details)), collapse = "|")), by = SessionToken][
  DetailsStr == unwanted_details_str, SessionToken]

unwanted_details <- c("Session started", "User on tabHome", "Session Ended")

# Convert the target event list to a sorted string for comparison
unwanted_details_str <- paste(sort(unwanted_details), collapse = "|")

sessions_to_exclude2 <- metadata[, .(DetailsStr = paste(sort(unique(Details)), collapse = "|")), by = SessionToken][
  DetailsStr == unwanted_details_str, SessionToken]

# Filter out those sessions
metadata_filtered <- metadata[!(SessionToken %in% c(sessions_to_exclude, sessions_to_exclude2)) & Details %in% c("Session started", "Session Ended")]

# ---- 1. Compute Average Sessions Per Hour for each Month ----
# Extract hour component and month
metadata_filtered[, Hour := floor_date(Datestamp, "hour")]

# Get unique session count per month and hour
sessions_per_hour_month <- metadata_filtered[, .(UniqueSessions = uniqueN(SessionToken)), by = .(Month, Hour)]

# Compute the average unique sessions per hour for each month
avg_sessions_per_month <- sessions_per_hour_month[, .(AvgSessionsPerHour = mean(UniqueSessions)), by = Month]

# ---- 2. Compute Average Session Length Per Hour ----
# Reshape data to wide format (matching start & end by SessionToken)
session_times <- dcast(metadata_filtered, SessionToken ~ Details, value.var = "Datestamp")

# Rename columns
setnames(session_times, c("SessionToken", "Session started", "Session Ended"), c("SessionToken", "Start", "End"))

# Compute session length in minutes
session_times[, SessionLength := as.numeric(difftime(End, Start, units = "mins"))]

# Aggregate average session length per hour for each month
session_times[, Month := floor_date(Start, "month")]
avg_session_length_month <- session_times[, .(AvgSessionLength = mean(SessionLength, na.rm = TRUE)), by = Month]

# ---- 3. Merge Data for Plotting ----
plot_data <- merge(avg_sessions_per_month, avg_session_length_month, by = "Month", all = TRUE)
plot_data[, Month := as.POSIXct(Month)]

# Convert to long format for ggplot
plot_data_long <- melt(plot_data, id.vars = "Month", variable.name = "Metric", value.name = "Value")

plot_data_final <- plot_data_long[plot_data_long$Value < 100]

# ---- 4. Plot the Data ----
ggplot(plot_data_final, aes(x = Month, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +    # Draws the connecting lines
  geom_point(size = 2) +     # Shows individual data points
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +  # Monthly labels
  labs(title = "Sessions Per Hour & Session Length by Month",
       x = "Month", y = "Value",
       color = "Metric") +
  scale_color_manual(values = c("AvgSessionsPerHour" = "blue", 
                                "AvgSessionLength" = "red"),  # Assign colors
                     labels = c("AvgSessionsPerHour" = "Sessions Per Hour", 
                                "AvgSessionLength" = "Session Length (mins)")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Load required libraries
library(data.table)
library(ggplot2)
library(lubridate)

# Read metadata
metadata <- fread("/media/sdrive/projects/CE_Data_Toolkit/Eva metadata/metadata.csv", header=TRUE)

# Convert Datestamp to POSIXct (ensure it's in UTC)
metadata[, `:=`(
  Datestamp = as.POSIXct(Datestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
  Date = as.Date(Datestamp), 
  Hour = floor_date(Datestamp, "hour")
)]

unwanted_details <- c("Session started", "User on tabHome", "Timed out", "Session Ended")

# Convert the target event list to a sorted string for comparison
unwanted_details_str <- paste(sort(unwanted_details), collapse = "|")

# Group by SessionToken, collapse details into sorted string
sessions_to_exclude <- metadata[, .(DetailsStr = paste(sort(unique(Details)), collapse = "|")), by = SessionToken][
  DetailsStr == unwanted_details_str, SessionToken]

unwanted_details <- c("Session started", "User on tabHome", "Session Ended")

# Convert the target event list to a sorted string for comparison
unwanted_details_str <- paste(sort(unwanted_details), collapse = "|")

sessions_to_exclude2 <- metadata[, .(DetailsStr = paste(sort(unique(Details)), collapse = "|")), by = SessionToken][
  DetailsStr == unwanted_details_str, SessionToken]

# Filter out those sessions
metadata_filtered <- metadata[!(SessionToken %in% c(sessions_to_exclude, sessions_to_exclude2)) & Details %in% c("Session started", "Session Ended")]

# ---- 1. Compute Average Sessions Per Hour ----
# Extract hour component
metadata_filtered[, Hour := floor_date(Datestamp, "hour")]


# Get unique session count per hour
# sessions_per_hour <- metadata_filtered[, .(UniqueSessions = uniqueN(SessionToken)), by = Hour]
sessions_per_hour <- metadata[, .(UniqueSessions = uniqueN(SessionToken)), by = .(Date, Hour)]

# Compute the average unique sessions per hour
# avg_sessions_per_hour <- sessions_per_hour[, .(AvgSessions = mean(UniqueSessions)), by = Hour]
avg_sessions_per_day <- sessions_per_hour[, .(AvgSessionsPerHour = mean(UniqueSessions)), by = Date]

# ---- 2. Compute Average Session Length Per Hour ----
# Reshape data to wide format (matching start & end by SessionToken)
session_times <- dcast(metadata_filtered, SessionToken ~ Details, value.var = "Datestamp")

# Rename columns
setnames(session_times, c("SessionToken", "Session started", "Session Ended"), c("SessionToken", "Start", "End"))

# Compute session length in minutes
session_times[, SessionLength := as.numeric(difftime(End, Start, units = "mins"))]

# Aggregate average session length per hour
session_times[, Date := as.Date(floor_date(Start, "day"))]
avg_session_length <- session_times[, .(AvgSessionLength = mean(SessionLength, na.rm = TRUE)), by = Date]

# ---- 3. Merge Data for Plotting ----
plot_data <- merge(avg_sessions_per_day, avg_session_length, by = "Date", all = TRUE)
plot_data[, Date := as.POSIXct(Date)]

# Convert to long format for ggplot
plot_data_long <- melt(plot_data, id.vars = "Date", variable.name = "Metric", value.name = "Value")

plot_data_final <- plot_data_long[plot_data_long$Value < 100 & plot_data_long$Date > ymd("2025-01-01")]

# ---- 4. Plot the Data ----
ggplot(plot_data_final, aes(x = Date, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +    # Draws the connecting lines
  geom_point(size = 2) +     # Shows individual data points
  scale_x_datetime(date_labels = "%b %d", date_breaks = "3 days") +  # Hourly labels
  labs(title = "Average Sessions Per Hour & Average Session Length",
       x = "Date (UTC)", y = "Value",
       color = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
