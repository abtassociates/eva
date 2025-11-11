library(gh)
library(dplyr)
library(lubridate)
library(jsonlite) # Often useful when working with API data structures

# --- Configuration ---
repo_owner <- "abtassociates" # Replace with your GitHub username or organization
repo_name <- "eva"       # Replace with your repository name
output_file <- "/media/sdrive/projects/CE_Data_Toolkit/github_pull_requests.csv" # Output file name

# Define the date range for filtering (inclusive)
# Start of 12/2023
start_date_filter <- ymd_hms("2023-12-01 00:00:00", tz = "UTC")
# End of 03/2025 (end of the day)
end_date_filter <- ymd_hms("2025-03-31 23:59:59", tz = "UTC")

# --- Authentication ---
# gh package automatically looks for GITHUB_PAT environment variable.
# If it's not set, you might need to authenticate manually, but using the ENV var is preferred.
# auth_from_env() # This is often not needed if GITHUB_PAT is set correctly

# --- Fetch Pull Requests ---

message("Fetching pull requests from ", repo_owner, "/", repo_name, "...")

all_prs <- list()
page <- 1
per_page <- 100 # Max allowed by GitHub API

while (TRUE) {
  message("Fetching page ", page, "...")
  
  # Make the API call to get pull requests
  # Fetching state='all' is necessary to get both open and closed PRs
  # which we then filter by date.
  response <- tryCatch({
    gh(
      glue::glue("GET /repos/{repo_owner}/{repo_name}/pulls"),
      owner = repo_owner,
      repo = repo_name,
      state = "all",
      per_page = per_page,
      page = page
    )
  }, error = function(e) {
    message("Error fetching page ", page, ": ", e$message)
    return(NULL) # Indicate error or end of data
  })
  
  if (is.null(response) || length(response) == 0) {
    message("No more PRs found or error occurred.")
    break # Stop fetching if response is empty or error
  }
  
  # Append current page data to the list
  all_prs <- c(all_prs, response)
  
  # Check if this is the last page
  if (length(response) < per_page) {
    message("Last page fetched.")
    break
  }
  
  page <- page + 1
  
  # Optional: Add a small delay to avoid hitting rate limits too hard
  # Sys.sleep(0.1)
}

message("Finished fetching ", length(all_prs), " potential pull requests.")

# --- Process and Filter Data ---

if (length(all_prs) == 0) {
  message("No pull requests found for this repository.")
} else {
  
  message("Processing and filtering data...")
  
  # Convert the list of PR data into a data frame
  # Use bind_rows for robustness against slightly different structures (though less common here)
  pr_data_raw <- bind_rows(lapply(all_prs, function(pr) {
    # Extract needed fields
    df <- data.frame(
      name = pr$title,
      date_opened = pr$created_at,
      date_closed = if(is.null(pr$closed_at)) "" else pr$closed_at, # Will be NULL for open PRs
      opened_by = pr$user$login,
      # total_commits = pr$commits,
      # total_files_changed = length(pr$changed_files),
      pr_branch = pr$head$ref,
      merge_to_branch = pr$base$ref,
      stringsAsFactors = FALSE
    )
    
    pr_detail <- tryCatch({
      gh(
        glue::glue("GET /repos/{repo_owner}/{repo_name}/pulls/{pr$num}"),
        owner = repo_owner,
        repo = repo_name,
        pull_number = pr$num
      )
    }, error = function(e) {
      message("  Error fetching details for PR #", pr$num, ": ", e$message)
      return(NULL) # Indicate error
    })
    
    df$total_commits <- pr_detail$commits
    df$total_files_changed <- pr_detail$changed_files
    
    df
  }))
  
  # Convert date columns to datetime objects
  pr_data_raw <- pr_data_raw %>%
    mutate(
      date_opened = ymd_hms(date_opened, tz = "UTC"),
      date_closed = ymd_hms(date_closed, tz = "UTC")
    )
  
  # Apply the date filter: opened OR closed within the range
  pr_data_filtered <- pr_data_raw %>%
    filter(
      (date_opened >= start_date_filter & date_opened <= end_date_filter) | # Opened within range
        (!is.na(date_closed) & date_closed >= start_date_filter & date_closed <= end_date_filter) # Closed within range
    )
  
  # Select and rename columns for the final output
  final_output_data <- pr_data_filtered %>%
    select(
      `Name of PR` = name,
      `Date opened` = date_opened,
      `Date closed` = date_closed,
      `Who opened the PR` = opened_by,
      `Total commits` = total_commits,
      `Total files changed` = total_files_changed,
      `PR Branch` = pr_branch,
      `Merge-To Branch` = merge_to_branch
    )
  
  # Format dates nicely for the output CSV (optional, default is ISO 8601)
  # You can adjust the format string as needed
  final_output_data <- final_output_data %>%
    mutate(
      `Date opened` = format(`Date opened`, "%Y-%m-%d %H:%M:%S"),
      `Date closed` = format(`Date closed`, "%Y-%m-%d %H:%M:%S")
    )
  
  
  # --- Export Data ---
  
  message("Exporting filtered data to ", output_file, "...")
  
  write.csv(final_output_data, file = output_file, row.names = FALSE, na = "")
  
  message("Script finished successfully. Data exported to ", output_file)
}