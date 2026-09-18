library(data.table)
library(zip)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================
INPUT_ZIP       <- "tests/FY26-test-good.zip"      # Template HMIS ZIP
OUTPUT_DIR      <- "/media/sdrive/projects/CE_Data_toolkit/New Data Sets" # Staging directory
OUTPUT_ZIP      <- paste0(OUTPUT_DIR, "synthetic_hmis_210mb.zip") # Final ZIP output
TARGET_STOP_MB  <- 200.0                      # Real-time target
HARD_MAX_MB     <- 200.0                      # Hard upper cap (Strictly < 222 MB)

# Reset working directories
unzip_dir <- tempfile()
dir.create(unzip_dir)
unzip(INPUT_ZIP, exdir = unzip_dir)

# ==============================================================================
# 2. DYNAMICALLY SCAN & CLASSIFY ALL CSV FILES
# ==============================================================================
cat("[1/4] Scanning and categorizing 100% of input CSVs...\n")

csv_files <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
if (length(csv_files) == 0) stop("No CSV files found in input ZIP!")

tables <- list()
for (f in csv_files) {
  fname <- basename(f)
  tables[[fname]] <- fread(f, colClasses = "character")
}

# Identify Core Tables
client_file <- grep("^Client\\.csv$", names(tables), value = TRUE, ignore.case = TRUE)[1]
enroll_file <- grep("^Enrollment\\.csv$", names(tables), value = TRUE, ignore.case = TRUE)[1]

if (is.na(client_file) || is.na(enroll_file)) {
  stop("Input ZIP must contain at least Client.csv and Enrollment.csv!")
}

client_dt <- tables[[client_file]]
enroll_dt <- tables[[enroll_file]]

# Classify Remaining Tables:
# - Static: No EnrollmentID or PersonalID (e.g. Export, Org, Project, CEParticipation, Funder, Inventory)
# - Downstream: Contains EnrollmentID and/or PersonalID (e.g. Assessment, Exit, Disabilities, Services, Event)
static_files     <- c()
downstream_files <- c()

for (fname in names(tables)) {
  if (fname %in% c(client_file, enroll_file)) next
  
  cols <- names(tables[[fname]])
  if ("EnrollmentID" %in% cols || "PersonalID" %in% cols) {
    downstream_files <- c(downstream_files, fname)
  } else {
    static_files <- c(static_files, fname)
  }
}

cat(sprintf("      -> Identified %d Static tables (written once):\n         [%s]\n", 
            length(static_files), paste(static_files, collapse = ", ")))
cat(sprintf("      -> Identified %d Dynamic Relational tables (streamed):\n         [%s]\n", 
            length(downstream_files) + 2, paste(c(client_file, enroll_file, downstream_files), collapse = ", ")))

# Write all Static tables to output directory immediately
for (sf in static_files) {
  fwrite(tables[[sf]], file.path(OUTPUT_DIR, sf), na = "", quote = "auto")
}

# ==============================================================================
# 3. DYNAMIC BATCH ENGINE
# ==============================================================================
append_table <- function(dt, filename, is_first_time) {
  if (!is.null(dt) && nrow(dt) > 0) {
    fwrite(
      dt, 
      file.path(OUTPUT_DIR, filename), 
      na = "", 
      quote = "auto", 
      append = !is_first_time, 
      col.names = is_first_time
    )
  }
}

# Helper to identify Primary Key of a child table
get_pk_col <- function(dt_cols) {
  pk_candidates <- grep("ID$", dt_cols, value = TRUE)
  pk <- setdiff(pk_candidates, c("EnrollmentID", "PersonalID", "HouseholdID", "ProjectID", "UserID", "OrganizationID"))
  if (length(pk) > 0) return(pk[1])
  return(NULL)
}

add_batches <- function(start_b, num_b) {
  end_b <- start_b + num_b - 1
  for (b in start_b:end_b) {
    first_time <- (b == 1)
    
    # 1. Expand Clients
    c_b <- copy(client_dt)
    c_b[, OldID := PersonalID]
    c_b[, PersonalID := sprintf("C_%d_%d", b, 1:.N)]
    
    # 2. Expand Enrollments & Link to Clients
    e_b <- copy(enroll_dt)
    e_b[, OldEID := EnrollmentID]
    e_b[, `:=`(
      EnrollmentID = sprintf("E_%d_%d", b, 1:.N),
      HouseholdID  = sprintf("HH_%d_%d", b, 1:.N)
    )]
    e_b[c_b, on = .(PersonalID = OldID), PersonalID := i.PersonalID]
    
    # 3. Dynamically Expand ALL Downstream Tables (Assessments, CE, Disabilities, Services, etc.)
    for (dfname in downstream_files) {
      d_dt <- tables[[dfname]]
      if (is.null(d_dt) || nrow(d_dt) == 0) next
      
      child_batch <- copy(d_dt)
      pk_col <- get_pk_col(names(child_batch))
      
      # Generate unique sequential PK
      if (!is.null(pk_col)) {
        prefix <- substr(pk_col, 1, 3)
        child_batch[[pk_col]] <- sprintf("%s_%d_%d", prefix, b, 1:nrow(child_batch))
      }
      
      # Re-link Foreign Keys
      if ("EnrollmentID" %in% names(child_batch)) {
        child_batch[e_b, on = .(EnrollmentID = OldEID), `:=`(
          EnrollmentID = i.EnrollmentID,
          PersonalID   = i.PersonalID
        )]
        child_batch <- child_batch[!is.na(EnrollmentID)]
      } else if ("PersonalID" %in% names(child_batch)) {
        child_batch[c_b, on = .(PersonalID = OldID), PersonalID := i.PersonalID]
        child_batch <- child_batch[!is.na(PersonalID)]
      }
      
      append_table(child_batch, dfname, first_time)
    }
    
    # 4. Write Core Tables
    c_b[, OldID := NULL]
    e_b[, OldEID := NULL]
    append_table(c_b, client_file, first_time)
    append_table(e_b, enroll_file, first_time)
  }
}

rezip <- function() {
  if (file.exists(OUTPUT_ZIP)) file.remove(OUTPUT_ZIP)
  files_to_zip <- list.files(OUTPUT_DIR, full.names = TRUE)
  zip::zip(zipfile = OUTPUT_ZIP, files = files_to_zip, mode = "cherry-pick")
  return(file.info(OUTPUT_ZIP)$size / (1024^2))
}

# ==============================================================================
# 4. ADAPTIVE REAL-TIME EXPANSION (STOPS AT ~208 MB)
# ==============================================================================
cat("[2/4] Expanding all tables adaptively with real-time compression monitoring...\n")

total_batches <- 0
current_zip_mb <- 0

# Seed with initial 8 batches
seed_batches <- 8
cat(sprintf("      -> Generating seed chunk (%d batches)...\n", seed_batches))
add_batches(start_b = 1, num_b = seed_batches)
total_batches <- seed_batches
current_zip_mb <- rezip()
cat(sprintf("      -> Initial ZIP size: %.2f MB\n", current_zip_mb))

# Adaptive growth loop
while (current_zip_mb < TARGET_STOP_MB) {
  remaining_mb <- TARGET_STOP_MB - current_zip_mb
  mb_per_batch <- current_zip_mb / total_batches
  
  # Adaptive leap calculation
  chunk_to_add <- max(1, floor(remaining_mb / mb_per_batch))
  chunk_to_add <- min(chunk_to_add, 30)
  
  # Fine-throttle when close to 208MB
  if (current_zip_mb > 175.0) chunk_to_add <- min(chunk_to_add, 5)
  if (current_zip_mb > 198.0) chunk_to_add <- min(chunk_to_add, 1)
  
  cat(sprintf("      -> Adding +%d batches (Total: %d)... ", chunk_to_add, total_batches + chunk_to_add))
  
  add_batches(start_b = total_batches + 1, num_b = chunk_to_add)
  total_batches <- total_batches + chunk_to_add
  
  current_zip_mb <- rezip()
  cat(sprintf("New Size: %.2f MB\n", current_zip_mb))
}

# ==============================================================================
# 5. HARD CEILING ENFORCEMENT (< 220 MB)
# ==============================================================================
cat("[3/4] Verifying hard ceiling...\n")

if (current_zip_mb > HARD_MAX_MB) {
  cat(sprintf("[!] Size (%.2f MB) over hard limit (%.2f MB). Trimming excess...\n", 
              current_zip_mb, HARD_MAX_MB))
  
  shrink_factor <- 208.0 / current_zip_mb
  
  c_cur <- fread(file.path(OUTPUT_DIR, client_file), colClasses = "character")
  keep_clients <- c_cur[1:floor(nrow(c_cur) * shrink_factor)]
  fwrite(keep_clients, file.path(OUTPUT_DIR, client_file), na = "")
  valid_pids <- keep_clients$PersonalID
  rm(c_cur); rm(keep_clients)
  
  e_cur <- fread(file.path(OUTPUT_DIR, enroll_file), colClasses = "character")
  keep_enroll <- e_cur[PersonalID %in% valid_pids]
  fwrite(keep_enroll, file.path(OUTPUT_DIR, enroll_file), na = "")
  valid_eids <- keep_enroll$EnrollmentID
  rm(e_cur); rm(keep_enroll)
  
  # Cascade trim through all downstream tables
  for (dfname in downstream_files) {
    p <- file.path(OUTPUT_DIR, dfname)
    if (file.exists(p)) {
      dt <- fread(p, colClasses = "character")
      if ("EnrollmentID" %in% names(dt)) {
        fwrite(dt[EnrollmentID %in% valid_eids], p, na = "")
      } else if ("PersonalID" %in% names(dt)) {
        fwrite(dt[PersonalID %in% valid_pids], p, na = "")
      }
      rm(dt)
    }
  }
  
  current_zip_mb <- rezip()
}

unlink(unzip_dir, recursive = TRUE)

# ==============================================================================
# 6. FINAL VERIFICATION & FILE MANIFEST
# ==============================================================================
final_files <- list.files(OUTPUT_DIR, pattern = "\\.csv$")

cat("\n======================================================================\n")
cat(sprintf("  UNIVERSAL EXPANSION SUCCESSFUL\n"))
cat(sprintf("  Output ZIP:      %s\n", OUTPUT_ZIP))
cat(sprintf("  Final Size:      %.2f MB (Strictly < 222 MB)\n", current_zip_mb))
cat(sprintf("  Included Tables: %d / %d original CSVs\n", length(final_files), length(csv_files)))
cat(sprintf("  Manifest:        %s\n", paste(final_files, collapse = ", ")))
cat("======================================================================\n")