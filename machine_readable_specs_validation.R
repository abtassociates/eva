# These are checks whose rules we get from the public-resources/FY26 HMIS-CSV-Machine-Readable-Specs.xlsx file
# 
# - Check 1: Column Mispelled/Misordered/Missing/Extra
# - Check 2: Incorrect Data Type
# - Check 3: Impermissible Characters
# - Check 4-8 (evaluated in run_templatable_validaitons): 
#     String Length Limit Exceeded
#     Unallowed Null (based on the Null column not being Y)
#     Non-Null Invalid
#       the easy case to check is when there's a list of valid values
#       there are special cases with more nuanced conditions
#     Duplicate UniqueID
#     FSA-Foreign Key checks
# These are all HP, except Affiliaton > ProjectID, Affiliation > ResdProjectID, and CEParticipation > ProjectID, which are errors)

# Loop through all files that actually have validation conditions
csv_issues <- list()
for(csv_name in unique(validation_info$CSV)) {
  print(glue("Checking FSA for {csv_name}"))
  logToConsole(parent.env(environment()), glue("Checking FSA for {csv_name}"))
  
  # ------- Setup ------------------
  dt <- get(csv_name)
  csv_validation_info <- validation_info %>%
    fsubset(CSV == csv_name)
  
  unique_id_colname <- unique_id_lookup[CSV == csv_name]$UniqueID
  
  
  # Check 1: Column Mispelled/Misordered/Missing/Extra ------------------------------
  # Reports on missing/extra (misspelled column names will count as both) and otherwise misordered
  imported_cols <- colnames(dt)
  expected_cols <- csv_validation_info$Name
  compare_columns <- function(imported_cols, expected_cols, max_spell_dist = 2) {
    
    n_imp <- length(imported_cols)
    imp_to_exp <- rep(NA_integer_, n_imp)      # Stores the index of the mapped expected col
    imp_status <- rep(NA_character_, n_imp)    # Stores the highest priority status
    exp_matched <- rep(FALSE, length(expected_cols))
    
    # --- PHASE 1: GREEDY MAPPING ---
    
    # 0. Exact Match (Done first so they don't get stolen)
    for(i in seq_along(imported_cols)) {
      match_idx <- which(imported_cols[i] == expected_cols & !exp_matched)
      if (length(match_idx) > 0) {
        imp_to_exp[i] <- match_idx[1]
        imp_status[i] <- "Correct"
        exp_matched[match_idx[1]] <- TRUE
      }
    }
    
    # 1. PRIORITY 1: Incorrectly Capitalized
    for(i in seq_along(imported_cols)) {
      if (is.na(imp_to_exp[i])) {
        match_idx <- which(tolower(imported_cols[i]) == tolower(expected_cols) & !exp_matched)
        if (length(match_idx) > 0) {
          imp_to_exp[i] <- match_idx[1]
          imp_status[i] <- "Incorrectly Capitalized"
          exp_matched[match_idx[1]] <- TRUE
        }
      }
    }
    
    # 2. PRIORITY 2: Misspelled
    for(i in seq_along(imported_cols)) {
      if (is.na(imp_to_exp[i])) {
        unmatched_exp <- which(!exp_matched)
        if (length(unmatched_exp) > 0) {
          dists <- adist(tolower(imported_cols[i]), tolower(expected_cols[unmatched_exp]))[1, ]
          min_dist <- min(dists)
          if (min_dist <= max_spell_dist) {
            match_idx <- unmatched_exp[which(dists == min_dist)[1]]
            
            imp_to_exp[i] <- match_idx
            imp_status[i] <- "Misspelled"
            exp_matched[match_idx] <- TRUE
          }
        }
      }
    }
    
    # 4. PRIORITY 4: Extra 
    # (Anything still unmapped is an extra column)
    for(i in seq_along(imported_cols)) {
      if (is.na(imp_to_exp[i])) {
        imp_status[i] <- "Extra"
      }
    }
    
    # --- PHASE 2: MINIMAL DISPLACEMENT ORDERING ---
    
    # 3. PRIORITY 3: Misordered
    valid_idx <- which(!is.na(imp_to_exp)) # Only evaluate ordering on columns that actually mapped
    
    if (length(valid_idx) > 0) {
      ranks <- imp_to_exp[valid_idx]
      
      # Your Left-to-Right & Right-to-Left Logic
      fwd_mismatch <- ranks < cummax(ranks)
      rev_mismatch <- ranks > rev(cummin(rev(ranks)))
      
      # Find the subset that disrupts the sequence the least
      if (sum(fwd_mismatch) < sum(rev_mismatch)) {
        bad_relative_idx <- which(fwd_mismatch)
      } else {
        bad_relative_idx <- which(rev_mismatch)
      }
      
      # Apply "Misordered" ONLY if it wasn't already flagged for Cap/Spell
      bad_absolute_idx <- valid_idx[bad_relative_idx]
      for (idx in bad_absolute_idx) {
        if (imp_status[idx] == "Correct") {
          imp_status[idx] <- "Misordered"
        }
      }
    }
    
    # --- PHASE 3: COMPILE RESULTS ---
    
    # Build mapping for expected names
    expected_match <- rep(NA_character_, n_imp)
    valid_match_mask <- !is.na(imp_to_exp)
    expected_match[valid_match_mask] <- expected_cols[imp_to_exp[valid_match_mask]]
    
    results <- data.frame(
      Name = imported_cols,
      incorrect_column_detail  = imp_status,
      stringsAsFactors = FALSE
    )
    
    # 4. PRIORITY 4: Missing
    missing_idx <- which(!exp_matched)
    if (length(missing_idx) > 0) {
      missing_df <- data.frame(
        Name = expected_cols[missing_idx],
        incorrect_column_detail  = "Missing",
        stringsAsFactors = FALSE
      )
      results <- rbind(results, missing_df)
    }
    
    # Filter out the fully correct ones
    flagged_cases <- results[results$incorrect_column_detail != "Correct", ]
    rownames(flagged_cases) <- NULL
    
    return(flagged_cases)
  }
  
  incorrect_columns <- compare_columns(imported_cols, expected_cols, max_spell_dist = 2)
  
  # All other checks are dependent on the actual data, so if there is no data, skip
  if(fnrow(dt) == 0)
    next
  
  csv_validation_info <- csv_validation_info |>
    fsubset(Name %in% names(dt))
  
  # Check 6: Incorrect Data Type ----------------
  # includes date and non-date
  # Just focus on the columns whose expected RClass differs from the fread-imported one 
  # When fread imports with colClasses specified, if any value cannot be coerced 
  # to the specified RClass, it will convert the column to character
  # So we can check that discrepancy to first just focus on the columns that differ
  # Then we can identify the particular rows
  incorrect_data_type_cols <- csv_validation_info %>%
    fmutate(
      RClass = sapply(Name, function(col) class(dt[[col]])[1]),
      Type = fifelse(substr(Type, 1, 1) == "S", "S", Type),
      Expected_RClass = vapply(
        Type,
        function(t) data_type_mapping[[t]][["RClass"]],
        character(1)
      )
    ) %>%
    fsubset(
      Expected_RClass != RClass & 
        Expected_RClass != "character", 
      Name, RClass, Expected_RClass
    )
  
  incorrect_data_types <- if(fnrow(incorrect_data_type_cols) > 0) {
    rbindlist(lapply(seq_row(incorrect_data_type_cols), function(i) {
      colName <- incorrect_data_type_cols$Name[i]

      expected_rclass <- incorrect_data_type_cols$Expected_RClass[i]
      raw_vals <- dt[[colName]]
      coerced <- NA
      if(canCoerce(raw_vals, expected_rclass))
        coerced  <- methods::as(raw_vals, expected_rclass) # or type-specific: as.numeric(), etc.
      
      invalid <- is.na(coerced) & !is.na(raw_vals)
      
      dt %>%
        fsubset(invalid == TRUE) %>%
        fmutate(Name = colName)
    })) |>
      join(incorrect_data_type_cols, on="Name")
  } else data.table()
  
  
  # Check 7: Impermissible Characters (Brackets) ---------------
  char_cols <- which(sapply(dt, is.character))
  if (length(char_cols) == 0) next
  
  m_mat <- as.matrix(dt[, ..char_cols, drop = FALSE])  # Convert relevant columns to matrix
  
  impermissible_characters <- if (any(grepl(bracket_regex, m_mat, perl=TRUE), na.rm=TRUE)) {
    data.table(temp = TRUE) 
  } else data.table()
  
  # Compile the standard validation files
  csv_issues[[csv_name]] <- rbindlist(list(
    "Column Mispelled/Misordered/Missing/Extra" = incorrect_columns,
    "Incorrect Data Type" = incorrect_data_types,
    "Impermissible Characters" = impermissible_characters
  ), fill=TRUE, idcol='issue_type')
}

specs_validation_issues <- rbindlist(
    Filter(fnrow, csv_issues),
    fill=TRUE,
    idcol="CSV"
  ) %>%
  join(
    reporting_info %>% fselect(CSV, issue_type, Issue, Priority, Guidance, `Detail Text`, `Key Fields`, AnchorID),
    on = c("CSV","issue_type"),
    drop.dup.cols = "x"
  ) %>%
  fmutate(
    detail_template = fifelse(is.na(`Key Fields`), stringi::stri_replace_all_fixed(`Detail Text`, "Key Info: {Key Field Info}", ""), `Detail Text`),
    key_template = fifelse(is.na(`Key Fields`), "", stringi::stri_replace_all_regex(`Key Fields`, "([A-Za-z0-9_.]+)", "$1 {$1}")),
    detail_template = stringi::stri_replace_all_fixed(detail_template, "{Key Field Info}", key_template)
  )

specs_validation_issues[, Detail := as.character(glue_data(.SD, detail_template[1L])), by = detail_template]

specs_validation_issues <- specs_validation_issues %>%
  fselect(c("CSV", "Column" = "Name", issue_display_cols, "AnchorID")) |>
  rbind(
    run_templatable_validations("file structure", data_env = environment()),
    fill = TRUE
  ) |>
  frename(
    "EnrollmentID or ProjectID" = AnchorID, 
    "ID Value" = AnchorValue
  )

rm(list = files_to_ignore)

rm(
  incorrect_columns, 
  # exceeds_dt, 
  # unallowed_null, 
  # non_null_invalid, 
  # duplicate_uniqueid, 
  incorrect_data_types, 
  # foreign_key_issues,
  # null_unless_issues,
  impermissible_characters,
  csv_issues
)