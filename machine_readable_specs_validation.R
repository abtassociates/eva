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
    fsubset(CSV == csv_name & Name %in% names(dt))
  
  unique_id_colname <- unique_id_lookup[CSV == csv_name]$UniqueID
  
  
  # Check 1: Column Mispelled/Misordered/Missing/Extra ------------------------------
  # Reports on missing/extra (misspelled column names will count as both) and otherwise misordered
  imported_cols <- colnames(dt)
  expected_cols <- csv_validation_info$Name
  
  misordered <- data.table(
    Name = {
      ranks <- match(imported_cols, expected_cols)
      ranks_filled <- fifelse(is.na(ranks), 0L, ranks)
      
      
      # 1. Left-to-Right check: Flags items that jumped backwards
      # (An item is bad if it's smaller than the maximum rank seen so far)
      fwd_mismatch <- ranks_filled < cummax(ranks_filled) & !is.na(ranks)
      
      # 2. Right-to-Left check: Flags items that jumped forwards (like your '38')
      # (An item is bad if it's larger than the minimum rank seen from the right)
      rev_mismatch <- ranks_filled > rev(cummin(rev(ranks_filled))) & !is.na(ranks)
      
      # 3. The true "out of order" items are the minority that disrupt the sequence.
      # We return whichever direction flagged the FEWEST items.
      if (fsum(fwd_mismatch) < fsum(rev_mismatch)) {
        imported_cols[fwd_mismatch]
      } else {
        imported_cols[rev_mismatch]
      }
    },
    incorrect_column_detail = "misordered"
  )
  
  missing_or_extra <- data.table(
    Name = c(
      setdiff(imported_cols, expected_cols), 
      setdiff(imported_cols, imported_cols)
    ),
    incorrect_column_detail = c("extra", "missing")
  )
  
  incorrect_columns <- rbind(misordered, missing_or_extra)
  
  # All other checks are dependent on the actual data, so if there is no data, skip
  if(fnrow(dt) == 0)
    next
  
  # Check 6: Incorrect Data Type ----------------
  # includes date and non-date
  # Just focus on the columns whose expected RClass differs from the fread-imported one 
  # When fread imports with colClasses specified, if any value cannot be coerced 
  # to the specified RClass, it will convert the column to character
  # So we can check that discrepancy to first just focus on the columns that differ
  # Then we can identify the particular rows
  incorrect_data_types <- csv_validation_info %>%
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
  
  incorrect_data_types <- if(fnrow(incorrect_data_types) > 0) {
    rbindlist(lapply(seq_row(incorrect_data_types), function(i) {
      colName <- incorrect_data_types$Name[i]
      expected_rclass <- incorrect_data_types$Expected_RClass[i]
      
      raw_vals <- dt[[colName]]
      coerced  <- methods::as(raw_vals, expected_class) # or type-specific: as.numeric(), etc.
      invalid <- is.na(coerced) & !is.na(raw_vals)
      
      dt %>%
        fsubset(invalid == TRUE) %>%
        fmutate(Name = colName)
    }))
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
  ), fill=TRUE, idcol='check_type')
}

specs_validation_issues <- rbindlist(
    Filter(fnrow, csv_issues),
    fill=TRUE,
    idcol="CSV"
  ) %>%
  join(
    reporting_info %>% fselect(CSV, check_type, Issue, Type = check_priority, Guidance, `Detail Text`, `Key Fields`, AnchorID),
    on = c("CSV","check_type"),
    drop.dup.cols = "x"
  ) %>%
  fmutate(
    key_template = gsub("([A-Za-z0-9_.]+)", "\\1 {\\1}", `Key Fields`),
    detail_template = stringi::stri_replace_all_fixed(`Detail Text`, "{Key Field Info}", key_template)
  )

specs_validation_issues[, Detail := as.character(glue_data(.SD, detail_template[1L])), by = detail_template]

specs_validation_issues <- specs_validation_issues %>%
  fselect(c("CSV", "Name", issue_display_cols, "AnchorID")) |>
  rbind(
    run_templatable_validations("file structure", data_env = environment()),
    fill = TRUE
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