# These are checks whose rules we get from the public-resources/FY26 HMIS-CSV-Machine-Readable-Specs.xlsx file
# 
# - Check 1: Column Mispelled/Misordered/Missing/Extra
# - Check 2: String Length Limit Exceeded
# - Check 3: Unallowed Null (based on the Null column not being Y)
# - Check 4: Non-Null Invalid
#     the easy case to check is when there's a list of valid values
#     there are special cases with more nuanced conditions
# - Check 5: Duplicate UniqueID
# - Check 6: Incorrect Data Type
# - Check 7: Impermissible Characters
# - Check 8: FSA-Foreign Key checks
#    - These are all HP, except Affiliaton > ProjectID, Affiliation > ResdProjectID, and CEParticipation > ProjectID, which are errors)
#
# Assumes the following objects are already available from load_specs.R:
#   - validation_info_by_csv
#   - null_unless_rules (with pre-parsed rule, pre-computed issueid)
#   - foreign_key_checks (with pre-computed on, issueid)
#   - csv_join_prerequisites
#
# Also including a reusable null_unless function


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
    incorrect_column_detail = "Misordered"
  )
  
  missing_or_extra <- data.table(
    Name = c(
      setdiff(imported_cols, expected_cols), 
      setdiff(imported_cols, imported_cols)
    ),
    incorrect_column_detail = c("Extra", "Missing")
  )
  
  incorrect_columns <- rbind(misordered, missing_or_extra) %>%
    fmutate(
      CSV = csv_name,
      check_type = "Column Mispelled/Misordered/Missing/Extra"
    ) %>%
    join(
      reporting_info %>% fsubset(Source == "file structure"),
      on=c("CSV", "check_type"),
      drop.dup.cols = "y"
    ) %>%
    fselect(-CSV, -check_type)
  
  # All other checks are dependent on the actual data, so if there is no data, skip
  if(fnrow(dt) == 0)
    next
  
  # Enrich dt with any prerequisite joins
  dt <- join_prereqs(dt, csv_name, envir = environment())
  
  # Check 2: String Length Limit Exceeded -------------------
  str_rules <- str_len_limit_rules %>%
    fsubset(CSV == csv_name & Name %in% names(dt))
  
  exceeds_dt <- if (fnrow(str_rules) > 0) {
    rbindlist(lapply(seq_row(str_rules), function(i) {
      rule        <- str_rules[i]
      invalid = vlengths(dt[[rule$Name]]) > rule$str_len_limit
      
      dt %>%
        fsubset(invalid == TRUE) %>%
        fmutate(Name = rule$Name, str_len_limit = rule$str_len_limit)
    }))
  } else data.table()
  
  # Check 3: Unallowed Null ----------------------
  csv_unallowed_null_info <- unallowed_null_info %>%
    fsubset(CSV == csv_name & Name %in% names(dt))
  
  unallowed_null <- if (fnrow(csv_unallowed_null_info) > 0) {
    rbindlist(lapply(csv_unallowed_null_info$Name, function(col_name) {
      invalid <- is.na(dt[[col_name]])
      
      dt %>%
        fsubset(invalid == TRUE) %>%
        fmutate(Name = col_name)
    }))
  } else data.table()

  # Check 4: Non-Null Invalid --------------
  csv_non_null_invalid_info <- non_null_invalid_info %>% 
    fsubset(CSV == csv_name & Name %in% names(dt))
  non_null_invalid <- if (fnrow(csv_non_null_invalid_info) > 0) {
    rbindlist(lapply(csv_validation_info$Name, function(col_name) {
      valid_vals <- csv_validation_info[Name == col_name, .(valid_values)]
      if(is.null(unlist(valid_vals))) return(data.table())
      
      invalid <- !dt[[col_name]] %in% unlist(valid_vals) & !is.na(dt[[col_name]])

      dt %>%
        fsubset(invalid == TRUE) %>%
        fmutate(Name = col_name, invalid_val = get(col_name))
    }))
  } else data.table()
  
  
  # Check 5: Duplicate UniqueID -------------------
  invalid = fduplicated(dt[[unique_id_colname]])
  duplicate_uniqueid <- dt %>%
    fsubset(invalid == TRUE) %>%
    fmutate(Name = unique_id_colname)
  
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
    ) # %>%
    # fmutate(across, safe_as)
  
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
    data.table(
      File = file,
      Detail = str_squish("Found one or more brackets in your HMIS CSV Export. 
                See Impermissible Character Detail export for the precise location 
                of these characters."),
      issueid = 134
    ) 
  } else data.table()
  
  # Check 8: Foreign Key Missing ----------------------
  foreign_key_issues <- get_foreign_key_issues(csv_name, "file structure")

  # Compile the standard validation files
  csv_issues[[csv_name]] <- rbindlist(list(
    "Column Mispelled/Misordered/Missing/Extra" = incorrect_columns,
    "String Length Limit Exceeded" = exceeds_dt,
    "Unallowed Null" = unallowed_null,
    "Non-Null Invalid" = non_null_invalid,
    "Duplicate UniqueID" = duplicate_uniqueid,
    "Incorrect Data Type" = incorrect_data_types,
    "Foreign Key Missing" = foreign_key_issues,
    # "Null Unless" = null_unless_issues,
    "Impermissible Characters" = impermissible_characters
  ), fill=TRUE, idcol='check_type')
}

specs_validation_issues <- rbindlist(
    Filter(fnrow, csv_issues),
    fill=TRUE,
    idcol="CSV"
  ) %>%
  add_reporting_info(reporting_source = "file structure")

rm(list = files_to_ignore)

rm(
  incorrect_columns, 
  exceeds_dt, 
  unallowed_null, 
  non_null_invalid, 
  duplicate_uniqueid, 
  incorrect_data_types, 
  foreign_key_issues,
  # null_unless_issues,
  impermissible_characters,
  csv_issues
)