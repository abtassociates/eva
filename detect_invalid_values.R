# FY26 vlaidation specs
validation_specs_bk <- "/media/sdrive/projects/CE_Data_Toolkit/FY26 HMIS-CSV-Machine-Readable-Specifications.xlsx"

# Read in Variable-List xwalk
# We will handle the nuanced ones later
var_list_xwalk <- read_excel(validation_specs_bk, sheet = "CSV Lists Data Dict FY2026") %>%
  fmutate(
    Type = gsub("\u00A0", "", Type),
    List = str_trim(ifelse(List == "1.1000000000000001", "1.1", List))
  ) %>%
  fsubset(
    !is.na(List),
    CSV, Name, Type, List, Null
  ) %>%
  funique() %>% #Dedup since some variables can appear multiple times, one for each Data Element, e.g. LOSUnderThreshold
  qDT()

# Read in CSV Lists
validation_lists <- read_excel(validation_specs_bk, sheet = "CSV Lists FY2026") %>%
  fmutate(
    Value = gsub("\u00A0", "", Value),
    List = ifelse(
      List == "1.1000000000000001", "1.1", 
      ifelse(
        List == "4.1399999999999997", "4.14",
        List
      )
    )
  )


# Convert to a named list, where the names are the unique values of `List` and the values are the vector of `Value`s.
result <- split(validation_lists$Value, validation_lists$List)


# Now loop through each variable in each CSV and make sure it has the right Type and List
validation_summary <- list()  # store flagged rows for each dataset

for(csv_name in intersect(unique(cols_and_data_types$File), var_list_xwalk$CSV)) {
  # 1. Get dataset
  dt <- get(csv_name) %>% qDT()
  
  # 2. Columns for this CSV
  cols <- var_list_xwalk[CSV == csv_name, Name]
  
  # 3. Build allowed values for each column
  # Handle nuanced columns separately
  allowed_values <- setNames(
    lapply(var_list_xwalk[CSV == csv_name, List], function(l) result[[as.character(l)]]),
    cols
  )

  # Allow Nulls, if
  for(col in cols) {
    
    if(csv_name == "Services" & col == "TypeProvided") {
      # For RecordType · 141 – list P1.2 · 142 – list R14.2 · 143 – list W1.2 · 144 – list V2.2 · 151 – list W2.2 · 152 – list V3.3 · 161 – list P2.2 · 200 – list 4.14 · 210 – list V8.2 300 – list C2.2
      
    } else if(csv_name == "Services" & col == "TypeProvided") {
      
    } else {
      nulls_allowed <- var_list_xwalk[CSV == csv_name & Name == col, Null]
      if(fcoalesce(nulls_allowed, "N") == "Y") {
        allowed_values[[col]] <- c(allowed_values[[col]], NA)
      }
    }
  }

  invalid_counts <- sapply(cols, function(col) {
    if (col %in% names(dt)) {
      sum(!(as.character(dt[[col]]) %in% allowed_values[[col]]), na.rm = TRUE)
    } else {
      0  # Column doesn't exist in data
    }
  })
  
  csv_summary <- data.table(
    CSV = csv_name,
    name = names(invalid_counts),
    n = as.numeric(invalid_counts)
  )
  
  validation_summary[[csv_name]] <- csv_summary
  # 
  # # 4. Check invalids
  # invalid_flags <- lapply(cols, function(col) {
  #   if (col %in% names(dt)) 
  #     !(as.character(dt[[col]]) %in% allowed_values[[col]])
  # })
  # setNames(invalid_flags, cols)
  # 
  # 
  # # 5. Combine flags across columns (any invalid)
  # invalid_any <- do.call(pmax.int, c(invalid_flags, list(na.rm = TRUE))) > 0
  # 
  # # 6. Keep flagged rows
  # if (any(invalid_any)) {
  #   flagged_rows <- dt[invalid_any, ..cols[which(sapply(invalid_flags, any, na.rm=TRUE))]]
  #   flagged_list[[csv_name]] <- flagged_rows
  # }
}
final_summary <- rbindlist(validation_summary) %>%
  fsubset(n > 0)
  
