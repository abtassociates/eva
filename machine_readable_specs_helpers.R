# Null Unless checks --------------
clean_rule_for_null_unless <- function(Name, validation_notes) {
  stringi::stri_replace_all_fixed(
    gsub("^Null unless ", "", validation_notes),
    pattern     = names(valid_list_lookup),
    replacement = lookup,
    vectorize_all = FALSE
  ) %>%
    clean_text() %>%
    purrr::map2(Name, ., ~rlang::parse_expr(glue("null_unless({.x}, {.y})")))
}


get_null_unless_issue_records <- function(csv_name, null_unless_rules, envir) {
  print(paste0("getting null unless issue records for csv ", csv_name))
  
  dt <- get(csv_name, envir = envir) %>%
    join_prereqs(csv_name, envir = envir)
  
  csv_null_unless_rules <- null_unless_rules %>%
    fsubset(CSV == csv_name & Name %in% names(dt))
  
  if(fnrow(csv_null_unless_rules) > 0) {
    rbindlist(lapply(seq_row(csv_null_unless_rules), function(i) {
      col <- csv_null_unless_rules[i, Name]
      rule <- csv_null_unless_rules[i, rule][[1]]
      
      invalid = eval(rule, dt)
      
      dt %>%
        fsubset(invalid == TRUE) %>%
        fmutate(Name = col)
    }))
  } else data.table()
}

null_unless <- function(col, cond) {
  (is.na(col) & cond) | (!is.na(col) & !cond)
}


# Foreign Key Checks -------------
get_foreign_key_issues <- function(csv_name, reporting_source) {
  csv_foreign_key_checks <- foreign_key_checks %>%
    fsubset(
      CSV == csv_name & 
        Name %in% names(dt) & 
        Source == reporting_source
    )
  
  foreign_key_issues <- rbindlist(
    lapply(seq_row(csv_foreign_key_checks), function(i) {
      spec_row <- csv_foreign_key_checks[i]
      foreign_tbl <- get(spec_row$tbl_name)
      
      missing <- dt %>%
        join(
          foreign_tbl %>% fselect(spec_row$id_col),
          on   = setNames(spec_row$id_col, spec_row$Name),
          column = TRUE
        ) %>%
        fsubset(.join == "x") %>%
        fmutate(
          Name = spec_row$Name, 
          foreign_tbl = spec_row$tbl_name, 
        )
      
      # See additional_note for Inventory > ProjectID
      if(csv_name == "Inventory") {
        missing <- missing %>%
          funique(cols = "ProjectID")
      }
      return(missing)
    }),
    fill = TRUE
  )
}

# Prep functions ---------------
## merging on helper datasets needed for checks ------------
join_prereqs <- function(dt, csv_name, envir) {
  prereqs <- csv_join_prerequisites[[csv_name]]
  
  if (!is.null(prereqs)) {
    for (prereq in prereqs) {
      foreign_dt <- get(prereq$tbl, envir = envir)
      
      dt <- dt |> 
        join(
          foreign_dt[, c(unname(prereq$on), prereq$cols), with=FALSE], 
          on = prereq$on,
          how = prereq$how %||% "left",
          column = prereq$column
        )
    }
  }
  return(dt)
}

run_templatable_validations <- function(target_source, data_env = parent.frame()) {
  # 1. Get only the rules meant for this specific phase (e.g., "dq", "pdde", "file structure")
  source_rules <- specs_rules |> 
    fsubset(Source == target_source)
  
  # If there are no rules for this source, exit early
  if (fnrow(source_rules) == 0) return(data.table())
  
  # 2. Dynamically loop over ONLY the CSVs that have checks in this phase
  all_issues <- lapply(unique(source_rules$CSV), function(csv_name) {
    print(paste0("getting issues for ", csv_name))
    
    # Safely get the dataset from the environment (skip if user didn't upload it)
    if (!exists(csv_name, envir = data_env)) return(NULL)
    dt <- get(csv_name, envir = data_env)
    
    # Skip if the dataset is empty
    if (fnrow(dt) == 0) return(NULL)
    
    # Apply prerequisite joins (e.g., bringing in Funder for Null Unless checks)
    dt <- join_prereqs(dt, csv_name, envir = data_env)
    
    # Get the specific rules for this CSV
    csv_rules <- source_rules |> fsubset(CSV == csv_name)
    
    # 3. Evaluate each rule against the dataset
    csv_issues <- lapply(seq_row(csv_rules), function(i) {
      
      rule_row <- csv_rules[i, ]
      
      rule <- rule_row$rule_expr[[1]]
                                 
      # Skip if there's no expression (e.g., structural checks you run separately)
      if (is.null(rule)) return(NULL)
      
      # Skip if the column isn't in the dataset
      if(!rule_row$Name %in% names(dt)) return(NULL)
      
      # EVALUATE THE RULE:
      # envir = as.list(dt) means it looks for column names first
      # enclos = data_env means if it needs foreign tables (get('Project')) or lists (valid_values), it looks in the data environment
      if (is.function(rule)) {
        # If it's a function, we just pass the dataset in
        is_invalid <- rule(dt)
      } else {
        # If it's a quote/expression, we use eval
        is_invalid <- eval(rule, envir = as.list(dt), enclos = data_env)
      }
      
      # Subset the dataset to only rows that failed the check
      invalid_dt <- dt |> fsubset(is_invalid == TRUE)
      
      if (fnrow(invalid_dt) > 0) {
        # Attach the metadata so we know exactly what failed
        cols_to_select <- c(
          unlist(strsplit(rule_row$`Key Fields`, ", ", fixed = TRUE)),
          rule_row$AnchorID,
          rule_row$Name
        ) |>
          na_omit()
        
        invalid_dt <- invalid_dt |> 
          fselect(cols_to_select) |>
          fmutate(
            CSV = csv_name,
            Name = rule_row$Name,      # Renaming 'Name' to 'Column' for the final output
            check_type = factor(rule_row$check_type),
            Issue = rule_row$Issue,
            Guidance = rule_row$Guidance,
            Type = rule_row$check_priority,
            foreign_tbl = rule_row$foreign_tbl,
            AnchorID = rule_row$AnchorID,
            str_len_limit = rule_row$str_len_limit,
            key_template = gsub("([A-Za-z0-9_.]+)", "\\1 {\\1}", rule_row$`Key Fields`),
            detail_template = stringi::stri_replace_all_fixed(rule_row$`Detail Text`, "{Key Field Info}", key_template) %>%
              stringi::stri_replace_all_fixed(., "{Value}", paste0("{", Name, "}"))
          )
        
        # Perform glue by-group to actually pipe the necessary values in to the Detail
        invalid_dt[, Detail := as.character(glue_data(.SD, detail_template[1L])), by = detail_template]
        invalid_dt[, AnchorValue := if (is.na(AnchorID[1L]) || AnchorID[1L] == "") NA
                   else as.character(get(AnchorID[1L])),
                   by = AnchorID]
        
        # 4. Final Cleanup: Select ONLY the columns you want in the final output
        # Note: Add the columns you need for your UI here
        invalid_dt <- invalid_dt |>
          fselect(c(issue_display_cols, "CSV", "Name", "AnchorID", "AnchorValue")) |>
          fmutate(
            Issue = factor(Issue),
            Type = factor(Type),
            Guidance = factor(Guidance),
            CSV = factor(CSV),
            Name = factor(Name),
            AnchorID = factor(AnchorID)
          )
        
        return(invalid_dt)
      }
      return(NULL)
    })
    
    # Bind all rule results for this CSV
    return(rbindlist(csv_issues, fill = TRUE))
  })
  
  # 4. Bind all CSV results into one master issues table
  final_issues <- rbindlist(all_issues, fill = TRUE)
  
  return(final_issues)
}























# 
# ## Merging in reporting info ------------
# add_reporting_info <- function(dt, reporting_source) {
#   browser()
#   dt %>%
#     join(column_priorities, on=c("CSV" = "File", "Name" = "Column")) %>%
#     join(
#       reporting_info %>% fsubset(Source == reporting_source),
#       on=c("CSV", "Name", "check_type")
#     ) %>%
#     fmutate(check_type = factor(check_type)) %>%
#     fmutate(
#       key_template = gsub("([A-Za-z0-9_.]+)", "\\1 {\\1}", `Key Fields`),
#       detail_template = stringi::stri_replace_all_fixed(`Detail Text`, "{Key Field Info}", key_template) %>%
#         stringi::stri_replace_all_fixed(., "{Value}", paste0("{", Name, "}"))
#     ) %>%
#     .[, Detail := as.character(glue_data(.SD, detail_template[1L])), by = detail_template] %>%
#     .[, AnchorValue := if (is.na(AnchorID[1L]) || AnchorID[1L] == "") NA_character_
#       else as.character(get(AnchorID[1L])),
#       by = AnchorID] %>%
#     fselect(c(issue_display_cols, "CSV", "Column" = "Name", "AnchorID", "AnchorValue")) %>%
#     fmutate(
#       Issue = factor(Issue),
#       Type = factor(Type),
#       Guidance = factor(Guidance),
#       CSV = factor(CSV),
#       Column = factor(Column),
#       AnchorID = factor(AnchorID)
#     )
# }
# 
# add_reporting_source <- function(dt) {
#   dt %>%
#     join(
#       reporting_info %>% fsubset(Source == reporting_source, CSV, Name, check_type, Source),
#       on=c("CSV", "Name", "check_type")
#     )
# }
# 
# ## Include the special validation rules --------------
# add_special_validation_rules <- function(dt, issue_name) {
#   dt %>%
#     fmutate(Issue = issue_name) %>%
#     join(
#       special_validation_rules_dt %>% fsubset(Issue == issue_name), 
#       on = c("CSV", "Name", "Issue"), 
#       how = "anti"
#     ) %>%
#     rbind(
#       special_validation_rules_dt %>% fsubset(Issue == issue_name), 
#       fill = TRUE
#     )
# }