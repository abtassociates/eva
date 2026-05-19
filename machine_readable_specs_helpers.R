record_type_list_lookup = c(
  "141" = "P1.2",
  "142" = "R14.2",
  "143" = "W1.2",
  "144" = "V2.2",
  "151" = "W2.2",
  "152" = "V3.3",
  "161" = "P2.2",
  "200" = "4.14",
  "210" = "V8.2",
  "300" = "C2.2"
)

# Null Unless checks --------------
clean_rule_for_null_unless <- function(Name, validation_notes) {
  stringi::stri_replace_all_fixed(
    gsub("^Null unless ", "", validation_notes),
    pattern     = names(valid_list_lookup),
    replacement = valid_list_lookup,
    vectorize_all = FALSE
  ) %>%
    clean_text()
}

null_unless <- function(col, cond) {
  (is.na(col) & cond) | (!is.na(col) & !cond)
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
      
      # print(glue::glue("rule_row = {rule_row[, .(Name, rule_expr)]}"))
      
      # For null-unless, add conditional funder+project type reqs
      # that is, we only run the check if the Funder and ProjectType match the specs
      if(rule_row$issue_type == "Null Unless") {
        nu <- null_unless_additional_reqs |>
          fsubset(CSV == csv_name & Name == rule_row$Name & (!is.na(Funder) | !is.na(ProjectType)))
        
        if(fnrow(nu) > 0) {
          if(!is.na(nu$Funder)) dt <- dt |> fsubset(Funder %in% nu$Funder)
          if(!is.na(nu$ProjectType)) dt <- dt |> fsubset(ProjectType %in% nu$ProjectType)
        }
      }
      
      if(fnrow(dt) == 0) return(NULL)
      
      # EVALUATE THE RULE:
      # envir = as.list(dt) means it looks for column names first
      # enclos = data_env means if it needs foreign tables (get('Project')) or lists (valid_values), it looks in the data environment
      if (is.function(rule)) {
        # If it's a function, we just pass the dataset in
        is_invalid <- rule(dt)
      } else {
        # If it's a quote/expression, we use eval
        is_invalid <- try(eval(rule, envir = as.list(dt), enclos = data_env), silent=TRUE)
        
        # IF any component field is not in the dataset, return
        if(class(is_invalid) == "try-error")
          return(NULL)
      }
      
      # Subset the dataset to only rows that failed the check
      invalid_dt <- dt |> fsubset(is_invalid == TRUE)
      
      if (fnrow(invalid_dt) > 0) {
        # Attach the metadata so we know exactly what failed
        cols_to_select <- unique(c(
          unlist(strsplit(rule_row$`Key Fields`, ", ", fixed = TRUE)),
          rule_row$AnchorID,
          rule_row$Name
        )) |>
          na_omit()
        
        invalid_dt <- invalid_dt |>
          fselect(cols_to_select) |>
          funique() |>
          fmutate(
            CSV = csv_name,
            Name = rule_row$Name,      # Renaming 'Name' to 'Column' for the final output
            List = if(is.na(rule_row$List))
              eval(parse(text = invalid_non_null_dynamic_lists_dt[CSV == csv_name & Name == rule_row$Name]$rule_text), envir = invalid_dt)
            else
              rule_row$List,
            issue_type = factor(rule_row$issue_type),
            Issue = rule_row$Issue,
            Guidance = rule_row$Guidance,
            Priority = rule_row$Priority,
            foreign_tbl = rule_row$foreign_tbl,
            AnchorID = rule_row$AnchorID,
            str_len_limit = rule_row$str_len_limit,
            validation_notes = rule_row$validation_notes,
            readable_validation_notes = rule_row$readable_validation_notes,
            key_template = fifelse(is.na(rule_row$`Key Fields`), "", gsub("([A-Za-z0-9_.]+)", "\\1 {\\1}", rule_row$`Key Fields`)),
            detail_template = stringi::stri_replace_all_fixed(rule_row$`Detail Text`, "{Key Field Info}", key_template) %>%
              stringi::stri_replace_all_fixed(., "{Value}", paste0("{", Name, "}"))
          )
        
        # Perform glue by-group to actually pipe the necessary values in to the Detail
        invalid_dt[, Detail := as.character(glue_data(.SD, detail_template[1L])), by = detail_template]
        invalid_dt[, AnchorValue := if (is.na(AnchorID[1L]) || AnchorID[1L] == "") NA
                   else as.character(get(AnchorID[1L])),
                   by = AnchorID]
        
        # 4. Final Cleanup
        # Note: Add the columns you need for your UI here
        invalid_dt <- invalid_dt |>
          fselect(c(issue_display_cols, "CSV", "Name", "AnchorID", "AnchorValue")) |>
          fmutate(
            Issue = factor(Issue),
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
  final_issues <- rbindlist(all_issues, fill = TRUE) |>
    join(
      column_priorities, 
      on=c("CSV" = "File", "Name" = "Column")
    ) %>%
    fmutate(
      Priority = fcase(
        DataTypeHighPriority == 1, "High Priority",
        Priority == "Warning", "Warning", 
        default = "Error"
      )
    ) |>
    fselect(-DataTypeHighPriority) |>
    frename("Column" = Name)
  
  return(final_issues)
}

# 1. Atomic Humanizer
humanize_atomic_clause <- function(clause, list_map, valid_values_df, override_list_id = NULL) {
  clause <- str_trim(clause)
  
  # ---- Case 0a: RecordType & TypeProvided ----
  if (str_detect(clause, "\\{COMBO_RT_TP\\}")) {
    parts <- str_split(clause, "\\s*\\{COMBO_RT_TP\\}\\s*")[[1]]
    p1_raw <- parts[1] 
    p2_raw <- parts[2] 
    
    rhs_p1 <- str_replace(p1_raw, "^.*?\\bis\\b", "")
    rt_vals <- str_extract_all(rhs_p1, "\\d+")[[1]]
    
    tp_list_ids <- unique(record_type_list_lookup[rt_vals])
    tp_list_ids <- tp_list_ids[!is.na(tp_list_ids)]
    if (length(tp_list_ids) == 0) tp_list_ids <- NULL
    
    p1_human <- humanize_atomic_clause(p1_raw, list_map, valid_values_df)
    p2_human <- humanize_atomic_clause(p2_raw, list_map, valid_values_df, override_list_id = tp_list_ids)
    
    return(paste0("([", p1_human, " AND ", p2_human, "])")) 
  }
  
  # ---- Case 0b: DisabilityType & DisabilityResponse ----
  if (str_detect(clause, "\\{COMBO_DT_DR\\}")) {
    parts <- str_split(clause, "\\s*\\{COMBO_DT_DR\\}\\s*")[[1]]
    p1_raw <- parts[1] 
    p2_raw <- parts[2] 
    
    rhs_p1 <- str_replace(p1_raw, "^.*?\\bis\\b", "")
    dt_vals <- str_extract_all(rhs_p1, "\\d+")[[1]]
    
    # Apply logic: If 10 -> 4.10.2. Any other -> 1.8
    if (length(dt_vals) == 0) {
      dr_list_ids <- "1.8"
    } else {
      dr_list_ids <- unique(ifelse(dt_vals == "10", "4.10.2", "1.8"))
    }
    
    p1_human <- humanize_atomic_clause(p1_raw, list_map, valid_values_df)
    p2_human <- humanize_atomic_clause(p2_raw, list_map, valid_values_df, override_list_id = dr_list_ids)
    
    return(paste0("([", p1_human, " AND ", p2_human, "])"))
  }
  
  # Remove wrapping parens for standard processing
  while(str_detect(clause, "^\\(")) {
    clause <- str_remove(clause, "^\\(") |> str_remove("\\)$") |> str_trim()
  }
  
  var <- str_extract(clause, "^[A-Za-z0-9_.]+")
  if (is.na(var)) return(clause)
  
  # Accept overrides for BOTH dependent variables
  if (!is.null(override_list_id) && var %in% c("TypeProvided", "DisabilityResponse")) {
    list_ids <- override_list_id
  } else {
    list_ids <- list_map[[var]]
  }
  
  if (is.null(list_ids)) list_ids <- character(0)
  
  # ---- Case A: "is one of c[...]" ----
  if (str_detect(clause, "is one of")) {
    if(clause == "Funder  is one of  c[13:19]")
      return("Funder is HUD: HOPWA")
    
    if(clause == "CurrentLivingSituation  is one of  c[215, 206, 207, 225, 204, 205, 302, 329, 314, 332, 336, 335, 410, 435, 421, 411]")
      return("CurrentLivingSituation is non-homeless")
    
    inner_c <- str_extract(clause, "(?<=c\\[).*?(?=\\])")
    
    if (!is.na(inner_c)) {
      vals <- tryCatch({
        as.character(eval(parse(text = paste0("c(", inner_c, ")"))))
      }, error = function(e) {
        str_split(inner_c, ",")[[1]] |> str_trim() |> str_remove_all("'|\"")
      })
      
      translated <- sapply(vals, function(v) {
        match_idx <- valid_values_df$List %in% list_ids & as.character(valid_values_df$Value) == v
        if (any(match_idx)) {
          paste0('"', valid_values_df$Text[match_idx][1], '"')
        } else {
          paste0('"', v, '"')
        }
      })
      
      return(paste0(var, " is one of: ", paste(unique(translated), collapse = " OR ")))
    }
  }
  
  # ---- Case B: Equality "is X" ----
  if (str_detect(clause, "\\bis\\s+(?!one of\\b)")) {
    val_raw <- str_split(clause, "\\bis\\s+")[[1]]
    if (length(val_raw) > 1) {
      val <- str_trim(val_raw[2]) |> str_remove_all("'|\"")
      
      match_idx <- valid_values_df$List %in% list_ids & as.character(valid_values_df$Value) == val
      if (any(match_idx)) {
        labels <- unique(valid_values_df$Text[match_idx])
        return(paste0(var, " is ", paste(paste0('"', labels, '"'), collapse = " OR ")))
      } else if(grepl("not missing", val)) {
        return(paste0(var, " is ", val))
      } else {
        return(paste0(var, " is \"", val, "\""))
      }
    }
  }
  
  # ---- Case C: "is not missing" ----
  if (str_detect(clause, "is not missing")) {
    return(clause)
  }
  
  clause
}

# 2. Main Humanizer Function
humanize_rule <- function(rules, specs_rules_full, valid_values_df) {
  
  list_map_df <- specs_rules_full |> 
    fsubset(!is.na(List) & List != "" & !is.na(Name)) |>
    fselect(Name, List) |>
    funique()
  
  list_map <- split(list_map_df$List, list_map_df$Name)
  
  vapply(as.character(rules), function(rule) {
    if (is.na(rule) || rule == "") return(NA_character_)
    
    rule_work <- rule |>
      str_replace_all("==", " is ") |>
      str_replace_all("%in%", " is one of ") |>
      str_replace_all("!is.na\\(([^)]+)\\)", "\\1 is not missing") |>
      str_replace_all("%between%\\s*list\\(([^,]+),\\s*([^)]+)\\)", " is between \\1 and \\2")
    
    # Protect c() 
    rule_work <- str_replace_all(rule_work, "c\\s*\\(([^)]+)\\)", "c[\\1]")
    
    # ---- PROTECT COMBOS ----
    # RecordType & TypeProvided
    combo_rt_tp <- "(RecordType\\s+is(?:\\s+one of)?\\s+[^&|()]+)\\s*&\\s*(TypeProvided\\s+is(?:\\s+one of)?\\s+[^&|()]+)"
    rule_work <- str_replace_all(rule_work, combo_rt_tp, "\\1 {COMBO_RT_TP} \\2")
    
    # DisabilityType & DisabilityResponse
    combo_dt_dr <- "(DisabilityType\\s+is(?:\\s+one of)?\\s+[^&|()]+)\\s*&\\s*(DisabilityResponse\\s+is(?:\\s+one of)?\\s+[^&|()]+)"
    rule_work <- str_replace_all(rule_work, combo_dt_dr, "\\1 {COMBO_DT_DR} \\2")
    
    # Tag logic ops
    rule_work <- rule_work |>
      str_replace_all("\\s*&\\s*", " {AND} ") |>
      str_replace_all("\\s*\\|\\s*", " {OR} ")
    
    # Split
    parts <- str_split(rule_work, "(?=\\{AND\\}|\\{OR\\}|[()])|(?<=\\{AND\\}|\\{OR\\}|[()])")[[1]]
    
    processed <- sapply(parts, function(p) {
      p_trim <- trimws(p)
      if (p_trim == "{AND}") return(" AND ")
      if (p_trim == "{OR}")  return(" OR ")
      if (p_trim %in% c("(", ")", "")) return(p_trim)
      
      humanize_atomic_clause(p_trim, list_map, valid_values_df)
    })
    
    paste(processed, collapse = "") |> 
      str_replace_all("\\s+", " ") |>
      str_replace_all("\\( ", "(") |>
      str_replace_all(" \\)", ")") |>
      str_trim()
    
  }, character(1), USE.NAMES = FALSE)
}