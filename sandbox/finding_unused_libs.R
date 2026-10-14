old_way <- function() {
  # Get packages used in scripts -------------------------------------------
  library(CodeDepends)
  get_libraries <- function(file_path) {
    script <- readScript(file_path)      # Read the script
    dependencies <- getInputs(script)    # Extract dependencies
    return(
      lapply(
        dependencies,
        function(l) l@libraries
      )
    )# Return libraries
  }
  
  # Get all R script files in the folder
  script_files <- list.files(".", pattern = "\\.R$", full.names = TRUE)
  
  # Extract libraries from all scripts
  all_libraries <- unique(unlist(lapply(script_files, get_libraries)))
  dev_libraries <- c("diffobj", "renv","shinytest2","chromote","praise","testthat")
  all_libraries <- c(all_libraries, dev_libraries)
  
  # Print the unique libraries found
  # print(all_libraries)
  
  
  # Get packages in renv.lock -----------------------------------------------
  library(renv)
  
  # Parse the lockfile
  lock_data <- jsonlite::fromJSON("renv.lock")
  
  # Get all packages in the lockfile
  lockfile_packages <- names(lock_data$Packages)
  
  
  # Compare --------------------------------------------------------------
  all_required_deps <- all_libraries
  
  for(pkg in all_libraries) {
    if(require(pkg, character.only = TRUE)) {
      deps <- tools::package_dependencies(pkg, recursive = TRUE)
      all_required_deps <- c(all_required_deps, unlist(deps))
    }
  }
  
  all_required_deps <- unique(all_required_deps)
  
  # Find packages in lockfile that aren't required
  potentially_unnecessary <- setdiff(lockfile_packages, all_required_deps)
  
  # function to check how often packages are referenced ---------------------
  # Enhanced function to detect function calls from packages
  analyze_package_usage <- function(package_name, app_files) {
    # Initialize results structure
    results <- list(
      direct_calls = list(),
      namespace_calls = list(),
      function_calls = list(),
      required_by = character(0)
    )
    
    # Try to get exported functions (only if package is installed)
    pkg_functions <- character(0)
    if(package_name %in% rownames(installed.packages())) {
      pkg_functions <- getNamespaceExports(package_name)
      
      # Find packages that depend on this one
      pkg_deps <- tools::package_dependencies(reverse = TRUE)
      if(package_name %in% names(pkg_deps)) {
        results$required_by <- pkg_deps[[package_name]]
      }
    }
    
    # Analyze each file
    for(file in app_files) {
      relative_path <- basename(file)
      content <- readLines(file)
      content_string <- paste(content, collapse = "\n")
      
      # 1. Check for library/require calls
      lib_pattern <- paste0("\\b(library|require)\\s*\\(\\s*['\"]?", package_name, "['\"]?\\s*[,)]")
      lib_matches <- gregexpr(lib_pattern, content_string)
      
      if(lib_matches[[1]][1] > 0) {
        # Find line numbers
        for(match_pos in lib_matches[[1]]) {
          # Find which line contains this match
          line_start <- 1
          line_num <- 1
          for(i in 1:length(content)) {
            line_end <- line_start + nchar(content[i])
            if(match_pos >= line_start && match_pos <= line_end) {
              line_num <- i
              break
            }
            line_start <- line_end + 1
          }
          
          results$direct_calls[[length(results$direct_calls) + 1]] <- list(
            file = relative_path,
            line = line_num,
            code = content[line_num]
          )
        }
      }
      
      # 2. Check for namespace calls
      ns_pattern <- paste0("\\b", package_name, ":{2,3}([A-Za-z][A-Za-z0-9._]*)\\s*\\(")
      ns_matches <- gregexpr(ns_pattern, content_string)
      
      if(ns_matches[[1]][1] > 0) {
        # Extract the match and get the function name
        matches <- regmatches(content_string, ns_matches)[[1]]
        for(match in matches) {
          func_name <- gsub(paste0("\\b", package_name, ":{2,3}([A-Za-z][A-Za-z0-9._]*)\\s*\\("), "\\1", match)
          
          # Find line number
          match_pos <- regexpr(match, content_string)[1]
          line_num <- 1
          line_start <- 1
          for(i in 1:length(content)) {
            line_end <- line_start + nchar(content[i])
            if(match_pos >= line_start && match_pos <= line_end) {
              line_num <- i
              break
            }
            line_start <- line_end + 1
          }
          
          results$namespace_calls[[length(results$namespace_calls) + 1]] <- list(
            file = relative_path,
            line = line_num,
            funct = func_name,
            code = content[line_num]
          )
        }
      }
      
      # 3. Check for plain function calls if we have the list of exported functions
      if(length(pkg_functions) > 0) {
        for(func in pkg_functions) {
          # Only check for functions with names at least 4 chars to reduce false positives
          if(nchar(func) >= 4) {
            func_pattern <- paste0("\\b", func, "\\s*\\(")
            func_matches <- gregexpr(func_pattern, content_string)
            
            if(func_matches[[1]][1] > 0) {
              for(match_pos in func_matches[[1]]) {
                # Find line number
                line_num <- 1
                line_start <- 1
                for(i in 1:length(content)) {
                  line_end <- line_start + nchar(content[i])
                  if(match_pos >= line_start && match_pos <= line_end) {
                    line_num <- i
                    break
                  }
                  line_start <- line_end + 1
                }
                
                results$function_calls[[length(results$function_calls) + 1]] <- list(
                  file = relative_path,
                  line = line_num,
                  funct = func,
                  code = content[line_num]
                )
              }
            }
          }
        }
      }
    }
    
    return(results)
  }
  
  analyze_package_usage("AsioHeaders", script_files)
  
  # Sort by usage frequency
  all_results <-enhanced_dependency_analysis(all_libraries, all_required_deps, script_files)
  
  # Display results
  print(all_results)
}

quick_way <- function() {
  # Check the entire Tidyverse ecosystem
  tv_pkgs <- tidyverse::tidyverse_packages(include_self = FALSE)
  
  tv_exports <- lapply(tv_pkgs, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      getNamespaceExports(pkg)
    } else {
      character(0)
    }
  })
  names(tv_exports) <- tv_pkgs
  
  files <- list.files(".", pattern = "\\.(R|r|Rmd|qmd)$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("^\\./renv", files)]
  
  all_tokens <- unique(unlist(lapply(files, function(f) {
    tryCatch({
      tokens <- getParseData(parse(f, keep.source = TRUE))
      tokens$text[tokens$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL")]
    }, error = function(e) character(0))
  })))
  
  results <- lapply(names(tv_exports), function(pkg) {
    used_fns <- intersect(all_tokens, tv_exports[[pkg]])
    data.frame(
      Package = pkg,
      Used = length(used_fns) > 0,
      Functions_Count = length(used_fns),
      Examples_Used = paste(head(used_fns, 5), collapse = ", ")
    )
  })
  
  results_df <- do.call(rbind, results)
  results_df[order(-results_df$Functions), ]
}

long_way <- function() {
  # 1. Grab all packages recorded in renv.lock
  lock_data <- jsonlite::fromJSON("renv.lock")
  all_lock_pkgs <- names(lock_data$Packages)
  
  # Exclude base R packages from the search
  base_pkgs <- c("base", "stats", "utils", "methods", "graphics", "grDevices", "tools")
  audit_pkgs <- setdiff(all_lock_pkgs, base_pkgs)
  
  # 2. Build the lookup dictionary of all exported functions
  pkg_lookup <- list()
  for (pkg in audit_pkgs) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      fns <- getNamespaceExports(pkg)
      for (fn in fns) {
        pkg_lookup[[fn]] <- c(pkg_lookup[[fn]], pkg)
      }
    }
  }
  
  # 3. Find all project files
  files <- list.files(".", pattern = "\\.(R|r|Rmd|qmd)$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("^\\./renv", files)]
  
  # 4. Parse AST and find every direct function hit
  matches <- list()
  direct_pkgs_used <- character(0)
  
  for (f in files) {
    tryCatch({
      parsed <- parse(f, keep.source = TRUE)
      pd <- getParseData(parsed)
      symbols <- pd[pd$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL"), ]
      
      hits <- symbols[symbols$text %in% names(pkg_lookup), ]
      if (nrow(hits) > 0) {
        for (i in seq_len(nrow(hits))) {
          fn_name <- hits$text[i]
          pkgs <- pkg_lookup[[fn_name]]
          direct_pkgs_used <- c(direct_pkgs_used, pkgs)
          
          matches[[length(matches) + 1]] <- data.frame(
            Package  = paste(pkgs, collapse = ", "),
            Function = fn_name,
            File     = basename(f),
            Line     = hits$line1[i],
            stringsAsFactors = FALSE
          )
        }
      }
    }, error = function(e) NULL)
  }
  
  # Full usage report with File and Line numbers for EVERYTHING
  usage_report <- do.call(rbind, matches)
  direct_pkgs_used <- unique(direct_pkgs_used)
  
  # 5. Resolve dependency chains:
  # Which packages aren't called directly, but are required by things that ARE?
  indirect_pkgs_needed <- character(0)
  for (pkg in direct_pkgs_used) {
    deps <- tools::package_dependencies(pkg, recursive = TRUE)
    indirect_pkgs_needed <- c(indirect_pkgs_needed, unlist(deps))
  }
  indirect_pkgs_needed <- unique(indirect_pkgs_needed)
  
  # 6. Categorize the entire lockfile
  all_needed <- unique(c(direct_pkgs_used, indirect_pkgs_needed))
  safe_to_remove <- setdiff(audit_pkgs, all_needed)
}