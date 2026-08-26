# ============================================================
# CONSOLIDATED MIRAI TRANSFER BENCHMARK (QS2 & ARROW)
# ============================================================

# ------------------------------------------------------------
# Configuration & Setup
# ------------------------------------------------------------
BENCHMARK_RUNS <- 3

BENCHMARK_DIR <- file.path(tempdir(), paste0("mirai_benchmark_", Sys.getpid()))
dir.create(BENCHMARK_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Memory & Process Helpers
# ------------------------------------------------------------
get_rss_mb <- function(pid = Sys.getpid()) {
  x <- system2("ps", c("-o", "rss=", "-p", pid), stdout = TRUE)
  x <- trimws(x)
  if (!length(x) || !nzchar(x)) return(NA_real_)
  as.numeric(x) / 1024
}

start_rss_monitor <- function(pid = Sys.getpid(), output_file, interval = 0.01) {
  monitor_script <- file.path(BENCHMARK_DIR, "rss_monitor.sh")
  writeLines(
    c(
      "#!/bin/bash",
      "PID=\"$1\"; OUTPUT=\"$2\"; INTERVAL=\"$3\"",
      "echo $$ > \"${OUTPUT}.pid\"",
      "while kill -0 \"$PID\" 2>/dev/null; do",
      "    NOW=$(date +%s%N)",
      "    RSS=$(ps -o rss= -p \"$PID\" 2>/dev/null | tr -d ' ')",
      "    [ -n \"$RSS\" ] && echo \"${NOW}|${RSS}\" >> \"$OUTPUT\"",
      "    sleep \"$INTERVAL\"",
      "done"
    ),
    monitor_script
  )
  Sys.chmod(monitor_script, mode = "0755")
  
  system2("/bin/bash", c(monitor_script, as.character(pid), output_file, as.character(interval)),
          wait = FALSE, stdout = FALSE, stderr = FALSE)
  
  pid_file <- paste0(output_file, ".pid")
  for (j in seq_len(100)) {
    if (file.exists(pid_file)) break
    Sys.sleep(0.01)
  }
  if (!file.exists(pid_file)) stop("RSS monitor failed to start.")
  
  as.integer(readLines(pid_file, n = 1))
}

stop_rss_monitor <- function(monitor_pid, output_file) {
  if (!is.na(monitor_pid)) {
    print("stopping RSS monitor")
    system2("kill", c("-TERM", as.character(monitor_pid)), stdout = FALSE, stderr = FALSE)
    Sys.sleep(0.02)
  }
}

read_peak_rss_mb <- function(output_file) {
  print("reading peak RSS")
  if (!file.exists(output_file)) return(NA_real_)
  x <- readLines(output_file, warn = FALSE)
  if (!length(x)) return(NA_real_)
  parts <- strsplit(x, "\\|")
  rss_vals <- as.numeric(vapply(parts, `[`, character(1), 2))
  max(rss_vals, na.rm = TRUE) / 1024
}

# ------------------------------------------------------------
# Dependency Helpers
# ------------------------------------------------------------
build_dependencies <- function(session) {
  deps <- mget(
    unique(c(dq_mirai_dependencies, pdde_mirai_dependencies)),
    envir = parent.env(environment())
  )
  deps[["IncomeBenefits"]] <- deps[["IncomeBenefits"]] %>%
    fsubset(DataCollectionStage %in% c(1,3))
  
  deps[["session"]] <- list(
    token = session$token,
    userData = list(
      Project0                 = session$userData$Project0,
      meta_HUDCSV_Export_Date  = session$userData$meta_HUDCSV_Export_Date,
      meta_HUDCSV_Export_Start = session$userData$meta_HUDCSV_Export_Start,
      meta_HUDCSV_Export_End   = session$userData$meta_HUDCSV_Export_End,
      validation               = session$userData$validation %>%
        fselect(c(
          "EnrollmentID",
          "HouseholdID",
          "PersonalID",
          "OrganizationName",
          "ProjectID",
          "ProjectName",
          "ProjectType",
          "EntryDate",
          "MoveInDateAdjust",
          "ExitDate"
        ))
    )
  )
  deps
}

describe_dependencies <- function(deps) {
  data.frame(
    name           = names(deps),
    class          = vapply(deps, function(x) paste(class(x), collapse = ", "), character(1)),
    object_size_mb = vapply(deps, function(x) as.numeric(object.size(x)) / 1024^2, numeric(1)),
    stringsAsFactors = FALSE
  ) |> roworder(-object_size_mb)
}

# ============================================================
# UNIFIED WORKER DISPATCHER (ZERO-COPY IPC + ERROR TRACE)
# ============================================================
dispatch_mirai_worker <- function(prepared) {
  print("dispatching mirai")
  mirai::mirai(
    {
      tryCatch({
        # --- 1. LOAD FROM DISK ---
        r_start <- Sys.time()
        
        log_memory("worker start")
      
        deps <- qs2::qs_read(paths)
        unlink(paths)
        log_memory("after qs_read")
          
        
        r_end <- Sys.time()
        
        # Populate environment with identical object names
        list2env(deps, envir = environment())
        rn(deps)
        log_memory("after list2env")
        
        
        # --- 2. EXECUTE DATA QUALITY & PDDE ---
        w_start <- Sys.time()
        logToConsole(session, "Benchmark: About to run dq_mirai")
        source(here::here("rcode", "05_data_quality.R"), local = TRUE)
        log_memory("after 05_data_quality.R")
        
        logToConsole(session, "Benchmark: About to run pdde_mirai")
        source(here::here("rcode", "06_PDDE_checker.R"), local = TRUE)
        w_end <- Sys.time()
        
        log_memory("after pdde")
        # --- 3. RETURN METRICS ---
        
        res <- list(
          success                  = TRUE,
          dq_main                  = dq_main,
          overlap_details          = overlap_details,
          outstanding_referrals    = outstanding_referrals,
          pdde_main                = pdde_main,
          long_stayers             = long_stayers,
          benchmark_read_seconds   = as.numeric(difftime(r_end, r_start, units = "secs")),
          benchmark_worker_seconds = as.numeric(difftime(w_end, w_start, units = "secs")),
          benchmark_worker_rss_mb  = get_rss()
        )
        
        rm(list = setdiff(ls(all.names = TRUE), "res"))
        
        log_memory("after rm")
        release_worker_memory()
        
        log_memory("after release_memory")
      }, error = function(e) {
        print(e)
        list(
          success   = FALSE,
          error_msg = conditionMessage(e),
          traceback = paste(capture.output(traceback()), collapse = "\n")
        )
      })
    },
    .args = list(paths = prepared$paths)
  )
}

# ============================================================
# GENERIC BENCHMARK ENGINE
# ============================================================

run_benchmark_engine <- function(approach_name, deps, write_fn, n_runs = BENCHMARK_RUNS) {
  results <- vector("list", n_runs)
  dep_size_mb <- as.numeric(object.size(deps)) / 1024^2
  
  for (i in seq_len(n_runs)) {
    gc()
    mem_before <- get_rss_mb()
    start_time <- Sys.time()
    
    monitor_file <- file.path(BENCHMARK_DIR, sprintf("rss_%s_%d.log", approach_name, i))
    monitor_pid  <- start_rss_monitor(output_file = monitor_file)
    
    # 1. Write / Serialize to Disk
    log_memory("before preparing data")
    
    write_start <- Sys.time()
    prepared_data <- write_fn(deps, i)
    
    write_end   <- Sys.time()
    
    # 2. Dispatch Mirai Worker
    log_memory("before mirai()")
    
    m <- dispatch_mirai_worker(prepared_data)
    print(object.size(m) / 1024^2)
    submitted_time <- Sys.time()
    
    # 4. Wait for Worker
    print("collecting results...")
    result <- m[]
    
    log_memory("after collecting mirai results")
    
    gc()
    
    log_memory("after gc()")
    
    gc(full = TRUE)
    
    log_memory("after gc(full=TRUE)")
    
    end_time <- Sys.time()
    
    # 3. Stop host monitoring
    stop_rss_monitor(monitor_pid, monitor_file)
    peak_shiny_rss_mb <- read_peak_rss_mb(monitor_file)
    unlink(c(monitor_file, paste0(monitor_file, ".pid")))
    
    if (mirai::is_error_value(result)) {
      stop(sprintf("[%s - Run %d] Worker failed: %s", approach_name, i, as.character(result)))
    }
    if (is.list(result) && !isTRUE(result$success)) {
      stop(sprintf("[%s - Run %d] Script error: %s\nTraceback:\n%s", 
                   approach_name, i, result$error_msg, result$traceback))
    }
    
    browser()
    # 5. Record Row
    results[[i]] <- data.frame(
      approach                  = approach_name,
      run                       = i,
      dependency_object_size_mb = dep_size_mb,
      memory_before_mb          = mem_before,
      peak_shiny_rss_mb         = peak_shiny_rss_mb,
      rss_increase_mb           = peak_shiny_rss_mb - mem_before,
      write_seconds             = as.numeric(difftime(write_end, write_start, units = "secs")),
      submission_seconds        = as.numeric(difftime(submitted_time, write_end, units = "secs")),
      worker_read_seconds       = result$benchmark_read_seconds,
      processing_seconds        = result$benchmark_worker_seconds,
      total_seconds             = as.numeric(difftime(end_time, start_time, units = "secs")),
      worker_rss_mb             = result$benchmark_worker_rss_mb,
      file_size_mb              = prepared_data$file_size_mb,
      stringsAsFactors          = FALSE
    )
    
    if (!is.null(prepared_data$cleanup_fn)) prepared_data$cleanup_fn()
    rm(m, result)
    gc()
  }
  
  dplyr::bind_rows(results)
}

# ============================================================
# STRATEGIES
# ============================================================

benchmark_qs2 <- function(deps, n_runs = BENCHMARK_RUNS) {
  write_fn <- function(deps, run_idx) {
    print("saving dependencies to qs")
    
    file <- file.path(BENCHMARK_DIR, sprintf("deps_qs2_%d.qs2", run_idx))
    qs2::qs_save(deps, file)
    list(
      paths            = file,
      file_size_mb     = file.info(file)$size / 1024^2,
      cleanup_fn       = function() unlink(file)
    )
  }
  run_benchmark_engine("qs2", deps, write_fn, n_runs)
}

# ============================================================
# MASTER RUNNER
# ============================================================

run_mirai_benchmark <- function(session, n_runs = BENCHMARK_RUNS) {
  cat("\n============================================================\n")
  cat("MIRAI TRANSFER BENCHMARK (QS2 & ARROW)\n")
  cat("============================================================\n")
  
  cat("\nBuilding dependencies...\n")
  deps <- build_dependencies(session)
  dep_summary <- describe_dependencies(deps)
  print(dep_summary)
  
  cat(sprintf("\nRunning benchmarks (%d runs each)...\n", n_runs))
  qs2_res   <- benchmark_qs2(deps, n_runs)
  # arrow_res <- benchmark_arrow(deps, n_runs)
  
  results <- dplyr::bind_rows(qs2_res)
  
  summary <- results |>
    dplyr::group_by(approach) |>
    dplyr::summarise(
      runs                    = dplyr::n(),
      dependency_size_mb      = dplyr::first(dependency_object_size_mb),
      mean_file_size_mb       = mean(file_size_mb, na.rm = TRUE),
      mean_memory_before_mb   = mean(memory_before_mb, na.rm = TRUE),
      mean_peak_shiny_rss_mb  = mean(peak_shiny_rss_mb, na.rm = TRUE),
      mean_rss_increase_mb    = mean(rss_increase_mb, na.rm = TRUE),
      mean_write_seconds      = mean(write_seconds, na.rm = TRUE),
      mean_submission_seconds = mean(submission_seconds, na.rm = TRUE),
      mean_read_seconds       = mean(worker_read_seconds, na.rm = TRUE),
      mean_processing_seconds = mean(processing_seconds, na.rm = TRUE),
      mean_total_seconds      = mean(total_seconds, na.rm = TRUE),
      mean_worker_rss_mb      = mean(worker_rss_mb, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nRAW RESULTS:\n"); print(results)
  cat("\nSUMMARY:\n");     print(summary)
  
  list(dependencies = deps, dependency_summary = dep_summary, results = results, summary = summary)
}