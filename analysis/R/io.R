io_init <- function() {
  #####
  # Initialize directory structure for analysis outputs
  #
  # Creates necessary directories for figures, tables, and processed data
  # if they don't already exist.
  #
  # Parameters
  # ----
  # None
  #
  # Returns
  # ----
  # NULL (invisible)
  #     Creates directories as side effect
  #####
  dirs <- c("analysis/outputs/figs", "analysis/outputs/tables", "data/processed")
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

io_read_raw <- function(path = "data/raw") {
  #####
  # Read raw task and questionnaire data from CSV files
  #
  # Reads all task and questionnaire CSV files from the specified directory,
  # extracts participant IDs from filenames, and combines them into tibbles.
  # Task files follow pattern: [ID]_task_data.csv
  # Questionnaire files follow pattern: [ID]_questionnaire_data.csv
  #
  # Parameters
  # ----
  # path : character
  #     Directory path containing raw CSV files (default: "data/raw")
  #
  # Returns
  # ----
  # list
  #     Named list with two elements:
  #     - task: Combined tibble of all task data with participant_id added
  #     - questionnaire: Combined tibble of all questionnaire data
  #####
  all_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

  if (length(all_files) == 0) {
    message("No CSVs found in data/raw; returning empty lists.")
    return(list(task = tibble::tibble(), questionnaire = tibble::tibble()))
  }

  # Separate task and questionnaire files based on filename pattern
  task_files <- grep("_task_data\\.csv$", all_files, value = TRUE)
  quest_files <- grep("_questionnaire_data\\.csv$", all_files, value = TRUE)

  # Read task data and add participant_id from filename
  read_task <- function(f) {
    participant_id <- sub("^(\\d+)_task_data\\.csv$", "\\1", basename(f))
    df <- readr::read_csv(f, show_col_types = FALSE)
    # Add participant_id as first column
    df <- dplyr::tibble(participant_id = as.character(participant_id), df)
    df
  }
  task_data <- dplyr::bind_rows(lapply(task_files, read_task))

  # Read questionnaire data with participant_id as character
  quest_data <- dplyr::bind_rows(lapply(quest_files, readr::read_csv,
                                        show_col_types = FALSE,
                                        col_types = readr::cols(participant_id = readr::col_character())))

  list(task = task_data, questionnaire = quest_data)
}

io_write_processed <- function(data_proc, path = "data/processed") {
  #####
  # Write preprocessed data to disk as CSV files
  #
  # Parameters
  # ----
  # data_proc : list
  #     Named list with 'task' and 'questionnaire' tibbles
  # path : character
  #     Directory path to write processed data files
  #####
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  readr::write_csv(data_proc$task, file.path(path, "all_task_data.csv"))
  readr::write_csv(data_proc$questionnaire, file.path(path, "all_questionnaire_data.csv"))

  message("Processed data written to ", path)
}

io_read_processed <- function(path = "data/processed") {
  #####
  # Read preprocessed data from disk
  #
  # Parameters
  # ----
  # path : character
  #     Directory path containing processed data files
  #
  # Returns
  # ----
  # list
  #     Named list with 'task' and 'questionnaire' tibbles
  #####
  task_file <- file.path(path, "all_task_data.csv")
  quest_file <- file.path(path, "all_questionnaire_data.csv")

  if (!file.exists(task_file) || !file.exists(quest_file)) {
    stop("Processed data files not found in ", path)
  }

  list(
    task = readr::read_csv(task_file, show_col_types = FALSE),
    questionnaire = readr::read_csv(quest_file, show_col_types = FALSE)
  )
}

io_get_model_output_dirs <- function(model_name, timestamp = NULL) {
  #####
  # Create and return output directories for a specific model run
  #
  # Creates a structured directory tree:
  # analysis/outputs/
  #   ├── figs/
  #   │   └── [model_name]_[timestamp]/
  #   │       ├── diagnostics/
  #   │       └── ppc/
  #   └── tables/
  #       └── [model_name]_[timestamp]/
  #           ├── diagnostics/
  #           └── ppc/
  #
  # Parameters
  # ----
  # model_name : character
  #     Name of the model (e.g., "model01_baseline")
  # timestamp : character
  #     Timestamp string (e.g., "20231025_120000"). If NULL, generates current time.
  #
  # Returns
  # ----
  # list
  #     Named list with paths:
  #     - figs: Path to figures directory
  #     - tables: Path to tables directory
  #     - figs_diag: Path to figures/diagnostics directory
  #     - tables_diag: Path to tables/diagnostics directory
  #     - figs_ppc: Path to figures/ppc directory
  #     - tables_ppc: Path to tables/ppc directory
  #####

  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }

  # Create base directory name
  run_id <- sprintf("%s_%s", model_name, timestamp)

  # Define paths
  base_figs <- file.path("analysis/outputs/figs", run_id)
  base_tables <- file.path("analysis/outputs/tables", run_id)

  figs_diag <- file.path(base_figs, "diagnostics")
  tables_diag <- file.path(base_tables, "diagnostics")
  figs_ppc <- file.path(base_figs, "ppc")
  tables_ppc <- file.path(base_tables, "ppc")

  # Create directories
  for (d in c(base_figs, base_tables, figs_diag, tables_diag, figs_ppc, tables_ppc)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  list(
    figs = base_figs,
    tables = base_tables,
    figs_diag = figs_diag,
    tables_diag = tables_diag,
    figs_ppc = figs_ppc,
    tables_ppc = tables_ppc,
    run_id = run_id
  )
}
