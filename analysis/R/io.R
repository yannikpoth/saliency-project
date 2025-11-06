io_init <- function() {
  dirs <- c("analysis/outputs/figs", "analysis/outputs/tables", "data/processed")
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

io_read_raw <- function(path = "data/raw") {
  # List all CSVs
  all_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

  if (length(all_files) == 0) {
    message("No CSVs found in data/raw; returning empty lists.")
    return(list(task = tibble::tibble(), questionnaire = tibble::tibble()))
  }

  # Separate task and questionnaire files based on filename pattern
  task_files <- grep("_task_data\\.csv$", all_files, value = TRUE)
  quest_files <- grep("_questionnaire_data\\.csv$", all_files, value = TRUE)

  # Read task data (no participant_id column, so no col_types for it; add from filename)
  read_task <- function(f) {
    participant_id <- sub("^(\\d+)_task_data\\.csv$", "\\1", basename(f))  # Extract ID from filename (e.g., "01", "10", "44")
    df <- readr::read_csv(f, show_col_types = FALSE)
    df$participant_id <- as.character(participant_id)  # Add as character column
    df
  }
  task_data <- dplyr::bind_rows(lapply(task_files, read_task))

  # Read questionnaire data (has participant_id; force to character)
  quest_data <- dplyr::bind_rows(lapply(quest_files, readr::read_csv,
                                        show_col_types = FALSE,
                                        col_types = readr::cols(participant_id = readr::col_character())))

  # Return as a named list for easy access in the pipeline
  list(task = task_data, questionnaire = quest_data)
}

# Placeholder function for writing results later
io_write_results <- function(summaries, metrics) {
  readr::write_csv(metrics, "analysis/outputs/tables/metrics_stub.csv")
}
