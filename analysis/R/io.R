io_init <- function() {
  dirs <- c("analysis/outputs/figs", "analysis/outputs/tables", "data/processed")
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

io_read_raw <- function(path = "data/raw") {
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    message("No CSVs found in data/raw; returning empty tibble.")
    return(tibble::tibble())
  }
  dplyr::bind_rows(lapply(files, readr::read_csv(show_col_types = FALSE)))
}

io_write_results <- function(summaries, metrics) {
  readr::write_csv(metrics, "analysis/outputs/tables/metrics_stub.csv")
}
