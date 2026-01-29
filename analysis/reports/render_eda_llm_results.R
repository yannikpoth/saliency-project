render_eda_llm_results <- function(params, output_dir = "analysis/outputs/reports", date_tag = format(Sys.time(), "%Y%m%d")) {
  #####
  # Render the EDA "LLM-ready" results Markdown (tables-only)
  #
  # This is a companion renderer to the HTML EDA report. It renders
  # `analysis/reports/eda_llm_results.Rmd` to a GitHub-flavored Markdown file
  # that includes only numeric/tabular outputs, structured for LLM inspection.
  #
  # Parameters
  # ----
  # params : list
  #     Named list of EDA outputs (same structure as passed into eda_report.Rmd)
  # output_dir : character
  #     Output directory for the rendered report (default: "analysis/outputs/reports")
  # date_tag : character
  #     Date tag used in output filename (default: YYYYMMDD for current time)
  #
  # Returns
  # ----
  # character
  #     Path to the rendered `.md` file.
  #####
  stopifnot("params must be a list" = is.list(params))
  stopifnot("output_dir must be character" = is.character(output_dir) && length(output_dir) == 1)
  stopifnot("date_tag must be character" = is.character(date_tag) && length(date_tag) == 1)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  input_rmd <- "analysis/reports/eda_llm_results.Rmd"
  if (!file.exists(input_rmd)) {
    stop(sprintf("Cannot find input Rmd: %s", input_rmd))
  }

  out_file <- sprintf("eda_llm_results_%s.md", date_tag)

  # IMPORTANT: render() injects a `params` object into the knit environment.
  # If we render inside a function that already has an argument named `params`,
  # rmarkdown can error ("params object already exists..."). Knit in a fresh env.
  knit_env <- new.env(parent = globalenv())

  rmarkdown::render(
    input = input_rmd,
    output_file = out_file,
    output_dir = output_dir,
    params = params,
    envir = knit_env,
    quiet = FALSE
  )

  file.path(output_dir, out_file)
}
