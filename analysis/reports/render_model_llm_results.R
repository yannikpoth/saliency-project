render_llm_markdown_report <- function(input_rmd,
                                       output_stem,
                                       params,
                                       output_dir = "analysis/outputs/reports",
                                       date_tag = format(Sys.time(), "%Y%m%d")) {
  #####
  # Render a parameterized GitHub Markdown report for LLM inspection
  #
  # This helper centralizes the report-rendering pattern used by the EDA and
  # model-side LLM reports. It renders the input Rmd into a dated Markdown file
  # in a fresh knit environment so the injected params object does not clash
  # with a function argument of the same name.
  #
  # Parameters
  # ----
  # input_rmd : character
  #     Path to the input R Markdown template
  # output_stem : character
  #     Filename stem used before the date tag (for example, "eda_llm_results")
  # params : list
  #     Named params list passed into rmarkdown::render()
  # output_dir : character
  #     Output directory for the rendered Markdown file
  # date_tag : character
  #     Date tag appended to the output filename (default: YYYYMMDD)
  #
  # Returns
  # ----
  # character
  #     Path to the rendered `.md` file
  #####
  stopifnot("input_rmd must be character" = is.character(input_rmd) && length(input_rmd) == 1)
  stopifnot("output_stem must be character" = is.character(output_stem) && length(output_stem) == 1)
  stopifnot("params must be a list" = is.list(params))
  stopifnot("output_dir must be character" = is.character(output_dir) && length(output_dir) == 1)
  stopifnot("date_tag must be character" = is.character(date_tag) && length(date_tag) == 1)

  if (!file.exists(input_rmd)) {
    stop(sprintf("Cannot find input Rmd: %s", input_rmd))
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  out_file <- sprintf("%s_%s.md", output_stem, date_tag)
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

render_main_model_llm_results <- function(params,
                                          output_dir = "analysis/outputs/reports",
                                          date_tag = format(Sys.time(), "%Y%m%d")) {
  #####
  # Render the winning-model main-analysis LLM Markdown report
  #
  # Parameters
  # ----
  # params : list
  #     Named params list for analysis/reports/model_llm_results.Rmd
  # output_dir : character
  #     Output directory for the rendered Markdown file
  # date_tag : character
  #     Date tag appended to the output filename (default: YYYYMMDD)
  #
  # Returns
  # ----
  # character
  #     Path to the rendered `.md` file
  #####
  render_llm_markdown_report(
    input_rmd = "analysis/reports/model_llm_results.Rmd",
    output_stem = "model_llm_results",
    params = params,
    output_dir = output_dir,
    date_tag = date_tag
  )
}

render_ppc_llm_results <- function(params,
                                   output_dir = "analysis/outputs/reports",
                                   date_tag = format(Sys.time(), "%Y%m%d")) {
  #####
  # Render the winning-model posterior-predictive-check LLM Markdown report
  #
  # Parameters
  # ----
  # params : list
  #     Named params list for analysis/reports/ppc_llm_results.Rmd
  # output_dir : character
  #     Output directory for the rendered Markdown file
  # date_tag : character
  #     Date tag appended to the output filename (default: YYYYMMDD)
  #
  # Returns
  # ----
  # character
  #     Path to the rendered `.md` file
  #####
  render_llm_markdown_report(
    input_rmd = "analysis/reports/ppc_llm_results.Rmd",
    output_stem = "ppc_llm_results",
    params = params,
    output_dir = output_dir,
    date_tag = date_tag
  )
}
