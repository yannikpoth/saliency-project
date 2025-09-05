# analysis/setup.R
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::consent(provided = TRUE)
if (!file.exists("renv.lock")) renv::snapshot(prompt = FALSE)
renv::restore(prompt = FALSE)

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
}
cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
if (!cmdstanr::cmdstan_version(TRUE)) cmdstanr::install_cmdstan(quiet = TRUE)
message("Setup complete.")
