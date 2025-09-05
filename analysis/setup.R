# analysis/setup.R — robust, non-interactive setup

# 0) Force a valid CRAN mirror for both base R and renv
# Why: Non-interactive sessions sometimes have repos set to "@CRAN@" (unset).
# We set CRAN explicitly and also tell renv to use it via an env override.
set_cran_repo <- function() {
  cran <- getOption("repos")
  # Normalize to a named vector with CRAN url
  if (is.null(cran) || isTRUE(is.na(cran["CRAN"])) || identical(cran["CRAN"], "@CRAN@") || identical(cran["CRAN"], "")) {
    cran <- c(CRAN = "https://cloud.r-project.org")
  } else if (is.null(names(cran)) || is.na(match("CRAN", names(cran)))) {
    # If it's an unnamed vector, give it the CRAN name
    cran <- c(CRAN = cran[[1]])
  }
  options(repos = cran)
  # Tell renv to always use this mirror when resolving packages
  Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = unname(cran["CRAN"]))
  invisible(cran)
}
cran <- set_cran_repo()

# 1) Ensure renv is present (install from the CRAN we just set)
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = c(CRAN = unname(cran["CRAN"])))
}
renv::consent(provided = TRUE)

# 2) Create/refresh the lockfile on first run, then restore packages
# Why: renv::restore() installs exactly what lockfile specifies; consistent for everyone.
if (!file.exists("renv.lock")) renv::snapshot(prompt = FALSE)
renv::restore(prompt = FALSE)

# 3) Ensure cmdstanr from Stan repo + CRAN
# Why: cmdstanr is hosted on mc-stan; provide BOTH repos explicitly to avoid "@CRAN@" issues.
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", unname(cran["CRAN"])))
}

# 4) Toolchain and CmdStan binaries (first run compiles toolchain; macOS needs Xcode CLT)
cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
if (!cmdstanr::cmdstan_version(TRUE)) cmdstanr::install_cmdstan(quiet = TRUE)

message("Setup complete.")
