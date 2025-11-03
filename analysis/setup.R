# 0) remember & switch working dir safely
.opwd <- getwd()
if (basename(.opwd) != "analysis" && dir.exists("analysis")) setwd("analysis")
on.exit(setwd(.opwd), add = TRUE)

# 1) pure-CRAN installs, no system package manager
Sys.setenv(R_BSPM_DISABLE = "true")

# 2) renv: no global cache (avoid broken symlink warnings in containers)
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", type = "source")
library(renv)
renv::consent(provided = TRUE)
renv::settings$use.cache(FALSE, persist = TRUE)

has_lock <- file.exists("renv.lock")

if (has_lock) {
  renv::restore(prompt = FALSE)
} else {
  renv::init(bare = TRUE)

  base_pkgs <- c("tibble", "dplyr", "readr")
  to_install <- base_pkgs[!base_pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install)

  # cmdstanr (Stan interface)
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    install.packages("cmdstanr",
      repos = c("https://mc-stan.org/r-packages/", getOption("repos"))
    )
  }
}

# 3) Toolchain + CmdStan (limit to 1 core; set MAKEFLAGS to be safe)
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  Sys.setenv(MAKEFLAGS = "-j1")  # avoid OOM during build
  cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
  ver <- tryCatch(cmdstanr::cmdstan_version(), error = function(e) NA)
  if (is.na(ver)) {
    cmdstanr::install_cmdstan(quiet = TRUE, cores = 1)
  }
}

# 4) Lock current state (creates/updates renv.lock)
renv::snapshot(prompt = FALSE)

message("Setup complete. renv is active; lockfile present: ", file.exists("renv.lock"))
