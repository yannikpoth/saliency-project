# setup.R - Environment setup for non-Docker users
# Docker users: This file is not needed; the container handles setup automatically

# Ensure we're in project root
if (!file.exists("renv.lock")) {
  stop("renv.lock not found. Please run this script from the project root directory.")
}

message("Setting up R environment...")

# 1. Restore R packages from lockfile
if (!requireNamespace("renv", quietly = TRUE)) {
  message("Installing renv...")
  install.packages("renv")
}

message("Restoring R packages from renv.lock (this may take a few minutes)...")
renv::restore(prompt = FALSE)
message("✓ R packages restored")

# 2. Check CmdStan installation
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  tryCatch({
    cmdstanr::check_cmdstan_toolchain(fix = FALSE, quiet = TRUE)
    version <- cmdstanr::cmdstan_version()
    message(paste0("✓ CmdStan found (version ", version, ")"))
  }, error = function(e) {
    message("⚠ CmdStan not found or not working properly.")
    message("  Install with: cmdstanr::install_cmdstan(version = '2.37.0')")
    message("  See: https://mc-stan.org/cmdstanr/articles/cmdstanr.html")
  })
} else {
  message("⚠ cmdstanr package not found after restore. Check renv.lock integrity.")
}

message("\n✓ Setup complete!")
message("Next steps:")
message("  - Run analysis: Rscript analysis/run_analysis.R")
message("  - Or use Make: make analysis")
