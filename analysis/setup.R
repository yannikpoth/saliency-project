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

message("\n✓ Setup complete!")
message("Next steps:")
message("  - Run analysis: Rscript analysis/run_analysis.R")
message("  - Or use Make: make analysis")
