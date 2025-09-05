# Base image: R + fast binary packages for Ubuntu 24.04
FROM rocker/r2u:24.04

# System build tools for Stan, plus utilities
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential git make pandoc \
 && rm -rf /var/lib/apt/lists/*

# Workdir inside the container
WORKDIR /work

# --- Leverage Docker layer caching for dependencies ---
# Copy only files needed to restore R packages first
COPY analysis/renv.lock analysis/.Rprofile analysis/setup.R analysis/
# Restore R env & CmdStan during build (cached unless lockfile changes)
RUN R -q -e "source('analysis/setup.R')"

# Now copy the rest of the repo (code, Makefile, metadata)
COPY . .

# Default command runs the full analysis
CMD ["make", "analysis"]
