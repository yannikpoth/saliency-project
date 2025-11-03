# docker/Dockerfile
FROM rocker/r2u:24.04
ENV R_BSPM_DISABLE=true
ENV MAKEFLAGS=-j1

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential git make pandoc \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /work
COPY . .

# Run setup and analysis in the SAME R session (critical)
CMD ["bash", "-lc", "Rscript --vanilla -e \"source('analysis/setup.R'); source('analysis/run_analysis.R')\""]
