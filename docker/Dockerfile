# docker/Dockerfile
# Base on official R image for a reproducible analysis environment
FROM rocker/r-ver:4.5.2

# Keep the renv library outside /workspace so bind-mounting the repo
# (docker run -v $(pwd):/workspace ...) does not hide installed packages.
ENV RENV_PATHS_LIBRARY=/renv/library

# Install system dependencies (for R packages like rstan)
RUN apt-get update && apt-get install -y \
ca-certificates \
cmake \
build-essential \
gfortran \
git \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
libfontconfig1-dev \
libharfbuzz-dev \
libfribidi-dev \
libfreetype6-dev \
libpng-dev \
libtiff5-dev \
libjpeg-dev \
openssh-client \
pandoc \
python3 \
python3-pip \
pre-commit \
&& apt-get clean && rm -rf /var/lib/apt/lists/*

# Install renv so the lockfile can be restored during the image build
RUN R -q -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

# Restore R packages from the lockfile at build time.
# Copy only renv metadata first so Docker can cache dependency layers.
WORKDIR /opt/project
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
COPY renv/.gitignore renv/.gitignore
RUN R -q -e "renv::restore(prompt = FALSE)"

# Runtime working directory (repo will typically be bind-mounted here)
WORKDIR /workspace

# Default command: Start R console for interactive work
CMD ["R"]
