# docker/Dockerfile
# Base on official R image for clean, reproducible setup
FROM rocker/r-ver:4.5.2

# Install system dependencies (for R packages like rstan)
RUN apt-get update && apt-get install -y \
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
pandoc \
make \
g++ \
python3 \
python3-pip \
&& apt-get clean && rm -rf /var/lib/apt/lists/*

# Install renv as root (into system libraries for all users)
RUN R -e "install.packages('renv', version = '1.1.5', repos = 'https://cloud.r-project.org')"

# Install pre-commit for git hooks
RUN pip3 install --no-cache-dir pre-commit

# Set working directory (files will be mounted here at runtime)
WORKDIR /workspace

# TODO: When development is done, uncomment below to bake renv restore into the image
# # Copy files needed for build-time installs (renv.lock, .Rprofile)
# COPY renv.lock .Rprofile ./
#
# # Restore R packages from lockfile
# RUN R -e "renv::restore()" && \
#     R -e "renv::repair()"

# Default command: Start R console for interactive work
CMD ["R"]
