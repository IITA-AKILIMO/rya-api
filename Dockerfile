# Base image https://hub.docker.com/u/rocker/
# FROM rocker/r-base:latest
FROM iita/akilimo-base:latest

## create directories
RUN mkdir -p /images


## copy files
COPY install_packages.R install_packages.R
COPY server.R server.R
COPY backend.R backend.R

## install R-packages
RUN Rscript install_packages.R

ENTRYPOINT ["Rscript","server.R"]