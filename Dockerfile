FROM --platform=linux/amd64 rocker/shiny:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake \
    gdal-bin \
    libcurl4-openssl-dev \
    libgdal-dev \
    libgit2-dev \
    libudunits2-dev \
    make \
    pandoc \
    git \
    && rm -rf /var/lib/apt/lists/*

# Remove the default example Shiny files first
RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server

# Copy renv metadata first
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install packages directly into the project library rather than
# symlinking to a root-owned renv cache
ENV RENV_CONFIG_CACHE_ENABLED=FALSE

# Restore the exact R package environment
RUN R -s -e "renv::restore()"

# Now copy the application without deleting the restored library
COPY . /srv/shiny-server/

# Allow the Shiny Server worker user to read the app and project library
RUN chown -R shiny:shiny /srv/shiny-server

RUN mkdir -p /home/shiny/.cache/R/renv \
    && chown -R shiny:shiny /home/shiny

EXPOSE 3838

USER shiny

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
