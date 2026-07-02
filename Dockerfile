FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c(
  'shiny',
  'dplyr',
  'DT',
  'leaflet',
  'plotly',
  'lubridate',
  'scales',
  'shinydashboard',
  'shinyWidgets',
  'tidyr'
), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/

WORKDIR /srv/shiny-server/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]