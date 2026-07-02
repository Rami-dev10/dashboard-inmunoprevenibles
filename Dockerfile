FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# 🔥 FORZAR INSTALACIÓN SIN DEPENDENCIAS ROTAS
RUN R -e "install.packages('leaflet', repos='https://cloud.r-project.org')"

RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
  install.packages(c( \
    'shiny', \
    'shinydashboard', \
    'shinyWidgets', \
    'plotly', \
    'leaflet', \
    'DT', \
    'dplyr', \
    'tidyr', \
    'scales', \
    'lubridate' \
  ), dependencies = TRUE)"

COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]