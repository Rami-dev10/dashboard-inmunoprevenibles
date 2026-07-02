FROM rocker/shiny:latest

# =========================
# SISTEMA (dependencias R spatial + gráficos)
# =========================
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

# =========================
# PAQUETES R (modo producción)
# =========================
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

# =========================
# APP
# =========================
COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server/

EXPOSE 3838

# =========================
# START APP
# =========================
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]