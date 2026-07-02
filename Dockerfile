FROM rocker/shiny:4.3.1

# Instalar dependencias del sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copiar primero solo los archivos necesarios para instalar paquetes
COPY . .

# Instalar paquetes de R CON MENSAJES DE ERROR VISIBLES
RUN R -e "install.packages(c('shinydashboard', 'shinyWidgets', 'plotly', 'leaflet', 'DT', 'dplyr', 'tidyr', 'scales', 'lubridate', 'ggplot2', 'Rcpp', 'htmltools', 'crosstalk'), repos='https://cloud.r-project.org/', dependencies=TRUE)" || echo "ERROR: Falló la instalación de paquetes R"

# Verificar que shiny está instalado
RUN R -e "library(shiny); print('✅ Shiny instalado correctamente')"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]