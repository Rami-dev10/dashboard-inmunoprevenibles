# ============================================================
# DOCKERFILE PARA DASHBOARD INMUNOPREVENIBLES - RENDER
# OIIS - Oficina de Inteligencia e Información Sanitaria
# ============================================================

# Usar la imagen oficial de R
FROM r-base:latest

# Instalar dependencias del sistema necesarias para paquetes de R
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Crear directorio de trabajo
WORKDIR /app

# Copiar archivos de dependencias primero (mejor para caching)
COPY DESCRIPTION ./

# Instalar paquetes necesarios de CRAN
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'plotly', 'leaflet', 'DT', 'dplyr', 'tidyr', 'scales', 'lubridate', 'ggplot2'), repos='https://cloud.r-project.org/')"

# Copiar el resto de la aplicación
COPY . .

# Exponer el puerto para Shiny
EXPOSE 3838

# Comando para ejecutar Shiny
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]