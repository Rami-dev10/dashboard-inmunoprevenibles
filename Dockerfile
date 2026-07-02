# ============================================================
# DOCKERFILE PARA DASHBOARD INMUNOPREVENIBLES - RENDER
# OIIS - Oficina de Inteligencia e Información Sanitaria
# VERSIÓN SIMPLIFICADA Y PROBADA
# ============================================================

# Usar la imagen oficial de R con todos los paquetes del sistema preinstalados
FROM rocker/r-ver:4.3.1

# Instalar dependencias del sistema necesarias (SOLO LAS ESENCIALES)
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Crear directorio de trabajo
WORKDIR /app

# Copiar archivos de la aplicación
COPY . .

# Instalar paquetes de R necesarios
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'plotly', 'leaflet', 'DT', 'dplyr', 'tidyr', 'scales', 'lubridate', 'ggplot2'), repos='https://cloud.r-project.org/')"

# Exponer el puerto
EXPOSE 3838

# Comando para ejecutar Shiny
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]