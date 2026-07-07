# ============================================================
# CONFIGURACIÓN GLOBAL - DASHBOARD INMUNOPREVENIBLES
# OIIS - Oficina de Inteligencia e Información en Salud
# VERSIÓN CORREGIDA
# ============================================================

# ==========================================
# CARGAR LIBRERÍAS
# ==========================================
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(DT)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)

# ==========================================
# CARGAR FUNCIONES AUXILIARES Y TEMA
# ==========================================
source("utils/helpers.R", local = TRUE)
source("utils/tema.R", local = TRUE)

# ==========================================
# FUNCIÓN PARA LIMPIAR NOMBRES DE COLUMNAS
# ==========================================
clean_column_names <- function(df) {
  if (nrow(df) > 0 && any(nchar(names(df)) > 100)) {
    names(df) <- gsub("\\s+", "_", names(df))
    names(df) <- gsub("[^a-zA-Z0-9_]", "", names(df))
    names(df) <- tolower(names(df))
    names(df) <- gsub("_+", "_", names(df))
    names(df) <- gsub("^_|_$", "", names(df))
  } else {
    names(df) <- tolower(names(df))
    names(df) <- gsub("[^a-z0-9_]", "_", names(df))
    names(df) <- gsub("_+", "_", names(df))
    names(df) <- gsub("^_|_$", "", names(df))
  }
  return(df)
}

# ==========================================
# FUNCIÓN PARA LIMPIEZA ESPECÍFICA DE TIA
# ==========================================
clean_tia_column_names <- function(df) {
  mapeo <- list(
    "RED" = "red",
    "REGION" = "region",
    "POBLACION ASEGURADA" = "poblacion_asegurada",
    "DIFTERIA" = "difteria",
    "DIFTERIA_TIA" = "difteria_tia",
    "ESAVI - EVENTO ADVERSO POST VACUNAL" = "esavi",
    "FIEBRE AMARILLA SELVATICA" = "fiebre_amarilla",
    "FIEBRE AMARILLA SELVATICA_TIA" = "fiebre_amarilla_tia",
    "HEPATITIS B" = "hepatitis_b",
    "HEPATITIS B_TIA" = "hepatitis_b_tia",
    "PARALISIS FLACIDA AGUDA" = "paralisis_flacida",
    "PARALISIS FLACIDA AGUDA_TIA" = "paralisis_flacida_tia",
    "PAROTIDITIS SIN COMPLICACIONES" = "parotiditis_sin_complicaciones",
    "PAROTIDITIS SIN COMPLICACIONES_TIA" = "parotiditis_sin_complicaciones_tia",
    "PAROTIDITIS CON COMPLICACIONES" = "parotiditis_complicaciones",
    "PAROTIDITIS CON COMPLICACIONES_TIA" = "parotiditis_complicaciones_tia",
    "RUBEOLA" = "rubeola",
    "RUBEOLA_TIA" = "rubeola_tia",
    "SARAMPION" = "sarampion",
    "SARAMPION_TIA" = "sarampion_tia",
    "TETANOS" = "tetanos",
    "TETANOS_TIA" = "tetanos_tia",
    "TOS FERINA" = "tos_ferina",
    "TOS FERINA_TIA" = "tos_ferina_tia",
    "VARICELA CON OTRAS COMPLICACIONES" = "varicela_con_complicaciones",
    "VARICELA CON OTRAS COMPLICACIONES_TIA" = "varicela_con_complicaciones_tia",
    "VARICELA SIN COMPLICACIONES" = "varicela_sin_complicaciones",
    "VARICELA SIN COMPLICACIONES_TIA" = "varicela_sin_complicaciones_tia"
  )
  
  nombres <- names(df)
  for (i in seq_along(nombres)) {
    nombre_limpio <- gsub("\\s+", " ", nombres[i])
    nombre_limpio <- trimws(nombre_limpio)
    
    if (nombre_limpio %in% names(mapeo)) {
      nombres[i] <- mapeo[[nombre_limpio]]
    } else {
      nombre_limpio <- gsub("[^a-zA-Z0-9_ ]", "", nombre_limpio)
      nombre_limpio <- gsub("\\s+", "_", nombre_limpio)
      nombre_limpio <- tolower(nombre_limpio)
      nombres[i] <- nombre_limpio
    }
  }
  
  names(df) <- nombres
  return(df)
}

# ==========================================
# CARGA DE DATOS PRINCIPALES (CASOS) - CON DETECCIÓN AUTOMÁTICA
# ==========================================
message("📂 Cargando datos de casos...")

detectar_separador <- function(archivo) {
  # Leer primeras líneas para detectar separador
  primera_linea <- readLines(archivo, n = 1, warn = FALSE)
  
  # Contar ocurrencias de posibles separadores
  conteo_coma <- stringr::str_count(primera_linea, ",")
  conteo_puntoycoma <- stringr::str_count(primera_linea, ";")
  conteo_tab <- stringr::str_count(primera_linea, "\t")
  
  # Decidir cuál usar (el que aparezca más veces)
  if(conteo_puntoycoma > conteo_coma && conteo_puntoycoma > conteo_tab) {
    return(";")
  } else if(conteo_coma > conteo_puntoycoma && conteo_coma > conteo_tab) {
    return(",")
  } else if(conteo_tab > 0) {
    return("\t")
  } else {
    return(",")  # Default
  }
}

if(file.exists("data/PRINCIPAL.csv")) {
  # Detectar separador automáticamente
  separador <- detectar_separador("data/PRINCIPAL.csv")
  message("🔍 Separador detectado para PRINCIPAL.csv: '", separador, "'")
  
  casos <- read.csv("data/PRINCIPAL.csv",
                    sep = separador, 
                    encoding = "UTF-8", 
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  message("✅ PRINCIPAL.csv cargado: ", nrow(casos), " filas, ", ncol(casos), " columnas")
} else {
  stop("❌ No se encuentra el archivo data/PRINCIPAL.csv")
}

# ==========================================
# LIMPIEZA DE NOMBRES DE COLUMNAS
# ==========================================
casos <- clean_column_names(casos)
message("📋 Columnas detectadas: ", paste(names(casos), collapse = ", "))

# ==========================================
# CORRECCIÓN DE NOMBRES ESPECÍFICOS
# ==========================================
if("a_o" %in% names(casos)) {
  names(casos)[names(casos) == "a_o"] <- "ano"
  message("✅ Columna 'a_o' renombrada a 'ano'")
}

# Si la columna se llama "año" con tilde
if("año" %in% names(casos)) {
  names(casos)[names(casos) == "año"] <- "ano"
  message("✅ Columna 'año' renombrada a 'ano'")
}

# ==========================================
# TRANSFORMACIÓN DE FECHAS
# ==========================================
if("fecha_de_inicio" %in% names(casos)) {
  # Intentar múltiples formatos de fecha
  fechas_originales <- casos[["fecha_de_inicio"]]
  
  # Primero intentar con dmy
  casos[["fecha_de_inicio"]] <- lubridate::parse_date_time(
    casos[["fecha_de_inicio"]], 
    orders = c("dmy", "mdy", "ymd", "d/m/y", "m/d/y")
  )
  casos[["fecha_de_inicio"]] <- as.Date(casos[["fecha_de_inicio"]])
  
  # Contar cuántas fechas se convirtieron bien
  fechas_validas <- sum(!is.na(casos[["fecha_de_inicio"]]))
  message("📅 Fechas de inicio convertidas: ", fechas_validas, " de ", length(fechas_originales), " válidas")
  
  # Para las que no se pudieron convertir, mostrar ejemplos
  if (fechas_validas < length(fechas_originales)) {
    ejemplos <- head(fechas_originales[is.na(casos[["fecha_de_inicio"]])], 3)
    message("⚠️ Ejemplos de fechas no convertidas: ", paste(ejemplos, collapse = ", "))
  }
} else {
  casos[["fecha_de_inicio"]] <- Sys.Date()
  message("⚠️ No se encontró columna 'fecha_de_inicio'")
}

if("fecha_defuncion" %in% names(casos)) {
  casos[["fecha_defuncion"]] <- lubridate::parse_date_time(
    casos[["fecha_defuncion"]], 
    orders = c("dmy", "mdy", "ymd", "d/m/y", "m/d/y")
  )
  casos[["fecha_defuncion"]] <- as.Date(casos[["fecha_defuncion"]])
  fallecidos_count <- sum(!is.na(casos[["fecha_defuncion"]]))
  message("💀 Fechas de defunción convertidas: ", fallecidos_count, " fallecidos detectados")
} else {
  casos[["fecha_defuncion"]] <- NA
  message("⚠️ No se encontró columna 'fecha_defuncion'")
}

# ==========================================
# CORRECCIÓN DE CAMPOS NUMÉRICOS
# ==========================================
if("sem_epi" %in% names(casos)) {
  casos[["sem_epi"]] <- as.numeric(casos[["sem_epi"]])
} else {
  casos[["sem_epi"]] <- 1
}
casos[["sem_epi"]][is.na(casos[["sem_epi"]])] <- 1

if("mes" %in% names(casos)) {
  casos[["mes"]] <- as.numeric(casos[["mes"]])
} else {
  casos[["mes"]] <- as.numeric(format(casos[["fecha_de_inicio"]], "%m"))
}
casos[["mes"]][is.na(casos[["mes"]])] <- 1

if("ano" %in% names(casos)) {
  casos[["ano"]] <- as.numeric(casos[["ano"]])
} else {
  casos[["ano"]] <- as.numeric(format(casos[["fecha_de_inicio"]], "%Y"))
}
casos[["ano"]][is.na(casos[["ano"]])] <- 2025

if("edad" %in% names(casos)) {
  casos[["edad"]] <- as.numeric(casos[["edad"]])
} else {
  casos[["edad"]] <- 0
}
casos[["edad"]][is.na(casos[["edad"]])] <- 0

# ==========================================
# MOSTRAR VALORES ÚNICOS DE TIPO DE CASO
# ==========================================
if("tipo_de_caso" %in% names(casos)) {
  message("📋 Tipos de caso encontrados: ", paste(unique(casos[["tipo_de_caso"]]), collapse = ", "))
} else {
  message("⚠️ No se encontró columna 'tipo_de_caso'")
}

# ==========================================
# CARGA DE TABLA TIA (RESUMEN) - CON DETECCIÓN AUTOMÁTICA
# ==========================================
message("📂 Cargando tabla TIA...")

# Variable global para la tabla TIA
tabla_tia <- NULL

tia_file <- "data/TIA.csv"

if (file.exists(tia_file)) {
  # Detectar separador automáticamente
  separador_tia <- detectar_separador(tia_file)
  message("🔍 Separador detectado para TIA.csv: '", separador_tia, "'")
  
  tabla_tia <- read.csv(tia_file, 
                        sep = separador_tia, 
                        encoding = "UTF-8",
                        stringsAsFactors = FALSE, 
                        check.names = FALSE)
  
  message("📋 Nombres originales TIA: ", paste(names(tabla_tia), collapse = ", "))
  
  tabla_tia <- clean_tia_column_names(tabla_tia)
  
  message("✅ TIA cargada desde: ", tia_file, " - ", nrow(tabla_tia), " filas, ", ncol(tabla_tia), " columnas")
  message("📋 Columnas TIA después de limpieza: ", paste(names(tabla_tia), collapse = ", "))
  
  # Convertir población a numérica
  if ("poblacion_asegurada" %in% names(tabla_tia)) {
    tabla_tia[["poblacion_asegurada"]] <- as.numeric(
      as.character(tabla_tia[["poblacion_asegurada"]])
    )
  }
  
  # Asegurar que todas las columnas _tia sean numéricas
  cols_tia <- grep("_tia$", names(tabla_tia), value = TRUE)
  for (col in cols_tia) {
    tabla_tia[[col]] <- as.numeric(as.character(tabla_tia[[col]]))
  }
  
  message("📊 Columnas TIA detectadas (", length(cols_tia), "): ", paste(cols_tia, collapse = ", "))
  
} else {
  message("⚠️ No se encontró archivo data/TIA.csv")
}

# ==========================================
# LISTAS PARA FILTROS (INCLUYE DESCARTADO)
# ==========================================
if("diagnostico" %in% names(casos)) {
  enfermedades_lista <- sort(unique(casos[["diagnostico"]]))
  message("📋 Enfermedades: ", paste(enfermedades_lista, collapse = ", "))
} else {
  enfermedades_lista <- c("SIN DIAGNÓSTICOS")
  message("⚠️ No se encontró columna 'diagnostico'")
}

if("ano" %in% names(casos)) {
  anios_disponibles <- sort(unique(casos[["ano"]]))
  message("📅 Años: ", paste(anios_disponibles, collapse = ", "))
} else {
  anios_disponibles <- c(2025)
}

if("redes" %in% names(casos)) {
  redes_lista <- sort(unique(casos[["redes"]]))
  message("📋 Redes: ", paste(redes_lista, collapse = ", "))
} else {
  redes_lista <- c("SIN REDES")
}

if("tipo_de_caso" %in% names(casos)) {
  tipos_caso_lista <- sort(unique(casos[["tipo_de_caso"]]))
  message("📋 Tipos de caso: ", paste(tipos_caso_lista, collapse = ", "))
} else {
  tipos_caso_lista <- c("SIN TIPOS")
}

anios_opciones <- c("Todos", anios_disponibles)

# ==========================================
# KPIs GLOBALES INICIALES
# ==========================================
total_casos       <- nrow(casos)
total_confirmados <- if("tipo_de_caso" %in% names(casos)) {
  sum(casos[["tipo_de_caso"]] == "CONFIRMADO", na.rm = TRUE)
} else { 0 }
total_fallecidos  <- if("fecha_defuncion" %in% names(casos)) {
  sum(!is.na(casos[["fecha_defuncion"]]), na.rm = TRUE)
} else { 0 }
total_redes       <- length(redes_lista)
total_enfermedades <- length(enfermedades_lista)

# ==========================================
# MESES ABREVIADOS
# ==========================================
meses_abrev <- c(
  "1" = "Ene", "2" = "Feb", "3" = "Mar",
  "4" = "Abr", "5" = "May", "6" = "Jun",
  "7" = "Jul", "8" = "Ago", "9" = "Sep",
  "10" = "Oct", "11" = "Nov", "12" = "Dic"
)

if("mes" %in% names(casos)) {
  casos$mes_abr <- meses_abrev[as.character(casos$mes)]
}

# ==========================================
# RESUMEN FINAL DE CARGA
# ==========================================
message("")
message("═══════════════════════════════════════════")
message("   DASHBOARD INMUNOPREVENIBLES")
message("   ✅ Datos cargados correctamente")
message("   📊 Casos totales: ", total_casos)
message("   ✅ Confirmados: ", total_confirmados)
message("   💀 Fallecidos: ", total_fallecidos)
message("   🏥 Redes: ", total_redes)
message("   🦠 Enfermedades: ", total_enfermedades)
message("   📅 Años: ", paste(anios_disponibles, collapse = ", "))
message("═══════════════════════════════════════════")
message("")

# ==========================================
# CARGAR MÓDULOS
# ==========================================
source("R/mod_tablero.R", local = TRUE)
message("✅ Módulo tablero cargado")
source("R/mod_perfil.R", local = TRUE)
message("✅ Módulo perfil cargado")
source("R/mod_geografico.R", local = TRUE)
message("✅ Módulo geográfico cargado")