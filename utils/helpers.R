# ============================================================
# FUNCIONES AUXILIARES DEL DASHBOARD
# OIIS - Oficina de Inteligencia e Información en Salud
# ============================================================

# ==========================================
# 1. CREAR MEDIDOR T.I.A. (GAUGE)
# ==========================================
crear_medidor_tia <- function(valor_tia) {
  
  if(is.na(valor_tia) || !is.finite(valor_tia) || is.null(valor_tia)) {
    valor_tia <- 0
  }
  
  if(valor_tia <= 50) {
    color_aguja <- "#27AE60"
    zona <- "BAJO RIESGO"
  } else if(valor_tia <= 100) {
    color_aguja <- "#F1C40F"
    zona <- "PRECAUCIÓN"
  } else {
    color_aguja <- "#E74C3C"
    zona <- "ALERTA"
  }
  
  max_eje <- max(150, valor_tia + 20)
  
  plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = round(valor_tia, 1),
    number = list(
      suffix = "",
      font = list(size = 18, color = "#2C3E50", family = "Segoe UI"),
      valueformat = ".1f"
    ),
    title = list(
      text = paste0("<b>T.I.A.</b><br><span style='font-size:11px;'>", zona, "</span>"),
      font = list(size = 13, family = "Segoe UI")
    ),
    gauge = list(
      axis = list(
        range = list(0, max_eje),
        tickwidth = 1,
        tickcolor = "#2C3E50",
        tickfont = list(size = 9)
      ),
      bar = list(color = color_aguja, thickness = 0.25),
      bgcolor = "white",
      borderwidth = 2,
      bordercolor = "#ECF0F1",
      steps = list(
        list(range = c(0, 50), color = "rgba(39, 174, 96, 0.15)"),
        list(range = c(50, 100), color = "rgba(241, 196, 15, 0.15)"),
        list(range = c(100, max_eje), color = "rgba(231, 76, 60, 0.15)")
      ),
      threshold = list(
        line = list(color = "#E74C3C", width = 3),
        thickness = 0.6,
        value = 100
      )
    )
  ) %>%
    layout(
      margin = list(l = 15, r = 15, t = 45, b = 15),
      paper_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Segoe UI")
    )
}

# ==========================================
# 2. CREAR MEDIDOR DE LETALIDAD (GAUGE)
# ==========================================
crear_medidor_letalidad <- function(porcentaje) {
  
  if(is.na(porcentaje) || !is.finite(porcentaje) || is.null(porcentaje)) {
    porcentaje <- 0
  }
  
  if(porcentaje <= 5) {
    color_aguja <- "#27AE60"
    zona <- "CONTROLADO"
  } else if(porcentaje <= 10) {
    color_aguja <- "#F1C40F"
    zona <- "VIGILANCIA"
  } else {
    color_aguja <- "#E74C3C"
    zona <- "CRÍTICO"
  }
  
  max_eje <- max(20, porcentaje + 5)
  
  plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = round(porcentaje, 1),
    number = list(
      suffix = "%",
      font = list(size = 18, color = "#2C3E50", family = "Segoe UI"),
      valueformat = ".1f"
    ),
    title = list(
      text = paste0("<b>LETALIDAD</b><br><span style='font-size:11px;'>", zona, "</span>"),
      font = list(size = 13, family = "Segoe UI")
    ),
    gauge = list(
      axis = list(
        range = list(0, max_eje),
        tickwidth = 1,
        tickcolor = "#2C3E50",
        tickfont = list(size = 9)
      ),
      bar = list(color = color_aguja, thickness = 0.25),
      bgcolor = "white",
      borderwidth = 2,
      bordercolor = "#ECF0F1",
      steps = list(
        list(range = c(0, 5), color = "rgba(39, 174, 96, 0.15)"),
        list(range = c(5, 10), color = "rgba(241, 196, 15, 0.15)"),
        list(range = c(10, max_eje), color = "rgba(231, 76, 60, 0.15)")
      ),
      threshold = list(
        line = list(color = "#E74C3C", width = 3),
        thickness = 0.6,
        value = 10
      )
    )
  ) %>%
    layout(
      margin = list(l = 15, r = 15, t = 45, b = 15),
      paper_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Segoe UI")
    )
}

message("✅ helpers.R cargado correctamente")