# ============================================================
# MÓDULO: TABLERO DE CONTROL (PESTAÑA 1)
# OIIS - Oficina de Inteligencia e Información en Salud
# VERSIÓN FINAL v19 - Títulos dinámicos + KPIs mejorados
# ============================================================

# ==========================================
# UI DEL MÓDULO
# ==========================================
mod_tablero_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # FILTROS
    fluidRow(
      div(style = "padding: 15px 15px 0 15px;",
        div(style = "background-color: #0D4F85; border-radius: 12px; padding: 15px;",
          fluidRow(
            column(3,
              tags$label("RED ASISTENCIAL | PRESTACIONAL", style = "color: white; font-size: 11px; font-weight: bold;"),
              pickerInput(ns("filtro_red"), NULL,
                choices = c("TODOS" = "todos", redes_lista), 
                selected = "todos", multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, 
                  `live-search` = TRUE, 
                  size = 8,
                  `selected-text-format` = "count > 3",
                  `count-selected-text` = "TODOS ({0} seleccionados)"
                ),
                width = "100%")
            ),
            column(3,
              tags$label("DIAGNÓSTICO", style = "color: white; font-size: 11px; font-weight: bold;"),
              pickerInput(ns("filtro_diag"), NULL,
                choices = c("TODOS" = "todos", enfermedades_lista), 
                selected = "todos", multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, 
                  `live-search` = TRUE, 
                  size = 8,
                  `selected-text-format` = "count > 3",
                  `count-selected-text` = "TODOS ({0} seleccionados)"
                ),
                width = "100%")
            ),
            column(2,
              tags$label("TIPO DE CASO", style = "color: white; font-size: 11px; font-weight: bold;"),
              pickerInput(ns("filtro_tipo"), NULL,
                choices = c("TODOS" = "todos", "CONFIRMADO", "PROBABLE", "SOSPECHOSO", "DESCARTADO"),
                selected = "todos", multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, 
                  size = 6,
                  `selected-text-format` = "count > 3",
                  `count-selected-text` = "TODOS ({0} seleccionados)"
                ),
                width = "100%")
            ),
            column(2,
              tags$label("AÑO", style = "color: white; font-size: 11px; font-weight: bold;"),
              pickerInput(ns("filtro_anio"), NULL,
                choices = c("TODOS" = "TODOS", anios_opciones),
                selected = "TODOS", multiple = FALSE, width = "100%")
            ),
            column(2,
              tags$label(" ", style = "color: white; font-size: 11px;"),
              actionButton(ns("borrar_filtros"), "BORRAR FILTROS", class = "btn-borrar",
                style = "width: 100%; margin-top: 5px;")
            )
          )
        )
      )
    ),
    
    fluidRow(column(12, uiOutput(ns("titulo_completo")))),
    
    # BANNER ROTATIVO DE ALERTAS
    fluidRow(column(12, uiOutput(ns("alerta_vigilancia")))),
    
    # BLOQUE UNIFICADO DE INDICADORES
    fluidRow(
      div(style = "padding: 0 15px;",
        div(style = "background: linear-gradient(135deg, #0D4F85 0%, #1681D9 100%); 
                    border-radius: 12px; padding: 15px; margin-bottom: 15px;
                    box-shadow: 0 4px 15px rgba(0,0,0,0.15);",
          
          div(style = "color: white; font-size: 13px; font-weight: bold; 
                      margin-bottom: 12px; text-align: center; letter-spacing: 1px;",
              "INDICADORES DE VIGILANCIA EPIDEMIOLÓGICA"),
          
          fluidRow(
            column(3, div(style = "background: rgba(255,255,255,0.12); border-radius: 8px; padding: 10px; text-align: center; margin: 3px;", uiOutput(ns("kpi_tosferina_block")))),
            column(3, div(style = "background: rgba(255,255,255,0.12); border-radius: 8px; padding: 10px; text-align: center; margin: 3px;", uiOutput(ns("kpi_varicela_block")))),
            column(3, div(style = "background: rgba(255,255,255,0.12); border-radius: 8px; padding: 10px; text-align: center; margin: 3px;", uiOutput(ns("kpi_parotiditis_block")))),
            column(3, div(style = "background: rgba(255,255,255,0.12); border-radius: 8px; padding: 10px; text-align: center; margin: 3px;", uiOutput(ns("kpi_hepatitis_block"))))
          ),
          
          div(style = "border-top: 1px solid rgba(255,255,255,0.2); margin: 12px 0;"),
          
          fluidRow(
            column(4, div(style = "background: rgba(255,255,255,0.12); border-radius: 8px; padding: 10px; text-align: center; margin: 3px;", uiOutput(ns("kpi_casos_block")))),
            column(4, div(style = "background: rgba(255,255,255,0.12); border-radius: 8px; padding: 10px; text-align: center; margin: 3px;", uiOutput(ns("kpi_confirmados_block")))),
            column(4, div(style = "background: rgba(255,255,255,0.12); border-radius: 8px; padding: 10px; text-align: center; margin: 3px;", uiOutput(ns("kpi_fallecidos_block"))))
          )
        )
      )
    ),
    
    # NUEVA FILA: HEATMAP G1 (TABLA DE CALOR) + MAPA (CON TOOLTIP)
    fluidRow(
      box(title = div(uiOutput(ns("titulo_g1")), tags$button(class = "btn-fullscreen",
        onclick = paste0("var el = document.getElementById('", ns("g1_container"), "'); if(el.requestFullscreen) { el.requestFullscreen(); }"),
        tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 6,
        div(id = ns("g1_container"), style = "overflow-x: auto; width: 100%;", 
            DTOutput(ns("g1_heatmap")))),
      box(title = div(uiOutput(ns("titulo_mapa")), tags$button(class = "btn-fullscreen",
        onclick = paste0("var el = document.getElementById('", ns("mapa_container"), "'); if(el.requestFullscreen) { el.requestFullscreen(); }"),
        tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 6,
        div(id = ns("mapa_container"), style = "width: 100%;", leafletOutput(ns("mapa_tia"), height = 400)))
    ),
    
    # TENDENCIA MENSUAL
    fluidRow(
      box(title = div(uiOutput(ns("titulo_tendencia")), tags$button(class = "btn-fullscreen",
        onclick = paste0("var el = document.getElementById('", ns("tendencia_container"), "'); if(el.requestFullscreen) { el.requestFullscreen(); }"),
        tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 12,
        div(id = ns("tendencia_container"), style = "width: 100%; min-height: 350px;", plotlyOutput(ns("tendencia_mensual"), height = 350)))
    ),
    
    # RANKING TIA + TOP ENFERMEDADES
    fluidRow(
      box(title = div(uiOutput(ns("titulo_ranking")), tags$button(class = "btn-fullscreen",
        onclick = paste0("var el = document.getElementById('", ns("ranking_container"), "'); if(el.requestFullscreen) { el.requestFullscreen(); }"),
        tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 6,
        div(id = ns("ranking_container"), style = "width: 100%; min-height: 400px;", plotlyOutput(ns("ranking_tia"), height = 400))),
      box(title = div(uiOutput(ns("titulo_g2")), tags$button(class = "btn-fullscreen",
        onclick = paste0("var el = document.getElementById('", ns("g2_container"), "'); if(el.requestFullscreen) { el.requestFullscreen(); }"),
        tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 6,
        div(id = ns("g2_container"), style = "width: 100%; min-height: 400px;", plotlyOutput(ns("g2_barras_enfermedades"), height = 400)))
    ),
    
    # TABLA
    fluidRow(
      box(title = div(style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        uiOutput(ns("titulo_g6")),
        div(downloadButton(ns("descargar_excel"), "EXCEL", class = "btn-descarga"), downloadButton(ns("descargar_csv"), "CSV", class = "btn-descarga"))),
        status = "primary", solidHeader = TRUE, width = 12, DTOutput(ns("g6_tabla_casos")))
    ),
    
    fluidRow(column(12, div(class = "footer-oiis",
      tags$img(src = "OIIS.png", style = "height: 45px; margin-bottom: 10px;"),
      tags$p("Elaborado por: Oficina de Inteligencia e Información Sanitaria OIIS", style = "font-weight: bold; margin: 5px 0;"),
      tags$p("Información preliminar sujeta a modificación", style = "font-style: italic; color: #7F8C8D;"),
      tags$hr(style = "border-color: #ECF0F1; margin: 10px 40px;"),
      tags$p(tags$i(class = "fas fa-info-circle", style = "color: #3498DB; margin-right: 5px;"),
        "Población asegurada de referencia: marzo 2026 (datos del 1 al 31 de marzo de 2026).",
        style = "font-size: 11px; color: #7F8C8D;"))))
  )
}

# ==========================================
# SERVER DEL MÓDULO
# ==========================================
mod_tablero_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Función helper para procesar selecciones con "TODOS"
    procesar_seleccion <- function(seleccion, lista_completa) {
      if(length(seleccion) == 0) return(lista_completa)
      if("todos" %in% seleccion) return(lista_completa)
      return(seleccion)
    }
    
    observeEvent(input$borrar_filtros, {
      updatePickerInput(session, "filtro_red", selected = "todos")
      updatePickerInput(session, "filtro_diag", selected = "todos")
      updatePickerInput(session, "filtro_tipo", selected = "todos")
      updatePickerInput(session, "filtro_anio", selected = "TODOS")
    })
    
    color_anio <- reactive({
      anio <- input$filtro_anio
      if(anio == "TODOS") return("#3498DB")
      if(anio == "2024") return("#2ECC71")
      if(anio == "2025") return("#E67E22")
      if(anio == "2026") return("#9B59B6")
      return("#3498DB")
    })
    
    calcular_tia_total <- function(df, cols_tia) {
      if(length(cols_tia) == 0) return(rep(0, nrow(df)))
      n_filas <- nrow(df)
      n_cols <- length(cols_tia)
      mat <- matrix(NA_real_, nrow = n_filas, ncol = n_cols)
      for(j in seq_along(cols_tia)) {
        if(cols_tia[j] %in% names(df)) {
          mat[, j] <- as.numeric(as.character(df[[cols_tia[j]]]))
        } else {
          mat[, j] <- 0
        }
      }
      if(n_filas == 1) {
        return(sum(mat, na.rm = TRUE))
      } else {
        return(rowSums(mat, na.rm = TRUE))
      }
    }
    
    datos_tia_filtrados <- reactive({
      if(is.null(tabla_tia) || nrow(tabla_tia) == 0) return(NULL)
      d <- tabla_tia
      redes_seleccionadas <- procesar_seleccion(input$filtro_red, redes_lista)
      if(length(redes_seleccionadas) > 0 && length(redes_seleccionadas) < length(redes_lista)) {
        d <- d[d[["red"]] %in% redes_seleccionadas, ]
      }
      cols_tia <- grep("_tia$", names(d), value = TRUE)
      if(length(cols_tia) > 0) d$tia_total <- calcular_tia_total(d, cols_tia)
      return(d)
    })
    
    # ==========================================
    # DATOS FILTRADOS PARA G1 (TABLA DE CALOR)
    # ==========================================
    datos_filtrados_todos <- reactive({
      d <- casos
      if(input$filtro_anio != "TODOS") d <- d[d[["ano"]] == as.numeric(input$filtro_anio), ]
      
      redes_seleccionadas <- procesar_seleccion(input$filtro_red, redes_lista)
      if(length(redes_seleccionadas) > 0 && length(redes_seleccionadas) < length(redes_lista)) {
        d <- d[d[["redes"]] %in% redes_seleccionadas, ]
      }
      
      diag_seleccionados <- procesar_seleccion(input$filtro_diag, enfermedades_lista)

# Traducir selecciones a formato CSV para filtrar correctamente
diag_seleccionados_csv <- sapply(diag_seleccionados, function(diag) {
  traducir_diagnostico_csv(diag)
})

if(length(diag_seleccionados_csv) > 0 && length(diag_seleccionados_csv) < length(enfermedades_lista)) {
  d <- d[d[["diagnostico"]] %in% diag_seleccionados_csv, ]
}

      
      tipos_seleccionados <- procesar_seleccion(input$filtro_tipo, c("CONFIRMADO", "PROBABLE", "SOSPECHOSO", "DESCARTADO"))
      if(length(tipos_seleccionados) > 0 && length(tipos_seleccionados) < 4) {
        d <- d[d[["tipo_de_caso"]] %in% tipos_seleccionados, ]
      }
      
      return(d)
    })
    
    datos_filtrados <- reactive({
      d <- datos_filtrados_todos()
      d <- d[d[["tipo_de_caso"]] != "DESCARTADO", ]
      return(d)
    })
    
    # ==========================================
    # FUNCIÓN PARA TÍTULOS CON COLOR BADGE - 🔥 CORREGIDA
    # ==========================================
    titulo_badge <- function(base, filtros) {
      badges <- c()
      
      # Red
      if(!is.null(filtros$red) && length(filtros$red) > 0 && !("todos" %in% filtros$red)) {
        if(length(filtros$red) == 1) {
          badges <- c(badges, paste0('<span style="background:#0D4F85;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', filtros$red, '</span>'))
        } else {
          badges <- c(badges, paste0('<span style="background:#0D4F85;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', length(filtros$red), ' redes</span>'))
        }
      }
      
      # 🔥 CORRECCIÓN: Diagnóstico - Traducir a nombre visual
      if(!is.null(filtros$diagnostico) && length(filtros$diagnostico) > 0 && !("todos" %in% filtros$diagnostico)) {
        # Traducir cada diagnóstico a su nombre visual
        diagnosticos_traducidos <- sapply(filtros$diagnostico, function(d) {
          traducir_diagnostico_visual(d)
        })
        
        if(length(diagnosticos_traducidos) == 1) {
          badges <- c(badges, paste0('<span style="background:#E74C3C;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', diagnosticos_traducidos, '</span>'))
        } else {
          badges <- c(badges, paste0('<span style="background:#E74C3C;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', length(diagnosticos_traducidos), ' diags</span>'))
        }
      }
      
      # Tipo
      if(!is.null(filtros$tipo) && length(filtros$tipo) > 0 && !("todos" %in% filtros$tipo)) {
        if(length(filtros$tipo) == 1) {
          badges <- c(badges, paste0('<span style="background:#F39C12;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', filtros$tipo, '</span>'))
        } else {
          badges <- c(badges, paste0('<span style="background:#F39C12;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', length(filtros$tipo), ' tipos</span>'))
        }
      }
      
      # Año
      if(!is.null(filtros$anio) && filtros$anio != "TODOS") {
        badges <- c(badges, paste0('<span style="background:#9B59B6;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', filtros$anio, '</span>'))
      }
      
      if(length(badges) == 0) {
        return(base)
      } else {
        return(paste0(base, ' ', paste(badges, collapse = " ")))
      }
    }
    
    # ==========================================
    # TÍTULO COMPLETO - 🔥 CORREGIDO
    # ==========================================
    output$titulo_completo <- renderUI({
      red_sel <- input$filtro_red
      if("todos" %in% red_sel || length(red_sel) == 0) {
        txt_red <- "TODAS LAS REDES"
      } else if(length(red_sel) == 1) {
        txt_red <- paste("Red", red_sel)
      } else {
        txt_red <- paste("Redes:", paste(red_sel, collapse = ", "))
      }
      
      tipo_sel <- input$filtro_tipo
      if("todos" %in% tipo_sel || length(tipo_sel) == 0 || length(tipo_sel) >= 4) {
        txt_tipo <- "Todos los tipos"
      } else if(length(tipo_sel) == 1) {
        txt_tipo <- paste("Tipo:", tipo_sel)
      } else {
        txt_tipo <- paste("Tipos:", paste(tipo_sel, collapse = ", "))
      }
      
      anio_sel <- input$filtro_anio
      txt_anio <- if(anio_sel == "TODOS") "Todos los años" else paste("Año:", anio_sel)
      
      diag_sel <- input$filtro_diag
      
      # 🔥 CORRECCIÓN: Traducir diagnósticos a nombres visuales
      if("todos" %in% diag_sel || length(diag_sel) == 0 || length(diag_sel) >= length(enfermedades_lista)) {
        txt_diag <- "Todos los diagnósticos"
      } else if(length(diag_sel) == 1) {
        # Traducir el diagnóstico seleccionado
        diag_traducido <- traducir_diagnostico_visual(diag_sel)
        txt_diag <- paste("Diag:", diag_traducido)
      } else {
        # Traducir todos los diagnósticos seleccionados
        diag_traducidos <- sapply(diag_sel, traducir_diagnostico_visual)
        txt_diag <- paste("Diag:", paste(diag_traducidos, collapse = ", "))
      }
      
      div(class = "red-seleccionada", style = "font-size: 13px;", 
          paste0("TABLERO DE CONTROL | ", txt_red, " | ", txt_tipo, " | ", txt_anio, " | ", txt_diag))
    })
    
    # ==========================================
    # TÍTULOS DINÁMICOS CON COLOR BADGE
    # ==========================================
    output$titulo_g1 <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("TABLA DE CALOR POR DIAGNÓSTICO", filtros))
    })
    
    output$titulo_mapa <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("TASA DE INCIDENCIA ACUMULADA (TIA) POR RED", filtros))
    })
    
    output$titulo_tendencia <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("ENFERMEDADES CON MAYOR FRECUENCIA", filtros))
    })
    
    output$titulo_ranking <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("TIA POR DIAGNÓSTICO", filtros))
    })
    
    output$titulo_g2 <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("DISTRIBUCIÓN DE DIAGNÓSTICOS", filtros))
    })
    
    output$titulo_g6 <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("REGISTRO DE CASOS NOTIFICADOS", filtros))
    })
    
# ==========================================
# BANNER ROTATIVO DE ALERTAS (DINÁMICO - SOLO TIA > 0)
# ==========================================
output$alerta_vigilancia <- renderUI({
  d <- datos_tia_filtrados()
  if(is.null(d) || nrow(d) == 0) {
    return(div(style="padding:0 15px;margin-bottom:10px;",
      div(style="background:#FFF3CD;border-left:4px solid #FFC107;padding:10px 15px;border-radius:4px;display:flex;align-items:center;",
        tags$i(class="fas fa-info-circle", style="color:#856404;font-size:18px;margin-right:10px;"),
        tags$span(style="color:#856404;","No hay datos de TIA para los filtros seleccionados."))))
  }
  
  # 🔥 OBTENER INFORMACIÓN DE FILTROS PARA CONTEXTO
  filtros_contexto <- c()
  
  redes_sel <- input$filtro_red
  if(!is.null(redes_sel) && length(redes_sel) > 0 && !("todos" %in% redes_sel)) {
    if(length(redes_sel) == 1) {
      filtros_contexto <- c(filtros_contexto, paste0("Red: ", redes_sel))
    } else {
      filtros_contexto <- c(filtros_contexto, paste0(length(redes_sel), " redes"))
    }
  }
  
  diag_sel <- input$filtro_diag
  if(!is.null(diag_sel) && length(diag_sel) > 0 && !("todos" %in% diag_sel)) {
    if(length(diag_sel) == 1) {
      filtros_contexto <- c(filtros_contexto, paste0("Diag: ", diag_sel))
    } else {
      filtros_contexto <- c(filtros_contexto, paste0(length(diag_sel), " diagnósticos"))
    }
  }
  
  tipo_sel <- input$filtro_tipo
  if(!is.null(tipo_sel) && length(tipo_sel) > 0 && !("todos" %in% tipo_sel)) {
    if(length(tipo_sel) == 1) {
      filtros_contexto <- c(filtros_contexto, paste0("Tipo: ", tipo_sel))
    } else {
      filtros_contexto <- c(filtros_contexto, paste0(length(tipo_sel), " tipos"))
    }
  }
  
  anio_sel <- input$filtro_anio
  if(!is.null(anio_sel) && anio_sel != "TODOS") {
    filtros_contexto <- c(filtros_contexto, paste0("Año: ", anio_sel))
  }
  
  texto_contexto <- if(length(filtros_contexto) > 0) {
    paste0(" (", paste(filtros_contexto, collapse = " | "), ")")
  } else {
    ""
  }
  
  alertas <- list()
  agregar_alerta <- function(red, enfermedad, valor, tipo) {
    if(tipo == "roja") {
      list(red=red, enfermedad=enfermedad, valor=valor, icono="🔴", borde="#E74C3C", fondo="#FFF5F5", etiqueta="CRÍTICO")
    } else {
      list(red=red, enfermedad=enfermedad, valor=valor, icono="🟡", borde="#F39C12", fondo="#FFFDE7", etiqueta="OBSERVACIÓN")
    }
  }
  
  # 🔥 LISTA DE ENFERMEDADES CON TIA (EXCLUYENDO ESAVI)
  enfermedades_tia <- list(
    "tos_ferina_tia" = "TOS FERINA",
    "varicela_sin_complicaciones_tia" = "VARICELA S.C.",
    "varicela_con_complicaciones_tia" = "VARICELA C.C.",
    "hepatitis_b_tia" = "HEPATITIS B",
    "parotiditis_sin_complicaciones_tia" = "PAROTIDITIS S.C.",
    "parotiditis_complicaciones_tia" = "PAROTIDITIS C.C.",
    "difteria_tia" = "DIFTERIA",
    "sarampion_tia" = "SARAMPIÓN",
    "rubeola_tia" = "RUBEOLA",
    "tetanos_tia" = "TÉTANOS",
    "fiebre_amarilla_tia" = "FIEBRE AMARILLA",
    "paralisis_flacida_tia" = "PARÁLISIS FLÁCIDA"
  )
  
  # 🔥 OBTENER LOS DIAGNÓSTICOS SELECCIONADOS EN EL FILTRO
  diag_filtro <- input$filtro_diag
  es_todos <- ("todos" %in% diag_filtro)
  
  # 🔥 RECORRER TODAS LAS ENFERMEDADES CON TIA
  for(col_name in names(enfermedades_tia)) {
    enf_nombre <- enfermedades_tia[[col_name]]
    
    # Verificar si esta enfermedad está en los filtros
    if(es_todos || any(sapply(diag_filtro, function(f) comparar_diagnosticos(enf_nombre, f)))) {
      
      if(col_name %in% names(d)) {
        # 🔥 OBTENER SOLO FILAS DONDE TIA > 0 (diferente de 0.00)
        d_filtrado <- d %>% filter(!!sym(col_name) > 0)
        
        # Si hay datos con TIA > 0
        if(nrow(d_filtrado) > 0) {
          # Buscar valores críticos (> 1.0)
          altas <- d_filtrado %>% filter(!!sym(col_name) > 1.0)
          for(i in seq_len(nrow(altas))) {
            alertas[[length(alertas)+1]] <- agregar_alerta(
              altas$red[i], 
              enf_nombre, 
              round(altas[[col_name]][i], 1), 
              "roja"
            )
          }
          # Buscar valores de observación (> 0.5 y <= 1.0)
          medias <- d_filtrado %>% filter(!!sym(col_name) > 0.5 & !!sym(col_name) <= 1.0)
          for(i in seq_len(nrow(medias))) {
            alertas[[length(alertas)+1]] <- agregar_alerta(
              medias$red[i], 
              enf_nombre, 
              round(medias[[col_name]][i], 1), 
              "amarilla"
            )
          }
          # 🔥 Buscar valores bajos (> 0 y <= 0.5) - solo para mostrar que hay TIA
          bajos <- d_filtrado %>% filter(!!sym(col_name) > 0 & !!sym(col_name) <= 0.5)
          for(i in seq_len(nrow(bajos))) {
            alertas[[length(alertas)+1]] <- agregar_alerta(
              bajos$red[i], 
              enf_nombre, 
              round(bajos[[col_name]][i], 2), 
              "verde"
            )
          }
        }
      }
    }
  }
  
  # 🔥 SI NO HAY ALERTAS, MOSTRAR MENSAJE CON CONTEXTO DE FILTROS
  if(length(alertas) == 0) {
    n_redes <- nrow(d)
    return(div(style="padding:0 15px;margin-bottom:10px;",
      div(style="background:#F0FFF0;border-left:4px solid #27AE60;padding:10px 15px;border-radius:4px;display:flex;align-items:center;",
        tags$i(class="fas fa-check-circle", style="color:#27AE60;font-size:18px;margin-right:10px;"),
        tags$strong(style="color:#27AE60;","✅ SIN ALERTAS ACTIVAS"),
        tags$span(style="color:#2C3E50;margin-left:5px;",
          paste0("para ", n_redes, " red(es)", texto_contexto, " con TIA dentro de rangos esperados.")))))
  }
  
  total_alertas <- length(alertas)
  
  tagList(
    tags$style(HTML(".alerta-item{display:none;}.alerta-item.active{display:flex;align-items:center;animation:fadeInOut 4s ease-in-out;}@keyframes fadeInOut{0%{opacity:0;transform:translateX(20px);}10%{opacity:1;transform:translateX(0);}80%{opacity:1;transform:translateX(0);}100%{opacity:0;transform:translateX(-20px);}}@keyframes pulse{0%,100%{opacity:1;}50%{opacity:0.5;}}")),
    tags$script(HTML(paste0("var alertaIndex=0;var totalAlertas=",total_alertas,";function rotarAlertas(){var items=document.querySelectorAll('.alerta-item');items.forEach(function(item){item.classList.remove('active');});var actual=document.getElementById('alerta-'+alertaIndex);if(actual){actual.classList.add('active');}var contador=document.getElementById('alerta-contador');if(contador){contador.textContent=(alertaIndex+1)+'/'+totalAlertas;}var puntos=document.querySelectorAll('.alerta-punto');puntos.forEach(function(p,idx){p.style.opacity=idx===alertaIndex?'1':'0.4';});alertaIndex=(alertaIndex+1)%totalAlertas;}setInterval(rotarAlertas,8000);setTimeout(function(){rotarAlertas();},100);"))),
    div(style="padding:0 15px;margin-bottom:10px;",
      div(style="background:#FFF5F5;border-radius:8px;overflow:hidden;box-shadow:0 2px 8px rgba(0,0,0,0.08);",
        div(style="background:#E74C3C;color:white;padding:6px 15px;display:flex;align-items:center;justify-content:space-between;",
          div(tags$i(class="fas fa-bell",style="margin-right:8px;animation:pulse 2s infinite;"), 
              tags$strong("ALERTAS DE VIGILANCIA EPIDEMIOLÓGICA"),
              tags$span(style="font-size:11px;font-weight:normal;margin-left:8px;opacity:0.8;", texto_contexto)),
          tags$small(id="alerta-contador",style="opacity:0.9;",paste0("1/",total_alertas))),
        div(style="padding:8px;min-height:40px;",lapply(seq_along(alertas),function(i){
          a <- alertas[[i]]
          # 🔥 DETERMINAR COLOR DE FONDO SEGÚN EL VALOR
          if(a$etiqueta == "CRÍTICO") {
            bg_color <- "#E74C3C"
            text_color <- "white"
          } else if(a$etiqueta == "OBSERVACIÓN") {
            bg_color <- "#F39C12"
            text_color <- "white"
          } else {
            bg_color <- "#27AE60"
            text_color <- "white"
          }
          div(id=paste0("alerta-",i-1),class="alerta-item",style=paste0("padding:6px 10px;border-radius:4px;background:",a$fondo,";border-left:4px solid ",a$borde,";"),
            tags$span(style="font-size:16px;margin-right:10px;",a$icono),
            tags$strong(style="color:#2C3E50;",a$red),
            tags$span(style="color:#7F8C8D;margin:0 8px;","→"),
            tags$span(style="color:#2C3E50;",paste0(a$enfermedad,": TIA ",a$valor)),
            tags$span(style=paste0("margin-left:auto;padding:2px 10px;border-radius:12px;font-size:10px;font-weight:bold;color:",text_color,";background:",bg_color,";"),a$etiqueta)
          )
        })),
        div(style="display:flex;justify-content:center;gap:6px;padding:6px;background:rgba(0,0,0,0.05);",
          lapply(seq_along(alertas),function(i){
            a <- alertas[[i]]
            div(class="alerta-punto",style=paste0("width:8px;height:8px;border-radius:50%;background:",a$borde,";opacity:0.4;"))
          })
        )
      )
    )
  )
})
    
    # ==========================================
    # FUNCIÓN KPI TIA MÁXIMO
    # ==========================================
    crear_kpi_tia_max <- function(df, columna_tia, icono_nombre) {
      if(is.null(df) || nrow(df) == 0 || !columna_tia %in% names(df)) {
        return(list(valor = "SIN CASOS", red = "", color = "black", icono = icono_nombre))
      }
      valores <- df[[columna_tia]]
      if(all(is.na(valores) | valores == 0)) {
        return(list(valor = "SIN CASOS", red = "", color = "black", icono = icono_nombre))
      }
      idx_max <- which.max(valores)
      valor_max <- round(valores[idx_max], 2)
      red_max <- df[["red"]][idx_max]
      if(valor_max <= 0.5) {
        color <- "green"
      } else if(valor_max <= 1.0) {
        color <- "yellow"
      } else if(valor_max <= 2.0) {
        color <- "orange"
      } else {
        color <- "red"
      }
      return(list(valor = valor_max, red = red_max, color = color, icono = icono_nombre))
    }
    
    color_semaforo <- function(color) {
      switch(color,
        green = "#27AE60",
        yellow = "#F1C40F",
        orange = "#E67E22",
        red = "#E74C3C",
        "#95A5A6"
      )
    }
    
    
# ==========================================
# TARJETAS DE TIA DINÁMICAS (CON FALLECIDOS CONDICIONAL)
# ==========================================
obtener_top4_enfermedades <- reactive({
  d <- casos
  if(input$filtro_anio != "TODOS") d <- d[d[["ano"]] == as.numeric(input$filtro_anio), ]
  
  redes_seleccionadas <- procesar_seleccion(input$filtro_red, redes_lista)
  if(length(redes_seleccionadas) > 0 && length(redes_seleccionadas) < length(redes_lista)) {
    d <- d[d[["redes"]] %in% redes_seleccionadas, ]
  }
  
  diag_seleccionados <- procesar_seleccion(input$filtro_diag, enfermedades_lista)
  
  # 🔥 Traducir selecciones a formato CSV para filtrar correctamente
  diag_seleccionados_csv <- sapply(diag_seleccionados, function(diag) {
    traducir_diagnostico_csv(diag)
  })
  
  if(length(diag_seleccionados_csv) > 0 && length(diag_seleccionados_csv) < length(enfermedades_lista)) {
    d <- d[d[["diagnostico"]] %in% diag_seleccionados_csv, ]
  }
  
  d <- d %>% filter(tipo_de_caso == "CONFIRMADO")
  if(nrow(d) == 0) return(character(0))
  top4 <- d %>% count(diagnostico, sort = TRUE) %>% head(4) %>% pull(diagnostico)
  return(top4)
})


# 🔥 NUEVA FUNCIÓN: Obtener fallecidos por diagnóstico
obtener_fallecidos_por_diagnostico <- function(diagnostico) {
  d <- casos %>%
    filter(
      diagnostico == !!diagnostico,
      tipo_de_caso == "CONFIRMADO",
      !is.na(fecha_defuncion),
      as.character(fecha_defuncion) != ""
    )
  
  # Aplicar filtros adicionales
  if(input$filtro_anio != "TODOS") {
    d <- d %>% filter(ano == as.numeric(input$filtro_anio))
  }
  
  redes_seleccionadas <- procesar_seleccion(input$filtro_red, redes_lista)
  if(length(redes_seleccionadas) > 0 && length(redes_seleccionadas) < length(redes_lista)) {
    d <- d %>% filter(redes %in% redes_seleccionadas)
  }
  
  return(nrow(d))
}

# 🔥 NUEVA FUNCIÓN: Verificar si el filtro está en modo "TODOS"
es_filtro_todos <- function() {
  diag_filtro <- input$filtro_diag
  return(is.null(diag_filtro) || length(diag_filtro) == 0 || "todos" %in% diag_filtro)
}

# Mapeo de diagnóstico visual a columna TIA
diag_to_tia_col <- function(diagnostico) {
  mapping <- list(
    "TOS FERINA" = "tos_ferina_tia",
    "HEPATITIS B" = "hepatitis_b_tia",
    "PAROTIDITIS SIN COMPLICACIONES" = "parotiditis_sin_complicaciones_tia",
    "PAROTIDITIS CON COMPLICACIONES" = "parotiditis_complicaciones_tia",
    "VARICELA SIN COMPLICACIONES" = "varicela_sin_complicaciones_tia",
    "VARICELA CON OTRAS COMPLICACIONES" = "varicela_con_complicaciones_tia",
    "DIFTERIA" = "difteria_tia",
    "SARAMPION" = "sarampion_tia",
    "RUBEOLA" = "rubeola_tia",
    "TETANOS" = "tetanos_tia",
    "FIEBRE AMARILLA SELVATICA" = "fiebre_amarilla_tia",
    "PARALISIS FLACIDA AGUDA" = "paralisis_flacida_tia"
  )
  return(mapping[[diagnostico]])
}

# Mapeo de diagnóstico a ícono
diag_to_icon <- function(diagnostico) {
  mapping <- list(
    "TOS FERINA" = "lungs",
    "HEPATITIS B" = "shield-virus",
    "PAROTIDITIS SIN COMPLICACIONES" = "head-side-virus",
    "PAROTIDITIS CON COMPLICACIONES" = "head-side-virus",
    "VARICELA SIN COMPLICACIONES" = "dot-circle",
    "VARICELA CON OTRAS COMPLICACIONES" = "dot-circle",
    "DIFTERIA" = "syringe",
    "SARAMPION" = "virus",
    "RUBEOLA" = "virus",
    "TETANOS" = "syringe",
    "FIEBRE AMARILLA SELVATICA" = "mosquito",
    "PARALISIS FLACIDA AGUDA" = "wheelchair"
  )
  if(is.null(mapping[[diagnostico]])) return("chart-bar")
  return(mapping[[diagnostico]])
}

# Mapeo de diagnóstico a nombre corto
diag_to_short <- function(diagnostico) {
  mapping <- list(
    "TOS FERINA" = "TOS FERINA",
    "HEPATITIS B" = "HEPATITIS B",
    "PAROTIDITIS SIN COMPLICACIONES" = "PAROTIDITIS S.C.",
    "PAROTIDITIS CON COMPLICACIONES" = "PAROTIDITIS C.C.",
    "VARICELA SIN COMPLICACIONES" = "VARICELA S.C.",
    "VARICELA CON OTRAS COMPLICACIONES" = "VARICELA C.C.",
    "DIFTERIA" = "DIFTERIA",
    "SARAMPION" = "SARAMPIÓN",
    "RUBEOLA" = "RUBEOLA",
    "TETANOS" = "TETANOS",
    "FIEBRE AMARILLA SELVATICA" = "FIEBRE AMARILLA",
    "PARALISIS FLACIDA AGUDA" = "PARALISIS FLAC."
  )
  if(is.null(mapping[[diagnostico]])) return(toupper(diagnostico))
  return(mapping[[diagnostico]])
}

# ==========================================
# CREAR TARJETA DINÁMICA - 🔥 CORREGIDA
# ==========================================
crear_tarjeta_dinamica <- function(pos, top4) {
  # Verificar si hay filtro de diagnóstico activo
  diag_filtro <- input$filtro_diag
  hay_filtro_diag <- !is.null(diag_filtro) && length(diag_filtro) > 0 && !("todos" %in% diag_filtro)
  modo_todos <- es_filtro_todos()
  
  # Si no hay suficientes enfermedades en el top4
  if(length(top4) < pos) {
    return(div(
      tags$i(class = "fas fa-chart-bar", style = "font-size: 24px; color: white; margin-bottom: 8px;"),
      div(style = "color: white; font-size: 14px; font-weight: bold; margin-bottom: 5px;", "SIN CASOS"),
      div(style = "color: white; font-size: 24px; font-weight: bold;", "-"),
      div(style = "color: rgba(255,255,255,0.8); font-size: 10px; margin-top: 5px;", "Filtros sin coincidencias")
    ))
  }
  
  # 🔥 CORRECCIÓN: Obtener el nombre del diagnóstico y traducirlo
  diag_name <- top4[pos]  # Esto es "PAROTIDITIS" del CSV
  
  # 🔥 Traducir el nombre del CSV a nombre VISUAL
  diag_name_visual <- traducir_diagnostico_visual(diag_name)
  
  # Usar el nombre visual para los mapeos
  tia_col <- diag_to_tia_col(diag_name_visual)
  icon_name <- diag_to_icon(diag_name_visual)
  short_name <- diag_to_short(diag_name_visual)
  
  # Si no hay columna TIA para este diagnóstico
  if(is.null(tia_col)) {
    return(div(
      tags$i(class = paste0("fas fa-", icon_name), style = "font-size: 24px; color: white; margin-bottom: 8px;"),
      div(style = "color: white; font-size: 14px; font-weight: bold; margin-bottom: 5px;", short_name),
      div(style = "color: white; font-size: 24px; font-weight: bold;", "SIN CASOS"),
      div(style = "color: rgba(255,255,255,0.8); font-size: 10px; margin-top: 5px;", "Sin TIA disponible")
    ))
  }
  
  res <- crear_kpi_tia_max(datos_tia_filtrados(), tia_col, icon_name)
  
  # Si no hay casos (TIA = 0 o sin datos)
  if(res$valor == "SIN CASOS") {
    # 🔥 Si hay filtro activo, mostrar fallecidos
    if(hay_filtro_diag) {
      fallecidos <- obtener_fallecidos_por_diagnostico(diag_name)  # Usar nombre CSV para filtrar
      if(fallecidos > 0) {
        return(div(
          tags$i(class = paste0("fas fa-", icon_name), style = "font-size: 24px; color: white; margin-bottom: 8px;"),
          div(style = "color: white; font-size: 14px; font-weight: bold; margin-bottom: 5px;", short_name),
          div(style = "color: white; font-size: 24px; font-weight: bold;", "SIN CASOS"),
          div(style = "color: rgba(255,255,255,0.8); font-size: 10px; margin-top: 5px;", "No hay casos confirmados"),
          div(style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid rgba(255,255,255,0.2);",
            tags$i(class = "fas fa-skull", style = "font-size: 14px; color: #E74C3C; margin-right: 5px;"),
            span(style = "color: #E74C3C; font-size: 16px; font-weight: bold;", paste0("Fallecidos: ", fallecidos))
          )
        ))
      }
    }
    
    return(div(
      tags$i(class = paste0("fas fa-", icon_name), style = "font-size: 24px; color: white; margin-bottom: 8px;"),
      div(style = "color: white; font-size: 14px; font-weight: bold; margin-bottom: 5px;", short_name),
      div(style = "color: white; font-size: 24px; font-weight: bold;", "SIN CASOS"),
      div(style = "color: rgba(255,255,255,0.8); font-size: 10px; margin-top: 5px;", "No hay casos confirmados")
    ))
  }
  
  # 🔥 Si hay casos, mostrar TIA con etiqueta
  elementos_tarjeta <- list(
    tags$i(class = paste0("fas fa-", icon_name), style = "font-size: 24px; color: white; margin-bottom: 8px;"),
    div(style = "color: white; font-size: 14px; font-weight: bold; margin-bottom: 5px;", short_name),
    div(style = "color: white; font-size: 28px; font-weight: bold;", 
        paste0("TIA: ", res$valor)),
    div(style = "color: rgba(255,255,255,0.8); font-size: 10px; margin-top: 5px;", 
        paste0("Red: ", res$red)),
    div(style = paste0("display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: ", 
                       color_semaforo(res$color), "; margin-top: 6px;"))
  )
  
  # 🔥 AGREGAR INFORMACIÓN DE FALLECIDOS SOLO SI HAY FILTRO ACTIVO
  if(hay_filtro_diag) {
    fallecidos <- obtener_fallecidos_por_diagnostico(diag_name)
    if(fallecidos > 0) {
      elementos_tarjeta <- c(elementos_tarjeta, list(
        div(style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid rgba(255,255,255,0.2);",
          tags$i(class = "fas fa-skull", style = "font-size: 14px; color: #E74C3C; margin-right: 5px;"),
          span(style = "color: #E74C3C; font-size: 16px; font-weight: bold;", paste0("Fallecidos: ", fallecidos))
        )
      ))
    }
  }
  
  do.call(div, elementos_tarjeta)
}

# KPIs DE ENFERMEDADES (Renderizados)
output$kpi_tosferina_block <- renderUI({
  top4 <- obtener_top4_enfermedades()
  crear_tarjeta_dinamica(1, top4)
})

output$kpi_varicela_block <- renderUI({
  top4 <- obtener_top4_enfermedades()
  crear_tarjeta_dinamica(2, top4)
})

output$kpi_parotiditis_block <- renderUI({
  top4 <- obtener_top4_enfermedades()
  crear_tarjeta_dinamica(3, top4)
})

output$kpi_hepatitis_block <- renderUI({
  top4 <- obtener_top4_enfermedades()
  crear_tarjeta_dinamica(4, top4)
})

# ==========================================
# KPIs DE CASOS (TAMAÑO UNIFORME)
# ==========================================
output$kpi_casos_block <- renderUI({
  total <- nrow(datos_filtrados())
  div(
    tags$i(class = "fas fa-users", style = "font-size: 24px; color: #F39C12; margin-bottom: 8px;"),
    div(style = "color: white; font-size: 28px; font-weight: bold;", 
        if(total == 0) "SIN CASOS" else format(total, big.mark = ",")),
    div(style = "color: rgba(255,255,255,0.9); font-size: 14px; font-weight: bold; margin-top: 5px;", 
        "TOTAL CASOS")
  )
})

output$kpi_confirmados_block <- renderUI({
  d <- datos_filtrados()
  conf <- sum(d[["tipo_de_caso"]] == "CONFIRMADO", na.rm = TRUE)
  total <- nrow(d)
  div(
    tags$i(class = "fas fa-check-circle", style = "font-size: 24px; color: #2ECC71; margin-bottom: 8px;"),
    div(style = "color: white; font-size: 28px; font-weight: bold;", 
        if(total == 0 || conf == 0) "SIN CASOS" else format(conf, big.mark = ",")),
    div(style = "color: rgba(255,255,255,0.9); font-size: 14px; font-weight: bold; margin-top: 5px;", 
        "CONFIRMADOS")
  )
})

output$kpi_fallecidos_block <- renderUI({
  d <- datos_filtrados()
  fall <- sum(!is.na(d[["fecha_defuncion"]]) & as.character(d[["fecha_defuncion"]]) != "", na.rm = TRUE)
  total <- nrow(d)
  div(
    tags$i(class = "fas fa-skull", style = "font-size: 24px; color: #E74C3C; margin-bottom: 8px;"),
    div(style = "color: white; font-size: 28px; font-weight: bold;", 
        if(total == 0 || fall == 0) "SIN CASOS" else format(fall, big.mark = ",")),
    div(style = "color: rgba(255,255,255,0.9); font-size: 14px; font-weight: bold; margin-top: 5px;", 
        "FALLECIDOS")
  )
})
    
    
    # ==========================================
# G1 - TABLA DE CALOR CON TODOS LOS 13 DIAGNÓSTICOS
# ==========================================
output$g1_heatmap <- DT::renderDataTable({
  d <- datos_filtrados_todos()
  if(is.null(d) || nrow(d) == 0) {
    df_vacio <- data.frame("SIN DATOS" = "No hay casos", check.names = FALSE)
    return(DT::datatable(df_vacio, options = list(pageLength = 10, dom = "t",
      language = list(info = "", emptyTable = "No hay casos")), rownames = FALSE, class = 'cell-border stripe compact'))
  }
  
  d <- d %>% mutate(region_red = paste0(region, " (", redes, ")"))
  
  lista_completa_13 <- c(
    "TOS FERINA",
    "HEPATITIS B", 
    "PAROTIDITIS SIN COMPLICACIONES",
    "PAROTIDITIS CON COMPLICACIONES",
    "VARICELA SIN COMPLICACIONES",
    "VARICELA CON OTRAS COMPLICACIONES",
    "DIFTERIA",
    "SARAMPIÓN",
    "RUBEOLA",
    "TETANOS",
    "FIEBRE AMARILLA SELVATICA",
    "PARÁLISIS FLÁCIDA AGUDA",
    "ESAVI - EVENTO ADVERSO POST VACUNAL"
  )
  
  d$diagnostico <- toupper(d$diagnostico)
  top_diag <- intersect(lista_completa_13, unique(d$diagnostico))
  
  if(!"ESAVI" %in% top_diag) {
    top_diag <- c(top_diag, "ESAVI")
  }
  
  if(length(top_diag) == 0) {
    top_diag <- unique(d$diagnostico)
  }
  
  top_reg <- d %>% 
    count(region_red, sort = TRUE) %>% 
    head(12) %>% 
    pull(region_red)
  
  d_tabla <- d %>% 
    filter(diagnostico %in% top_diag, region_red %in% top_reg) %>%
    count(region_red, diagnostico) %>% 
    pivot_wider(names_from = diagnostico, values_from = n, values_fill = 0)
  
  for(diag in lista_completa_13) {
    if(!diag %in% names(d_tabla)) {
      d_tabla[[diag]] <- 0
    }
  }
  
  d_tabla <- d_tabla[, c("region_red", lista_completa_13), drop = FALSE]
  names(d_tabla)[1] <- "REGIÓN (RED)"
  
  valores <- as.matrix(d_tabla[, -1])
  
  # 🔥 CORRECCIÓN: Manejar caso de valores vacíos
  max_val <- 0
  if(!is.null(valores) && length(valores) > 0 && any(!is.na(valores))) {
    max_val <- max(valores, na.rm = TRUE)
  }
  if(max_val == 0 || !is.finite(max_val)) max_val <- 1
  
  color_escala <- function(valor, maximo) {
    if(is.na(valor) || valor == 0) return("#FFFFFF")
    pct <- valor / maximo
    if(pct <= 0.25) return("#27AE60")
    if(pct <= 0.50) return("#F1C40F")
    if(pct <= 0.75) return("#E67E22")
    return("#E74C3C")
  }
  
  for (col in names(d_tabla)[-1]) {
    d_tabla[[col]] <- sapply(d_tabla[[col]], function(val) {
      color_fondo <- color_escala(val, max_val)
      color_texto <- ifelse(val > max_val * 0.5, "white", "#2C3E50")
      sprintf('<div style="background:%s;color:%s;padding:8px 12px;text-align:center;font-weight:bold;min-width:60px;">%s</div>',
              color_fondo, color_texto, val)
    })
  }
  
  DT::datatable(d_tabla,
    extensions = 'Buttons',
    options = list(
      pageLength = 15, 
      dom = "Btip",
      buttons = list(
        list(
          extend = 'excel', 
          text = '📊 EXCEL',
          className = 'btn-descarga-excel',
          filename = 'Tabla_Calor'
        ),
        list(
          extend = 'csv', 
          text = '📄 CSV',
          className = 'btn-descarga-csv',
          filename = 'Tabla_Calor'
        )
      ),
      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"), 
      scrollX = TRUE,
      columnDefs = list(
        list(targets = 0, className = "dt-left", width = "150px"),
        list(targets = "_all", className = "dt-center", width = "80px")
      ),
      autoWidth = FALSE,
      searching = TRUE,
      ordering = TRUE,
      lengthMenu = list(c(10, 15, 20, -1), c('10', '15', '20', 'Todos'))
    ),
    rownames = FALSE, 
    escape = FALSE, 
    class = 'cell-border stripe compact'
  )
})
    
    # ==========================================
    # BOTONES DE DESCARGA PARA G1
    # ==========================================
    output$descargar_excel_g1 <- downloadHandler(
      filename = function() { paste0("Tabla_Calor_", Sys.Date(), ".csv") },
      content = function(file) { 
        df_export <- datos_filtrados_todos()
        write.csv(df_export, file, row.names = FALSE, fileEncoding = "UTF-8") 
      }
    )
    
    output$descargar_csv_g1 <- downloadHandler(
      filename = function() { paste0("Tabla_Calor_", Sys.Date(), ".csv") },
      content = function(file) { 
        df_export <- datos_filtrados_todos()
        write.csv(df_export, file, row.names = FALSE, fileEncoding = "UTF-8") 
      }
    )
    
    # ==========================================
# HEATMAP TIA CON TODOS LOS DIAGNÓSTICOS Y COLORES PERSONALIZADOS
# ==========================================
output$heatmap_tia <- DT::renderDataTable({
  d <- datos_tia_filtrados()
  if(is.null(d) || nrow(d) == 0) {
    return(DT::datatable(data.frame(SIN_DATOS="No hay casos"), options=list(dom="t")))
  }
  
  columnas_tia <- grep("_tia$", names(d), value = TRUE)
  
  if(length(columnas_tia) == 0) {
    return(DT::datatable(data.frame(INFO="Sin columnas TIA"), options=list(dom="t")))
  }
  
  nombres_cortos <- c()
  for(col in columnas_tia) {
    nombre <- gsub("_tia$", "", col)
    nombre <- gsub("_", " ", nombre)
    nombre <- tools::toTitleCase(nombre)
    nombre <- gsub("Varicela Sin Complicaciones", "VARICELA S.C.", nombre)
    nombre <- gsub("Varicela Con Otras Complicaciones", "VARICELA C.C.", nombre)
    nombre <- gsub("Parotiditis Con Complicaciones", "PAROTIDITIS C.C.", nombre)
    nombre <- gsub("Paralisis Flacida Aguda", "PARALISIS FLAC.", nombre)
    nombre <- gsub("Fiebre Amarilla Selvatica", "FIEBRE AMARILLA", nombre)
    nombre <- gsub("Hepatitis B", "HEPATITIS B", nombre)
    nombre <- gsub("Tos Ferina", "TOS FERINA", nombre)
    nombre <- gsub("Parotiditis Sin Complicaciones", "PAROTIDITIS S.C.", nombre)
    nombre <- gsub("Difteria", "DIFTERIA", nombre)
    nombre <- gsub("Sarampion", "SARAMPIÓN", nombre)
    nombre <- gsub("Rubeola", "RUBEOLA", nombre)
    nombre <- gsub("Tetanos", "TETANOS", nombre)
    nombre <- gsub("Esavi", "ESAVI", nombre)
    nombres_cortos <- c(nombres_cortos, toupper(nombre))
  }
  
  columnas_mostrar <- c("red", intersect(columnas_tia, names(d)))
  d_out <- d[, columnas_mostrar, drop = FALSE]
  names(d_out)[1] <- "RED"
  
  for(i in seq_along(columnas_tia)) {
    if(columnas_tia[i] %in% names(d_out)) {
      names(d_out)[names(d_out) == columnas_tia[i]] <- nombres_cortos[i]
    }
  }
  
  valores <- d_out[, -1, drop = FALSE]
  
  # 🔥 CORRECCIÓN: Manejar caso de valores vacíos
  max_val <- 0
  if(!is.null(valores) && ncol(valores) > 0 && nrow(valores) > 0) {
    max_val <- max(sapply(valores, function(x) max(as.numeric(x), na.rm = TRUE)), na.rm = TRUE)
  }
  if(max_val == 0 || !is.finite(max_val)) max_val <- 1
  
  dt <- DT::datatable(
    d_out, 
    options = list(
      pageLength = 15,
      dom = "tip",
      scrollX = TRUE,
      autoWidth = TRUE,
      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")
    ),
    rownames = FALSE, 
    class = 'cell-border stripe compact'
  )
  
  for(col in names(d_out)[-1]) {
    color <- color_para_enf(col)
    dt <- dt %>% formatStyle(
      col,
      background = styleColorBar(c(0, max_val), color),
      backgroundSize = "100% 90%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    )
  }
  
  dt
})
    
    # ==========================================
    # FUNCIÓN PARA COLORES PERSONALIZADOS POR ENFERMEDAD
    # ==========================================
    color_para_enf <- function(nombre_columna) {
      paleta_colores <- c(
        "TOS FERINA" = "#E74C3C",
        "VARICELA S.C." = "#3498DB",
        "PAROTIDITIS S.C." = "#2ECC71",
        "HEPATITIS B" = "#F39C12",
        "DIFTERIA" = "#9B59B6",
        "SARAMPIÓN" = "#1ABC9C",
        "RUBEOLA" = "#E67E22",
        "TETANOS" = "#34495E",
        "FIEBRE AMARILLA" = "#F1C40F",
        "PARALISIS FLAC." = "#E74C3C",
        "VARICELA C.C." = "#2980B9",
        "PAROTIDITIS C.C." = "#27AE60",
        "ESAVI" = "#8E44AD"
      )
      
      for(nombre in names(paleta_colores)) {
        if(grepl(nombre, nombre_columna, ignore.case = TRUE)) {
          return(paleta_colores[[nombre]])
        }
      }
      return("#7F8C8D")
    }
    
   # ==========================================
    # MAPA TIA CON POPUP (ESTÁTICO + SCROLL)
    # ==========================================
    output$mapa_tia <- renderLeaflet({
      if(is.null(tabla_tia) || nrow(tabla_tia) == 0) {
        return(leaflet() %>% 
          addProviderTiles("CartoDB.Positron") %>% 
          setView(lng = -75.5, lat = -9.5, zoom = 5) %>%
          addControl("No hay casos disponibles", position = "topright"))
      }
      
      tia_data <- tabla_tia
      if(!"red" %in% names(tia_data)) {
        return(leaflet() %>% 
          addProviderTiles("CartoDB.Positron") %>% 
          setView(lng = -75.5, lat = -9.5, zoom = 5) %>%
          addControl("Datos sin columna 'red'", position = "topright"))
      }
      
      redes_seleccionadas <- procesar_seleccion(input$filtro_red, redes_lista)
      if(length(redes_seleccionadas) > 0 && length(redes_seleccionadas) < length(redes_lista)) {
        tia_data <- tia_data[tia_data[["red"]] %in% redes_seleccionadas, ]
      }
      
      diag_seleccionado <- input$filtro_diag
      es_todos_diag <- ("todos" %in% diag_seleccionado)
      
      diag_to_col <- list(
        "TOS FERINA" = "tos_ferina_tia",
        "VARICELA SIN COMPLICACIONES" = "varicela_sin_complicaciones_tia",
        "VARICELA CON OTRAS COMPLICACIONES" = "varicela_con_complicaciones_tia",
        "HEPATITIS B" = "hepatitis_b_tia",
        "PAROTIDITIS SIN COMPLICACIONES" = "parotiditis_sin_complicaciones_tia",
        "PAROTIDITIS CON COMPLICACIONES" = "parotiditis_complicaciones_tia",
        "DIFTERIA" = "difteria_tia",
        "SARAMPION" = "sarampion_tia",
        "RUBEOLA" = "rubeola_tia",
        "TETANOS" = "tetanos_tia",
        "FIEBRE AMARILLA SELVATICA" = "fiebre_amarilla_tia",
        "PARALISIS FLACIDA AGUDA" = "paralisis_flacida_tia",
        "ESAVI - EVENTO ADVERSO POST VACUNAL" = "esavi_tia"
      )
      
      coords_ref <- data.frame(
        region = c("AMAZONAS","ANCASH","APURIMAC","AREQUIPA","AYACUCHO","CAJAMARCA",
                   "CALLAO","CUSCO","HUANCAVELICA","HUANUCO","ICA","JUNIN","LA LIBERTAD",
                   "LAMBAYEQUE","LIMA","LORETO","MADRE DE DIOS","MOQUEGUA","PASCO",
                   "PIURA","PUNO","SAN MARTIN","TACNA","TUMBES","UCAYALI"),
        lat = c(-5.2,-9.5,-13.9,-16.4,-13.2,-7.2,-12.1,-13.5,-12.8,-9.9,-14.1,
                -11.2,-8.1,-6.7,-12.0,-4.0,-11.0,-17.0,-10.7,-5.2,-15.8,-7.2,
                -18.0,-3.6,-8.4),
        lon = c(-78.5,-77.6,-72.9,-71.5,-74.2,-78.5,-77.0,-71.9,-74.9,-76.2,
                -75.7,-75.5,-78.5,-79.9,-77.0,-73.5,-70.5,-70.9,-76.2,-80.7,
                -70.0,-76.8,-70.3,-80.5,-74.5),
        stringsAsFactors = FALSE
      )
      
      if("region" %in% names(tia_data)) {
        tia_data <- tia_data %>% left_join(coords_ref, by = "region")
      }
      
      if(!"lat" %in% names(tia_data)) {
        tia_data$lat <- -9.5
        tia_data$lon <- -75.5
      }
      tia_data$lat[is.na(tia_data$lat)] <- -9.5
      tia_data$lon[is.na(tia_data$lon)] <- -75.5
      
      set.seed(123)
      tia_data$lat <- tia_data$lat + runif(nrow(tia_data), -0.08, 0.08)
      tia_data$lon <- tia_data$lon + runif(nrow(tia_data), -0.08, 0.08)
      
      cols_tia <- grep("_tia$", names(tia_data), value = TRUE)
      
      for(col in cols_tia) {
        tia_data[[col]] <- as.numeric(as.character(tia_data[[col]]))
        tia_data[[col]][is.na(tia_data[[col]])] <- 0
      }
      
      if(length(cols_tia) > 0) {
        tia_data$tia_max <- apply(tia_data[, cols_tia, drop = FALSE], 1, function(x) {
          max(x, na.rm = TRUE)
        })
      } else {
        tia_data$tia_max <- 0
      }
      tia_data$tia_max[!is.finite(tia_data$tia_max)] <- 0
      
      max_tia <- max(tia_data$tia_max, na.rm = TRUE)
      if(max_tia == 0 || !is.finite(max_tia)) max_tia <- 1
      
      pal <- colorBin(
        palette = c("#27AE60", "#F1C40F", "#E67E22", "#E74C3C"),
        domain = c(0, max_tia),
        bins = c(0, max_tia * 0.25, max_tia * 0.5, max_tia * 0.75, max_tia + 1),
        na.color = "#040404"
      )
      
      nombres_enfermedades <- list(
        "tos_ferina_tia" = "TOS FERINA",
        "varicela_sin_complicaciones_tia" = "VARICELA S.C.",
        "varicela_con_complicaciones_tia" = "VARICELA C.C.",
        "hepatitis_b_tia" = "HEPATITIS B",
        "parotiditis_sin_complicaciones_tia" = "PAROTIDITIS S.C.",
        "parotiditis_complicaciones_tia" = "PAROTIDITIS C.C.",
        "difteria_tia" = "DIFTERIA",
        "sarampion_tia" = "SARAMPIÓN",
        "rubeola_tia" = "RUBEOLA",
        "tetanos_tia" = "TÉTANOS",
        "fiebre_amarilla_tia" = "FIEBRE AMARILLA",
        "paralisis_flacida_tia" = "PARÁLISIS FLÁCIDA",
        "esavi_tia" = "ESAVI"
      )
      
      # 🔥 FUNCIÓN PARA CONSTRUIR POPUP CON SCROLL
      construir_popup <- function(fila) {
        color_fondo <- pal(fila[["tia_max"]])
        filas_tabla <- ""
        
        for(col in cols_tia) {
          if(col %in% names(nombres_enfermedades)) {
            nombre <- nombres_enfermedades[[col]]
          } else {
            nombre <- gsub("_tia$", "", col)
            nombre <- gsub("_", " ", nombre)
            nombre <- tools::toTitleCase(nombre)
          }
          
          valor <- fila[[col]]
          if(is.na(valor)) valor <- 0
          
          columna_diag <- NULL
          for(diag in names(diag_to_col)) {
            if(diag_to_col[[diag]] == col) {
              columna_diag <- diag
              break
            }
          }
          
          destacar <- FALSE
          if(!is.null(columna_diag) && valor > 0) {
            if(es_todos_diag || columna_diag %in% diag_seleccionado || toupper(columna_diag) %in% toupper(diag_seleccionado)) {
              destacar <- TRUE
            }
          }
          
          if(destacar) {
            filas_tabla <- paste0(
              filas_tabla,
              sprintf(
                '<tr style="background:#FFF8E1;border-left:3px solid #F39C12;">
                  <td style="padding:4px 8px;font-weight:bold;color:#E67E22;">★ %s:</td>
                  <td style="text-align:right;font-weight:bold;padding:4px 8px;color:#E74C3C;font-size:13px;">%.2f</td>
                </tr>',
                nombre, valor
              )
            )
          } else if(valor > 0) {
            filas_tabla <- paste0(
              filas_tabla,
              sprintf(
                '<tr>
                  <td style="padding:2px 8px;">%s:</td>
                  <td style="text-align:right;font-weight:bold;padding:2px 8px;">%.2f</td>
                </tr>',
                nombre, valor
              )
            )
          } else {
            filas_tabla <- paste0(
              filas_tabla,
              sprintf(
                '<tr style="opacity:0.4;">
                  <td style="padding:2px 8px;color:#626869;">%s:</td>
                  <td style="text-align:right;padding:2px 8px;color:#626869;">%.2f</td>
                </tr>',
                nombre, valor
              )
            )
          }
        }
        
        # 🔥 POPUP CON SCROLL Y TAMAÑO FIJO
        sprintf(
          '<div style="font-family:Segoe UI;width:320px;background:white;border-radius:8px;overflow:hidden;box-shadow:0 4px 12px rgba(0,0,0,0.2);">
            <div style="background:%s;color:white;padding:10px 15px;font-size:14px;font-weight:bold;">
              🏥 %s
            </div>
            <div style="padding:10px 15px;max-height:350px;overflow-y:auto;">
              <table style="width:100%%;font-size:11px;border-collapse:collapse;">
                %s
                <tr><td colspan="2"><hr style="margin:6px 0;border-color:#ECF0F1;"></td></tr>
                <tr>
                  <td style="padding:2px 8px;font-weight:bold;">📊 TIA Máxima:</td>
                  <td style="text-align:right;font-size:16px;font-weight:bold;color:%s;padding:2px 8px;">%.2f</td>
                </tr>
              </table>
            </div>
          </div>',
          color_fondo,
          toupper(fila[["red"]]),
          filas_tabla,
          color_fondo,
          round(fila[["tia_max"]], 2)
        )
      }
      
      # 🔥 CREAR POPUPS EN LUGAR DE TOOLTIPS
      tia_data$popup_content <- character(nrow(tia_data))
      for(i in 1:nrow(tia_data)) {
        fila_lista <- as.list(tia_data[i, ])
        tia_data$popup_content[i] <- construir_popup(fila_lista)
      }
      
      leaflet(tia_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -75.5, lat = -9.5, zoom = 5) %>%
        addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          radius = ~sqrt(tia_max + 1) * 6,
          color = ~pal(tia_max),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 2,
          popup = ~lapply(popup_content, HTML),
          popupOptions = popupOptions(
            maxWidth = 350,
            minWidth = 300,
            className = "tia-popup"
          ),
          label = ~toupper(red),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "background" = "rgba(255,255,255,0.9)",
              "border" = "1px solid #DDD",
              "border-radius" = "4px",
              "padding" = "2px 6px",
              "font-size" = "9px",
              "font-weight" = "bold"
            )
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~tia_max,
          title = "T.I.A.",
          opacity = 1,
          labFormat = function(type, cuts, p) {
            n <- length(cuts)
            labels <- c()
            for(i in 1:(n-1)) {
              labels <- c(labels, paste0(
                format(round(cuts[i], 2), nsmall = 2),
                " - ",
                format(round(cuts[i+1] - 0.01, 2), nsmall = 2)
              ))
            }
            return(labels)
          }
        )
    })
    


# ==========================================
# TENDENCIA MENSUAL (VERSIÓN FINAL - SIN WARNINGS)
# ==========================================
output$tendencia_mensual <- renderPlotly({
  d <- datos_filtrados()
  if(nrow(d) == 0) return(plotly_vacio())
  
  # Obtener top 4 diagnósticos
  top_diag <- d %>% 
    count(diagnostico, sort = TRUE) %>% 
    head(4) %>% 
    pull(diagnostico)
  
  # Preparar datos de tendencia
  d_tend <- d %>% 
    filter(diagnostico %in% top_diag) %>% 
    count(mes, diagnostico) %>% 
    mutate(mes_nombre = meses_abrev[as.character(mes)])
  
  # Crear dataframe con todos los meses para evitar huecos
  todos_meses <- data.frame(
    mes = 1:12, 
    mes_nombre = meses_abrev[as.character(1:12)], 
    stringsAsFactors = FALSE
  )
  
  # Paleta de colores mejorada
  paleta <- c("#3498DB", "#E74C3C", "#2ECC71", "#F39C12")
  names(paleta) <- top_diag
  
  # Crear gráfico base
  p <- plot_ly()
  
  # Agregar trazas para cada diagnóstico
  for(i in seq_along(top_diag)) {
    diag_name <- top_diag[i]
    
    # Filtrar y completar datos
    d_diag <- d_tend %>% 
      filter(diagnostico == diag_name)
    
    # Unir con todos los meses para tener series completas
    d_diag <- todos_meses %>% 
      left_join(d_diag, by = c("mes", "mes_nombre"))
    
    # Rellenar valores faltantes
    d_diag$n[is.na(d_diag$n)] <- 0
    d_diag$diagnostico[is.na(d_diag$diagnostico)] <- diag_name
    
    # Agregar barra con tooltip individual
    p <- p %>% 
      add_trace(
        data = d_diag, 
        x = ~mes_nombre, 
        y = ~n, 
        type = "bar",
        marker = list(
          color = paleta[i], 
          opacity = 0.9,
          line = list(
            color = "white",
            width = 2
          )
        ),
        name = diag_name,
        hovertext = ~paste0(
          "<b>", mes_nombre, "</b><br>",
          "Casos: ", n
        ),
        hoverinfo = "text",
        hoverlabel = list(
          bgcolor = paleta[i],
          font = list(color = "white", size = 12)
        ),
        width = 0.15
      )
  }
  
  # Calcular el máximo para establecer límites
  max_valor <- max(d_tend$n, na.rm = TRUE)
  
  # Establecer límite superior con margen
  if(max_valor == 0) {
    limite_superior <- 10
  } else if(max_valor <= 10) {
    limite_superior <- 15
  } else if(max_valor <= 20) {
    limite_superior <- ceiling(max_valor * 1.3)
  } else if(max_valor <= 50) {
    limite_superior <- ceiling(max_valor * 1.2)
  } else {
    limite_superior <- ceiling(max_valor * 1.15)
  }
  
  # Configurar ticks del eje Y
  if(limite_superior <= 10) {
    ticks_y <- seq(0, limite_superior, by = 1)
  } else if(limite_superior <= 20) {
    ticks_y <- seq(0, limite_superior, by = 2)
  } else if(limite_superior <= 50) {
    ticks_y <- seq(0, limite_superior, by = 5)
  } else if(limite_superior <= 100) {
    ticks_y <- seq(0, limite_superior, by = 10)
  } else {
    ticks_y <- seq(0, limite_superior, by = 20)
  }
  
  # Layout FINAL - SIN bargroupgap
  p %>% 
    layout(
      xaxis = list(
        title = list(
          text = "MES", 
          font = list(size = 13, color = "#2C3E50", family = "Arial")
        ),
        categoryorder = "array", 
        categoryarray = unname(meses_abrev[as.character(1:12)]),
        gridcolor = "#ECF0F1",
        gridwidth = 0.5,
        tickfont = list(size = 10),
        tickangle = 0
      ),
      yaxis = list(
        title = list(
          text = "CASOS", 
          font = list(size = 13, color = "#2C3E50", family = "Arial")
        ),
        range = c(0, limite_superior),
        tickmode = "array",
        tickvals = ticks_y,
        ticktext = as.character(ticks_y),
        gridcolor = "#ECF0F1",
        gridwidth = 0.5,
        zeroline = TRUE,
        zerolinecolor = "#BDC3C7",
        zerolinewidth = 1.5,
        tickfont = list(size = 10)
      ),
      legend = list(
        orientation = "h",
        y = -0.2,
        x = 0.5,
        xanchor = "center",
        font = list(size = 11, color = "#2C3E50"),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "#ECF0F1",
        borderwidth = 1
      ),
      margin = list(
        l = 60, 
        r = 40, 
        t = 20, 
        b = 80,
        pad = 5
      ),
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      barmode = "group",
      bargap = 0.15,
      hovermode = "closest",
      showlegend = TRUE
    ) %>%
    config(
      displayModeBar = TRUE,
      responsive = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      toImageButtonOptions = list(
        format = "png",
        filename = "tendencia_mensual",
        width = 1200,
        height = 600
      )
    )
})
    


# ==========================================
# RANKING TIA POR DIAGNÓSTICO (CORREGIDO - SIN WARNINGS)
# ==========================================
output$ranking_tia <- renderPlotly({
  # Obtener datos filtrados
  d <- datos_tia_filtrados()
  if(is.null(d) || nrow(d) == 0) return(plotly_vacio())
  
  # ==========================================
  # 🔥 CORRECCIÓN: USAR EL FILTRO CORRECTO
  # ==========================================
  # Obtener diagnóstico(s) seleccionado(s) del filtro DIAGNÓSTICO
  diagnostico_seleccionado <- input$filtro_diag
  
  # Identificar columnas TIA
  cols_tia <- grep("_tia$", names(d), value = TRUE)
  cols_tia <- cols_tia[!grepl("esavi", cols_tia, ignore.case = TRUE)]
  
  # 🔥 CORRECCIÓN: Verificar que hay columnas TIA
  if(length(cols_tia) == 0) {
    return(plotly_vacio())
  }
  
  # 🔥 CORRECCIÓN: Calcular TIA máxima con manejo de errores
  datos_diag <- data.frame(
    diagnostico_original = gsub("_tia$", "", cols_tia),
    tia_max = apply(d[, cols_tia, drop = FALSE], 2, function(x) {
      # Convertir a numérico y manejar NAs
      vals <- as.numeric(x)
      # Si todos son NA, devolver 0
      if(all(is.na(vals))) return(0)
      # Si hay valores válidos, devolver el máximo
      return(max(vals, na.rm = TRUE))
    })
  )
  
  # 🔥 CORRECCIÓN: Reemplazar valores -Inf o NaN con 0
  datos_diag$tia_max[!is.finite(datos_diag$tia_max)] <- 0
  
  # Limpiar nombres para visualización
  datos_diag$diagnostico <- datos_diag$diagnostico_original
  datos_diag$diagnostico <- gsub("_", " ", datos_diag$diagnostico)
  datos_diag$diagnostico <- toupper(datos_diag$diagnostico)
  
  # Abreviar nombres largos
  datos_diag$diagnostico <- gsub("VARICELA SIN COMPLICACIONES", "VARICELA S.C.", datos_diag$diagnostico)
  datos_diag$diagnostico <- gsub("VARICELA CON OTRAS COMPLICACIONES", "VARICELA C.C.", datos_diag$diagnostico)
  datos_diag$diagnostico <- gsub("PAROTIDITIS CON COMPLICACIONES", "PAROTIDITIS C.C.", datos_diag$diagnostico)
  datos_diag$diagnostico <- gsub("PARALISIS FLACIDA AGUDA", "PARALISIS FLAC.", datos_diag$diagnostico)
  datos_diag$diagnostico <- gsub("FIEBRE AMARILLA SELVATICA", "FIEBRE AMARILLA", datos_diag$diagnostico)
  
  # 🔥 CAMBIADO: Renombrar Parotiditis simple a S.C.
  datos_diag$diagnostico <- gsub("PAROTIDITIS SIN COMPLICACIONES", "PAROTIDITIS S.C.", datos_diag$diagnostico)
  
  # Ordenar de mayor a menor TIA
  datos_diag <- datos_diag %>% arrange(desc(tia_max))
  
  # ==========================================
  # 🔥 CORRECCIÓN: COMPARAR CON NOMBRES EN MAYÚSCULAS
  # ==========================================
  
  # Determinar si hay filtro activo
  hay_filtro <- !is.null(diagnostico_seleccionado) && 
                length(diagnostico_seleccionado) > 0 && 
                !("todos" %in% diagnostico_seleccionado)
  
  # Vector lógico: TRUE si el diagnóstico está seleccionado
  if(hay_filtro) {
    # 🔥 Convertir a mayúsculas para comparación
    diag_sel_upper <- toupper(diagnostico_seleccionado)
    
    # Buscar coincidencias (comparar en mayúsculas)
    datos_diag$seleccionado <- toupper(datos_diag$diagnostico_original) %in% diag_sel_upper |
                               datos_diag$diagnostico %in% diag_sel_upper
  } else {
    # Sin filtro, todos aparecen normales
    datos_diag$seleccionado <- FALSE
  }
  
  # ==========================================
  # CONFIGURACIÓN VISUAL: DESTACAR VS ATENUAR
  # ==========================================
  
  # Colores base según TIA (semáforo)
  datos_diag$color_base <- case_when(
    datos_diag$tia_max == 0 ~ "#BDC3C7",      # Gris (sin casos)
    datos_diag$tia_max <= 0.5 ~ "#2ECC71",    # Verde (bajo)
    datos_diag$tia_max <= 1.0 ~ "#F1C40F",    # Amarillo (moderado)
    datos_diag$tia_max <= 2.0 ~ "#E67E22",    # Naranja (alto)
    TRUE ~ "#E74C3C"                           # Rojo (muy alto)
  )
  
  # Aplicar opacidad según selección
  datos_diag <- datos_diag %>%
    mutate(
      # Opacidad: 1.0 para seleccionados, 0.15 para no seleccionados
      opacidad = case_when(
        seleccionado & hay_filtro ~ 1.0,
        !hay_filtro ~ 0.9,
        TRUE ~ 0.15  # 🔥 Muy transparente para no seleccionados
      ),
      
      # Ancho del borde: grueso para seleccionados
      ancho_borde = ifelse(seleccionado & hay_filtro, 3.5, 0.5),
      
      # Color del borde: dorado para seleccionados, gris para el resto
      color_borde = ifelse(seleccionado & hay_filtro, "#FFD700", "rgba(0,0,0,0.1)")
    )
  
  # Convertir colores a rgba con opacidad
  datos_diag$color_final <- mapply(function(hex_color, opacidad) {
    rgb_vals <- col2rgb(hex_color)
    sprintf("rgba(%d, %d, %d, %.2f)", 
            rgb_vals[1], rgb_vals[2], rgb_vals[3], opacidad)
  }, datos_diag$color_base, datos_diag$opacidad)
  
  # ==========================================
  # TEXTO ENRIQUECIDO PARA HOVER
  # ==========================================
  
  datos_diag$hover_text <- ifelse(
    datos_diag$seleccionado & hay_filtro,
    sprintf("<b>⭐ %s</b><br><b>TIA: %.2f</b><br><i>Diagnóstico seleccionado</i>", 
            datos_diag$diagnostico, datos_diag$tia_max),
    sprintf("<span style='color:gray'>%s</span><br>TIA: %.2f", 
            datos_diag$diagnostico, datos_diag$tia_max)
  )
  
  # ==========================================
  # CREAR GRÁFICO
  # ==========================================
  
  p <- plot_ly(
    data = datos_diag,
    x = ~tia_max,
    y = ~reorder(diagnostico, tia_max),
    type = "bar",
    orientation = "h",
    marker = list(
      color = ~color_final,
      line = list(
        color = ~color_borde,
        width = ~ancho_borde
      )
    ),
    hovertext = ~hover_text,
    hoverinfo = "text",
    showlegend = FALSE,
    width = 0.7
  )
  
  # ==========================================
  # AGREGAR MARCADORES PARA DIAGNÓSTICOS SELECCIONADOS
  # ==========================================
  
  if(hay_filtro && sum(datos_diag$seleccionado) > 0) {
    datos_seleccionados <- datos_diag[datos_diag$seleccionado, ]
    
    p <- p %>%
      add_trace(
        data = datos_seleccionados,
        x = ~tia_max,
        y = ~reorder(diagnostico, tia_max),
        type = "scatter",
        mode = "markers",
        marker = list(
          symbol = "diamond",
          size = 12,
          color = "rgba(255, 215, 0, 1)",  # Dorado brillante
          line = list(
            color = "#2C3E50",
            width = 2
          )
        ),
        hoverinfo = "skip",
        showlegend = FALSE
      )
  }
  
  # ==========================================
  # CONFIGURACIÓN DEL LAYOUT (SIN bargroupgap)
  # ==========================================
  
  p %>% layout(
    autosize = TRUE,
    xaxis = list(
      title = list(
        text = "T.I.A. (Tasa de Incidencia Acumulada)", 
        font = list(size = 13, color = "#2C3E50", family = "Arial")
      ),
      rangemode = "nonnegative",
      gridcolor = "#ECF0F1",
      gridwidth = 0.5,
      zerolinecolor = "#BDC3C7",
      zerolinewidth = 1,
      tickfont = list(size = 10)
    ),
    yaxis = list(
      title = list(
        text = "Diagnósticos", 
        font = list(size = 13, color = "#2C3E50", family = "Arial")
      ),
      gridcolor = "#ECF0F1",
      gridwidth = 0.5,
      zeroline = FALSE,
      tickfont = list(size = 10),
      categoryorder = "array",
      categoryarray = rev(datos_diag$diagnostico)
    ),
    margin = list(l = 150, r = 60, t = 20, b = 60, pad = 5),
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    hovermode = "y unified",
    showlegend = FALSE,
    bargap = 0.3
  ) %>%
  config(
    displayModeBar = TRUE,
    responsive = TRUE,
    toImageButtonOptions = list(
      format = "png",
      width = 1200,
      height = 800
    )
  )
})



    # ==========================================
    # TOP ENFERMEDADES
    # ==========================================
    output$g2_barras_enfermedades <- renderPlotly({
      d <- datos_filtrados() %>% group_by(diagnostico) %>% summarise(total = n()) %>% arrange(desc(total)) %>% head(10)
      if(nrow(d) == 0) return(plotly_vacio())
      max_val <- max(d$total)
      plot_ly(d, x = ~total, y = ~reorder(diagnostico, total), type = "bar", orientation = "h",
        marker = list(color = color_anio()), text = ~as.character(total), textposition = "outside",
        hovertext = ~paste0(diagnostico, ": ", total), hoverinfo = "text") %>%
        layout(
          xaxis = list(title = "Casos", range = c(0, max_val * 1.15)),
          yaxis = list(title = ""),
          margin = list(l = 200, r = 40, t = 20, b = 40),
          paper_bgcolor = "white", plot_bgcolor = "white"
        )
    })
    
    # ==========================================
    # TABLA DE CASOS
    # ==========================================
    output$g6_tabla_casos <- DT::renderDataTable({
      df <- datos_filtrados() %>% arrange(desc(fecha_de_inicio)) %>% head(10)
      if(nrow(df) == 0) {
        return(DT::datatable(data.frame("Sin casos" = "No hay casos"), options = list(dom = "t"),
          rownames = FALSE, class = 'cell-border stripe compact'))
      }
      cols_mostrar <- c("id_caso", "diagnostico", "tipo_de_caso", "fecha_de_inicio", "redes", "grupos_de_edad", "sexo")
      cols_presentes <- intersect(cols_mostrar, names(df))
      df <- df[, cols_presentes, drop = FALSE]
      if("fecha_de_inicio" %in% names(df)) {
        df[["fecha_de_inicio"]] <- format(df[["fecha_de_inicio"]], "%d/%m/%Y")
      }
      nombres <- c("id_caso" = "ID Caso", "diagnostico" = "Diagnóstico", "tipo_de_caso" = "Tipo de Caso",
                   "fecha_de_inicio" = "Fecha de Inicio", "redes" = "Red", "grupos_de_edad" = "Grupo de Edad",
                   "sexo" = "Sexo")
      names(df) <- nombres[names(df)]
      DT::datatable(df, options = list(pageLength = 10, dom = "t",
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE, class = 'cell-border stripe compact')
    })
    
    output$descargar_excel <- downloadHandler(
      filename = function() { paste0("Casos_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(datos_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    
    output$descargar_csv <- downloadHandler(
      filename = function() { paste0("Casos_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(datos_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    
# ==========================================
# FUNCIÓN PARA GRÁFICOS VACÍOS (SIN WARNINGS)
# ==========================================
plotly_vacio <- function() {
  plot_ly() %>%
    add_annotations(
      text = "Sin casos para mostrar",
      x = 0.5,
      y = 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(size = 16, color = "#7F8C8D")
    ) %>%
    layout(
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        range = c(0, 1)
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        range = c(0, 1)
      )
    ) %>%
    config(displayModeBar = FALSE) %>%
    add_trace(
      x = 0.5,
      y = 0.5,
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0),
      hoverinfo = "none",
      showlegend = FALSE
    )
}
    
  })
}

# ==========================================
# FUNCIONES DE TRADUCCIÓN DIAGNÓSTICO (CORREGIDAS)
# ==========================================

# TRADUCCIÓN: CSV -> TEXTO VISUAL
# Convierte nombres del CSV a nombres para mostrar en la interfaz
traducir_diagnostico_visual <- function(diagnostico) {
  # Mapeo de CSV a nombre VISUAL (para mostrar en la interfaz)
  mapa_visual <- c(
    "PAROTIDITIS SIN COMPLICACIONES" = "PAROTIDITIS SIN COMPLICACIONES",  # 🔥 CORREGIDO: Parotiditis simple -> S.C.
    "PAROTIDITIS CON COMPLICACIONES" = "PAROTIDITIS CON COMPLICACIONES",
    "TOS FERINA" = "TOS FERINA",
    "HEPATITIS B" = "HEPATITIS B",
    "VARICELA SIN COMPLICACIONES" = "VARICELA SIN COMPLICACIONES",
    "VARICELA CON OTRAS COMPLICACIONES" = "VARICELA CON OTRAS COMPLICACIONES",
    "DIFTERIA" = "DIFTERIA",
    "SARAMPION" = "SARAMPIÓN",
    "RUBEOLA" = "RUBEOLA",
    "TETANOS" = "TETANOS",
    "FIEBRE AMARILLA SELVATICA" = "FIEBRE AMARILLA SELVATICA",
    "PARALISIS FLACIDA AGUDA" = "PARÁLISIS FLÁCIDA AGUDA",
    "ESAVI - EVENTO ADVERSO POST VACUNAL" = "ESAVI - EVENTO ADVERSO POST VACUNAL"
  )
  
  # Buscar el diagnóstico en mayúsculas
  clave <- toupper(trimws(diagnostico))
  resultado <- mapa_visual[clave]
  
  # Si no encuentra, devolver el original
  if (is.na(resultado)) {
    return(diagnostico)
  }
  return(resultado)
}

# TRADUCCIÓN INVERSA: TEXTO VISUAL -> CLAVE CSV
# Convierte nombres de la interfaz a nombres del CSV para filtrar datos
traducir_diagnostico_csv <- function(diagnostico_visual) {
  # Mapeo de TEXTO VISUAL a CSV (para filtrar datos)
  mapa_csv <- c(
    "PAROTIDITIS SIN COMPLICACIONES" = "PAROTIDITIS SIN COMPLICACIONES",  # 🔥 CORREGIDO: Visual S.C. -> CSV simple
    "PAROTIDITIS CON COMPLICACIONES" = "PAROTIDITIS CON COMPLICACIONES",
    "TOS FERINA" = "TOS FERINA",
    "HEPATITIS B" = "HEPATITIS B",
    "VARICELA SIN COMPLICACIONES" = "VARICELA SIN COMPLICACIONES",
    "VARICELA CON OTRAS COMPLICACIONES" = "VARICELA CON OTRAS COMPLICACIONES",
    "DIFTERIA" = "DIFTERIA",
    "SARAMPIÓN" = "SARAMPION",  # 🔥 CORREGIDO: Con tilde -> Sin tilde
    "RUBEOLA" = "RUBEOLA",
    "TETANOS" = "TETANOS",
    "FIEBRE AMARILLA SELVATICA" = "FIEBRE AMARILLA SELVATICA",
    "PARÁLISIS FLÁCIDA AGUDA" = "PARALISIS FLACIDA AGUDA",  # 🔥 CORREGIDO: Con tildes -> Sin tildes
    "ESAVI - EVENTO ADVERSO POST VACUNAL" = "ESAVI - EVENTO ADVERSO POST VACUNAL"
  )
  
  # Buscar en mayúsculas
  clave <- toupper(trimws(diagnostico_visual))
  resultado <- mapa_csv[clave]
  
  # Si no encuentra, devolver el original
  if (is.na(resultado)) {
    return(diagnostico_visual)
  }
  return(resultado)
}

# COMPARACIÓN SEGURA: Ignorar diferencias de nombre
# Compara dos diagnósticos ignorando diferencias menores
comparar_diagnosticos <- function(diag1, diag2) {
  normalizar <- function(d) {
    d <- toupper(trimws(d))
    # Normalizar Parotiditis
    if (d == "PAROTIDITIS SIN COMPLICACIONES" || d == "PAROTIDITIS" || d == "PAROTIDITIS S.C.") {
      return("PAROTIDITIS SIN COMPLICACIONES")  # Todos se normalizan a "PAROTIDITIS"
    }
    # Normalizar Sarampión
    if (d == "SARAMPIÓN" || d == "SARAMPION") {
      return("SARAMPION")
    }
    # Normalizar Parálisis Flácida
    if (d == "PARÁLISIS FLÁCIDA AGUDA" || d == "PARALISIS FLACIDA AGUDA") {
      return("PARALISIS FLACIDA AGUDA")
    }
    return(d)
  }
  return(normalizar(diag1) == normalizar(diag2))
}