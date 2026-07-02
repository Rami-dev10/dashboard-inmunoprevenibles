# ============================================================
# MÓDULO: PERFIL EPIDEMIOLÓGICO (PESTAÑA 2)
# OIIS - Oficina de Inteligencia e Información en Salud
# VERSIÓN v11 - Color Badge + Títulos dinámicos
# ============================================================

# ==========================================
# UI DEL MÓDULO
# ==========================================
mod_perfil_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ==========================================
    # FILTROS EN BARRA HORIZONTAL
    # ==========================================
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
              actionButton(ns("borrar_filtros"), "BORRAR FILTROS",
                class = "btn-borrar", style = "width: 100%; margin-top: 5px;")
            )
          )
        )
      )
    ),
    
    fluidRow(column(12, uiOutput(ns("titulo_completo")))),
    
    # ==========================================
    # 🆕 KPI CARDS DE RESUMEN (5 KPIs)
    # ==========================================
    fluidRow(
      column(2,
        valueBoxOutput(ns("kpi_total"), width = 12)
      ),
      column(2,
        valueBoxOutput(ns("kpi_confirmados_pct"), width = 12)
      ),
      column(3,
        valueBoxOutput(ns("kpi_distribucion_tipos"), width = 12)
      ),
      column(3,
        valueBoxOutput(ns("kpi_grupo_edad_top"), width = 12)
      ),
      column(2,
        valueBoxOutput(ns("kpi_mediana_edad"), width = 12)
      )
    ),
    
    # ==========================================
    # 🆕 ALERTA N PEQUEÑO
    # ==========================================
    fluidRow(
      column(12,
        uiOutput(ns("alerta_n_pequeno"))
      )
    ),
    
    # FILA 1: PIRÁMIDE + CURSO DE VIDA
    fluidRow(
      box(
        title = div(uiOutput(ns("titulo_g1")),  # 🔥 CAMBIADO a uiOutput
          tags$button(class = "btn-fullscreen",
            onclick = paste0("var el = document.getElementById('", ns("g1_container"), "');",
              "if(el.requestFullscreen) { el.requestFullscreen(); }"),
            tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 6,
        div(id = ns("g1_container"), style = "width: 100%; min-height: 450px;",
          plotlyOutput(ns("g1_piramide"), height = 420))
      ),
      box(
        title = div(uiOutput(ns("titulo_g2")),  # 🔥 CAMBIADO a uiOutput
          tags$button(class = "btn-fullscreen",
            onclick = paste0("var el = document.getElementById('", ns("g2_container"), "');",
              "if(el.requestFullscreen) { el.requestFullscreen(); }"),
            tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 6,
        div(id = ns("g2_container"), style = "width: 100%; min-height: 450px;",
          plotlyOutput(ns("g2_curso_vida"), height = 420))
      )
    ),
    
    # FILA 2: MEDIANA POR ENFERMEDAD
    fluidRow(
      box(
        title = div(uiOutput(ns("titulo_g3")),  # 🔥 CAMBIADO a uiOutput
          tags$button(class = "btn-fullscreen",
            onclick = paste0("var el = document.getElementById('", ns("g3_container"), "');",
              "if(el.requestFullscreen) { el.requestFullscreen(); }"),
            tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 12,
        div(id = ns("g3_container"), style = "width: 100%; min-height: 400px;",
          plotlyOutput(ns("g3_mediana_edad"), height = 400))
      )
    ),
    
    # FILA 3: TIPO DE CASO x CURSO DE VIDA + ESTACIONALIDAD
    fluidRow(
      box(
        title = div(uiOutput(ns("titulo_g4")),  # 🔥 CAMBIADO a uiOutput
          tags$button(class = "btn-fullscreen",
            onclick = paste0("var el = document.getElementById('", ns("g4_container"), "');",
              "if(el.requestFullscreen) { el.requestFullscreen(); }"),
            tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 6,
        div(id = ns("g4_container"), style = "width: 100%; min-height: 400px;",
          plotlyOutput(ns("g4_tipo_curso"), height = 400))
      ),
      box(
        title = div(uiOutput(ns("titulo_g5")),  # 🔥 CAMBIADO a uiOutput
          tags$button(class = "btn-fullscreen",
            onclick = paste0("var el = document.getElementById('", ns("g5_container"), "');",
              "if(el.requestFullscreen) { el.requestFullscreen(); }"),
            tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 6,
        div(id = ns("g5_container"), style = "width: 100%; min-height: 400px;",
          plotlyOutput(ns("g5_mensual"), height = 400))
      )
    ),
    
    # ==========================================
    # 🗑️ TABLA DEMOGRÁFICA ELIMINADA
    # ==========================================
    
    fluidRow(
      column(12,
        div(class = "footer-oiis",
          tags$img(src = "OIIS.png", style = "height: 45px; margin-bottom: 10px;"),
          tags$p("Elaborado por: Oficina de Inteligencia e Información Sanitaria OIIS",
                 style = "font-weight: bold; margin: 5px 0;"),
          tags$p("Información preliminar sujeta a modificación",
                 style = "font-style: italic; color: #7F8C8D;"),
          tags$hr(style = "border-color: #ECF0F1; margin: 10px 40px;"),
          tags$p(
            tags$i(class = "fas fa-info-circle", style = "color: #3498DB; margin-right: 5px;"),
            "Población asegurada de referencia: marzo 2026 (datos del 1 al 31 de marzo de 2026).",
            style = "font-size: 11px; color: #7F8C8D;"
          )
        )
      )
    )
  )
}

# ==========================================
# SERVER DEL MÓDULO
# ==========================================
mod_perfil_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # FUNCIÓN PARA PROCESAR SELECCIONES CON "TODOS"
    # ==========================================
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
    
    color_masculino <- "#3498DB"
    color_femenino  <- "#F5B7B1"
    
    # Orden estandarizado de curso de vida
    orden_curso_vida <- c("NIÑO", "ADOLESCENTE", "JOVEN", "ADULTO", "ADULTO MAYOR")
    
    colores_curso_vida <- c(
      "NIÑO" = "#FF6B6B", 
      "ADOLESCENTE" = "#FFD93D", 
      "JOVEN" = "#6BCB77",
      "ADULTO" = "#4D96FF", 
      "ADULTO MAYOR" = "#9B59B6"
    )
    
    colores_tipo <- c(
      "CONFIRMADO" = "#27AE60", 
      "PROBABLE" = "#F39C12",
      "SOSPECHOSO" = "#E67E22", 
      "DESCARTADO" = "#95A5A6"
    )
    
    paleta_10 <- c("#3498DB", "#E74C3C", "#2ECC71", "#F39C12", "#9B59B6",
                   "#1ABC9C", "#E67E22", "#2980B9", "#C0392B", "#27AE60")
    
    # ==========================================
    # FUNCIÓN PARA TÍTULOS CON COLOR BADGE
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
      
      # Diagnóstico
      if(!is.null(filtros$diagnostico) && length(filtros$diagnostico) > 0 && !("todos" %in% filtros$diagnostico)) {
        if(length(filtros$diagnostico) == 1) {
          badges <- c(badges, paste0('<span style="background:#E74C3C;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', filtros$diagnostico, '</span>'))
        } else {
          badges <- c(badges, paste0('<span style="background:#E74C3C;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', length(filtros$diagnostico), ' diags</span>'))
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
    # DATOS FILTRADOS (CON LIMPIEZA DE NAs)
    # ==========================================
    datos_filtrados <- reactive({
      req(input$filtro_anio)
      d <- casos
      
      # Aplicar filtro de año
      if(input$filtro_anio != "TODOS") d <- d[d[["ano"]] == as.numeric(input$filtro_anio), ]
      
      # Aplicar filtro de redes con "TODOS"
      redes_seleccionadas <- procesar_seleccion(input$filtro_red, redes_lista)
      if(length(redes_seleccionadas) > 0 && length(redes_seleccionadas) < length(redes_lista)) {
        d <- d[d[["redes"]] %in% redes_seleccionadas, ]
      }
      
      # Aplicar filtro de diagnósticos con "TODOS"
      diag_seleccionados <- procesar_seleccion(input$filtro_diag, enfermedades_lista)
      if(length(diag_seleccionados) > 0 && length(diag_seleccionados) < length(enfermedades_lista)) {
        d <- d[d[["diagnostico"]] %in% diag_seleccionados, ]
      }
      
      # Aplicar filtro de tipos con "TODOS"
      tipos_seleccionados <- procesar_seleccion(input$filtro_tipo, c("CONFIRMADO", "PROBABLE", "SOSPECHOSO", "DESCARTADO"))
      if(length(tipos_seleccionados) > 0 && length(tipos_seleccionados) < 4) {
        d <- d[d[["tipo_de_caso"]] %in% tipos_seleccionados, ]
      }
      
      # Limpieza de NAs en variables clave
      d <- d %>% filter(
        !is.na(curso_de_vida), curso_de_vida != "",
        !is.na(tipo_de_caso), tipo_de_caso != "",
        !is.na(sexo), sexo != "",
        !is.na(grupos_de_edad), grupos_de_edad != ""
      )
      
      # Normalización: convertir a factor con levels definidos
      d$curso_de_vida <- factor(d$curso_de_vida, levels = orden_curso_vida)
      
      return(d)
    })
    
    # ==========================================
    # TOTAL DE CASOS (para KPIs y alerta)
    # ==========================================
    total_casos_reactivo <- reactive({
      nrow(datos_filtrados())
    })
    
    # ==========================================
    # TÍTULO COMPLETO
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
      if("todos" %in% diag_sel || length(diag_sel) == 0 || length(diag_sel) >= length(enfermedades_lista)) {
        txt_diag <- "Todos los diagnósticos"
      } else if(length(diag_sel) == 1) {
        txt_diag <- paste("Diag:", diag_sel)
      } else {
        txt_diag <- paste("Diag:", paste(diag_sel, collapse = ", "))
      }
      
      div(class = "red-seleccionada", style = "font-size: 13px;",
          paste0("PERFIL EPIDEMIOLÓGICO | ", txt_red, " | ", txt_tipo, " | ", txt_anio, " | ", txt_diag))
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
      HTML(titulo_badge("DISTRIBUCIÓN POR EDAD Y SEXO", filtros))
    })
    
    output$titulo_g2 <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("DISTRIBUCIÓN POR CURSO DE VIDA", filtros))
    })
    
    output$titulo_g3 <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("MEDIANA DE EDAD POR ENFERMEDAD", filtros))
    })
    
    output$titulo_g4 <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("TIPO DE CASO POR CURSO DE VIDA", filtros))
    })
    
    output$titulo_g5 <- renderUI({
      filtros <- list(
        red = input$filtro_red,
        diagnostico = input$filtro_diag,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio
      )
      HTML(titulo_badge("TENDENCIA ESTACIONAL Y ACUMULACIÓN DE CASOS", filtros))
    })
    
    validar_datos <- function(d, mensaje = "No hay registros disponibles para los filtros seleccionados. Intenta ampliar el rango de búsqueda.") {
      if(is.null(d) || nrow(d) == 0) return(list(valido = FALSE, mensaje = mensaje))
      return(list(valido = TRUE, datos = d))
    }
    
    # ==========================================
    # KPI 1: TOTAL DE CASOS SELECCIONADOS
    # ==========================================
    output$kpi_total <- renderValueBox({
      total <- total_casos_reactivo()
      valueBox(
        value = format(total, big.mark = ","),
        subtitle = "TOTAL CASOS SELECCIONADOS",
        icon = icon("users"),
        color = "blue"
      )
    })
    
    # ==========================================
    # KPI 2: PORCENTAJE DE CONFIRMADOS
    # ==========================================
    output$kpi_confirmados_pct <- renderValueBox({
      d <- datos_filtrados()
      total <- nrow(d)
      confirmados <- sum(d$tipo_de_caso == "CONFIRMADO", na.rm = TRUE)
      pct <- if(total > 0) round((confirmados / total) * 100, 1) else 0
      valueBox(
        value = paste0(pct, "%"),
        subtitle = "CASOS CONFIRMADOS",
        icon = icon("check-circle"),
        color = "green"
      )
    })
    
    # ==========================================
    # KPI 3: DISTRIBUCIÓN DE TIPOS DE CASO
    # ==========================================
    output$kpi_distribucion_tipos <- renderValueBox({
      d <- datos_filtrados()
      total <- nrow(d)
      
      if(total == 0) {
        return(valueBox(
          value = "Sin datos",
          subtitle = "DISTRIBUCIÓN POR TIPO",
          icon = icon("balance-scale"),
          color = "blue"
        ))
      }
      
      confirmados <- sum(d$tipo_de_caso == "CONFIRMADO", na.rm = TRUE)
      probables <- sum(d$tipo_de_caso == "PROBABLE", na.rm = TRUE)
      sospechosos <- sum(d$tipo_de_caso == "SOSPECHOSO", na.rm = TRUE)
      descartados <- sum(d$tipo_de_caso == "DESCARTADO", na.rm = TRUE)
      
      pct_conf <- if(total > 0) round((confirmados / total) * 100, 1) else 0
      pct_prob <- if(total > 0) round((probables / total) * 100, 1) else 0
      pct_sosp <- if(total > 0) round((sospechosos / total) * 100, 1) else 0
      pct_desc <- if(total > 0) round((descartados / total) * 100, 1) else 0
      
      html_texto <- HTML(paste0(
        '<div style="display: flex; flex-direction: column; align-items: center; gap: 2px; width: 100%; padding: 2px 0;">',
        '<div style="display: flex; align-items: center; justify-content: center; gap: 6px; width: 100%;">',
        '<span style="color: #27AE60; font-size: 13px;">✅</span>',
        '<span style="color: #27AE60; font-weight: bold; font-size: 15px;">', format(confirmados, big.mark = ","), '</span>',
        '<span style="color: #27AE60; font-size: 11px;">(', pct_conf, '%)</span>',
        '</div>',
        if(probables > 0) paste0(
          '<div style="display: flex; align-items: center; justify-content: center; gap: 6px; width: 100%;">',
          '<span style="color: #F39C12; font-size: 13px;">⚡</span>',
          '<span style="color: #F39C12; font-weight: bold; font-size: 15px;">', format(probables, big.mark = ","), '</span>',
          '<span style="color: #F39C12; font-size: 11px;">(', pct_prob, '%)</span>',
          '</div>'
        ),
        if(sospechosos > 0) paste0(
          '<div style="display: flex; align-items: center; justify-content: center; gap: 6px; width: 100%;">',
          '<span style="color: #E67E22; font-size: 13px;">❓</span>',
          '<span style="color: #E67E22; font-weight: bold; font-size: 15px;">', format(sospechosos, big.mark = ","), '</span>',
          '<span style="color: #E67E22; font-size: 11px;">(', pct_sosp, '%)</span>',
          '</div>'
        ),
        if(descartados > 0) paste0(
          '<div style="display: flex; align-items: center; justify-content: center; gap: 6px; width: 100%;">',
          '<span style="color: #95A5A6; font-size: 13px;">🚫</span>',
          '<span style="color: #95A5A6; font-weight: bold; font-size: 15px;">', format(descartados, big.mark = ","), '</span>',
          '<span style="color: #95A5A6; font-size: 11px;">(', pct_desc, '%)</span>',
          '</div>'
        ),
        '<div style="display: flex; width: 90%; height: 4px; border-radius: 2px; overflow: hidden; background: #ecf0f1; margin-top: 3px;">',
        if(confirmados > 0) paste0('<div style="background: #27AE60; width: ', pct_conf, '%; height: 100%;"></div>'),
        if(probables > 0) paste0('<div style="background: #F39C12; width: ', pct_prob, '%; height: 100%;"></div>'),
        if(sospechosos > 0) paste0('<div style="background: #E67E22; width: ', pct_sosp, '%; height: 100%;"></div>'),
        if(descartados > 0) paste0('<div style="background: #95A5A6; width: ', pct_desc, '%; height: 100%;"></div>'),
        '</div>',
        '</div>'
      ))
      
      valueBox(
        value = html_texto,
        subtitle = "DISTRIBUCIÓN POR TIPO DE CASO",
        icon = icon("balance-scale"),
        color = "blue"
      )
    })
    
    # ==========================================
    # KPI 4: GRUPO DE EDAD CON MAYOR INCIDENCIA
    # ==========================================
    output$kpi_grupo_edad_top <- renderValueBox({
      d <- datos_filtrados()
      total <- nrow(d)
      
      if(total == 0) {
        return(valueBox(value = "N/A", subtitle = "GRUPO EDAD PREDOMINANTE", icon = icon("layer-group"), color = "yellow"))
      }
      
      grupo_top <- d %>%
        count(grupos_de_edad, sort = TRUE) %>%
        slice(1)
      
      grupo_nombre <- grupo_top$grupos_de_edad
      grupo_casos <- grupo_top$n
      grupo_pct <- round((grupo_casos / total) * 100, 1)
      
      valueBox(
        value = grupo_nombre,
        subtitle = paste0("GRUPO PREDOMINANTE (", grupo_pct, "% de casos)"),
        icon = icon("layer-group"),
        color = "yellow"
      )
    })
    
    # ==========================================
    # KPI 5: MEDIANA DE EDAD GLOBAL
    # ==========================================
    output$kpi_mediana_edad <- renderValueBox({
      d <- datos_filtrados()
      mediana <- if(nrow(d) > 0) round(median(d$edad, na.rm = TRUE), 0) else 0
      rango_iqr <- if(nrow(d) > 1) {
        q1 <- quantile(d$edad, 0.25, na.rm = TRUE)
        q3 <- quantile(d$edad, 0.75, na.rm = TRUE)
        paste0("(IQR: ", q1, "-", q3, ")")
      } else {
        ""
      }
      
      valueBox(
        value = paste0(mediana, " años"),
        subtitle = paste("MEDIANA DE EDAD", rango_iqr),
        icon = icon("clock"),
        color = "red"
      )
    })
    
    # ==========================================
    # ALERTA N PEQUEÑO
    # ==========================================
    output$alerta_n_pequeno <- renderUI({
      total <- total_casos_reactivo()
      if(total > 0 && total < 10) {
        div(
          style = "background-color: #FFF3CD; border: 1px solid #FFC107; border-radius: 8px; padding: 10px 15px; margin-bottom: 10px;",
          tags$i(class = "fas fa-exclamation-triangle", style = "color: #856404;"),
          tags$span(style = "color: #856404; font-weight: bold; margin-left: 8px;",
                    paste0("Nota: El análisis se basa en un universo reducido de casos (n = ", total, "). Interpretar con precaución."))
        )
      }
    })
    
    # ==========================================
    # G1: PIRÁMIDE POBLACIONAL
    # ==========================================
    output$g1_piramide <- renderPlotly({
      d <- datos_filtrados()
      validacion <- validar_datos(d)
      if(!validacion$valido) return(plotly_vacio_con_mensaje(validacion$mensaje))
      
      grupos_unicos <- unique(d$grupos_de_edad)
      if(length(grupos_unicos) < 1) return(plotly_vacio_con_mensaje("No se encontraron grupos de edad"))
      
      extraer_edad_min <- function(x) {
        if(is.na(x)) return(0)
        if(grepl("85|MÁS|MAS", x, ignore.case = TRUE)) return(85)
        num <- suppressWarnings(as.numeric(gsub("[^0-9].*", "", x)))
        if(is.na(num) || length(num) == 0) return(0)
        return(num[1])
      }
      
      edades_orden <- sapply(grupos_unicos, extraer_edad_min)
      orden_edad <- grupos_unicos[order(-edades_orden)]
      
      d_pir <- d %>%
        filter(grupos_de_edad %in% orden_edad) %>%
        group_by(grupos_de_edad, sexo) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(grupos_de_edad = factor(grupos_de_edad, levels = orden_edad))
      
      if(nrow(d_pir) == 0) return(plotly_vacio_con_mensaje("No se pudieron agrupar los datos"))
      
      total_general <- sum(d_pir$n)
      if(total_general == 0) return(plotly_vacio_con_mensaje("El total de casos es 0"))
      
      d_pir <- d_pir %>% mutate(porcentaje = (n / total_general) * 100)
      
      todos_grupos <- data.frame(grupos_de_edad = factor(orden_edad, levels = orden_edad))
      d_m <- todos_grupos %>% left_join(d_pir %>% filter(sexo == "M"), by = "grupos_de_edad")
      d_m$porcentaje[is.na(d_m$porcentaje)] <- 0; d_m$n[is.na(d_m$n)] <- 0
      d_f <- todos_grupos %>% left_join(d_pir %>% filter(sexo == "F"), by = "grupos_de_edad")
      d_f$porcentaje[is.na(d_f$porcentaje)] <- 0; d_f$n[is.na(d_f$n)] <- 0
      
      max_pct <- max(c(d_m$porcentaje, d_f$porcentaje), na.rm = TRUE) * 1.3
      if(max_pct == 0 || !is.finite(max_pct)) max_pct <- 10
      
      p <- plot_ly()
      if(sum(d_m$n) > 0) {
        p <- p %>% add_trace(data = d_m, x = ~-porcentaje, y = ~grupos_de_edad, type = "bar",
                orientation = "h", name = "MASCULINO",
                marker = list(color = color_masculino, opacity = 0.85, line = list(color = "white", width = 0.5)),
                hovertext = ~paste0("<b>MASCULINO</b><br>", grupos_de_edad, "<br>", round(porcentaje,1), "% (n=", n, ")"),
                hoverinfo = "text")
      }
      if(sum(d_f$n) > 0) {
        p <- p %>% add_trace(data = d_f, x = ~porcentaje, y = ~grupos_de_edad, type = "bar",
                orientation = "h", name = "FEMENINO",
                marker = list(color = color_femenino, opacity = 0.85, line = list(color = "white", width = 0.5)),
                hovertext = ~paste0("<b>FEMENINO</b><br>", grupos_de_edad, "<br>", round(porcentaje,1), "% (n=", n, ")"),
                hoverinfo = "text")
      }
      
      tick_vals <- pretty(c(0, max_pct), n = 6)
      tick_vals <- tick_vals[tick_vals >= 0]
      
      p %>% layout(
        barmode = "relative",
        xaxis = list(title = "PORCENTAJE (%)", tickvals = c(-rev(tick_vals), tick_vals[-1]),
                    ticktext = c(rev(tick_vals), tick_vals[-1]), range = c(-max_pct, max_pct),
                    showgrid = TRUE, zeroline = TRUE, zerolinecolor = "#2C3E50", zerolinewidth = 2),
        yaxis = list(title = "", autorange = "reversed", showgrid = FALSE, tickfont = list(size = 10)),
        legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center", font = list(size = 11)),
        margin = list(l = 160, r = 40, t = 20, b = 60), paper_bgcolor = "white", plot_bgcolor = "white"
      )
    })
    
    # ==========================================
# G2: CURSO DE VIDA (DONUT CON ORDEN CORREGIDO Y LEYENDA)
# ==========================================
output$g2_curso_vida <- renderPlotly({
  d <- datos_filtrados()
  validacion <- validar_datos(d)
  if(!validacion$valido) return(plotly_vacio_con_mensaje(validacion$mensaje))
  
  # 🔥 Asegurar que el orden sea NIÑO, ADOLESCENTE, JOVEN, ADULTO, ADULTO MAYOR
  d <- d %>% 
    mutate(curso_de_vida = factor(curso_de_vida, levels = orden_curso_vida)) %>%
    group_by(curso_de_vida) %>% 
    summarise(total = n(), .groups = "drop") %>%
    mutate(porcentaje = round((total / sum(total)) * 100, 1))
  
  if(nrow(d) == 0) return(plotly_vacio_con_mensaje("No se encontraron datos de curso de vida"))
  
  d$color <- colores_curso_vida[as.character(d$curso_de_vida)]
  d$color[is.na(d$color)] <- "#BDC3C7"
  
  # 🔥 ORDENAR LOS DATOS SEGÚN EL ORDEN DE CURSO DE VIDA
  d <- d %>% arrange(factor(curso_de_vida, levels = orden_curso_vida))
  
  plot_ly(d, 
          labels = ~curso_de_vida, 
          values = ~total, 
          type = "pie", 
          hole = 0.55,
          textposition = "outside", 
          textinfo = "label+percent", 
          textfont = list(size = 10),
          marker = list(colors = ~color, line = list(color = "white", width = 1)),
          hovertext = ~paste0(curso_de_vida, "<br>", porcentaje, "% (", total, " casos)"),
          hoverinfo = "text",
          showlegend = TRUE,
          sort = FALSE,  # 🔥 EVITA QUE PLOTLY REORDENE
          legendgroup = ~curso_de_vida) %>%
    layout(
      legend = list(
        title = list(text = "<b>CURSO DE VIDA</b>"),
        orientation = "h", 
        y = -0.12, 
        x = 0.5, 
        xanchor = "center",
        font = list(size = 10),
        traceorder = "normal"  # 🔥 MANTIENE EL ORDEN ORIGINAL
      ),
      margin = list(l = 20, r = 20, t = 30, b = 80),
      paper_bgcolor = "white", 
      plot_bgcolor = "white"
    )
})
    
    # ==========================================
    # G3: MEDIANA DE EDAD POR ENFERMEDAD
    # ==========================================
    output$g3_mediana_edad <- renderPlotly({
      d <- datos_filtrados()
      validacion <- validar_datos(d)
      if(!validacion$valido) return(plotly_vacio_con_mensaje(validacion$mensaje))
      
      d <- d %>% group_by(diagnostico) %>%
        summarise(mediana = round(median(edad, na.rm = TRUE), 0), total = n(), .groups = "drop") %>%
        filter(total > 0) %>% arrange(desc(total)) %>% head(10)
      
      if(nrow(d) == 0) return(plotly_vacio_con_mensaje("No se pudo calcular la mediana"))
      
      mediana_global <- round(median(datos_filtrados()$edad, na.rm = TRUE), 0)
      max_med <- max(d$mediana, na.rm = TRUE) * 1.2
      if(max_med <= 0 || !is.finite(max_med)) max_med <- 50
      
      d <- d %>% arrange(desc(mediana))
      d$diagnostico <- factor(d$diagnostico, levels = d$diagnostico)
      d$color_barra <- paleta_10[1:nrow(d)]
      n_barras <- nrow(d)
      
      plot_ly() %>%
        add_trace(data = d, x = ~mediana, y = ~diagnostico, type = "bar", orientation = "h",
                  marker = list(color = ~color_barra, opacity = 0.85, line = list(color = "white", width = 1)),
                  text = ~as.character(mediana), textposition = "outside",
                  textfont = list(color = "#2C3E50", size = 12),
                  hovertext = ~paste0("<b>", diagnostico, "</b><br>Mediana: ", mediana, " años<br>Total: ", total, " casos"),
                  hoverinfo = "text", name = "Mediana por enfermedad") %>%
        layout(
          xaxis = list(title = "EDAD", range = c(0, max_med),
                      dtick = ifelse(max_med > 50, 10, 5), showgrid = TRUE, zeroline = TRUE),
          yaxis = list(title = "", autorange = "reversed", showgrid = FALSE, tickfont = list(size = 11)),
          shapes = list(list(type = "line", x0 = mediana_global, x1 = mediana_global,
                            y0 = -0.4, y1 = n_barras - 0.6,
                            line = list(color = "#E74C3C", width = 2, dash = "dash"))),
          annotations = list(list(x = mediana_global, y = -0.6, text = paste0("Global: ", mediana_global),
                                 showarrow = FALSE, font = list(size = 10, color = "#E74C3C"), xanchor = "center")),
          showlegend = FALSE,
          margin = list(l = 200, r = 60, t = 20, b = 50), paper_bgcolor = "white", plot_bgcolor = "white"
        )
    })
    
    # ==========================================
# G4: TIPO DE CASO POR CURSO DE VIDA (BARRAS 100%)
# ==========================================
output$g4_tipo_curso <- renderPlotly({
  d <- datos_filtrados()
  validacion <- validar_datos(d)
  if(!validacion$valido) return(plotly_vacio_con_mensaje(validacion$mensaje))
  
  # 🔥 FILTRAR SOLO LOS TIPOS QUE NOS INTERESAN (SOSPECHOSO, PROBABLE, CONFIRMADO)
  orden_tipos <- c("SOSPECHOSO", "PROBABLE", "CONFIRMADO")
  
  # Verificar si hay casos de los tipos que nos interesan
  d_filtrado <- d %>% filter(tipo_de_caso %in% orden_tipos)
  
  # 🔥 SI NO HAY CASOS DE SOSPECHOSO, PROBABLE O CONFIRMADO → MOSTRAR MENSAJE
  if(nrow(d_filtrado) == 0) {
    return(plotly_vacio_con_mensaje(
      "No hay casos de tipo SOSPECHOSO, PROBABLE o CONFIRMADO.\nLos datos solo contienen casos DESCARTADOS."
    ))
  }
  
  d_plot <- d_filtrado %>%
    count(tipo_de_caso, curso_de_vida) %>%
    group_by(tipo_de_caso) %>%
    mutate(total_tipo = sum(n), porcentaje = round((n / total_tipo) * 100, 1)) %>%
    ungroup()
  
  if(nrow(d_plot) == 0) return(plotly_vacio_con_mensaje("No se encontraron datos para los tipos seleccionados"))
  
  d_plot$tipo_de_caso <- factor(d_plot$tipo_de_caso, levels = orden_tipos)
  d_plot$curso_de_vida <- factor(d_plot$curso_de_vida, levels = orden_curso_vida)
  
  plot_ly(d_plot, x = ~porcentaje, y = ~tipo_de_caso, color = ~curso_de_vida,
          type = "bar", orientation = "h", 
          colors = colores_curso_vida,
          legendgroup = ~curso_de_vida,
          hovertext = ~paste0("<b>", tipo_de_caso, "</b><br>", curso_de_vida, ": ", n, " casos (", porcentaje, "%)<br>Total: ", total_tipo),
          hoverinfo = "text") %>%
    layout(
      xaxis = list(title = "(%)", ticksuffix = "%", range = c(0, 105), showgrid = TRUE),
      yaxis = list(
        title = "", 
        categoryorder = "array", 
        categoryarray = orden_tipos
      ),
      barmode = "stack",
      legend = list(
        title = list(text = "<b>CURSO DE VIDA</b>"),
        orientation = "h", 
        y = -0.25, 
        x = 0.5, 
        xanchor = "center", 
        font = list(size = 9),
        traceorder = "normal"
      ),
      margin = list(l = 120, r = 40, t = 20, b = 70), 
      paper_bgcolor = "white", 
      plot_bgcolor = "white"
    )
})
    
# ==========================================
# G5: ESTACIONALIDAD MENSUAL (SIN WARNINGS - EJES CORREGIDOS + TICKS INTELIGENTES)
# ==========================================
output$g5_mensual <- renderPlotly({
  d <- datos_filtrados()
  validacion <- validar_datos(d)
  if(!validacion$valido) return(plotly_vacio_con_mensaje(validacion$mensaje))
  
  d <- d %>% filter(!is.na(mes) & mes >= 1 & mes <= 12) %>%
    group_by(mes) %>% summarise(total = n(), .groups = "drop") %>% arrange(mes)
  if(nrow(d) == 0) return(plotly_vacio_con_mensaje("No hay datos mensuales"))
  
  todos_meses <- data.frame(mes = 1:12, mes_nombre = meses_abrev[as.character(1:12)], stringsAsFactors = FALSE)
  d <- todos_meses %>% left_join(d, by = "mes")
  d$total[is.na(d$total)] <- 0
  
  total_anual <- sum(d$total, na.rm = TRUE)
  if(total_anual > 0) {
    d$porcentaje_acumulado <- (cumsum(d$total) / total_anual) * 100
  } else {
    d$porcentaje_acumulado <- 0
  }
  
  mes_mediana <- NA
  nombre_mes_mediana <- ""
  for(i in 1:nrow(d)) {
    if(!is.na(d$porcentaje_acumulado[i]) && d$porcentaje_acumulado[i] >= 50) {
      mes_mediana <- d$mes[i]
      nombre_mes_mediana <- d$mes_nombre[i]
      break
    }
  }
  
  if(nombre_mes_mediana == "" && total_anual > 0) {
    for(i in nrow(d):1) {
      if(d$total[i] > 0) {
        nombre_mes_mediana <- d$mes_nombre[i]
        break
      }
    }
  }
  if(nombre_mes_mediana == "") nombre_mes_mediana <- "Sin datos"
  
  titulo_dinamico <- paste0(
    "📊 Estacionalidad Mensual | Mediana: ",
    nombre_mes_mediana,
    " (50% casos)"
  )
  
  color_base <- color_anio()
  
  # 🔥 CALCULAR RANGO PARA EL EJE Y
  max_casos <- max(d$total, na.rm = TRUE)
  if(max_casos == 0) max_casos <- 1
  
  # 🔥 DETECTAR SI HAY POCOS CASOS
  hay_pocos_casos <- max_casos <= 20
  
  # 🔥 FUNCIÓN PARA GENERAR TICKS INTELIGENTES
  generar_ticks_inteligentes <- function(max_valor) {
    if(max_valor <= 10) {
      paso <- 1
    } else if(max_valor <= 20) {
      paso <- 2
    } else if(max_valor <= 50) {
      paso <- 5
    } else if(max_valor <= 100) {
      paso <- 10
    } else if(max_valor <= 250) {
      paso <- 20
    } else if(max_valor <= 500) {
      paso <- 50
    } else if(max_valor <= 1000) {
      paso <- 100
    } else {
      paso <- 200
    }
    
    ticks <- seq(0, max_valor + paso, by = paso)
    
    if(length(ticks) > 15) {
      paso <- paso * 2
      ticks <- seq(0, max_valor + paso, by = paso)
    }
    
    ticks <- ticks[ticks <= max_valor + paso]
    
    return(list(ticks = ticks, paso = paso))
  }
  
  ticks_info <- generar_ticks_inteligentes(max_casos)
  tick_vals <- ticks_info$ticks
  
  if(length(tick_vals) < 3 && max_casos > 0) {
    paso <- max(1, round(max_casos / 4))
    tick_vals <- seq(0, max_casos + paso, by = paso)
  }
  
  tick_vals <- round(tick_vals)
  
  # 🔥 CREAR ETIQUETAS DE TEXTO CON POSICIONES ALTERNADAS PARA EVITAR SUPERPOSICIÓN
  # Para el texto de porcentaje, alternar posición: arriba, abajo, izquierda, derecha
  posiciones_texto <- c()
  for(i in 1:nrow(d)) {
    pct <- d$porcentaje_acumulado[i]
    if(pct == 0) {
      posiciones_texto <- c(posiciones_texto, "none")  # No mostrar si es 0
    } else if(pct <= 15) {
      # Para porcentajes bajos, poner el texto a la derecha del punto
      posiciones_texto <- c(posiciones_texto, "top right")
    } else if(pct >= 85) {
      # Para porcentajes altos, poner el texto a la izquierda del punto
      posiciones_texto <- c(posiciones_texto, "top left")
    } else {
      # Para valores intermedios, alternar arriba y abajo
      if(i %% 2 == 0) {
        posiciones_texto <- c(posiciones_texto, "bottom center")
      } else {
        posiciones_texto <- c(posiciones_texto, "top center")
      }
    }
  }
  
  plot_ly() %>%
    # Barras
    add_trace(
      data = d, 
      x = ~mes_nombre, 
      y = ~total, 
      type = "bar",
      marker = list(
        color = color_base,
        opacity = 0.35,
        line = list(color = color_base, width = 1)
      ),
      name = "Casos",
      # 🔥 TEXTO DE BARRAS - SIN MOSTRAR SI HAY POCOS CASOS (PARA EVITAR SUPERPOSICIÓN)
      text = if(hay_pocos_casos) {
        # Si hay pocos casos, NO mostrar texto en las barras (solo en hover)
        rep("", nrow(d))
      } else {
        ~total
      },
      textposition = "outside",
      textfont = list(size = 10, color = "#2C3E50"),
      hoverinfo = "y",
      showlegend = TRUE
    ) %>%
    # Línea acumulada
    add_trace(
      data = d, 
      x = ~mes_nombre, 
      y = ~porcentaje_acumulado, 
      type = "scatter",
      mode = "lines+markers+text",
      yaxis = "y2",
      line = list(color = "#E74C3C", width = 2.5),
      marker = list(
        size = if(hay_pocos_casos) 10 else 8,  # 🔥 PUNTOS MÁS GRANDES SI HAY POCOS CASOS
        color = "#E74C3C",
        line = list(color = "white", width = 1.5)
      ),
      name = "Acumulado %",
      # 🔥 TEXTO DE PORCENTAJES CON POSICIÓN ALTERNADA
      text = ~paste0(round(porcentaje_acumulado, 1), "%"),
      textposition = posiciones_texto,  # 🔥 POSICIÓN DINÁMICA
      textfont = list(
        size = if(hay_pocos_casos) 10 else 9,
        color = if(hay_pocos_casos) "#2C3E50" else "#2C3E50"
      ),
      hoverinfo = "text",
      hovertext = ~paste0(
        mes_nombre, 
        ": ", 
        round(porcentaje_acumulado, 1), 
        "% acumulado (", 
        total, 
        " casos)"
      ),
      showlegend = TRUE
    ) %>%
    # Línea de mediana
    add_trace(
      x = ~mes_nombre,
      y = rep(50, nrow(d)),
      type = "scatter",
      mode = "lines",
      yaxis = "y2",
      line = list(color = "#2C3E50", width = 1.5, dash = "dash"),
      name = "Mediana (50%)",
      hoverinfo = "text",
      hovertext = paste0("50% acumulado - Mes de mediana: ", nombre_mes_mediana),
      showlegend = TRUE
    ) %>%
    layout(
      title = list(
        text = titulo_dinamico,
        font = list(size = 12, color = "#2C3E50"),
        y = 0.98,
        x = 0.5,
        xanchor = "center",
        yanchor = "top"
      ),
      xaxis = list(
        title = "MES", 
        categoryorder = "array", 
        categoryarray = unname(meses_abrev[as.character(1:12)]),
        titlefont = list(size = 12),
        tickfont = list(size = 10)
      ),
      yaxis = list(
        title = "CASOS", 
        rangemode = "nonnegative",
        side = "left",
        showgrid = TRUE,
        zeroline = TRUE,
        range = c(0, max_casos * 1.1),
        tickmode = "array",
        tickvals = tick_vals,
        ticktext = format(tick_vals, big.mark = ","),
        tickfont = list(size = ifelse(max_casos > 100, 9, 11)),
        tickangle = 0,
        tickformat = "d"
      ),
      yaxis2 = list(
        title = "ACUMULADO %",
        rangemode = "nonnegative",
        overlaying = "y",
        side = "right",
        tickformat = ".0f",
        ticksuffix = "%",
        showgrid = FALSE,
        zeroline = FALSE,
        range = c(0, 105),
        tickfont = list(size = 10)
      ),
      legend = list(
        orientation = "h", 
        y = -0.22,
        x = 0.5, 
        xanchor = "center",
        font = list(size = 10)
      ),
      margin = list(
        l = 70,
        r = 60, 
        t = 50,
        b = 80
      ),
      paper_bgcolor = "white", 
      plot_bgcolor = "white"
    )
})
    
# ==========================================
# FUNCIÓN PARA GRÁFICOS VACÍOS (SIN WARNINGS)
# ==========================================
plotly_vacio_con_mensaje <- function(mensaje = "No hay registros disponibles para los filtros seleccionados. Intenta ampliar el rango de búsqueda.") {
  plot_ly() %>%
    add_annotations(
      text = mensaje, 
      x = 0.5, 
      y = 0.5, 
      xref = "paper", 
      yref = "paper",
      showarrow = FALSE, 
      font = list(size = 14, color = "#7F8C8D")
    ) %>%
    layout(
      paper_bgcolor = "white", 
      plot_bgcolor = "white",
      xaxis = list(
        showgrid = FALSE, 
        zeroline = FALSE, 
        showticklabels = FALSE,
        range = c(0, 1)  # 🔥 Añadir rango para evitar warnings
      ),
      yaxis = list(
        showgrid = FALSE, 
        zeroline = FALSE, 
        showticklabels = FALSE,
        range = c(0, 1)  # 🔥 Añadir rango para evitar warnings
      )
    )
}
    
  })
}