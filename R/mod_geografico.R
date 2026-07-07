# ============================================================
# MÓDULO: ANÁLISIS GEOGRÁFICO (PESTAÑA 3)
# OIIS - Oficina de Inteligencia e Información en Salud
# VERSIÓN FINAL v13 - Selectores con "TODOS" + Gráficos simplificados
# ============================================================

# ==========================================
# UI DEL MÓDULO
# ==========================================
mod_geografico_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      div(style = "padding: 15px 15px 0 15px;",
        div(style = "background-color: #0D4F85; border-radius: 12px; padding: 15px;",
          fluidRow(
            column(3,
              tags$label("REGIÓN", style = "color: white; font-size: 11px; font-weight: bold;"),
              pickerInput(ns("filtro_region"), NULL,
                choices = c("TODOS" = "todos", sort(unique(casos$region))), 
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
              tags$label("CURSO DE VIDA", style = "color: white; font-size: 11px; font-weight: bold;"),
              pickerInput(ns("filtro_curso"), NULL,
                choices = c("TODOS" = "todos", sort(unique(casos$curso_de_vida))), 
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
            )
          ),
          # NUEVA FILA: FILTRO POR RED ASISTENCIAL
          fluidRow(
            style = "margin-top: 10px;",
            column(8,
              tags$label("RED ASISTENCIAL | PRESTACIONAL", style = "color: white; font-size: 11px; font-weight: bold;"),
              pickerInput(ns("filtro_red"), NULL,
                choices = c("TODOS" = "todos", sort(unique(casos$redes))), 
                selected = "todos", multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, 
                  `live-search` = TRUE, 
                  size = 10,
                  `selected-text-format` = "count > 3",
                  `count-selected-text` = "TODOS ({0} seleccionados)"
                ),
                width = "100%")
            ),
            column(4,
              tags$label(" ", style = "color: white; font-size: 11px;"),
              actionButton(ns("borrar_filtros"), "BORRAR FILTROS", class = "btn-borrar",
                style = "width: 100%; margin-top: 5px;")
            )
          )
        )
      )
    ),
    
    fluidRow(column(12, uiOutput(ns("titulo_completo")))),
    
    # PANEL DE HOTSPOTS
    fluidRow(column(12, uiOutput(ns("panel_hotspots")))),
    
    # FILA 1: MAPA DE BURBUJAS POR RED (OCUPA 12 COLUMNAS)
    fluidRow(
      box(
        title = div(uiOutput(ns("titulo_mapa")), tags$button(class = "btn-fullscreen",
          onclick = paste0("var el = document.getElementById('", ns("mapa_container"), "');",
            "if(el.requestFullscreen) { el.requestFullscreen(); }"),
          tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 12,
        div(id = ns("mapa_container"), style = "width: 100%;",
          leafletOutput(ns("mapa_burbujas"), height = 550))
      )
    ),
    
    # FILA 2: CURSO DE VIDA x REGIÓN (OCUPA 12 COLUMNAS)
    fluidRow(
      box(
        title = div(uiOutput(ns("titulo_g5")), tags$button(class = "btn-fullscreen",
          onclick = paste0("var el = document.getElementById('", ns("g5_container"), "');",
            "if(el.requestFullscreen) { el.requestFullscreen(); }"),
          tags$i(class = "fas fa-expand"), " Pantalla Completa")),
        status = "primary", solidHeader = TRUE, width = 12,
        div(id = ns("g5_container"), style = "width: 100%; min-height: 400px;",
          plotlyOutput(ns("g5_curso_region"), height = 450))
      )
    ),
    
    # FILA 3: TABLA GEOGRÁFICA
    fluidRow(
      box(
        title = div(style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          uiOutput(ns("titulo_g6")),
          div(downloadButton(ns("descargar_excel"), "EXCEL", class = "btn-descarga"),
              downloadButton(ns("descargar_csv"), "CSV", class = "btn-descarga"))),
        status = "primary", solidHeader = TRUE, width = 12,
        DTOutput(ns("g6_tabla_geo"))
      )
    ),
    
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
mod_geografico_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # FUNCIÓN PARA PROCESAR SELECCIONES CON "TODOS"
    # ==========================================
    procesar_seleccion <- function(seleccion, lista_completa) {
      if(length(seleccion) == 0) return(lista_completa)
      if("todos" %in% seleccion) return(lista_completa)
      return(seleccion)
    }
    
    # Botón para borrar filtros
    observeEvent(input$borrar_filtros, {
      updatePickerInput(session, "filtro_region", selected = "todos")
      updatePickerInput(session, "filtro_diag", selected = "todos")
      updatePickerInput(session, "filtro_curso", selected = "todos")
      updatePickerInput(session, "filtro_tipo", selected = "todos")
      updatePickerInput(session, "filtro_anio", selected = "TODOS")
      updatePickerInput(session, "filtro_red", selected = "todos")
    })
    
    color_anio <- reactive({
      anio <- input$filtro_anio
      if(anio == "TODOS") return("#3498DB")
      if(anio == "2024") return("#2ECC71")
      if(anio == "2025") return("#E67E22")
      if(anio == "2026") return("#9B59B6")
      return("#3498DB")
    })
    
    colores_curso_vida <- c(
      "NIÑO" = "#FF6B6B", "ADOLESCENTE" = "#FFD93D", "JOVEN" = "#6BCB77",
      "ADULTO" = "#4D96FF", "ADULTO MAYOR" = "#9B59B6"
    )
    
    # ==========================================
    # FUNCIÓN PARA TÍTULOS CON COLOR BADGE
    # ==========================================
    titulo_badge <- function(base, filtros) {
      badges <- c()
      
      # Región
      if(!is.null(filtros$region) && length(filtros$region) > 0 && !("todos" %in% filtros$region)) {
        if(length(filtros$region) == 1) {
          badges <- c(badges, paste0('<span style="background:#0D4F85;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', filtros$region, '</span>'))
        } else {
          badges <- c(badges, paste0('<span style="background:#0D4F85;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', length(filtros$region), ' regiones</span>'))
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
      
      # Curso de vida
      if(!is.null(filtros$curso) && length(filtros$curso) > 0 && !("todos" %in% filtros$curso)) {
        if(length(filtros$curso) == 1) {
          badges <- c(badges, paste0('<span style="background:#2ECC71;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', filtros$curso, '</span>'))
        } else {
          badges <- c(badges, paste0('<span style="background:#2ECC71;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', length(filtros$curso), ' cursos</span>'))
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
      
      # Red
      if(!is.null(filtros$red) && length(filtros$red) > 0 && !("todos" %in% filtros$red)) {
        if(length(filtros$red) == 1) {
          badges <- c(badges, paste0('<span style="background:#8E44AD;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', filtros$red, '</span>'))
        } else {
          badges <- c(badges, paste0('<span style="background:#8E44AD;color:white;padding:1px 10px;border-radius:12px;font-size:10px;font-weight:bold;">', length(filtros$red), ' redes</span>'))
        }
      }
      
      if(length(badges) == 0) {
        return(base)
      } else {
        return(paste0(base, ' ', paste(badges, collapse = " ")))
      }
    }
    
    # ==========================================
    # DATOS FILTRADOS
    # ==========================================
    datos_filtrados_todos <- reactive({
      req(input$filtro_anio)
      d <- casos
      
      # Año
      if(input$filtro_anio != "TODOS") d <- d[d[["ano"]] == as.numeric(input$filtro_anio), ]
      
      # Región
      regiones_seleccionadas <- procesar_seleccion(input$filtro_region, unique(casos$region))
      if(length(regiones_seleccionadas) > 0 && length(regiones_seleccionadas) < length(unique(casos$region))) {
        d <- d[d[["region"]] %in% regiones_seleccionadas, ]
      }
      
      # Diagnóstico
      diag_seleccionados <- procesar_seleccion(input$filtro_diag, enfermedades_lista)
      if(length(diag_seleccionados) > 0 && length(diag_seleccionados) < length(enfermedades_lista)) {
        d <- d[d[["diagnostico"]] %in% diag_seleccionados, ]
      }
      
      # Curso de vida
      cursos_seleccionados <- procesar_seleccion(input$filtro_curso, unique(casos$curso_de_vida))
      if(length(cursos_seleccionados) > 0 && length(cursos_seleccionados) < length(unique(casos$curso_de_vida))) {
        d <- d[d[["curso_de_vida"]] %in% cursos_seleccionados, ]
      }
      
      # Tipo de caso
      tipos_seleccionados <- procesar_seleccion(input$filtro_tipo, c("CONFIRMADO", "PROBABLE", "SOSPECHOSO", "DESCARTADO"))
      if(length(tipos_seleccionados) > 0 && length(tipos_seleccionados) < 4) {
        d <- d[d[["tipo_de_caso"]] %in% tipos_seleccionados, ]
      }
      
      # Red
      redes_seleccionadas <- procesar_seleccion(input$filtro_red, unique(casos$redes))
      if(length(redes_seleccionadas) > 0 && length(redes_seleccionadas) < length(unique(casos$redes))) {
        d <- d[d[["redes"]] %in% redes_seleccionadas, ]
      }
      
      return(d)
    })
    
    datos_filtrados <- reactive({
      d <- datos_filtrados_todos()
      d <- d[d[["tipo_de_caso"]] != "DESCARTADO", ]
      return(d)
    })
    
    datos_tia_filtrados <- reactive({
      if(is.null(tabla_tia) || nrow(tabla_tia) == 0) return(NULL)
      d <- tabla_tia
      
      # Región
      regiones_seleccionadas <- procesar_seleccion(input$filtro_region, unique(casos$region))
      if(length(regiones_seleccionadas) > 0 && length(regiones_seleccionadas) < length(unique(casos$region))) {
        d <- d[d[["region"]] %in% regiones_seleccionadas, ]
      }
      
      # Red
      redes_seleccionadas <- procesar_seleccion(input$filtro_red, unique(casos$redes))
      if(length(redes_seleccionadas) > 0 && length(redes_seleccionadas) < length(unique(casos$redes))) {
        d <- d[d[["red"]] %in% redes_seleccionadas, ]
      }
      
      return(d)
    })
    
    # ==========================================
    # TÍTULOS DINÁMICOS
    # ==========================================
    output$titulo_completo <- renderUI({
      red_sel <- input$filtro_red
      if("todos" %in% red_sel || length(red_sel) == 0) {
        txt_red <- "TODAS LAS REDES"
      } else if(length(red_sel) == 1) {
        txt_red <- paste("Red", red_sel)
      } else {
        txt_red <- paste("Redes:", length(red_sel))
      }
      
      region_sel <- input$filtro_region
      if("todos" %in% region_sel || length(region_sel) == 0) {
        txt_region <- "TODAS LAS REGIONES"
      } else if(length(region_sel) == 1) {
        txt_region <- paste("Región", region_sel)
      } else {
        txt_region <- paste("Regiones:", length(region_sel))
      }
      
      anio_sel <- input$filtro_anio
      txt_anio <- if(anio_sel == "TODOS") "Todos los años" else paste("Año:", anio_sel)
      
      div(class = "red-seleccionada", style = "font-size: 13px;",
          paste0("ANÁLISIS GEOGRÁFICO | ", txt_region, " | ", txt_anio, " | ", txt_red))
    })
    
    output$titulo_mapa <- renderUI({
      filtros <- list(
        region = input$filtro_region,
        diagnostico = input$filtro_diag,
        curso = input$filtro_curso,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio,
        red = input$filtro_red
      )
      HTML(titulo_badge("MAPA DE INTENSIDAD POR RED ASISTENCIAL", filtros))
    })
    
    output$titulo_g5 <- renderUI({
      filtros <- list(
        region = input$filtro_region,
        diagnostico = input$filtro_diag,
        curso = input$filtro_curso,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio,
        red = input$filtro_red
      )
      HTML(titulo_badge("CURSO DE VIDA POR REGIÓN", filtros))
    })
    
    output$titulo_g6 <- renderUI({
      filtros <- list(
        region = input$filtro_region,
        diagnostico = input$filtro_diag,
        curso = input$filtro_curso,
        tipo = input$filtro_tipo,
        anio = input$filtro_anio,
        red = input$filtro_red
      )
      HTML(titulo_badge("TABLA DE DISTRIBUCIÓN GEOGRÁFICA", filtros))
    })
    
    validar_datos <- function(d, mensaje = "No hay datos") {
      if(is.null(d) || nrow(d) == 0) return(list(valido = FALSE, mensaje = mensaje))
      return(list(valido = TRUE, datos = d))
    }
    
    # ==========================================
    # PANEL DE HOTSPOTS
    # ==========================================
    output$panel_hotspots <- renderUI({
      d <- datos_filtrados()
      if(is.null(d) || nrow(d) == 0) return(NULL)
      top_distrito <- d %>% count(distrito, sort = TRUE) %>% slice(1)
      top_region <- d %>% count(region, sort = TRUE) %>% slice(1)
      top_red <- d %>% count(redes, sort = TRUE) %>% slice(1)
      div(style = "padding: 0 15px; margin-bottom: 10px;",
        div(style = "background: linear-gradient(135deg, #FFF5F5 0%, #FFFDE7 100%); 
                    border-left: 4px solid #E74C3C; border-radius: 8px; padding: 10px 15px;
                    display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 15px;",
          div(tags$i(class = "fas fa-map-marker-alt", style = "color: #E74C3C; margin-right: 8px;"),
              tags$strong("DISTRITO: "),
              tags$span(style = "color: #2C3E50; font-weight: bold;", top_distrito$distrito),
              tags$span(style = "color: #7F8C8D; margin-left: 5px;", paste0("(", top_distrito$n, " casos)"))),
          div(tags$i(class = "fas fa-globe-americas", style = "color: #E67E22; margin-right: 8px;"),
              tags$strong("REGIÓN: "),
              tags$span(style = "color: #2C3E50; font-weight: bold;", top_region$region),
              tags$span(style = "color: #7F8C8D; margin-left: 5px;", paste0("(", top_region$n, " casos)"))),
          div(tags$i(class = "fas fa-hospital", style = "color: #8E44AD; margin-right: 8px;"),
              tags$strong("RED: "),
              tags$span(style = "color: #2C3E50; font-weight: bold;", top_red$redes),
              tags$span(style = "color: #7F8C8D; margin-left: 5px;", paste0("(", top_red$n, " casos)"))),
          div(tags$i(class = "fas fa-database", style = "color: #3498DB; margin-right: 8px;"),
              tags$strong("TOTAL: "),
              tags$span(style = "color: #2C3E50; font-weight: bold;", paste0(nrow(d), " casos")))
        )
      )
    })
    
    # ============================================================
    # MAPA DE BURBUJAS POR RED ASISTENCIAL
    # ============================================================
    output$mapa_burbujas <- renderLeaflet({
      d <- datos_filtrados()
      if(is.null(d) || nrow(d) == 0) {
        return(leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -75.5, lat = -9.5, zoom = 5))
      }
      
      # Agrupar por red
      d_red <- d %>% count(redes, region, sort = TRUE)
      
      # Coordenadas de referencia
      coords_ref <- data.frame(
        region = c("AMAZONAS","ANCASH","APURIMAC","AREQUIPA","AYACUCHO","CAJAMARCA","CALLAO","CUSCO","HUANCAVELICA","HUANUCO",
                   "ICA","JUNIN","LA LIBERTAD","LAMBAYEQUE","LIMA","LORETO","MADRE DE DIOS","MOQUEGUA","PASCO","PIURA",
                   "PUNO","SAN MARTIN","TACNA","TUMBES","UCAYALI"),
        lat_base = c(-5.2,-9.5,-13.9,-16.4,-13.2,-7.2,-12.1,-13.5,-12.8,-9.9,-14.1,-11.2,-8.1,-6.7,-12.0,-4.0,-11.0,-17.0,-10.7,-5.2,-15.8,-7.2,-18.0,-3.6,-8.4),
        lon_base = c(-78.5,-77.6,-72.9,-71.5,-74.2,-78.5,-77.0,-71.9,-74.9,-76.2,-75.7,-75.5,-78.5,-79.9,-77.0,-73.5,-70.5,-70.9,-76.2,-80.7,-70.0,-76.8,-70.3,-80.5,-74.5),
        stringsAsFactors = FALSE)
      
      d_map <- d_red %>% left_join(coords_ref, by = "region")
      d_map$lat_base[is.na(d_map$lat_base)] <- -9.5
      d_map$lon_base[is.na(d_map$lon_base)] <- -75.5
      
      set.seed(123)
      d_map <- d_map %>%
        group_by(region) %>%
        mutate(
          n_redes_en_region = n(),
          idx = row_number(),
          offset_lat = case_when(
            n_redes_en_region == 1 ~ 0,
            n_redes_en_region == 2 ~ ifelse(idx == 1, -0.25, 0.25),
            n_redes_en_region == 3 ~ ifelse(idx == 1, -0.35, ifelse(idx == 2, 0, 0.35)),
            TRUE ~ (idx - (n_redes_en_region + 1) / 2) * 0.2
          ),
          offset_lon = case_when(
            n_redes_en_region == 1 ~ 0,
            n_redes_en_region == 2 ~ ifelse(idx == 1, -0.25, 0.25),
            n_redes_en_region == 3 ~ ifelse(idx == 1, -0.35, ifelse(idx == 2, 0, 0.35)),
            TRUE ~ (idx - (n_redes_en_region + 1) / 2) * 0.2
          ),
          lat = lat_base + offset_lat,
          lon = lon_base + offset_lon
        ) %>%
        ungroup()
      
      max_n <- max(d_map$n, na.rm = TRUE)
      if(max_n == 0 || !is.finite(max_n)) max_n <- 1
      
      pal <- colorBin(palette = c("#27AE60", "#F1C40F", "#E67E22", "#E74C3C"), 
                      domain = c(0, max_n), 
                      bins = c(0, round(max_n * 0.25), round(max_n * 0.50), round(max_n * 0.75), max_n + 1), 
                      na.color = "#95A5A6")
      
      total_casos <- sum(d_map$n)
      
      d_map$tooltip_label <- sprintf(
        '<div style="font-family:Segoe UI,sans-serif;min-width:220px;background:white;border-radius:8px;padding:0;overflow:hidden;box-shadow:0 4px 12px rgba(0,0,0,0.2);">
          <div style="background:%s;color:white;padding:10px 15px;font-size:14px;font-weight:bold;">🏥 %s</div>
          <div style="padding:12px 15px;">
            <div style="display:flex;align-items:center;margin-bottom:8px;">
              <span style="font-size:12px;color:#7F8C8D;width:80px;">🌎 Región:</span>
              <span style="font-weight:bold;color:#2C3E50;">%s</span>
            </div>
            <div style="border-top:1px solid #ECF0F1;margin:8px 0;padding-top:8px;">
              <div style="display:flex;align-items:center;justify-content:space-between;">
                <span style="font-size:12px;color:#7F8C8D;">📊 Casos:</span>
                <span style="font-size:22px;font-weight:bold;color:%s;">%s</span>
              </div>
              <div style="display:flex;align-items:center;justify-content:space-between;margin-top:4px;">
                <span style="font-size:11px;color:#7F8C8D;">📈 Proporción:</span>
                <span style="font-size:11px;font-weight:bold;color:#2C3E50;">%s%%</span>
              </div>
            </div>
            <div style="margin-top:8px;background:#ECF0F1;border-radius:4px;height:6px;overflow:hidden;">
              <div style="background:%s;height:100%%;width:%s%%;"></div>
            </div>
          </div>
        </div>',
        pal(d_map$n), 
        toupper(d_map$redes), 
        d_map$region, 
        pal(d_map$n), 
        format(d_map$n, big.mark = ","),
        round(d_map$n / total_casos * 100, 1), 
        pal(d_map$n), 
        round(d_map$n / max_n * 100))
      
      leaflet(d_map) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -75.5, lat = -9.5, zoom = 5) %>%
        addCircleMarkers(
          lng = ~lon, 
          lat = ~lat, 
          radius = ~sqrt(n) * 5,
          color = ~pal(n),
          fillOpacity = 0.6, 
          stroke = TRUE, 
          weight = 2,
          label = ~lapply(tooltip_label, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "0"),
            textsize = "0px", 
            direction = "auto"
          )
        ) %>%
        addLabelOnlyMarkers(
          lng = ~lon, 
          lat = ~lat,
          label = ~lapply(
            paste0("<strong style='color:#2C3E50;font-size:10px;'>", toupper(redes), "</strong>"), 
            HTML
          ),
          labelOptions = labelOptions(
            noHide = TRUE, 
            direction = "center", 
            offset = c(0, 0), 
            textOnly = TRUE,
            style = list(
              "background" = "rgba(255,255,255,0.85)", 
              "border" = "1px solid #DDD",
              "border-radius" = "4px", 
              "padding" = "2px 6px", 
              "font-size" = "9px"
            )
          )
        ) %>%
        addLegend(
          position = "bottomright", 
          pal = pal, 
          values = ~n, 
          title = "Casos", 
          opacity = 1,
          labFormat = function(type, cuts, p) {
            n <- length(cuts)
            paste0(format(round(cuts[-n]), big.mark = ","), " - ", format(round(cuts[-1] - 1), big.mark = ","))
          }
        )
    })
    
    # ==========================================
    # G5: CURSO DE VIDA POR REGIÓN
    # ==========================================
    output$g5_curso_region <- renderPlotly({
      d <- datos_filtrados()
      validacion <- validar_datos(d, "No hay datos para curso de vida por región")
      if(!validacion$valido) return(plotly_vacio_con_mensaje(validacion$mensaje))
      
      top_reg <- d %>% count(region, sort = TRUE) %>% head(5) %>% pull(region)
      d_cv <- d %>% filter(region %in% top_reg) %>% count(region, curso_de_vida) %>%
        group_by(region) %>% mutate(pct = round((n / sum(n)) * 100, 1), porcentaje = (n / sum(n)) * 100) %>% ungroup()
      
      if(nrow(d_cv) == 0) return(plotly_vacio_con_mensaje("No hay datos"))
      
      # Asegurar orden de curso de vida
      d_cv$curso_de_vida <- factor(d_cv$curso_de_vida, levels = c("NIÑO", "ADOLESCENTE", "JOVEN", "ADULTO", "ADULTO MAYOR"))
      
      plot_ly(d_cv, x = ~region, y = ~porcentaje, color = ~curso_de_vida, type = "bar", 
              colors = colores_curso_vida,
              hovertext = ~paste0(region, "<br>", curso_de_vida, ": ", n, " (", pct, "%)"), 
              hoverinfo = "text") %>%
        layout(xaxis = list(title = ""), yaxis = list(title = "(%)", ticksuffix = "%"), barmode = "stack",
               legend = list(title = list(text = "<b>CURSO DE VIDA</b>"), orientation = "h", y = -0.25, x = 0.5, 
                             xanchor = "center", font = list(size = 9), traceorder = "normal"),
               margin = list(l = 60, r = 40, t = 20, b = 70), paper_bgcolor = "white", plot_bgcolor = "white")
    })
    
# ==========================================
# G6: TABLA GEOGRÁFICA (BARRAS DE CASOS CON COLOR - VERSIÓN OPTIMIZADA)
# ==========================================
output$g6_tabla_geo <- DT::renderDataTable({
  d <- datos_filtrados_todos()
  if(nrow(d) == 0) {
    df_vacio <- data.frame("SIN DATOS" = "No hay casos", check.names = FALSE)
    return(DT::datatable(df_vacio, options = list(pageLength = 10, dom = "t",
      language = list(info = "", emptyTable = "No hay casos")), rownames = FALSE, class = 'cell-border stripe compact'))
  }
  
  d_tabla <- d %>% 
    group_by(RED = redes, REGIÓN = region, PROVINCIA = provincia, DISTRITO = distrito, 
             DIAGNÓSTICO = diagnostico, `TIPO DE CASO` = tipo_de_caso) %>%
    summarise(CASOS = n(), .groups = "drop") %>% 
    arrange(desc(CASOS))
  
  # Limitar a 100 filas para mejor rendimiento
  if(nrow(d_tabla) > 100) {
    d_tabla <- d_tabla %>% head(100)
  }
  
  max_casos <- max(d_tabla$CASOS, na.rm = TRUE)
  if(max_casos == 0) max_casos <- 1
  
  # 🔥 PALETA DE 13 COLORES PARA DIAGNÓSTICOS (VERSIONES CON OPACIDAD PARA MEJOR VISUALIZACIÓN)
  colores_diagnosticos <- c(
    "DIFTERIA" = "#9B59B6",
    "ESAVI - EVENTO ADVERSO POST VACUNAL" = "#8E44AD",
    "FIEBRE AMARILLA SELVATICA" = "#F1C40F",
    "HEPATITIS B" = "#F39C12",
    "PARALISIS FLACIDA AGUDA" = "#E74C3C",
    "PAROTIDITIS SIN COMPLICACIONES" = "#2ECC71",
    "PAROTIDITIS CON COMPLICACIONES" = "#27AE60",
    "RUBEOLA" = "#E67E22",
    "SARAMPION" = "#C0392B",
    "TETANOS" = "#34495E",
    "TOS FERINA" = "#E74C3C",
    "VARICELA CON OTRAS COMPLICACIONES" = "#2980B9",
    "VARICELA SIN COMPLICACIONES" = "#3498DB"
  )
  
  obtener_color <- function(diag) {
    for(nombre in names(colores_diagnosticos)) {
      if(grepl(nombre, diag, ignore.case = TRUE) || diag == nombre) {
        return(colores_diagnosticos[[nombre]])
      }
    }
    return("#95A5A6")
  }
  
  # 🔥 APLICAR COLOR DE UNA SOLA VEZ USANDO UN VECTOR
  # Crear un vector de colores para cada fila
  colores_filas <- sapply(d_tabla$DIAGNÓSTICO, obtener_color)
  
  # 🔥 CREAR TABLA
  dt <- DT::datatable(
    d_tabla,
    options = list(
      pageLength = 15, 
      dom = "tip",
      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"), 
      scrollX = TRUE,
      processing = FALSE  # 🔥 Desactivar procesamiento para mayor velocidad
    ),
    rownames = FALSE, 
    class = 'cell-border stripe compact'
  )
  
  # 🔥 APLICAR BARRA DE PROGRESO CON COLOR - UNA SOLA VEZ
  # Usamos un solo formatStyle con un vector de colores
  dt <- dt %>% formatStyle(
    "CASOS",
    background = styleColorBar(c(0, max_casos), "#79befb"),  # Color base
    backgroundSize = "98% 88%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = "#2C3E50",
    fontWeight = "bold"
  )
  
  # 🔥 Alternativa: Si quieres colores por diagnóstico, usa esta versión más eficiente
  # (Agrupa por diagnóstico para aplicar el mismo color a todas las filas del mismo diagnóstico)
  
  dt
})
    
    # ==========================================
    # DESCARGAS
    # ==========================================
    nombre_descarga <- reactive({
      region_sel <- input$filtro_region
      if("todos" %in% region_sel || length(region_sel) == 0) {
        reg_nombre <- "Todas_Regiones"
      } else {
        reg_nombre <- paste(region_sel, collapse = "_")
      }
      paste0("Distribucion_Geografica_", reg_nombre)
    })
    
    output$descargar_excel <- downloadHandler(
      filename = function() { paste0(nombre_descarga(), "_", Sys.Date(), ".csv") },
      content = function(file) {
        d <- datos_filtrados_todos()
        if(nrow(d) == 0) { write.csv(data.frame(Mensaje = "Sin datos"), file, row.names = FALSE); return() }
        d_out <- d %>% count(RED = redes, REGIÓN = region, PROVINCIA = provincia, DISTRITO = distrito, 
                             `TIPO DE CASO` = tipo_de_caso, sort = TRUE)
        write.csv(d_out, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    output$descargar_csv <- downloadHandler(
      filename = function() { paste0(nombre_descarga(), "_", Sys.Date(), ".csv") },
      content = function(file) {
        d <- datos_filtrados_todos()
        if(nrow(d) == 0) { write.csv(data.frame(Mensaje = "Sin datos"), file, row.names = FALSE); return() }
        d_out <- d %>% count(RED = redes, REGIÓN = region, PROVINCIA = provincia, DISTRITO = distrito, 
                             `TIPO DE CASO` = tipo_de_caso, sort = TRUE)
        write.csv(d_out, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # ==========================================
    # FUNCIÓN PARA GRÁFICOS VACÍOS
    # ==========================================
    plotly_vacio_con_mensaje <- function(mensaje = "Sin datos para mostrar") {
      plot_ly() %>% add_annotations(text = mensaje, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE, font = list(size = 16, color = "#7F8C8D")) %>%
        layout(paper_bgcolor = "white", plot_bgcolor = "white",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, range = c(0, 1)),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, range = c(0, 1)))
    }
    
  })
}