# ============================================================
# MÓDULO: DATOS (PESTAÑA 4)
# OIIS - Oficina de Inteligencia e Información en Salud
# VERSIÓN FINAL v7 - Validaciones robustas
# ============================================================

# ==========================================
# UI DEL MÓDULO
# ==========================================
mod_datos_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        div(class = "red-seleccionada", style = "font-size: 14px; padding: 10px 15px;",
            "DATOS COMPLETOS - DESCARGA DE INFORMACIÓN")
      )
    ),
    
    tabsetPanel(
      id = ns("subpestanas"),
      type = "tabs",
      
      # ==========================================
      # TAB 1: CASOS
      # ==========================================
      tabPanel(
        title = div(icon("list"), " CASOS"),
        fluidRow(
          column(12,
            div(style = "padding: 10px 15px;",
              fluidRow(
                column(8,
                  pickerInput(ns("columnas_casos"), 
                    label = tags$span(style = "color: #2C3E50; font-weight: bold;", "SELECCIONAR COLUMNAS:"),
                    choices = NULL, selected = NULL, multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE, 
                      `live-search` = TRUE,
                      `none-selected-text` = "Todas las columnas",
                      `select-all-text` = "Seleccionar todas",
                      `deselect-all-text` = "Ninguna",
                      `count-selected-text` = "{0} columnas",
                      `selected-text-format` = "count > 3"
                    ),
                    width = "100%")
                ),
                column(4,
                  div(style = "display: flex; align-items: center; justify-content: flex-end; height: 100%; gap: 10px; padding-top: 25px;",
                    downloadButton(ns("descargar_csv_casos"), 
                      label = tags$span(icon("file-csv"), " CSV"),
                      class = "btn-descarga-opaco"),
                    downloadButton(ns("descargar_excel_casos"), 
                      label = tags$span(icon("file-excel"), " EXCEL"),
                      class = "btn-descarga-opaco")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = div(
              span("REGISTRO DE CASOS NOTIFICADOS"),
              tags$button(class = "btn-fullscreen",
                onclick = paste0(
                  "var el = document.getElementById('", ns("casos_container"), "');",
                  "if(el.requestFullscreen) { el.requestFullscreen(); }",
                  "else if(el.webkitRequestFullscreen) { el.webkitRequestFullscreen(); }",
                  "else if(el.msRequestFullscreen) { el.msRequestFullscreen(); }"
                ),
                tags$i(class = "fas fa-expand"), " Pantalla Completa"
              )
            ),
            status = "primary", solidHeader = TRUE, width = 12,
            div(id = ns("casos_container"), style = "overflow-x: auto;",
              DTOutput(ns("tabla_casos"))
            )
          )
        )
      ),
      
      # ==========================================
      # TAB 2: TIA
      # ==========================================
      tabPanel(
        title = div(icon("chart-bar"), " TIA - TASA DE INCIDENCIA ACUMULADA"),
        fluidRow(
          column(12,
            div(style = "padding: 10px 15px;",
              fluidRow(
                column(8,
                  pickerInput(ns("columnas_tia"),
                    label = tags$span(style = "color: #2C3E50; font-weight: bold;", "SELECCIONAR COLUMNAS:"),
                    choices = NULL, selected = NULL, multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE, 
                      `live-search` = TRUE,
                      `none-selected-text` = "Todas las columnas",
                      `select-all-text` = "Seleccionar todas",
                      `deselect-all-text` = "Ninguna",
                      `count-selected-text` = "{0} columnas",
                      `selected-text-format` = "count > 3"
                    ),
                    width = "100%")
                ),
                column(4,
                  div(style = "display: flex; align-items: center; justify-content: flex-end; height: 100%; gap: 10px; padding-top: 25px;",
                    downloadButton(ns("descargar_csv_tia"),
                      label = tags$span(icon("file-csv"), " CSV"),
                      class = "btn-descarga-opaco"),
                    downloadButton(ns("descargar_excel_tia"),
                      label = tags$span(icon("file-excel"), " EXCEL"),
                      class = "btn-descarga-opaco")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = div(
              span("TASA DE INCIDENCIA ACUMULADA POR RED"),
              tags$button(class = "btn-fullscreen",
                onclick = paste0(
                  "var el = document.getElementById('", ns("tia_container"), "');",
                  "if(el.requestFullscreen) { el.requestFullscreen(); }",
                  "else if(el.webkitRequestFullscreen) { el.webkitRequestFullscreen(); }",
                  "else if(el.msRequestFullscreen) { el.msRequestFullscreen(); }"
                ),
                tags$i(class = "fas fa-expand"), " Pantalla Completa"
              )
            ),
            status = "primary", solidHeader = TRUE, width = 12,
            div(id = ns("tia_container"), style = "overflow-x: auto;",
              DTOutput(ns("tabla_tia"))
            )
          )
        )
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
mod_datos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # FUNCIÓN PARA PROCESAR SELECCIONES CON "TODOS"
    # ==========================================
    procesar_seleccion <- function(seleccion, lista_completa) {
      if(length(seleccion) == 0) return(lista_completa)
      if(length(seleccion) == length(lista_completa)) return(lista_completa)
      return(seleccion)
    }
    
    # ==========================================
    # DATOS DE CASOS CON VALIDACIONES ROBUSTAS
    # ==========================================
    datos_casos <- reactive({
      d <- casos
      
      # 🔥 VALIDACIÓN 1: Si no hay datos
      if(is.null(d) || nrow(d) == 0) {
        return(data.frame(
          MENSAJE = "⚠️ No hay datos disponibles. Verifica la carga del archivo PRINCIPAL.csv",
          stringsAsFactors = FALSE
        ))
      }
      
      # 🔥 VALIDACIÓN 2: Verificar columnas necesarias
      columnas_necesarias <- c("id_caso", "diagnostico", "tipo_de_caso", "edad", "sexo", 
                               "fecha_de_inicio", "cie", "redes", "region", "provincia", 
                               "distrito", "grupos_de_edad", "curso_de_vida", "sem_epi", 
                               "mes", "ano")
      
      columnas_faltantes <- columnas_necesarias[!columnas_necesarias %in% names(d)]
      
      if(length(columnas_faltantes) > 0) {
        warning("⚠️ Columnas faltantes en casos: ", paste(columnas_faltantes, collapse = ", "))
        
        # Crear un dataframe con las columnas que existen
        d_out <- data.frame(stringsAsFactors = FALSE)
        
        for(col in columnas_necesarias) {
          if(col %in% names(d)) {
            d_out[[col]] <- d[[col]]
          } else {
            d_out[[col]] <- rep(NA, nrow(d))
          }
        }
        
        # Renombrar para mostrar en mayúsculas
        names(d_out) <- toupper(names(d_out))
        
        # Agregar columna de advertencia
        d_out$ADVERTENCIA <- paste("Columna faltante:", paste(columnas_faltantes, collapse = ", "))
        
        return(d_out)
      }
      
      # 🔥 CREAR DATAFRAME CON TODAS LAS COLUMNAS
      d_out <- data.frame(
        ID_CASO = d$id_caso,
        DIAGNOSTICO = d$diagnostico,
        TIPO_DE_CASO = d$tipo_de_caso,
        EDAD = d$edad,
        SEXO = d$sexo,
        FECHA_DE_INICIO = format(as.Date(d$fecha_de_inicio), "%d/%m/%Y"),
        CIE = d$cie,
        RED = d$redes,
        REGION = d$region,
        PROVINCIA = d$provincia,
        DISTRITO = d$distrito,
        GRUPO_DE_EDAD = d$grupos_de_edad,
        CURSO_DE_VIDA = d$curso_de_vida,
        SEMANA_EPI = d$sem_epi,
        MES = d$mes,
        ANO = d$ano,
        stringsAsFactors = FALSE
      )
      
      # Fecha de defunción (opcional)
      if("fecha_defuncion" %in% names(d)) {
        d_out$FECHA_DEFUNCION <- sapply(d$fecha_defuncion, function(x) {
          if (is.na(x) || is.null(x) || as.character(x) == "" || as.character(x) == "NA") return("")
          format(as.Date(x), "%d/%m/%Y")
        })
      } else {
        d_out$FECHA_DEFUNCION <- ""
      }
      
      return(d_out)
    })
    
    # ==========================================
    # DATOS DE TIA CON VALIDACIONES
    # ==========================================
    datos_tia <- reactive({
      if(is.null(tabla_tia) || nrow(tabla_tia) == 0) {
        return(data.frame(
          MENSAJE = "⚠️ No hay datos de TIA disponibles. Verifica la carga del archivo TIA.csv",
          stringsAsFactors = FALSE
        ))
      }
      d <- tabla_tia
      names(d) <- toupper(names(d))
      return(d)
    })
    
    # ==========================================
    # ACTUALIZAR SELECTORES DE COLUMNAS CON "TODOS"
    # ==========================================
    observe({
      d_casos <- datos_casos()
      
      # Verificar que no sea el dataframe de error
      if(!("MENSAJE" %in% names(d_casos)) && !("ADVERTENCIA" %in% names(d_casos))) {
        cols_casos <- names(d_casos)
        opciones_casos <- c("TODOS" = "todos", cols_casos)
        
        updatePickerInput(session, "columnas_casos",
          choices = opciones_casos,
          selected = "todos"
        )
      }
      
      d_tia <- datos_tia()
      if(!("MENSAJE" %in% names(d_tia))) {
        cols_tia <- names(d_tia)
        opciones_tia <- c("TODOS" = "todos", cols_tia)
        
        updatePickerInput(session, "columnas_tia",
          choices = opciones_tia,
          selected = "todos"
        )
      }
    })
    
    # ==========================================
    # FILTRAR COLUMNAS DE CASOS
    # ==========================================
    datos_casos_filtrados <- reactive({
      d <- datos_casos()
      
      # Si es un dataframe de error, devolverlo tal cual
      if("MENSAJE" %in% names(d) || "ADVERTENCIA" %in% names(d)) {
        return(d)
      }
      
      if(length(input$columnas_casos) > 0 && !("todos" %in% input$columnas_casos)) {
        cols_presentes <- intersect(input$columnas_casos, names(d))
        if(length(cols_presentes) > 0) {
          d <- d[, cols_presentes, drop = FALSE]
        }
      }
      return(d)
    })
    
    # ==========================================
    # FILTRAR COLUMNAS DE TIA
    # ==========================================
    datos_tia_filtrados <- reactive({
      d <- datos_tia()
      
      # Si es un dataframe de error, devolverlo tal cual
      if("MENSAJE" %in% names(d)) {
        return(d)
      }
      
      if(length(input$columnas_tia) > 0 && !("todos" %in% input$columnas_tia)) {
        cols_presentes <- intersect(input$columnas_tia, names(d))
        if(length(cols_presentes) > 0) {
          d <- d[, cols_presentes, drop = FALSE]
        }
      }
      return(d)
    })
    
    # ==========================================
    # TABLA DE CASOS
    # ==========================================
    output$tabla_casos <- DT::renderDataTable({
      d <- datos_casos_filtrados()
      
      # Si es un dataframe de error, mostrarlo con estilo
      if("MENSAJE" %in% names(d) || "ADVERTENCIA" %in% names(d)) {
        return(DT::datatable(d,
          options = list(
            dom = "t",
            language = list(info = "", emptyTable = ""),
            columnDefs = list(
              list(className = "dt-center", targets = "_all")
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe compact',
          escape = FALSE
        ))
      }
      
      DT::datatable(d,
        filter = "top",
        options = list(
          pageLength = 25,
          dom = "Blfrtip",
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json",
            search = "BUSCAR:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ registros"
          ),
          scrollX = TRUE,
          scrollY = "500px",
          stateSave = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "'background-color': '#0D4F85', 'color': 'white',",
            "'font-weight': 'bold', 'font-size': '11px', 'text-align': 'center'",
            "});",
            "}"
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact hover'
      )
    })
    
    # ==========================================
    # TABLA DE TIA
    # ==========================================
    output$tabla_tia <- DT::renderDataTable({
      d <- datos_tia_filtrados()
      
      # Si es un dataframe de error, mostrarlo con estilo
      if("MENSAJE" %in% names(d)) {
        return(DT::datatable(d,
          options = list(
            dom = "t",
            language = list(info = "", emptyTable = ""),
            columnDefs = list(
              list(className = "dt-center", targets = "_all")
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe compact',
          escape = FALSE
        ))
      }
      
      DT::datatable(d,
        filter = "top",
        options = list(
          pageLength = 30,
          dom = "Blfrtip",
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json",
            search = "BUSCAR:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ registros"
          ),
          scrollX = TRUE,
          stateSave = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "'background-color': '#0D4F85', 'color': 'white',",
            "'font-weight': 'bold', 'font-size': '11px', 'text-align': 'center'",
            "});",
            "}"
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact hover'
      )
    })
    
    # ==========================================
    # DESCARGA CASOS
    # ==========================================
    output$descargar_csv_casos <- downloadHandler(
      filename = function() { paste0("Casos_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(datos_casos_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    output$descargar_excel_casos <- downloadHandler(
      filename = function() { paste0("Casos_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(datos_casos_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # ==========================================
    # DESCARGA TIA
    # ==========================================
    output$descargar_csv_tia <- downloadHandler(
      filename = function() { paste0("TIA_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(datos_tia_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    output$descargar_excel_tia <- downloadHandler(
      filename = function() { paste0("TIA_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(datos_tia_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
  })
}