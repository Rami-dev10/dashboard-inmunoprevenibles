# ============================================================
# TEMA VISUAL DEL DASHBOARD
# OIIS - Oficina de Inteligencia e Información en Salud
# VERSIÓN MEJORADA - Fullscreen, botones descarga, pestañas, btn opaco
# ============================================================

# ==========================================
# PALETA DE COLORES
# ==========================================
azul_fondo      <- "#1681D9"
azul_oscuro     <- "#0D4F85"
blanco          <- "#FFFFFF"
naranja_kpi     <- "#E67E22"
verde_kpi       <- "#27AE60"
rojo_kpi        <- "#E74C3C"
gris_claro      <- "#ECF0F1"
gris_fondo      <- "#F8F9FA"

# ==========================================
# FUNCIÓN PRINCIPAL DEL TEMA
# ==========================================
tema_essalud <- function() {
  tags$head(
    # FONT AWESOME 6
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"),
    
    # ESTILOS CSS
    tags$style(HTML(paste0("
      /* ========== FONDO GENERAL ========== */
      .content-wrapper, .right-side {
        background-color: ", gris_fondo, " !important;
      }
      
      /* ========== HEADER ========== */
      .main-header .logo {
        background-color: ", azul_oscuro, " !important;
        color: white !important;
        font-weight: bold;
        height: 70px !important;
        line-height: 70px !important;
      }
      
      .main-header .navbar {
        background-color: ", azul_oscuro, " !important;
        box-shadow: 0 2px 10px rgba(0,0,0,0.2);
      }
      
      /* ========== SIDEBAR (DESHABILITADO) ========== */
      .main-sidebar {
        display: none !important;
      }
      
      /* ========== CONTENIDO PRINCIPAL ========== */
      .content {
        margin-left: 0 !important;
        padding: 0 !important;
      }
      
      /* ========== PESTAÑAS DE NAVEGACIÓN ========== */
      .nav-tabs {
        background-color: #0D4F85 !important;
        border-bottom: 3px solid #1681D9 !important;
        padding: 0 15px !important;
        margin: 0 !important;
      }
      
      .nav-tabs > li {
        margin-bottom: -3px !important;
      }
      
      .nav-tabs > li > a {
        color: rgba(255,255,255,0.8) !important;
        background-color: transparent !important;
        border: none !important;
        font-weight: bold !important;
        font-size: 13px !important;
        padding: 10px 20px !important;
        transition: all 0.3s !important;
        border-radius: 8px 8px 0 0 !important;
        margin-right: 3px !important;
      }
      
      .nav-tabs > li > a:hover {
        background-color: #1681D9 !important;
        color: white !important;
        border: none !important;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: white !important;
        color: #2C3E50 !important;
        border: none !important;
        font-weight: bold !important;
      }
      
      .tab-content {
        background-color: transparent !important;
        padding: 15px !important;
      }
      
      .tab-content > .tab-pane {
        padding: 0 !important;
      }
      
      /* ========== TÍTULO DE RED SELECCIONADA ========== */
      .red-seleccionada {
        color: #2C3E50 !important;
        padding: 8px 15px;
        font-size: 15px;
        font-weight: bold;
        background-color: transparent;
      }
      
      /* ========== CAJAS (BOX) ========== */
      .box {
        border-radius: 12px;
        background-color: white !important;
        border: 1px solid #e0e0e0 !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08) !important;
        margin-bottom: 15px;
      }
      
      .box-header {
        color: white !important;
        border-bottom: 1px solid rgba(255,255,255,0.2);
        padding: 12px 15px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        background-color: ", azul_fondo, " !important;
        border-radius: 12px 12px 0 0 !important;
      }
      
      .box-title {
        color: white !important;
        font-weight: bold !important;
        font-size: 13px !important;
        flex: 1;
      }
      
      .box-body {
        background-color: white !important;
        border-radius: 0 0 12px 12px;
        padding: 15px !important;
      }
      
      .box-tools {
        display: flex;
        align-items: center;
      }
      
      /* ========== BOTÓN PANTALLA COMPLETA ========== */
      .btn-fullscreen {
        background: rgba(255,255,255,0.2);
        color: white;
        border: 1px solid rgba(255,255,255,0.3);
        border-radius: 5px;
        padding: 3px 10px;
        font-size: 10px;
        cursor: pointer;
        margin-left: 8px;
        transition: all 0.3s;
        white-space: nowrap;
      }
      
      .btn-fullscreen:hover { 
        background: rgba(255,255,255,0.4); 
        border-color: white;
      }
      
      /* ========== BOTONES DESCARGA (HEADER) ========== */
      .btn-descarga {
        background: rgba(255,255,255,0.2);
        color: white !important;
        border: 1px solid rgba(255,255,255,0.3);
        border-radius: 5px;
        padding: 3px 10px;
        font-size: 10px;
        cursor: pointer;
        margin-left: 6px;
        transition: all 0.3s;
        white-space: nowrap;
        text-decoration: none !important;
      }
      
      .btn-descarga:hover { 
        background: rgba(255,255,255,0.4); 
        border-color: white;
        color: white !important;
      }
      
      /* ========== BOTONES DESCARGA OPACOS (PESTAÑA DATOS) ========== */
      .btn-descarga-opaco {
        background-color: #0D4F85 !important;
        color: white !important;
        border: 2px solid #0D4F85 !important;
        border-radius: 8px !important;
        padding: 8px 18px !important;
        font-size: 12px !important;
        font-weight: bold !important;
        cursor: pointer !important;
        transition: all 0.3s !important;
        text-decoration: none !important;
        white-space: nowrap !important;
        display: inline-block !important;
      }
      
      .btn-descarga-opaco:hover { 
        background-color: #1681D9 !important; 
        border-color: #1681D9 !important;
        color: white !important;
      }
      
      /* ========== KPI VALUE BOXES ========== */
      .small-box {
        border-radius: 12px;
        padding: 15px;
        color: white !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        margin-bottom: 15px;
      }
      
      .small-box h3 {
        font-size: 30px;
        font-weight: bold;
      }
      
      .small-box p {
        font-size: 13px;
      }
      
      .small-box .icon-large {
        font-size: 42px;
        opacity: 0.3;
      }
      
      .small-box .inner {
        padding: 10px;
      }
      
      /* ========== BOTÓN BORRAR FILTROS ========== */
      .btn-borrar {
        background-color: white;
        color: ", azul_fondo, ";
        border: 2px solid white;
        border-radius: 8px;
        padding: 8px 20px;
        font-weight: bold;
        cursor: pointer;
        transition: all 0.3s;
      }
      
      .btn-borrar:hover {
        background-color: ", azul_oscuro, ";
        color: white;
        border-color: white;
      }
      
      /* ========== SELECTORES - TEXTO NEGRO ========== */
      .bootstrap-select .dropdown-toggle,
      .bootstrap-select .dropdown-toggle:focus,
      .bootstrap-select .dropdown-toggle:active,
      .bootstrap-select .dropdown-toggle:hover {
        background-color: white !important;
        color: #2C3E50 !important;
        border: 1px solid #DDD;
        border-radius: 8px;
        font-size: 12px;
      }
      
      .bootstrap-select .dropdown-menu {
        color: #2C3E50 !important;
      }
      
      .bootstrap-select .dropdown-menu > li > a {
        color: #2C3E50 !important;
      }
      
      .bootstrap-select .filter-option {
        color: #2C3E50 !important;
      }
      
      .selectize-input {
        border-radius: 8px;
        border: 1px solid #DDD;
        font-size: 12px;
        color: #2C3E50 !important;
        background: white !important;
      }
      
      .selectize-input input {
        color: #2C3E50 !important;
      }
      
      .selectize-dropdown {
        color: #2C3E50 !important;
      }
      
      /* ========== FOOTER ========== */
      .footer-oiis {
        text-align: center;
        padding: 20px;
        background-color: white;
        margin-top: 20px;
        border-radius: 8px;
        font-size: 12px;
        color: #2C3E50;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #e0e0e0;
      }
      
      /* ========== SCROLLBAR ========== */
      ::-webkit-scrollbar {
        width: 6px;
      }
      
      ::-webkit-scrollbar-track {
        background: rgba(0,0,0,0.05);
        border-radius: 3px;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #BDC3C7;
        border-radius: 3px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: #95A5A6;
      }
      
      /* ========== FULLSCREEN - CUBRE TODO ========== */
      *:fullscreen {
        background-color: white !important;
        width: 100vw !important;
        height: 100vh !important;
      }
      
      *:-webkit-full-screen {
        background-color: white !important;
        width: 100vw !important;
        height: 100vh !important;
      }
      
      *:-moz-full-screen {
        background-color: white !important;
        width: 100vw !important;
        height: 100vh !important;
      }
      
      /* Fullscreen para divs específicos */
      :fullscreen .box,
      :fullscreen .box-body,
      :fullscreen .plotly,
      :fullscreen .leaflet-container {
        width: 100% !important;
        height: 100% !important;
        max-width: 100vw !important;
        max-height: 100vh !important;
      }
      
      :-webkit-full-screen .box,
      :-webkit-full-screen .box-body,
      :-webkit-full-screen .plotly,
      :-webkit-full-screen .leaflet-container {
        width: 100% !important;
        height: 100% !important;
        max-width: 100vw !important;
        max-height: 100vh !important;
      }
      
      /* ========== MEJORAS PARA GAUGES ========== */
      .box.box-warning .box-body,
      .box.box-danger .box-body {
        padding: 5px !important;
        background-color: white !important;
      }
      
      /* ========== TABLAS ========== */
      .dataTables_wrapper {
        font-size: 13px;
      }
      
      table.dataTable {
        border-collapse: collapse !important;
      }
      
      table.dataTable td, 
      table.dataTable th {
        padding: 8px 10px !important;
      }
    ")))
  )
}

message("✅ tema.R cargado correctamente")