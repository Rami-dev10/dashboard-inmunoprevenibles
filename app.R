# ============================================================
# DASHBOARD INMUNOPREVENIBLES - ESSALUD 2025+
# VERSIÓN 4.9 - KPIS MÁS GRANDES Y MEJOR ESPACIO
# ============================================================

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

# Cargar funciones y módulos
source("utils/helpers.R", local = TRUE)
source("utils/tema.R", local = TRUE)
source("R/mod_tablero.R", local = TRUE)
source("R/mod_perfil.R", local = TRUE)
source("R/mod_geografico.R", local = TRUE)
source("R/mod_datos.R", local = TRUE)

# Cargar datos
source("global.R")

# ============================================================
# UI CON CARÁTULA REDISEÑADA v4.9 - KPIS MEJORADOS
# ============================================================

header <- dashboardHeader(
  title = div(
    style = "line-height: 1.2; padding: 8px 0;",
    div("Tablero de Vigilancia de Enfermedades de Notificación Obligatoria",
        style = "font-weight: bold; font-size: 16px; color: white;"),
    div("Actualización de datos de la semana 1 al corte de la semana 24",
        style = "font-size: 11px; color: #BDC3C7; font-weight: 300;")
  ),
  titleWidth = 550
)

body <- dashboardBody(
  tema_essalud(),
  
  # 🔥 CSS REDISEÑO v4.9 - KPIS MÁS GRANDES
  tags$head(
    tags$style(HTML("
      /* ==========================================
         CARÁTULA REDISEÑADA v4.9
         ========================================== */
      .cover-page {
        background: linear-gradient(135deg, #002B4F 0%, #004B7A 30%, #0066A1 60%, #0080C8 100%);
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: flex-start;
        padding: 40px 30px 100px;
        position: relative;
        overflow: hidden;
      }
      
      /* ==========================================
         EFECTO DINÁMICO FONDO CON MOVIMIENTO SUAVE
         ========================================== */
      .cover-page::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: 
          radial-gradient(circle at 20% 50%, rgba(255,255,255,0.03) 0%, transparent 50%),
          radial-gradient(circle at 80% 80%, rgba(255,255,255,0.02) 0%, transparent 50%);
        pointer-events: none;
        animation: gradient-shift 15s ease infinite;
      }
      
      @keyframes gradient-shift {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.7; }
      }
      
      /* ==========================================
         ICONOS FLOTANTES EN FONDO
         ========================================== */
      .cover-page .fa-background {
        position: absolute;
        opacity: 0.06;
        pointer-events: none;
        color: white;
        font-size: 180px;
        line-height: 1;
        z-index: 1;
        animation: float-slow 12s ease-in-out infinite;
      }
      
      .cover-page .fa-background:nth-child(1) {
        top: 5%;
        left: 3%;
        transform: rotate(-20deg);
        animation-delay: 0s;
      }
      
      .cover-page .fa-background:nth-child(2) {
        top: 15%;
        right: 2%;
        font-size: 200px;
        transform: rotate(15deg);
        animation-delay: 2s;
        animation-direction: reverse;
      }
      
      .cover-page .fa-background:nth-child(3) {
        bottom: 15%;
        left: 1%;
        font-size: 160px;
        transform: rotate(25deg);
        animation-delay: 4s;
      }
      
      .cover-page .fa-background:nth-child(4) {
        bottom: 5%;
        right: 3%;
        font-size: 190px;
        transform: rotate(-30deg);
        animation-delay: 1s;
        animation-direction: reverse;
      }
      
      .cover-page .fa-background:nth-child(5) {
        top: 50%;
        left: 5%;
        font-size: 150px;
        opacity: 0.04;
        transform: rotate(-15deg);
        animation-delay: 3s;
      }
      
      .cover-page .fa-background:nth-child(6) {
        top: 40%;
        right: 4%;
        font-size: 170px;
        opacity: 0.05;
        animation-delay: 5s;
        animation-direction: reverse;
      }
      
      @keyframes float-slow {
        0%, 100% { transform: translateY(0px) rotate(0deg); }
        50% { transform: translateY(-30px) rotate(5deg); }
      }
      
      /* ==========================================
         LOGO ESSALUD - ESQUINA SUPERIOR IZQUIERDA
         ========================================== */
      .cover-logo-essalud {
        position: absolute;
        top: 25px;
        left: 30px;
        z-index: 10;
        width: 150px;
        height: 150px;
        padding: 0;
        border-radius: 0;
        background: transparent;
        border: none;
        box-shadow: none;
        transition: all 0.3s ease;
      }
      
      .cover-logo-essalud:hover {
        transform: scale(1.08);
      }
      
      .cover-logo-essalud img {
        width: 100%;
        height: 100%;
        object-fit: contain;
        filter: drop-shadow(0 2px 8px rgba(0,0,0,0.2));
      }
      
      /* ==========================================
         CONTENIDO PRINCIPAL
         ========================================== */
      .cover-content {
        position: relative;
        z-index: 5;
        max-width: 1300px;
        width: 100%;
        text-align: center;
        animation: fadeInUp 0.8s ease-out;
        margin-top: 0;
        padding-top: 10px;
      }
      
      @keyframes fadeInUp {
        from { 
          opacity: 0; 
          transform: translateY(40px); 
        }
        to { 
          opacity: 1; 
          transform: translateY(0); 
        }
      }
      
      /* ==========================================
         TÍTULOS Y SUBTÍTULOS
         ========================================== */
      .cover-title {
        color: white;
        font-size: 48px;
        font-weight: 800;
        text-shadow: 0 6px 30px rgba(0,0,0,0.3);
        margin-bottom: 8px;
        letter-spacing: 2px;
        line-height: 1.1;
      }
      
      .cover-title-secondary {
        font-size: 24px;
        font-weight: 300;
        margin-bottom: 20px;
        letter-spacing: 0.8px;
      }
      
      .cover-subtitle {
        color: rgba(255,255,255,0.95);
        font-size: 16px;
        font-weight: 300;
        margin-bottom: 3px;
        text-shadow: 0 2px 10px rgba(0,0,0,0.15);
      }
      
      .cover-subtitle-strong {
        font-weight: 600;
        color: white;
      }
      
      /* PERÍODO */
      .cover-period {
        color: rgba(255,255,255,0.9);
        font-size: 14px;
        font-weight: 400;
        background: rgba(255,255,255,0.1);
        padding: 12px 40px;
        border-radius: 25px;
        display: inline-block;
        margin-top: 18px;
        margin-bottom: 45px;
        backdrop-filter: blur(6px);
        border: 1px solid rgba(255,255,255,0.12);
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        letter-spacing: 0.3px;
      }
      
      /* ==========================================
         KPIs / TARJETAS DE ESTADÍSTICAS - MÁS GRANDES
         ========================================== */
      .cover-stats {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 30px;
        max-width: 1300px;
        width: 100%;
        margin: 0 auto 50px auto;
        padding: 0 10px;
      }
      
      .cover-stat {
        border-radius: 20px;
        padding: 40px 25px 35px;
        text-align: center;
        border: none;
        transition: all 0.4s cubic-bezier(0.34, 1.56, 0.64, 1);
        min-height: 180px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        box-shadow: 0 12px 40px rgba(0,0,0,0.3);
        position: relative;
        overflow: hidden;
      }
      
      .cover-stat:hover {
        transform: translateY(-8px) scale(1.02);
        box-shadow: 0 30px 70px rgba(0,0,0,0.4);
      }
      
      /* KPI 1 - Rojo/Naranja */
      .cover-stat:nth-child(1) {
        background: linear-gradient(135deg, #FF6B6B, #EE5A24);
      }
      .cover-stat:nth-child(1):hover {
        box-shadow: 0 30px 70px rgba(238, 90, 36, 0.5);
      }
      
      /* KPI 2 - Naranja/Dorado */
      .cover-stat:nth-child(2) {
        background: linear-gradient(135deg, #FFB347, #FF8C00);
      }
      .cover-stat:nth-child(2):hover {
        box-shadow: 0 30px 70px rgba(255, 140, 0, 0.5);
      }
      
      /* KPI 3 - Púrpura/Lavanda */
      .cover-stat:nth-child(3) {
        background: linear-gradient(135deg, #A29BFE, #6C5CE7);
      }
      .cover-stat:nth-child(3):hover {
        box-shadow: 0 30px 70px rgba(108, 92, 231, 0.5);
      }
      
      /* KPI 4 - Rosa/Fucsia */
      .cover-stat:nth-child(4) {
        background: linear-gradient(135deg, #FD79A8, #E84393);
      }
      .cover-stat:nth-child(4):hover {
        box-shadow: 0 30px 70px rgba(232, 67, 147, 0.5);
      }
      
      .cover-stat .stat-icon {
        font-size: 42px;
        display: block;
        margin-bottom: 14px;
        color: rgba(255,255,255,0.95);
        text-shadow: 0 2px 15px rgba(0,0,0,0.25);
        transition: all 0.3s ease;
      }
      
      .cover-stat:hover .stat-icon {
        transform: scale(1.2) rotate(8deg);
      }
      
      .cover-stat .number {
        font-size: 56px;
        font-weight: 900;
        color: white;
        display: block;
        text-shadow: 0 4px 25px rgba(0,0,0,0.3);
        letter-spacing: -2px;
        line-height: 1.05;
        margin-bottom: 6px;
      }
      
      .cover-stat .label {
        font-size: 14px;
        color: rgba(255,255,255,0.95);
        font-weight: 700;
        letter-spacing: 2px;
        margin-top: 12px;
        text-transform: uppercase;
        text-shadow: 0 2px 10px rgba(0,0,0,0.15);
        border-top: 1px solid rgba(255,255,255,0.2);
        padding-top: 12px;
        width: 80%;
      }
      
      /* ==========================================
         BOTONES DE NAVEGACIÓN - COLORES FRÍOS/METÁLICOS
         ========================================== */
      .cover-buttons {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 25px;
        margin: 0 auto 60px auto;
        max-width: 1200px;
        width: 100%;
        padding: 0 10px;
      }
      
      .cover-btn {
        border-radius: 20px;
        padding: 30px 20px 26px;
        text-align: center;
        color: white;
        cursor: pointer;
        transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
        text-decoration: none;
        backdrop-filter: blur(8px);
        position: relative;
        overflow: hidden;
        min-height: 150px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        box-shadow: 0 12px 35px rgba(0,0,0,0.2);
        border: 4px solid;
      }
      
      /* Botón 1 - Turquesa */
      .cover-btn:nth-child(1) {
        background: rgba(0, 206, 209, 0.35);
        border-color: rgba(0, 206, 209, 0.7);
      }
      .cover-btn:nth-child(1):hover {
        background: rgba(0, 206, 209, 0.55);
        border-color: rgba(0, 206, 209, 1);
        box-shadow: 0 25px 60px rgba(0, 206, 209, 0.4);
        transform: translateY(-12px);
      }
      
      /* Botón 2 - Azul */
      .cover-btn:nth-child(2) {
        background: rgba(100, 149, 237, 0.35);
        border-color: rgba(100, 149, 237, 0.7);
      }
      .cover-btn:nth-child(2):hover {
        background: rgba(100, 149, 237, 0.55);
        border-color: rgba(100, 149, 237, 1);
        box-shadow: 0 25px 60px rgba(100, 149, 237, 0.4);
        transform: translateY(-12px);
      }
      
      /* Botón 3 - Verde-azulado */
      .cover-btn:nth-child(3) {
        background: rgba(72, 209, 204, 0.35);
        border-color: rgba(72, 209, 204, 0.7);
      }
      .cover-btn:nth-child(3):hover {
        background: rgba(72, 209, 204, 0.55);
        border-color: rgba(72, 209, 204, 1);
        box-shadow: 0 25px 60px rgba(72, 209, 204, 0.4);
        transform: translateY(-12px);
      }
      
      /* Botón 4 - Celeste */
      .cover-btn:nth-child(4) {
        background: rgba(135, 206, 235, 0.35);
        border-color: rgba(135, 206, 235, 0.7);
      }
      .cover-btn:nth-child(4):hover {
        background: rgba(135, 206, 235, 0.55);
        border-color: rgba(135, 206, 235, 1);
        box-shadow: 0 25px 60px rgba(135, 206, 235, 0.4);
        transform: translateY(-12px);
      }
      
      /* EFECTO SHINE (Brillo deslizante) */
      .cover-btn::before {
        content: '';
        position: absolute;
        top: -50%;
        left: -50%;
        width: 200%;
        height: 200%;
        background: linear-gradient(45deg, transparent, rgba(255,255,255,0.15), transparent);
        transform: rotate(45deg);
        animation: shine-effect 3s infinite;
        pointer-events: none;
      }
      
      @keyframes shine-effect {
        0% { transform: translateX(-100%) translateY(-100%) rotate(45deg); }
        100% { transform: translateX(100%) translateY(100%) rotate(45deg); }
      }
      
      .cover-btn:active {
        transform: translateY(-5px) scale(0.98);
      }
      
      .cover-btn .icon-wrap {
        font-size: 40px;
        display: block;
        margin-bottom: 12px;
        transition: all 0.3s cubic-bezier(0.34, 1.56, 0.64, 1);
        text-shadow: 0 2px 15px rgba(0,0,0,0.3);
        position: relative;
        z-index: 2;
      }
      
      .cover-btn:hover .icon-wrap {
        transform: scale(1.3) translateY(-4px);
      }
      
      .cover-btn .btn-label {
        font-size: 15px;
        font-weight: 700;
        display: block;
        letter-spacing: 1px;
        line-height: 1.3;
        margin-bottom: 4px;
        position: relative;
        z-index: 2;
        text-shadow: 0 2px 10px rgba(0,0,0,0.2);
      }
      
      .cover-btn .btn-desc {
        font-size: 11.5px;
        opacity: 0.85;
        display: block;
        font-weight: 300;
        letter-spacing: 0.5px;
        position: relative;
        z-index: 2;
      }
      
      /* ==========================================
         FOOTER FIJO CON LOGO OIIS
         ========================================== */
      .cover-footer {
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        z-index: 10;
        padding: 15px 20px;
        background: linear-gradient(180deg, transparent 0%, rgba(0,43,79,0.3) 30%, rgba(0,43,79,0.8) 100%);
        backdrop-filter: blur(4px);
        text-align: center;
      }
      
      .footer-logo-container {
        display: inline-block;
        background: rgba(255,255,255,0.95);
        border-radius: 24px;
        padding: 12px 40px;
        backdrop-filter: blur(10px);
        border: 2px solid rgba(255,255,255,0.6);
        box-shadow: 0 28px 80px rgba(0,0,0,0.4);
        transition: all 0.35s cubic-bezier(0.34, 1.56, 0.64, 1);
      }
      
      .footer-logo-container:hover {
        transform: translateY(-5px);
        box-shadow: 0 32px 95px rgba(0,0,0,0.5);
        background: rgba(255,255,255,1);
      }
      
      .footer-logo-container .logo-wrapper {
        display: inline-block;
        width: 80px;
        height: 80px;
        background: rgba(0, 107, 179, 0.1);
        border-radius: 50%;
        padding: 8px;
        margin-right: 16px;
        vertical-align: middle;
      }
      
      .footer-logo-container .logo-wrapper img {
        width: 100%;
        height: 100%;
        object-fit: contain;
      }
      
      .footer-text-wrapper {
        display: inline-block;
        vertical-align: middle;
        text-align: left;
      }
      
      .footer-text-title {
        color: #006BB3;
        font-size: 16px;
        font-weight: 800;
        letter-spacing: 1.2px;
        display: block;
        margin-bottom: 2px;
      }
      
      .footer-text-subtitle {
        color: #0077CC;
        font-size: 12px;
        font-weight: 400;
        letter-spacing: 0.5px;
        display: block;
      }
      
      /* ==========================================
         RESPONSIVE DESIGN - KPIS AJUSTADOS
         ========================================== */
      @media (max-width: 1200px) {
        .cover-stats { gap: 25px; }
        .cover-stat { min-height: 160px; padding: 35px 20px 30px; }
        .cover-stat .number { font-size: 48px; }
        .cover-stat .stat-icon { font-size: 38px; }
        .cover-buttons { gap: 20px; }
      }
      
      @media (max-width: 1024px) {
        .cover-title { font-size: 40px; }
        .cover-title-secondary { font-size: 20px; }
        .cover-subtitle { font-size: 15px; }
        .cover-stats { 
          grid-template-columns: repeat(2, 1fr); 
          gap: 20px;
          margin-bottom: 40px;
          max-width: 800px;
        }
        .cover-stat { 
          min-height: 150px; 
          padding: 30px 20px 25px; 
        }
        .cover-stat .number { font-size: 44px; }
        .cover-stat .stat-icon { font-size: 36px; }
        .cover-stat .label { font-size: 13px; }
        .cover-buttons { 
          grid-template-columns: repeat(2, 1fr); 
          gap: 18px;
          margin-bottom: 50px;
        }
        .cover-btn { min-height: 130px; padding: 26px 16px 22px; }
        .cover-btn .icon-wrap { font-size: 36px; }
        .cover-logo-essalud { width: 70px; height: 70px; top: 20px; left: 25px; }
        .cover-page .fa-background { opacity: 0.04; font-size: 140px; }
        .cover-footer { padding: 12px 15px; }
        .footer-logo-container { padding: 10px 30px; }
        .footer-text-title { font-size: 14px; }
        .footer-text-subtitle { font-size: 11px; }
      }
      
      @media (max-width: 768px) {
        .cover-title { font-size: 30px; margin-bottom: 6px; }
        .cover-title-secondary { font-size: 17px; margin-bottom: 15px; }
        .cover-subtitle { font-size: 13px; }
        .cover-stats { 
          grid-template-columns: repeat(2, 1fr); 
          gap: 15px; 
          margin-bottom: 30px;
          max-width: 600px;
        }
        .cover-stat { 
          padding: 25px 16px 20px; 
          min-height: 130px; 
        }
        .cover-stat .number { font-size: 38px; }
        .cover-stat .stat-icon { font-size: 32px; margin-bottom: 10px; }
        .cover-stat .label { font-size: 11px; letter-spacing: 1.5px; margin-top: 10px; padding-top: 10px; }
        .cover-buttons { 
          grid-template-columns: 1fr 1fr; 
          gap: 14px; 
          margin-bottom: 40px; 
        }
        .cover-btn { padding: 22px 14px 18px; min-height: 110px; border-width: 3px; }
        .cover-btn .icon-wrap { font-size: 32px; margin-bottom: 10px; }
        .cover-btn .btn-label { font-size: 13px; }
        .cover-btn .btn-desc { font-size: 10px; }
        .cover-logo-essalud { width: 55px; height: 55px; top: 15px; left: 20px; }
        .cover-period { font-size: 12px; padding: 10px 28px; margin-bottom: 35px; }
        .cover-footer { padding: 10px 15px; }
        .footer-logo-container { padding: 10px 20px; }
        .footer-logo-container .logo-wrapper { width: 45px; height: 45px; margin-right: 12px; padding: 6px; }
        .footer-text-title { font-size: 12px; }
        .footer-text-subtitle { font-size: 10px; }
        .cover-page .fa-background { font-size: 100px; }
        .cover-page { padding: 30px 20px 90px; }
      }
      
      @media (max-width: 480px) {
        .cover-title { font-size: 24px; }
        .cover-title-secondary { font-size: 14px; margin-bottom: 10px; }
        .cover-subtitle { font-size: 11px; }
        .cover-stats { 
          grid-template-columns: 1fr 1fr; 
          gap: 10px; 
          margin-bottom: 25px;
          max-width: 400px;
        }
        .cover-stat { 
          padding: 20px 12px 16px; 
          min-height: 110px; 
        }
        .cover-stat .number { font-size: 32px; }
        .cover-stat .stat-icon { font-size: 28px; margin-bottom: 8px; }
        .cover-stat .label { font-size: 10px; letter-spacing: 1px; margin-top: 8px; padding-top: 8px; width: 90%; }
        .cover-buttons { 
          grid-template-columns: 1fr 1fr; 
          gap: 10px; 
          margin-bottom: 30px; 
        }
        .cover-btn { padding: 18px 10px 14px; min-height: 95px; border-width: 3px; }
        .cover-btn .icon-wrap { font-size: 28px; margin-bottom: 8px; }
        .cover-btn .btn-label { font-size: 11px; }
        .cover-btn .btn-desc { font-size: 9px; }
        .cover-logo-essalud { width: 45px; height: 45px; top: 12px; left: 15px; }
        .cover-period { font-size: 11px; padding: 8px 20px; margin-bottom: 25px; }
        .cover-footer { padding: 8px 10px; }
        .footer-logo-container { padding: 8px 16px; }
        .footer-logo-container .logo-wrapper { width: 40px; height: 40px; margin-right: 10px; padding: 6px; }
        .footer-text-title { font-size: 11px; }
        .footer-text-subtitle { font-size: 9px; }
        .cover-page .fa-background { font-size: 80px; }
        .cover-page { padding: 20px 15px 80px; }
      }
    "))
  ),
  
  # ============================================================
  # TABSET PANEL CON CARÁTULA
  # ============================================================
  tabsetPanel(
    id = "pestanas",
    type = "tabs",
    
    # 🔥 PESTAÑA: INICIO / CARÁTULA REDISEÑADA
    tabPanel(
      title = div(icon("home"), " INICIO"),
      value = "inicio",
      div(class = "cover-page",
        
        # ==========================================
        # ICONOS FONT AWESOME DE FONDO
        # ==========================================
        div(class = "fa-background", icon("heartbeat")),
        div(class = "fa-background", icon("stethoscope")),
        div(class = "fa-background", icon("microscope")),
        div(class = "fa-background", icon("hospital")),
        div(class = "fa-background", icon("users")),
        div(class = "fa-background", icon("chart-line")),
        
        # ==========================================
        # LOGO ESSALUD - LIMPIO
        # ==========================================
        div(class = "cover-logo-essalud",
          tags$img(src = "ESSALUD.png", alt = "EsSalud")
        ),
        
        # ==========================================
        # CONTENIDO PRINCIPAL
        # ==========================================
        div(class = "cover-content",
          
          # TÍTULOS
          div(class = "cover-title", "TABLERO DE VIGILANCIA"),
          div(class = "cover-title cover-title-secondary", 
              "DE ENFERMEDADES DE NOTIFICACIÓN OBLIGATORIA"),
          
          # SUBTÍTULOS
          div(class = "cover-subtitle", "Brotes y Contingencias en la población asegurada"),
          div(class = "cover-subtitle cover-subtitle-strong", "de EsSalud"),
          
          # PERÍODO
          div(class = "cover-period",
            icon("calendar-alt"), 
            " Actualización: Semana 1 al corte de la semana 24"
          ),
          
          # ==========================================
          # TARJETAS DE ESTADÍSTICAS - KPIS MÁS GRANDES
          # ==========================================
          div(class = "cover-stats",
            div(class = "cover-stat",
              span(class = "stat-icon", icon("virus")),
              span(class = "number", format(total_casos, big.mark = ",")),
              span(class = "label", "TOTAL CASOS")
            ),
            div(class = "cover-stat",
              span(class = "stat-icon", icon("check-circle")),
              span(class = "number", 
                   format(total_confirmados, big.mark = ",")),
              span(class = "label", "CONFIRMADOS")
            ),
            div(class = "cover-stat",
              span(class = "stat-icon", icon("building")),
              span(class = "number", total_redes),
              span(class = "label", "REDES")
            ),
            div(class = "cover-stat",
              span(class = "stat-icon", icon("diagnoses")),
              span(class = "number", total_enfermedades),
              span(class = "label", "DIAGNÓSTICOS")
            )
          ),
          
          # ==========================================
          # BOTONES DE NAVEGACIÓN - ABAJO
          # ==========================================
          div(class = "cover-buttons",
            div(class = "cover-btn",
              onclick = "Shiny.setInputValue('switch_tab', 'tablero', {priority: 'event'})",
              span(class = "icon-wrap", icon("chart-line")),
              span(class = "btn-label", "TABLERO DE CONTROL"),
              span(class = "btn-desc", "Vigilancia epidemiológica")
            ),
            div(class = "cover-btn",
              onclick = "Shiny.setInputValue('switch_tab', 'perfil', {priority: 'event'})",
              span(class = "icon-wrap", icon("user-check")),
              span(class = "btn-label", "CARACTERIZACIÓN"),
              span(class = "btn-desc", "Epidemiológica")
            ),
            div(class = "cover-btn",
              onclick = "Shiny.setInputValue('switch_tab', 'geografico', {priority: 'event'})",
              span(class = "icon-wrap", icon("globe")),
              span(class = "btn-label", "ANÁLISIS GEOGRÁFICO"),
              span(class = "btn-desc", "Distribución espacial")
            ),
            div(class = "cover-btn",
              onclick = "Shiny.setInputValue('switch_tab', 'datos', {priority: 'event'})",
              span(class = "icon-wrap", icon("download")),
              span(class = "btn-label", "DESCARGA DE DATOS"),
              span(class = "btn-desc", "Información")
            )
          )
        ),
        
        # ==========================================
        # FOOTER FIJO CON LOGO OIIS
        # ==========================================
        div(class = "cover-footer",
          div(class = "footer-logo-container",
            div(class = "logo-wrapper",
              tags$img(src = "OIIS.png", alt = "OIIS")
            ),
            div(class = "footer-text-wrapper",
              span(class = "footer-text-title", "OIIS"),
              span(class = "footer-text-subtitle", "Oficina de Inteligencia e Información Sanitaria")
            )
          )
        )
      )
    ),
    
    # PESTAÑA: TABLERO DE CONTROL
    tabPanel(
      title = div(icon("chart-line"), " TABLERO DE CONTROL"),
      value = "tablero",
      mod_tablero_ui("tablero")
    ),
    
    # PESTAÑA: CARACTERIZACIÓN EPIDEMIOLÓGICA
    tabPanel(
      title = div(icon("users"), " CARACTERIZACIÓN EPIDEMIOLÓGICA"),
      value = "perfil",
      mod_perfil_ui("perfil")
    ),
    
    # PESTAÑA: ANÁLISIS GEOGRÁFICO
    tabPanel(
      title = div(icon("globe"), " ANÁLISIS GEOGRÁFICO"),
      value = "geografico",
      mod_geografico_ui("geografico")
    ),
    
    # PESTAÑA: DESCARGA DE DATOS
    tabPanel(
      title = div(icon("download"), " DESCARGA DE DATOS"),
      value = "datos",
      mod_datos_ui("datos")
    )
  )
)

ui <- dashboardPage(
  skin = "blue",
  header = header,
  sidebar = dashboardSidebar(disable = TRUE),
  body = body
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # 🔥 NAVEGACIÓN DESDE BOTONES
  observeEvent(input$switch_tab, {
    updateTabsetPanel(session, "pestanas", selected = input$switch_tab)
  })
  
  # Módulos
  mod_tablero_server("tablero")
  mod_perfil_server("perfil")
  mod_geografico_server("geografico")
  mod_datos_server("datos")
}

# ============================================================
# VALIDACIONES
# ============================================================
if (!exists("ui")) stop("ERROR: No se pudo crear el objeto ui")
if (!exists("server")) stop("ERROR: No se pudo crear el objeto server")

message("✅ Dashboard v4.9 - KPIs MÁS GRANDES - Listo!")
cat("\n🚀 Carátula profesional con KPIs mejorados:\n")
cat("  ✨ Números aumentados de 38px a 56px\n")
cat("  ✨ Tarjetas más altas (180px min-height)\n")
cat("  ✨ Iconos más grandes (42px)\n")
cat("  ✨ Más espacio entre tarjetas (30px gap)\n")
cat("  ✨ Grid más ancho (1300px max-width)\n")
cat("  ✨ Efecto hover con escala (scale 1.02)\n")
cat("  ✨ Línea separadora en labels\n")
cat("  ✨ Padding aumentado en tarjetas\n")
cat("  ✨ Responsive optimizado para todos los tamaños\n\n")

shinyApp(ui = ui, server = server)