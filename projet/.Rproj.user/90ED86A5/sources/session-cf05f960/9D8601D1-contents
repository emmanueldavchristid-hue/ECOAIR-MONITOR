# Chargement des bibliothèques
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(readxl)
library(leaflet)
library(zoo)
library(randomForest)
library(e1071)
library(naivebayes)
library(memoise)

# Packages optionnels
if (!require("shinyWidgets", quietly = TRUE)) {
  cat("Package shinyWidgets non installé. Utilisation des contrôles standard.\n")
  use_shinyWidgets <- FALSE
} else {
  use_shinyWidgets <- TRUE
}

if (!require("shinycssloaders", quietly = TRUE)) {
  cat("Package shinycssloaders non installé. Pas de spinners.\n")
  use_spinners <- FALSE
} else {
  use_spinners <- TRUE
}

# Options globales
options(shiny.maxRequestSize = 30*1024^2)  # 30MB max
options(DT.options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))

# Chargement des données
data <- read.csv("C:/Users/Dell/Desktop/DATA VIZ/data_analyse.csv", stringsAsFactors = FALSE)

# Nettoyage des données
data$date <- as.Date(data$date)
data$pays[data$ville == "Helsinki"] <- "Finlande"
data$pays[data$ville == "Hong Kong"] <- "Chine"
data$pays[data$ville == "Kigali"] <- "Rwanda"
data$pays[data$ville == "Lima"] <- "Pérou"
data$pays[data$ville == "Sofia"] <- "Bulgarie"
data <- data %>% filter(!is.na(date))
data$no2 <- as.numeric(data$no2)
data$o3 <- as.numeric(data$o3)
data$pm25 <- as.numeric(data$pm25)

# Coordonnées pour les villes
ville_coords <- data.frame(
  ville = c("Helsinki", "Hong Kong", "Kigali", "Lima", "Sofia"),
  lat = c(60.17, 22.3, -1.95, -12.05, 42.7),
  lng = c(24.94, 114.2, 30.06, -77.05, 23.32)
)

# Seuils OMS
who_thresholds <- list(pm25 = 15, no2 = 25, o3 = 100)

# Fonction pour créer les features d'ingénierie
create_features <- function(df) {
  df %>%
    arrange(ville, date) %>%
    group_by(ville) %>%
    mutate(
      jour_semaine = weekdays(date),
      mois = month(date),
      saison = case_when(
        mois %in% c(12, 1, 2) ~ "Hiver",
        mois %in% c(3, 4, 5) ~ "Printemps",
        mois %in% c(6, 7, 8) ~ "Été",
        mois %in% c(9, 10, 11) ~ "Automne"
      ),
      pm25_ma7 = lag(zoo::rollmean(pm25, k = 7, fill = NA, align = "right"), 1),
      no2_ma7 = lag(zoo::rollmean(no2, k = 7, fill = NA, align = "right"), 1),
      o3_ma7 = lag(zoo::rollmean(o3, k = 7, fill = NA, align = "right"), 1),
      temp_ma7 = lag(zoo::rollmean(temp_moy, k = 7, fill = NA, align = "right"), 1),
      pm25_trend = pm25 - lag(pm25, 1),
      no2_trend = no2 - lag(no2, 1),
      o3_trend = o3 - lag(o3, 1),
      aqi_index = case_when(
        !is.na(pm25) & !is.na(no2) & !is.na(o3) & 
          pm25 > 0 & no2 > 0 & o3 > 0 ~ (pm25/who_thresholds$pm25 + no2/who_thresholds$no2 + o3/who_thresholds$o3) / 3,
        TRUE ~ NA_real_
      ),
      pm25_classe = case_when(
        pm25 <= who_thresholds$pm25 * 0.5 ~ "Excellent",
        pm25 <= who_thresholds$pm25 ~ "Bon",
        pm25 <= who_thresholds$pm25 * 1.5 ~ "Modéré",
        pm25 <= who_thresholds$pm25 * 2 ~ "Mauvais",
        TRUE ~ "Très mauvais"
      ),
      no2_classe = case_when(
        no2 <= who_thresholds$no2 * 0.5 ~ "Excellent",
        no2 <= who_thresholds$no2 ~ "Bon",
        no2 <= who_thresholds$no2 * 1.5 ~ "Modéré",
        no2 <= who_thresholds$no2 * 2 ~ "Mauvais",
        TRUE ~ "Très mauvais"
      ),
      o3_classe = case_when(
        o3 <= who_thresholds$o3 * 0.5 ~ "Excellent",
        o3 <= who_thresholds$o3 ~ "Bon",
        o3 <= who_thresholds$o3 * 1.5 ~ "Modéré",
        o3 <= who_thresholds$o3 * 2 ~ "Mauvais",
        TRUE ~ "Très mauvais"
      ),
      pm25_depassement = ifelse(pm25 > who_thresholds$pm25, 1, 0),
      no2_depassement = ifelse(no2 > who_thresholds$no2, 1, 0),
      o3_depassement = ifelse(o3 > who_thresholds$o3, 1, 0),
      pm25_dep_7j = lag(zoo::rollsum(pm25_depassement, k = 7, fill = NA, align = "right"), 1),
      no2_dep_7j = lag(zoo::rollsum(no2_depassement, k = 7, fill = NA, align = "right"), 1),
      o3_dep_7j = lag(zoo::rollsum(o3_depassement, k = 7, fill = NA, align = "right"), 1)
    ) %>%
    ungroup()
}

# Version mise en cache
create_features_cached <- memoise(create_features)

# Appliquer l'ingénierie des features
data_enhanced <- create_features_cached(data)

# Pré-calculer les classes de polluants
data_enhanced <- data_enhanced %>%
  mutate(
    pm25_classe = factor(pm25_classe, levels = c("Excellent", "Bon", "Modéré", "Mauvais", "Très mauvais")),
    no2_classe = factor(no2_classe, levels = c("Excellent", "Bon", "Modéré", "Mauvais", "Très mauvais")),
    o3_classe = factor(o3_classe, levels = c("Excellent", "Bon", "Modéré", "Mauvais", "Très mauvais"))
  )

# Fonction optimisée pour calculer les métriques
calculate_metrics_fast <- function(actual, predicted) {
  confusion_matrix <- table(predicted, actual)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  classes <- levels(as.factor(actual))
  n_classes <- length(classes)
  
  precision <- recall <- f1 <- numeric(n_classes)
  
  for (i in seq_along(classes)) {
    tp <- confusion_matrix[classes[i], classes[i]]
    fp <- sum(confusion_matrix[classes[i], ]) - tp
    fn <- sum(confusion_matrix[, classes[i]]) - tp
    
    precision[i] <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall[i] <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1[i] <- ifelse(precision[i] + recall[i] == 0, 0, 
                    2 * precision[i] * recall[i] / (precision[i] + recall[i]))
  }
  
  list(
    accuracy = accuracy,
    precision = mean(precision, na.rm = TRUE),
    recall = mean(recall, na.rm = TRUE),
    f1 = mean(f1, na.rm = TRUE),
    confusion_matrix = confusion_matrix
  )
}

# CSS personnalisé
custom_css <- "
/* Background principal avec dégradé atmosphérique */
body {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  min-height: 100vh;
}

/* Style pour le header principal */
.content-wrapper, .right-side {
  background: transparent !important;
}

.main-header .navbar {
  background: linear-gradient(45deg, #2c3e50, #3498db) !important;
  border: none !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.1);
}

.main-header .navbar-brand {
  color: white !important;
  font-weight: bold;
  font-size: 24px;
}

/* Sidebar moderne */
.main-sidebar {
  background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%) !important;
  box-shadow: 4px 0 20px rgba(0,0,0,0.1);
}

.sidebar-menu > li > a {
  color: #ecf0f1 !important;
  border-left: 3px solid transparent;
  transition: all 0.3s ease;
}

.sidebar-menu > li > a:hover {
  background: rgba(52, 152, 219, 0.2) !important;
  border-left-color: #3498db;
  transform: translateX(5px);
}

.sidebar-menu > li.active > a {
  background: rgba(52, 152, 219, 0.3) !important;
  border-left-color: #3498db;
}

/* Section des contrôles */
.control-section {
  background: rgba(255, 255, 255, 0.1) !important;
  backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 15px;
  padding: 20px;
  margin: 15px;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
}

.control-section h4 {
  color: white !important;
  margin-bottom: 20px !important;
  text-align: center;
  font-weight: 600;
  text-shadow: 0 2px 4px rgba(0,0,0,0.3);
}

/* Cartes de contenu avec effet glassmorphism */
.box {
  background: rgba(255, 255, 255, 0.1) !important;
  backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 20px !important;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
  margin-bottom: 20px;
  transition: transform 0.3s ease, box-shadow 0.3s ease;
}

.box:hover {
  transform: translateY(-5px);
  box-shadow: 0 12px 40px rgba(0, 0, 0, 0.15);
}

.box-header {
  background: transparent !important;
  border-bottom: 1px solid rgba(255, 255, 255, 0.1) !important;
  color: white !important;
}

.box-title {
  color: white !important;
  font-weight: 600;
  font-size: 18px;
}

/* Value boxes avec animations */
.small-box {
  border-radius: 15px !important;
  background: linear-gradient(45deg, rgba(255,255,255,0.1), rgba(255,255,255,0.05)) !important;
  backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: all 0.3s ease;
  animation: fadeInUp 0.6s ease-out;
}

.small-box:hover {
  transform: translateY(-10px) scale(1.02);
  box-shadow: 0 15px 35px rgba(0, 0, 0, 0.2);
}

.small-box h3, .small-box p {
  color: white !important;
}

.small-box .icon {
  color: rgba(255, 255, 255, 0.3) !important;
}

/* Animations */
@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(30px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.fade-in {
  animation: fadeInUp 0.8s ease-out;
}

/* Améliorations pour les contrôles de saisie */
.form-group {
  margin-bottom: 20px;
}

.form-group label {
  color: white !important;
  font-weight: 500;
  margin-bottom: 8px;
  display: block;
  font-size: 14px;
}

/* Contrôles standard */
.form-control {
  background: rgba(255, 255, 255, 0.9) !important;
  border: 2px solid rgba(255, 255, 255, 0.3) !important;
  color: #2c3e50 !important;
  border-radius: 10px !important;
  padding: 8px 12px;
  font-size: 14px;
  transition: all 0.3s ease;
}

.form-control:focus {
  background: rgba(255, 255, 255, 0.95) !important;
  border-color: #3498db !important;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25) !important;
  color: #2c3e50 !important;
}

.form-control option {
  background: white !important;
  color: #2c3e50 !important;
}

/* Contrôles shinyWidgets */
.bootstrap-select .dropdown-toggle {
  background: rgba(255, 255, 255, 0.9) !important;
  border: 2px solid rgba(255, 255, 255, 0.3) !important;
  color: #2c3e50 !important;
  border-radius: 10px !important;
  padding: 8px 12px;
  font-size: 14px;
  transition: all 0.3s ease;
}

.bootstrap-select .dropdown-toggle:focus,
.bootstrap-select .dropdown-toggle:hover {
  background: rgba(255, 255, 255, 0.95) !important;
  border-color: #3498db !important;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25) !important;
}

.bootstrap-select .dropdown-menu {
  background: rgba(255, 255, 255, 0.95) !important;
  backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: 10px;
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
}

.bootstrap-select .dropdown-menu li a {
  color: #2c3e50 !important;
  transition: all 0.2s ease;
}

.bootstrap-select .dropdown-menu li a:hover,
.bootstrap-select .dropdown-menu li.active a {
  background: rgba(52, 152, 219, 0.1) !important;
  color: #3498db !important;
}

/* Contrôles de date */
.shiny-date-input input {
  background: rgba(255, 255, 255, 0.9) !important;
  border: 2px solid rgba(255, 255, 255, 0.3) !important;
  color: #2c3e50 !important;
  border-radius: 10px !important;
  padding: 8px 12px;
  font-size: 14px;
}

.shiny-date-input input:focus {
  background: rgba(255, 255, 255, 0.95) !important;
  border-color: #3498db !important;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25) !important;
}

/* Selectize input */
.selectize-input {
  background: rgba(255, 255, 255, 0.9) !important;
  border: 2px solid rgba(255, 255, 255, 0.3) !important;
  color: #2c3e50 !important;
  border-radius: 10px !important;
  padding: 8px 12px;
  font-size: 14px;
}

.selectize-input.focus {
  background: rgba(255, 255, 255, 0.95) !important;
  border-color: #3498db !important;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25) !important;
}

.selectize-dropdown {
  background: rgba(255, 255, 255, 0.95) !important;
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: 10px;
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
}

.selectize-dropdown-content .option {
  color: #2c3e50 !important;
}

.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active {
  background: rgba(52, 152, 219, 0.1) !important;
  color: #3498db !important;
}

/* Graphiques avec bordures stylisées */
.plotly {
  border-radius: 15px;
  overflow: hidden;
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.1);
}

/* Leaflet map styling */
.leaflet-container {
  border-radius: 15px !important;
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15) !important;
}

/* DataTable styling */
.dataTables_wrapper {
  color: white !important;
}

.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select {
  background: rgba(255, 255, 255, 0.9) !important;
  border: 2px solid rgba(255, 255, 255, 0.3) !important;
  color: #2c3e50 !important;
  border-radius: 8px !important;
  padding: 6px 10px;
}

/* Loader styling */
.load-container {
  color: white !important;
}

/* Titre principal avec effet néon */
.main-title {
  text-align: center;
  color: white;
  font-size: 2.5em;
  font-weight: bold;
  text-shadow: 0 0 20px rgba(52, 152, 219, 0.5);
  margin-bottom: 30px;
  animation: glow 2s ease-in-out infinite alternate;
}

@keyframes glow {
  from { text-shadow: 0 0 20px rgba(52, 152, 219, 0.5); }
  to { text-shadow: 0 0 30px rgba(52, 152, 219, 0.8), 0 0 40px rgba(52, 152, 219, 0.3); }
}

/* Particules d'arrière-plan */
.particles {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  pointer-events: none;
  z-index: -1;
}

.particle {
  position: absolute;
  width: 4px;
  height: 4px;
  background: rgba(255, 255, 255, 0.3);
  border-radius: 50%;
  animation: float 6s ease-in-out infinite;
}

@keyframes float {
  0%, 100% { transform: translateY(0px); opacity: 0.3; }
  50% { transform: translateY(-20px); opacity: 0.8; }
}
"

# Interface utilisateur avec shinydashboard
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$i(class = "fa fa-leaf", style = "margin-right: 10px; font-size: 20px;"),
      "EcoAir Monitor"
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("📊 Vue d'ensemble", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("📈 Séries Temporelles", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("📦 Distributions", tabName = "distributions", icon = icon("chart-bar")),
      menuItem("🗺️ Cartographie", tabName = "maps", icon = icon("map-marked-alt")),
      menuItem("⚠️ Alertes", tabName = "alerts", icon = icon("exclamation-triangle")),
      menuItem("🤖 Prédictions IA", tabName = "predictions", icon = icon("robot")),
      menuItem("🔮 Prédictions Linéaires", tabName = "linear_predictions", icon = icon("chart-area")),
      menuItem("📈 Performance Modèles", tabName = "model_performance", icon = icon("chart-pie"))
    ),
    div(class = "control-section",
        h4("🎛️ Contrôles"),
        
        # Ville
        div(class = "form-group",
            if(use_shinyWidgets) {
              pickerInput("city", 
                          label = "🏙️ Sélectionnez une ville :",
                          choices = setNames(unique(data$ville), 
                                             paste("📍", unique(data$ville))),
                          selected = "Helsinki",
                          options = list(
                            style = "btn-outline-primary",
                            size = 5
                          ))
            } else {
              selectInput("city", 
                          label = "🏙️ Sélectionnez une ville :",
                          choices = setNames(unique(data$ville), 
                                             paste("📍", unique(data$ville))),
                          selected = "Helsinki")
            }
        ),
        
        # Polluant
        div(class = "form-group",
            if(use_shinyWidgets) {
              pickerInput("pollutant", 
                          label = "🌫️ Sélectionnez un polluant :",
                          choices = c("🌫️ PM2.5 (Particules fines)" = "pm25", 
                                      "🏭 NO2 (Dioxyde d'azote)" = "no2", 
                                      "💨 O3 (Ozone)" = "o3"),
                          selected = "pm25",
                          options = list(
                            style = "btn-outline-primary",
                            size = 5
                          ))
            } else {
              selectInput("pollutant", 
                          label = "🌫️ Sélectionnez un polluant :",
                          choices = c("🌫️ PM2.5 (Particules fines)" = "pm25", 
                                      "🏭 NO2 (Dioxyde d'azote)" = "no2", 
                                      "💨 O3 (Ozone)" = "o3"),
                          selected = "pm25")
            }
        ),
        
        # Modèle de classification
        div(class = "form-group",
            if(use_shinyWidgets) {
              pickerInput("model_type", 
                          label = "🤖 Modèle de classification :",
                          choices = c("🌲 Random Forest" = "rf", 
                                      "🔢 SVM" = "svm", 
                                      "📊 Naive Bayes" = "nb"),
                          selected = "rf",
                          options = list(
                            style = "btn-outline-success",
                            size = 5
                          ))
            } else {
              selectInput("model_type", 
                          label = "🤖 Modèle de classification :",
                          choices = c("🌲 Random Forest" = "rf", 
                                      "🔢 SVM" = "svm", 
                                      "📊 Naive Bayes" = "nb"),
                          selected = "rf")
            }
        ),
        
        # Date
        div(class = "form-group",
            dateRangeInput("date_range", 
                           label = "📅 Sélectionnez la période :",
                           start = min(data$date), 
                           end = max(data$date),
                           min = min(data$date), 
                           max = max(data$date),
                           separator = " à ",
                           language = "fr",
                           format = "dd/mm/yyyy")
        )
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css)),
      tags$script(HTML("
        // Créer des particules flottantes
        $(document).ready(function() {
          var particles = $('<div class=\"particles\"></div>');
          $('body').prepend(particles);
          
          for(var i = 0; i < 50; i++) {
            var particle = $('<div class=\"particle\"></div>');
            particle.css({
              'left': Math.random() * 100 + '%',
              'top': Math.random() * 100 + '%',
              'animation-delay': Math.random() * 6 + 's',
              'animation-duration': (3 + Math.random() * 6) + 's'
            });
            particles.append(particle);
          }
        });
      "))
    ),
    
    tabItems(
      # Vue d'ensemble
      tabItem("overview",
              div(class = "main-title", "🌍 Qualité de l'Air - Prédiction"),
              
              fluidRow(
                valueBoxOutput("pm25_box", width = 4),
                valueBoxOutput("no2_box", width = 4),
                valueBoxOutput("o3_box", width = 4)
              ),
              
              fluidRow(
                box(title = "📊 Résumé des Données", status = "primary", solidHeader = TRUE, width = 12,
                    if(use_spinners) withSpinner(DTOutput("summary_overview"), color = "#3498db") else DTOutput("summary_overview")
                )
              )
      ),
      
      # Séries temporelles
      tabItem("timeseries",
              fluidRow(
                box(title = "📈 Évolution Temporelle", status = "primary", solidHeader = TRUE, width = 12,
                    if(use_spinners) withSpinner(plotlyOutput("time_series_plot", height = "500px"), color = "#3498db") else plotlyOutput("time_series_plot", height = "500px")
                )
              )
      ),
      
      # Distributions
      tabItem("distributions",
              fluidRow(
                box(title = "📦 Distribution par Ville", status = "primary", solidHeader = TRUE, width = 12,
                    if(use_spinners) withSpinner(plotlyOutput("boxplot", height = "500px"), color = "#3498db") else plotlyOutput("boxplot", height = "500px")
                )
              )
      ),
      
      # Carte
      tabItem("maps",
              fluidRow(
                box(title = "🗺️ Carte Interactive", status = "primary", solidHeader = TRUE, width = 12,
                    if(use_spinners) withSpinner(leafletOutput("big_map", height = "600px"), color = "#3498db") else leafletOutput("big_map", height = "600px")
                )
              )
      ),
      
      # Alertes
      tabItem("alerts",
              fluidRow(
                box(title = "⚠️ Dépassements des Seuils OMS", status = "warning", solidHeader = TRUE, width = 12,
                    if(use_spinners) withSpinner(plotlyOutput("exceedance_plot", height = "500px"), color = "#f39c12") else plotlyOutput("exceedance_plot", height = "500px")
                )
              )
      ),
      
      # Prédictions IA
      tabItem("predictions",
              fluidRow(
                valueBoxOutput("accuracy_box", width = 3),
                valueBoxOutput("precision_box", width = 3),
                valueBoxOutput("recall_box", width = 3),
                valueBoxOutput("f1_box", width = 3)
              ),
              fluidRow(
                box(title = "🎯 Matrice de Confusion", status = "success", solidHeader = TRUE, width = 6,
                    if(use_spinners) withSpinner(plotlyOutput("confusion_matrix", height = "400px"), color = "#27ae60") else plotlyOutput("confusion_matrix", height = "400px")
                ),
                box(title = "🔮 Prédictions vs Réalité", status = "info", solidHeader = TRUE, width = 6,
                    if(use_spinners) withSpinner(plotlyOutput("prediction_comparison", height = "400px"), color = "#3498db") else plotlyOutput("prediction_comparison", height = "400px")
                )
              ),
              fluidRow(
                box(title = "📊 Importance des Variables", status = "primary", solidHeader = TRUE, width = 12,
                    if(use_spinners) withSpinner(plotlyOutput("feature_importance", height = "400px"), color = "#3498db") else plotlyOutput("feature_importance", height = "400px")
                )
              )
      ),
      
      # Prédictions linéaires
      tabItem("linear_predictions",
              fluidRow(
                valueBoxOutput("prediction_box", width = 4),
                valueBoxOutput("correlation_box", width = 4),
                valueBoxOutput("r_squared_box", width = 4)
              ),
              fluidRow(
                box(title = "🔮 Relation Polluant vs Température", status = "success", solidHeader = TRUE, width = 12,
                    if(use_spinners) withSpinner(plotlyOutput("prediction_plot", height = "500px"), color = "#27ae60") else plotlyOutput("prediction_plot", height = "500px")
                )
              )
      ),
      
      # Performance des modèles
      tabItem("model_performance",
              fluidRow(
                box(title = "📈 Comparaison des Modèles", status = "primary", solidHeader = TRUE, width = 12,
                    if(use_spinners) withSpinner(plotlyOutput("model_comparison_plot", height = "500px"), color = "#3498db") else plotlyOutput("model_comparison_plot", height = "500px")
                )
              )
      )
    )
  )
)

# Fonction server
server <- function(input, output, session) {
  model_cache <- reactiveValues()
  
  filtered_data <- reactive({
    data %>% filter(ville == input$city, date >= input$date_range[1], date <= input$date_range[2])
  }) %>% debounce(500)
  
  trained_model <- reactive({
    cache_key <- paste(input$city, input$pollutant, input$model_type, 
                       input$date_range[1], input$date_range[2], sep = "_")
    
    if (!is.null(model_cache[[cache_key]])) {
      return(model_cache[[cache_key]])
    }
    
    df <- filtered_data()
    
    if (nrow(df) < 50) {
      return(NULL)
    }
    
    target_var <- paste0(input$pollutant, "_classe")
    
    if (!target_var %in% names(data_enhanced)) {
      return(NULL)
    }
    
    withProgress(message = "Entraînement du modèle IA...", value = 0, {
      incProgress(0.2, detail = "Préparation des données")
      
      model_data <- data_enhanced %>%
        filter(ville == input$city, 
               date >= input$date_range[1], 
               date <= input$date_range[2]) %>%
        select(temp_moy, mois, aqi_index, all_of(target_var)) %>%
        filter(complete.cases(.))
      
      # Diagnostic : Vérifier les données
      print("Résumé de model_data avant entraînement :")
      print(summary(model_data))
      if (any(is.na(model_data)) || any(is.infinite(as.matrix(model_data[, c("temp_moy", "mois", "aqi_index")])))) {
        print("Valeurs problématiques détectées dans model_data :")
        print(model_data[!complete.cases(model_data) | 
                           apply(model_data[, c("temp_moy", "mois", "aqi_index")], 1, function(x) any(is.infinite(x))), ])
        return(NULL)
      }
      
      incProgress(0.4, detail = "Configuration du modèle")
      
      if (nrow(model_data) < 30) {
        incProgress(1)
        return(NULL)
      }
      
      X <- model_data[, c("temp_moy", "mois", "aqi_index"), drop = FALSE]
      y <- as.factor(model_data[[target_var]])
      
      class_counts <- table(y)
      if (any(class_counts == 0)) {
        incProgress(1, detail = "Classes vides détectées")
        return(NULL)
      }
      
      incProgress(0.6, detail = "Division train/test")
      
      set.seed(42)
      train_idx <- sample(nrow(model_data), floor(0.8 * nrow(model_data)))
      
      X_train <- X[train_idx, , drop = FALSE]
      X_test <- X[-train_idx, , drop = FALSE]
      y_train <- y[train_idx]
      y_test <- y[-train_idx]
      
      incProgress(0.8, detail = "Entraînement en cours...")
      
      model <- NULL
      predictions <- NULL
      
      if (input$model_type == "rf") {
        model <- randomForest(x = X_train, y = y_train, 
                              ntree = 50, nodesize = 5, maxnodes = 20)
        predictions <- predict(model, X_test)
      } else if (input$model_type == "svm") {
        model <- svm(x = X_train, y = y_train, 
                     kernel = "radial", cost = 1, gamma = "scale")
        predictions <- predict(model, X_test)
      } else if (input$model_type == "nb") {
        model <- naiveBayes(x = X_train, y = y_train)
        predictions <- predict(model, X_test)
      }
      
      incProgress(0.9, detail = "Calcul des métriques")
      
      metrics <- calculate_metrics_fast(y_test, predictions)
      
      incProgress(1, detail = "Terminé!")
      
      result <- list(
        model = model,
        metrics = metrics,
        predictions = predictions,
        actual = y_test,
        features = c("temp_moy", "mois", "aqi_index"),
        model_type = input$model_type,
        target_var = target_var
      )
      
      model_cache[[cache_key]] <- result
      return(result)
    })
  })
  
  model_comparison <- reactive({
    df <- filtered_data()
    
    if (nrow(df) < 50) {
      return(NULL)
    }
    
    target_var <- paste0(input$pollutant, "_classe")
    
    if (!target_var %in% names(data_enhanced)) {
      return(NULL)
    }
    
    withProgress(message = "Comparaison des modèles...", value = 0, {
      incProgress(0.2, detail = "Préparation des données")
      
      model_data <- data_enhanced %>%
        filter(ville == input$city, 
               date >= input$date_range[1], 
               date <= input$date_range[2]) %>%
        select(temp_moy, mois, aqi_index, all_of(target_var)) %>%
        filter(complete.cases(.))
      
      # Diagnostic : Vérifier les données
      print("Résumé de model_data pour comparaison :")
      print(summary(model_data))
      if (any(is.na(model_data)) || any(is.infinite(as.matrix(model_data[, c("temp_moy", "mois", "aqi_index")])))) {
        print("Valeurs problématiques détectées dans model_data :")
        print(model_data[!complete.cases(model_data) | 
                           apply(model_data[, c("temp_moy", "mois", "aqi_index")], 1, function(x) any(is.infinite(x))), ])
        return(NULL)
      }
      
      incProgress(0.4, detail = "Vérification des données")
      
      if (nrow(model_data) < 30) {
        incProgress(1)
        return(NULL)
      }
      
      X <- model_data[, c("temp_moy", "mois", "aqi_index"), drop = FALSE]
      y <- as.factor(model_data[[target_var]])
      
      class_counts <- table(y)
      if (any(class_counts == 0)) {
        incProgress(1, detail = "Classes vides détectées")
        return(NULL)
      }
      
      incProgress(0.6, detail = "Division train/test")
      
      set.seed(42)
      train_idx <- sample(nrow(model_data), floor(0.8 * nrow(model_data)))
      
      X_train <- X[train_idx, , drop = FALSE]
      X_test <- X[-train_idx, , drop = FALSE]
      y_train <- y[train_idx]
      y_test <- y[-train_idx]
      
      incProgress(0.8, detail = "Entraînement des modèles")
      
      results <- list()
      
      rf_model <- randomForest(x = X_train, y = y_train, 
                               ntree = 50, nodesize = 5, maxnodes = 20)
      rf_pred <- predict(rf_model, X_test)
      results[["rf"]] <- calculate_metrics_fast(y_test, rf_pred)
      
      svm_model <- tryCatch({
        svm(x = X_train, y = y_train, 
            kernel = "radial", cost = 1, gamma = "scale")
      }, error = function(e) {
        print(paste("Erreur SVM :", e$message))
        NULL
      })
      if (!is.null(svm_model)) {
        svm_pred <- predict(svm_model, X_test)
        results[["svm"]] <- calculate_metrics_fast(y_test, svm_pred)
      } else {
        results[["svm"]] <- list(accuracy = 0, precision = 0, recall = 0, f1 = 0)
      }
      
      nb_model <- naiveBayes(x = X_train, y = y_train)
      nb_pred <- predict(nb_model, X_test)
      results[["nb"]] <- calculate_metrics_fast(y_test, nb_pred)
      
      incProgress(1, detail = "Terminé")
      
      comparison_df <- data.frame(
        Model = c("Random Forest", "SVM", "Naive Bayes"),
        Accuracy = c(results$rf$accuracy, results$svm$accuracy, results$nb$accuracy) * 100,
        Precision = c(results$rf$precision, results$svm$precision, results$nb$precision) * 100,
        Recall = c(results$rf$recall, results$svm$recall, results$nb$recall) * 100,
        F1 = c(results$rf$f1, results$svm$f1, results$nb$f1) * 100
      )
      
      return(comparison_df)
    })
  })
  
  output$pm25_box <- renderValueBox({
    df <- filtered_data()
    avg_val <- round(mean(df$pm25, na.rm = TRUE), 1)
    status_color <- if(avg_val <= who_thresholds$pm25) "green" else if(avg_val <= who_thresholds$pm25 * 1.5) "yellow" else "red"
    
    valueBox(
      value = paste0(avg_val, " µg/m³"),
      subtitle = "PM2.5 Moyen",
      icon = icon("smog"),
      color = status_color
    )
  })
  
  output$no2_box <- renderValueBox({
    df <- filtered_data()
    avg_val <- round(mean(df$no2, na.rm = TRUE), 1)
    status_color <- if(avg_val <= who_thresholds$no2) "green" else if(avg_val <= who_thresholds$no2 * 1.5) "yellow" else "red"
    
    valueBox(
      value = paste0(avg_val, " µg/m³"),
      subtitle = "NO2 Moyen",
      icon = icon("industry"),
      color = status_color
    )
  })
  
  output$o3_box <- renderValueBox({
    df <- filtered_data()
    avg_val <- round(mean(df$o3, na.rm = TRUE), 1)
    status_color <- if(avg_val <= who_thresholds$o3) "green" else if(avg_val <= who_thresholds$o3 * 1.5) "yellow" else "red"
    
    valueBox(
      value = paste0(avg_val, " µg/m³"),
      subtitle = "O3 Moyen",
      icon = icon("wind"),
      color = status_color
    )
  })
  
  output$accuracy_box <- renderValueBox({
    model_result <- trained_model()
    if (!is.null(model_result)) {
      accuracy <- round(model_result$metrics$accuracy * 100, 1)
      color <- if(accuracy > 80) "green" else if(accuracy > 60) "yellow" else "red"
      
      valueBox(
        value = paste0(accuracy, "%"),
        subtitle = "🎯 Précision du Modèle",
        icon = icon("bullseye"),
        color = color
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Données insuffisantes ou classes vides",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$precision_box <- renderValueBox({
    model_result <- trained_model()
    if (!is.null(model_result)) {
      precision <- round(model_result$metrics$precision * 100, 1)
      color <- if(precision > 80) "green" else if(precision > 60) "yellow" else "red"
      
      valueBox(
        value = paste0(precision, "%"),
        subtitle = "📊 Précision Moyenne",
        icon = icon("chart-bar"),
        color = color
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Données insuffisantes ou classes vides",
        icon = icon("times"),
        color = "red"
      )
    }
  })
  
  output$recall_box <- renderValueBox({
    model_result <- trained_model()
    if (!is.null(model_result)) {
      recall <- round(model_result$metrics$recall * 100, 1)
      color <- if(recall > 80) "green" else if(recall > 60) "yellow" else "red"
      
      valueBox(
        value = paste0(recall, "%"),
        subtitle = "🔍 Rappel Moyen",
        icon = icon("search"),
        color = color
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Données insuffisantes ou classes vides",
        icon = icon("times"),
        color = "red"
      )
    }
  })
  
  output$f1_box <- renderValueBox({
    model_result <- trained_model()
    if (!is.null(model_result)) {
      f1 <- round(model_result$metrics$f1 * 100, 1)
      color <- if(f1 > 80) "green" else if(f1 > 60) "yellow" else "red"
      
      valueBox(
        value = paste0(f1, "%"),
        subtitle = "⚖️ Score F1",
        icon = icon("balance-scale"),
        color = color
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Données insuffisantes ou classes vides",
        icon = icon("times"),
        color = "red"
      )
    }
  })
  
  output$time_series_plot <- renderPlotly({
    df <- filtered_data()
    threshold <- who_thresholds[[input$pollutant]]
    
    if (!is.null(df) && nrow(df) > 0 && sum(!is.na(df[[input$pollutant]])) > 0) {
      p <- plot_ly(df, x = ~date, y = as.formula(paste0("~", input$pollutant)),
                   type = "scatter", mode = "lines+markers", 
                   name = toupper(input$pollutant),
                   line = list(color = '#3498db', width = 3),
                   marker = list(size = 6, color = '#e74c3c')) %>%
        layout(
          title = list(
            text = paste("🌫️ Évolution de", toupper(input$pollutant), "à", input$city),
            font = list(size = 16, color = 'white'),
            x = 0.5, xanchor = 'center'
          ),
          xaxis = list(
            title = list(
              text = "📅 Date",
              font = list(size = 12, color = 'white')
            ),
            tickfont = list(size = 10, color = 'white')
          ),
          yaxis = list(
            title = list(
              text = paste(toupper(input$pollutant), "(µg/m³)"),
              font = list(size = 12, color = 'white')
            ),
            tickfont = list(size = 10, color = 'white')
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(size = 12, color = 'white'),
          hovermode = 'x unified',
          shapes = list(
            list(
              type = "line",
              x0 = min(df$date, na.rm = TRUE),
              x1 = max(df$date, na.rm = TRUE),
              y0 = threshold,
              y1 = threshold,
              line = list(color = "red", dash = "dash", width = 2)
            )
          ),
          margin = list(l = 60, r = 40, t = 60, b = 60)
        )
      p
    } else {
      plot_ly(type = "scatter", mode = "text") %>%
        add_text(x = 0.5, y = 0.5, text = "❌ Données insuffisantes pour afficher la série temporelle", 
                 textfont = list(size = 16, color = 'white')) %>%
        layout(
          title = list(text = "", font = list(size = 16, color = 'white')),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    }
  })
  
  output$boxplot <- renderPlotly({
    df <- data %>% filter(date >= input$date_range[1], date <= input$date_range[2])
    
    if (!is.null(df) && nrow(df) > 0 && sum(!is.na(df[[input$pollutant]])) > 0) {
      plot_ly(df, x = ~ville, y = as.formula(paste0("~", input$pollutant)),
              type = "box", 
              marker = list(color = '#3498db'),
              line = list(color = '#2980b9')) %>%
        layout(
          title = list(text = paste("📦 Distribution de", toupper(input$pollutant), "par ville"),
                       font = list(size = 16, color = 'white')),
          xaxis = list(title = "🏙️ Ville", color = 'white'),
          yaxis = list(title = paste(toupper(input$pollutant), "(µg/m³)"), color = 'white'),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    } else {
      plot_ly(type = "scatter", mode = "text") %>%
        add_text(x = 0.5, y = 0.5, text = "❌ Données insuffisantes pour afficher la distribution", 
                 textfont = list(size = 16, color = 'white')) %>%
        layout(
          title = list(text = "", font = list(size = 16, color = 'white')),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    }
  })
  
  output$big_map <- renderLeaflet({
    selected_pollutant <- input$pollutant
    seuil <- who_thresholds[[selected_pollutant]]
    
    latest_data <- data %>%
      filter(date >= input$date_range[1], date <= input$date_range[2]) %>%
      group_by(ville) %>%
      summarise(valeur = mean(get(selected_pollutant), na.rm = TRUE)) %>%
      left_join(ville_coords, by = "ville") %>%
      mutate(couleur = case_when(
        valeur <= seuil ~ "green",
        valeur <= seuil * 1.5 ~ "orange",
        valeur > seuil * 1.5 ~ "red",
        TRUE ~ "gray"
      ))
    
    leaflet(latest_data) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        label = ~paste0(ville, ": ", round(valeur, 1), " µg/m³"),
        popup = ~paste0("<b>", ville, "</b><br/>",
                        toupper(selected_pollutant), ": ", round(valeur, 1), " µg/m³<br/>",
                        "Seuil OMS: ", seuil, " µg/m³"),
        radius = 15, 
        color = "white",
        fillColor = ~couleur, 
        fillOpacity = 0.8,
        weight = 2
      ) %>%
      addLegend("bottomright", 
                colors = c("green", "orange", "red"),
                labels = c("✅ Bon", "⚠️ Modéré", "🚨 Élevé"),
                title = paste("Niveau de", toupper(selected_pollutant)),
                opacity = 0.8)
  })
  
  output$exceedance_plot <- renderPlotly({
    df <- data %>% 
      filter(date >= input$date_range[1], date <= input$date_range[2]) %>%
      group_by(ville) %>%
      summarise(
        PM2.5 = sum(pm25 > who_thresholds$pm25, na.rm = TRUE),
        NO2 = sum(no2 > who_thresholds$no2, na.rm = TRUE),
        O3 = sum(o3 > who_thresholds$o3, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = -ville, names_to = "Polluant", values_to = "Jours")
    
    if (!is.null(df) && nrow(df) > 0 && sum(df$Jours, na.rm = TRUE) > 0) {
      plot_ly(df, x = ~ville, y = ~Jours, color = ~Polluant, type = "bar",
              colors = c('#e74c3c', '#f39c12', '#9b59b6')) %>%
        layout(
          title = list(text = "⚠️ Jours de dépassement des seuils OMS",
                       font = list(size = 16, color = 'white')),
          barmode = "group",
          xaxis = list(title = "🏙️ Ville", color = 'white'),
          yaxis = list(title = "📊 Nombre de jours", color = 'white'),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    } else {
      plot_ly(type = "scatter", mode = "text") %>%
        add_text(x = 0.5, y = 0.5, text = "❌ Aucun dépassement détecté ou données insuffisantes", 
                 textfont = list(size = 16, color = 'white')) %>%
        layout(
          title = list(text = "", font = list(size = 16, color = 'white')),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    }
  })
  
  output$summary_overview <- renderDT({
    filtered_data() %>%
      select(no2, o3, pm25) %>%
      summarise_all(list(
        Moyenne = ~round(mean(., na.rm = TRUE), 1),
        Médiane = ~round(median(., na.rm = TRUE), 1),
        Min = ~round(min(., na.rm = TRUE), 1),
        Max = ~round(max(., na.rm = TRUE), 1),
        `Écart-type` = ~round(sd(., na.rm = TRUE), 1)
      )) %>%
      pivot_longer(everything(), names_to = c("Polluant", "Métrique"), 
                   names_sep = "_", values_to = "Valeur") %>%
      pivot_wider(names_from = Métrique, values_from = Valeur) %>%
      mutate(Polluant = case_when(
        Polluant == "pm25" ~ "PM2.5",
        Polluant == "no2" ~ "NO2",
        Polluant == "o3" ~ "O3"
      )) %>%
      datatable(
        options = list(
          pageLength = 5,
          dom = 't',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE,
        caption = paste("📊 Statistiques pour", input$city)
      ) %>%
      formatStyle(columns = 1:6, backgroundColor = 'rgba(255,255,255,0.1)', color = 'white')
  })
  
  prediction_data <- reactive({
    df <- filtered_data()
    df <- df %>% filter(!is.na(temp_moy), !is.na(get(input$pollutant)))
    
    if(nrow(df) > 5) {
      model <- lm(get(input$pollutant) ~ temp_moy, data = df)
      list(
        model = model,
        correlation = cor(df$temp_moy, df[[input$pollutant]], use = "complete.obs"),
        r_squared = summary(model)$r.squared,
        prediction_20c = predict(model, newdata = data.frame(temp_moy = 20))
      )
    } else {
      NULL
    }
  })
  
  output$prediction_box <- renderValueBox({
    pred_data <- prediction_data()
    if(!is.null(pred_data)) {
      pred_val <- round(pred_data$prediction_20c, 1)
      valueBox(
        value = paste0(pred_val, " µg/m³"),
        subtitle = paste("Prédiction à 20°C -", toupper(input$pollutant)),
        icon = icon("thermometer-half"),
        color = "green"
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Données insuffisantes",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$correlation_box <- renderValueBox({
    pred_data <- prediction_data()
    if(!is.null(pred_data)) {
      corr_val <- round(pred_data$correlation, 3)
      color_corr <- if(abs(corr_val) > 0.7) "green" else if(abs(corr_val) > 0.4) "yellow" else "red"
      valueBox(
        value = corr_val,
        subtitle = "Corrélation Temp/Pollution",
        icon = icon("link"),
        color = color_corr
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Corrélation indisponible",
        icon = icon("unlink"),
        color = "red"
      )
    }
  })
  
  output$r_squared_box <- renderValueBox({
    pred_data <- prediction_data()
    if(!is.null(pred_data)) {
      r2_val <- round(pred_data$r_squared * 100, 1)
      color_r2 <- if(r2_val > 50) "green" else if(r2_val > 25) "yellow" else "red"
      valueBox(
        value = paste0(r2_val, "%"),
        subtitle = "Variance Expliquée (R²)",
        icon = icon("percentage"),
        color = color_r2
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "R² indisponible",
        icon = icon("question"),
        color = "red"
      )
    }
  })
  
  output$prediction_plot <- renderPlotly({
    pred_data <- prediction_data()
    df <- filtered_data()
    df <- df %>% filter(!is.na(temp_moy), !is.na(get(input$pollutant)))
    
    if (!is.null(pred_data) && nrow(df) > 5) {
      df$pred <- predict(pred_data$model, newdata = df)
      
      plot_ly(df, x = ~temp_moy, y = as.formula(paste0("~", input$pollutant)), 
              type = 'scatter', mode = 'markers', name = "🔍 Observé",
              marker = list(color = '#e74c3c', size = 8)) %>%
        add_lines(x = ~temp_moy, y = ~pred, name = "📈 Prédit",
                  line = list(color = '#27ae60', width = 3)) %>%
        layout(
          title = list(text = paste("🔮 Relation", toupper(input$pollutant), "vs Température"),
                       font = list(size = 20, color = 'white')),
          xaxis = list(title = "🌡️ Température moyenne (°C)", color = 'white'),
          yaxis = list(title = paste(toupper(input$pollutant), "(µg/m³)"), color = 'white'),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white'),
          annotations = list(
            list(
              x = 0.02, y = 0.98,
              xref = 'paper', yref = 'paper',
              text = paste0("R² = ", round(pred_data$r_squared, 3), 
                            "<br>Corrélation = ", round(pred_data$correlation, 3)),
              showarrow = FALSE,
              font = list(color = 'white', size = 12),
              bgcolor = 'rgba(0,0,0,0.3)',
              bordercolor = 'white',
              borderwidth = 1
            )
          )
        )
    } else {
      plot_ly(type = "scatter", mode = "text") %>%
        add_text(x = 0.5, y = 0.5, text = "❌ Données insuffisantes pour la prédiction", 
                 textfont = list(size = 16, color = 'white')) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = 'white')),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    }
  })
  
  output$confusion_matrix <- renderPlotly({
    model_result <- trained_model()
    if (!is.null(model_result)) {
      cm <- model_result$metrics$confusion_matrix
      cm_df <- as.data.frame(as.table(cm))
      names(cm_df) <- c("Prédit", "Réel", "Fréquence")
      
      plot_ly(cm_df, x = ~Réel, y = ~Prédit, z = ~Fréquence,
              type = "heatmap", 
              colorscale = list(c(0, "white"), c(1, "#3498db")),
              hovertemplate = "Réel: %{x}<br>Prédit: %{y}<br>Nombre: %{z}<extra></extra>") %>%
        layout(
          title = list(text = "🎯 Matrice de Confusion", 
                       font = list(size = 16, color = 'white')),
          xaxis = list(title = "Classes Réelles", color = 'white'),
          yaxis = list(title = "Classes Prédites", color = 'white'),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    } else {
      plot_ly() %>%
        layout(
          title = list(text = "❌ Modèle non disponible (données insuffisantes ou classes vides)", 
                       font = list(size = 16, color = 'white')),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    }
  })
  
  output$prediction_comparison <- renderPlotly({
    model_result <- trained_model()
    if (!is.null(model_result)) {
      comparison_df <- data.frame(
        Index = seq_along(model_result$actual),
        Réel = as.character(model_result$actual),
        Prédit = as.character(model_result$predictions),
        Correct = model_result$actual == model_result$predictions
      )
      
      # Créer customdata comme une liste pour chaque point
      custom_data <- lapply(seq_len(nrow(comparison_df)), function(i) {
        list(Réel = comparison_df$Réel[i], Prédit = comparison_df$Prédit[i])
      })
      
      plot_ly(comparison_df, 
              x = ~Index, 
              y = ~Correct, 
              type = "bar",
              marker = list(color = ~Correct, 
                            colorscale = list(list(0, "#e74c3c"), list(1, "#27ae60"))),
              hovertemplate = "Index: %{x}<br>Réel: %{customdata.Réel}<br>Prédit: %{customdata.Prédit}<extra></extra>",
              customdata = custom_data) %>%
        layout(
          title = list(text = "🔮 Comparaison Prédictions vs Réalité", 
                       font = list(size = 16, color = 'white')),
          xaxis = list(title = "Index des Échantillons", color = 'white'),
          yaxis = list(title = "Prédictions (Correct/Incorrect)", color = 'white', 
                       tickvals = c(0, 1), ticktext = c("Incorrect", "Correct")),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white'),
          showlegend = FALSE
        )
    } else {
      plot_ly() %>%
        layout(
          title = list(text = "❌ Aucune prédiction disponible (données insuffisantes ou classes vides)", 
                       font = list(size = 16, color = 'white')),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    }
  })
  
  output$feature_importance <- renderPlotly({
    model_result <- trained_model()
    if (!is.null(model_result) && model_result$model_type == "rf") {
      importance_df <- data.frame(
        Variable = row.names(importance(model_result$model)),
        Importance = importance(model_result$model)[, 1]
      ) %>%
        arrange(desc(Importance))
      
      plot_ly(importance_df, x = ~reorder(Variable, Importance), y = ~Importance,
              type = "bar", 
              marker = list(color = '#9b59b6')) %>%
        layout(
          title = list(text = "📊 Importance des Variables (Random Forest)", 
                       font = list(size = 16, color = 'white')),
          xaxis = list(title = "Variables", color = 'white'),
          yaxis = list(title = "Importance", color = 'white'),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    } else if (!is.null(model_result)) {
      plot_ly() %>%
        layout(
          title = list(text = paste("ℹ️ Importance non disponible pour", toupper(model_result$model_type)), 
                       font = list(size = 16, color = 'white')),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    } else {
      plot_ly() %>%
        layout(
          title = list(text = "❌ Modèle non disponible (données insuffisantes ou classes vides)", 
                       font = list(size = 16, color = 'white')),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    }
  })
  
  output$model_comparison_plot <- renderPlotly({
    comparison_df <- model_comparison()
    
    if (!is.null(comparison_df)) {
      plot_ly(comparison_df) %>%
        add_bars(x = ~Model, y = ~Accuracy, name = "Accuracy", marker = list(color = '#3498db')) %>%
        add_bars(x = ~Model, y = ~Precision, name = "Precision", marker = list(color = '#e74c3c'), visible = "legendonly") %>%
        add_bars(x = ~Model, y = ~Recall, name = "Recall", marker = list(color = '#f39c12'), visible = "legendonly") %>%
        add_bars(x = ~Model, y = ~F1, name = "F1 Score", marker = list(color = '#27ae60'), visible = "legendonly") %>%
        layout(
          title = list(text = "📈 Comparaison des Performances des Modèles", 
                       font = list(size = 16, color = 'white')),
          xaxis = list(title = "Modèle", color = 'white'),
          yaxis = list(title = "Performance (%)", color = 'white', range = c(0, 100)),
          barmode = "group",
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white'),
          legend = list(
            title = list(text = "Métrique", font = list(color = 'white')),
            font = list(color = 'white'),
            bgcolor = 'rgba(0,0,0,0.3)'
          ),
          hovermode = "x unified"
        )
    } else {
      plot_ly() %>%
        layout(
          title = list(text = "❌ Comparaison non disponible (données insuffisantes ou classes vides)", 
                       font = list(size = 16, color = 'white')),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'white')
        )
    }
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)