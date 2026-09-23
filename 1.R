# ============================================================================
# Application Shiny - Analyse BRFSS
# Projet Master 2 Data Science - Visualisation de données
# VERSION AMÉLIORÉE - Dynamique et Interactive
# ============================================================================

# Chargement des bibliothèques
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(grid)
library(gridExtra)
library(scales)

# Packages optionnels
use_shinyWidgets <- requireNamespace("shinyWidgets", quietly = TRUE)
use_spinners      <- requireNamespace("shinycssloaders", quietly = TRUE)

# Options globales
options(shiny.maxRequestSize = 100*1024^2)  # 100 MB max
options(DT.options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))

# ============================================================================
# CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================================
load_and_prepare_data <- function(file_path, year) {
  tryCatch({
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    # Échantillonnage si nécessaire pour performances
    if (nrow(data) > 50000) {
      set.seed(123)
      data <- data[sample(nrow(data), 50000), ]
    }
    
    data %>%
      mutate(
        Year = year,
        GENHLTH   = factor(GENHLTH,   levels = 1:5, labels = c("Excellent", "Très bon", "Bon", "Moyen", "Mauvais")),
        SEX       = factor(SEX,       levels = 1:2, labels = c("Homme", "Femme")),
        X_AGE_G   = factor(X_AGE_G,   levels = 1:6, labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
        EDUCA     = factor(EDUCA,     levels = 1:6, labels = c("Jamais scolarisé", "Primaire", "Collège", "Lycée", "Université", "Diplômé")),
        INCOME2   = factor(INCOME2,   levels = 1:8, labels = c("<10k", "10-15k", "15-20k", "20-25k", "25-35k", "35-50k", "50-75k", "75k+")),
        HLTHPLN1  = factor(HLTHPLN1,  levels = 1:2, labels = c("Oui", "Non")),
        EXERANY2  = factor(EXERANY2,  levels = 1:2, labels = c("Oui", "Non")),
        X_SMOKER3 = factor(X_SMOKER3, levels = 1:4, labels = c("Actuel quotidien", "Actuel occasionnel", "Ancien", "Jamais")),
        BMI       = ifelse(is.na(X_BMI5) | X_BMI5 == 0, NA_real_, X_BMI5 / 100),
        BMI_CAT   = cut(BMI, breaks = c(0, 18.5, 25, 30, 100),
                        labels = c("Insuffisant", "Normal", "Surpoids", "Obèse"))
      )
  }, error = function(e) {
    message("Erreur lors du chargement de ", file_path, " → Données simulées utilisées pour ", year)
    data.frame(
      Year      = year,
      GENHLTH   = sample(c("Excellent", "Très bon", "Bon", "Moyen", "Mauvais"), 1000, TRUE),
      SEX       = sample(c("Homme", "Femme"), 1000, TRUE),
      X_AGE_G   = sample(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), 1000, TRUE),
      EDUCA     = sample(c("Jamais scolarisé", "Primaire", "Collège", "Lycée", "Université", "Diplômé"), 1000, TRUE),
      INCOME2   = sample(c("<10k", "10-15k", "15-20k", "20-25k", "25-35k", "35-50k", "50-75k", "75k+"), 1000, TRUE),
      HLTHPLN1  = sample(c("Oui", "Non"), 1000, TRUE),
      EXERANY2  = sample(c("Oui", "Non"), 1000, TRUE),
      X_SMOKER3 = sample(c("Actuel quotidien", "Actuel occasionnel", "Ancien", "Jamais"), 1000, TRUE),
      BMI       = rnorm(1000, mean = 26, sd = 5),
      BMI_CAT   = sample(c("Insuffisant", "Normal", "Surpoids", "Obèse"), 1000, TRUE),
      X_LLCPWT  = runif(1000, 0.5, 2)
    )
  })
}

# Chargement des deux jeux de données
data2014 <- load_and_prepare_data("C:/Users/Dell/Desktop/DATA VIZ/data/2014.csv", 2014)
data2015 <- load_and_prepare_data("C:/Users/Dell/Desktop/DATA VIZ/data/2015.csv", 2015)
data <- bind_rows(data2014, data2015)

# ============================================================================
# CSS PERSONNALISÉ AMÉLIORÉ
# ============================================================================
custom_css <- "
/* Animation de pulsation pour les éléments importants */
@keyframes pulse {
  0% { transform: scale(1); }
  50% { transform: scale(1.05); }
  100% { transform: scale(1); }
}

/* Animation de glow */
@keyframes glow {
  from { text-shadow: 0 0 20px rgba(52, 152, 219, 0.5); }
  to { text-shadow: 0 0 30px rgba(52, 152, 219, 0.8), 0 0 40px rgba(52, 152, 219, 0.3); }
}

/* Animation de slide-in */
@keyframes slideInFromLeft {
  0% { transform: translateX(-100%); opacity: 0; }
  100% { transform: translateX(0); opacity: 1; }
}

body {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  min-height: 100vh;
}

.content-wrapper, .right-side {
  background: transparent !important;
}

.main-header .navbar {
  background: linear-gradient(45deg, #2c3e50, #3498db) !important;
  border: none !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.2);
  animation: slideInFromLeft 0.5s ease-out;
}

.main-sidebar {
  background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%) !important;
  box-shadow: 4px 0 20px rgba(0,0,0,0.1);
}

.sidebar-menu > li > a {
  color: #ecf0f1 !important;
  border-left: 3px solid transparent;
  transition: all 0.3s ease;
  padding: 15px 20px !important;
}

.sidebar-menu > li > a:hover {
  background: rgba(52, 152, 219, 0.3) !important;
  border-left-color: #3498db;
  transform: translateX(8px);
  box-shadow: 0 4px 15px rgba(52, 152, 219, 0.3);
}

.sidebar-menu > li.active > a {
  background: rgba(52, 152, 219, 0.4) !important;
  border-left-color: #3498db;
  font-weight: 600;
}

.box {
  background: rgba(255, 255, 255, 0.15) !important;
  backdrop-filter: blur(15px);
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 20px !important;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.15);
  margin-bottom: 20px;
  transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
}

.box:hover {
  transform: translateY(-8px) scale(1.02);
  box-shadow: 0 15px 45px rgba(0, 0, 0, 0.25);
  background: rgba(255, 255, 255, 0.2) !important;
}

.box-header {
  background: transparent !important;
  border-bottom: 2px solid rgba(255, 255, 255, 0.2) !important;
  color: white !important;
  padding: 20px !important;
}

.box-title {
  color: white !important;
  font-weight: 700;
  font-size: 19px;
  text-align: center;
  text-shadow: 0 2px 8px rgba(0,0,0,0.3);
  letter-spacing: 0.5px;
}

.small-box {
  border-radius: 18px !important;
  background: linear-gradient(135deg, rgba(255,255,255,0.15), rgba(255,255,255,0.08)) !important;
  backdrop-filter: blur(12px);
  border: 1px solid rgba(255, 255, 255, 0.25);
  transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
  cursor: pointer;
}

.small-box:hover {
  transform: translateY(-12px) scale(1.05);
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.3);
  animation: pulse 1.5s infinite;
}

.small-box h3, .small-box p {
  color: white !important;
  text-shadow: 0 2px 6px rgba(0,0,0,0.3);
}

.small-box h3 {
  font-size: 42px !important;
  font-weight: 800 !important;
}

.small-box .icon {
  color: rgba(255, 255, 255, 0.35) !important;
  font-size: 85px !important;
}

.control-section {
  background: rgba(255, 255, 255, 0.15) !important;
  backdrop-filter: blur(12px);
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 18px;
  padding: 25px;
  margin: 15px;
  box-shadow: 0 10px 35px rgba(0, 0, 0, 0.15);
  transition: all 0.3s ease;
}

.control-section:hover {
  box-shadow: 0 15px 45px rgba(0, 0, 0, 0.2);
  transform: translateY(-3px);
}

.control-section h4 {
  color: white !important;
  margin-bottom: 25px !important;
  text-align: center;
  font-weight: 700;
  text-shadow: 0 3px 6px rgba(0,0,0,0.4);
  font-size: 20px !important;
  letter-spacing: 1px;
}

.form-group label {
  color: white !important;
  font-weight: 600;
  margin-bottom: 10px;
  display: block;
  font-size: 15px;
  text-shadow: 0 2px 4px rgba(0,0,0,0.2);
}

.form-control, .selectize-input {
  background: rgba(255, 255, 255, 0.95) !important;
  border: 2px solid rgba(255, 255, 255, 0.4) !important;
  color: #2c3e50 !important;
  border-radius: 12px !important;
  padding: 10px 15px;
  font-size: 15px;
  font-weight: 500;
  transition: all 0.3s ease;
  box-shadow: 0 4px 10px rgba(0,0,0,0.1);
}

.form-control:focus, .selectize-input.focus {
  background: white !important;
  border-color: #3498db !important;
  box-shadow: 0 0 0 0.3rem rgba(52, 152, 219, 0.35) !important;
  transform: translateY(-2px);
}

.main-title {
  text-align: center;
  color: white;
  font-size: 2.8em;
  font-weight: 900;
  text-shadow: 0 0 25px rgba(52, 152, 219, 0.6);
  margin-bottom: 35px;
  animation: glow 2s ease-in-out infinite alternate;
  letter-spacing: 2px;
}

/* Style pour le tableau DataTable */
.dataTables_wrapper {
  background: white !important;
  padding: 25px 35px !important;
  border-radius: 15px;
  box-shadow: 0 8px 25px rgba(0,0,0,0.1);
  margin: 0 auto !important;
  max-width: 900px !important;
}

table.dataTable {
  background: white !important;
  border-radius: 12px;
  overflow: hidden;
  width: 100% !important;
  margin: 0 auto !important;
}

table.dataTable thead th {
  background: linear-gradient(135deg, #3498db, #2980b9) !important;
  color: white !important;
  font-weight: 700 !important;
  font-size: 18px !important;
  padding: 18px 20px !important;
  border: none !important;
  text-align: left !important;
}

table.dataTable thead th:nth-child(2) {
  text-align: center !important;
}

table.dataTable tbody td {
  color: #2c3e50 !important;
  font-size: 17px !important;
  padding: 16px 20px !important;
  font-weight: 600 !important;
  border-bottom: 2px solid rgba(52, 152, 219, 0.15) !important;
}

table.dataTable tbody td:first-child {
  text-align: left !important;
}

table.dataTable tbody td:nth-child(2) {
  text-align: center !important;
}

table.dataTable tbody tr {
  transition: all 0.3s ease;
}

table.dataTable tbody tr:hover {
  background: linear-gradient(90deg, rgba(52, 152, 219, 0.08), rgba(52, 152, 219, 0.04)) !important;
  transform: translateX(5px);
  box-shadow: 0 4px 15px rgba(52, 152, 219, 0.15);
}

/* Animations d'entrée pour les boxes */
.box {
  animation: fadeInUp 0.6s ease-out;
}

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

/* Style pour les graphiques plotly */
.plotly {
  border-radius: 15px;
  overflow: hidden;
}
"

# ============================================================================
# INTERFACE UTILISATEUR
# ============================================================================
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$i(class = "fa fa-heartbeat", style = "margin-right: 10px; font-size: 22px;"),
      "Analyse BRFSS – Santé et Comportements"
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("🏠 Accueil",              tabName = "overview",   icon = icon("tachometer-alt")),
      menuItem("👥 Population",           tabName = "population", icon = icon("users")),
      menuItem("🏥 Accès aux soins",      tabName = "healthcare", icon = icon("hospital")),
      menuItem("⚠️ Comportements à risque", tabName = "behaviors",  icon = icon("running")),
      menuItem("📊 Analyses croisées",    tabName = "analysis",   icon = icon("chart-line")),
      menuItem("🎨 Visualisation avancée", tabName = "advanced",  icon = icon("layer-group"))
    ),
    
    div(class = "control-section",
        h4("⚙️ FILTRES D'ANALYSE"),
        
        selectInput("year_filter",
                    label = "📅 Année des données",
                    choices = c("Toutes les années" = "all", "2014" = "2014", "2015" = "2015"),
                    selected = "all"),
        
        selectInput("health_var",
                    label = "❤️ Indicateur de santé",
                    choices = c("État de santé perçu" = "GENHLTH",
                                "Indice de masse corporelle" = "BMI",
                                "Catégorie d'IMC" = "BMI_CAT"),
                    selected = "GENHLTH"),
        
        selectInput("demo_var",
                    label = "👤 Caractéristique socio-démographique",
                    choices = c("Sexe" = "SEX",
                                "Groupe d'âge" = "X_AGE_G",
                                "Niveau d'éducation" = "EDUCA",
                                "Niveau de revenu" = "INCOME2"),
                    selected = "X_AGE_G"),
        
        selectInput("behavior_var",
                    label = "🏃 Comportement à risque",
                    choices = c("Activité physique" = "EXERANY2",
                                "Tabagisme" = "X_SMOKER3"),
                    selected = "EXERANY2")
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css)),
      tags$script(HTML("
        $(document).ready(function() {
          // Animation au clic sur les value boxes
          $('.small-box').on('click', function() {
            $(this).addClass('pulse');
            setTimeout(() => $(this).removeClass('pulse'), 1000);
          });
        });
      "))
    ),
    
    tabItems(
      # Vue d'ensemble
      tabItem("overview",
              div(class = "main-title", "📈 ANALYSE DES COMPORTEMENTS ET DE LA SANTÉ PERÇUE"),
              fluidRow(
                valueBoxOutput("total_obs", width = 3),
                valueBoxOutput("excellent_health", width = 3),
                valueBoxOutput("with_insurance", width = 3),
                valueBoxOutput("physically_active", width = 3)
              ),
              fluidRow(
                box(title = "📋 RÉSUMÉ GÉNÉRAL DES INDICATEURS", status = "primary",
                    solidHeader = TRUE, width = 12,
                    DTOutput("summary_table"))
              )
      ),
      
      # Population
      tabItem("population",
              fluidRow(
                box(title = "📊 Répartition de la population selon les caractéristiques choisies",
                    status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("demo_distribution", height = "420px")),
                box(title = "💚 État de santé perçu par les répondants",
                    status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("health_status", height = "420px"))
              ),
              fluidRow(
                box(title = "👫 Répartition détaillée par âge et sexe",
                    status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("age_sex_distribution", height = "480px"))
              )
      ),
      
      # Accès aux soins
      tabItem("healthcare",
              fluidRow(
                box(title = "🛡️ Taux de couverture par une assurance santé",
                    status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("insurance_coverage", height = "420px")),
                box(title = "💰 Couverture assurance selon le niveau de revenu",
                    status = "danger", solidHeader = TRUE, width = 6,
                    plotlyOutput("insurance_by_income", height = "420px"))
              ),
              fluidRow(
                box(title = "⚖️ Comparaison de l'état de santé perçu : assurés vs non-assurés",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("health_by_insurance", height = "480px"))
              )
      ),
      
      # Comportements
      tabItem("behaviors",
              fluidRow(
                box(title = "🏃‍♂️ Niveau d'activité physique déclarée",
                    status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("physical_activity", height = "420px")),
                box(title = "🚬 Statut tabagique des répondants",
                    status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("smoking_status", height = "420px"))
              ),
              fluidRow(
                box(title = "⚖️ Indice de masse corporelle selon les caractéristiques démographiques",
                    status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("bmi_by_demo", height = "480px"))
              )
      ),
      
      # Analyses croisées
      tabItem("analysis",
              fluidRow(
                box(title = "🔗 Interactions entre caractéristiques démographiques, comportements et santé",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("multivariate_analysis", height = "520px"))
              ),
              fluidRow(
                box(title = "📈 Relations entre âge, activité physique et état de santé perçu",
                    status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("correlation_plot", height = "520px"))
              )
      ),
      
      # Visualisation avancée
      tabItem("advanced",
              fluidRow(
                box(title = "🎨 COMPOSITION AVANCÉE DE VISUALISATIONS",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("grid_plot", height = "800px"))
              )
      )
    )
  )
)

# ============================================================================
# SERVEUR
# ============================================================================
server <- function(input, output, session) {
  
  # Données filtrées par année
  filtered_data <- reactive({
    if (input$year_filter == "all") data else data %>% filter(Year == as.numeric(input$year_filter))
  })
  
  # Value Boxes (pondérées) avec animations
  output$total_obs <- renderValueBox({
    valueBox(
      formatC(round(sum(filtered_data()$X_LLCPWT, na.rm = TRUE), 0), format="d", big.mark=" "),
      "Observations pondérées (total)",
      icon = icon("database"), 
      color = "blue"
    )
  })
  
  output$excellent_health <- renderValueBox({
    pct <- round(100 * weighted.mean(filtered_data()$GENHLTH == "Excellent", filtered_data()$X_LLCPWT, na.rm = TRUE), 1)
    valueBox(paste0(pct, "%"), "Santé perçue excellente", icon = icon("heart"), color = "green")
  })
  
  output$with_insurance <- renderValueBox({
    pct <- round(100 * weighted.mean(filtered_data()$HLTHPLN1 == "Oui", filtered_data()$X_LLCPWT, na.rm = TRUE), 1)
    valueBox(paste0(pct, "%"), "Personnes couvertes par assurance", icon = icon("shield-alt"), color = "yellow")
  })
  
  output$physically_active <- renderValueBox({
    pct <- round(100 * weighted.mean(filtered_data()$EXERANY2 == "Oui", filtered_data()$X_LLCPWT, na.rm = TRUE), 1)
    valueBox(paste0(pct, "%"), "Personnes physiquement actives", icon = icon("running"), color = "purple")
  })
  
  # Tableau résumé pondéré - VERSION PARFAITEMENT CENTRÉE
  output$summary_table <- renderDT({
    fd <- filtered_data()
    data.frame(
      Indicateur = c("📊 IMC moyen pondéré", 
                     "👩 Proportion de femmes", 
                     "🛡️ Proportion assurées", 
                     "🏃 Proportion actives physiquement",
                     "🚬 Proportion fumeurs actuels"),
      Valeur = c(
        round(weighted.mean(fd$BMI, fd$X_LLCPWT, na.rm = TRUE), 1),
        paste0(round(100 * weighted.mean(fd$SEX == "Femme", fd$X_LLCPWT, na.rm = TRUE), 1), "%"),
        paste0(round(100 * weighted.mean(fd$HLTHPLN1 == "Oui", fd$X_LLCPWT, na.rm = TRUE), 1), "%"),
        paste0(round(100 * weighted.mean(fd$EXERANY2 == "Oui", fd$X_LLCPWT, na.rm = TRUE), 1), "%"),
        paste0(round(100 * weighted.mean(grepl("Actuel", fd$X_SMOKER3), fd$X_LLCPWT, na.rm = TRUE), 1), "%")
      )
    ) %>%
      datatable(
        options = list(
          dom = 't',
          ordering = FALSE,
          autoWidth = FALSE,
          scrollX = FALSE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0, width = '70%'),
            list(className = 'dt-center', targets = 1, width = '30%')
          )
        ),
        rownames = FALSE,
        class = 'display compact'
      ) %>%
      formatStyle(
        columns = c('Indicateur', 'Valeur'),
        color = '#2c3e50',
        backgroundColor = 'white',
        fontWeight = '600',
        fontSize = '17px'
      ) %>%
      formatStyle(
        'Indicateur',
        fontWeight = 'bold',
        color = '#2980b9',
        paddingLeft = '20px'
      ) %>%
      formatStyle(
        'Valeur',
        fontWeight = 'bold',
        color = '#27ae60',
        fontSize = '19px'
      )
  })
  
  # Graphiques interactifs avec plotly
  output$demo_distribution <- renderPlotly({
    filtered_data() %>%
      group_by(!!sym(input$demo_var)) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      mutate(pct = count / sum(count) * 100) %>%
      plot_ly(x = ~.data[[input$demo_var]], y = ~pct, type = "bar", 
              marker = list(color = '#3498db',
                            line = list(color = '#2980b9', width = 2)),
              hovertemplate = paste('<b>%{x}</b><br>',
                                    'Pourcentage: %{y:.1f}%<br>',
                                    '<extra></extra>')) %>%
      layout(title = list(text = paste("Répartition selon", input$demo_var),
                          font = list(size = 18, color = '#2c3e50', family = 'Arial, sans-serif')),
             xaxis = list(title = "", tickfont = list(size = 14)),
             yaxis = list(title = "Pourcentage (%)", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$health_status <- renderPlotly({
    filtered_data() %>%
      group_by(GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(labels = ~GENHLTH, values = ~count, type = "pie",
              marker = list(colors = c('#27ae60','#2ecc71','#f39c12','#e67e22','#e74c3c'),
                            line = list(color = '#fff', width = 2)),
              textposition = 'inside',
              textinfo = 'label+percent',
              hoverinfo = 'label+value+percent') %>%
      layout(title = list(text = "État de santé perçu",
                          font = list(size = 18, color = '#2c3e50')),
             showlegend = TRUE,
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$age_sex_distribution <- renderPlotly({
    filtered_data() %>%
      group_by(X_AGE_G, SEX) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(x = ~X_AGE_G, y = ~count, color = ~SEX, type = "bar", 
              colors = c('#3498db', '#e74c3c'),
              hovertemplate = paste('<b>%{x}</b><br>',
                                    '%{fullData.name}<br>',
                                    'Nombre: %{y:,.0f}<br>',
                                    '<extra></extra>')) %>%
      layout(barmode = "group",
             title = list(text = "Répartition par groupe d'âge et sexe",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "Groupe d'âge", tickfont = list(size = 14)),
             yaxis = list(title = "Nombre pondéré", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$insurance_coverage <- renderPlotly({
    filtered_data() %>%
      group_by(HLTHPLN1) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(labels = ~HLTHPLN1, values = ~count, type = "pie",
              marker = list(colors = c('#27ae60', '#e74c3c'),
                            line = list(color = '#fff', width = 2)),
              textposition = 'inside',
              textinfo = 'label+percent') %>%
      layout(title = list(text = "Couverture par assurance santé",
                          font = list(size = 18, color = '#2c3e50')),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$insurance_by_income <- renderPlotly({
    filtered_data() %>%
      group_by(INCOME2, HLTHPLN1) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      group_by(INCOME2) %>%
      mutate(pct = count / sum(count) * 100) %>%
      filter(HLTHPLN1 == "Oui") %>%
      plot_ly(x = ~INCOME2, y = ~pct, type = "bar", 
              marker = list(color = '#f39c12',
                            line = list(color = '#e67e22', width = 2))) %>%
      layout(title = list(text = "Proportion de personnes assurées selon le revenu",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "Niveau de revenu", tickfont = list(size = 14)),
             yaxis = list(title = "Pourcentage (%)", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$health_by_insurance <- renderPlotly({
    filtered_data() %>%
      group_by(HLTHPLN1, GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      group_by(HLTHPLN1) %>%
      mutate(pct = count / sum(count) * 100) %>%
      plot_ly(x = ~GENHLTH, y = ~pct, color = ~HLTHPLN1, type = "bar", 
              colors = c('#27ae60', '#e74c3c')) %>%
      layout(barmode = "group",
             title = list(text = "État de santé perçu selon le statut d'assurance",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "État de santé", tickfont = list(size = 14)),
             yaxis = list(title = "Pourcentage (%)", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$physical_activity <- renderPlotly({
    filtered_data() %>%
      group_by(EXERANY2) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(labels = ~EXERANY2, values = ~count, type = "pie",
              marker = list(colors = c('#27ae60', '#95a5a6'),
                            line = list(color = '#fff', width = 2)),
              textposition = 'inside',
              textinfo = 'label+percent') %>%
      layout(title = list(text = "Niveau d'activité physique",
                          font = list(size = 18, color = '#2c3e50')),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$smoking_status <- renderPlotly({
    filtered_data() %>%
      group_by(X_SMOKER3) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      mutate(pct = count / sum(count) * 100) %>%
      plot_ly(x = ~X_SMOKER3, y = ~pct, type = "bar",
              marker = list(color = c('#e74c3c', '#e67e22', '#f39c12', '#27ae60'),
                            line = list(color = '#c0392b', width = 2))) %>%
      layout(title = list(text = "Statut tabagique des répondants",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "", tickfont = list(size = 14)),
             yaxis = list(title = "Pourcentage (%)", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$bmi_by_demo <- renderPlotly({
    filtered_data() %>%
      group_by(!!sym(input$demo_var)) %>%
      summarise(mean_bmi = weighted.mean(BMI, X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(x = ~.data[[input$demo_var]], y = ~mean_bmi, type = "bar",
              marker = list(color = '#3498db',
                            line = list(color = '#2980b9', width = 2))) %>%
      layout(title = list(text = paste("IMC moyen selon", input$demo_var),
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "", tickfont = list(size = 14)),
             yaxis = list(title = "IMC moyen", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$multivariate_analysis <- renderPlotly({
    filtered_data() %>%
      group_by(!!sym(input$demo_var), !!sym(input$behavior_var), !!sym(input$health_var)) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(x = ~.data[[input$demo_var]], y = ~count,
              color = ~.data[[input$behavior_var]], type = "bar") %>%
      layout(barmode = "stack",
             title = list(text = "Interactions multivariées",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(tickfont = list(size = 14)),
             yaxis = list(title = "Nombre pondéré", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$correlation_plot <- renderPlotly({
    filtered_data() %>%
      filter(GENHLTH %in% c("Excellent", "Très bon", "Bon")) %>%
      group_by(X_AGE_G, EXERANY2, GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(x = ~X_AGE_G, y = ~count, color = ~EXERANY2,
              facet_col = ~GENHLTH, type = "bar") %>%
      layout(title = list(text = "Relations âge, activité physique et santé",
                          font = list(size = 18, color = '#2c3e50')),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  # Visualisation Grid avancée - CORRIGÉE avec textes nets
  output$grid_plot <- renderPlot({
    # Paramètres pour améliorer la qualité des textes
    par(family = "sans")
    
    grid.newpage()
    
    pushViewport(viewport(layout = grid.layout(2, 2,
                                               widths = unit(c(0.5, 0.5), "npc"),
                                               heights = unit(c(0.48, 0.48), "npc"))))
    
    # Graphique 1: État de santé
    vp1 <- viewport(layout.pos.row = 1, layout.pos.col = 1)
    pushViewport(vp1)
    df1 <- filtered_data() %>%
      group_by(GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    p1 <- ggplot(df1, aes(x = GENHLTH, y = count, fill = GENHLTH)) +
      geom_bar(stat = "identity", color = "white", size = 1.2) +
      scale_fill_manual(values = c('#27ae60', '#2ecc71', '#f39c12', '#e67e22', '#e74c3c')) +
      theme_minimal(base_size = 16) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold", color = "#2c3e50"),
        axis.text.y = element_text(size = 14, face = "bold", color = "#2c3e50"),
        axis.title = element_text(size = 15, face = "bold", color = "#2c3e50"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2c3e50"),
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)
      ) +
      labs(title = "État de santé perçu", x = "", y = "Nombre pondéré")
    
    print(p1, newpage = FALSE)
    popViewport()
    
    # Graphique 2: IMC par sexe
    vp2 <- viewport(layout.pos.row = 1, layout.pos.col = 2)
    pushViewport(vp2)
    df2 <- filtered_data() %>% filter(!is.na(BMI), !is.na(SEX))
    
    p2 <- ggplot(df2, aes(x = SEX, y = BMI, fill = SEX)) +
      geom_boxplot(alpha = 0.8, color = "#2c3e50", size = 1.2) +
      scale_fill_manual(values = c('#3498db', '#e74c3c')) +
      theme_minimal(base_size = 16) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14, face = "bold", color = "#2c3e50"),
        axis.title = element_text(size = 15, face = "bold", color = "#2c3e50"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2c3e50"),
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)
      ) +
      labs(title = "IMC par sexe", x = "", y = "IMC")
    
    print(p2, newpage = FALSE)
    popViewport()
    
    # Graphique 3: Assurance par revenu
    vp3 <- viewport(layout.pos.row = 2, layout.pos.col = 1)
    pushViewport(vp3)
    df3 <- filtered_data() %>%
      group_by(INCOME2, HLTHPLN1) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      group_by(INCOME2) %>%
      mutate(percentage = count / sum(count) * 100)
    
    p3 <- ggplot(df3, aes(x = INCOME2, y = percentage, fill = HLTHPLN1)) +
      geom_bar(stat = "identity", position = "stack", color = "white", size = 1) +
      scale_fill_manual(values = c('#27ae60', '#e74c3c')) +
      theme_minimal(base_size = 16) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13, face = "bold", color = "#2c3e50"),
        axis.text.y = element_text(size = 14, face = "bold", color = "#2c3e50"),
        axis.title = element_text(size = 15, face = "bold", color = "#2c3e50"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2c3e50"),
        legend.title = element_text(size = 14, face = "bold", color = "#2c3e50"),
        legend.text = element_text(size = 13, face = "bold", color = "#2c3e50"),
        legend.position = "bottom",
        plot.margin = margin(20, 20, 20, 20)
      ) +
      labs(title = "Couverture assurance / revenu", x = "Niveau de revenu", y = "Pourcentage", fill = "Assurance")
    
    print(p3, newpage = FALSE)
    popViewport()
    
    # Graphique 4: Activité par âge
    vp4 <- viewport(layout.pos.row = 2, layout.pos.col = 2)
    pushViewport(vp4)
    df4 <- filtered_data() %>%
      group_by(X_AGE_G, EXERANY2) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    p4 <- ggplot(df4, aes(x = X_AGE_G, y = count, fill = EXERANY2)) +
      geom_bar(stat = "identity", position = "dodge", color = "white", size = 1) +
      scale_fill_manual(values = c('#27ae60', '#95a5a6')) +
      theme_minimal(base_size = 16) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13, face = "bold", color = "#2c3e50"),
        axis.text.y = element_text(size = 14, face = "bold", color = "#2c3e50"),
        axis.title = element_text(size = 15, face = "bold", color = "#2c3e50"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2c3e50"),
        legend.title = element_text(size = 14, face = "bold", color = "#2c3e50"),
        legend.text = element_text(size = 13, face = "bold", color = "#2c3e50"),
        legend.position = "bottom",
        plot.margin = margin(20, 20, 20, 20)
      ) +
      labs(title = "Activité physique / âge", x = "Groupe d'âge", y = "Nombre pondéré", fill = "Activité")
    
    print(p4, newpage = FALSE)
    popViewport()
    
    # Titre global de la composition avec police plus grande et nette
    grid.text("🎨 COMPOSITION AVANCÉE DE VISUALISATIONS – BRFSS 2014-2015",
              x = 0.5, y = 0.98,
              gp = gpar(fontsize = 24, fontface = "bold", col = "#2c3e50", fontfamily = "sans"))
  }, res = 120, bg = "white")  # Augmentation de la résolution pour plus de netteté
}

# ============================================================================
# LANCEMENT DE L'APPLICATION
# ============================================================================
shinyApp(ui = ui, server = server)