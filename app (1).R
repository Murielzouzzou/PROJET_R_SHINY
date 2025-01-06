library(shiny)
library(httr)
library(jsonlite)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(DT)
library(tidygeocoder)
library(ggplot2)
library(geosphere)
library(maps)

# Désactiver l'avertissement pour `renderDataTable()`
options(shiny.legacy.datatable = FALSE)

# Charger les données globales
global_data <- read.csv('resultats_dpe.csv', header = TRUE, sep = ';', dec = '.')

# Vérifier si le fichier est vide
if (nrow(global_data) == 0) {
  stop("Le fichier 'resultats_dpe.csv' est vide ou n'a pas pu être chargé.")
}

# Nettoyer les noms des colonnes
names(global_data) <- gsub(" ", "_", names(global_data))

# Convertir le code postal en numérique
global_data$Code_postal_.BAN. <- as.numeric(global_data$Code_postal_.BAN.)

# Interface utilisateur (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Tableau_de_bord_DPE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Documentation", tabName = "doc", icon = icon("info")),
      menuItem("Tableau de données", tabName = "data", icon = icon("table")),
      menuItem("Statistiques", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("Carte", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      # Documentation des données
      tabItem(
        tabName = "doc",
        fluidPage(
          titlePanel("Documentation des données"),
          DTOutput("doc_table")
        )
      ),
      
      # Tableau de données DPE
      tabItem(
        tabName = "data",
        fluidPage(
          titlePanel("Parcourir les données DPE"),
          sidebarLayout(
            sidebarPanel(
              selectInput("postcode_filter", "Filtrer par code postal:",
                          c("Tous", unique(global_data$Code_postal_.BAN.))),
              actionButton("refreshButton", "Actualiser les données"),
              downloadButton("export_csv", "Exporter les données (.csv)")
            ),
            mainPanel(
              DTOutput("table", width = "100%", height = "600px")
            )
          )
        )
      ),
      
      # Statistiques DPE
      tabItem(
        tabName = "statistics",
        fluidPage(
          titlePanel("Statistiques DPE"),
          fluidRow(
            column(4, box(title = "Répartition des Étiquettes DPE", plotOutput("DPE_histogram", height = "400px"), 
                          downloadButton("download_DPE_histogram", "Exporter (.png)"))),
            column(4, box(title = "Répartition des coûts de chauffage", plotOutput("heating_cost_histogram", height = "400px"), 
                          downloadButton("download_heating_cost_histogram", "Exporter (.png)"))),
            column(4, box(title = "Proportion des logements", plotOutput("proportion_chart", height = "400px"), 
                          downloadButton("download_proportion_chart", "Exporter (.png)")))
          ),
          fluidRow(
            column(6, box(title = "Nuage de points (Surface vs Code postal)", plotOutput("scatter_plot", height = "400px"),
                          downloadButton("download_scatter_plot", "Exporter (.png)"))),
            column(6, box(title = "Résumé statistique", verbatimTextOutput("summary_stats")))
          )
        )
      ),
      
      # Carte DPE
      tabItem(
        tabName = "map",
        fluidPage(
          titlePanel("Carte DPE"),
          sidebarLayout(
            sidebarPanel(
              numericInput('loc', 'Votre code postal', value = NULL),
              actionButton('go', 'Géocoder')
            ),
            mainPanel(
              leafletOutput('map', height = "920px")
            )
          )
        )
      )
    )
  )
)

# Partie serveur
server <- function(input, output, session) {
  
  # Documentation des données
  output$doc_table <- DT::renderDT({
    data.frame(
      Variable = c("Coût_chauffage", "Coût_éclairage", "Nom_commune", "Date_réception_DPE", "Coût_total_5_usages", "Code_postal", "Coût_auxiliaires", "Coordonnée_cartographique_Y_.BAN.", "Coordonnée_cartographique_X_.BAN.", "Etiquette_DPE", "X_score"),
      Description = c("Coût du chauffage", "Coût de l'éclairage", "Nom de la commune", "Date de réception du DPE", "Coût total des 5 usages", "Code postal", "Coût des auxiliaires", "Coordonnée Y de la géolocalisation", "Coordonnée X de la géolocalisation", "Etiquette DPE", "Score énergétique")
    )
  }, options = list(pageLength = 10))
  
  # Filtrer les données selon le code postal
  filtered_data <- reactive({
    req(global_data)
    data_filtered <- global_data
    
    if (!is.null(input$postcode_filter) && input$postcode_filter != "Tous") {
      data_filtered <- data_filtered[data_filtered$Code_postal_.BAN. == input$postcode_filter, ]
    }
    
    return(data_filtered)
  })
  
  # Affichage du tableau de données
  output$table <- DT::renderDT({
    datatable(filtered_data(), 
              options = list(
                pageLength = 50,     # Nombre de lignes par page
                lengthMenu = c(10, 25, 50, 100),  # Options de taille de page
                scrollX = TRUE       # Ajoute une barre de défilement horizontale
              ), 
              class = 'display')
  })
  
  # Actualiser le filtre de code postal
  observeEvent(input$refreshButton, {
    updateSelectInput(session, "postcode_filter", selected = "Tous")
  })
  
  # Histogramme des étiquettes DPE
  output$DPE_histogram <- renderPlot({
    req(filtered_data())
    p <- ggplot(filtered_data(), aes(x = Etiquette_DPE)) +
      geom_bar(fill = "#0072B2", color = "black") +
      labs(title = "Répartition des Étiquettes DPE", x = "Étiquette DPE", y = "Nombre")
    print(p)
    return(p)
  })
  
  # Histogramme des coûts de chauffage
  output$heating_cost_histogram <- renderPlot({
    req(filtered_data())
    p <- ggplot(filtered_data(), aes(x = Coût_chauffage)) +
      geom_histogram(binwidth = 10, fill = "#E69F00", color = "black") +
      labs(title = "Répartition des coûts de chauffage", x = "Coût chauffage (€)", y = "Fréquence")
    print(p)
    return(p)
  })
  
  # Diagramme circulaire des proportions par code postal
  output$proportion_chart <- renderPlot({
    req(filtered_data())
    data_proportion <- filtered_data() %>%
      count(Code_postal_.BAN.) %>%
      mutate(proportion = n / sum(n))
    
    p <- ggplot(data_proportion, aes(x = "", y = proportion, fill = Code_postal_.BAN.)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = "Proportion des logements par code postal")
    print(p)
    return(p)
  })
  
  # Nuage de points entre surface et code postal
  output$scatter_plot <- renderPlot({
    req(filtered_data())
    p <- ggplot(filtered_data(), aes(x = Coût_total_5_usages, y = Code_postal_.BAN.)) +
      geom_point(alpha = 0.6) +
      labs(title = "Coût total vs Code postal", x = "Coût total 5 usages (€)", y = "Code postal")
    print(p)
    return(p)
  })
  
  # Résumé statistique
  output$summary_stats <- renderPrint({
    req(filtered_data())
    summary(filtered_data()[, c("Coût_chauffage", "Coût_total_5_usages", "Code_postal_.BAN.")])
  })
  
  # Fonction pour exporter les graphiques
  output$download_DPE_histogram <- downloadHandler(
    filename = function() { paste("DPE_histogram", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      p <- ggplot(filtered_data(), aes(x = Etiquette_DPE)) +
        geom_bar(fill = "#0072B2", color = "black") +
        labs(title = "Répartition des Étiquettes DPE", x = "Étiquette DPE", y = "Nombre")
      ggsave(file, plot = p, device = "png")
    }
  )
  
  output$download_heating_cost_histogram <- downloadHandler(
    filename = function() { paste("heating_cost_histogram", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      p <- ggplot(filtered_data(), aes(x = Coût_chauffage)) +
        geom_histogram(binwidth = 10, fill = "#E69F00", color = "black") +
        labs(title = "Répartition des coûts de chauffage", x = "Coût chauffage (€)", y = "Fréquence")
      ggsave(file, plot = p, device = "png")
    }
  )
  
  output$download_proportion_chart <- downloadHandler(
    filename = function() { paste("proportion_chart", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      data_proportion <- filtered_data() %>%
        count(Code_postal_.BAN.) %>%
        mutate(proportion = n / sum(n))
      
      p <- ggplot(data_proportion, aes(x = "", y = proportion, fill = Code_postal_.BAN.)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        labs(title = "Proportion des logements par code postal")
      ggsave(file, plot = p, device = "png")
    }
  )
  
  output$download_scatter_plot <- downloadHandler(
    filename = function() { paste("scatter_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      p <- ggplot(filtered_data(), aes(x = Coût_total_5_usages, y = Code_postal_.BAN.)) +
        geom_point(alpha = 0.6) +
        labs(title = "Coût total vs Code postal", x = "Coût total 5 usages (€)", y = "Code postal")
      ggsave(file, plot = p, device = "png")
    }
  )
  
  # Exporter les données en CSV
  output$export_csv <- downloadHandler(
    filename = function() {
      paste("donnees_selectionnees", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Géocodage et affichage sur la carte
  observeEvent(input$go, {
    req(filtered_data())
    
    loc_saisie <- as.character(input$loc)  # Code postal en tant que caractère
    
    if (!is.null(loc_saisie) && loc_saisie != "") {
      # Géocodage avec gestion des erreurs
      addres <- tibble::tibble(addr = loc_saisie)
      
      # Utiliser tryCatch pour capturer les erreurs éventuelles lors du géocodage
      lat_longs <- tryCatch({
        addres %>% geocode(addr, method = 'osm', lat = latitude, long = longitude)
      }, error = function(e) {
        showNotification(paste("Erreur lors du géocodage:", e$message), type = "error")
        return(NULL)
      })
      
      if (!is.null(lat_longs) && nrow(lat_longs) > 0 && !is.na(lat_longs$latitude) && !is.na(lat_longs$longitude)) {
        output$map <- renderLeaflet({
          leaflet(lat_longs) %>%
            addTiles() %>%
            addMarkers(lng = ~longitude, lat = ~latitude, popup = ~paste("Code Postal:", loc_saisie))
        })
      } else {
        showNotification("Aucune donnée géographique trouvée pour ce code postal.", type = "error")
      }
    } else {
      showNotification("Veuillez saisir un code postal.", type = "error")
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)