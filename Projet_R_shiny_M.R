setwd("C:/Users/randh/OneDrive - univ-lyon2.fr/Programmation R/dataset")

# Chargement des packages
library(httr)
library(jsonlite)

# Liste des codes postaux
cp <- sort(c(69790, 69170, 69250, 69380, 69009, 69008, 69006, 69007, 69005, 
             69001, 69004, 69002, 69003, 69780, 69360, 69124, 69580, 69720, 
             69960, 69320, 69800, 69140, 69330, 69970, 69730, 69740, 69150, 
             69680, 69510, 69390, 69910, 69100, 69640, 69770, 69400, 69430, 
             69820, 69460, 69200, 69120, 69420, 69670, 69890, 69620, 69240, 
             69160, 69220, 69440, 69490, 69700, 69560, 69590, 69270, 69870, 
             69210, 69850, 69930, 69690, 69550, 69650, 69830, 69290, 69230, 
             69610, 69110, 69190, 69450, 69370, 69280, 69470, 69480, 69530, 
             69310, 69600, 69350, 69860, 69760, 69840, 69540, 69520, 69340, 
             69130, 69570, 69660, 69115, 69260, 69410, 69630, 69300, 69500, 
             69126))

# Dataframe global pour stocker les résultats
global_data <- data.frame()

# Parcours de chaque code postal
for (i in cp) {
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
  
  # Récupération initiale (2021-2024)
  params <- list(
    
    select = elect = "N°DPE,Date_établissement_DPE,Date_visite_diagnostiqueur,Modèle_DPE,Code_postal_(BAN),Nom_commune(BAN),Etiquette_DPE,Version_DPE,Date_réception_DPE,Coût_total_5_usages,Coût_chauffage,Coût_éclairage,Coût_ECS,Méthode_application_DPE,Coût_refroidissement,Coût_auxiliaires,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN),Type_bâtiment",
    page = 1,
    size = 10000, # Augmenté pour récupérer plus de données
    q = i,
    q_fields = "Code_postal_(BAN)",
    qs = "Date_réception_DPE:[2021-01-01 TO 2024-09-01]"
  ) 
  
  # Encodage des paramètres
  url_encoded <- modify_url(base_url, query = params)
  response <- GET(url_encoded)
  # Vérification du statut de la réponse
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content), flatten = TRUE)
    
    # Vérifier si des données ont été retournées
    if (!is.null(content$results)) {
      # Ajouter les données au dataframe global avec rbind
      global_data <- rbind(global_data, content$results)
    } else {
      message("Pas de données pour le code postal : ", i)
    }
  } else {
    message("Erreur avec le code postal : ", i, " - Code statut : ", status_code(response))
  }
  
}

# Écriture du dataframe global en fichier CSV après la boucle
write.table(global_data, "resultats_dpe.csv", sep = ";", row.names = FALSE)

