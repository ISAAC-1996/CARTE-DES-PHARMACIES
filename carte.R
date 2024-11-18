library(leaflet)
library(htmlwidgets)

# Créer une carte centrée sur Londres avec un marqueur
leaflet() %>%
  addTiles() %>%  # Ajouter les tuiles OpenStreetMap par défaut
  setView(lng = -0.1276, lat = 51.5074, zoom = 12) %>%  # Centrer la carte sur Londres
  addMarkers(
    lng = -0.1439, lat = 51.5096,  # Coordonnées du marqueur
    popup = "A pretty CSS popup.<br>Easily customizable."
  )




library(leaflet)
library(htmlwidgets)

# Définir une icône verte pour les pharmacies ouvertes
greenPharmacyIcon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png",
  iconWidth = 25, 
  iconHeight = 41, 
  iconAnchorX = 12.5,  
  iconAnchorY = 41   
)

# Définir une icône rouge pour les pharmacies fermées
redPharmacyIcon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
  iconWidth = 25,  
  iconHeight = 41, 
  iconAnchorX = 12.5,  
  iconAnchorY = 41   
)

# Créer une carte avec deux pharmacies
my_map <- leaflet() %>%
  addTiles() %>%  
  setView(lng = -0.1276, lat = 51.5074, zoom = 12) %>%  
  addMarkers(
    lng = -0.1439, lat = 51.5096,  
    popup = "Pharmacie ouverte 24h/24",  
    icon = greenPharmacyIcon  
  ) %>%
  addMarkers(
    lng = -0.1200, lat = 51.5033,  
    popup = "Pharmacie fermée pour rénovation",  
    icon = redPharmacyIcon  
  )

# Sauvegarder la carte dans un fichier HTML
saveWidget(my_map, "C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandesma_carte_pharmacies.html", selfcontained = TRUE)
