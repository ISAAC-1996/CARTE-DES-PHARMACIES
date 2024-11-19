library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)
library(DBI)
library(odbc)
library(datamodelr)
library(DiagrammeR)
library(writexl)
library(sf)
library(leaflet)
library(shiny)
library(geosphere)
library(shinyjs)
library(openxlsx)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(plotly)

# Connexion à la base de données OSP_DATASTAT
con_osp_datastat <- DBI::dbConnect(odbc::odbc(), 
                                   Database = "OSP_DATASTAT", 
                                   Driver = Sys.getenv("sql_driver"), 
                                   Server = Sys.getenv("sql_server"), 
                                   UID = Sys.getenv("sql_uid"), 
                                   PWD = Sys.getenv("sql_pwd"), 
                                   Port = Sys.getenv("sql_port"))

# Table des coordonnées des régions françaises corrigées
coords_regions <- data.frame(
  nom= c("ALSACE", "AQUITAINE", "AUVERGNE", "BASSE NORMANDIE", "BOURGOGNE", 
         "BRETAGNE", "CENTRE", "CHAMPAGNE ARDENNES", "CORSE", "FRANCHE COMTE", 
         "HAUTE NORMANDIE", "ILE DE FRANCE", "LANGUEDOC ROUSSILLON", "LIMOUSIN", 
         "LORRAINE", "MIDI PYRENEES", "NORD PAS DE CALAIS", "PAYS DE LA LOIRE", 
         "PICARDIE", "POITOU CHARENTES", "PROVENCE COTE D'AZUR", "RHONE ALPES"),
  latitude = c(48.58, 44.84, 45.77, 49.18, 47.03, 48.12, 47.91, 48.96, 42.04, 47.34, 49.43, 48.85, 43.61, 45.83, 48.69, 43.6, 50.63, 47.47, 49.42, 46.58, 43.94, 45.76),
  longitude = c(7.75, -0.58, 3.08, -0.12, 4.83, -2.76, 1.71, 4.36, 9.06, 6.03, 1.09, 2.35, 3.87, 1.25, 6.18, 1.44, 3.06, -0.72, 2.83, -0.34, 6.07, 4.84))
view(coords_regions)

############ Importation des vaccin vendu 
Vac_2024 <- dbGetQuery(con_osp_datastat, "select nom_region, sum(qt_vendu_lcollect) as qte_2024,
SUM(qt_vendu_lcollect)*12.77 as ca_ht_2024,
count(distinct n_auto_adhpha_ecollect) as nbre_pheis_2024
from v_el_collect_ospharm
	 left join os_adhpha on n_auto_adhpha_ecollect = n_auto_adhpha
	 left join os_region on region_adhpha = n_auto_region
where dextinct_adhpha is null and syndic_adhpha = 1
and n_auto_adhpha_ecollect not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202310 and 202411 and moisok_completudepha in (0,8,9))
and n_auto_adhpha_ecollect in (select n_auto_adhpha from ex_adhpha where date_dernier_vente >= '2024-11-09')
and dateheure_ecollect between '2024-10-15 00:00:00.000' and '2024-11-09 23:59:00.000'
  and periode between 202410 and 202411
and n_auto_artic_lcollect in (select n_auto_artic from os_classif where n_auto_famille = '1J07A01')
group by nom_region")
Vac_2024  <- as_tibble(Vac_2024)
Vac_2023 <- dbGetQuery(con_osp_datastat, "select nom_region, sum(qt_vendu_lcollect) as qte_2023, 
SUM((pv_ttc_lcollect / (1 + (tva_lcollect / 100)))* qt_vendu_lcollect) as ca_h_2023t, 
count(distinct n_auto_adhpha_ecollect) as nbre_pheis_2023
from v_el_collect_ospharm
	 left join os_adhpha on n_auto_adhpha_ecollect = n_auto_adhpha
	 left join os_region on region_adhpha = n_auto_region
where dextinct_adhpha is null and syndic_adhpha = 1
and n_auto_adhpha_ecollect not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202310 and 202411 and moisok_completudepha in (0,8,9))
and dateheure_ecollect between '2023-10-15 00:00:00.000' and '2023-11-09 23:59:00.000'
  and n_auto_adhpha_ecollect in (select n_auto_adhpha from ex_adhpha where date_dernier_vente >= '2024-11-09')
  and periode between 202310 and 202402
and n_auto_artic_lcollect in (select n_auto_artic from os_classif where n_auto_famille = '1J07A01')
group by nom_region")
Vac_2023  <- as_tibble(Vac_2023)

Base_vaccin_vendu <- Vac_2024%>%
  left_join(Vac_2023, "nom_region")%>%
  rename(ca_ht_2023t = ca_h_2023t   )
view(Base_vaccin_vendu)
str(Base_vaccin_vendu)


####### Administré
admin_2024 <- dbGetQuery(con_osp_datastat, "select nom_region,
sum(qt_vendu_lcollect) as qte, 
count(distinct n_auto_adhpha_ecollect) as nbre_pheis 
from v_el_collect_ospharm
	 left join os_adhpha on n_auto_adhpha_ecollect = n_auto_adhpha
	 left join os_region on region_adhpha = n_auto_region
where dextinct_adhpha is null and syndic_adhpha = 1
and n_auto_adhpha_ecollect not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202310 and 202411 and moisok_completudepha in (0,8,9))
  and n_auto_adhpha_ecollect in (select n_auto_adhpha from ex_adhpha where date_dernier_vente >= '2024-11-09')
and dateheure_ecollect between '2024-10-15 00:00:00.000' and '2024-11-09 23:00:00.000'
and acteB2 = 'VGP'
group by nom_region")
admin_2024  <- as_tibble(admin_2024)

admin_2023 <- dbGetQuery(con_osp_datastat, "select nom_region,
sum(qt_vendu_lcollect) as qte, 
count(distinct n_auto_adhpha_ecollect) as nbre_pheis 
from v_el_collect_ospharm
	 left join os_adhpha on n_auto_adhpha_ecollect = n_auto_adhpha
	 left join os_region on region_adhpha = n_auto_region
where dextinct_adhpha is null and syndic_adhpha = 1
and n_auto_adhpha_ecollect not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202310 and 202411 and moisok_completudepha in (0,8,9))
  and n_auto_adhpha_ecollect in (select n_auto_adhpha from ex_adhpha where date_dernier_vente >= '2024-11-09')
and dateheure_ecollect between '2023-10-15 00:00:00.000' and '2023-11-09 23:00:00.000'
and acteB2 = 'VGP'
group by nom_region")
admin_2023  <- as_tibble(admin_2023)
Base_vaccin_admin <- admin_2024%>%
  left_join(admin_2023, "nom_region")%>%
  rename( qte_2024 = qte.x,
          nbre_pheis_2024 = nbre_pheis.x,
          qte_2023 = qte.y,
          nbre_pheis_2023 = nbre_pheis.y)
view(Base_vaccin_admin)
str(Base_vaccin_admin)
colnames(Base_vaccin_admin)



##############################################################

Base_vaccin_admin<- Base_vaccin_admin %>%
  mutate(nom_region = toupper(trimws(nom_region))) %>%  # Normaliser les noms
  left_join(coords_regions, by = c("nom_region" = "nom") )
str(Base_vaccin_admin)


Base_vaccin_vendu <- Base_vaccin_vendu %>%
  mutate(nom_region = toupper(trimws(nom_region))) %>%  # Normaliser les noms
  left_join(coords_regions, by = c("nom_region" = "nom") )
view(Base_vaccin_vendu)


# Créer un dossier temporaire pour enregistrer les histogrammes
temp_dir <- tempdir()

# Fonction pour générer les histogrammes
generate_histograms <- function(data, variable1, variable2, var_name, fill_colors) {
  icon_paths <- c()
  
  for (i in 1:nrow(data)) {
    region <- data[i, ]
    data_values <- c(region[[variable1]], region[[variable2]])
    years <- c("2023", "2024")
    
    # Créer l'histogramme avec ggplot2 et ajouter les étiquettes
    p <- ggplot(data = data.frame(years, data_values), aes(x = years, y = data_values, fill = years)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = format(data_values, big.mark = ",")), vjust = -0.5, size = 3) +  # Ajouter les étiquettes
      scale_fill_manual(values = fill_colors) +
      labs(title = NULL, x = NULL, y = var_name) +
      theme_void() +
      ylim(0, max(data[[variable2]]) * 1.1)  # Échelle basée sur les quantités maximales
    
    # Enregistrer l'histogramme en tant qu'image PNG
    file_path <- file.path(temp_dir, paste0("region_", i, "_", variable1, "_", variable2, ".png"))
    ggsave(file_path, plot = p, width = 2, height = 2, dpi = 300)
    icon_paths <- c(icon_paths, file_path)
  }
  
  return(icon_paths)
}

# Générer les histogrammes pour les vaccins vendus (rouge et bleu)
Base_vaccin_vendu$qte_icons <- generate_histograms(
  Base_vaccin_vendu,
  variable1 = "qte_2023",
  variable2 = "qte_2024",
  var_name = "Quantités vendues",
  fill_colors = c("2023" = "red", "2024" = "blue")
)

# Générer les histogrammes pour les vaccins administrés (orange et vert)
Base_vaccin_admin$qte_icons <- generate_histograms(
  Base_vaccin_admin,
  variable1 = "qte_2023",
  variable2 = "qte_2024",
  var_name = "Quantités administrées",
  fill_colors = c("2023" = "orange", "2024" = "green")
)

# Créer la carte interactive avec Leaflet
leaflet_map <- leaflet() %>%
  addTiles() %>%
  
  # Ajouter les marqueurs pour Vaccins Vendus
  addMarkers(
    data = Base_vaccin_vendu,
    ~longitude, ~latitude,
    icon = ~icons(iconUrl = qte_icons, iconWidth = 50, iconHeight = 50),
    popup = ~paste(
      "<b>Région :</b> ", nom_region, "<br>",
      "<b>Quantité 2023 :</b> ", format(qte_2023, big.mark = ","), "<br>",
      "<b>Quantité 2024 :</b> ", format(qte_2024, big.mark = ","), "<br>",
      "<b>Chiffre d'affaires 2023 :</b> ", format(ca_ht_2023t, big.mark = ","), " €<br>",
      "<b>Chiffre d'affaires 2024 :</b> ", format(ca_ht_2024, big.mark = ","), " €<br>",
      "<b>Nombre de pharmacies 2024 :</b> ", nbre_pheis_2024
    ),
    group = "Vaccins Vendus"
  ) %>%
  
  # Ajouter les marqueurs pour Vaccins Administrés
  addMarkers(
    data = Base_vaccin_admin,
    ~longitude, ~latitude,
    icon = ~icons(iconUrl = qte_icons, iconWidth = 50, iconHeight = 50),
    popup = ~paste(
      "<b>Région :</b> ", nom_region, "<br>",
      "<b>Quantité 2023 :</b> ", format(qte_2023, big.mark = ","), "<br>",
      "<b>Quantité 2024 :</b> ", format(qte_2024, big.mark = ","), "<br>",
      "<b>Nombre de pharmacies 2023 :</b> ", nbre_pheis_2023, "<br>",
      "<b>Nombre de pharmacies 2024 :</b> ", nbre_pheis_2024
    ),
    group = "Vaccins Administrés"
  ) %>%
  
  # Ajouter le contrôle des couches
  addLayersControl(
    overlayGroups = c("Vaccins Vendus", "Vaccins Administrés"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Ajouter des légendes spécifiques à chaque groupe
  addLegend(
    position = "bottomright",
    colors = c("red", "blue"),
    labels = c("Quantité 2023", "Quantité 2024"),
    title = "Vaccins Vendus",
    group = "Vaccins Vendus"
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("orange", "green"),
    labels = c("Quantité 2023", "Quantité 2024"),
    title = "Vaccins Administrés",
    group = "Vaccins Administrés"
  )

# Sauvegarder la carte comme fichier HTML autonome
saveWidget(leaflet_map, "carte_vaccins_vendus_administres.html", selfcontained = TRUE)

# Ouvrir la carte dans le navigateur
browseURL("carte_vaccins_vendus_administres.html")





















#exemple 
library(ggplot2)
library(leaflet)
library(dplyr)

# Données fictives : Régions françaises avec populations et chômage
regions_france <- data.frame(
  region = c("Île-de-France", "Auvergne-Rhône-Alpes", "Nouvelle-Aquitaine", 
             "Occitanie", "Hauts-de-France", "Provence-Alpes-Côte d'Azur",
             "Grand Est", "Bretagne", "Normandie", "Centre-Val de Loire"),
  latitude = c(48.85, 45.76, 44.84, 43.6, 50.63, 43.94, 48.58, 48.11, 49.18, 47.91),
  longitude = c(2.35, 4.84, -0.58, 3.87, 3.06, 6.07, 7.75, -2.76, 0.12, 1.71),
  population_2022 = c(12500000, 8000000, 6000000, 5900000, 6000000, 5000000, 5500000, 3300000, 3300000, 2700000),
  population_2023 = c(12100000, 8050000, 6050000, 5920000, 6050000, 5050000, 5520000, 3350000, 3330000, 2720000),
  chomage_2022 = c(8.5, 7.5, 8.0, 8.8, 9.5, 10.2, 7.8, 6.5, 7.9, 7.0),
  chomage_2023 = c(8.0, 7.3, 7.8, 8.5, 9.2, 9.8, 7.5, 6.3, 7.7, 6.8)
)

# Créer un dossier temporaire pour enregistrer les histogrammes
temp_dir <- tempdir()

generate_histograms <- function(variable, var_name, fill_colors) {
  icon_paths <- c()
  
  for (i in 1:nrow(regions_france)) {
    region <- regions_france[i, ]
    data_values <- c(region[[paste0(variable, "_2022")]], region[[paste0(variable, "_2023")]])
    years <- c("2022", "2023")
    
    # Créer l'histogramme avec ggplot2 et ajouter les étiquettes
    p <- ggplot(data = data.frame(years, data_values), aes(x = years, y = data_values, fill = years)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(data_values, 1)), vjust = -0.5, size = 3) +  # Ajouter les étiquettes
      scale_fill_manual(values = fill_colors) +
      labs(title = NULL, x = NULL, y = var_name) +
      theme_void() +
      ylim(0, max(regions_france[[paste0(variable, "_2023")]]) * 1.1)
    
    # Enregistrer l'histogramme en tant qu'image PNG
    file_path <- file.path(temp_dir, paste0("region_", i, "_", variable, ".png"))
    ggsave(file_path, plot = p, width = 2, height = 2, dpi = 300)
    icon_paths <- c(icon_paths, file_path)
  }
  
  return(icon_paths)
}

# Générer les histogrammes pour Population et Chômage avec étiquettes
regions_france$population_icons <- generate_histograms("population", "Population", c("2022" = "red", "2023" = "blue"))
regions_france$chomage_icons <- generate_histograms("chomage", "Taux de Chômage (%)", c("2022" = "orange", "2023" = "green"))


# Créer des groupes pour les thématiques
leaflet_map <- leaflet() %>%
  addTiles() %>%
  
  # Ajouter les marqueurs pour Population
  addMarkers(
    data = regions_france,
    ~longitude, ~latitude,
    icon = ~icons(iconUrl = population_icons, iconWidth = 50, iconHeight = 50),
    popup = ~paste(
      "<b>Région :</b> ", region, "<br>",
      "<b>Population 2022 :</b> ", format(population_2022, big.mark = ","),
      "<br><b>Population 2023 :</b> ", format(population_2023, big.mark = ",")
    ),
    group = "Population"
  ) %>%
  
  # Ajouter les marqueurs pour Chômage
  addMarkers(
    data = regions_france,
    ~longitude, ~latitude,
    icon = ~icons(iconUrl = chomage_icons, iconWidth = 50, iconHeight = 50),
    popup = ~paste(
      "<b>Région :</b> ", region, "<br>",
      "<b>Chômage 2022 :</b> ", chomage_2022, "%",
      "<br><b>Chômage 2023 :</b> ", chomage_2023, "%"
    ),
    group = "Chômage"
  ) %>%
  
  # Ajouter le contrôle des couches
  addLayersControl(
    overlayGroups = c("Population", "Chômage"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Ajouter une légende dynamique
leaflet_map <- leaflet_map %>%
  addLegend(
    position = "bottomright",
    colors = c("red", "blue"),
    labels = c("Population 2022", "Population 2023"),
    title = "Légende",
    group = "Population"
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("orange", "green"),
    labels = c("Chômage 2022", "Chômage 2023"),
    title = "Légende",
    group = "Chômage"
  )

# Sauvegarder la carte comme fichier HTML autonome
saveWidget(leaflet_map, "carte_population_chomage_legende_dynamique.html", selfcontained = TRUE)

# Ouvrir la carte dans le navigateur
browseURL("carte_population_chomage_legende_dynamique.html")









