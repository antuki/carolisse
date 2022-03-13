library(mapview)
library(leafgl)
library(sf)

options(viewer = NULL)

# Fonction permettant de faire le chargement souhaité
st_read_maison <- function(chemin_tab){
  requete <- "SELECT IdINSPIRE,Depcom,I_est_cr,Men, Log_soc, geom
            FROM Filosofi2015_carreaux_200m_metropole
            WHERE SUBSTR(Depcom, 1, 2) IN ('75','92','93','94') "
  sf::st_read(chemin_tab, query = requete)
}

# Chargement des données
chemin_file <- paste0("https://minio.lab.sspcloud.fr/projet-formation/r-lissage-spatial/Filosofi2015_carreaux_200m_metropole.gpkg")
#carreaux <- st_read(chemin_file)
carreaux2 <-  st_read_maison(chemin_file)  %>% st_transform(4326)

# Carto
mapviewOptions(
  platform = "leafgl"
  #platform = "leaflet"
  , viewer.suppress = FALSE
)

mapview(
  carreaux2
  , zcol = "Men"
  , at = quantile(carreaux2$Men, seq(0, 1, 1/8))
  , lwd = 0
)

rm(list=ls())
library(mapview)
library(leafgl)
library(sf)
library(dplyr)
library(stringr)
tailleCarreaux <- 200
carr <- data.table::fread("C:/Users/HVOSQM/Downloads/Filosofi2015_carreaux_200m_metropole.csv")
carr$x <- as.integer(str_sub(str_extract(carr$IdINSPIRE, "N\\d+"), 2))
carr$y <- as.integer(str_sub(str_extract(carr$IdINSPIRE, "E\\d+"), 2))
# carr$xmin <- carr$x - cellsize/2
# carr$xmax <- carr$x + cellsize/2
# carr$ymin <- carr$y - cellsize/2
# carr$ymax <- carr$y + cellsize/2

carr$geometry <- sprintf("POLYGON ((%i %i, %i %i, %i %i, %i %i, %i %i))", 
                         carr$x, carr$y, 
                         carr$x + tailleCarreaux, carr$y, 
                         carr$x + tailleCarreaux, carr$y + tailleCarreaux, 
                         carr$x, carr$y + tailleCarreaux, 
                         carr$x, carr$y) 

carreauxSf <- sf::st_as_sf(carr, wkt = "geometry", crs = 3035)
carr_sf = st_as_sf(carr, coords = c("x", "y"), crs = 3035, agr = "constant")

# Carto
mapviewOptions(
  platform = "leafgl"
  #platform = "leaflet"
  , viewer.suppress = TRUE
)

toto <- carr_sf %>% slice(1:100)

square_green <-
  leaflet::makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/green-square-1.png",
                    iconWidth = 18,
                    iconHeight = 18)

library(leaflet)
m <- mapview(toto)

m@map %>%addMarkers(~x, ~y,data=toto)

leaflet(toto) %>% addTiles() %>% addCircleMarkers()

leaflet(toto) %>% addTiles() %>% addRectangles(lat1=toto$ymin, lat2=toto$ymax,
                                               lng1=toto$xmin,lng2=toto$xmax)


leaflet(breweries91) %>% 
  addTiles() %>% 
  
  addCircleMarkers() %>%
  addMarkers(
    icon = ~ icons(
      iconUrl = "https://www.freeiconspng.com/uploads/green-square-1.png"
    )
  )

greenLeafIcon <- makeIcon(
  iconUrl = "https://www.freeiconspng.com/uploads/green-square-1.png",
  iconWidth = 38, iconHeight = 95,
)

leaflet(data = quakes[1:4,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = greenLeafIcon)

#https://rstudio.github.io/leaflet/markers.html



# Erreur dans les liens source : Error: NetworkError when attempting to fetch resource.
# URL de la ressource : https://user-kantunez-563070-0.kub.sspcloud.fr/session/viewhtml4653ec8102e/lib/Leaflet.glify-2.2.0/glify-browser.js
# URL du lien source : /glify-browser.js.map

