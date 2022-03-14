rm(list=ls())
library(mapview)
library(leafgl)
library(sf)
library(dplyr)
library(stringr)
library(leaflet)
library(RColorBrewer)
df <- data.table::fread("C:/Users/HVOSQM/Downloads/Filosofi2015_carreaux_200m_metropole.csv")
df$y <- as.integer(str_sub(str_extract(df$IdINSPIRE, "N\\d+"), 2))
df$x <- as.integer(str_sub(str_extract(df$IdINSPIRE, "E\\d+"), 2))

carroyage <- function(df, iCellSize=10000, var="Men", epsg="3035"){
  points_carroyage <- df # On repart de la base filtrée selon la première méthode
  points_carroyage$x_centroide = points_carroyage$x -
    (points_carroyage$x %% iCellSize) + (iCellSize / 2)
  points_carroyage$y_centroide = points_carroyage$y -
    (points_carroyage$y %% iCellSize) + (iCellSize / 2)
  
  points_carroyage <- points_carroyage %>% 
    group_by(x=x_centroide,y=y_centroide) %>% 
    count(name = var)
  points_carroyage <- btb::dfToGrid(df = points_carroyage, sEPSG = epsg, iCellSize = iCellSize)
  return(points_carroyage)
}

toto <- carroyage(df)

mapviewOptions(
  #platform = "leafgl"
  platform = "leaflet"
  , viewer.suppress = FALSE
)

mapview(
  toto
  , zcol = "Men"
  , at = quantile(toto$Men, seq(0, 1, 1/4))
  , col.regions=brewer.pal(9, "YlGn")
  , lwd = 0
)

# carr$geometry <- sprintf("POLYGON ((%i %i, %i %i, %i %i, %i %i, %i %i))", 
#                              carr$x, carr$y, 
#                              carr$x + tailleCarreaux, carr$y, 
#                              carr$x + tailleCarreaux, carr$y + tailleCarreaux, 
#                              carr$x, carr$y + tailleCarreaux, 
#                              carr$x, carr$y) 

#carreauxSf <- sf::st_as_sf(carr, wkt = "geometry", crs = 3035)

# Carto
