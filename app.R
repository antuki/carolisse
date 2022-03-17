library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinyWidgets)
library(mapview)
library(sf)
library(dplyr)
library(stringr)

# mapviewOptions(
#   viewer.suppress = FALSE
# )

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderTextInput(
                  inputId = "range",
                  label = "Taille carreaux (m)", 
                  choices = c(200, 500, 1000, 2000, 10000),
                  grid = FALSE
                ),
                
                
                checkboxInput("legend", "Légende", TRUE)
  )
)

server <- function(input, output, session) {
  
  ## chargement des données complètes
  df <- data.table::fread("https://minio.lab.sspcloud.fr/kantunez/filo/Filosofi2015_carreaux_200m_metropole_small.csv")
  df$y <- as.integer(str_sub(str_extract(df$IdINSPIRE, "N\\d+"), 2))
  df$x <- as.integer(str_sub(str_extract(df$IdINSPIRE, "E\\d+"), 2))
  
  # tata <- st_as_sf(df, coords=c("x","y"), crs = 3035) %>%
  #   st_transform(4326) 
  # tata <- tata %>% cbind(x = st_coordinates(tata)[,1],y = st_coordinates(tata)[,2] )
  #tata <- tata %>% st_drop_geometry()
  # write.csv(tata,"tata.csv")
  
  carroyage <- function(df, iCellSize=10000, epsg="3035", var="Men"){
    points_carroyage <- df # On repart de la base filtrée selon la première méthode
    points_carroyage$x_centroide = points_carroyage$x -
      (points_carroyage$x %% iCellSize) + (iCellSize / 2)
    points_carroyage$y_centroide = points_carroyage$y -
      (points_carroyage$y %% iCellSize) + (iCellSize / 2)
    
    points_carroyage <- points_carroyage %>% 
      group_by(x=x_centroide,y=y_centroide) %>% 
      count(name = "Men")
    points_carroyage <- btb::dfToGrid(df = points_carroyage, sEPSG = epsg, iCellSize = iCellSize) %>% st_transform(4326) 
    return(points_carroyage)
  }
  
  toto <- carroyage(df)
  
  # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   quakes[quakes$mag >= 4 & quakes$mag <= 6,]
  # })

    # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
 #  colorpal <- reactive({
 # #   colorNumeric(input$colors, quakes$mag)
 #  })
  
  output$map <- renderLeaflet({
    # éléments non dynamique
     leaflet(toto) %>% addTiles() %>% 
         fitBounds(-5.162033,41.366285,9.626612,51.140740 )
        
  })
  
 df_filtre <- reactive({
   bounds <- input$map_bounds
   bbox_new <- data.frame(lon = c(bounds$west, bounds$east),
                          lat =  c(bounds$north,bounds$south)) %>%
     st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
     st_transform(3035) %>% st_bbox()
   df2 <- df[df$x >= bbox_new[[1]] & df$x <= bbox_new[[3]] & df$y >= bbox_new[[2]] & df$y <= bbox_new[[4]], ]
   toto <- carroyage(df2)   

   })
  
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  observeEvent(input$map_bounds, {
    leafletProxy("map", data = df_filtre()) %>%
     clearShapes() %>% 
       addPolygons(weight = 0, smoothFactor = 1,
                   fillOpacity = 0.6,
                   fillColor = ~colorQuantile("YlOrRd", Men)(Men))
  })
  
  
}

shinyApp(ui, server)