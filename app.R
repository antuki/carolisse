library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinyWidgets)
library(mapview)
library(sf)
library(dplyr)
library(stringr)
library(btb)

# mapviewOptions(
#   viewer.suppress = FALSE
# )

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                 switchInput(
                  "lisser",
                  label = "lisser",
                  value = FALSE,
                  onLabel = "OUI",
                  offLabel = "NON",
                  size = "large",
                  width = "100px"
                )
                  )
)


server <- function(input, output, session) {
  
  ## chargement des données complètes
  df <- data.table::fread("https://minio.lab.sspcloud.fr/kantunez/filo/Filosofi2015_carreaux_200m_metropole_small.csv",
                          select=c("IdINSPIRE","Men"))
  df$y <- as.integer(str_sub(str_extract(df$IdINSPIRE, "N\\d+"), 2))
  df$x <- as.integer(str_sub(str_extract(df$IdINSPIRE, "E\\d+"), 2))
  
  # tata <- st_as_sf(df, coords=c("x","y"), crs = 3035) %>%
  #   st_transform(4326) 
  # tata <- tata %>% cbind(x = st_coordinates(tata)[,1],y = st_coordinates(tata)[,2] )
  #tata <- tata %>% st_drop_geometry()
  # write.csv(tata,"tata.csv")
  
  carroyage <- function(df, iCellSize=20000, epsg="3035", var="Men"){
    points_carroyage <- df # On repart de la base filtrée selon la première méthode
    points_carroyage$x_centroide = points_carroyage$x -
      (points_carroyage$x %% iCellSize) + (iCellSize / 2)
    points_carroyage$y_centroide = points_carroyage$y -
      (points_carroyage$y %% iCellSize) + (iCellSize / 2)
    
    points_carroyage <- points_carroyage %>% 
      group_by(x=x_centroide,y=y_centroide) %>%
      summarise(Men = sum(Men), .groups="drop")

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

 
  getBounds <- reactive({
    bounds <<- input$map_bounds
    bbox_new <- data.frame(lon = c(bounds$west, bounds$east),
                           lat =  c(bounds$north,bounds$south)) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(3035) %>% st_bbox()
    bbox_new <- bbox_new*c(0.8,0.8,1.2,1.2)
  })
  
  taillCar <- reactive({
    zoom <- input$map_zoom
    taille <- min(max(1200000/exp(0.7*zoom),200),20000)
    print(taille)
  })
  
  indicLiss <- reactive({
    input$lisser
  })
    
  df_filtre <- reactive({
    liss <- indicLiss()
    bbox_new <- getBounds()
    df2 <- df[df$x >= bbox_new[[1]] & df$x <= bbox_new[[3]] & df$y >= bbox_new[[2]] & df$y <= bbox_new[[4]], ]
    toto <- carroyage(df2, iCellSize=taillCar())
    print(input$lisser)
    if(liss){
      toto <- kernelSmoothing(dfObservations = toto %>% st_drop_geometry(), 
                              sEPSG = "3035",
                              iCellSize = taillCar(), 
                              iBandwidth = 3*taillCar()) %>% st_transform(4326)
      
    }
    toto
   })
  
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.

  #observeEvent(df_filtre(), {
  observe({
    pal <- colorBin("YlOrRd", domain = df_filtre()$Men, bins = unique(quantile(df_filtre()$Men,probs=seq(0, 1, 0.2))))
    
    leafletProxy("map", data = df_filtre()) %>%
     clearShapes() %>% 
       addPolygons(weight = 0, smoothFactor = 1,
                   fillOpacity = 0.6,
                   fillColor = ~pal(Men))
  })
  
  
}

shinyApp(ui, server)