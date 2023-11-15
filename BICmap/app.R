### Bowen Island Conservancy Web Mapping Application

library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(htmltools)
library(sf)
library(bcmaps)

dat_path <- "C:/Users/jeff.matheson/OneDrive/Documents/Spatial data library"

# Bowen boundary
bowen <- st_read(paste0(dat_path, "/Bowen_base/bowen_muni_bdry.gpkg")) %>% 
  st_transform(crs = 4326)

# Parcels
parcels <- st_read(paste0(dat_path, "/Bowen_base/Parcels/parcels_bowen.gpkg")) %>% 
  st_transform(crs = 4326)

# Create basemap
m <- leaflet() %>%
  addTiles(group = "OSM (default)", layerId = 1)  %>%
  addProviderTiles(providers$OpenTopoMap, group = "Open Topo") %>%    
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") %>%
  addPolygons(data = bowen, color = "green", fill = FALSE) %>% 
  addPolygons(data = parcels, color = "grey", fill  = FALSE, weight = 1,
              group = "Parcels") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Open Topo Map", "ESRI Imagery"),
    overlayGroups = c("Parcels"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addMeasure(primaryLengthUnit = "metres",
             primaryAreaUnit = "hectares") %>% 
  addGraticule()
m  

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Set theme
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    sidebarPanel(
      
      # App title
      h2("BICmap"),
      p ("Spatial layers relevant to biodiversity conservation on Bowen Island"),
      hr(),
      hr(),
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      leafletOutput("mymap", width = "100%", height = "600px")
    )
    )
  )

server <- function(input, output) {

  # Create Map    
  output$mymap <- renderLeaflet({
    m
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
