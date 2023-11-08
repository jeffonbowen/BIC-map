### Bowen Island Conservancy WEb Mapping Application


library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(htmltools)
library(sf)
library(bcmaps)

dat_path <- "C:/Users/jeff.matheson/OneDrive/Documents/Spatial data library"

# Bowen boundary
# Comes in bc albers.Lidar data is in UTM Xone 10.
munis <- bcmaps::municipalities()
bowen <- filter(munis, ADMIN_AREA_ABBREVIATION == "Bowen Island") %>% 
  st_transform(crs = 4326) %>% as_Spatial()

parcels <- st_read(paste0(dat_path, "/Bowen_base/Parcels/parcels_bowen.shp"))


# Create basemap

basemap <- leaflet(options = leafletOptions(minZoom = 5, maxZoom = 30)) %>% 
  addTiles(group = "OSM (default)")  %>%   
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%    
  addProviderTiles(providers$Wikimedia, group = "Wikimedia") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Street Map", "Wikimedia", "Imagery"),
#      overlayGroups = c("Point Count Locations","Detections"),
    options = layersControlOptions(collapsed = TRUE)) %>% 
  # Add measurement tool
  addMeasure(primaryLengthUnit = "metres",
             primaryAreaUnit = "hectares") %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
             opacity = 0.8, fillOpacity = 0, data = bowen, 
             group = "Boundaries") 
basemap


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Set theme
  theme = shinytheme("flatly"),
  
  # App title
  titlePanel("Bowen Island Conservancy - BICmap"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    sidebarPanel(
      
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
    basemap
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
