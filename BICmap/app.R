### Bowen Island Conservancy Web Mapping Application - BICmap ###


library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(htmltools)
library(sf)

## Prep data and map

# Password to reveal map layers
pw <- "Naturenow!"

# For dev, load layers this way.
dat_path <- "C:/Users/jeff.matheson/OneDrive/Documents/Spatial data library"
dat_path <- "C:/Users/jeff.matheson/OneDrive/Documents/Spatial data library"


# Load layers for development. Transform to lat long.
# bowen <- st_read("BICmap/dat_spatial/bowen_muni_bdry.gpkg") %>%
#   st_transform(crs = 4326)
# parcels <- st_read("BICmap/dat_spatial/parcels_bowen.gpkg") %>%
#   st_transform(crs = 4326)
# parks <- st_read("BICmap/dat_spatial/parcels_bowen_park.gpkg") %>%
#   st_transform(crs = 4326)

# Load layers for deployment. Transform to lat long.
bowen <- st_read("dat_spatial/bowen_muni_bdry.gpkg") %>% 
  st_transform(crs = 4326)
parcels <- st_read("dat_spatial/parcels_bowen.gpkg") %>%
  st_transform(crs = 4326)
parks <- st_read("dat_spatial/parcels_bowen_park.gpkg") %>%
  st_transform(crs = 4326)



# Create basemap
m <- leaflet() %>%
  addTiles(group = "OSM (default)", layerId = 1)  %>%
# addProviderTiles(providers$OpenTopoMap, group = "Open Topo") %>%    
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") %>%
  addPolygons(data = bowen, color = "green", fill = FALSE) %>% 
  addPolygons(data = parcels, color = "grey", fill  = FALSE, weight = 1,
              group = "Parcels") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "ESRI Imagery"),
    overlayGroups = c("Parcels"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addMeasure(primaryLengthUnit = "metres",
             primaryAreaUnit = "hectares") %>% 
  addGraticule()
m  


ui <- page_sidebar(
  
  title = "BICmap",

# Sidebar content

    sidebar = sidebar(
    textInput("password", label = "Password to reveal map layers:", "Not enabled"),
    h5("Map Layers"),
    checkboxInput("muniparks", label = "Municipal Parks", value = FALSE),
    checkboxInput("covenants", label = "Mapped Covenants (n/a)")
  ),

  
  
# Main panel content

    leafletOutput("mymap", width = "100%", height = "100%")
    
)


shinyApp(ui, function(input, output) {
  
  output$mymap <- renderLeaflet({
    m
  })

  observeEvent(
    input$muniparks, {
      draw <- input$muniparks
      if (draw == TRUE) {
        leafletProxy("mymap") %>%
          addPolygons(data = parks, color = "red", fill  = FALSE, weight = 1)
      }
      if (draw == FALSE) 
        {
        output$mymap <- renderLeaflet({
          m
        })
        }
      }
    )
  })