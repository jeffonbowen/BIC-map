### Bowen Island Conservancy Web Mapping Application - BICmap ###

library(shiny)
library(shinymanager)
library(bslib)
library(tidyverse)
library(leaflet)
library(htmltools)
library(sf)
library(terra)


# Authentication ----------------------------------------------------------

credentials <- data.frame(
  user = c("nature", "admin"), # mandatory
  password = c("lover", "12345"), # mandatory
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)


## Prep data and map

# Password to reveal map layers
pw <- "Naturenow!"


## Load overlay group layers

# For webmap dev, set this path.
dat_path <- "BICmap/"
# For shiny deployment
dat_path <- ""

bowen <- st_read(paste0(dat_path, "dat_spatial/bowen_muni_bdry.gpkg")) |> 
  st_transform(crs = 4326)
parcels <- st_read(paste0(dat_path, "dat_spatial/parcels_bowen.gpkg")) |>
  st_transform(crs = 4326)
parks <- st_read(paste0(dat_path, "dat_spatial/ParksGreenSpaces.shp")) |>
  st_transform(crs = 4326) |> 
  filter(!is.na(purpose)) |> filter(parkname != "Mount Gardner")
chm <- terra::rast("dat_spatial/chm10.tif")
sei <- st_read(paste0(dat_path, "dat_spatial/SEI_MV_Bowen.shp")) |> 
  st_set_crs(26910) |> 
  st_transform(crs = 4326) |> 
  filter(substr(secl_1,1,1) != "x") |> 
  mutate(secl_1 = factor(secl_1))


# User Interface ----------------------------------------------------------

ui <- page_sidebar(
  
  theme = bs_theme(bootswatch = "minty"),
  
  title = "BICmap",

# Sidebar content

  sidebar = sidebar(
      textInput("password", 
                label = "Enter password to view available map layers")
  ),

# Main panel content

  leafletOutput("mymap", width = "100%", height = "100%")
    
)



# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  overlays <- c("Parcels", "Protected Areas", "Canopy Height", "SEI")
  
  pal.chm <- colorNumeric(c("white", "green", "darkgreen"), values(chm),
                         na.color = "transparent")
  
# previewColors(colorFactor(c("#CF5988", "#59261C", "#2F4063", "#FFA600", "#B35344", "#A2AB74", "#518724", "#96753F", "#314510", "#E85442", "#522961", "#871030", "#246954", "#B8E157"), domain = NULL), c("AP", "ES", "FW", "HB", "IT", "MF", "MF", "OD", "OF", "RI", "SV", "WN", "WD", "YF", "YS"))
  
# pal.sei <- colorFactor(c("#2F4063", "#FFA600", "#B35344", "#518724", "#96753F", 
#                            "#314510", "#E85442", "#522961", "#246954", "#871030", 
#                            "#E8DBBA", "#B8E157", "#FDD3BD"), 
#                          c("FW", "HB", "IT", "MF", "OD", "OF", "RI", "SV", "WD", "WN", "XX", "YF", "YS"))

pal.sei <- colorFactor(palette = c("#2F4063", "#FFA600", "#B35344", "#518724", 
                                   "#96753F", 
                            "#314510", "#E85442", "#522961", "#246954", "#871030",
                            "#E8DBBA", "#B8E157", "#FDD3BD"), 
                          sei$secl_1)

observeEvent(input$password, {
  if(input$password == pw) {
    
    output$mymap <- renderLeaflet({
      leaflet() |>
        addTiles(group = "OSM (default)")  |>
        addProviderTiles(providers$OpenTopoMap, group = "Open Topo") |>    
        addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") |>
        addPolygons(data = bowen, color = "green", fill = FALSE) |> 
        addPolygons(data = parcels, color = "grey", fill  = FALSE, weight = 1,
                    group = "Parcels") |> 
        addPolygons(data = parks, color = "red", fill  = FALSE, weight = 2,
                    group = "Protected Areas",
                    popup = parks$parkname) |> 
        addPolygons(data = sei, fillColor = "blue",
                    group = "SEI",
  #                  color = ~pal.sei(sei$secl_1),
                    popup = paste(sei$comp1lgnd, "<br>", 
                                  sei$comp2lgnd, "<br>", sei$comp3lgnd)) |> 
        addRasterImage(chm, colors = pal.chm, opacity = 0.8,
                       group = "Canopy Height") |>
        addLayersControl(
          baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topleft") |>
        addMeasure(primaryLengthUnit = "metres",
                   primaryAreaUnit = "hectares") |> 
        addGraticule() |> 
        hideGroup(overlays)
    })
  }
}
)

  # observeEvent(input$password, {
  #   if(input$password == pw) {
  #     leafletProxy("mymap") |>
  #       addLayersControl(
  #         baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
  #         overlayGroups = overlays,
  #         options = layersControlOptions(collapsed = FALSE),
  #         position = "topleft") |>
  #       addLegend(pal = pal.chm, values = values(chm),
  #                 title = "Canopy Ht (m)", group = "Canopy Height") |> 
  #       hideGroup("Canopy Height") |> 
  #       showGroup("Canopy Height")
  #       
  #   }
  #   }
  #   )
}

shinyApp(ui, server)