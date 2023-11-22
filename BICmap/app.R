### Bowen Island Conservancy Web Mapping Application - BICmap ###

library(shiny)
library(shinymanager)
library(bslib)
library(tidyverse)
library(leaflet)
library(leafgl)
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

# Prep data and map -------------------------------------------------------

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
gbif_dat <- read_csv("dat_spatial/gbif_common.csv") |> 
  mutate(kingdom = factor(kingdom))


# User Interface ----------------------------------------------------------

ui <- page_navbar(
  
  theme = bs_theme(bootswatch = "minty"),
  
  title = "BICmap",
  
  nav_panel(
    title = "Home",
    leafletOutput("main", width = "100%", height = "100%")
    ),
  
  nav_panel(
    title = "Sensitive Ecosystems",
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 4fr"),
      
      card(
        card_header("Explore Locations of Sensitive Ecosystems"),
        card_body(
          selectInput("class",
                      label = "Select Sensitive Ecosystem",
                      choices = c("Wetland", "Mature Forest"))
          ),
        ),
      
      card(
        leafletOutput("sei", width = "100%", height = "100%")
        )
  )
  ),
  
  nav_panel(
    title = "Biodiversity Data",
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 4fr"),
      card(
        card_body("Explore data from the Global Biodiversity Information Facility (GBIF)")
        ),
      card(
        leafletOutput("gbif", width = "100%", height = "100%")
        )    
      )
    ),

  nav_spacer(),  

  nav_menu(
    title = "About",
    align = "right"
    )
  )

# Wrap your UI with secure_app
ui <- secure_app(ui)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Main map
  
  overlays.main <- c("Parcels", "Protected Areas", "Canopy Height")
  
  pal.chm <- colorNumeric(c("white", "green", "darkgreen"), values(chm),
                         na.color = "transparent")

  output$home <- renderLeaflet({
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
      addRasterImage(chm, colors = pal.chm, opacity = 0.8,
                     group = "Canopy Height") |>
      addLayersControl(
        baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
        overlayGroups = overlays.main,
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft") |>
      addLegend(pal = pal.chm, values = values(chm),
                title = "Canopy Ht (m)", group = "Canopy Height") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |> 
      addGraticule() |> 
      hideGroup(overlays.main)
  })
  
  # SEI Map

  overlays.sei <- "SEI"
  
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

  output$sei <- renderLeaflet({
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
      addLayersControl(
        baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
        overlayGroups = overlays.sei,
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |> 
      addGraticule()
  })

  
  overlay.gbif  <- c("Parcels", "Protected Areas", "GBIF Observations")
  pal.gbif <- colorFactor(topo.colors(6), gbif_common$kingdom)
  
  output$gbif <- renderLeaflet({
      leaflet() |>
      addTiles(group = "OSM (default)")  |>
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo") |>    
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") |>
      addPolygons(data = bowen, color = "green", fill = FALSE) |> 
      addPolygons(data = parcels, color = "grey", fill  = FALSE, weight = 1,
                  group = "Parcels") |> 
      addCircles(data = gbif_common, ~long, ~lat, radius = 1,
                 popup = gbif_common,
                 color = ~pal.gbif(kingdom),
                 group = "GBIF Observations") |> 
      addPolygons(data = parks, color = "red", fill  = FALSE, weight = 2,
                  group = "Protected Areas",
                  popup = parks$parkname) |> 
      addLayersControl(
        baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
        overlayGroups = overlay.gbif,
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft") |>
      addLegend(pal = pal.gbif, values = ~kingdom,
                title = "Kingdom", group = "GBIF Observations") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares")
    })
  
}

shinyApp(ui, server)