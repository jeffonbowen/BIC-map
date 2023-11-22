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
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 4fr"),
      
      card(
        card_header("Bowen Island Conservancy's Biodiversity Data Explorer"),
        card_body(p("This website is currently in development. Updates and revisions are ongoing."),
                  p("Select layers in the map window."),
                  p("Other map themes can be accessed in the menu bar.")
        )
        ),
      
      card(
        card_body(
          leafletOutput("main", width = "100%", height = "100%")
          )
        )
      )
    ),
  
  nav_panel(
    title = "Sensitive Ecosystems",
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 4fr"),
      
      card(
        card_header("Explore Locations of Sensitive Ecosystems"),
        card_body(
          p("Colour theme to be revised"),
          p("Still working on this selection tool."),
          selectInput("class",
                      label = "Select Sensitive Ecosystem",
                      choices = distinct(sei, comp1lgnd))
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
        card_body("Explore data from the Global Biodiversity Information Facility (GBIF)",
                  p("GBIF aggregates biodivsersity records from thousands of repositories, including eBird, iNaturalist and BC Conservation Data Centre."),
                  p("Select a point to show the species names."),
                  p("Lots to fix up here. Many common names missing."),
                  p("Show distribution of a selected species. Still refining it."),
                  selectInput("name",
                              label = "Select common name",
                              choices = arrange(distinct(gbif_dat, `English Name`))))
        ),
      card(
        leafletOutput("gbif", width = "100%", height = "100%")
        )    
      )
    ),

  nav_spacer(),  

  nav_menu(
    title = "About",
    align = "right",
    value = "Nothing here yet"
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

  output$main <- renderLeaflet({
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

  overlays.sei <-  c("Parcels", "Protected Areas", "SEI")
  
  # Need to fix up the pallete.
# previewColors(colorFactor(c("#CF5988", "#59261C", "#2F4063", "#FFA600", "#B35344", "#A2AB74", "#518724", "#96753F", "#314510", "#E85442", "#522961", "#871030", "#246954", "#B8E157"), domain = NULL), c("AP", "ES", "FW", "HB", "IT", "MF", "MF", "OD", "OF", "RI", "SV", "WN", "WD", "YF", "YS"))
  
# pal.sei <- colorFactor(c("#2F4063", "#FFA600", "#B35344", "#518724", "#96753F", 
#                            "#314510", "#E85442", "#522961", "#246954", "#871030", 
#                            "#E8DBBA", "#B8E157", "#FDD3BD"), 
#                          c("FW", "HB", "IT", "MF", "OD", "OF", "RI", "SV", "WD", "WN", "XX", "YF", "YS"))

  pal.sei <- colorFactor(palette = topo.colors(13), 
                          sei$comp1lgnd)

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
      addPolygons(data = sei, 
                  group = "SEI",
                  color = ~pal.sei(sei$comp1lgnd), 
                  fillOpacity = 0.6,
                  stroke = FALSE,
                  popup = paste("1: ", sei$comp1lgnd, "<br>", 
                                "2: ", sei$comp2lgnd_, "<br>", 
                                "3: ", sei$comp3lgnd)) |> 
      addLayersControl(
        baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
        overlayGroups = overlays.sei,
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft") |> 
      addLegend(pal = pal.sei, values = sei$comp1lgnd,
                title = "Dominant Eco", group = "SEI") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |> 
      addGraticule()
  })

#     observeEvent(
#     input$class, {
# #      sei1 <- filter(sei, comp1lgnd == input$class)
#       leafletProxy(sei) %>%
#         hideGroup("SEI") |> 
#         addPolygons(data = filter(sei, comp1lgnd == input$class), 
#                     color = "red", fill = TRUE, 
#                     fillOpacity = 0.6, stroke = FALSE, 
#                     group = "sei_filter") |> 
#         addPolygons(data = filter(sei, comp2lgnd == input$class), 
#                     color = "orange", fill = TRUE, 
#                     fillOpacity = 0.6, stroke = FALSE,
#                     group = "sei_filter") |> 
#         addPolygons(data = filter(sei, comp3lgnd == input$class), 
#                     color = "yellow", fill = TRUE, 
#                     fillOpacity = 0.6, stroke = FALSE,
#                     group = "sei_filter") |> 
#         addLegend(pal = c("red", "orange", "yellow"), 
#                   values = c("Primary", "Secondary", "Tertiary"),
#                   title = "SEI Class", group = "sei_filter")
#       }
#     )
#   
  
  overlay.gbif  <- c("Parcels", "Protected Areas", "GBIF Observations")
  pal.gbif <- colorFactor(topo.colors(6), gbif_dat$kingdom)
  
  output$gbif <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(group = "OSM (default)")  |>
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo") |>    
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") |>
      addPolygons(data = bowen, color = "green", fill = FALSE) |> 
      addPolygons(data = parcels, color = "grey", fill  = FALSE, weight = 1,
                  group = "Parcels") |> 
      addCircles(data = gbif_dat, ~long, ~lat, radius = 1,
                 popup = paste(gbif_dat$species, "<br>", gbif_dat$`English Name`),
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
      addLegend(pal = pal.gbif, values = gbif_dat$kingdom,
                title = "Kingdom", group = "GBIF Observations") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |> 
      hideGroup(c("Parcels", "Protected Areas"))
    })
  
  observeEvent(
    input$name, {
      gbif_filter <- filter(gbif_dat, `English Name` == input$name)
        leafletProxy("gbif") %>%
          clearGroup("gbif_select") |> 
          addCircles(data = gbif_filter, color = "black", fill = FALSE, 
                     radius = 40, weight = 8, opacity = 1, group = "gbif_select")
        }
    )
  
}

shinyApp(ui, server)