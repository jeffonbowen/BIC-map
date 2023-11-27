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
library(DT)

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

# For dev, set this working directory
# setwd("C:/Users/jeff.matheson/OneDrive - Tetra Tech, Inc/Documents/R_Projects/BowenMapping/BICmap")

bowen <- st_read("dat/bowen_muni_bdry.gpkg") |> 
  st_transform(crs = 4326)
parcels <- st_read("dat/parcels_bowen.gpkg") |>
  st_transform(crs = 4326)
parks <- st_read("dat/ParksGreenSpaces.shp") |>
  st_transform(crs = 4326) |> 
  filter(!is.na(purpose)) |> filter(parkname != "Mount Gardner")
chm <- terra::rast("dat/chm10.tif")
sei <- st_read("dat/SEI_MV_Bowen.shp") |> 
  st_set_crs(26910) |> 
  st_transform(crs = 4326) |> 
  filter(substr(secl_1,1,1) != "x") 
gbif <- readRDS(file = "dat/gbif_test.rds") |> 
  mutate(kingdom = factor(kingdom))

# Prep the SEI data
sei_lgnd <- tibble(
  complgnd = c("Alpine", "Estuarine", "Freshwater", "Herbaceous", "Intertidal", 
             "Mature Forest (ME)", "Mature Forest (SE)", "Old Field", 
             "Old Forest", "Riparian", "Sparsely Vegetated", "Woodland", 
             "Wetland", "Non SE/ME", "Young Forest", "Yound Forest (small)",
             "River (Riparian subclass)", "Freshwater Reservoirs"),
  hex_colour = c("#CF5988", "#59261C", "#2F4063", "#FFA600", "#B35344", 
                 "#A2AB74", "#518724", "#96753F", "#314510", "#E85442", 
                 "#522961", "#246954", "#871030", "#E8DBBA", "#B8E157", 
                 "#FDD3BD", "#6C81A6", "#6EB5DB")) |> 
  filter(!(complgnd %in% c("Alpine", "River (Riparian subclass)", 
                           "Freshwater Reservoirs")))
sei <- sei |> 
  mutate(comp1lgnd = factor(comp1lgnd, levels = sei_lgnd$complgnd),
         comp2lgnd = factor(comp2lgnd, levels = sei_lgnd$complgnd),
         comp3lgnd = factor(comp3lgnd, levels = sei_lgnd$complgnd)
         )


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
    title = "Sensitive Ecosystem Explorer",
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 4fr"),
      
      card(
        card_header("Explore Locations of Sensitive Ecosystems"),
        card_body(
          selectInput("class",
                      label = "Select Sensitive Ecosystem",
                      choices = sei_lgnd$complgnd),
          p("The sensitive ecosystem may occur as the dominant ecosystem in a polygon or as a secondary or tertiary ecocosystem.")
          )
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
        card_body(
          "Explore data from the Global Biodiversity Information Facility (GBIF)",
          p("GBIF aggregates biodivsersity records from thousands of repositories, including eBird, iNaturalist and BC Conservation Data Centre."),
          p("Select a point to show the species name."),
          p("Lots to fix up here. Many common names missing."),
          p("Show distribution of a selected species. Still refining it."),
          selectInput("name",
                      label = "Select common name",
                      choices = arrange(distinct(gbif, verbatimScientificName))))
        ),
      card(
        leafletOutput("gbif", width = "100%", height = "100%")
        )    
      )
    ),

  nav_panel(
    title = "Species Tables",
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 4fr"),
      card(
        card_header("Explorer GBIF  Records"),
        card_body(
          "Number of records",
          "Plot by Kingdom"),
        card_footer(
          p("GBIF Occurrence Download https://doi.org/10.15468/dl.pcmuhe Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2023-11-26", style = 'font-size: 10px;')
          ),
        card_header("Selected Record Location"),
        card_body(
          leafletOutput("gbif", width = "100%", height = "100%")
        )
      ),
      card(
        DTOutput("species_table", height = "auto", fill = TRUE)
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
  }
  )
  
  # Main map
  
  overlays.main <- c("Parcels", "Protected Areas", "SEI", "Canopy Height")
  
  pal.chm <- colorNumeric(c("white", "green", "darkgreen"), values(chm),
                         na.color = "transparent")

  pal.sei <- colorFactor(sei_lgnd$hex_colour, sei_lgnd$complgnd)
  
  pop.sei <- paste("1: ", sei$comp1lgnd_, "<br>", 
                   "2: ", sei$comp2lgnd_, "<br>", 
                   "3: ", sei$comp3lgnd_)
  
  output$main <- renderLeaflet({
    leaflet() |>
      addTiles(group = "OSM (default)")  |>
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo") |>    
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") |>
      addPolygons(data = bowen, color = "green", fill = FALSE) |> 
      addPolygons(data = sei, 
                  group = "SEI",
                  color = ~pal.sei(sei$comp1lgnd), 
                  fillOpacity = 1.0, stroke = FALSE,
                  popup = pop.sei) |> 
      addRasterImage(chm, colors = pal.chm, opacity = 0.8,
                     group = "Canopy Height") |>
      addPolygons(data = parcels, color = "grey", fill  = FALSE, weight = 1,
                  group = "Parcels") |> 
      addPolygons(data = parks, color = "red", fill  = FALSE, weight = 2,
                  group = "Protected Areas",
                  popup = parks$parkname) |> 
      addLayersControl(
        baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
        overlayGroups = overlays.main,
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft") |>
      addLegend(pal = pal.sei, values = sei$comp1lgnd,
                title = "Dominant Eco", group = "SEI", opacity = 1) |> 
      addLegend(pal = pal.chm, values = values(chm),
                title = "Canopy Ht (m)", group = "Canopy Height") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |> 
      hideGroup(overlays.main)
  })
  
  # SEI Explorer

  overlays.sei <-  c("Parcels", "Protected Areas", "SEI")

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
                  color = "green", 
                  fill = TRUE, fillOpacity = 0, stroke = TRUE,
                  weight = 1, popup = pop.sei) |> 
      addLayersControl(
        baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
        overlayGroups = overlays.sei,
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |> 
      hideGroup(c("Parcels", "Protected Areas"))
  })

     observeEvent(
       input$class, {
         leafletProxy("sei") %>%
           clearGroup("select") |>
           clearControls() |>
           addPolygons(data = filter(sei, comp3lgnd == input$class),
                       color = "yellow", fill = TRUE,
                       fillOpacity = 0.8, stroke = FALSE,
                       group = "select") |>
           addPolygons(data = filter(sei, comp2lgnd == input$class),
                       color = "orange", fill = TRUE,
                       fillOpacity = 0.8, stroke = FALSE,
                       group = "select") |>
           addPolygons(data = filter(sei, comp1lgnd == input$class),
                       color = "red", fill = TRUE,
                       fillOpacity = 0.8, stroke = FALSE,
                       group = "select") |>
           addPolygons(data = sei, 
                       group = "SEI",
                       color = "green", 
                       fill = TRUE, fillOpacity = 0, stroke = TRUE,
                       weight = 1, popup = pop.sei) |> 
           addLegend(colors = c("red", "orange", "yellow"),
                     labels = c("Primary", "Secondary", "Tertiary"),
                     title = "Ecosystem Dominance")
         }
       )

  
  overlay.gbif  <- c("Parcels", "Protected Areas", "GBIF Observations")
  pal.gbif <- colorFactor(topo.colors(6), gbif$kingdom)
  
  output$gbif <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(group = "OSM (default)")  |>
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo") |>    
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") |>
      addPolygons(data = bowen, color = "green", fill = FALSE) |> 
      addPolygons(data = parcels, color = "grey", fill  = FALSE, weight = 1,
                  group = "Parcels") |> 
      addCircles(data = gbif, ~decimalLongitude, ~decimalLatitude, radius = 1,
                 popup = paste(gbif$scientificName, "<br>", 
                               gbif$vernacularName),
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
      addLegend(pal = pal.gbif, values = gbif$kingdom,
                title = "Kingdom", group = "GBIF Observations") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |> 
      hideGroup(c("Parcels", "Protected Areas"))
    })
  
  observeEvent(
    input$name, {
      gbif_filter <- filter(gbif, verbatimScientificName == input$name)
        leafletProxy("gbif") %>%
          clearGroup("gbif_select") |> 
          addCircles(data = gbif_filter, color = "black", fill = FALSE, 
                     radius = 40, weight = 8, opacity = 1, group = "gbif_select")
        }
    )
  

# Species Tables ----------------------------------------------------------

  output$species_table <- renderDT(
    ""
    # gbif_vern[, c(1, 4, 6, 14, 51, 33)],
    # server = TRUE 
  )
}

shinyApp(ui, server)
