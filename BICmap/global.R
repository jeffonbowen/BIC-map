### Bowen Island Conservancy Web Mapping Application - BICmap ###

{
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
}

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

changelog <- read.csv("changelog.txt", header = FALSE, sep  = "&")


## Load overlay group layers

# For dev, set this working directory
# setwd("C:/Users/jeff.matheson/OneDrive - Tetra Tech, Inc/Documents/R_Projects/BowenMapping/BICmap")

bowen <- st_read("dat/bowen_muni_bdry.gpkg") |> 
  st_transform(crs = 4326)
parcels <- st_read("dat/parcels_bowen.gpkg") |>
  st_transform(crs = 4326)
parks <- st_read("dat/ParksGreenSpaces.shp") |>
  st_transform(crs = 4326) |> 
  filter(!is.na(purpose)) 
chm <- terra::rast("dat/chm10.tif")|>
  terra::project("epsg:4326")
hillshade <- rast("dat/hillshade10.tif")
sei <- st_read("dat/SEI_MV_Bowen.shp") |> 
  st_set_crs(26910) |> 
  st_transform(crs = 4326) |> 
  filter(substr(secl_1,1,1) != "x")
lakes <- st_read("dat/Lakes.shp") |> 
  st_transform(crs = 4326)
ponds <- st_read("dat/Ponds.shp") |> 
  st_transform(crs = 4326)

# Can't get streams to draw properly
# streams <- st_read("dat/Streams.shp") |> 
#   st_transform(crs = 4326)

wetlands <- st_read("dat/Wetlands.shp") |> 
  st_transform(crs = 4326)

# streams_ajw <- st_read("dat/streams_fish.shp") |>
#   st_transform(crs = 4326)
streams_ajw <- st_read("dat/streams_ajw.gpkg")

# wetlands_ajw <- st_read("dat/wetlands2015.shp") |>
#   st_set_crs(26910) |>
#   st_transform(crs = 4326)
wetlands_ajw <- st_read("dat/wetlands_ajw.gpkg")

coven <- st_read("dat/coven.gpkg")

inat <- read_csv(file = "dat/observations-380595.csv") |> 
  arrange(taxon_id) |> 
  mutate(taxon_kingdom_name = factor(taxon_kingdom_name),
         common_name = factor(common_name)) |> 
  filter(quality_grade == "research")

# Create sp list for table
sp_list <- inat |> 
  select(Kingdom = taxon_kingdom_name, Class = taxon_class_name, 
         "Scientific Name" = scientific_name, 
         "Common Name" = common_name) |> 
  group_by(Kingdom, Class, `Scientific Name`, `Common Name`) |> 
  summarize(Records = n()) |> 
  ungroup()


# Load other data
sei_summary <- read_csv("dat/sei_summary.csv")

# Prep the SEI data
sei_lgnd <- tibble(
  complgnd = c("Alpine", "Estuarine", "Freshwater", "Herbaceous", "Intertidal", 
               "Mature Forest (ME)", "Mature Forest (SE)", "Old Field", 
               "Old Forest", "Riparian", "Sparsely Vegetated", "Woodland", 
               "Wetland", "Non SE/ME", "Young Forest", "Yound Forest (small)",
               "River (Riparian subclass)", "Freshwater Reservoirs"),
  hex_colour = c("#CE5888", "#59261C", "#2E4064", "#FFA500", "#B35241", 
                 "#A2AB74", "#508624", "#97753E", "#304411", "#E35541",
                 "#512960", "#246954", "#87102E", "#E8DBBA", "#B7E057",
                 "#FBD2BD", "#6B81A6", "#6DB4D9")) |> 
  filter(!(complgnd %in% c("Alpine", "River (Riparian subclass)", 
                           "Freshwater Reservoirs")))
sei <- sei |> 
  mutate(comp1lgnd = factor(comp1lgnd, levels = sei_lgnd$complgnd),
         comp2lgnd = factor(comp2lgnd, levels = sei_lgnd$complgnd),
         comp3lgnd = factor(comp3lgnd, levels = sei_lgnd$complgnd)
  )




# Base Map ----------------------------------------------------------------

basegroups <- c("OSM (default)", "Open Topo", "ESRI Imagery", "ESRI GrayCanvas")

overlays.base <- c("Parcels", "Parks and Green Spaces", "Hillshade",
                   "Canopy Height")

pal.chm <- colorNumeric(c("white", "green", "darkgreen"), values(chm),
                        na.color = "transparent")
  
base <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addTiles(group = "OSM (default)")  |>
  addProviderTiles(providers$OpenTopoMap, group = "Open Topo") |>    
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") |>
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "ESRI GrayCanvas") |>
    addRasterImage(hillshade, colors = grey(0:100/100), 
                 group = "Hillshade") |>
  addRasterImage(chm, colors = pal.chm, opacity = 1.0,
                 group = "Canopy Height") |>
  addPolygons(data = bowen, color = "green", fill = FALSE) |> 
  addPolygons(data = parcels, color = "grey", fill  = FALSE, weight = 1,
              group = "Parcels") |> 
  addPolygons(data = parks, color = "red", fill  = TRUE, fillOpacity = 0, 
              weight = 2,
              group = "Parks and Green Spaces",
              popup = paste(parks$parkname, "<br>", parks$purpose)) |> 
  addLayersControl(
    baseGroups = basegroups,
    overlayGroups = overlays.base,
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft") |>
  addMeasure(primaryLengthUnit = "metres",
             primaryAreaUnit = "hectares") |> 
  hideGroup(c("Hillshade", "Canopy Height"))

# Main Map ----------------------------------------------------------------

overlays.main <- c(overlays.base, "Covenants (to Dec 8)","SEI", "Lakes", 
                   "Ponds", "Wetlands (BIM)", "Streams (BIM)","Wetlands (AJW)", 
                   "Streams (AJW)")

pal.sei <- colorFactor(sei_lgnd$hex_colour, sei_lgnd$complgnd)

pop.sei <- paste("1: ", sei$comp1lgnd_, "<br>", 
                 "2: ", sei$comp2lgnd_, "<br>", 
                 "3: ", sei$comp3lgnd_)

main <- base |> 
  addPolygons(dat = coven, stroke = TRUE, 
              color = "red4", fillOpacity = 0,opacity = 1,
              weight = 2,
              popup = coven$Keywords,
              group = "Covenants (to Dec 8)") |> 
  addPolygons(data = sei, 
              group = "SEI",
              color = ~pal.sei(sei$comp1lgnd), 
              fillOpacity = 1.0, stroke = FALSE,
              popup = pop.sei) |> 
  addPolygons(dat = lakes, color = "blue", stroke = FALSE, 
              fillOpacity = 1, group = "Lakes",
              popup = lakes$name) |> 
  addPolygons(dat = ponds, color = "royalblue", stroke = FALSE, 
              fillOpacity = 1, group = "Ponds") |> 
  addPolygons(dat = wetlands, color = "green", stroke = FALSE,
              fillOpacity = 1, group = "Wetlands (BIM)",
              popup = wetlands$feat_name) |>
  addPolygons(dat = wetlands_ajw, color = "greenyellow", stroke = FALSE,
              fillOpacity = 1, group = "Wetlands (AJW)") |>
#  addPolylines(dat = streams, weight = 2, group = "Streams (BIM)") |>
  addPolylines(dat = streams_ajw, color = "blue", weight = 2,
               group  = "Streams (AJW)", popup = streams_ajw$CREEK_NAME) |>
  addLayersControl(
    baseGroups = basegroups,
    overlayGroups = overlays.main,
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft") |>
  addLegend(pal = pal.sei, values = sei$comp1lgnd,
            title = "Dominant Eco", group = "SEI", opacity = 1)|> 
  hideGroup(overlays.main)
