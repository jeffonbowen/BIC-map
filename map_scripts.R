### Bowen Island Mapping and Analysis ###
{
library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(leaflet)
library(bcmaps)
library(bcdata)
library(terra)
library(rgbif)
library(DT)
library(janitor)
library(data.table)
library(SpatialKDE)
}

# Bowen boundary
# Comes in bc albers. Lidar data is in UTM zone 10.
# munis <- bcmaps::municipalities()
# bowen <- filter(munis, ADMIN_AREA_ABBREVIATION == "Bowen Island")


# SEI ---------------------------------------------------------------------

dat_path <- "C:/Users/jeff.matheson/OneDrive/Documents/Spatial data library"
SEI <- st_read(paste0(dat_path, "/Ecosystems/MV/SEI_MV_Bowen.shp"))
st_crs(SEI) <- 26910

tm_shape(SEI) +
  tm_polygons(fill = "comp1lgnd")

names(SEI)
table(SEI$sedec_1)

# Create a summary of all deciles

summary(SEI)

sum(SEI$utmstarea)/10000
sum(SEI$wsize_se1)/10000

SEI <- SEI |> 
  filter(substr(secl_1,1,1) != "x") 
d1 <- SEI |> 
  st_drop_geometry() |> 
  group_by(comp1lgnd) |> 
  summarize(Primary = sum(sedec_1 / 10 * st_area_sh / 10000)) |> 
  rename(`SEI Class` = comp1lgnd)
d2 <- SEI |> 
  st_drop_geometry() |> 
  group_by(comp2lgnd) |> 
  summarize(Secondary = sum(sedec_2 / 10 * st_area_sh / 10000))|> 
  rename(`SEI Class` = comp2lgnd)
d3 <- SEI |> 
  st_drop_geometry() |> 
  group_by(comp3lgnd) |> 
  summarize(Tertiary = sum(sedec_3 / 10 * st_area_sh / 10000)) |> 
  rename(`SEI Class` = comp3lgnd)
sei_summary <- d1 |> 
  full_join(d2, by = "SEI Class") |> 
  full_join(d3, by = "SEI Class") |> 
  rowwise() |> 
  mutate(`Total Area` = sum(c_across(2:4), na.rm = TRUE),
         `% of Bowen` = `Total Area` / 5071.2 * 100) |> 
  ungroup() |> 
  pivot_longer(!`SEI Class`) |> 
  pivot_wider(names_from = `SEI Class`, values_from = value) |> 
  rename(Dominance = name)

write_csv(sei_summary, "dat/sei_summary.csv")



# TEM data from IT --------------------------------------------------------
dat_path <- "C:/Users/jeff.matheson/OneDrive/Documents/Spatial data library"
ITEM <- st_read(paste0(dat_path, "/Ecosystems/ITEM/Bowen_ITEM.shp"))
st_crs(ITEM) <- 4326
ITEM <- st_transform(ITEM, CRS = 26910)
tm_shape(ITEM) +
  tm_polygons()


# Get Parcel Data ---------------------------------------------------------

bcdata::bcdc_browse("4cf233c2-f020-4f7a-9b87-1923252fbc24")
bcdc_search("4cf233c2-f020-4f7a-9b87-1923252fbc24")

# Takes awhile.
parcel_bc <- bcdc_get_data("4cf233c2-f020-4f7a-9b87-1923252fbc24")

parcels <- st_crop(parcel_bc, bowen)
plot(parcels)
parcels <-st_transform(parcels, crs = 26910)
# Tmap does not like the following field:
parcels <- select(parcels, !SE_ANNO_CAD_DATA)
st_write(parcels, paste0(dat_path, "/Bowen_base/Parcels/parcels_bowen.gpkg"))

# Read saved parcel data
parcels <- st_read(paste0(dat_path, "/Bowen_base/Parcels/parcels_bowen.gpkg"))

tmap_mode("view")
tm_shape(parcels_park) +
  tm_polygons() 

st_write(parcels_park, paste0(dat_path, "/Bowen_base/Parcels/parcels_bowen_park.gpkg"))

## Combined

tm_shape(SEI) +
  tm_polygons(fill = "comp1lgnd") +
  tm_shape(parcels) +
  tm_lines()



# Hillshade ---------------------------------------------------------------

dtm <- rast("dat_spatial/bowen_dtm.tif")

dtm10 <- aggregate(dtm, 10, fun = "mean") |> 
  terra::project("epsg:4326")
writeRaster(dtm10, "dat_spatial/dtm10.tif", overwrite = TRUE)

leaflet() |> 
  addTiles() |> 
  addRasterImage(dtm10, colors = pal.dtm)

pal.dtm <- colorNumeric(c("darkgreen", "orange"), values(dtm10),
                        na.color = "transparent")

pal.dtm <- colorNumeric(c("white", "green", "darkgreen"), values(dtm10),
                        na.color = "transparent")

# hillshade
dtm_prod <- terrain(dtm10, v = c("slope", "aspect"), unit = "radians")
dtm_hillshade <- shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
plot(dtm_hillshade, col =gray(0:30/30), legend = FALSE)
writeRaster(dtm_hillshade, "dat_spatial/hillshade10.tif", overwrite = TRUE)

leaflet() |> 
  addTiles() |> 
  addRasterImage(dtm_hillshade, colors = grey(0:100/100))


# BIM Data ----------------------------------------------------------------

dat_path <- "C:/Users/jeff.matheson/OneDrive/Documents/Spatial data library"

list.files(dat_path)

list.files(dat_path, pattern = "*shp", full.names = TRUE, recursive = TRUE,
           include.dirs = FALSE)


dat_path |>
  dir_ls(recurse = TRUE, regexp = 'shp$') 

parks <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/ParksGreenSpaces/ParksGreenSpaces.shp"))
lakes <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/DPAs/Lakes.shp")) |> 
  st_transform(crs = 4326)
ponds <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/DPAs/Ponds.shp")) |> 
  st_transform(crs = 4326)
streams <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/DPAs/Streams.shp")) |> 
  st_transform(crs = 4326)
watersheds <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/DPAs/Watersheds.shp")) |> 
  st_transform(crs = 4326)
wetlands <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/DPAs/Wetlands.shp")) |> 
  st_transform(crs = 4326)
eelgrass <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/Eel Grass/MarineEelGrass.shp")) |> 
  st_transform(crs = 4326)
foragefish <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/Forage Fish/ForageFish.shp")) |> 
  st_transform(crs = 4326)
herons <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/Wildlife/HeronNest.shp")) |> 
  st_transform(crs = 4326)
raptors <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/Wildlife/RaptorNest.shp")) |> 
  st_transform(crs = 4326)
ravens <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/Wildlife/RavenNest.shp")) |> 
  st_transform(crs = 4326)

leaflet() |> 
  addTiles() |> 
  # addPolygons(dat = lakes, color = "blue", stroke = FALSE) |> 
  addPolygons(dat = ponds, color = "red", stroke = FALSE) |> 
  # addPolygons(dat = wetlands, color = "green", stroke = TRUE) |> 
  # addPolygons(dat = wetlands_ajw, color = "yellow", stroke = TRUE) |> 
#  addPolylines(dat = streams, weight = 2) |> 
  addPolylines(dat = streams_ajw, color = "darkgreen", weight = 2)
  # addPolygons(dat = eelgrass, color = "red") |> 
  # addPolylines(dat = foragefish, color = "orange")
  
c("Lakes", "Ponds", "Wetlands", "Streams", "Wetlands (AJW)", "Streams (AJW)")

plot(lakes)



# Covenants ---------------------------------------------------------------

c1 <- st_read("../dat_spatial/bowen-covenants-1.gpkg") |> 
  st_transform(4326) |> select(!id) |> 
  rename("Covenant" = "Coven..")
c2 <- st_read("../dat_spatial/bowen-covenants-2.gpkg") |> 
  st_transform(4326) |> 
  rename("Covenant" = "covenant..",
         "Keywords" = "keywords",
         "Filename" = "filename")

leaflet() |> 
  addTiles() |> 
  addPolygons(dat = c1, stroke = FALSE, 
              color = "yellow", fillOpacity = 1) |> 
  addPolygons(dat = c2, stroke = FALSE, 
              color = "orange", fillOpacity = 1)

coven <- bind_rows(c1, c2)
leaflet() |> 
  addTiles() |> 
  addPolygons(dat = coven, stroke = FALSE, 
              color = "orange", fillOpacity = 1,
              popup = coven$Keywords)
  

st_write(coven, "dat/coven.gpkg")

# Canopy Height Model -----------------------------------------------------

chm <- rast("dat_spatial/chm.tif")
chm5 <- rast("dat_spatial/chm5.tif") |> 
  terra::project("epsg:4326")
chm10 <- aggregate(chm, 10, fun = "max") |> 
  terra::project("epsg:4326")
writeRaster(chm10, "dat_spatial/chm10.tif", overwrite = TRUE)

chm10 <- rast("dat_spatial/chm10.tif")

pal <- colorNumeric(c("white", "green", "darkgreen"), values(chm10),
                    na.color = "transparent")

leaflet() |>
  addTiles() |> 
  addRasterImage(chm10, colors = pal, opacity = 0.8) |> 
  addLegend(pal = pal, values = values(chm10),
            title = "Veg Ht (Max)")
  


# GBIF --------------------------------------------------------------------

# View credentials
usethis::edit_r_environ()

gbif_download <- 
  occ_download(pred("gadm", "CAN.2.14.4_1"),
               pred("hasGeospatialIssue", FALSE),
               pred("hasCoordinate", TRUE),
               pred("occurrenceStatus","PRESENT"), 
               pred_not(pred_in("basisOfRecord",
                                c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
               format = "SIMPLE_CSV"
               )

#gbif_download
occ_download_wait(gbif_download)

gbif_dat <- occ_download_get(gbif_download, overwrite = TRUE) %>% 
  occ_download_import()

res <- occ_download_meta(gbif_download)
gbif_citation(res)

write_csv(gbif_dat, "dat/gbif_dat.csv")

# Visualization

gbif_dat |> 
  ggplot(aes(x=reorder(kingdom, kingdom, function(x)-length(x)))) +
  geom_bar() +
#  scale_x_discrete(limits=rev) +
  scale_y_continuous(limits= c(0, 50000)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  xlab("Kingdom") +
  theme_bw()

gbif_dat |> 
  filter(kingdom == "Animalia") |> 
  ggplot(aes(y = reorder(class, class, function(x)-length(x)))) +
  geom_bar(show.legend = FALSE) + 
  theme_bw() +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(limits= c(0, 45000)) +
  ylab("Class in Animalia")
  geom_text(aes(label = ..count..), stat = "count", hjust = -0.2)
  
gbif_dat |> 
#  mutate(year = factor(year)) |> 
  group_by(year) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = year, y = count)) +
  scale_x_continuous(breaks = scales::breaks_width(10)) +
  geom_point() +
  theme_bw()

gbif_dat |> 
  ggplot(aes(y = collectionCode, fill = collectionCode)) +
  geom_bar(stat = "count", show.legend = FALSE) +
  theme_bw() +
  scale_y_discrete(limits=rev)

gbif_dat |> 
  group_by(species) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = year, y = count)) +
  scale_x_continuous(breaks = scales::breaks_width(10)) +
  geom_point() +
  theme_bw()


    

# Locations
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
  addCircles(data = gbif_dat, ~decimalLongitude, ~decimalLatitude, radius = 5,
             popup = ~species) |> 
addLayersControl(
    baseGroups = c("OSM (default)", "Open Topo", "ESRI Imagery"),
    overlayGroups = overlay.gbif,
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft") |>
  # addLegend(pal = pal.chm, values = values(chm),
  #           title = "Canopy Ht (m)", group = "Canopy Height") |> 
  addMeasure(primaryLengthUnit = "metres",
             primaryAreaUnit = "hectares")


# Common names from GBIF backbone

vern <- read_tsv("dat/backbone/VernacularName.tsv") |> 
  filter(language == "en")

gbif_vern <- gbif_dat |> 
  left_join(vern, join_by(taxonKey == taxonID))

noname <- gbif_vern |> 
  filter(is.na(vernacularName))
gbif_vern |> 
  group_by(occurrenceID) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = count)) +
    geom_bar(stat = "count")

# Many common names for each record. Pick first, for now,
gbif_vern <- gbif_vern |> 
  group_by(gbifID) |> 
  slice_head(n= 1)

write_csv(gbif_vern, "dat/gbif_vern.csv")
write_csv(gbif_vern, "BICmap/dat/gbif_vern.csv")
saveRDS(gbif_vern, file = "BICmap/dat/gbif_vern.rds")

gbif_vern <- readRDS(file = "BICmap/dat/gbif_vern.rds")

gbif_test <- dplyr::slice_head(gbif_vern, n = 500)

gbif_test <- gbif_vern[1:500, ]
saveRDS(gbif_test, file = "BICmap/dat/gbif_test.rds")


# Select priority columns for BICmap

# Serve it up

  
datatable(gbif_vern[, c(1, 4, 6, 14, 51, 33)])
  

# Join with BCSEE

bcsee <- read_excel("dat/summaryExport.xlsx", col_types = "text")

# High-level look at what has been downloaded
table(bcsee$Kingdom, bcsee$`Classification Level`)

# So many columns. Keep the columns that are most likely to be used. 
# Also add a new column with just genus-species binomial for linking to GBIF
# Remove the Local Terrestrial Community so that its species only.

bcsee <- bcsee %>% 
  select("Element Code", "Scientific Name", "English Name", "Classification Level",
         "Species Code", "Class (English)", "Kingdom", "Family", "Global Status", 
         "Prov Status", "BC List", "COSEWIC", "SARA Schedule", "SARA Status", 
         "Migratory Bird Convention Act"
  ) %>% 
  mutate(species = word(`Scientific Name`, 1, 2)) %>% 
  select("Element Code", "Scientific Name", species, everything()) %>% 
  filter(`Classification Level` != "Local Terrestrial Community")

gbif_common <- gbif_dat |> 
  left_join(bcsee, by = "species") |> 
  dplyr::select(kingdom, species, Kingdom, `Class (English)`, `English Name`, lat, long, eventDate)

write_csv(gbif_common, "dat/gbif_common.csv")






x <-   name_lookup(name='Helianthus annuus')
  name_backbone(name='Helianthus', rank='genus', kingdom='plants')
  name_backbone(name='Poa', rank='genus', family='Poaceae')
  
  # Verbose - gives back alternatives
  ## Strictness
name_backbone_verbose(name='Poa', kingdom='plants',
                        strict=FALSE)
name_backbone_verbose(name='Helianthus annuus', kingdom='plants',
                        strict=TRUE)
  
  # Non-existent name - returns list of length 3 stating no match
name_backbone(name='Aso')
name_backbone(name='Oenante')
  
  # Pass on curl options
name_backbone(name='Oenante', curlopts = list(verbose=TRUE))

  
# Inaturalist -------------------------------------------------------------
  
library(rinat)

# Bowen bounds
-123.437
-123.304
  
inat <- read_csv("../inat/observations-380595.csv")

names(inat)

# Kernal density

inat_sf <- inat |> 
  st_as_sf(coords = c("longitude", "latitude"), dim = "XY") |> 
  st_set_crs(4326) |> 
  st_transform(26910)
  

cell_size <- 100
band_width <- 1
  
raster_inat <- inat_sf %>%
  create_raster(cell_size = cell_size, side_offset = band_width)  

kde <- inat_sf %>%
  kde(band_width = band_width, kernel = "quartic", grid = raster_inat)  

plot(kde)

pal = colorNumeric(c("yellow", "orange", "red"), kde@data@values,
                   na.color = "transparent")

leaflet() |> 
  addTiles() |> 
  addCircles(dat = inat, lng = ~longitude, lat = ~latitude,
             radius = 1) |> 
  addRasterImage(x = kde, colors = ~pal)

library(tmap)
tm_shape(kde) +
  tm_raster(palette = "viridis", title = "KDE Estimate") +
  tm_layout(legend.outside = TRUE)
