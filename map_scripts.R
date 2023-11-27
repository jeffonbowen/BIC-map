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


# BIM Data ----------------------------------------------------------------

dat_path <- "C:/Users/jeff.matheson/OneDrive/Documents/Spatial data library"

list.files(dat_path)

list.files(dat_path, pattern = "*shp", full.names = TRUE, recursive = TRUE,
           include.dirs = FALSE)


dat_path |>
  dir_ls(recurse = TRUE, regexp = 'shp$') 

parks <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/ParksGreenSpaces/ParksGreenSpaces.shp"))


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





# Density

library(adehabitatHR)

gbif_sp <- gbif_dat |> sf::st_as_sf(coords = c("long", "lat"),
                    crs = 4326) |>
  mutate(id = 1) |> 
  as("Spatial")

dat_kud <- kernelUD(gbif_sp, xy = data$coords, grid = 10, same4all = TRUE)







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

  
# Inaturalist
  
library(rinat)

  
  
  
  
  
  

