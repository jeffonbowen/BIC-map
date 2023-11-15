### Bowen Island Mapping and Analysis ###

library(tidyverse)
library(sf)
library(tmap)
library(bcmaps)
library(bcdata)

# Bowen boundary
# Comes in bc albers.Lidar data is in UTM Xone 10.
munis <- bcmaps::municipalities()
bowen <- filter(munis, ADMIN_AREA_ABBREVIATION == "Bowen Island")

tmap_mode("view")
tm_shape(bowen) +
  tm_lines()
#  st_transform(crs = 4326) %>% as_Spatial()

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
parks <- st_read(paste0(dat_path, "/Bowen_base/BIM_data/ParksGreenSpaces/ParksGreenSpaces.shp"))

