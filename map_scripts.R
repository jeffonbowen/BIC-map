### Bowen Island Mapping and Analysis ###

library(tidyverse)
library(sf)
library(tmap)
library(bcmaps)


# Bowen boundary
# Comes in bc albers.Lidar data is in UTM Xone 10.
munis <- bcmaps::municipalities()
bowen <- filter(munis, ADMIN_AREA_ABBREVIATION == "Bowen Island")

tmap_mode("view")
tm_shape(bowen) +
  tm_polygons(fill.alpha = 1)
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
