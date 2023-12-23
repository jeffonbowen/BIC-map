### Bowen Island DEM ###

library(sf)
library(terra)
library(tidyverse)
library(bcmaps)
library(leaflet)

# Bowen boundary
# Comes in bc albers.Lidar data is in UTM Xone 10.
munis <- bcmaps::municipalities()
bowen <- filter(munis, ADMIN_AREA_ABBREVIATION == "Bowen Island") %>% 
  st_transform(crs = 3157)
leaflet() |> 
  addTiles() |> 
  addPolygons(dat = bowen)

# Load, mosaic, and crop existing dem -------------------------------------------------------

ls <- list.files("dat_spatial/dem_download", "tif$", full.names=TRUE) # the $ excludes aux-files
ic <- sprc(lapply(ls, rast))
dem <- mosaic(ic, fun = "mean") %>% 
  crop(bowen) %>% 
  mask(bowen)  

# Get rid of the band of ocean
dem <- ifel(dem < 0, NA, dem)

writeRaster(dem, "dat_spatial/dem_bowen.tif", overwrite = TRUE)

# Make some lower res versions

dem5 <- aggregate(dem, 5, fun = "mean") |> 
  terra::project("epsg:4326")
writeRaster(dem5, "dat_spatial/dem_bowen_5m.tif", overwrite = TRUE)

dem10 <- aggregate(dem, 10, fun = "mean") |> 
  terra::project("epsg:4326")
writeRaster(dem10, "dat_spatial/dem_bowen_10m.tif", overwrite = TRUE)

pal.dem <- colorNumeric(c("darkgreen", "yellow", "darkred"), values(dem5),
                        na.color = "transparent")

pal.dem <- colorNumeric(c("white", "green", "darkgreen"), values(dem5),
                        na.color = "transparent")


plot(dem5, col = terrain.colors())

pal.dem <- terrain.colors(30)

leaflet() |> 
  addTiles() |> 
  addRasterImage(dem5, colors = terrain.colors())



# Hillshade ---------------------------------------------------------------

# Load
dem <- rast("dat_spatial/dem_bowen.tif")
dem5 <- rast("dat_spatial/dem_bowen_5m.tif")
dem10 <- rast("dat_spatial/dem_bowen_10m.tif")

# Hillshade
dem_prod <- terrain(dem, v = c("slope", "aspect"), unit = "radians")
hillshade <- shade(slope = dem_prod$slope, aspect = dem_prod$aspect)
plot(dem_hillshade, col =gray(0:30/30), legend = FALSE)
plot(dem, col=terrain.colors(100, alpha=0.35), add=TRUE)
writeRaster(dem_hillshade, "dat_spatial/hillshade_1m.tif", overwrite = TRUE)

# Hillshade 5 m
dem_prod <- terrain(dem5, v = c("slope", "aspect"), unit = "radians")
hillshade5 <- shade(slope = dem_prod$slope, aspect = dem_prod$aspect)
plot(hillshade5, col =gray(0:30/30), legend = FALSE)
plot(dem5, col=terrain.colors(100, alpha=0.35), add=TRUE)
writeRaster(dem_hillshade, "dat_spatial/hillshade_5m.tif", overwrite = TRUE)

# Hillshade 10 m
dem_prod <- terrain(dem10, v = c("slope", "aspect"), unit = "radians")
hillshade10 <- shade(slope = dem_prod$slope, aspect = dem_prod$aspect)
plot(hillshade10, col =gray(0:30/30), legend = FALSE)
plot(dem10, col=terrain.colors(100, alpha=0.35), add=TRUE)
writeRaster(dem_hillshade, "dat_spatial/hillshade_10m.tif", overwrite = TRUE)

# Export to a png 
png("out/hillshade_terrain.png")
plot(hillshade5, col =gray(0:30/30), legend = FALSE)
plot(dem5, col=terrain.colors(100, alpha=0.35), add=TRUE)
dev.off()

# Or plot in leaflet
leaflet() |> 
  addTiles() |> 
  addRasterImage(dem_hillshade, colors = grey(0:100/100)) |> 
  addRasterImage(dem10, colors = terrain.colors(50), opacity = 0.35)






