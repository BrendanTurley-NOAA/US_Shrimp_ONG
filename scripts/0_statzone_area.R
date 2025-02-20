rm(list=ls())
gc()
library(cmocean)
library(data.table)
library(fields)
library(lubridate)
library(ncdf4)
library(sf)
library(terra)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
utm <- "+proj=utm +zone=15 units=km"
# gom_utm <- project(gom, utm)
# gom_utm <- gom_utm[which(gom_utm$StatZone>12), ]
# gom_sf <- st_as_sf(gom_utm)

### gom shapfile for buffer subsetting
gom_fed_act <- gom[which(gom$Jurisdict=='Federal' &
                           gom$Activity=='Y' &
                           gom$StatZone>12),] |>
  aggregate('StatZone') |>
  project(utm) |>
  st_as_sf()


setwd("C:/Users/brendan.turley/Documents/data/bathy")
bathy <- rast('etopo1.nc')

### isolate shelf
bathy[values(bathy$Band1) > 0] <- NA
bathy[values(bathy$Band1) < (-150)] <- NA

plot(bathy,
     main = 'Continential Shelf cutout')

### set all values to 1; then make into shapefile
bathy[!is.na(values(bathy$Band1))] <- 1
bathy2 <- as.polygons(bathy)

bathy_c <- crop(bathy2,
                ext(gom)) |>
  st_as_sf() |>
  st_transform(st_crs(gom_fed_act)) |>
  st_simplify(dTolerance = 0)

shrimpable <- st_intersection(bathy_c, gom_fed_act)

plot(st_geometry(shrimpable))

shrimp_area_sz <- st_area(shrimpable)
activity_layer_sz <- st_area(gom_fed_act)

setwd("~/R_projects/US_Shrimp_ONG/data")
save(shrimp_area_sz, activity_layer_sz, file='statzone_areas.RData')
load('statzone_areas.RData')

plot(st_geometry(gom_fed_act))
plot(st_geometry(shrimpable),add=T)

plot(c(st_area(shrimpable),st_area(gom_fed_act)),rep(13:21,2))
segments(st_area(shrimpable),13:21,st_area(gom_fed_act),13:21)
