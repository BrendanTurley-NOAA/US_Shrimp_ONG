library(lubridate)
library(ncdf4)
library(sf)
library(terra)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
# crs(gom)
utm <- "+proj=utm +zone=15 units=km"
gom_utm <- project(gom, utm)
gom_utm <- gom_utm[which(gom_utm$StatZone>12), ]
gom_sf <- st_as_sf(gom_utm)
gom_sf <- st_as_sf(gom_utm)

### bathymetry
url <- 'https://www.ngdc.noaa.gov/thredds/dodsC/global/ETOPO2022/30s/30s_bed_elev_netcdf/ETOPO_2022_v1_30s_N90W180_bed.nc'
dat <- nc_open(url)
lon <- ncvar_get(dat, 'lon')
lat <- ncvar_get(dat, 'lat')
lon_ind <- which(lon>(-100) & lon<(-81))
lat_ind <- which(lat>24 & lat<30.5)

z <- ncvar_get(dat, 'z',
               start = c(lon_ind[1], lat_ind[1]),
               count = c(length(lon_ind), length(lat_ind)))
nc_close(dat)
lon <- lon[lon_ind]
lat <- lat[lat_ind]

bathy <- rast(t(z[, ncol(z):1]),
              extent = ext(min(lon),
                           max(lon),
                           min(lat),
                           max(lat)) )
### isolate shelf
bathy[values(bathy) > 0] <- NA
bathy[values(bathy) < (-100)] <- NA
### set all values to 1; then make into shapefile
bathy[!is.na(values(bathy))] <- 1
bathy_poly <- as.polygons(bathy)
crs(bathy_poly) <- '+proj=longlat +datum=WGS84'
### what's total area?
### total area calc
gom_utm2 <- gom_utm[which(gom_utm$DepZone!='Inshore'), ]
gom_stats <- aggregate(gom_utm2, by='StatZone',fun='sum')
tot_a <- bathy_poly |>
  project(crs(gom_stats)) |>
  crop(gom_stats) |>
  st_as_sf() |>
  st_area()
rm(bathy, bathy_poly, gom_utm2, gom_stats, z, lon, lat)

for(i in 2015:2019){
  setwd("C:/Users/brendan.turley/Documents/data/elb_effort")
  f_name <- paste0('effort_', i, '.RData')
  load(f_name)
  # load('effort_2015.RData')
  effort <- output_files$effort_pings
  # effort$id <- paste(effort$VSBN, effort$SERIAL, effort$trip, effort$tow, sep='-')
  rm(output_files)
  gc()

  effort <- effort[which(effort$species_grp=='PENAEID'), ]
  effort <- effort[which(effort$LONGITUDE<=(-87)), ]

  effort$tow_lth <- as.numeric(effort$tow_end_date - effort$tow_start_date)
  effort$month <- month(effort$STAMP)

  eff_sf <- st_as_sf(effort,
                     coords = c('LONGITUDE', 'LATITUDE'),
                     crs = 4326)
  eff_sf <- st_transform(eff_sf, crs = st_crs(gom_utm))
  eff_j <- st_join(eff_sf, gom_sf,
                   join = st_intersects,
                   left = T)

  eff_inx <- which(eff_j$StatZone>12 & eff_j$DepZone!='Inshore')
  eff_keep <- eff_j[eff_inx, ]
  rm(eff_inx, eff_sf, eff_j, effort)
  gc()

  ### total effort per area (could also do per vessel)
  tot_epa <- sum(eff_keep$hours_eff)/tot_a

  setwd("~/R_projects/US_Shrimp_ONG/data")
  save(eff_keep, tot_epa, tot_a, file = paste0('effort_', i, '_subset.RData'))
}


