### modify code to exclude state waters and land and areas outside statzone of interest

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
gom_utm <- project(gom, utm)
gom_utm <- gom_utm[which(gom_utm$StatZone>12), ]
gom_sf <- st_as_sf(gom_utm)

setwd("~/R_projects/US_Shrimp_ONG/data")
platforms <- readRDS('gomx_platforms.rds')
plat_sf <- st_as_sf(platforms,
                    coords = c('Longitude', 'Latitude'),
                    crs = 4326)
plat_sf <- st_transform(plat_sf, crs = st_crs(gom_sf))
plat_j <- st_join(plat_sf, gom_sf,
                  join = st_intersects,
                  left = T)
plt_keep <- subset(plat_j, StatZone>12 & water_depth_m<150)
rm(plat_sf, plat_j)
gc()

### linked structures
# which(platforms$Maj.Struc.Flag=='Y')
com_id <- sort(unique(platforms$Complex.Id.Num))
keep <- setNames(data.frame(matrix(NA,
                                   length(com_id),
                                   3)),
                 c('ind', 'lon', 'lat'))
ind_rm <- rep(NA, length(com_id))
m <- 1
n <- 0
for(i in 1:length(com_id)){
  # i=com_id[100]
  ind <- which(platforms$Complex.Id.Num==com_id[i])
  if(length(ind)>1){
    tmp <- platforms[ind, ]

    n <- n + length(ind) - 1
    keep[i,] <- cbind(ind[1], mean(tmp$Longitude), mean(tmp$Latitude))
    ind_rm[m:n] <- ind[-1]
    m <- n + 1
  }
}
keep <- keep[which(!is.na(keep$ind)),]
ind_rm <- ind_rm[1:(m-1)]
platforms2 <- platforms
platforms[keep$ind,c('Longitude', 'Latitude')] <- keep[,2:3]
platforms <- platforms[-ind_rm, ]

plat_sf <- st_as_sf(platforms,
                    coords = c('Longitude', 'Latitude'),
                    crs = 4326)
plat_sf <- st_transform(plat_sf, crs = st_crs(gom_sf))
plat_j <- st_join(plat_sf, gom_sf,
                  join = st_intersects,
                  left = T)
plt_keep_notlinked <- subset(plat_j, StatZone>12 & water_depth_m<200)
rm(plat_sf, gom_sf, plat_j)
gc()


removals <- aggregate(Block.Number~year(Removal.Date), length, data = plt_keep) |>
  setNames(c('year','removals'))
sum(removals$removals)
barplot(removals$removals, names = removals$year)

installs <- aggregate(Block.Number ~ year(Install.Date), length, data = plt_keep) |>
  setNames(c('year','installs')) |>
  merge(data.frame(year=1948:2023), by = 'year', all = T)
installs$installs[which(is.na(installs$installs))] <- 0
sum(installs$installs, na.rm = T)
barplot(installs$installs, names = installs$year)

plot(installs$year,cumsum(installs$installs))

### total rigs end of 2019
subset(plt_keep, Removal.Date>='2019-12-31' | is.na(Removal.Date)) |>
  subset(Install.Date<='2019-12-31') |>
  nrow()

### total number removed
subset(plt_keep, Removal.Date>='2015-01-01' & Removal.Date<='2019-12-31') |>
  nrow()

subset(plt_keep, !is.na(Removal.Date)) |>
  nrow()
subset(plt_keep, is.na(Removal.Date)) |>
  nrow()

plt_rm <- subset(plt_keep, Removal.Date>='2015-01-01' & Removal.Date<='2019-12-31')

plot(st_coordinates(plt_keep), asp = 1)
points(st_coordinates(plt_rm),col=2)
plot(gom_utm, add=T)

removals <- aggregate(Block.Number ~ StatZone + year(Removal.Date), length, data = plt_rm) |>
  setNames(c('Statzone','year','removals')) |>
  merge(expand.grid(Statzone=13:21,year=2015:2019),by=c('Statzone','year'),all=T)

image(13:21,2015:2019,matrix(removals$removals,9,5))


###
plt_sz <- aggregate(Area.Code ~ StatZone, data = plt_keep, length) |>
  setNames(c('StatZone','number'))
median(plt_sz$number)

### distance

rig_d <- st_distance(plt_keep_notlinked)
diag(rig_d) <- NA
quantile(rig_d,na.rm=T)
mean(rig_d,na.rm=T)
median(rig_d,na.rm=T)
median(apply(rig_d, 1, min, na.rm = T))
median(apply(rig_d, 2, min, na.rm = T))
