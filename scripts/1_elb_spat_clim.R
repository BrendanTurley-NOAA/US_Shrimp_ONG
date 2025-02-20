#### seasonal ELB effort rasters

### modify code to exclude state waters and land and areas outside statzone of interest

rm(list=ls())
gc()
library(abind)
library(cmocean)
library(data.table)
library(fields)
library(lattice)
library(latticeExtra)
library(lubridate)
library(ncdf4)
library(rasterVis)
library(sf)
library(sp)
library(terra)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
gom2 <- gom
gom <- gom[which(gom$StatZone>12), ]
gom_sf <- st_as_sf(gom)
# gom_sp <- as(gom_sf, 'Spatial')
gom_agg <- aggregate(gom2, c('StatZone'))
gom_sp <- as(gom_agg, 'Spatial')
# utm <- "+proj=utm +zone=15 units=km"
# gom_utm <- project(gom, utm)
# gom_utm <- gom_utm[which(gom_utm$StatZone>12), ]
# gom_sf <- st_as_sf(gom_utm)
# gom_sp <- as(vect(gom_sf), "Spatial")


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/BOEM GOM WEA Options")
wea <- vect('BOEM_GOM_WEA_Options.shp') |>
  project(crs(gom)) |>
  as('Spatial')


setwd("~/R_projects/US_Shrimp_ONG/data")
platforms <- readRDS('gomx_platforms.rds')
plat_sf <- st_as_sf(platforms,
                    coords = c('Longitude', 'Latitude'),
                    crs = 4326)
# plat_sf <- st_transform(plat_sf, crs = st_crs(gom_sf))
plat_j <- st_join(plat_sf, gom_sf,
                  join = st_intersects,
                  left = T)
plt_keep <- subset(plat_j, plat_j$StatZone>12)


### gom shapfile for buffer subsetting
gom_fed_act <- gom[which(gom$Jurisdict=='Federal' &
                           gom$Activity=='Y'),] |>
  aggregate('StatZone') |>
  st_as_sf()

rm(gom, plat_sf, plat_j)
gc()

yrs <- 2015:2019

proj <- '+init=epsg:4326'
w_ext <- (-98)
e_ext <- (-88)
n_ext <- 31
s_ext <- 25.5
nx <- ceiling(length(w_ext:e_ext)*12) # multiply by 20 to be ~ size of BOEM lease blocks
ny <- ceiling(length(s_ext:n_ext)*12)

r <- rast(nrows = ny, ncols = nx,
          extent = ext(w_ext, e_ext, s_ext, n_ext),
          crs = proj)
cellSize(r, unit = 'km')


e_stack <- v_stack <- c(r)

n <- 1
pb <- txtProgressBar(min = 0, max = length(yrs)*12, initial = 0, style = 3)
system.time(
  for(i in 1:5){
    # i <- 1
    yr <- yrs[i]

    load(paste0('effort_',yr,'_subset.RData'))
    eff_keep <- st_transform(eff_keep, crs(r))
    # eff_keep$geometry
    eff_a <- array(NA,c(12,nx,ny))
    ves_a <- array(NA,c(12,nx,ny))

    for(j in 1:12){
      eff_j <- subset(eff_keep, month(STAMP)==j)

      ### vessels
      rves <- rasterize(st_coordinates(eff_j$geometry),
                      r,
                      value = eff_j$VSBN,
                      fun = function(x) length(unique(x)))
      rves2 <- matrix(rves$values, nx, ny)
      rves2 <- rves2[,ncol(rves2):1]

      v_stack <- c(v_stack, rves)
      ves_a[j,,] <- rves2

      rm(rves2, rves)
      gc()

      ### effort
      reff <- rasterize(st_coordinates(eff_j$geometry),
                      r,
                      value = eff_j$hours_eff,
                      fun = 'sum')
      reff2 <- matrix(reff$sum, nx, ny)
      reff2 <- reff2[,ncol(reff2):1]

      e_stack <- c(e_stack, reff) # formerly r_stack
      eff_a[j,,] <- reff2

      rm(eff_j, reff2, reff)
      gc()

      n <- n + 1
      setTxtProgressBar(pb, n)
      # imagePlot(seq(ext(r2)[1], ext(r2)[2], length.out = nx),
      #           seq(ext(r2)[3], ext(r2)[4], length.out = ny),
      #           (r3), asp=  1, las = 1,
      #           col = cmocean('dense')(60),
      #           xlab = 'Longitude (W)', ylab = 'Latitude (N)',
      #           xlim = ext(r2)[1:2], ylim = ext(r2)[3:4])
      #
      # image(r3)
    }
    assign(paste0('eff_yr_',yr), eff_a)
    assign(paste0('ves_yr_',yr), ves_a)
    rm(eff_keep, eff_a, ves_a)
    gc()
  }
)
close(pb)
# User  system elapsed
# 255.61   10.45  267.16
# user  system elapsed
# 374.74   14.06  392.08

# writeRaster(r_stack, 'elb_eff_null.tif')
# save(eff_yr_2015,
#      eff_yr_2016,
#      eff_yr_2017,
#      eff_yr_2018,
#      eff_yr_2019,
#      # r_stack,
#      # nx, ny, r,
#      file = 'eff_spatial_clim.RData')
# load('eff_spatial_clim.RData')
# r_stack <- rast('elb_eff_null.tif')

writeRaster(e_stack, 'elb_eff_rast.tif')
writeRaster(v_stack, 'elb_ves_rast.tif')
save(eff_yr_2015,
     eff_yr_2016,
     eff_yr_2017,
     eff_yr_2018,
     eff_yr_2019,
     ves_yr_2015,
     ves_yr_2016,
     ves_yr_2017,
     ves_yr_2018,
     ves_yr_2019,
     file = 'eff_ves_spatial_clim.RData')
load('eff_spatial_clim.RData')
e_stack <- rast('elb_eff_rast.tif')
v_stack <- rast('elb_ves_rast.tif')
### cannot save SpatRaster objects in .RData file
### see: https://stackoverflow.com/questions/76517502/error-external-pointer-is-not-valid-when-saving-spatrasters-from-the-r-environm


# r_mth_m <- r
# for(i in 1:12){
#   eff_mth_i <- abind(eff_yr_2015[i,,],
#                      eff_yr_2016[i,,],
#                      eff_yr_2017[i,,],
#                      eff_yr_2018[i,,],
#                      eff_yr_2019[i,,],
#                      along = 0) |>
#     apply(c(2,3), sum, na.rm = T)
#   eff_mth_i[which(eff_mth_i==0)] <- NA
#
#   # obj <- levelplot(eff_mth_i,
#   #                  row.values = seq(ext(r)[1], ext(r)[2], length.out = nx),
#   #                  column.values = seq(ext(r)[3], ext(r)[4], length.out = ny),
#   #                  col.regions = cmocean('dense')(60))
#
#   assign(paste0('eff_',i), eff_mth_i)
#   # assign(paste0('obj_',i), obj)
#
#   r_mth_m <- c(r_mth_m, sum(r_stack[[seq(i,60,12)]], na.rm = T))
# }

e_mth_m <- v_mth_m <- r
for(i in 1:12){
  eff_mth_i <- abind(eff_yr_2015[i,,],
                     eff_yr_2016[i,,],
                     eff_yr_2017[i,,],
                     eff_yr_2018[i,,],
                     eff_yr_2019[i,,],
                     along = 0) |>
    apply(c(2,3), median, na.rm = T)
  eff_mth_i[which(eff_mth_i==0)] <- NA

  ves_mth_i <- abind(ves_yr_2015[i,,],
                     ves_yr_2016[i,,],
                     ves_yr_2017[i,,],
                     ves_yr_2018[i,,],
                     ves_yr_2019[i,,],
                     along = 0) |>
    apply(c(2,3), median, na.rm = T)
  ves_mth_i[which(ves_mth_i==0)] <- NA

  # obj <- levelplot(ves_mth_i,
  #                  row.values = seq(ext(r)[1], ext(r)[2], length.out = nx),
  #                  column.values = seq(ext(r)[3], ext(r)[4], length.out = ny),
  #                  col.regions = cmocean('dense')(60))

  assign(paste0('eff_',i), eff_mth_i)
  assign(paste0('ves_',i), ves_mth_i)
  # assign(paste0('obj_',i), obj)

  e_mth_m <- c(e_mth_m, median(e_stack[[seq(i,60,12)]], na.rm = T))
  v_mth_m <- c(v_mth_m, median(v_stack[[seq(i,60,12)]], na.rm = T))
}

brks <- quantile(cbind(eff_yr_2015,eff_yr_2016,eff_yr_2017,eff_yr_2018,eff_yr_2019),
         seq(0,1,.05),na.rm=T)
brks2 <- quantile(cbind(ves_yr_2015,ves_yr_2016,ves_yr_2017,ves_yr_2018,ves_yr_2019),
                 seq(0,1,.05),na.rm=T)

names(e_mth_m) <- names(v_mth_m) <- month.name[1:12]

# plot(e_mth_m)
# plot(v_mth_m)

myTheme <- rasterTheme(region = cmocean('dense')(10))

# png('elb_spatial_clim.png', width = 8, height = 7, units = 'in', res = 300)
# rasterVis::levelplot(e_mth_m, par.settings = myTheme, zscaleLog = F)
# rasterVis::levelplot(v_mth_m, par.settings = myTheme, zscaleLog = F)
# dev.off()

# comb_levObj <- c(obj_1, obj_2, obj_3, obj_4, obj_5, obj_6,
#                  obj_7, obj_8, obj_9, obj_10, obj_11, obj_12,
#                  layout = c(3,4), merge.legends = FALSE)
# print(comb_levObj)


### effort
eff_all <- abind(eff_1,eff_2,eff_3,eff_4,eff_5,eff_6,
      eff_7,eff_8,eff_9,eff_10,eff_11,eff_12,
      along = 3) |>
  aperm(c(2,1,3))

eff_all <- eff_all[nrow(eff_all):1,,]|>
rast(extent = ext(w_ext, e_ext, s_ext, n_ext),
       crs = proj)

cellSize(eff_all, unit = 'km')

# plot(eff_all)
plot(median(eff_all,na.rm=T))
plot(median(e_mth_m,na.rm=T))
levelplot(median(e_mth_m,na.rm=T)) + layer(sp.lines(gom_sp, lwd=0.5, col='gray20')) +
  layer(sp.lines(wea, lwd=0.5, col='skyblue'))

quantile(values(eff_all), seq(0,1,.1), na.rm = T)
hist(values(eff_all))
values(eff_all)[which(values(eff_all)>200)] <- 200

names(eff_all) <- month.name[1:12]

myTheme <- rasterTheme(region = cmocean('dense')(10))
# myTheme <- rasterTheme(region = rocket(10))
my.at <- seq(0,200,10)
p <- rasterVis::levelplot(eff_all, at=my.at, par.settings = myTheme)

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('eff_spatial_clim.png', width = 8, height = 7, units = 'in', res = 300)
# rasterVis::levelplot(eff_all, at=my.at, par.settings = myTheme)
p + layer(sp.lines(gom_sp, lwd=0.5, col='gray20')) +
  layer(sp.lines(wea, lwd=0.5, col='magenta'))
dev.off()


### vessels
ves_all <- abind(ves_1,ves_2,ves_3,ves_4,ves_5,ves_6,
                 ves_7,ves_8,ves_9,ves_10,ves_11,ves_12,
                 along = 3) |>
  aperm(c(2,1,3))

ves_all <- ves_all[nrow(ves_all):1,,]|>
  rast(extent = ext(w_ext, e_ext, s_ext, n_ext),
       crs = proj)

cellSize(ves_all, unit = 'km')

# plot(ves_all)
plot(median(ves_all,na.rm=T))
plot(median(v_mth_m,na.rm=T))
levelplot(median(v_mth_m,na.rm=T)) + layer(sp.lines(gom_sp, lwd=0.5, col='gray20')) +
  layer(sp.lines(wea, lwd=0.5, col='skyblue'))

quantile(values(ves_all), seq(0,1,.1), na.rm = T)
hist(values(ves_all))
values(ves_all)[which(values(ves_all)>40)] <- 40
# values(ves_all)[which(values(ves_all)<2)] <- 2

names(ves_all) <- month.name[1:12]

myTheme <- rasterTheme(region = cmocean('speed')(10))
# myTheme <- rasterTheme(region = rocket(10))
# my.at <- pretty(values(ves_all), n = 20)
my.at <- seq(0,40,2)
p <- rasterVis::levelplot(ves_all, at=my.at, par.settings = myTheme)

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('ves_spatial_clim.png', width = 8, height = 7, units = 'in', res = 300)
# rasterVis::levelplot(ves_all, at=my.at, par.settings = myTheme)
p + layer(sp.lines(gom_sp, lwd=0.5, col='gray20')) +
  layer(sp.lines(wea, lwd=0.5, col='magenta'))
dev.off()
