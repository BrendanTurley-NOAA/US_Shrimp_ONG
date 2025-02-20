
rm(list=ls())
gc()
library(data.table)
library(fields)
library(lubridate)
library(ncdf4)
library(sf)
library(terra)
library(vioplot)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
utm <- "+proj=utm +zone=15 units=km"
gom_utm <- project(gom, utm)
gom_utm <- gom_utm[which(gom_utm$StatZone>12), ]
gom_sf <- st_as_sf(gom_utm)

gom_fy <- gom_utm[which(gom_utm$StatZone>12 &
                          gom_utm$Jurisdict=='Federal' &
                          gom_utm$Activity=='Y'), ] |>
  aggregate('StatZone')
# sz_a <- cbind(13:21, st_area(st_as_sf((gom_fy))))

setwd("~/R_projects/US_Shrimp_ONG/data")
load('statzone_areas.RData')
sz_a <- cbind(13:21, shrimp_area_sz)


setwd("~/R_projects/US_Shrimp_ONG/data")
platforms <- readRDS('gomx_platforms.rds')
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
plt_keep <- subset(plat_j, plat_j$StatZone>12)
# plt_keep <- subset(plt_keep, water_depth_m<200)
plt_keep <- subset(plt_keep, water_depth_m<150)
rm(plat_sf, gom_sf, plat_j)
gc()

### details per statzone
# 1-nearest neighbor; min,25%, median, 75%, max
# 2-number of rigs
# 3-depth distribution

b <- boxplot(plt_keep$water_depth_m ~ plt_keep$StatZone)
vioplot(plt_keep$water_depth_m ~ plt_keep$StatZone)



statz <- 13:21
dists <- st_distance(plt_keep)
all_dist_min <- apply(dists, 1, function(x) min(x[which(x>0)]))
all_dist_mean <- apply(dists, 1, function(x) mean(x[which(x>0)]))
all_dist_median <- apply(dists, 1, function(x) median(x[which(x>0)]))

# plot(1,1,typ='n',xlim=c(13,21),ylim=c(0,30), xaxt = 'n')
plot(1,1,typ='n',xlim=c(13,21),ylim=c(0,200), xaxt = 'n')
axis(1, 13:21)

st_nn_sum <- matrix(NA, 9, 6)
st_all_sum <- matrix(NA, 9, 6)
for(i in 1:length(statz)){
  ind <- which(plt_keep$StatZone==statz[i])
  s_dists <- dists[ind,ind]

  # nn <- apply(s_dists, 1, function(x) min(x[which(x>0)]))
  nn <- apply(s_dists, 1, function(x) mean(x[which(x>0)]))
  out <- summary(nn)
  st_nn_sum[i,] <- out

  boxplot(nn, at = statz[i], add = T, xaxt = 'n', yaxt = 'n')

  alls <- summary(s_dists[upper.tri(s_dists)])
  st_all_sum[i,] <- alls

  # boxplot(s_dists[upper.tri(s_dists)], at = statz[i], add= T, xaxt = 'n', yaxt = 'n')
}

rigs_sz <- aggregate(plt_keep$Area.Code, by = list(plt_keep$StatZone), length)

barplot(rigs_sz$x/sz_a[,2]*100, names = 13:21)
barplot(rigs_sz$x, names = 13:21)
barplot(sz_a[,2], names = 13:21)


### rigs summary
# 1 - rigs per SZ
# 2 - median nearest neighbor per SZ
rig_nn_med <- aggregate(all_dist_min, by = list(plt_keep$StatZone), median)
# 3 - median distance per SZ
# rig_dist_med <- aggregate(all_dist_median, by = list(plt_keep$StatZone), median)
# instead try this from above:
st_nn_sum[,3]
# 4 - mean depth per SZ
rigs_dep <- aggregate(plt_keep$water_depth_m, by = list(plt_keep$StatZone), median)

rigs_sum <- data.frame(StatZone = 13:21,
                       sz_a = sz_a[,2],
                       rigs_p_sz = rigs_sz$x,
                       rig_dens_10sq = rigs_sz$x/sz_a[,2]*100,
                       rig_nn_med = rig_nn_med$x,
                       # rig_dist_med = rig_dist_med$x,
                       rig_dist_med = st_nn_sum[,3],
                       rigs_dep = rigs_dep$x)

write.csv(rigs_sum,'rigs_summary.csv', row.names = F, append = T)

