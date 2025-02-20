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

### gom shapfile for buffer subsetting
gom_fed_act <- gom_utm[which(gom_utm$Jurisdict=='Federal' &
                               gom_utm$Activity=='Y'),] |>
  aggregate('StatZone') |>
  st_as_sf()


### statzone buffers -----------------------

# dist_km <- c(.5,1,2,3,5,10,20,40)
dist_km <- c(.5,1,2,3,5,7.5,10,12.5,15,17.5,20,25,30,40)
units(dist_km) <- 'km'
# st_crs(eff_ij)$units
pts <- c(rep(16,5), rep(17,4))
yrs <- 2015:2019

pb <- txtProgressBar(min = 0, max = length(yrs), initial = 0, style = 3)
system.time(
  for(i in 1:length(yrs)){
    # i=1 # year
    # print(i)
    yr <- yrs[i]

    load(paste0('effort_',yr,'_subset.RData'))

    # plat_i <- which(year(plt_keep$Install.Date)<yr &
    #                   year(plt_keep$Removal.Date)<=!yr |
    #                   is.na(plt_keep$Removal.Date))
    # plt_i_alt <- plt_keep[plat_i,]

    ### new selection 2024/12/20 ###
    plat_i <- which(year(plt_keep$Install.Date)<yr &
                      year(plt_keep$Removal.Date)>yr)
    plat_j <- which(year(plt_keep$Install.Date)<yr &
                      is.na(plt_keep$Removal.Date))
    plat_ij <- union(plat_i, plat_j)
    plt_i <- plt_keep[plat_ij,]
    ###

    out <- matrix(NA,length(dist_km)*length(13:21),6)
    indxs <- list()
    m <- 1
    n <- length(dist_km)
    for(j in 13:21){
      # j=13 # statzone
      gom_j <- subset(gom_fed_act, StatZone==j)
      # eff_ij <- eff_keep[which(eff_keep$StatZone==j), ]
      eff_ij <- subset(eff_keep, StatZone==j & Jurisdict=='Federal')
      plt_ij <- subset(plt_i, StatZone==j)
      gc()

      indx <- list()
      for(k in 1:length(dist_km)){
        # k=1 # buffer
        buffs <- st_buffer(plt_ij,
                           dist = dist_km[k],
                           nQuadSegs = 30,
                           endCapStyle = "ROUND",
                           joinStyle = "ROUND")
        ### -> after buffer creation, remove unwanted areas from buffers
        buff_clip <- st_intersection(buffs, gom_j)
        ### -> onward!
        eff_buff <- st_intersects(eff_ij, buff_clip)
        sh_buf <- which(lengths(eff_buff)>0)
        indx[[k]] <- sh_buf
        # print(k)
      }
      rm(buffs, eff_buff, sh_buf)
      gc()
      # indxs[[i-2013]] <- indx
      indxs[[j-12]] <- indx

      eff_x <- sapply(indx, function(x) sum(eff_ij$hours_eff[x])) # sum of effort in buffers
      eff_xsum <- eff_x/sum(eff_ij$hours_eff)

      vessel_x <- sapply(indx, function(x) length(unique(eff_ij$VSBN[x]))) # number of vessels in buffers
      vessel_xsum <- vessel_x/length(unique(eff_ij$VSBN))

      out[m:n,] <- cbind(statzone=rep(j, length(dist_km)),
                         dist_km=dist_km,
                         effort_hrs=eff_x,
                         effort_pr=eff_xsum,
                         vessel_nu=vessel_x,
                         vessel_pr=vessel_xsum)
      m <- n + 1
      n <- n + length(dist_km)
    }
    out <- data.frame(out)
    names(out) <- c('statzone','dist_km','effort_hrs','effort_pr','vessel_nu','vessel_pr')
    write.csv(out,paste0(yr,'_eff_ves_dist4.csv'), row.names = F)


    png(paste0(yr,'_buffers_statzone4.png'), width = 8, height = 8, units = 'in', res = 300)
    par(mfrow=c(2,1), mar = c(4,4,1,1))
    plot(out$dist_km, out$effort_pr, typ = 'n', las = 1,
         xlab = 'Distance from rig (km)', ylab = 'Proportion of effort')
    mtext(yr)
    for(l in 13:21){
      tmp <- subset(out, out$statzone==l)
      points(tmp$dist_km, tmp$effort_pr, typ = 'o', pch = pts[l-12], col = l-12)
    }
    # legend('topleft',paste('StatZone',13:21), col = 1:19, pch = pts, bty = 'n', cex = .8)

    plot(out$dist_km, out$vessel_pr, typ = 'n', las = 1,
         xlab = 'Distance from rig (km)', ylab = 'Proportion of vessels')
    for(l in 13:21){
      tmp <- subset(out, out$statzone==l)
      points(tmp$dist_km, tmp$vessel_pr, typ = 'o', pch = pts[l-12], col = l-12)
    }
    legend('bottomright',paste('StatZone',13:21), col = 1:9, pch = pts, bty = 'n', cex = .8)
    dev.off()

    setTxtProgressBar(pb, i)
  }
)
close(pb)
# user  system elapsed
# 4658.81   82.01 4791.05

### plots -----------------------

setwd("~/R_projects/US_Shrimp_ONG/data")
load('horizon.RData')

setwd("~/R_projects/US_Shrimp_ONG/data")

yrs <- 2015:2019
for(i in 1:length(yrs)){
  yr <- yrs[i]
  tmp <- read.csv(paste0(yr,'_eff_ves_dist3.csv'))
  assign(paste0('eff_',yr), tmp)
}
