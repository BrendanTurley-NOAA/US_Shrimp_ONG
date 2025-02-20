### simulate the probability of seeing one or more rigs at certain distances
# 1 - randomly select locations
# 1.1 - define shape to populate with random points
# 2 - create buffers based on line of sight calculations
# 3 - sum the number of buffers that have one or more rigs; divide by number of random points

rm(list=ls())
gc()
library(cmocean)
library(data.table)
library(fields)
library(lubridate)
library(ncdf4)
library(scales)
library(sf)
library(spatstat)
library(terra)
library(vioplot)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
utm <- "+proj=utm +zone=15 units=km"
gom_utm <- project(gom, utm)
gom_utm <- gom_utm[which(gom_utm$StatZone>12 & gom_utm$Jurisdict=='Federal'), ]
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
rm(plat_sf, gom_sf, plat_j)
gc()

# plot(plt_keep$geometry)

setwd("C:/Users/brendan.turley/Documents/data/bathy")
bathy <- rast('etopo1.nc')
bathy_utm <- project(bathy, utm)
bathy[values(bathy$Band1)>0] <- NA
bathy[values(bathy$Band1)<(-150)] <- NA
bathy[!is.na(values(bathy$Band1))] <- 1
# plot(bathy)
bathy2 <- as.polygons(bathy, round=TRUE, aggregate=TRUE)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
gom_s <- gom[which(gom$StatZone>12 & gom$Jurisdict=='Federal'), ]
gom_s <- aggregate(gom_s, by = 'StatZone')
new <- terra::intersect(gom_s, bathy2)
new <- project(new, utm) #26915, 32199, 32615
# plot(new)
# points(platforms$Longitude,platforms$Latitude)

gom <- st_as_sf(new) |>
  st_simplify(dTolerance = .01)
gom <- st_transform(gom, utm) #26915, 32199, 32615
# plot(as.owin(gom))
gwon <- as.owin(gom)

### random points ---------------
# set.seed(8)
# rpts <- runifpoint(nrow(plt_keep),gwon)
# rpts2 <- st_as_sf(data.frame(lon=rpts$x,lat=rpts$y),coords=c('lon','lat'))
# st_crs(rpts2) <- utm
#
# d_sf <- st_distance(rpts2, by_element=F)
# d_sf_m <- d_sf
# diag(d_sf_m) <- NA
# min.dist <- apply(d_sf_m,2,min,na.rm=T)
# summary(min.dist)
#
# d_sf <- st_distance(plt_keep, by_element=F)
# d_sf_m <- d_sf
# diag(d_sf_m) <- NA
# min.dist <- apply(d_sf_m,2,min,na.rm=T)
# summary(min.dist)
#
# rpts2 <- st_transform(rpts2, st_crs(new))
# rpts2 <- st_multipoint(cbind(unlist(rpts2$geometry)[seq(1,nrow(rpts2)*2,2)],
#                              unlist(rpts2$geometry)[seq(2,nrow(rpts2)*2,2)]))
#
# plot(new)
# points(rpts2,pch=16,cex=.5,col=4)
# points(st_coordinates(plt_keep),pch=16,cex=.5,col=2)
# legend('bottomright',c('random pts','oil rigs'),pch=16,col=c(4,2),bty='n')




### random points per statzone ---------------
# hor <- 31 # 7.5 , 21, 31
dist_km <- c(.5,1,2,3,5,7.5,10,12.5,15,17.5,20,25,30,40)
units(dist_km) <- 'km'
n_rpts <- 1e4

rig_p <- dist_i <- ls_dist_i <- lths_i <- list()
pb <- txtProgressBar(min = 0, max = length(dist_km), initial = 0, style = 3)
system.time(
  for(h in 1:length(dist_km)){
    # h=10
    for(i in 13:21){
      # i=13
      gom_i <- new[which(new$StatZone==i),]
      plt_i <- subset(plt_keep, StatZone==i)

      rpts <- runifpoint(n_rpts, as.owin(st_as_sf(gom_i)))
      rpts2 <- st_as_sf(data.frame(x = rpts$x,
                                   y = rpts$y),
                        coords = c('x','y'),
                        crs = utm)
      ### extract depths
      rptsv <- vect(rpts2)
      z_i <- extract(bathy_utm, rptsv)

      # plot(gom_i)
      # points(rpts2,pch=16,cex=.5,col=4)
      # points(plt_i,pch=16,cex=.5,col=2)
      # plot(buffs, add = T)

      ind_j <- list()
      dist_j <- rep(NA, n_rpts)
      ls_dist_j <- rep(NA, n_rpts)

      for(j in 1:nrow(rpts2)){
        # j=10
        pt_j <- rpts2[j, ]

        buff <- st_buffer(pt_j,
                          dist = dist_km[h],
                          nQuadSegs = 30,
                          endCapStyle = "ROUND",
                          joinStyle = "ROUND")

        rpt_buff <- st_intersects(plt_i, buff)
        pt_buf <- which(lengths(rpt_buff)>0)
        if(length(pt_buf)==0) {
          pt_buf <- 0
        }
        if(length(pt_buf>1)){
          dists <- st_distance(plt_i[pt_buf,])
          diag(dists) <- NA
          dist_j[j] <- median(dists, na.rm = T)

          ls_d <- st_distance(pt_j,plt_i[pt_buf,])
          ls_dist_j[j] <- median(ls_d, na.rm = T)
        }
        ind_j[[j]] <- pt_buf
      }
      ### tabulate accounting for 0s
      ind_0 <- sapply(ind_j, function(x) all(x == 0, na.rm = TRUE))
      tab <- c(table(lengths(ind_j[ind_0])) ,table(lengths(ind_j[!ind_0])))
      names(tab)[1] <- '0'
      # tab/n_rpts

      lths <- lengths(ind_j)
      lths[ind_0] <- 0

      # hist(lths,breaks=seq(-.5,max(lths)+1,1), main = paste('StatZone',i))
      # hist(dist_j)
      # plot(lths, dist_j, pch = 16, col = alpha(1, .3))

      rig_p[[i-12]] <- tab/n_rpts
      dist_i[[i-12]] <- dist_j
      ls_dist_i[[i-12]] <- ls_dist_j
      lths_i[[i-12]] <- lths

    }
    assign(paste0('rpts_out_', h), list(rig_p = rig_p,
                                        dist_i = dist_i,
                                        ls_dist_i = ls_dist_i,
                                        lths_i = lths_i,
                                        z_i = z_i$Band1))
    setTxtProgressBar(pb, h)
  }
)
close(pb)
# user    system   elapsed
# 99598.91  18797.94 118973.86
# user    system   elapsed
# 98347.75  18700.90 117471.81

sv_ls <- as.list(ls()[grep('rpts_',ls())])
# sv_ls <- as.list(get(ls()[grep('rpts_',ls())]))
tmp <- as.list(lapply(ls()[grep('rpts_',ls())],get))
names(tmp) <- sv_ls

setwd("~/R_projects/US_Shrimp_ONG")
save(tmp, file='./data/rpts_10000-2.rdata')
save(rpts_out_1,
     rpts_out_10,
     rpts_out_11,
     rpts_out_12,
     rpts_out_13,
     rpts_out_14,
     rpts_out_2,
     rpts_out_3,
     rpts_out_4,
     rpts_out_5,
     rpts_out_6,
     rpts_out_7,
     rpts_out_8,
     rpts_out_9, file='./data/rpts_10000-3.rdata')

# save(rig_p, dist_i, ls_dist_i, lths_i, file='./data/rig_probs4.rdata')
# load('./data/rig_probs2.rdata')

sv_ls <- sv_ls[c(1,7:14,2:6)]

rigs_out <- matrix(NA, 14, 9)
for(i in 1:14){
  rigs_p <- with(get(unlist(sv_ls[i])),
                 rbind(as.vector(sapply(rig_p, function(x) sum(x[2:length(x)]))),
                       as.vector(sapply(rig_p, function(x) x[1]))))
  rigs_out[i,] <- rigs_p[1,]
}
# rigs_out <- rigs_out[c(1,7:14,2:6),]
imagePlot(as.numeric(dist_km), 13:21, rigs_out, col = cmocean('haline')(60))


cols <- cmocean('thermal')(9)

plot(dist_km, rigs_out[,1], typ='l', col = cols[1], lwd = 2)
for(i in 2:9){
  points(dist_km, rigs_out[,i], typ='l', col = cols[i], lwd = 2)
}
legend('topleft', legend = c(13:21), col = cols, lwd = 2, bty = 'n')

plot(dist_km, c(0,diff(rigs_out[,1])), typ='l', col = cols[1], lwd = 2)
for(i in 2:9){
  points(dist_km, c(0,diff(rigs_out[,i])), typ='l', col = cols[i], lwd = 2)
}
legend('topleft', legend = c(13:21), col = cols, lwd = 2, bty = 'n')


rigs_p <- with(rpts_out_1,
               rbind(as.vector(sapply(rig_p, function(x) sum(x[2:length(x)]))),
                     as.vector(sapply(rig_p, function(x) x[1]))))

barplot(rigs_p,
        names = 13:21)

with(rpts_out_1,
     hist(z_i))

with(rpts_out_1,
     plot(z_i, lths_i))

with(rpts_out_4,
     {
       vioplot(dist_i,side='left',col=2,plotCentre = "line",names=13:21)
       vioplot(ls_dist_i,side='right',col=4,add=T,plotCentre = "line")
       legend('topright',c('b/t rigs','LoS'),fill=c(2,4),horiz=T,bty='n')
       abline(h=7.5,col='gold3',lty=5)
       mtext()
     })
rpts_out_5$dist_i


for(i in 1:14){
  with(get(unlist(sv_ls[i])),
                 hist(unlist(dist_i)))
}

par(mfrow=c(2,2))
for(i in 1:14){
  with(get(unlist(sv_ls[i])), {
    hist(unlist(ls_dist_i))
    hist(unlist(dist_i))
    hist(unlist(lths_i))
    plot(unlist(lths_i), unlist(ls_dist_i))
    print(dist_km[i])
    print(wilcox.test(unlist(ls_dist_i),
           unlist(dist_i), alternative = 'greater'))
  }
  )
}
# the wilcox results: line of sight distance is (significantly) greater at less/equal to 10 km
# the wilcox results: line of sight distance is (significantly) less at greater than 10 km

