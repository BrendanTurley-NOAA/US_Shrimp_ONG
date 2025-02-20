### simulate the probability of seeing one or more rigs at certain distances
# 1 - randomly select locations
# 1.1 - define shape to populate with random points
# 2 - create buffers based on line of sight calculations
# 3 - sum the number of buffers that have one or more rigs; divide by number of random points


###----------------> do this but instead of random points, random elb pings

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

setwd("C:/Users/brendan.turley/Documents/data/bathy")
bathy <- rast('etopo1.nc')
bathy_utm <- project(bathy, utm)
bathy[values(bathy$Band1)>0] <- NA
bathy[values(bathy$Band1)<(-150)] <- NA
bathy[!is.na(values(bathy$Band1))] <- 1
# plot(bathy)
bathy2 <- as.polygons(bathy, round=TRUE, aggregate=TRUE)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
# gom <- vect('GOM_2500ft.shp')
gom_s <- gom[which(gom$StatZone>12 & gom$Jurisdict=='Federal'), ]
gom_s <- aggregate(gom_s, by = 'StatZone')
new <- terra::intersect(gom_s, bathy2)
new <- project(new, utm) #26915, 32199, 32615
# plot(new)
# points(platforms$Longitude,platforms$Latitude)

gom <- st_as_sf(new) |>
  st_simplify(dTolerance = .01)
# gom <- st_transform(gom, utm) #26915, 32199, 32615
gwon <- as.owin(gom)
gc()

# ### random points ---------------
# set.seed(8)
# rpts <- runifpoint(nrow(plt_keep),gwon)
# rpts2 <- st_as_sf(data.frame(lon=rpts$x,lat=rpts$y),coords=c('lon','lat'))
# # st_crs(rpts2) <- 32615
# st_crs(rpts2) <- utm
#
# d_sf_rp <- st_distance(rpts2, by_element=F)
# diag(d_sf_rp) <- NA
# min_dist_rp <- apply(d_sf_rp,2,min,na.rm=T)
# summary(min_dist_rp)
#
# d_sf_plt <- st_distance(plt_keep, by_element=F)
# diag(d_sf_plt) <- NA
# min_dist_plt <- apply(d_sf_plt,2,min,na.rm=T)
# summary(min_dist_plt)
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
n_rpts <- 2e3 # update 2015/01/17, 2000 x 5 years = 10000 points per statzone, which is the same number for the random points used in 2_rig_probs_dist.R, previous 1e3
yrs <- 2015:2019
n <- 0

pb <- txtProgressBar(min = 0, max = 5*9*14, initial = 0, style = 3)
system.time(
  for(g in 1:length(yrs)){
    # g=4 # year
    # print(h)
    yr <- yrs[g]

    setwd("~/R_projects/US_Shrimp_ONG/data")
    load(paste0('effort_',yr,'_subset.RData'))

    # plat_g <- which(year(plt_keep$Install.Date)<yr &
    #                   year(plt_keep$Removal.Date)<=!yr |
    #                   is.na(plt_keep$Removal.Date))
    # plt_g <- plt_keep[plat_g,]

    ### new selection 2024/12/20 ###
    plat_i <- which(year(plt_keep$Install.Date)<yr &
                      year(plt_keep$Removal.Date)>yr)
    plat_j <- which(year(plt_keep$Install.Date)<yr &
                      is.na(plt_keep$Removal.Date))
    plat_ij <- union(plat_i, plat_j)
    plt_g <- plt_keep[plat_ij,]
    ###

    rig_p <- dist_i <- ls_dist_i <- lths_i <- z_i <- mth_i <- list()
    for(h in 1:length(dist_km)){
      # h=6
      for(i in 13:21){
        # i=13
        gom_i <- new[which(new$StatZone==i),]
        plt_i <- subset(plt_g, StatZone==i)
        eff_i <- subset(eff_keep,StatZone==i & Jurisdict=='Federal')

        samps <- sample(1:nrow(eff_i), n_rpts)
        eff_rpts <- eff_i[samps,]

        ### extract depths
        rptsv <- vect(eff_rpts)
        z <- extract(bathy_utm, rptsv)


        ind_j <- list()
        dist_j <- rep(NA, n_rpts)
        ls_dist_j <- rep(NA, n_rpts)
        # hor <- 7.5 # 7.5 , 21, 31
        for(j in 1:nrow(eff_rpts)){
          # j=1
          pt_j <- eff_rpts[j, ]

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

        lths <- lengths(ind_j)
        lths[ind_0] <- 0

        rig_p[[i-12]] <- tab/n_rpts
        dist_i[[i-12]] <- dist_j
        ls_dist_i[[i-12]] <- ls_dist_j
        lths_i[[i-12]] <- lths
        z_i[[i-12]] <- z$Band1
        mth_i[[i-12]] <- eff_rpts$month

        n <- n + 1
        setTxtProgressBar(pb, n)
      }
      assign(paste('eff_out', yr, dist_km[h], sep='_'),
             list(rig_p = rig_p,
                  dist_i = dist_i,
                  ls_dist_i = ls_dist_i,
                  lths_i = lths_i,
                  z_i = z_i,
                  mth_i = mth_i))
    }
  }
)
# user   system  elapsed
# 47850.39  9094.80 57319.75
# user   system  elapsed
# 53383.59  9986.14 63623.17
# user   system  elapsed
# 52824.31  9661.86 62681.88
# user    system   elapsed
# 103819.03  19929.62 124291.97
close(pb)


sv_ls <- as.list(ls()[grep('eff_out_',ls())])
# sv_ls <- as.list(get(ls()[grep('rpts_',ls())]))
eff_out_all <- as.list(lapply(ls()[grep('eff_out_',ls())],get))
names(eff_out_all) <- sv_ls

setwd("~/R_projects/US_Shrimp_ONG")
save(eff_out_all, file='./data/eff_out_1000-4.rdata')

