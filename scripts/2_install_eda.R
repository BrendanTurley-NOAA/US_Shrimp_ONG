### rig removal setup
# 1 - load rig data and select year for removals (2016-2018 so as to have one year before & after as treatment)
# 2 - save locations of rigs removed per year and create buffer shapefiles
# 3 - load ELB data year before
# 4 - per statzone: quantify total effort
# 5 - sum effort per rig-buffer

dev.off()
rm(list=ls())
gc()
library(cmocean)
library(data.table)
library(effsize)
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
# utm <- "+proj=utm +zone=15 units=km"
utm_15n <- 'EPSG:26715'
gom_utm <- project(gom, utm_15n)
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
# plt_keep <- subset(plat_j, StatZone>12 & water_depth_m<200)
plt_keep <- subset(plat_j, StatZone>12 & water_depth_m<150 & Jurisdict=='Federal' & year(Install.Date)<2020)
rm(plat_sf, gom_sf, plat_j)
gc()

### construction
hist(year(platforms$Install.Date),breaks=seq(1940,2024,1))
abline(v=seq(1940,2030,10),lty=5)
# nrow(subset(platforms, year(Install.Date)>2014 & year(Install.Date)<2020))
construction <- subset(plt_keep, year(Install.Date)>2014 & year(Install.Date)<2020)
table(construction$StatZone, year(construction$Install.Date))
table(construction$water_depth_m, year(construction$Install.Date))
summary(construction$water_depth_m)

plot(gom_utm)
plot(st_geometry(plt_keep), add = T)
plot(st_geometry(construction), add = T,
     pch = 21, bg = 'magenta', cex = log(construction$water_depth_m))
hist(construction)

### removals
hist(year(platforms$Removal.Date),breaks=seq(1940,2024,1))
abline(v=seq(1940,2030,10),lty=5)
removals <- subset(plt_keep, year(Removal.Date)>2014 & year(Removal.Date)<2020)
table(removals$StatZone, year(removals$Removal.Date))


yr <- 2015:2019
for(i in 1:length(yr)){
  # plat_i <- which(year(plt_keep$Install.Date)<yr[i] &
  #                   year(plt_keep$Removal.Date)<=!yr[i] |
  #                   is.na(plt_keep$Removal.Date))
  # plt_i <- plt_keep[plat_i,]

  plat_i <- which(year(plt_keep$Install.Date)<yr[i] &
                    year(plt_keep$Removal.Date)>yr[i])
  plat_j <- which(year(plt_keep$Install.Date)<yr[i] &
                    is.na(plt_keep$Removal.Date))
  plat_ij <- union(plat_i, plat_j)
  plt_i <- plt_keep[plat_ij,]
}



yrs <- sort(unique(year(construction$Install.Date)))


### gom shapfile for buffer subsetting
gom_fed_act <- gom_utm[which(gom_utm$Jurisdict=='Federal' &
                               gom_utm$Activity=='Y'),] |>
  aggregate('StatZone') |>
  st_as_sf()


### statzone buffers -----------------------

dist_km <- c(1,7.5)
units(dist_km) <- 'km'

yrs <- 2016:2018

m <- 1
n <- 0
out <- matrix(NA,length(dist_km)*length(13:21)*length(yrs)*600,10)

pb <- txtProgressBar(min = 0, max = length(yrs), initial = 0, style = 3)
system.time(
  for(i in 1:length(yrs)){
    # i=1 # year
    # print(i)
    yr <- yrs[i]

    plt_rm_i <- subset(plt_keep, year(Install.Date)==yr)

    load(paste0('effort_',yr-1,'_subset.RData'))
    eff_pre <- eff_keep
    load(paste0('effort_',yr+1,'_subset.RData'))
    eff_post <- eff_keep
    rm(eff_keep)
    gc()

    for(j in 13:21){
      # j=13 # statzone
      plt_rm_ij <- subset(plt_rm_i, StatZone==j)

      if(nrow(plt_rm_ij)>0){
        gom_j <- subset(gom_fed_act, StatZone==j)
        # eff_ij <- eff_keep[which(eff_keep$StatZone==j), ]
        eff_pre_ij <- subset(eff_pre, StatZone==j & Jurisdict=='Federal')
        eff_post_ij <- subset(eff_post, StatZone==j & Jurisdict=='Federal')
        eff_pre_ij <- st_transform(eff_pre_ij, crs = st_crs(plt_rm_ij))
        eff_post_ij <- st_transform(eff_post_ij, crs = st_crs(plt_rm_ij))
        gc()

        for(k in 1:length(dist_km)){
          # k=1 # buffer
          buffs <- st_buffer(plt_rm_ij,
                             dist = dist_km[k],
                             nQuadSegs = 30,
                             endCapStyle = "ROUND",
                             joinStyle = "ROUND")
          ### -> after buffer creation, remove unwanted areas from buffers
          buff_clip <- st_intersection(buffs, gom_j)
          n <- n + nrow(plt_rm_ij)

          ### -> onward!
          eff_pre_buff <- st_intersects(buff_clip,eff_pre_ij) |>
            sapply(function(x) sum(eff_pre_ij$hours_eff[x]))

          eff_post_buff <- st_intersects(buff_clip,eff_post_ij) |>
            sapply(function(x) sum(eff_post_ij$hours_eff[x]))

          out[m:n,] <- cbind(rep(yr,length(m:n)),
                             rep(dist_km[k], length(m:n)),
                             plt_rm_ij$Complex.Id.Num,
                             paste(plt_rm_ij$Complex.Id.Num, plt_rm_ij$Structure.Name, sep='-'),
                             plt_rm_ij$water_depth_m,
                             plt_rm_ij$StatZone,
                             eff_pre_buff,
                             eff_post_buff,
                             sum(eff_pre_ij$hours_eff,na.rm=T),
                             sum(eff_post_ij$hours_eff,na.rm=T))
          m <- n + 1
          gc()
        }
      }
    }
    setTxtProgressBar(pb, i)
  }
)
close(pb)
dim(out)
out <- data.frame(out)
names(out) <- c('year','dist_km','complex_id','id_name','depth_m','statzone','effort_pre','effort_post','tot_sz_eff_pre','tot_sz_eff_post')
out <- out[which(!is.na(out$year)),]
out <- type.convert(out)

setwd("~/R_projects/US_Shrimp_ONG")
# save(out, construction, file = './data/installs_prep.RData')
load('./data/installs_prep.RData')

table(out$statzone)
table(out$statzone,out$year)
table(out$dist_km)

### make effort proportion of total sz effort
out$eff_pre_pro <- out$effort_pre/out$tot_sz_eff_pre
out$eff_post_pro <- out$effort_post/out$tot_sz_eff_post

hist(out$effort_pre, breaks = seq(0,5000,50))
hist(out$effort_post, breaks = seq(0,5000,50))

with(subset(out, dist_km==1),
     boxplot(effort_pre,
             effort_post))
with(subset(out, dist_km==7.5),
     boxplot(effort_pre,
             effort_post))

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('construction_eda.png', width = 6, height = 8, units = 'in', res = 300)

par(mfrow = c(2,1), mar = c(4,5,1,1))
with(subset(out, dist_km==7.5),
     barplot(rbind(effort_pre,
             effort_post), beside = T,
             names = year, las = 1))
mtext('Effort (hours)', 2, line = 3.5)

with(subset(out, dist_km==7.5),
     barplot(rbind(eff_pre_pro,
                   eff_post_pro), beside = T,
             names = year, las = 1))
mtext('Effort (proportion)', 2, line = 3.5)
legend('topleft', c('Prior', 'Post'), fill = c('gray20', 'gray90'), bty = 'n')

dev.off()


out$effort_pre - out$effort_post
out$eff_pre_pro - out$eff_post_pro

barplot(out[,c(7,8)])
