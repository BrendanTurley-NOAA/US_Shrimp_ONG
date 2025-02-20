### rig removal setup
# 1 - load rig data and select year for removals (2016-2018 so as to have one year before & after as treatment)
# 2 - save locations of rigs removed per year and create buffer shapefiles
# 3 - load ELB data year before
# 4 - per statzone: quantify total effort
# 5 - sum effort per rig-buffer


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
plt_keep <- subset(plat_j, StatZone>12 & water_depth_m<200)
rm(plat_sf, gom_sf, plat_j)
gc()

### removals
removals <- subset(plt_keep, year(Removal.Date)>2014 & year(Removal.Date)<2020)
addmargins(table(removals$StatZone, year(removals$Removal.Date)))
summary(removals$water_depth_m)

removals_s <- subset(plt_keep, year(Removal.Date)>2015 & year(Removal.Date)<2019)
addmargins(table(removals_s$StatZone, year(removals_s$Removal.Date)))
summary(removals_s$water_depth_m)

plot(gom_utm)
plot(st_geometry(plt_keep), add = T)
plot(st_geometry(removals), add = T,
     pch = 21, bg = 'magenta', cex = 1.5)
plot(st_geometry(removals_s), add = T,
     pch = 21, bg = 'cornflowerblue', cex = 1.5)



removals_2016 <- subset(plt_keep, year(Removal.Date)==2016)
removals_2017 <- subset(plt_keep, year(Removal.Date)==2017)
removals_2018 <- subset(plt_keep, year(Removal.Date)==2018)

nrow(removals_2016)
nrow(removals_2017)
nrow(removals_2018)

rm_2016 <- data.frame(table(removals_2016$StatZone)) |>
  setNames(c('statzone','removals')) |>
  data.frame(year = 2016)
rm_2017 <- data.frame(table(removals_2017$StatZone)) |>
  setNames(c('statzone','removals')) |>
  data.frame(year = 2017)
rm_2018 <- data.frame(table(removals_2018$StatZone)) |>
  setNames(c('statzone','removals')) |>
  data.frame(year = 2018)

removals_all <- rbind(rm_2016, rm_2017, rm_2018)

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

    plt_rm_i <- subset(plt_keep, year(Removal.Date)==yr)

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

table(out$statzone)
table(out$statzone,out$year)
table(out$dist_km)

setwd("~/R_projects/US_Shrimp_ONG")
# save(out, file='removals_prep.RData')
load('./data/removals_prep.RData')

hist(out$effort_pre, breaks = seq(0,5000,50))
hist(out$effort_post, breaks = seq(0,5000,50))

with(subset(out, dist_km==1),
     boxplot(effort_pre,
             effort_post))
with(subset(out, dist_km==7.5),
     boxplot(effort_pre,
             effort_post))

out <- merge(out, removals_all, by = c('year', 'statzone'), all = T)

length(which(out$effort_pre==0 & out$effort_post==0))
out <- out[-which(out$effort_pre==0 & out$effort_post==0),]

### make effort proportion of total sz effort
out$eff_pre_pro <- out$effort_pre/out$tot_sz_eff_pre
out$eff_post_pro <- out$effort_post/out$tot_sz_eff_post

dist_h <- c(1, 7.5)


cd_eff <- matrix(NA,2*length(13:20),5)
n <- 1

pdf('./figures/rig_rm_effort.pdf', width = 18, height = 9)
par(mfcol=c(2,8))
for(h in dist_h){
  for(i in 13:20){
    tmp <- subset(out, statzone==i & dist_km==h)
    slope <- tmp$effort_post - tmp$effort_pre
    d <- cohen.d(tmp$effort_post, tmp$effort_pre,
                 paired = T, na.rm = T,
                 hedges.correction = T, noncentral = T)

    plot(rep(1,nrow(tmp)),tmp$effort_pre,
         xlim=c(0.5,2.5), ylim = c(0,max(tmp[,7:8])),
         xaxt = 'n', xlab = '', ylab = 'effort (hours)',
         pch = 21, col = 'white', bg = ifelse(slope<0,4,2))
    segments(rep(1,nrow(tmp)),tmp$effort_pre,
             rep(2,nrow(tmp)),tmp$effort_post,
             col = ifelse(slope<0,4,2))
    points(rep(2,nrow(tmp)),tmp$effort_post,
           pch = 21, col = 'white', bg = ifelse(slope<0,4,2))
    axis(1, 1:2, tick = F, c('pre \nremoval', 'post \nremoval'))
    mtext(paste0('pos = ', length(which(slope>0)),
                 '\nneg = ', length(which(slope<0))),
          adj = 1)
    mtext(paste0('SZ \n',i),adj=0)

    vioplot(slope, names='',xaxt='n',ylab='slope (hours)')
    abline(h=0, lty=5,)
    mtext(paste0('Cohens D: ', round(d$estimate,2),
                 '\nUCI: ', round(d$conf.int[2],2),
                 '\nLCI: ', round(d$conf.int[1],2)),adj=1)

    cd_eff[n,] <- c(h,i,d$estimate,d$conf.int)
    n <- n + 1
  }
}
dev.off()


cd_eff_pro <- matrix(NA,2*length(13:20),5)
n <- 1

pdf('./figures/rig_rm_effort_p.pdf', width = 18, height = 9)
par(mfcol=c(2,8))
for(h in dist_h){
  for(i in 13:20){
    tmp <- subset(out, statzone==i & dist_km==h)
    slope <- tmp$eff_post_pro - tmp$eff_pre_pro
    d <- cohen.d(tmp$eff_post_pro, tmp$eff_pre_pro,
                 paired = T, na.rm = T,
                 hedges.correction = T, noncentral = T)

    plot(rep(1,nrow(tmp)),tmp$eff_pre_pro,
         xlim=c(0.5,2.5), ylim = c(0,max(tmp[,12:13])),
         xaxt = 'n', xlab = '', ylab = 'effort proportion',
         pch = 21, col = 'white', bg = ifelse(slope<0,4,2))
    segments(rep(1,nrow(tmp)),tmp$eff_pre_pro,
             rep(2,nrow(tmp)),tmp$eff_post_pro,
             col = ifelse(slope<0,4,2))
    points(rep(2,nrow(tmp)),tmp$eff_post_pro,
           pch = 21, col = 'white', bg = ifelse(slope<0,4,2))
    axis(1, 1:2, tick = F, c('pre \nremoval', 'post \nremoval'))
    mtext(paste0('pos = ', length(which(slope>0)),
                 '\nneg = ', length(which(slope<0))),
          adj = 1)
    mtext(paste0('SZ \n',i),adj=0)

    vioplot(slope, names='',xaxt='n',ylab='slope')
    abline(h=0, lty=5,)
    mtext(paste0('Cohens D: ', round(d$estimate,2),
                 '\nUCI: ', round(d$conf.int[2],2),
                 '\nLCI: ', round(d$conf.int[1],2)),adj=1)

    cd_eff_pro[n,] <- c(h,i,d$estimate,d$conf.int)
    n <- n + 1
  }
}
dev.off()

png('./figures/removal_cohensd.png', width = 7, height = 7, units = 'in', res = 300)
par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(13:20-.1, cd_eff[1:8,3], xlim = c(13,20),
     ylim=range(cd_eff[,4:5]),
     xlab = 'StatZone', ylab = 'Cohen`s D')
segments(13:20-.1, cd_eff[1:8,4],
         13:20-.1 ,cd_eff[1:8,5], lwd = 2)
segments(13:20+.1, cd_eff[9:16,4],
         13:20+.1 ,cd_eff[9:16,5], lwd = 2)
points(13:20-.1, cd_eff[1:8,3], pch=16, cex = 1.5)
points(13:20+.1, cd_eff[9:16,3], pch=16, col = 2, cex = 1.5)
abline(h=0,lty=5)
legend('topleft',c('1km','7.5km'),pch=16,col=c(1,2),bty='n')
mtext('`Raw` effort hours', adj=0)

plot(13:20-.1, cd_eff_pro[1:8,3], xlim = c(13,20),
     ylim=range(cd_eff_pro[,4:5]),
     xlab = 'StatZone', ylab = 'Cohen`s D')
segments(13:20-.1, cd_eff_pro[1:8,4],
         13:20-.1 ,cd_eff_pro[1:8,5], lwd = 2)
segments(13:20+.1, cd_eff_pro[9:16,4],
         13:20+.1 ,cd_eff_pro[9:16,5], lwd = 2)
points(13:20-.1, cd_eff_pro[1:8,3], pch=16, cex = 1.5)
points(13:20+.1, cd_eff_pro[9:16,3], pch=16, col = 2, cex = 1.5)
abline(h=0,lty=5)
legend('topleft',c('1km','7.5km'),pch=16,col=c(1,2),bty='n')
mtext('Effort as proporition of total SZ effort', adj=0)
dev.off()


vioplot(out$eff_pre_p - out$eff_post_p ~ out$statzone)
abline(h=0,lty=5)

vioplot(out$eff_pre_p - out$eff_post_p ~ out$year)
abline(h=0,lty=5)

vioplot(out$eff_pre_p - out$eff_post_p ~ out$dist_km)
abline(h=0,lty=5)


### 1 km model

dat_1km <- subset(out, dist_km==1)

names(dat_1km)
dat_1km_pre <- dat_1km[,c(1:7,12,9,11)]
dat_1km_pre$treatment <- 'ant'
names(dat_1km_pre)[7:9] <- c('effort', 'effort_p','tot_sz_eff')
dat_1km_post <- dat_1km[c(1:6,8,13,10:11)]
dat_1km_post$treatment <- 'des'
names(dat_1km_post)[7:9] <- c('effort', 'effort_p','tot_sz_eff')

dat_1km <- rbind(dat_1km_pre, dat_1km_post)
dat_1km$yr_prepost <- as.factor(paste(dat_1km$year, dat_1km$treatment, sep='-'))
names(dat_1km)

### scaled
dat_1km$effort_s <- (dat_1km$effort - mean(dat_1km$effort, na.rm = T))/sd(dat_1km$effort, na.rm = T)
dat_1km$tot_sz_eff_s <- (dat_1km$tot_sz_eff - mean(dat_1km$tot_sz_eff, na.rm = T))/sd(dat_1km$tot_sz_eff, na.rm = T)



for(i in 13:21){
  with(subset(dat_1km, statzone==i),
       vioplot(effort ~ yr_prepost, col = c(2,3)))
  abline(v=c(2.5,4.55))
  mtext(paste('statzone',i))
}

for(i in 13:21){
  with(subset(dat_1km, statzone==i),
       vioplot(effort_p ~ yr_prepost, col = c(2,3)))
  abline(v=c(2.5,4.55))
  mtext(paste('statzone',i))
}



### 7.5km model

dat_7km <- subset(out, dist_km==7.5)

names(dat_7km)
dat_7km_pre <- dat_7km[,c(1:7,12,9,11)]
dat_7km_pre$treatment <- 'ant'
names(dat_7km_pre)[7:9] <- c('effort', 'effort_p','tot_sz_eff')
dat_7km_post <- dat_7km[c(1:6,8,13,10:11)]
dat_7km_post$treatment <- 'des'
names(dat_7km_post)[7:9] <- c('effort', 'effort_p','tot_sz_eff')

dat_7km <- rbind(dat_7km_pre, dat_7km_post)
dat_7km$yr_prepost <- as.factor(paste(dat_7km$year, dat_7km$treatment, sep='-'))
names(dat_7km)

### scaled
dat_7km$effort_s <- (dat_7km$effort - mean(dat_7km$effort, na.rm = T))/sd(dat_7km$effort, na.rm = T)
dat_7km$tot_sz_eff_s <- (dat_7km$tot_sz_eff - mean(dat_7km$tot_sz_eff, na.rm = T))/sd(dat_7km$tot_sz_eff, na.rm = T)



for(i in 13:21){
  with(subset(dat_7km, statzone==i),
       vioplot(effort ~ yr_prepost, col = c(2,3)))
  abline(v=c(2.5,4.55))
  mtext(paste('statzone',i))
}

for(i in 13:21){
  with(subset(dat_7km, statzone==i),
       vioplot(effort_p ~ yr_prepost, col = c(2,3)))
  abline(v=c(2.5,4.55))
  mtext(paste('statzone',i))
}


setwd("~/R_projects/US_Shrimp_ONG")
save(out, dat_1km, dat_7km, file='./data/removals_prep_v2.RData')
