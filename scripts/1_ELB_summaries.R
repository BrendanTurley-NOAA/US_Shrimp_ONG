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

# setwd("~/R_projects/US_Shrimp_ONG/data")
# platforms <- readRDS('gomx_platforms.rds')
# plat_sf <- st_as_sf(platforms,
#                     coords = c('Longitude', 'Latitude'),
#                     crs = 4326)
# plat_sf <- st_transform(plat_sf, crs = st_crs(gom_sf))
# plat_j <- st_join(plat_sf, gom_sf,
#                   join = st_intersects,
#                   left = T)
# plt_keep <- subset(plat_j, plat_j$StatZone>12)
# rm(plat_sf, plat_j)
gc()

### gom shapfile for buffer subsetting
gom_fed_act <- gom_utm[which(gom_utm$Jurisdict=='Federal' &
                               gom_utm$Activity=='Y'),] |>
  aggregate('StatZone') |>
  st_as_sf()

gom_sz_area <- data.frame('statzone' = 13:21, 'area_km2' = st_area(gom_fed_act))

sz_mth <- expand.grid(StatZone = 13:21, month = 1:12)
sz_wk <- expand.grid(StatZone = 13:21, week = 1:53)

### calculate ELB stats per year
setwd("~/R_projects/US/data")
yrs <- 2015:2019
tots <- matrix(NA, 5, 4)
mth_sum <- matrix(NA, length(13:21)*12*length(yrs), 5)
wk_sum <- matrix(NA, length(13:21)*53*length(yrs), 5)
m <- o <- 1
n <- p <- 0
pb <- txtProgressBar(min = 0, max = length(yrs), initial = 0, style = 3)
for(i in 1:length(yrs)){
  # setTxtProgressBar(pb, i-1)
  # i=2015 # year
  # print(i)
  yr <- yrs[i]
  load(paste0('effort_',yr,'_subset.RData'))

  # tot_e <- tot_epa * tot_a
  # assign(paste0('tot_e_',yr), tot_e)
  # assign(paste0('tot_a_',yr), tot_a)
  # assign(paste0('tot_epa_',yr), tot_epa)

  eff_keep <- subset(eff_keep, Jurisdict=='Federal')
  eff_keep$wk <- week(eff_keep$STAMP)

  tab <- table(eff_keep$VSBN, eff_keep$trip, eff_keep$tow)

  tot_eff <- sum(eff_keep$hours_eff)
  tot_tows <- sum(apply(tab, 1, function(x) length(which(x>0))))
  num_ves <- dim(tab)[1]
  tots[i,] <- c(yr, tot_eff, tot_tows, num_ves)


 ### aggregates
  ### monthly
  mth_eff <- setNames(aggregate(eff_keep$hours_eff,
                                by = list(eff_keep$StatZone, eff_keep$month),
                                sum, na.rm = T),
                      c('StatZone', 'month', 'effort'))
  mth_ves <- setNames(aggregate(eff_keep$VSBN,
                                by = list(eff_keep$StatZone, eff_keep$month),
                                function(x) length(unique(x))),
                      c('StatZone', 'month', 'vessel'))
  mth_eff_ves <- merge(sz_mth, mth_eff, by = c('StatZone', 'month'), all = T) |>
    merge(mth_ves, by = c('StatZone', 'month'), all = T)
  mth_eff_ves[is.na(mth_eff_ves)] <- 0

  ### weekly
  wk_eff <- setNames(aggregate(eff_keep$hours_eff,
                      by = list(eff_keep$StatZone, eff_keep$wk),
                      sum, na.rm = T),
                     c('StatZone', 'week', 'effort'))
  wk_ves <- setNames(aggregate(eff_keep$VSBN,
                      by = list(eff_keep$StatZone, eff_keep$wk),
                      function(x) length(unique(x))),
                     c('StatZone', 'week', 'vessel'))
  wk_eff_ves <- merge(sz_wk, wk_eff, by = c('StatZone', 'week'), all = T) |>
    merge(wk_ves, by = c('StatZone', 'week'), all = T)
  wk_eff_ves[is.na(wk_eff_ves)] <- 0

  n <- n + nrow(mth_eff_ves)
  p <- p + nrow(wk_eff_ves)

  mth_sum[m:n, ] <- cbind(yr, as.matrix(mth_eff_ves))
  wk_sum[o:p, ] <- cbind(yr, as.matrix(wk_eff_ves))

  m <- n + 1
  o <- p + 1

  rm(eff_keep, mth_eff, mth_ves, wk_eff, wk_ves, mth_eff_ves, wk_eff_ves)

  setTxtProgressBar(pb, i)
}
close(pb)

mth_sum <- setNames(as.data.frame(mth_sum), c('year', 'statzone','month','effort','vessels'))
wk_sum <- setNames(as.data.frame(wk_sum), c('year', 'statzone','week','effort','vessels'))
tots <- setNames(as.data.frame(tots), c('year', 'hours', 'trips', 'vessels'))

setwd("~/R_projects/US_Shrimp_ONG/data")
# save(mth_sum, wk_sum, tots, file='elb_summaries.RData')
load('elb_summaries.RData')


### statzone summation
eff_yr <- aggregate(effort ~ statzone, data = mth_sum, sum)
sum(mth_sum$effort)
quantile(eff_yr$effort, seq(0,1,.25))


BOEM_blk <- 23.04 # size of BOEM block

eff_mth_agg <- aggregate(mth_sum$effort,
                         by = list(statzone = mth_sum$statzone,
                                   month = mth_sum$month),
                         median, na.rm = T) |>
  merge(gom_sz_area, by = c('statzone'), all = T)
eff_mth_agg$eff_area <- eff_mth_agg$x / eff_mth_agg$area_km2

eff_mth_agg <- eff_mth_agg[order(eff_mth_agg$statzone, eff_mth_agg$month),]
# mth_agg <- aggregate(mth_sum$effort,
#                      by = list(statzone = mth_sum$statzone,
#                                month = mth_sum$month),
#                      function(x) sd(x,na.rm=T)/sqrt(length(x)))

ves_mth_agg <- aggregate(mth_sum$vessels,
                         by = list(statzone = mth_sum$statzone,
                                   month = mth_sum$month),
                         median, na.rm = T) |>
  merge(gom_sz_area, by = c('statzone'), all = T)
ves_mth_agg$ves_area <- ves_mth_agg$x / ves_mth_agg$area_km2

ves_mth_agg <- ves_mth_agg[order(ves_mth_agg$statzone, ves_mth_agg$month),]
ves_mth_agg$eff_ves <- eff_mth_agg$x / ves_mth_agg$x
ves_mth_agg$eff_ves[is.na(ves_mth_agg$eff_ves)] <- 0
ves_mth_agg$eff_ves_a <- eff_mth_agg$x / ves_mth_agg$x / ves_mth_agg$area_km2
ves_mth_agg$eff_ves_a[is.na(ves_mth_agg$eff_ves_a)] <- 0


cols <- cmocean('thermal')(length(13:21))
pts <- rep(c(21,24,23),3)

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_clim.png', width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(3,1), mar = c(3,5,1,1))

plot(eff_mth_agg$month, eff_mth_agg$x,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(eff_mth_agg$x), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(eff_mth_agg$x), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median effort (hours)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(eff_mth_agg, statzone==i),
       points(month, x,typ = 'l', lwd = 3, col = 1))
  with(subset(eff_mth_agg, statzone==i),
       points(month, x,typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(eff_mth_agg, statzone==i),
       points(month, x,typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}
legend('topleft', legend=c(paste(13:16,'LA',sep='-'),
                           paste(17,'LA/TX',sep='-'),
                           paste(18:21,'TX',sep='-')),
       pch = pts, pt.bg = cols, bty = 'n')
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')

plot(ves_mth_agg$month, ves_mth_agg$x,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(ves_mth_agg$x), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(ves_mth_agg$x), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median number of vessels', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(month, x, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(month, x, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(month, x, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

plot(ves_mth_agg$month, ves_mth_agg$eff_ves,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(ves_mth_agg$eff_ves), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(ves_mth_agg$eff_ves), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median effort (hours / vessel)', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(month, eff_ves,typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(month, eff_ves,typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(month, eff_ves,typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

dev.off()


setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_clim2.png', width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(3,1), mar = c(3,5,1,1))

plot(eff_mth_agg$month, eff_mth_agg$eff_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(eff_mth_agg$eff_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(eff_mth_agg$eff_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median effort density (hrs / km^2)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(eff_mth_agg, statzone==i),
       points(month, eff_area,typ = 'l', lwd = 3, col = 1))
  with(subset(eff_mth_agg, statzone==i),
       points(month, eff_area,typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(eff_mth_agg, statzone==i),
       points(month, eff_area,typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}
legend('topleft', legend=c(paste(13:16,'LA',sep='-'),
                           paste(17,'LA/TX',sep='-'),
                           paste(18:21,'TX',sep='-')),
       pch = pts, pt.bg = cols, bty = 'n')
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')

plot(ves_mth_agg$month, ves_mth_agg$ves_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(ves_mth_agg$ves_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(ves_mth_agg$ves_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median vessel density (1/km^2)', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(month, ves_area, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(month, ves_area, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(month, ves_area, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

plot(ves_mth_agg$month, ves_mth_agg$eff_ves_a,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(ves_mth_agg$eff_ves_a), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(ves_mth_agg$eff_ves_a), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median vessel effort density (hrs / vessel / km^2)', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(month, eff_ves_a,typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(month, eff_ves_a,typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(month, eff_ves_a,typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

dev.off()






eff_mth_agg <- aggregate(effort ~ statzone + year,
                         data = mth_sum,
                         sum, na.rm = T) |>
  merge(gom_sz_area, by = c('statzone'), all = T)
eff_mth_agg$eff_area <- eff_mth_agg$effort / eff_mth_agg$area_km2
eff_mth_agg <- eff_mth_agg[order(eff_mth_agg$statzone, eff_mth_agg$year),]

ves_mth_agg <- aggregate(vessels ~ statzone + year,
                         data = mth_sum,
                         sum, na.rm = T) |>
  merge(gom_sz_area, by = c('statzone'), all = T)
ves_mth_agg$ves_area <- ves_mth_agg$vessels / ves_mth_agg$area_km2

ves_mth_agg <- ves_mth_agg[order(ves_mth_agg$statzone, ves_mth_agg$year),]
ves_mth_agg$eff_ves <- eff_mth_agg$effort / ves_mth_agg$vessels
ves_mth_agg$eff_ves[is.na(ves_mth_agg$eff_ves)] <- 0
ves_mth_agg$eff_ves_a <- eff_mth_agg$effort / ves_mth_agg$vessels / ves_mth_agg$area_km2
ves_mth_agg$eff_ves_a[is.na(ves_mth_agg$eff_ves_a)] <- 0


cols <- cmocean('thermal')(length(13:21))
pts <- rep(c(21,24,23),3)

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_ann.png', width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(3,1), mar = c(3,5,1,1))

plot(eff_mth_agg$year, eff_mth_agg$effort,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median effort (hours)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(eff_mth_agg, statzone==i),
       points(year, effort, typ = 'l', lwd = 3, col = 1))
  with(subset(eff_mth_agg, statzone==i),
       points(year, effort, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(eff_mth_agg, statzone==i),
       points(year, effort, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}
legend('topleft', legend=c(paste(13:16,'LA',sep='-'),
                           paste(17,'LA/TX',sep='-'),
                           paste(18:21,'TX',sep='-')),
       pch = pts, pt.bg = cols, bty = 'n')
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')

plot(ves_mth_agg$year, ves_mth_agg$vessels,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median number of vessels', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(year, vessels, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(year, vessels, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(year, vessels, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

plot(ves_mth_agg$year, ves_mth_agg$eff_ves,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median effort (hours / vessel)', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(year, eff_ves,typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(year, eff_ves,typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(year, eff_ves,typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

dev.off()


setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_ann2.png', width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(3,1), mar = c(3,5,1,1))

plot(eff_mth_agg$year, eff_mth_agg$eff_area,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median effort density (hrs / km^2)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(eff_mth_agg, statzone==i),
       points(year, eff_area, typ = 'l', lwd = 3, col = 1))
  with(subset(eff_mth_agg, statzone==i),
       points(year, eff_area, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(eff_mth_agg, statzone==i),
       points(year, eff_area, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}
legend('topleft', legend=c(paste(13:16,'LA',sep='-'),
                           paste(17,'LA/TX',sep='-'),
                           paste(18:21,'TX',sep='-')),
       pch = pts, pt.bg = cols, bty = 'n')
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')

plot(ves_mth_agg$year, ves_mth_agg$ves_area,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median vessel density (1/km^2)', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(year, ves_area, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(year, ves_area, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(year, ves_area, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

plot(ves_mth_agg$year, ves_mth_agg$eff_ves_a,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median vessel effort density (hrs / vessel / km^2)', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(year, eff_ves_a,typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(year, eff_ves_a,typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(year, eff_ves_a,typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

dev.off()



png('elb_eff_ves_mth.png', width = 24, height = 12, units = 'in', res = 300)
# par(mfrow=c(5,3), mar = c(2,5,1.5,1.5))
par(mfcol=c(3,5), mar = c(2,5,1.5,1.5))
for(i in 2015:2019){
  mth_sum_i <- subset(mth_sum, year==i)

  plot(1, 1, xlim = c(1,12), ylim = c(0,max(mth_sum$effort)),
       typ = 'n', xaxt = 'n', las = 1,
       xlab = '', ylab = '')
  rect(4.5, 0,
       7.5, max(mth_sum$effort), col = 'gray90', border = NA)
  rect(5.5, 0,
       6.5, max(mth_sum$effort), col = 'gray70', border = NA)
  axis(1,1:12,month.abb[1:12])
  grid()
  for(j in 13:21){
    # j=13
    mth_sum_ij <- subset(mth_sum_i, statzone==j)
    points(mth_sum_ij$month, mth_sum_ij$effort,
           typ = 'l', lwd = 2, col = cols[j-12])
    points(mth_sum_ij$month, mth_sum_ij$effort,
           typ = 'p', pch = pts[j-12], bg = cols[j-12], cex = 1.5)
  }
  if(i==2015){
    legend('topleft', legend = c(13:21), pch = c(21,24,23), pt.bg = cols, bty = 'n')
  }
  mtext(i)
  mtext('Effort (hours)', 2, 3.5)

  plot(1, 1, xlim = c(1,12), ylim = c(0,max(mth_sum$vessels)),
       typ = 'n', xaxt = 'n', las = 1,
       xlab = '', ylab = '')
  rect(4.5, 0,
       7.5, max(mth_sum$vessels), col = 'gray90', border = NA)
  rect(5.5, 0,
       6.5, max(mth_sum$vessels), col = 'gray70', border = NA)
  axis(1,1:12,month.abb[1:12])
  grid()
  for(j in 13:21){
    # j=13
    mth_sum_ij <- subset(mth_sum_i, statzone==j)
    points(mth_sum_ij$month, mth_sum_ij$vessels,
           typ = 'l', lwd = 2, col = cols[j-12])
    points(mth_sum_ij$month, mth_sum_ij$vessels,
           typ = 'p', pch = pts[j-12], bg = cols[j-12], cex = 1.5)
  }
  # legend('topleft', legend = c(13:21), pch = c(21,24,23), pt.bg = cols, bty = 'n')
  # mtext(i)
  mtext('Vessels', 2, 3.5)

  plot(1, 1, xlim = c(1,12), ylim = c(0,max(mth_sum$effort/mth_sum$vessels,na.rm=T)),
       typ = 'n', xaxt = 'n', las = 1,
       xlab = '', ylab = '')
  rect(4.5, 0,
       7.5, max(mth_sum$effort/mth_sum$vessels,na.rm=T), col = 'gray90', border = NA)
  rect(5.5, 0,
       6.5, max(mth_sum$effort/mth_sum$vessels,na.rm=T), col = 'gray70', border = NA)
  axis(1,1:12,month.abb[1:12])
  grid()
  for(j in 13:21){
    # j=13
    mth_sum_ij <- subset(mth_sum_i, statzone==j)
    mth_sum_ij$vessels[which(mth_sum_ij$vessels==0)] <- 1
    points(mth_sum_ij$month, mth_sum_ij$effort/mth_sum_ij$vessels,
           typ = 'l', lwd = 2, col = cols[j-12])
    points(mth_sum_ij$month, mth_sum_ij$effort/mth_sum_ij$vessels,
           typ = 'p', pch = pts[j-12], bg = cols[j-12], cex = 1.5)
  }
  # legend('topleft', legend = c(13:21), pch = 21, pt.bg = cols, bty = 'n')
  # mtext(i)
  mtext('Effort (hours / vessel)', 2, 3.5)

}
dev.off()
