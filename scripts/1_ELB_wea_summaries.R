rm(list=ls())
gc()
library(cmocean)
library(data.table)
library(dplyr)
library(fields)
library(lubridate)
library(ncdf4)
library(sf)
library(terra)
library(units)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
gom <- aggregate(gom,'StatZone')
utm_15n <- 'EPSG:26715'
gom_utm <- project(gom, utm_15n)
# gom_utm <- gom_utm[which(gom_utm$StatZone>16), ]
# gom_sf <- st_as_sf(gom_utm)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/BOEM GOM WEA Options")
wea <- vect('BOEM_GOM_WEA_Options.shp')
wea <- wea[order(wea$WEA),]
lonlat <- 'EPSG:4326'
wea_ll <- project(wea, lonlat)
# utm <- "+proj=utm +zone=15 units=km" #depreciated
utm_15n <- 'EPSG:26715'
wea_utm <- project(wea, utm_15n)
wea_sf <- st_as_sf(wea_utm)
wea_sf$area <- set_units(st_area(wea_sf),'km^2')
wea_area <- st_drop_geometry(wea_sf[,c(2,4)])
names(wea_area)[1] <- 'wea'
# plot(wea_utm)

gom <-  project(gom,crs(wea))
# gom <-  project(gom,crs(wea_ll))

sf_use_s2(FALSE)

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('mini_wea_plot.png', width = 5, height = 5, units = 'in', res = 300)
# plot(gom_ll)
plot(wea_ll, col = rev(cmocean('haline')(14)),
     xlim = c(-97.5, -92),
     ylim = c(26,30))
# plot(wea, col = rev(cmocean('haline')(14)))
# plot(st_geometry(st_centroid(st_as_sf(wea))), pch = 21, col = 'gray', bg = 1, add = TRUE, cex = 3)
text(vect(st_centroid(st_as_sf(wea_ll))),wea_ll$WEA, halo = T, col = 1)
plot(project(gom, lonlat), add=T)
text(project(gom, lonlat), gom$StatZone, halo = T, col = 1)
dev.off()

sf_use_s2(TRUE)

setwd("~/R_projects/US_Shrimp_ONG/data")

if(!file.exists('elb_wea_summaries.RData')){

  setwd("~/R_projects/US_Shrimp_ONG/data")
  yrs <- 2015:2019
  wea_mth <- expand.grid(WEA = LETTERS[1:14], month = 1:12)
  wea_wea_mth_sum <- matrix(NA, length(LETTERS[1:14])*12*length(yrs), 5)
  # sz_wea_mth <- expand.grid(WEA = LETTERS[1:14], StatZone = 16:21, month = 1:12)
  sz_wea_mth_sum <- matrix(NA, length(LETTERS[1:14])*12*length(yrs)*length(16:21), 6)
  m <- o <- 1
  n <- p <- 0
  pb <- txtProgressBar(min = 0, max = length(yrs), initial = 0, style = 3)
  for(i in 1:length(yrs)){
    # i <- 1
    yr <- yrs[i]
    load(paste0('effort_',yr,'_subset.RData'))
    # st_crs(eff_keep)
    eff_keep <- st_transform(eff_keep, utm_15n)

    eff_i <- st_join(eff_keep, wea_sf,
                     join = st_intersects,
                     left = F)

    ### aggregates
    ### monthly
    mth_eff <- setNames(aggregate(hours_eff ~ WEA + month,
                                  data = eff_i,
                                  sum, na.rm = T),
                        c('WEA', 'month', 'effort'))
    mth_ves <- setNames(aggregate(VSBN ~ WEA + month,
                                  data = eff_i,
                                  function(x) length(unique(x))),
                        c('WEA', 'month', 'vessel'))
    mth_eff_ves <- merge(wea_mth, mth_eff, by = c('WEA', 'month'), all = T) |>
      merge(mth_ves, by = c('WEA', 'month'), all = T)
    mth_eff_ves[is.na(mth_eff_ves)] <- 0

    n <- n + nrow(mth_eff_ves)

    wea_mth_sum[m:n, ] <- cbind(yr, as.matrix(mth_eff_ves))

    m <- n + 1

    ### month x statzone
    sz_mth_eff <- aggregate(hours_eff ~ WEA + StatZone + month,
                         data = eff_i,
                         sum, na.rm = T)
    sz_mth_ves <- aggregate(VSBN ~ WEA + StatZone + month,
                                  data = eff_i,
                                  function(x) length(unique(x)))

    sz_mth_eff_ves <- merge(sz_mth_eff, sz_mth_ves, by = c('StatZone', 'WEA', 'month'), all = T)

    p <- p + nrow(sz_mth_eff_ves)

    sz_wea_mth_sum[o:p, ] <- cbind(yr, as.matrix(sz_mth_eff_ves))

    o <- p + 1

    rm(eff_keep, eff_i, mth_eff, mth_ves, mth_eff_ves,
       sz_mth_eff, sz_mth_ves, sz_mth_eff_ves)

    setTxtProgressBar(pb, i)
  }
  close(pb)

  wea_mth_sum <- setNames(as.data.frame(wea_mth_sum), c('year', 'wea','month','effort','vessels'))
  sz_wea_mth_sum <- setNames(as.data.frame(sz_wea_mth_sum), c('year', 'statzone', 'wea','month','effort','vessels'))

  setwd("~/R_projects/US_Shrimp_ONG/data")
  save(wea_mth_sum, sz_wea_mth_sum, file = 'elb_wea_summaries.RData')

} else {

  load('elb_wea_summaries.RData')
  wea_mth_sum <- type.convert(wea_mth_sum, as.is = T)
  sz_wea_mth_sum <- type.convert(sz_wea_mth_sum, as.is = T)
  print('elb_wea_summaries.RData loaded')

}


### by month -------------------------

eff_mth_agg <- aggregate(effort ~ wea + month,
                         data = wea_mth_sum,
                         median, na.rm = T) |>
  merge(wea_area, by = c('wea'), all = T)
eff_mth_agg$eff_area <- eff_mth_agg$effort / eff_mth_agg$area
eff_mth_agg <- eff_mth_agg[order(eff_mth_agg$wea, eff_mth_agg$month),]

ves_mth_agg <- aggregate(vessels ~ wea + month,
                         data = wea_mth_sum,
                         median, na.rm = T) |>
  merge(wea_area, by = c('wea'), all = T)
ves_mth_agg$ves_area <- ves_mth_agg$vessels / ves_mth_agg$area
ves_mth_agg <- ves_mth_agg[order(ves_mth_agg$wea, ves_mth_agg$month),]
ves_mth_agg$eff_ves <- eff_mth_agg$effort / ves_mth_agg$vessels
ves_mth_agg$eff_ves[is.na(ves_mth_agg$eff_ves)] <- 0
ves_mth_agg$eff_ves_a <- eff_mth_agg$effort / ves_mth_agg$vessels / ves_mth_agg$area
ves_mth_agg$eff_ves_a[is.na(ves_mth_agg$eff_ves_a)] <- 0


# cols <- cmocean('phase')(nrow(wea_area))
cols <- viridis(nrow(wea_area))
pts <- rep(c(21,24,23,25,22),3)[-15]
wea_n <- LETTERS[1:14]

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_wea_clim.png', width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(3,1), mar = c(3,5,1,1))

plot(eff_mth_agg$month, eff_mth_agg$effort,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(eff_mth_agg$effort), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(eff_mth_agg$effort), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median effort (hours)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(eff_mth_agg, wea==wea_n[i]),
       points(month, effort, typ = 'l', lwd = 3, col = 1))
  with(subset(eff_mth_agg, wea==wea_n[i]),
       points(month, effort, typ = 'l', lwd = 2, col = cols[i]))
  with(subset(eff_mth_agg, wea==wea_n[i]),
       points(month, effort, typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}
legend('topleft', legend=c(wea_n),
       pch = pts, pt.bg = cols, bty = 'n')
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')

plot(ves_mth_agg$month, ves_mth_agg$vessels,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(ves_mth_agg$vessels), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(ves_mth_agg$vessels), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median number of vessels', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, vessels, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, vessels, typ = 'l', lwd = 2, col = cols[i]))
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, vessels, typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
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
for(i in 1:14){
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves,typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves,typ = 'l', lwd = 2, col = cols[i]))
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves,typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}

dev.off()


setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_wea_clim2.png', width = 8, height = 10, units = 'in', res = 300)
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
for(i in 1:14){
  with(subset(eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'l', lwd = 3, col = 1))
  with(subset(eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'l', lwd = 2, col = cols[i]))
  with(subset(eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}
legend('topleft', legend=c(wea_n),
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
for(i in 1:14){
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, ves_area, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, ves_area, typ = 'l', lwd = 2, col = cols[i]))
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, ves_area, typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
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
for(i in 1:14){
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves_a,typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves_a,typ = 'l', lwd = 2, col = cols[i]))
  with(subset(ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves_a,typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}

dev.off()


### by year -------------------------

eff_yr_agg <- aggregate(effort ~ wea + year,
                         data = wea_mth_sum,
                         sum, na.rm = T) |>
  merge(wea_area, by = c('wea'), all = T)
eff_yr_agg$eff_area <- eff_yr_agg$effort / eff_yr_agg$area
eff_yr_agg <- eff_yr_agg[order(eff_yr_agg$wea, eff_yr_agg$year),]

ves_yr_agg <- aggregate(vessels ~ wea + year,
                         data = wea_mth_sum,
                         sum, na.rm = T) |>
  merge(wea_area, by = c('wea'), all = T)
ves_yr_agg$ves_area <- ves_yr_agg$vessels / ves_yr_agg$area
ves_yr_agg <- ves_yr_agg[order(ves_yr_agg$wea, ves_yr_agg$year),]
ves_yr_agg$eff_ves <- eff_yr_agg$effort / ves_yr_agg$vessels
ves_yr_agg$eff_ves[is.na(ves_yr_agg$eff_ves)] <- 0
ves_yr_agg$eff_ves_a <- eff_yr_agg$effort / ves_yr_agg$vessels / ves_yr_agg$area
ves_yr_agg$eff_ves_a[is.na(ves_yr_agg$eff_ves_a)] <- 0


# cols <- cmocean('phase')(nrow(wea_area))
cols <- viridis(nrow(wea_area))
pts <- rep(c(21,24,23,25,22),3)[-15]
wea_n <- LETTERS[1:14]

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_wea_ann.png', width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(3,1), mar = c(3,5,1,1))

plot(eff_yr_agg$year, eff_yr_agg$effort,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median effort (hours)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(eff_yr_agg, wea==wea_n[i]),
       points(year, effort, typ = 'l', lwd = 3, col = 1))
  with(subset(eff_yr_agg, wea==wea_n[i]),
       points(year, effort, typ = 'l', lwd = 2, col = cols[i]))
  with(subset(eff_yr_agg, wea==wea_n[i]),
       points(year, effort, typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}
legend('topleft', legend=c(wea_n),
       pch = pts, pt.bg = cols, bty = 'n')
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')

plot(ves_yr_agg$year, ves_yr_agg$vessels,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median number of vessels', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, vessels, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, vessels, typ = 'l', lwd = 2, col = cols[i]))
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, vessels, typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}

plot(ves_yr_agg$year, ves_yr_agg$eff_ves,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median effort (hours / vessel)', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, eff_ves,typ = 'l', lwd = 3, col = 1))
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, eff_ves,typ = 'l', lwd = 2, col = cols[i]))
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, eff_ves,typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}

dev.off()


setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_wea_ann2.png', width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(3,1), mar = c(3,5,1,1))

plot(eff_yr_agg$year, eff_yr_agg$eff_area,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median effort density (hrs / km^2)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(eff_yr_agg, wea==wea_n[i]),
       points(year, eff_area, typ = 'l', lwd = 3, col = 1))
  with(subset(eff_yr_agg, wea==wea_n[i]),
       points(year, eff_area, typ = 'l', lwd = 2, col = cols[i]))
  with(subset(eff_yr_agg, wea==wea_n[i]),
       points(year, eff_area, typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}
legend('topleft', legend=c(wea_n),
       pch = pts, pt.bg = cols, bty = 'n')
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')

plot(ves_yr_agg$year, ves_yr_agg$ves_area,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median vessel density (1/km^2)', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, ves_area, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, ves_area, typ = 'l', lwd = 2, col = cols[i]))
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, ves_area, typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}

plot(ves_yr_agg$year, ves_yr_agg$eff_ves_a,
     typ = 'n', las = 1,
     xlab = '', ylab = '')
grid()
mtext('Median vessel effort density (hrs / vessel / km^2)', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, eff_ves_a,typ = 'l', lwd = 3, col = 1))
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, eff_ves_a,typ = 'l', lwd = 2, col = cols[i]))
  with(subset(ves_yr_agg, wea==wea_n[i]),
       points(year, eff_ves_a,typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5))
}

dev.off()
