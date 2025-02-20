rm(list=ls())
gc()
library(cmocean)
library(data.table)
library(fields)
library(lubridate)
library(ncdf4)
library(sf)
library(terra)
library(units)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
utm <- "+proj=utm +zone=15 units=km"
gom_utm <- project(gom, utm)
gom_utm <- gom_utm[which(gom_utm$StatZone>12), ]
gom_sf <- st_as_sf(gom_utm)
gc()

### gom shapfile for buffer subsetting
gom_fed_act <- gom_utm[which(gom_utm$Jurisdict=='Federal' &
                               gom_utm$Activity=='Y'),] |>
  aggregate('StatZone') |>
  st_as_sf()

gom_sz_area <- data.frame('statzone' = 13:21, 'area_km2' = st_area(gom_fed_act))

setwd("~/R_projects/US_Shrimp_ONG/data")
load(file='statzone_areas.RData')

gom_sz_area$area_km2_sh <- shrimp_area_sz


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/BOEM GOM WEA Options")
wea <- vect('BOEM_GOM_WEA_Options.shp')
wea <- wea[order(wea$WEA),]
utm_15n <- 'EPSG:26715'
wea_utm <- project(wea, utm_15n)
wea_sf <- st_as_sf(wea_utm)
wea_sf$area <- set_units(st_area(wea_sf),'km^2')
wea_area <- st_drop_geometry(wea_sf[,c(2,4)])
names(wea_area)[1] <- 'wea'

setwd("~/R_projects/US_Shrimp_ONG/data")
load('elb_wea_summaries.RData')
wea_mth_sum <- type.convert(wea_mth_sum, as.is = T)

setwd("~/R_projects/US_Shrimp_ONG/data")
load('elb_summaries.RData')

sz_mth <- merge(mth_sum, gom_sz_area, by = c('statzone'), all = T)
sz_mth$eff_area <- sz_mth$effort / sz_mth$area_km2_sh
sz_mth$ves_area <- sz_mth$vessels / sz_mth$area_km2_sh

wea_mth <- merge(wea_mth_sum, wea_area, by = c('wea'), all = T)
wea_mth$eff_area <- wea_mth$effort / wea_mth$area
wea_mth$ves_area <- wea_mth$vessels / wea_mth$area

par(mfrow=c(2,2))
boxplot(sz_mth$effort ~ sz_mth$statzone)
boxplot(wea_mth$effort ~ wea_mth$wea)
boxplot(sz_mth$vessels ~ sz_mth$statzone)
boxplot(wea_mth$vessels ~ wea_mth$wea)

par(mfrow=c(2,2))
boxplot(sz_mth$effort ~ sz_mth$month)
boxplot(wea_mth$effort ~ wea_mth$month)
boxplot(sz_mth$vessels ~ sz_mth$month)
boxplot(wea_mth$vessels ~ wea_mth$month)

par(mfrow=c(2,2))
boxplot(sz_mth$effort ~ sz_mth$year)
boxplot(wea_mth$effort ~ wea_mth$year)
boxplot(sz_mth$vessels ~ sz_mth$year)
boxplot(wea_mth$vessels ~ wea_mth$year)

par(mfrow=c(2,2))
boxplot(sz_mth$eff_area ~ sz_mth$statzone)
boxplot(wea_mth$eff_area ~ wea_mth$wea)
boxplot(sz_mth$ves_area ~ sz_mth$statzone)
boxplot(wea_mth$ves_area ~ wea_mth$wea)

par(mfrow=c(2,2))
boxplot(sz_mth$eff_area ~ sz_mth$month)
boxplot(wea_mth$eff_area ~ wea_mth$month)
boxplot(sz_mth$ves_area ~ sz_mth$month)
boxplot(wea_mth$ves_area ~ wea_mth$month)

par(mfrow=c(2,2))
boxplot(sz_mth$eff_area ~ sz_mth$year)
boxplot(wea_mth$eff_area ~ wea_mth$year)
boxplot(sz_mth$ves_area ~ sz_mth$year)
boxplot(wea_mth$ves_area ~ wea_mth$year)

weas <- LETTERS[1:14]
i=weas[1]
par(mfrow=c(4,4))
for(i in 1:14){
  with(subset(wea_mth, wea==weas[i]),
       boxplot(effort ~ month, ylim = c(0,3000)))
}
par(mfrow=c(4,4))
for(i in 1:14){
  with(subset(wea_mth, wea==weas[i]),
       boxplot(vessels ~ month))
}



BOEM_blk <- 23.04 # sqkm, size of BOEM block
### boem blocks are 9 sqmi or 3x3 mi

eff_mth_agg <- aggregate(mth_sum$effort,
                         by = list(statzone = mth_sum$statzone,
                                   month = mth_sum$month),
                         median, na.rm = T) |>
  merge(gom_sz_area, by = c('statzone'), all = T)
eff_mth_agg$eff_area <- eff_mth_agg$x / eff_mth_agg$area_km2_sh

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
ves_mth_agg$ves_area <- ves_mth_agg$x / ves_mth_agg$area_km2_sh

ves_mth_agg <- ves_mth_agg[order(ves_mth_agg$statzone, ves_mth_agg$month),]
ves_mth_agg$eff_ves <- eff_mth_agg$x / ves_mth_agg$x
ves_mth_agg$eff_ves[is.na(ves_mth_agg$eff_ves)] <- 0
ves_mth_agg$eff_ves_a <- eff_mth_agg$x / ves_mth_agg$x / ves_mth_agg$area_km2_sh
ves_mth_agg$eff_ves_a[is.na(ves_mth_agg$eff_ves_a)] <- 0


cols <- cmocean('thermal')(length(13:21))
pts <- rep(c(21,24,23),3)

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


### by month -------------------------

elb_eff_mth_agg <- aggregate(effort ~ wea + month,
                         data = wea_mth_sum,
                         median, na.rm = T) |>
  merge(wea_area, by = c('wea'), all = T)
elb_eff_mth_agg$eff_area <- elb_eff_mth_agg$effort / elb_eff_mth_agg$area
elb_eff_mth_agg <- elb_eff_mth_agg[order(elb_eff_mth_agg$wea, elb_eff_mth_agg$month),]

elb_ves_mth_agg <- aggregate(vessels ~ wea + month,
                         data = wea_mth_sum,
                         median, na.rm = T) |>
  merge(wea_area, by = c('wea'), all = T)
elb_ves_mth_agg$ves_area <- elb_ves_mth_agg$vessels / elb_ves_mth_agg$area
elb_ves_mth_agg <- elb_ves_mth_agg[order(elb_ves_mth_agg$wea, elb_ves_mth_agg$month),]
elb_ves_mth_agg$eff_ves <- elb_eff_mth_agg$effort / elb_ves_mth_agg$vessels
elb_ves_mth_agg$eff_ves[is.na(elb_ves_mth_agg$eff_ves)] <- 0
elb_ves_mth_agg$eff_ves_a <- elb_eff_mth_agg$effort / elb_ves_mth_agg$vessels / elb_ves_mth_agg$area
elb_ves_mth_agg$eff_ves_a[is.na(elb_ves_mth_agg$eff_ves_a)] <- 0

cols[4:9]
wea_col <- cols[c(9,8,8,7,7,7,7,6,6,6,5,5,5,4)]
# cols2 <- cmocean('phase')(nrow(wea_area))
cols2 <- viridis(nrow(wea_area))
pts2 <- rep(c(21,24,23,25,22),3)[-15]
wea_n <- rev(LETTERS[1:14])


setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_wea_clim2.png', width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(3,1), mar = c(3,5,1,1))

plot(elb_eff_mth_agg$month, elb_eff_mth_agg$eff_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(elb_eff_mth_agg$eff_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(elb_eff_mth_agg$eff_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median effort density (hrs / km^2)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'l', lwd = 3, col = 'gray90'))
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'l', lwd = 2,
              col = cols2[i]))
              # col = wea_col[i]))
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'p', pch = pts2[i], cex = 1.5,
              bg = cols2[i]))
              # bg = wea_col[i]))
}
legend('topleft', legend=c(wea_n),
       pch = pts2, pt.bg = cols2, bty = 'n')
       # pch = pts2, pt.bg = wea_col, bty = 'n')
# legend('topleft', legend=c(13:21), pch = pts2, pt.bg = cols2, bty = 'n')

plot(elb_ves_mth_agg$month, elb_ves_mth_agg$ves_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(elb_ves_mth_agg$ves_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(elb_ves_mth_agg$ves_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median vessel density (1/km^2)', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, ves_area, typ = 'l', lwd = 3, col = 1))
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, ves_area, typ = 'l', lwd = 2, col = cols2[i]))
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, ves_area, typ = 'p', pch = pts2[i], bg = cols2[i], cex = 1.5))
}

plot(elb_ves_mth_agg$month, elb_ves_mth_agg$eff_ves_a,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(elb_ves_mth_agg$eff_ves_a), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(elb_ves_mth_agg$eff_ves_a), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median vessel effort density (hrs / vessel / km^2)', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 1:14){
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves_a,typ = 'l', lwd = 3, col = 1))
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves_a,typ = 'l', lwd = 2, col = cols2[i]))
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, eff_ves_a,typ = 'p', pch = pts2[i], bg = cols2[i], cex = 1.5))
}

dev.off()


### combined ----------------

boxplot(sz_mth$eff_area ~ sz_mth$statzone)
boxplot(wea_mth$eff_area ~ wea_mth$wea)

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_clim_combined3.png', width = 14, height = 10, units = 'in', res = 300)
par(mfrow=c(2,2), mar = c(4,5,1,1))

plot(eff_mth_agg$month, eff_mth_agg$eff_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '',
     ylim = c(0, max(c(elb_eff_mth_agg$eff_area,eff_mth_agg$eff_area))))
rect(4.5, 0,
     7.5, max(eff_mth_agg$eff_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(eff_mth_agg$eff_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext(expression(paste('Median Effort density (hrs km'^-2,')')), 2, 3, cex = 1.5)
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

plot(elb_eff_mth_agg$month, elb_eff_mth_agg$eff_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '',
     ylim = c(0, max(c(elb_eff_mth_agg$eff_area,eff_mth_agg$eff_area))))
rect(4.5, 0,
     7.5, max(eff_mth_agg$eff_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(eff_mth_agg$eff_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
# mtext('Median effort density (hrs / km^2)', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 1:14){
  # with(subset(elb_eff_mth_agg, wea==wea_n[i]),
  #      points(month, eff_area, typ = 'l', lwd = 3, col = 1))
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'l', lwd = 2, col = cols2[i]))
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'p', pch = pts2[i], bg = cols2[i], cex = 1.5))
}
legend('topleft', legend=c(wea_n),
       pch = pts2, pt.bg = cols2, bty = 'n')
# abline(h = 2, col = 1, lty = 5,lwd = 2)

boxplot(sz_mth$eff_area ~ sz_mth$statzone,
        col = cols, lty = 1, pch = 16, staplewex = 0, lwd = 1.5, las = 1,
        ann=FALSE, ylim = range(wea_mth$eff_area))
mtext(expression(paste('Effort density (hrs km'^-2,')')), 2, 3, cex = 1.5)
mtext('StatZones', 1, 2.5, cex = 1.5)
mtext('c)',side=3, adj=0, font=2)

boxplot(wea_mth$eff_area ~ wea_mth$wea,
        col = rev(cols2), lty = 1, pch = 16, staplewex = 0, lwd = 1.5, las = 1,
        ann=FALSE)
mtext('WEAs', 1, 2.5, cex = 1.5)
mtext('d)',side=3, adj=0, font=2)

dev.off()



setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_clim_combined2.png', width = 14, height = 10, units = 'in', res = 300)
par(mfrow=c(2,2), mar = c(3,5,1,1))

plot(eff_mth_agg$month, eff_mth_agg$eff_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '',
     ylim = c(0, max(c(elb_eff_mth_agg$eff_area,eff_mth_agg$eff_area))))
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

plot(elb_eff_mth_agg$month, elb_eff_mth_agg$eff_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '',
     ylim = c(0, max(c(elb_eff_mth_agg$eff_area,eff_mth_agg$eff_area))))
rect(4.5, 0,
     7.5, max(eff_mth_agg$eff_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(eff_mth_agg$eff_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
# mtext('Median effort density (hrs / km^2)', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 1:14){
  # with(subset(elb_eff_mth_agg, wea==wea_n[i]),
  #      points(month, eff_area, typ = 'l', lwd = 3, col = 1))
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'l', lwd = 2, col = cols2[i]))
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, eff_area, typ = 'p', pch = pts2[i], bg = cols2[i], cex = 1.5))
}
# abline(h = 2, col = 1, lty = 5,lwd = 2)

plot(ves_mth_agg$month, ves_mth_agg$ves_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
     # ylim = c(0, max(c(ves_mth_agg$ves_area,elb_ves_mth_agg$ves_area))))
rect(4.5, 0,
     7.5, max(ves_mth_agg$ves_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(ves_mth_agg$ves_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median vessel density (1/km^2)', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(month, ves_area, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(month, ves_area, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(month, ves_area, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}

plot(elb_ves_mth_agg$month, elb_ves_mth_agg$ves_area,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(elb_ves_mth_agg$ves_area), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(elb_ves_mth_agg$ves_area), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
# mtext('Median vessel density (1/km^2)', 2, 3.5)
mtext('d)',side=3, adj=0, font=2)
for(i in 1:14){
  # with(subset(elb_ves_mth_agg, wea==wea_n[i]),
  #      points(month, ves_area, typ = 'l', lwd = 3, col = 1))
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, ves_area, typ = 'l', lwd = 2, col = cols2[i]))
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, ves_area, typ = 'p', pch = pts2[i], bg = cols2[i], cex = 1.5))
}
legend('topleft', legend=c(wea_n),
       pch = pts2, pt.bg = cols2, bty = 'n')
abline(h = max(ves_mth_agg$ves_area), col = 1, lty = 5,lwd = 2)

dev.off()


setwd("~/R_projects/US_Shrimp_ONG/figures")
png('elb_clim_combined.png', width = 14, height = 10, units = 'in', res = 300)
par(mfrow=c(2,2), mar = c(3,5,1,1))

plot(eff_mth_agg$month, eff_mth_agg$x,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(eff_mth_agg$x), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(eff_mth_agg$x), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median effort (hrs)', 2, 3.5)
mtext('a)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(eff_mth_agg, statzone==i),
       points(month, x, typ = 'l', lwd = 3, col = 1))
  with(subset(eff_mth_agg, statzone==i),
       points(month, x, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(eff_mth_agg, statzone==i),
       points(month, x, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}
legend('topleft', legend=c(paste(13:16,'LA',sep='-'),
                           paste(17,'LA/TX',sep='-'),
                           paste(18:21,'TX',sep='-')),
       pch = pts, pt.bg = cols, bty = 'n')
abline(h = max(elb_eff_mth_agg$effort), col = 1, lty = 5,lwd = 2)

plot(elb_eff_mth_agg$month, elb_eff_mth_agg$effort,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(elb_eff_mth_agg$effort), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(elb_eff_mth_agg$effort), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
# mtext('Median effort density (hrs / km^2)', 2, 3.5)
mtext('b)',side=3, adj=0, font=2)
for(i in 1:14){
  # with(subset(elb_eff_mth_agg, wea==wea_n[i]),
  #      points(month, eff_area, typ = 'l', lwd = 3, col = 1))
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, effort, typ = 'l', lwd = 2, col = cols2[i]))
  with(subset(elb_eff_mth_agg, wea==wea_n[i]),
       points(month, effort, typ = 'p', pch = pts2[i], bg = cols2[i], cex = 1.5))
}

plot(ves_mth_agg$month, ves_mth_agg$x,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
# ylim = c(0, max(c(ves_mth_agg$ves_area,elb_ves_mth_agg$ves_area))))
rect(4.5, 0,
     7.5, max(ves_mth_agg$x), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(ves_mth_agg$x), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
mtext('Median number of vessels', 2, 3.5)
mtext('c)',side=3, adj=0, font=2)
for(i in 13:21){
  with(subset(ves_mth_agg, statzone==i),
       points(month, x, typ = 'l', lwd = 3, col = 1))
  with(subset(ves_mth_agg, statzone==i),
       points(month, x, typ = 'l', lwd = 2, col = cols[i-12]))
  with(subset(ves_mth_agg, statzone==i),
       points(month, x, typ = 'p', pch = pts[i-12], bg = cols[i-12], cex = 1.5))
}
abline(h = max(elb_ves_mth_agg$vessels), col = 1, lty = 5,lwd = 2)

plot(elb_ves_mth_agg$month, elb_ves_mth_agg$vessels,
     typ = 'n', las = 1, xaxt = 'n',
     xlab = '', ylab = '')
rect(4.5, 0,
     7.5, max(elb_ves_mth_agg$vessels), col = 'gray90', border = NA)
rect(5.5, 0,
     6.5, max(elb_ves_mth_agg$vessels), col = 'gray70', border = NA)
axis(1, 1:12, month.abb[1:12])
grid()
# mtext('Median vessel density (1/km^2)', 2, 3.5)
mtext('d)',side=3, adj=0, font=2)
for(i in 1:14){
  # with(subset(elb_ves_mth_agg, wea==wea_n[i]),
  #      points(month, ves_area, typ = 'l', lwd = 3, col = 1))
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, vessels, typ = 'l', lwd = 2, col = cols2[i]))
  with(subset(elb_ves_mth_agg, wea==wea_n[i]),
       points(month, vessels, typ = 'p', pch = pts2[i], bg = cols2[i], cex = 1.5))
}
legend('topleft', legend=c(wea_n),
       pch = pts2, pt.bg = cols2, bty = 'n')

dev.off()

