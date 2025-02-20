rm(list=ls())
gc()
library(data.table)
library(fields)
library(lubridate)
library(ncdf4)
library(sf)
library(terra)
library(cmocean)

setwd("~/R_projects/US_Shrimp_ONG/data")
load('horizon.RData')


setwd("~/R_projects/US_Shrimp_ONG/data")

yrs <- 2015:2019
for(i in 1:length(yrs)){
  yr <- yrs[i]
  tmp <- read.csv(paste0(yr,'_eff_ves_dist4.csv'))
  assign(paste0('eff_',yr), tmp)
}

effort <- rbind(eff_2015,eff_2016,eff_2017,eff_2018,eff_2019)
effort$year <- as.numeric(sort(paste(rep(yrs,126))))

eff_pr_agg <- aggregate(cbind(effort$effort_pr,effort$effort_hrs),
                        by = list(effort$dist_km, effort$statzone),
                        median, na.rm=T) |>
  setNames(c('dist_km','statzone','effort_pr','effort_hrs'))

ves_pr_agg <- aggregate(cbind(effort$vessel_pr,effort$vessel_nu),
                        by = list(effort$dist_km, effort$statzone),
                        median, na.rm=T) |>
  setNames(c('dist_km','statzone','vessel_pr','vessel_nu'))


pts <- rep(c(21,24,23),3)
cols <- cmocean('thermal')(length(pts))



# png('buffers_allyears.png', width = 13, height = 9, units = 'in', res = 300)
# par(mar=c(4,4,1,1))
# layout(matrix(c(1,0,2,3),ncol=2))

setwd("~/R_projects/US_Shrimp_ONG/figures")

png('buffers_allyears.png', width = 8, height = 11, units = 'in', res = 300)
par(mfrow=c(3,1),mar=c(4,4,1,1))

plot(0,0,xlim=c(0,40),ylim=c(0,.45), typ = 'n',
     xlab='', ylab='', las = 1)
rect(hor, 0, hor_twothirds, 1e5, col = 'gray80', border = NA)
rect(hor_twothirds, 0, hor_top, 1e5, col = 'gray50', border = NA)
rect(hor_top, 0, 40, 1e5, col = 'gray30', border = NA)
grid()
text(hor, .45, '\n horizon', srt = 90, adj = 1)
text(hor_twothirds, .45, '\n partially visible', srt = 90, adj = 1, col = 'white')
text(hor_top, .45, '\n not visible', srt = 90, adj = 1, col = 'white')
for(i in 13:21){
  tmp <- subset(eff_pr_agg, statzone==i)
  points(tmp$dist_km,
         c(tmp$effort_hrs[1], diff(tmp$effort_hrs))/sum(c(tmp$effort_hrs[1], diff(tmp$effort_hrs))),
         typ = 'l',  col = cols[i-12], lwd = 2)
  points(tmp$dist_km,
         c(tmp$effort_hrs[1], diff(tmp$effort_hrs))/sum(c(tmp$effort_hrs[1], diff(tmp$effort_hrs))),
         typ = 'p',  pch = pts[i-12], bg = cols[i-12], cex = 1.5)
}
legend('topleft', legend=c(paste(13:16,'LA',sep='-'),
                           paste(17,'LA/TX',sep='-'),
                           paste(18:21,'TX',sep='-')),
       pch = pts, pt.bg = cols, bty = 'n')
mtext('Proportion of effort', 2, 2.5)
mtext('a)',side=3, adj=0, font=2)

plot(eff_pr_agg$dist_km, eff_pr_agg$effort_pr, typ = 'n',
     xlab='', ylab='', las = 1)
# rect(0, 0, hor, 1, col = 'gray80', border = NA)
rect(hor, 0, hor_twothirds, 1, col = 'gray80', border = NA)
rect(hor_twothirds, 0, hor_top, 1, col = 'gray50', border = NA)
rect(hor_top, 0, 40, 1, col = 'gray30', border = NA)
grid()
for(j in 13:21){
  with(subset(eff_pr_agg, statzone==j),{
    points(dist_km, effort_pr,
           typ = 'l', pch = pts[j-12], col = cols[j-12], lwd = 2)
    points(dist_km, effort_pr,
           typ = 'p', pch = pts[j-12], bg = cols[j-12], cex = 1.5)
  }
  )
}
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')
mtext('Cumulative proportion of effort', 2, 2.5)
mtext('b)',side=3, adj=0, font=2)

plot(ves_pr_agg$dist_km, ves_pr_agg$vessel_pr, typ = 'n',
     xlab='', ylab='', las = 1)
# rect(0, 0, hor, 1, col = 'gray80', border = NA)
rect(hor, 0, hor_twothirds, 1, col = 'gray80', border = NA)
rect(hor_twothirds, 0, hor_top, 1, col = 'gray50', border = NA)
rect(hor_top, 0, 40, 1, col = 'gray30', border = NA)
grid()
text(hor, .45, '\n horizon', srt = 90, adj = 1)
text(hor_twothirds, .8, '\n partially visible', srt = 90, adj = 1, col = 'white')
text(hor_top, .8, '\n not visible', srt = 90, adj = 1, col = 'white')
for(j in 13:21){
  with(subset(ves_pr_agg, statzone==j),{
    points(dist_km, vessel_pr,
           typ = 'l', pch = pts[j-12], col = cols[j-12], lwd = 2)
    points(dist_km, vessel_pr,
           typ = 'p', pch = pts[j-12], bg = cols[j-12], cex = 1.5)
  }
  )
}
mtext('Cumulative proportion of vessels', 2, 2.5)
mtext('Distance from rig (km)',1, 2.5)
mtext('c)',side=3, adj=0, font=2)

dev.off()


png('buffers_allyears_alt.png', width = 10, height = 11, units = 'in',
    res = 300, pointsize = 15)
par(mfrow=c(2,1),mar=c(4,4,1,1))

plot(eff_pr_agg$dist_km, eff_pr_agg$effort_pr, typ = 'n',
     xlab='', ylab='', las = 1)
# rect(0, 0, hor, 1, col = 'gray80', border = NA)
rect(hor, 0, hor_twothirds, 1, col = 'gray80', border = NA)
rect(hor_twothirds, 0, hor_top, 1, col = 'gray50', border = NA)
rect(hor_top, 0, 40, 1, col = 'gray30', border = NA)
grid()
for(j in 13:21){
  with(subset(eff_pr_agg, statzone==j),{
    points(dist_km, effort_pr,
           typ = 'l', pch = pts[j-12], col = cols[j-12], lwd = 2)
    points(dist_km, effort_pr,
           typ = 'p', pch = pts[j-12], bg = cols[j-12], cex = 1.5)
  }
  )
}
# legend('topleft', legend=c(13:21), pch = pts, pt.bg = cols, bty = 'n')
mtext('Cumulative proportion of effort', 2, 2.5, cex = 1.25)
mtext('a)',side=3, adj=0, font=2)

legend('topleft', legend=c(paste(13:16,'LA',sep='-'),
                           paste(17,'LA/TX',sep='-'),
                           paste(18:21,'TX',sep='-')),
       pch = pts, pt.bg = cols, bty = 'n')


plot(ves_pr_agg$dist_km, ves_pr_agg$vessel_pr, typ = 'n',
     xlab='', ylab='', las = 1)
# rect(0, 0, hor, 1, col = 'gray80', border = NA)
rect(hor, 0, hor_twothirds, 1, col = 'gray80', border = NA)
rect(hor_twothirds, 0, hor_top, 1, col = 'gray50', border = NA)
rect(hor_top, 0, 40, 1, col = 'gray30', border = NA)
grid()
text(hor, .45, '\n horizon', srt = 90, adj = 1)
text(hor_twothirds, .8, '\n partially visible', srt = 90, adj = 1, col = 'white')
text(hor_top, .8, '\n not visible', srt = 90, adj = 1, col = 'white')
for(j in 13:21){
  with(subset(ves_pr_agg, statzone==j),{
    points(dist_km, vessel_pr,
           typ = 'l', pch = pts[j-12], col = cols[j-12], lwd = 2)
    points(dist_km, vessel_pr,
           typ = 'p', pch = pts[j-12], bg = cols[j-12], cex = 1.5)
  }
  )
}
mtext('Cumulative proportion of vessels', 2, 2.5, cex = 1.25)
mtext('Distance from rig (km)',1, 2.5, cex = 1.25)
mtext('b)',side=3, adj=0, font=2)

dev.off()



#### multi-year plots ----------------------------
pts <- c(rep(16,5), rep(17,4))
png('effort_buffers_years.png', width = 12, height = 6, units = 'in', res = 300)
par(mfrow=c(2,3), mar=c(4,4,1.5,1))
for(i in yrs){
  tmp <- subset(effort, year==i)

  plot(tmp$dist_km, tmp$effort_pr, typ = 'n',
       xlab='', ylab='')
  grid()
  mtext(i)
  for(j in 13:21){
    with(subset(tmp, statzone==j),
         points(dist_km, effort_pr, typ = 'o', pch = pts[j-12], col = j-12))
  }
  if(i==2015){
    legend('bottomright', legend=c(13:21), cex = .7, pch = pts, col = 1:9, bty = 'n')
  }
  if(i==2015 | i==2018){
    mtext('Proportion of effort', 2, 2.5)
  }
  if(i>=2017){
    mtext('Distance from rig (km)',1, 2.5)
  }
}
dev.off()


png('vessels_buffers_year.png', width = 12, height = 6, units = 'in', res = 300)
par(mfrow=c(2,3), mar=c(4,4,1.5,1))
for(i in yrs){
  tmp <- subset(effort, year==i)

  plot(tmp$dist_km, tmp$vessel_pr, typ = 'n',
       xlab='', ylab='', ylim = c(0,1))
  grid()
  mtext(i)
  for(j in 13:21){
    with(subset(tmp, statzone==j),
         points(dist_km, vessel_pr, typ = 'o', pch = pts[j-12], col = j-12))
  }
  if(i==2015){
    legend('bottomright', legend=c(13:21), cex = .7, pch = pts, col = 1:9, bty = 'n')
  }
  if(i==2015 | i==2018){
    mtext('Proportion of vessels', 2, 2.5)
  }
  if(i>=2017){
    mtext('Distance from rig (km)',1, 2.5)
  }
}
dev.off()
