rm(list=ls())
gc()
library(cmocean)
library(data.table)
library(fields)
library(lubridate)
library(ncdf4)
library(sf)
library(terra)
library(vioplot)


setwd("~/R_projects/US_Shrimp_ONG")
# load('./data/eff_out_1000.rdata')
# load('./data/eff_out_1000-2.rdata')
load('./data/eff_out_1000-4.rdata')
load('./data/rpts_10000-2.rdata')

setwd("~/R_projects/US_Shrimp_ONG/data")
load('horizon.RData')

rpts_out_all <- tmp
rm(tmp)
rpts_out_all <- rpts_out_all[c(1,7:14,2:6)]
names(rpts_out_all)

dist_km <- c(.5,1,2,3,5,7.5,10,12.5,15,17.5,20,25,30,40)
units(dist_km) <- 'km'

rpts_out <- matrix(NA, 14, 9)
for(i in 1:14){
  rigs_p <- with(rpts_out_all[[i]],
                 rbind(as.vector(sapply(rig_p, function(x) sum(x[2:length(x)]))),
                       as.vector(sapply(rig_p, function(x) x[1]))))
  rpts_out[i,] <- rigs_p[1,]
}


eff_out <- array(NA, c(5, 14, 9))
for(h in 2015:2019){
  # h=2017
  ind <- grep(h, names(eff_out_all))
  eff_t <- eff_out_all[ind]
  eff_t <- eff_t[c(1:2,7,10,13:14,3:6,8:9,11:12)]

  # eff_out <- matrix(NA, 14, 9)
  for(i in 1:14){
    rigs_p <- with(eff_t[[i]],
                   rbind(as.vector(sapply(rig_p, function(x) sum(x[2:length(x)]))),
                         as.vector(sapply(rig_p, function(x) x[1]))))
    eff_out[h-2014, i,] <- rigs_p[1,]
  }

}
eff_out[is.na(eff_out)] <- 0
eff_out_m <- apply(eff_out, c(2,3), median, na.rm=T)

imagePlot(as.numeric(dist_km),
          13:21,
          eff_out_m,
          col = cmocean('delta')(60))

s_i <- (eff_out_m - rpts_out)
brks <- seq(-round(max(abs(s_i),na.rm=T),digits = 1),
            round(max(abs(s_i),na.rm=T),digits = 1),
            length.out = 61)

imagePlot(as.numeric(dist_km),
          13:21,
          s_i,
          breaks = brks,
          col = cmocean('delta')(60))


cols <- cmocean('thermal')(9)
pts <- rep(c(21,24,23),3)


setwd("C:/Users/brendan.turley/Documents/R_projects/US_Shrimp_ONG/figures")
png('encounter_probs_all.png', width = 8, height = 5, units = 'in', res = 300)
par(mar=c(4,4,1.5,1))

plot(as.numeric(dist_km), rep(0, length(dist_km)),
     ylim = range(s_i), typ = 'n',
     xlab = '', ylab = '', las = 1)
rect(hor, -1, hor_twothirds, 1, col = 'gray80', border = NA)
rect(hor_twothirds, -1, hor_top, 1, col = 'gray50', border = NA)
rect(hor_top, -1, 40, 1, col = 'gray30', border = NA)
grid()
rug(x = seq(-.25,.05,.1), side = 2, ticksize = -.02)
# mtext(2017)
text(hor, -.45, '\n horizon', srt = 90, adj = 1)
text(hor_twothirds, -.45, '\n partially visible', srt = 90, adj = 1, col = 'white')
text(hor_top, -.45, '\n not visible', srt = 90, adj = 1, col = 'white')
for(i in 1:9){
  points(dist_km, s_i[,i], typ ='l', lwd = 4)
  points(dist_km, s_i[,i], typ ='l', lwd = 2, col = cols[i])
  points(dist_km, s_i[,i], typ = 'p', pch = pts[i], bg = cols[i], cex = 1.5)
}
abline(h = 0, lty = 5, lwd = 2)
legend('bottomleft', legend=c(paste(13:16,'LA',sep='-'),
                              paste(17,'LA/TX',sep='-'),
                              paste(18:21,'TX',sep='-')),
       pch = pts, pt.bg = cols, bty = 'n')
# legend('bottomleft', legend = c(13:21), pch = pts, pt.bg = cols, bty = 'n')
mtext('Rig Avoidance [-] vs. Attraction [+]', 2, 2.5)
mtext('Distance from observation (km)',1, 2.5)

dev.off()

