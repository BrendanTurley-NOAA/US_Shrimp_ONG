### new buffer metric

rm(list=ls())
gc()
library(cmocean)
library(data.table)
library(fields)
library(lubridate)
library(ncdf4)
library(scales)
library(sf)
library(terra)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')

gom <- aggregate(gom, c('StatZone'))
gom <- gom[which(gom$StatZone>12),]

png('mini_SZ_plot.png', width = 5, height = 5, units = 'in', res = 300)
plot(gom, col = cmocean('thermal')(9))
text(vect(st_centroid(st_as_sf(gom))),gom$StatZone, halo = T, col = 'gray20')
dev.off()
