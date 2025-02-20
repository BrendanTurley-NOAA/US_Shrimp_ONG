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

# setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
# gom <- vect('GOM_2500ft.shp')
# gom <- aggregate(gom,'StatZone')
# utm_15n <- 'EPSG:26715'
# gom_utm <- project(gom, utm_15n)
# gom_utm <- gom_utm[which(gom_utm$StatZone>16), ]
# gom_sf <- st_as_sf(gom_utm)

utm_15n <- 'EPSG:26715'

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/BOEM_GOMX_WEAS")
rwe <- vect('Export_Output.shp') |>
  project(utm_15n) |>
  st_as_sf()

hectate <- vect('Unsolicited.shp') |>
  project(utm_15n) |>
  st_as_sf()


leased <- cbind(data.frame(id = 1:3, name = c('RWE', 'WEA C', 'WEA D')),
                c(rwe$geometry, hectate$geometry)) |>
  st_as_sf()



setwd("~/R_projects/shrimp-hypoxia-wind/data")

if(!file.exists('leased_summaries.RData')){

  setwd("~/R_projects/shrimp-hypoxia-wind/data")
  yrs <- 2015:2019
  wea_mth <- expand.grid(name = c('RWE', 'WEA C', 'WEA D'), month = 1:12)
  wea_mth_sum <- matrix(NA, 3*12*length(yrs), 5)
  m <- 1
  n <- 0
  pb <- txtProgressBar(min = 0, max = length(yrs), initial = 0, style = 3)
  for(i in 1:length(yrs)){
    # i <- 1
    yr <- yrs[i]
    load(paste0('effort_',yr,'_subset.RData'))
    # st_crs(eff_keep)
    eff_keep <- st_transform(eff_keep, utm_15n)

    eff_i <- st_join(eff_keep, leased,
                     join = st_intersects,
                     left = F)

    ### aggregates
    ### monthly
    mth_eff <- setNames(aggregate(hours_eff ~ name + month,
                                  data = eff_i,
                                  sum, na.rm = T),
                        c('name', 'month', 'effort'))
    mth_ves <- setNames(aggregate(VSBN ~ name + month,
                                  data = eff_i,
                                  function(x) length(unique(x))),
                        c('name', 'month', 'vessel'))
    mth_eff_ves <- merge(wea_mth, mth_eff, by = c('name', 'month'), all = T) |>
      merge(mth_ves, by = c('name', 'month'), all = T)
    mth_eff_ves[is.na(mth_eff_ves)] <- 0

    n <- n + nrow(mth_eff_ves)

    wea_mth_sum[m:n, ] <- cbind(yr, as.matrix(mth_eff_ves))

    m <- n + 1

    rm(eff_keep, eff_i, mth_eff, mth_ves, mth_eff_ves)

    setTxtProgressBar(pb, i)
  }
  close(pb)

  leased_mth_sum <- setNames(as.data.frame(wea_mth_sum), c('year', 'name','month','effort','vessels'))

  setwd("~/R_projects/shrimp-hypoxia-wind/data")
  save(leased_mth_sum, file = 'leased_summaries.RData')

} else {

  load('leased_summaries.RData')
  leased_mth_sum <- type.convert(leased_mth_sum, as.is = T)
  print('leased_summaries.RData loaded')

}
