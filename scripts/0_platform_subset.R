### subset platform data for uniform usage across analyses
library(NISTunits)
library(lubridate)

setwd("C:/Users/brendan.turley/Documents/data/boem")
### data downloaded from https://www.data.boem.gov/Platform/PlatformStructures/Default.aspx
platforms <- read.csv('PlatStruc2.csv')

platforms$Install.Date <- mdy(platforms$Install.Date)
platforms$Removal.Date <- mdy(platforms$Removal.Date)
platforms$Site.Clear.Date <- mdy(platforms$Site.Clear.Date)

platforms <- platforms[-which(is.na(platforms$Install.Date)),] |>
  subset(Water.Depth..feet.<=2500)

keep <- which(year(platforms$Removal.Date)>=2014 |
                is.na(platforms$Removal.Date))

# keep <- which(year(platforms$Removal.Date)>=2014 |
#                 is.na(platforms$Removal.Date))

platforms_keep <- platforms[keep,]

plot(platforms$Longitude, platforms$Latitude, asp = 1)
points(platforms_keep$Longitude, platforms_keep$Latitude, col = 2)

### search for duplication
platforms_keep <- platforms_keep[order(platforms_keep$Longitude,platforms_keep$Latitude),]
dups1 <- which(duplicated(platforms_keep$Longitude,platforms_keep$Latitude))
dups2 <- which(duplicated(platforms_keep$Longitude,platforms_keep$Latitude,fromLast = T))
dups_sort <- rep(NA,length(dups1)*2)
dups_sort[seq(1, length(dups_sort), by = 2)] <- dups1
dups_sort[seq(2, length(dups_sort), by = 2)] <- dups2
plat_dups <- platforms_keep[dups_sort,]
plot(plat_dups$Longitude,plat_dups$Latitude,asp=1)
platforms_keep <- platforms_keep[-dups2,]

platforms_keep$water_depth_m <- NISTftTOmeter(platforms_keep$Water.Depth..feet.)

setwd("~/R_projects/US_Shrimp_ONG/data")
saveRDS(platforms_keep, 'gomx_platforms.rds')
