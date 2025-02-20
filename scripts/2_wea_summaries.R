### WEAs effort
# 1 - total effort; tot per year
# 2 - effort as percentage per statzone across LA-TX shelf
# 3 - effort in lease areas and unsolicated WEA

rm(list=ls())
gc()
library(terra)

setwd("~/R_projects/US_Shrimp_ONG/data")
load('elb_summaries.RData')
sz_mth_sum <- mth_sum
rm(mth_sum, wk_sum)

load('elb_wea_summaries.RData')
wea_mth_sum <- type.convert(wea_mth_sum, as.is = T)
sz_wea_mth_sum <- type.convert(sz_wea_mth_sum, as.is = T)

load('leased_summaries.RData')
leased_mth_sum <- type.convert(leased_mth_sum, as.is = T)


sz_yr_sum <- aggregate(effort ~ statzone + year, data = sz_mth_sum, sum)
aggregate(effort ~ statzone, data = sz_yr_sum, median)
aggregate(effort ~ statzone, data = sz_yr_sum, sum)

sz_yr_sum_ves <- aggregate(vessels ~ statzone + year, data = sz_mth_sum, sum)
aggregate(vessels ~ statzone, data = sz_yr_sum_ves, median)
aggregate(vessels ~ statzone, data = sz_yr_sum_ves, sum)

wea_yr_sum <- aggregate(effort ~ wea + year, data = wea_mth_sum, sum)
aggregate(effort ~ wea, data = wea_yr_sum, median)
aggregate(effort ~ wea, data = wea_yr_sum, sum)

wea_yr_sum_ves <- aggregate(vessels ~ wea + year, data = wea_mth_sum, sum)
aggregate(vessels ~ wea, data = wea_yr_sum_ves, median)
aggregate(vessels ~ wea, data = wea_yr_sum_ves, sum)


with(subset(sz_mth_sum, statzone<17), boxplot(effort ~ month, at = seq(1,24,2), col = 'purple'))
with(subset(sz_mth_sum, statzone>=17), boxplot(effort ~ month, add = T, at = seq(2,24,2), col = 'gold'))

with(subset(sz_mth_sum, statzone<17), boxplot(vessels ~ month, at = seq(1,24,2), col = 'purple'))
with(subset(sz_mth_sum, statzone>=17), boxplot(vessels ~ month, add = T, at = seq(2,24,2), col = 'gold'))


# 1 - total effort
sum(wea_mth_sum$effort)
sum(wea_mth_sum$effort) / sum(sz_mth_sum$effort)

boxplot(wea_yr_sum$effort~wea_yr_sum$year)
boxplot(wea_yr_sum$effort~wea_yr_sum$wea)
boxplot(wea_yr_sum_ves$vessels~wea_yr_sum_ves$year)
boxplot(wea_yr_sum_ves$vessels~wea_yr_sum_ves$wea)


# 2 - effort as percentage per statzone across LA-TX shelf
yr_agg <- merge(aggregate(effort ~ statzone, data = sz_wea_mth_sum, sum),
      aggregate(effort ~ statzone, data = subset(sz_mth_sum, statzone>15), sum),
      by = c('statzone'), all = T)
yr_agg$percent <- yr_agg$effort.x / yr_agg$effort.y

summary(yr_agg$percent)
quantile(yr_agg$percent, seq(0,1,.25))


# 3 - effort in lease areas and unsolicated WEA
sum(leased_mth_sum$effort)
sum(leased_mth_sum$effort) / sum(sz_mth_sum$effort)

per_area <- aggregate(effort ~ name, data = leased_mth_sum, sum)
per_area$percent <- per_area$effort / sum(sz_mth_sum$effort)

aggregate(effort ~ year, data = leased_mth_sum, sum)
