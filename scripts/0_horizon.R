### calculate the distance to the horizon and distance to something seen at the horizon
# sources:
# https://sites.math.washington.edu/~conroy/m120-general/horizon.pdf
# https://astronavigationdemystified.com/the-disappearing-lighthouse-distance-to-the-horizon/

h1 <- NISTunits::NISTftTOmeter(15)/1000 # height of observer in km
h2 <- NISTunits::NISTftTOmeter(150)/1000 # height of rig in km; average rig height 150-200 feet; from chatGPT
rE <- 6367.45 # mean radius of earth in km
# rE <- 6371/1000 # mean radius of earth in km

### to the horizon
hor <- sqrt(2*rE*h1)
### distance to something that only the top can be seen
hor_top <- sqrt(2*rE*h1) + sqrt(2*rE*h2)
### but that is the very tip; assume that to really be visible half that needs to be seen
hor_half <- sqrt(2*rE*h1) + sqrt(2*rE*h2/2)
### or the top 2/3 need to be seen
hor_twothirds <- sqrt(2*rE*h1) + sqrt(2*rE*h2*1/3)

setwd("~/R_projects/US_Shrimp_ONG/data")
save(hor, hor_half, hor_top, hor_twothirds, file = 'horizon.RData')


### gut check
hs <- h2*seq(0,1,.05)
# hs <- h2*seq(1,0,-.05)
lths <- hor + sqrt(2*rE*hs)

plot(lths,1-seq(0,1,.05))
abline(h = c(2/3,1/2), lty = 5, col = c(2,4))

plot(lths,seq(0,1,.05))
abline(h = c(1/3,1/2), lty = 5, col = c(2,4))

plot(lths,seq(1,0,-.05))
plot(lths,(h2-hs)*1000)
abline(h = c(2/3,1/2)*h2*1000, lty = 5, col = c(2,4))
