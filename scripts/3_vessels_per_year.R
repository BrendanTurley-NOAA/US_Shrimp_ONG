
yrs <- 2015:2019

ves_ls <- list()
for(i in 1:5){
  yr <- yrs[i]

  setwd("~/R_projects/US_Shrimp_ONG/data")
  load(paste0('effort_',yr,'_subset.RData'))
  eff_keep <- subset(eff_keep, Jurisdict=='Federal')

 ves_ls[[i]] <- sort(unique(eff_keep$VSBN))

}

ves_15 <- data.frame(year=2015,VSBN=ves_ls[[1]])
ves_16 <- data.frame(year=2016,VSBN=ves_ls[[2]])
ves_17 <- data.frame(year=2017,VSBN=ves_ls[[3]])
ves_18 <- data.frame(year=2018,VSBN=ves_ls[[4]])
ves_19 <- data.frame(year=2019,VSBN=ves_ls[[5]])

ves_year <- merge(ves_15, ves_16, by = 'VSBN', all=T) |>
  merge(ves_17, by = 'VSBN', all=T) |>
  merge(ves_18, by = 'VSBN', all=T) |>
  merge(ves_19, by = 'VSBN', all=T)

ves_sums <- rowSums(ves_year[,-1])
length(which(is.na(ves_sums)))

apply(ves_year[,-1], 2, function(x) length(which(!is.na(x))))
apply(ves_year[,-1], 2, function(x) length(which(is.na(x))))

ves_year2 <- as.matrix(ves_year[,-1])
ves_year2[which(ves_year2>0)] <- 1
ves_year2[which(is.na(ves_year2))] <- 0

h1 <- hist(rowSums(ves_year2),breaks=seq(.5,5.5,1))
h1$density

setwd("~/R_projects/US_Shrimp_ONG/figures")
png('vessel_pro_year.png', width = 4, height = 4, units = 'in', res = 300)
barplot(h1$density,
        names = 1:5, las = 1,
        xlab = 'Number of years fished',
        ylab = 'Proportion of vessels')
dev.off()
