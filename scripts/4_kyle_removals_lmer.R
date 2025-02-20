
rm(list=ls())
gc()
library(lattice)
# library(lme4)
library(lmerTest)
library(vioplot)

load('./data/removals_prep_v2.RData')


### 1 km model
mod_1km <- lmerTest::lmer(effort ~ yr_prepost + offset(tot_sz_eff) + (1 | id_name),
                          data = dat_1km, REML = F)

mod_1km_reduced <- lmerTest::lmer(effort ~ 1 + offset(tot_sz_eff) + (1 | id_name),
                                  data = dat_1km, REML = F)

mod_1km <- lmerTest::lmer(effort_s ~ yr_prepost + offset(tot_sz_eff_s) + (1 | id_name),
                          data = dat_1km, REML = F)

mod_1km_reduced <- lmerTest::lmer(effort_s ~ 1 + offset(tot_sz_eff_s) + (1 | id_name),
                                  data = dat_1km, REML = F)

anova(mod_1km, mod_1km_reduced)

anova(mod_1km)
rand(mod_1km)

rePCA(mod_1km)
vcov(mod_1km)
sigma(mod_1km)
deviance(mod_1km)
deviance(mod_1km_reduced)

summary(mod_1km)
summary(mod_1km_reduced)

qqnorm(residuals(mod_1km))
qqline(residuals(mod_1km))

plot(mod_1km)
# plot(fitted(mod_1km), resid(mod_1km))
plot(resid(mod_1km))
hist(resid(mod_1km))

confint(mod_1km, oldNames = F)

print(dotplot(ranef(mod_1km, condVar = TRUE)))


### 7.5 km model
mod_7km <- lmerTest::lmer(effort ~ yr_prepost + offset(tot_sz_eff) + (1 | id_name),
                          data = dat_7km, REML = F)

mod_7km_reduced <- lmerTest::lmer(effort ~ 1 + offset(tot_sz_eff) + (1 | id_name),
                                  data = dat_7km, REML = F)

mod_7km <- lmerTest::lmer(effort_s ~ yr_prepost + offset(tot_sz_eff_s) + (1 | id_name),
                          data = dat_7km, REML = F)

mod_7km_reduced <- lmerTest::lmer(effort_s ~ 1 + offset(tot_sz_eff_s) + (1 | id_name),
                                  data = dat_7km, REML = F)

anova(mod_7km, mod_7km_reduced)

anova(mod_7km)
rand(mod_7km)

summary(mod_7km)

qqnorm(residuals(mod_7km))
qqline(residuals(mod_7km))

plot(mod_7km)
# plot(fitted(mod_7km), resid(mod_7km))
plot(resid(mod_7km))
hist(resid(mod_7km))

confint(mod_7km, oldNames = F)

dotplot(ranef(mod_7km, condVar = TRUE))

