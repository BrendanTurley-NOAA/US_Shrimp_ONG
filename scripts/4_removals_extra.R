library(broom.mixed)
library(ggeffects)
library(vioplot)

predict_response(fit1_red, c('statzone','treatment')) |>
  plot()
predict_response(fit1_red, c('statzone')) |>
  plot()
predict_response(fit1_red, c('tot_sz_eff','treatment')) |>
  plot()



predict(fit1,type='disp')
predict(fit1,type='response',se.fit = T)
dat1$treatment
boxplot(predict(fit1,type='response')~as.factor(dat1$treatment)*as.factor(dat1$statzone))

vioplot(dat1$effort_p~as.factor(dat1$treatment)*as.factor(dat1$statzone),col=c(3,2))
vioplot(predict(fit1,type='response')~as.factor(dat1$treatment)*as.factor(dat1$statzone),col=c(3,2))

out <- tidy(fit1_red1,conf.int = T, n=50)


pred_se <- predict(fit1,type='response',se.fit = T, cov.fit = T)

preds <- data.frame(pred_val = pred_se$fit,
           pred_se = pred_se$se.fit,
           statzone = as.factor(dat1$statzone),
           treatment = dat1$treatment)
preds$statzone <- factor(preds$statzone,sort(levels(preds$statzone)))
aggregate(pred_se ~ statzone + treatment, data = preds,
          function(x) cbind(max(x), median(x), min(x)))


pre <- subset(preds, treatment=='ant')
post <- subset(preds, treatment=='des')
vioplot(pre$pred_val ~ pre$statzone, side = 'left', plotCentre = 'line', col = 2)
vioplot(post$pred_val ~ post$statzone, side = 'right', add = T, plotCentre = 'line', col = 4)

vioplot(pre$pred_val, side = 'left', plotCentre = 'line', col = 2)
vioplot(post$pred_val, side = 'right', add = T, plotCentre = 'line', col = 4)

