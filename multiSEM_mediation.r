library(lavaan)
library(lavaan.survey)

model <- readLines('moderation.lav')

fit.naive <- sem(model, data = data1, std.lv=TRUE, estimator="MLM")
survey.design <- svydesign(ids = ~schids, prod = ~1, data = data1)

fit.survey <- lavaan.survey(fit.naive, survey.design)
summary(fit.survey)

library(semPlot)
semPaths(fit.survey,what = 'paths', 
         style = 'OpenMX', layout = 'tree', 
         edge.label.cex = 1.25, nCharNodes = 0, 
         sizeMan = 8, sizeLat = 12, 
         residuals = FALSE, intercepts = FALSE)

#描述统计
library(psych)
describeBy(data1, data1$schids, ranges=FALSE, skew=FALSE)

fit_aov <- aov(bad~schids, data = data1)
summary(fit_aov)