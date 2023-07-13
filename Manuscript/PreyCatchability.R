library(tidyverse)
library(raster)
## Loading Packages
pcks <- list("sp","car","rstatix","corrplot","Hmisc","terra", "scico","sf", "raster", "rgdal","bbmle", "rgeos", "ggmap", "adehabitatHR", "sjPlot", "tidyverse", "lme4","AICcmodavg")
sapply(pcks, require, char = TRUE)
#Loading RSF data---
data.rsf <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyCatchability.csv")
data.rsf <- data.rsf[,-1]

#Summary of folds
fold_sum <- data.rsf %>%
  group_by(fold)%>%
  summarise(fold_count = n())

#Covariates----
#****Vegetation Openness 1m----
veg1m <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/percent_cover/percent_cover/percent_cover_1m.tif")

#****Slope----
slope <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/slope/slope.tif")

#****Distance to Major Rivers----
rivers <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_major_rivers/dist_major_rivers.tif")
#Distance to Rivers
rivers <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_major_rivers/dist_major_rivers.tif")

#Distance to Major Riversr
data.rsf$rivers<- raster::extract(rivers, data.rsf[,1:2])

# Fitting RSF ----

#**** RSF1 Slope, Veg1m----

#Fitting RSF
RSF1.fit <- glm(Used ~ veg1m + slope,
                data = data.rsf,
                family = binomial(link = "logit"))


#**** RSF2 Slope----

RSF2.fit <- glm(Used ~ slope,
                data = data.rsf,
                family = binomial(link = "logit"))


#**** RSF3 Veg1m----

RSF3.fit <- glm(Used ~ veg1m,
                data = data.rsf,
                family = binomial(link = "logit"))



#**** RSF4 Slope, Veg1m, Rivers ----

#Fitting RSF
RSF4.fit <- glm(Used ~ veg1m + slope + rivers,
                data = data.rsf,
                family = binomial(link = "logit"))

#AIC ----
models <- c("RSF1", "RSF2", "RSF3", "RSF4")

AICtab(RSF1.fit,
       RSF2.fit,
       RSF3.fit,
       RSF4.fit)

aic_scores <- AIC(RSF1.fit,
                  RSF2.fit,
                  RSF3.fit,
                  RSF4.fit)

aics <- tibble(Model = models,
               AIC = aic_scores$AIC)
