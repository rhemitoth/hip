library(tidyverse)

#Loading RSF data---
data.rsf <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyAb_RSF_points.csv")
data.rsf <- data.rsf[,-1]
data.rsf$ndviFire <- data.rsf$ndvi

#Summary of folds
fold_sum <- data.rsf %>%
  group_by(fold)%>%
  summarise(fold_count = n())

#Loading additional habitat covariates----

#Vegetation Openness 1.5m
veg1.5m <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/percent_cover/percent_cover/percent_cover_15m.tif")

#Distance to Rivers
rivers <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_major_rivers/dist_major_rivers.tif")

#AverageNDVI
ndvi <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/NDVI/2014/2014_average_ndvi.tif")


#Slope
slope <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/slope/slope.tif")

#Veg1m
veg1m <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/percent_cover/percent_cover/percent_cover_1m.tif")
#Extracting Veg1.5 and Dist to Rivers----

# Vegetation Openness 1.5m
data.rsf$veg1.5m <- raster::extract(veg1.5m, data.rsf[,1:2])

#Distance to Major Riversr=
data.rsf$rivers<- raster::extract(rivers, data.rsf[,1:2])

#Distance to Major Riversr=
data.rsf$ndvi<- raster::extract(ndvi, data.rsf[,1:2])

#Fitting RSF
RSF1.fit <- glm(Used ~ veg1m + slope + rivers + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF1.fit)%>%exp()
vif(RSF1.fit)

#**** RSF2 Avg NDVI, Rivers, Slope, Veg 1.5m----
RSF2.fit <- glm(Used ~ veg1.5m + slope + rivers + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

#**** RSF3 Avg NDVI, Slope, Veg 1m----
RSF3.fit <- glm(Used ~ veg1m + slope + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))


#**** RSF4 Avg NDVI,Veg 1m----
RSF4.fit <- glm(Used ~ veg1m + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

#**** RSF5 Veg 1m----
RSF5.fit <- glm(Used ~ veg1m,
                data = data.rsf,
                family = binomial(link = "logit"))


#**** RSF6 Avg NDVI, Rivers, Slope----

#Fitting RSF
RSF6.fit <- glm(Used ~ slope + rivers + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

#**** RSF7 Avg NDVI, Rivers----

#Fitting RSF
RSF7.fit <- glm(Used ~ rivers + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

#**** RSF8 Avg NDVI, Slope, Veg 1.5 m, ----

#Fitting RSF
RSF8.fit <- glm(Used ~ veg1.5m + slope + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))


#**** RSF9 Fire NDVI, Rivers, Slope, Veg 1m, ----

#Fitting RSF
RSF9.fit <- glm(Used ~ veg1m + slope + ndviFire + rivers,
                data = data.rsf,
                family = binomial(link = "logit"))


#**** RSF10 Fire NDVI, Slope, Veg 1m, ----

#Fitting RSF
RSF10.fit <- glm(Used ~ veg1m + slope + ndviFire,
                 data = data.rsf,
                 family = binomial(link = "logit"))


#**** RSF11 Fire NDVI,  Veg 1m, ----

#Fitting RSF
RSF11.fit <- glm(Used ~ veg1m + ndviFire,
                 data = data.rsf,
                 family = binomial(link = "logit"))



#**** RSF12 Fire NDVI,Veg 1.5m, ----

#Fitting RSF
RSF12.fit <- glm(Used ~ veg1.5m + ndviFire,
                 data = data.rsf,
                 family = binomial(link = "logit"))


#**** AIC ----
models <- c("RSF1", "RSF2", "RSF3", "RSF4", "RSF5","RSF6","RSF7","RSF8","RSF9","RSF10","RSF11","RSF12")

AICtab(RSF1.fit,
       RSF2.fit,
       RSF3.fit,
       RSF4.fit,
       RSF5.fit,
       RSF6.fit,
       RSF7.fit,
       RSF8.fit,
       RSF9.fit,
       RSF10.fit,
       RSF11.fit,
       RSF12.fit)

aic_scores <- AIC(RSF1.fit,
                  RSF2.fit,
                  RSF3.fit,
                  RSF4.fit,
                  RSF5.fit,
                  RSF6.fit,
                  RSF7.fit,
                  RSF8.fit,
                  RSF9.fit,
                  RSF10.fit,
                  RSF11.fit,
                  RSF12.fit)

aics <- tibble(Model = models,
               AIC = aic_scores$AIC)
