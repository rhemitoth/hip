## ****************************************************
## Title: HiP Herbivore NyalaRSF
## Author: Rhemi Toth
## Date Created: 01/04/2023
## Date Last Updated: 01/04/2023
## Email: rhemitoth@g.harvard.edu
##
## ****************************************************
##
## Notes:
##
##
## ****************************************************
##
## Loading Packages
# for loading our data
library(raster)
library(readr)
library(readxl)
library(sf)
# for datasets
library(maps)
library(spData)
# for creating animations
library(magick)
# for plotting
library(grid)
library(tmap)
##
## ****************************************************

#Importing data via data prep script----

source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/herb_RSF_data.R")

Nyala <- herbivores_raw %>%
  filter(tblSpecies == "Nyala")

#Assigning k-folds----

#Randomly assigning CV id to herbivore observations

ids <- 1:nrow(Nyala)
set.seed(872436)
ids_rand <- sample(ids)
Nyala$cvID <- ids_rand

#Assigning each herbivore observation to a fold
#Using 10 folds

Nyala$fold <- NA

fold_assignments <- tibble(cvID = seq(40.4,nrow(Nyala),40.4),
                           fold = seq(1,10,1))

for (i in 1:length(Nyala$cvID)){
  if(Nyala$cvID[i] <= fold_assignments$cvID[1]){
    Nyala$fold[i] <- 1
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[2]){
    Nyala$fold[i] <- 2
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[3]){
    Nyala$fold[i] <- 3
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[4]){
    Nyala$fold[i] <- 4
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[5]){
    Nyala$fold[i] <- 5
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[6]){
    Nyala$fold[i] <- 6
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[7]){
    Nyala$fold[i] <- 7
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[8]){
    Nyala$fold[i] <- 8
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[9]){
    Nyala$fold[i] <- 9
  }else if (Nyala$cvID[i] <= fold_assignments$cvID[10]){
    Nyala$fold[i] <- 10
  }
}

#Summary of folds
fold_sum <- Nyala %>%
  group_by(fold)%>%
  summarise(fold_count = n())

# Observed and Random Points ----

# Observed
xy.obs <- Nyala[,c("Animal_lon_UTM", "Animal_lat_UTM", "Date", "TransectNa", "ObsDate", "fold")]%>%
  rename(X = Animal_lon_UTM,
         Y = Animal_lat_UTM,
         ObsDate = ObsDate,
         Transect = TransectNa,
         fold = fold)%>%
  mutate(ndviDate = as.Date("1/1/1900", format = "%m/%d/%Y"))


for (i in 1:length(xy.obs$X)){

  dateObs <- xy.obs$ObsDate[i]
  d1 <- as.Date("7/22/2014", format = "%m/%d/%Y")
  d2 <- as.Date("8/23/2014", format = "%m/%d/%Y")
  if (d1 > dateObs){
    xy.obs$ndviDate[i] <- as.Date("7/22/2014", format = "%m/%d/%Y")
  } else if(d2 < dateObs){
    xy.obs$ndviDate[i] <- as.Date("8/23/2014", format = "%m/%d/%Y")
  } else{
    t1 <- length(seq(d1, dateObs, by = "day")) - 1
    t2 <- length(seq(dateObs,d2,by = "day")) - 1
    if (t1 > t2){
      xy.obs$ndviDate[i] <- as.Date("8/23/2014", format = "%m/%d/%Y")
    }else{
      xy.obs$ndviDate[i] <- as.Date("7/22/2014", format = "%m/%d/%Y")
    }
  }
}

# Random Points----

xy.random.df <- data.frame(x = double(),
                        y = double(),
                        Transect = character(),
                        ndviDate = as.Date(x = integer(0), origin = "1970-01-01"),
                        fold = double())

for (i in 1:length(xy.obs$X)){
  trans_name <- toString(xy.obs$Transect[i])
  ndvi_date <- xy.obs$ndviDate[i]
  trans <- transects[transects$TransectID == trans_name,]
  fld <- xy.obs$fold[i]
  rand_pt <- spsample(trans,
                      n = 1,
                      "random",
                      iter = 10)
  rand_pt_df <- as.data.frame(rand_pt)
  rand_pt_df$Transect <- trans_name
  rand_pt_df$ndviDate <- ndvi_date
  rand_pt_df$fold <- fld
  colnames(rand_pt_df)[1] <- "x"
  colnames(rand_pt_df)[2] <- "y"
  colnames(rand_pt_df)[3] <- "Transect"
  colnames(rand_pt_df)[4] <- "ndviDate"
  colnames(rand_pt_df)[5] <- "fold"
  xy.random.df <- rbind(xy.random.df, rand_pt_df)
}



plot(transects)
points(SpatialPointsDataFrame(coords = xy.random.df[,c(1,2)], data = xy.random.df,
                              proj4string = CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs ")), col = "blue", cex = 0.6)
points(xy.obs, col = "orange", cex = 0.5)

xy.obs.df <- as.data.frame(xy.obs)
data.NyalaRSF <- data.frame(X = c(xy.obs.df[, 1], xy.random.df[, 1]),
                       Y = c(xy.obs.df[, 2], xy.random.df[, 2]),
                       Used = c(rep(TRUE, nrow(xy.obs.df)), rep(FALSE, nrow(xy.random.df))),
                       Transect = c(xy.obs.df[,4], xy.random.df[,3]),
                       ndviDate = c(xy.obs.df[,7], xy.random.df[,4]),
                       fold = c(xy.obs.df[,6], xy.random.df[,5]))

# Extract Covariates ----

# Vegetation Openness 1m
data.NyalaRSF$veg1m <- raster::extract(veg1m, data.NyalaRSF[,1:2])

# Vegetation Openness 1.5m
data.NyalaRSF$veg1.5m <- raster::extract(veg1.5m, data.NyalaRSF[,1:2])

# Slope
data.NyalaRSF$slope <- raster::extract(slope,data.NyalaRSF[,1:2])

#Distance to Major Rivers
data.NyalaRSF$rivers<- raster::extract(rivers, data.NyalaRSF[,1:2])

#Avg NDVI
data.NyalaRSF$ndvi <- raster::extract(ndvi, data.NyalaRSF[,1:2])

#Pre post fire ndvi

data.NyalaRSF$ndviFire <- NA

for ( i in 1:length(data.NyalaRSF$X)){
  date <- data.NyalaRSF$ndviDate[i]
  if (date == "2014-07-22"){
    data.NyalaRSF$ndviFire[i] <- raster::extract(ndvi_20140722, data.NyalaRSF[i,1:2])
  } else if (date == "2014-08-23"){
    data.NyalaRSF$ndviFire[i] <- raster::extract(ndvi_20140823, data.NyalaRSF[i,1:2])
  }
}

#Remove rows containing NA

data.NyalaRSF <- na.omit(data.NyalaRSF)

# Comparison of Used vs Available Points
boxplot(veg1m ~ Used, data = data.NyalaRSF, main = "Vegetation Openness 1m")
boxplot(veg1.5m ~ Used, data = data.NyalaRSF, main = "Vegetation Openness 1.5m")
boxplot(slope ~ Used, data = data.NyalaRSF, main = "Slope (degrees)")
boxplot(rivers ~ Used, data = data.NyalaRSF, "Distance to Major River (m)")
boxplot(ndvi ~ Used, data = data.NyalaRSF, "Average NDVI")

# Correlation Matrix ----

data.cor <- data.NyalaRSF[,6:11]
colnames(data.cor) <- c("Cover1m", "Cover1.5m", "Slope", "DistToRivers", "AvgNDVI", "PrePostFireNDVI")

#Spearmen
cor_matrix_spear <- data.cor%>%
  cor_mat(conf.level = 0.99,
          method = "spearman")

cor.lower.tri.spear <- cor_matrix_spear %>%
  cor_reorder() %>%
  pull_lower_triangle()

cor.lower.tri.spear%>%
  cor_plot(
    method = "color",
    label = TRUE,
    insignificant = "blank"
  )


#Pearson

cor_matrix_99CL <- data.cor%>%
  cor_mat(conf.level = 0.99)

cor.lower.tri.99CL <- cor_matrix_99CL %>%
  cor_reorder() %>%
  pull_lower_triangle()

cor.lower.tri.99CL %>%
  cor_plot(
    method = "color",
    label = TRUE,
    insignificant = "blank"
  )

# Fitting NyalaRSF ----

#**** NyalaRSF 1 Avg NDVI, Rivers, Slope, Veg 1m----

#Fitting NyalaRSF
NyalaRSF1.fit <- glm(Used ~ veg1m + slope + rivers + ndvi,
               data = data.NyalaRSF,
               family = binomial(link = "logit"))

coef(NyalaRSF1.fit)%>%exp()

hab1 <- stack(ndvi, rivers, slope, veg1m)
names(hab1) <- c("ndvi", "rivers", "slope", "veg1m")

#NyalaRSF Prediction Map
NyalaRSF1.predict <- terra::predict(object = hab1, model = NyalaRSF1.fit, type = "response")
plot(NyalaRSF1.predict, main = "NyalaRSF Prediction (NDVI, Rivers, Slope, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** NyalaRSF2 Avg NDVI, Rivers, Slope, Veg 1.5m----
NyalaRSF2.fit <- glm(Used ~ veg1.5m + slope + rivers + ndvi,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF2.fit)%>%exp()

hab2 <- stack(ndvi, rivers, slope, veg1.5m)
names(hab2) <- c("ndvi", "rivers", "slope", "veg1.5m")

#NyalaRSF Prediction Map
NyalaRSF2.predict <- terra::predict(object = hab2, model = NyalaRSF2.fit, type = "response")
plot(NyalaRSF2.predict, main = "NyalaRSF Prediction (NDVI, Rivers, Slope, Veg 1.5m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** NyalaRSF3 Avg NDVI, Slope, Veg 1m----
NyalaRSF3.fit <- glm(Used ~ veg1m + slope + ndvi,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF3.fit)%>%exp()

hab3 <- stack(veg1m, slope, ndvi)
names(hab3) <- c("veg1m","slope", "ndvi")

#NyalaRSF Prediction Map
NyalaRSF3.predict <- terra::predict(object = hab3, model = NyalaRSF3.fit, type = "response")
plot(NyalaRSF3.predict, main = "NyalaRSF Prediction (NDVI, Slope, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** NyalaRSF4 Avg NDVI,Veg 1m----
NyalaRSF4.fit <- glm(Used ~ veg1m + ndvi,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF4.fit)%>%exp()

vif(NyalaRSF4.fit)

hab4 <- stack(veg1m, ndvi)
names(hab4) <- c("veg1m", "ndvi")

#NyalaRSF Prediction Map
NyalaRSF4.predict <- terra::predict(object = hab4, model = NyalaRSF4.fit, type = "response")

plot(NyalaRSF4.predict, main = "NyalaRSF Prediction (NDVI, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** NyalaRSF5 Veg 1m----
NyalaRSF5.fit <- glm(Used ~ veg1m,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF5.fit)%>%exp()

hab5 <- stack(veg1m)
names(hab5) <- c("veg1m")

ggplot(data.NyalaRSF, aes(veg1m, as.numeric(Used)))+
  stat_smooth(method = "glm", family = binomial, formula = y~x)+
  geom_point()+
  labs(x = "Percent Cover at 1m",
       y = "Probability of Herbivore Occurrence")+
  ggtitle("NyalaRSF5: Percent Cover at 1m")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

#NyalaRSF Prediction Map
NyalaRSF5.predict <- terra::predict(object = hab5, model = NyalaRSF5.fit, type = "response")


plot(NyalaRSF5.predict, main = "NyalaRSF Prediction (Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** NyalaRSF6 Avg NDVI, Rivers, Slope----

#Fitting NyalaRSF
NyalaRSF6.fit <- glm(Used ~ slope + rivers + ndvi,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF6.fit)%>%exp()

hab6 <- stack(ndvi, rivers, slope)
names(hab6) <- c("ndvi", "rivers", "slope")

#NyalaRSF Prediction Map
NyalaRSF6.predict <- terra::predict(object = hab6, model = NyalaRSF6.fit, type = "response")
plot(NyalaRSF6.predict, main = "NyalaRSF Prediction (NDVI, Rivers, Slope)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** NyalaRSF7 Avg NDVI, Rivers----

#Fitting NyalaRSF
NyalaRSF7.fit <- glm(Used ~ rivers + ndvi,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF7.fit)%>%exp()

hab7 <- stack(ndvi, rivers)
names(hab7) <- c("ndvi", "rivers")

#NyalaRSF Prediction Map
NyalaRSF7.predict <- terra::predict(object = hab7, model = NyalaRSF7.fit, type = "response")
plot(NyalaRSF7.predict, main = "NyalaRSF Prediction (NDVI, Rivers)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** NyalaRSF8 Avg NDVI, Slope, Veg 1m, ----

#Fitting NyalaRSF
NyalaRSF8.fit <- glm(Used ~ veg1.5m + slope + ndvi,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF8.fit)%>%exp()

hab8 <- stack(ndvi, slope, veg1m)
names(hab8) <- c("ndvi", "slope", "veg1.5m")

#NyalaRSF Prediction Map
NyalaRSF8.predict <- terra::predict(object = hab8, model = NyalaRSF8.fit, type = "response")
plot(NyalaRSF8.predict, main = "NyalaRSF Prediction (NDVI, Slope, Veg 1.5m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** NyalaRSF9 Fire NDVI, Rivers, Slope, Veg 1m, ----

#Fitting NyalaRSF
NyalaRSF9.fit <- glm(Used ~ veg1m + slope + ndviFire + rivers,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF9.fit)%>%exp()

#**** NyalaRSF10 Fire NDVI, Slope, Veg 1m, ----

#Fitting NyalaRSF
NyalaRSF10.fit <- glm(Used ~ veg1m + slope + ndviFire,
                data = data.NyalaRSF,
                family = binomial(link = "logit"))

coef(NyalaRSF10.fit)%>%exp()

#**** NyalaRSF11 Fire NDVI,  Veg 1m, ----

#Fitting NyalaRSF
NyalaRSF11.fit <- glm(Used ~ veg1m + ndviFire,
                 data = data.NyalaRSF,
                 family = binomial(link = "logit"))

coef(NyalaRSF11.fit)%>%exp()

vif(NyalaRSF11.fit)

hab11_0722<- stack(ndvi_20140722, slope, veg1m)
names(hab11_0722) <- c("ndviFire", "slope", "veg1m")
NyalaRSF11_0722.predict <- terra::predict(object = hab11_0722, model = NyalaRSF11.fit, type = "response")

hab11_0823 <- stack(ndvi_20140823, slope, veg1m)
names(hab11_0823) <- c("ndviFire", "slope", "veg1m")
NyalaRSF11_0823.predict <- terra::predict(object = hab11_0823, model = NyalaRSF11.fit, type = "response")

#**** NyalaRSF12 Fire NDVI,Veg 1.5m, ----

#Fitting NyalaRSF
NyalaRSF12.fit <- glm(Used ~ veg1.5m + ndviFire,
                 data = data.NyalaRSF,
                 family = binomial(link = "logit"))

coef(NyalaRSF12.fit)%>%exp()


#**** AIC ----
models <- c("RSF1", "RSF2", "RSF3", "RSF4", "RSF5","RSF6","RSF7","RSF8","RSF9","RSF10","RSF11","RSF12")

AICtab(NyalaRSF1.fit,
       NyalaRSF2.fit,
       NyalaRSF3.fit,
    NyalaRSF4.fit,
    NyalaRSF5.fit,
    NyalaRSF6.fit,
    NyalaRSF7.fit,
    NyalaRSF8.fit,
    NyalaRSF9.fit,
    NyalaRSF10.fit,
    NyalaRSF11.fit,
    NyalaRSF12.fit)

aic_scores <- AIC(NyalaRSF1.fit,
       NyalaRSF2.fit,
       NyalaRSF3.fit,
       NyalaRSF4.fit,
       NyalaRSF5.fit,
       NyalaRSF6.fit,
       NyalaRSF7.fit,
       NyalaRSF8.fit,
       NyalaRSF9.fit,
       NyalaRSF10.fit,
       NyalaRSF11.fit,
       NyalaRSF12.fit)

aics <- tibble(Model = models,
               AIC = aic_scores$AIC)

write.csv(aics,
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/cv_results/nyala_RSF_AIC.csv")


#Plots----
#***** Data for Plots----
slope_spdf <- rasterToPoints(x = slope, spatial = TRUE)
slope_df <- data.frame(slope_spdf)

veg1m_spdf <- rasterToPoints(x = veg1m, spatial = TRUE)
veg1m_df <- data.frame(veg1m_spdf)

veg1.5m_spdf <- rasterToPoints(x = veg1.5m, spatial = TRUE)
veg1.5m_df <- data.frame(veg1.5m_spdf)

rivers_spdf <- rasterToPoints(x = rivers, spatial = TRUE)
rivers_df <- data.frame(rivers_spdf)

NyalaRSF5_spdf <- rasterToPoints(x = NyalaRSF5.predict, spatial = TRUE)
NyalaRSF5_df <- data.frame(NyalaRSF5_spdf)

NyalaRSF11_pre_spdf <- rasterToPoints(x = NyalaRSF11_0722.predict, spatial = TRUE)
NyalaRSF11_pre_df <- data.frame(NyalaRSF11_pre_spdf)

NyalaRSF11_post_spdf <- rasterToPoints(x = NyalaRSF11_0823.predict, spatial = TRUE)
NyalaRSF11_post_df <- data.frame(NyalaRSF11_post_spdf)

NyalaRSF4_spdf <- rasterToPoints(x = NyalaRSF4.predict, spatial = TRUE)
NyalaRSF4_df <- data.frame(NyalaRSF4_spdf)
#**** Slope----
p1 <- ggplot()+
  geom_raster(data = slope_df, aes(x = x, y = y, fill = slope))+
  ggtitle("Slope")+
  labs(x = "X", y = "Y")+
  scale_fill_gradientn(name = "Slope (Degrees)", colors = terrain.colors(10))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/slope.png",
#        plot = p1,
#        dpi = 350,
#        width = 7,
#        height = 5,
#        units = "in")

#**** Rivers----
p4 <- ggplot()+
  geom_raster(data = rivers_df, aes(x = x, y = y, fill = dist_major_rivers))+
  ggtitle("Distance to Perennial Rivers")+
  labs(x = "X", y = "Y")+
  scale_fill_viridis_c(name = "Distance (m)",option = "magma", direction = -1)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/rivers.png",
#        plot = p4,
#        dpi = 350,
#        width = 7,
#        height = 5,
#        units = "in")

#**** Cover 1m----
p2 <- ggplot()+
  geom_raster(data = veg1m_df, aes(x = x, y = y, fill = Band_1))+
  ggtitle("Percent Cover at 1 m")+
  labs(x = "X", y = "Y")+
  scale_fill_continuous(name = "Percent Cover", type = "viridis")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/cover1m.png",
#        plot = p2,
#        dpi = 350,
#        width = 7,
#        height = 5,
#        units = "in")

#**** Cover 1.5m----
p3 <- ggplot()+
  geom_raster(data = veg1.5m_df, aes(x = x, y = y, fill = Band_1))+
  ggtitle("Percent Cover at 1.5 m")+
  labs(x = "X", y = "Y")+
  scale_fill_continuous(name = "Percent Cover", type = "viridis")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/cover15m.png",
#        plot = p3,
#        dpi = 350,
#        width = 7,
#        height = 5,
#        units = "in")

#**** NyalaRSF5----

p5 <- ggplot()+
  geom_raster(data = NyalaRSF5_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "NyalaRSF5: Percent Cover at 1m")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/NyalaRSF5.png",
#        plot = p5,
#        dpi = 350,
#        width = 7,
#        height = 5,
#        units = "in")

#**** NyalaRSF11----

p6 <- ggplot()+
  geom_raster(data = NyalaRSF11_pre_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "NyalaRSF11: Pre/Post Fire NDVI + Percent Cover at 1m",
       caption = "Predicted values based on pre-fire NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/NyalaRSF11_1.png",
#        plot = p6,
#        dpi = 350,
#        width = 5,
#        height = 5,
#        units = "in")

p7 <- ggplot()+
  geom_raster(data = NyalaRSF11_post_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "NyalaRSF11: Pre/Post Fire NDVI + Percent Cover at 1m",
       caption = "Predicted values based on post-fire NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/NyalaRSF11_2.png",
#        plot = p7,
#        dpi = 350,
#        width = 5,
#        height = 5,
#        units = "in")

#**** NyalaRSF4----
p8 <- ggplot()+
  geom_raster(data = NyalaRSF4_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "NyalaRSF4:Percent Cover at 1m + Average NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/NyalaRSF4.png",
#        plot = p8,
#        dpi = 350,
#        width = 7,
#        height = 5,
#        units = "in")

#****Observed and Random Point Map----
HiP <- read_sf("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/HiP_Boundary/boundary_edited.shp")

pts <- data.NyalaRSF %>%
  st_as_sf(coords = c("X","Y"), crs = 32736 )%>%
  st_cast("POINT")

rand_pts <- xy.random.df %>%
  st_as_sf(coords = c("x","y"), crs = 32736 )%>%
  st_cast("POINT")

basemap <- tm_shape(HiP)+
  tm_fill(col = "darkgrey")

map <- basemap +
  tm_shape(pts)+
  tm_dots(size = 0.3, col = "Used", alpha = 0.3, pal = c("red", "blue"))+
  tm_compass(type = "4star", size = 2, position = c("right", "top"))+
  tm_scale_bar(breaks = c(0, 3, 6), text.size = 1)+
  tm_layout(main.title = "Used vs Available Points",
            fontface = "italic",
            fontfamily = "serif",
            legend.position = c("left", "top"),
            legend.text.size = 1,
            legend.title.size = 1.5,
            legend.frame = TRUE)


