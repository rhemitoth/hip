## ****************************************************
## Title: HiP Herbivore buffaloRSF
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

buffalo <- herbivores_raw %>%
  filter(tblSpecies == "Buffalo")

#Assigning k-folds----

#Randomly assigning CV id to herbivore observations

ids <- 1:nrow(buffalo)
set.seed(872436)
ids_rand <- sample(ids)
buffalo$cvID <- ids_rand

#Assigning each herbivore observation to a fold
#Using 10 folds

buffalo$fold <- NA

fold_assignments <- tibble(cvID = seq(38.9,nrow(buffalo),38.9),
                           fold = seq(1,10,1))

for (i in 1:length(buffalo$cvID)){
  if(buffalo$cvID[i] <= fold_assignments$cvID[1]){
    buffalo$fold[i] <- 1
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[2]){
    buffalo$fold[i] <- 2
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[3]){
    buffalo$fold[i] <- 3
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[4]){
    buffalo$fold[i] <- 4
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[5]){
    buffalo$fold[i] <- 5
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[6]){
    buffalo$fold[i] <- 6
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[7]){
    buffalo$fold[i] <- 7
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[8]){
    buffalo$fold[i] <- 8
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[9]){
    buffalo$fold[i] <- 9
  }else if (buffalo$cvID[i] <= fold_assignments$cvID[10]){
    buffalo$fold[i] <- 10
  }
}

#Summary of folds
fold_sum <- buffalo %>%
  group_by(fold)%>%
  summarise(fold_count = n())


# Observed and Random Points ----

# Observed
xy.obs <- buffalo[,c("Animal_lon_UTM", "Animal_lat_UTM", "Date", "TransectNa", "ObsDate", "fold")]%>%
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
data.buffaloRSF <- data.frame(X = c(xy.obs.df[, 1], xy.random.df[, 1]),
                       Y = c(xy.obs.df[, 2], xy.random.df[, 2]),
                       Used = c(rep(TRUE, nrow(xy.obs.df)), rep(FALSE, nrow(xy.random.df))),
                       Transect = c(xy.obs.df[,4], xy.random.df[,3]),
                       ndviDate = c(xy.obs.df[,7], xy.random.df[,4]),
                       fold = c(xy.obs.df[,6], xy.random.df[,5]))

# Extract Covariates ----

# Vegetation Openness 1m
data.buffaloRSF$veg1m <- raster::extract(veg1m, data.buffaloRSF[,1:2])

# Vegetation Openness 1.5m
data.buffaloRSF$veg1.5m <- raster::extract(veg1.5m, data.buffaloRSF[,1:2])

# Slope
data.buffaloRSF$slope <- raster::extract(slope,data.buffaloRSF[,1:2])

#Distance to Major Rivers
data.buffaloRSF$rivers<- raster::extract(rivers, data.buffaloRSF[,1:2])

#Avg NDVI
data.buffaloRSF$ndvi <- raster::extract(ndvi, data.buffaloRSF[,1:2])

#Pre post fire ndvi

data.buffaloRSF$ndviFire <- NA

for ( i in 1:length(data.buffaloRSF$X)){
  date <- data.buffaloRSF$ndviDate[i]
  if (date == "2014-07-22"){
    data.buffaloRSF$ndviFire[i] <- raster::extract(ndvi_20140722, data.buffaloRSF[i,1:2])
  } else if (date == "2014-08-23"){
    data.buffaloRSF$ndviFire[i] <- raster::extract(ndvi_20140823, data.buffaloRSF[i,1:2])
  }
}

#Remove rows containing NA

data.buffaloRSF <- na.omit(data.buffaloRSF)

# Comparison of Used vs Available Points
boxplot(veg1m ~ Used, data = data.buffaloRSF, main = "Vegetation Openness 1m")
boxplot(veg1.5m ~ Used, data = data.buffaloRSF, main = "Vegetation Openness 1.5m")
boxplot(slope ~ Used, data = data.buffaloRSF, main = "Slope (degrees)")
boxplot(rivers ~ Used, data = data.buffaloRSF, "Distance to Major River (m)")
boxplot(ndvi ~ Used, data = data.buffaloRSF, "Average NDVI")

# Correlation Matrix ----

data.cor <- data.buffaloRSF[,6:11]
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

# Fitting buffaloRSF ----

#**** buffaloRSF 1 Avg NDVI, Rivers, Slope, Veg 1m----

#Fitting buffaloRSF
buffaloRSF1.fit <- glm(Used ~ veg1m + slope + rivers + ndvi,
               data = data.buffaloRSF,
               family = binomial(link = "logit"))

coef(buffaloRSF1.fit)%>%exp()

hab1 <- stack(ndvi, rivers, slope, veg1m)
names(hab1) <- c("ndvi", "rivers", "slope", "veg1m")

#buffaloRSF Prediction Map
buffaloRSF1.predict <- terra::predict(object = hab1, model = buffaloRSF1.fit, type = "response")
plot(buffaloRSF1.predict, main = "buffaloRSF Prediction (NDVI, Rivers, Slope, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** buffaloRSF2 Avg NDVI, Rivers, Slope, Veg 1.5m----
buffaloRSF2.fit <- glm(Used ~ veg1.5m + slope + rivers + ndvi,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF2.fit)%>%exp()

hab2 <- stack(ndvi, rivers, slope, veg1.5m)
names(hab2) <- c("ndvi", "rivers", "slope", "veg1.5m")

#buffaloRSF Prediction Map
buffaloRSF2.predict <- terra::predict(object = hab2, model = buffaloRSF2.fit, type = "response")
plot(buffaloRSF2.predict, main = "buffaloRSF Prediction (NDVI, Rivers, Slope, Veg 1.5m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** buffaloRSF3 Avg NDVI, Slope, Veg 1m----
buffaloRSF3.fit <- glm(Used ~ veg1m + slope + ndvi,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF3.fit)%>%exp()

hab3 <- stack(veg1m, slope, ndvi)
names(hab3) <- c("veg1m","slope", "ndvi")

#buffaloRSF Prediction Map
buffaloRSF3.predict <- terra::predict(object = hab3, model = buffaloRSF3.fit, type = "response")
plot(buffaloRSF3.predict, main = "buffaloRSF Prediction (NDVI, Slope, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** buffaloRSF4 Avg NDVI,Veg 1m----
buffaloRSF4.fit <- glm(Used ~ veg1m + ndvi,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF4.fit)%>%exp()

vif(buffaloRSF4.fit)

hab4 <- stack(veg1m, ndvi)
names(hab4) <- c("veg1m", "ndvi")

#buffaloRSF Prediction Map
buffaloRSF4.predict <- terra::predict(object = hab4, model = buffaloRSF4.fit, type = "response")

plot(buffaloRSF4.predict, main = "buffaloRSF Prediction (NDVI, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** buffaloRSF5 Veg 1m----
buffaloRSF5.fit <- glm(Used ~ veg1m,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF5.fit)%>%exp()

hab5 <- stack(veg1m)
names(hab5) <- c("veg1m")

ggplot(data.buffaloRSF, aes(veg1m, as.numeric(Used)))+
  stat_smooth(method = "glm", family = binomial, formula = y~x)+
  geom_point()+
  labs(x = "Percent Cover at 1m",
       y = "Probability of Herbivore Occurrence")+
  ggtitle("buffaloRSF5: Percent Cover at 1m")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

#buffaloRSF Prediction Map
buffaloRSF5.predict <- terra::predict(object = hab5, model = buffaloRSF5.fit, type = "response")


plot(buffaloRSF5.predict, main = "buffaloRSF Prediction (Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** buffaloRSF6 Avg NDVI, Rivers, Slope----

#Fitting buffaloRSF
buffaloRSF6.fit <- glm(Used ~ slope + rivers + ndvi,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF6.fit)%>%exp()

hab6 <- stack(ndvi, rivers, slope)
names(hab6) <- c("ndvi", "rivers", "slope")

#buffaloRSF Prediction Map
buffaloRSF6.predict <- terra::predict(object = hab6, model = buffaloRSF6.fit, type = "response")
plot(buffaloRSF6.predict, main = "buffaloRSF Prediction (NDVI, Rivers, Slope)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** buffaloRSF7 Avg NDVI, Rivers----

#Fitting buffaloRSF
buffaloRSF7.fit <- glm(Used ~ rivers + ndvi,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF7.fit)%>%exp()

hab7 <- stack(ndvi, rivers)
names(hab7) <- c("ndvi", "rivers")

#buffaloRSF Prediction Map
buffaloRSF7.predict <- terra::predict(object = hab7, model = buffaloRSF7.fit, type = "response")
plot(buffaloRSF7.predict, main = "buffaloRSF Prediction (NDVI, Rivers)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** buffaloRSF8 Avg NDVI, Slope, Veg 1m, ----

#Fitting buffaloRSF
buffaloRSF8.fit <- glm(Used ~ veg1.5m + slope + ndvi,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF8.fit)%>%exp()

hab8 <- stack(ndvi, slope, veg1m)
names(hab8) <- c("ndvi", "slope", "veg1.5m")

#buffaloRSF Prediction Map
buffaloRSF8.predict <- terra::predict(object = hab8, model = buffaloRSF8.fit, type = "response")
plot(buffaloRSF8.predict, main = "buffaloRSF Prediction (NDVI, Slope, Veg 1.5m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** buffaloRSF9 Fire NDVI, Rivers, Slope, Veg 1m, ----

#Fitting buffaloRSF
buffaloRSF9.fit <- glm(Used ~ veg1m + slope + ndviFire + rivers,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF9.fit)%>%exp()

#**** buffaloRSF10 Fire NDVI, Slope, Veg 1m, ----

#Fitting buffaloRSF
buffaloRSF10.fit <- glm(Used ~ veg1m + slope + ndviFire,
                data = data.buffaloRSF,
                family = binomial(link = "logit"))

coef(buffaloRSF10.fit)%>%exp()

#**** buffaloRSF11 Fire NDVI,  Veg 1m, ----

#Fitting buffaloRSF
buffaloRSF11.fit <- glm(Used ~ veg1m + ndviFire,
                 data = data.buffaloRSF,
                 family = binomial(link = "logit"))

coef(buffaloRSF11.fit)%>%exp()

vif(buffaloRSF11.fit)

hab11_0722<- stack(ndvi_20140722, slope, veg1m)
names(hab11_0722) <- c("ndviFire", "slope", "veg1m")
buffaloRSF11_0722.predict <- terra::predict(object = hab11_0722, model = buffaloRSF11.fit, type = "response")

hab11_0823 <- stack(ndvi_20140823, slope, veg1m)
names(hab11_0823) <- c("ndviFire", "slope", "veg1m")
buffaloRSF11_0823.predict <- terra::predict(object = hab11_0823, model = buffaloRSF11.fit, type = "response")

#**** buffaloRSF12 Fire NDVI,Veg 1.5m, ----

#Fitting buffaloRSF
buffaloRSF12.fit <- glm(Used ~ veg1.5m + ndviFire,
                 data = data.buffaloRSF,
                 family = binomial(link = "logit"))

coef(buffaloRSF12.fit)%>%exp()


#**** AIC ----
models <- c("RSF1", "RSF2", "RSF3", "RSF4", "RSF5","RSF6","RSF7","RSF8","RSF9","RSF10","RSF11","RSF12")

daics <- AICtab(buffaloRSF1.fit,
       buffaloRSF2.fit,
       buffaloRSF3.fit,
    buffaloRSF4.fit,
    buffaloRSF5.fit,
    buffaloRSF6.fit,
    buffaloRSF7.fit,
    buffaloRSF8.fit,
    buffaloRSF9.fit,
    buffaloRSF10.fit,
    buffaloRSF11.fit,
    buffaloRSF12.fit)

aic_scores <- AIC(buffaloRSF1.fit,
       buffaloRSF2.fit,
       buffaloRSF3.fit,
       buffaloRSF4.fit,
       buffaloRSF5.fit,
       buffaloRSF6.fit,
       buffaloRSF7.fit,
       buffaloRSF8.fit,
       buffaloRSF9.fit,
       buffaloRSF10.fit,
       buffaloRSF11.fit,
       buffaloRSF12.fit)

aics <- tibble(Model = models,
               AIC = aic_scores$AIC)

write.csv(aics,
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/cv_results/buffalo_RSF_AIC.csv")
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

buffaloRSF5_spdf <- rasterToPoints(x = buffaloRSF5.predict, spatial = TRUE)
buffaloRSF5_df <- data.frame(buffaloRSF5_spdf)

buffaloRSF11_pre_spdf <- rasterToPoints(x = buffaloRSF11_0722.predict, spatial = TRUE)
buffaloRSF11_pre_df <- data.frame(buffaloRSF11_pre_spdf)

buffaloRSF11_post_spdf <- rasterToPoints(x = buffaloRSF11_0823.predict, spatial = TRUE)
buffaloRSF11_post_df <- data.frame(buffaloRSF11_post_spdf)

buffaloRSF4_spdf <- rasterToPoints(x = buffaloRSF4.predict, spatial = TRUE)
buffaloRSF4_df <- data.frame(buffaloRSF4_spdf)
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

#**** buffaloRSF5----

p5 <- ggplot()+
  geom_raster(data = buffaloRSF5_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "buffaloRSF5: Percent Cover at 1m")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/buffaloRSF5.png",
#        plot = p5,
#        dpi = 350,
#        width = 7,
#        height = 5,
#        units = "in")

#**** buffaloRSF11----

p6 <- ggplot()+
  geom_raster(data = buffaloRSF11_pre_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "buffaloRSF11: Pre/Post Fire NDVI + Percent Cover at 1m",
       caption = "Predicted values based on pre-fire NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/buffaloRSF11_1.png",
#        plot = p6,
#        dpi = 350,
#        width = 5,
#        height = 5,
#        units = "in")

p7 <- ggplot()+
  geom_raster(data = buffaloRSF11_post_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "buffaloRSF11: Pre/Post Fire NDVI + Percent Cover at 1m",
       caption = "Predicted values based on post-fire NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/buffaloRSF11_2.png",
#        plot = p7,
#        dpi = 350,
#        width = 5,
#        height = 5,
#        units = "in")

#**** buffaloRSF4----
p8 <- ggplot()+
  geom_raster(data = buffaloRSF4_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "buffaloRSF4:Percent Cover at 1m + Average NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/buffaloRSF4.png",
#        plot = p8,
#        dpi = 350,
#        width = 7,
#        height = 5,
#        units = "in")

#****Observed and Random Point Map----
HiP <- read_sf("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/HiP_Boundary/boundary_edited.shp")

pts <- data.buffaloRSF %>%
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


