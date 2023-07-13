## ****************************************************
## Title: HiP Herbivore RSF
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

# Observed and Random Points ----

# Observed
xy.obs <- herbivores_raw[,c("Animal_lon_UTM", "Animal_lat_UTM", "Date", "TransectNa", "ObsDate", "fold")]%>%
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


xy.obs.df <- as.data.frame(xy.obs)
data.rsf <- data.frame(X = c(xy.obs.df[, 1], xy.random.df[, 1]),
                       Y = c(xy.obs.df[, 2], xy.random.df[, 2]),
                       Used = c(rep(TRUE, nrow(xy.obs.df)), rep(FALSE, nrow(xy.random.df))),
                       Transect = c(xy.obs.df[,4], xy.random.df[,3]),
                       ndviDate = c(xy.obs.df[,7], xy.random.df[,4]),
                       fold = c(xy.obs.df[,6], xy.random.df[,5]))

# Extract Covariates ----

# Vegetation Openness 1m
data.rsf$veg1m <- raster::extract(veg1m, data.rsf[,1:2])

# Vegetation Openness 1.5m
data.rsf$veg1.5m <- raster::extract(veg1.5m, data.rsf[,1:2])

# Slope
data.rsf$slope <- raster::extract(slope,data.rsf[,1:2])

#Distance to Major Riversr=
data.rsf$rivers<- raster::extract(rivers, data.rsf[,1:2])

#Avg NDVI
data.rsf$ndvi <- raster::extract(ndvi, data.rsf[,1:2])

#Pre post fire ndvi

data.rsf$ndviFire <- NA

for ( i in 1:length(data.rsf$X)){
  date <- data.rsf$ndviDate[i]
  if (date == "2014-07-22"){
    data.rsf$ndviFire[i] <- raster::extract(ndvi_20140722, data.rsf[i,1:2])
  } else if (date == "2014-08-23"){
    data.rsf$ndviFire[i] <- raster::extract(ndvi_20140823, data.rsf[i,1:2])
  }
}

#Remove rows containing NA

data.rsf <- na.omit(data.rsf)

# Comparison of Used vs Available Points
boxplot(veg1m ~ Used, data = data.rsf, main = "Vegetation Openness 1m")
boxplot(veg1.5m ~ Used, data = data.rsf, main = "Vegetation Openness 1.5m")
boxplot(slope ~ Used, data = data.rsf, main = "Slope (degrees)")
boxplot(rivers ~ Used, data = data.rsf, "Distance to Major River (m)")
boxplot(ndvi ~ Used, data = data.rsf, "Average NDVI")

# Correlation Matrix ----

data.cor <- data.rsf[,7:12]
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

#Saving observed and random points csv----
write.csv(x = data.rsf,
          file = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/hebivore_rsf_data.csv")
# Fitting RSF ----

#**** RSF 1 Avg NDVI, Rivers, Slope, Veg 1m----

#Fitting RSF
RSF1.fit <- glm(Used ~ veg1m + slope + rivers + ndvi,
               data = data.rsf,
               family = binomial(link = "logit"))

coef(RSF1.fit)%>%exp()
vif(RSF1.fit)

hab1 <- stack(ndvi, rivers, slope, veg1m)
names(hab1) <- c("ndvi", "rivers", "slope", "veg1m")

#RSF Prediction Map
RSF1.predict <- terra::predict(object = hab1, model = RSF1.fit, type = "response")
plot(RSF1.predict, main = "RSF Prediction (NDVI, Rivers, Slope, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** RSF2 Avg NDVI, Rivers, Slope, Veg 1.5m----
RSF2.fit <- glm(Used ~ veg1.5m + slope + rivers + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF2.fit)%>%exp()
vif(RSF2.fit)

hab2 <- stack(ndvi, rivers, slope, veg1.5m)
names(hab2) <- c("ndvi", "rivers", "slope", "veg1.5m")

#RSF Prediction Map
RSF2.predict <- terra::predict(object = hab2, model = RSF2.fit, type = "response")
plot(RSF2.predict, main = "RSF Prediction (NDVI, Rivers, Slope, Veg 1.5m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** RSF3 Avg NDVI, Slope, Veg 1m----
RSF3.fit <- glm(Used ~ veg1m + slope + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF3.fit)%>%exp()

hab3 <- stack(veg1m, slope, ndvi)
names(hab3) <- c("veg1m","slope", "ndvi")

#RSF Prediction Map
RSF3.predict <- terra::predict(object = hab3, model = RSF3.fit, type = "response")
plot(RSF3.predict, main = "RSF Prediction (NDVI, Slope, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** RSF4 Avg NDVI,Veg 1m----
RSF4.fit <- glm(Used ~ veg1m + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF4.fit)%>%exp()

vif(RSF4.fit)

hab4 <- stack(veg1m, ndvi)
names(hab4) <- c("veg1m", "ndvi")

#RSF Prediction Map
RSF4.predict <- terra::predict(object = hab4, model = RSF4.fit, type = "response")

plot(RSF4.predict, main = "RSF Prediction (NDVI, Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))


#**** RSF5 Veg 1m----
RSF5.fit <- glm(Used ~ veg1m,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF5.fit)%>%exp()

hab5 <- stack(veg1m)
names(hab5) <- c("veg1m")

ggplot(data.rsf, aes(veg1m, as.numeric(Used)))+
  stat_smooth(method = "glm", family = binomial, formula = y~x)+
  geom_point()+
  labs(x = "Percent Cover at 1m",
       y = "Probability of Herbivore Occurrence")+
  ggtitle("RSF5: Percent Cover at 1m")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

#RSF Prediction Map
RSF5.predict <- terra::predict(object = hab5, model = RSF5.fit, type = "response")


plot(RSF5.predict, main = "RSF Prediction (Veg 1m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** RSF6 Avg NDVI, Rivers, Slope----

#Fitting RSF
RSF6.fit <- glm(Used ~ slope + rivers + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF6.fit)%>%exp()

hab6 <- stack(ndvi, rivers, slope)
names(hab6) <- c("ndvi", "rivers", "slope")

#RSF Prediction Map
RSF6.predict <- terra::predict(object = hab6, model = RSF6.fit, type = "response")
plot(RSF6.predict, main = "RSF Prediction (NDVI, Rivers, Slope)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** RSF7 Avg NDVI, Rivers----

#Fitting RSF
RSF7.fit <- glm(Used ~ rivers + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF7.fit)%>%exp()

hab7 <- stack(ndvi, rivers)
names(hab7) <- c("ndvi", "rivers")

#RSF Prediction Map
RSF7.predict <- terra::predict(object = hab7, model = RSF7.fit, type = "response")
plot(RSF7.predict, main = "RSF Prediction (NDVI, Rivers)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** RSF8 Avg NDVI, Slope, Veg 1.5 m, ----

#Fitting RSF
RSF8.fit <- glm(Used ~ veg1.5m + slope + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF8.fit)%>%exp()

hab8 <- stack(ndvi, slope, veg1m)
names(hab8) <- c("ndvi", "slope", "veg1.5m")

#RSF Prediction Map
RSF8.predict <- terra::predict(object = hab8, model = RSF8.fit, type = "response")
plot(RSF8.predict, main = "RSF Prediction (NDVI, Slope, Veg 1.5m)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** RSF9 Fire NDVI, Rivers, Slope, Veg 1m, ----

#Fitting RSF
RSF9.fit <- glm(Used ~ veg1m + slope + ndviFire + rivers,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF9.fit)%>%exp()
vif(RSF9.fit)

#**** RSF10 Fire NDVI, Slope, Veg 1m, ----

#Fitting RSF
RSF10.fit <- glm(Used ~ veg1m + slope + ndviFire,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF10.fit)%>%exp()

hab10_0722<- stack(ndvi_20140722, slope, veg1m)
names(hab10_0722) <- c("ndviFire", "slope", "veg1m")
RSF10_0722.predict <- terra::predict(object = hab10_0722, model = RSF10.fit, type = "response")

hab10_0823 <- stack(ndvi_20140823, slope, veg1m)
names(hab10_0823) <- c("ndviFire", "slope", "veg1m")
RSF10_0823.predict <- terra::predict(object = hab10_0823, model = RSF10.fit, type = "response")
#**** RSF11 Fire NDVI,  Veg 1m, ----

#Fitting RSF
RSF11.fit <- glm(Used ~ veg1m + ndviFire,
                 data = data.rsf,
                 family = binomial(link = "logit"))

coef(RSF11.fit)%>%exp()

vif(RSF11.fit)

hab11_0722<- stack(ndvi_20140722, veg1m)
names(hab11_0722) <- c("ndviFire", "veg1m")
RSF11_0722.predict <- terra::predict(object = hab11_0722, model = RSF11.fit, type = "response")

hab11_0823 <- stack(ndvi_20140823, veg1m)
names(hab11_0823) <- c("ndviFire",  "veg1m")
RSF11_0823.predict <- terra::predict(object = hab11_0823, model = RSF11.fit, type = "response")

#**** RSF12 Fire NDVI,Veg 1.5m, ----

#Fitting RSF
RSF12.fit <- glm(Used ~ veg1.5m + ndviFire,
                 data = data.rsf,
                 family = binomial(link = "logit"))

coef(RSF12.fit)%>%exp()


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

write.csv(aics,
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/cv_results/Herbivore_RSF_AIC.csv",
          row.names = FALSE)

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

RSF5_spdf <- rasterToPoints(x = RSF5.predict, spatial = TRUE)
RSF5_df <- data.frame(RSF5_spdf)

RSF11_pre_spdf <- rasterToPoints(x = RSF11_0722.predict, spatial = TRUE)
RSF11_pre_df <- data.frame(RSF11_pre_spdf)

RSF11_post_spdf <- rasterToPoints(x = RSF11_0823.predict, spatial = TRUE)
RSF11_post_df <- data.frame(RSF11_post_spdf)

RSF4_spdf <- rasterToPoints(x = RSF4.predict, spatial = TRUE)
RSF4_df <- data.frame(RSF4_spdf)

RSF10_pre_spdf <- rasterToPoints(x = RSF10_0722.predict, spatial = TRUE)
RSF10_pre_df <- data.frame(RSF10_pre_spdf)

#**** Slope----
p1 <- ggplot()+
  geom_raster(data = slope_df, aes(x = x, y = y, fill = slope))+
  ggtitle("Slope")+
  labs(x = "X", y = "Y")+
  scale_fill_gradientn(name = "Slope (Degrees)", colors = terrain.colors(10))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/slope.png",
       plot = p1,
       dpi = 350,
       width = 7,
       height = 5,
       units = "in")
#**** Rivers----
p4 <- ggplot()+
  geom_raster(data = rivers_df, aes(x = x, y = y, fill = dist_major_rivers))+
  ggtitle("Distance to Perennial Rivers")+
  labs(x = "X", y = "Y")+
  scale_fill_viridis_c(name = "Distance (m)",option = "magma", direction = -1)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/rivers.png",
       plot = p4,
       dpi = 350,
       width = 7,
       height = 5,
       units = "in")
#**** Cover 1m----
p2 <- ggplot()+
  geom_raster(data = veg1m_df, aes(x = x, y = y, fill = Band_1))+
  ggtitle("Percent Cover at 1 m")+
  labs(x = "X", y = "Y")+
  scale_fill_continuous(name = "Percent Cover", type = "viridis")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/cover1m.png",
       plot = p2,
       dpi = 350,
       width = 7,
       height = 5,
       units = "in")

#**** Cover 1.5m----
p3 <- ggplot()+
  geom_raster(data = veg1.5m_df, aes(x = x, y = y, fill = Band_1))+
  ggtitle("Percent Cover at 1.5 m")+
  labs(x = "X", y = "Y")+
  scale_fill_continuous(name = "Percent Cover", type = "viridis")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/cover15m.png",
       plot = p3,
       dpi = 350,
       width = 7,
       height = 5,
       units = "in")

#**** RSF5----

p5 <- ggplot()+
  geom_raster(data = RSF5_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "RSF5: Percent Cover at 1m")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/RSF5.png",
       plot = p5,
       dpi = 350,
       width = 7,
       height = 5,
       units = "in")

#**** RSF11----

p6 <- ggplot()+
  geom_raster(data = RSF11_pre_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "RSF11: Pre/Post Fire NDVI + Percent Cover at 1m",
       caption = "Predicted values based on pre-fire NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/RSF11_1.png",
       plot = p6,
       dpi = 350,
       width = 5,
       height = 5,
       units = "in")

p7 <- ggplot()+
  geom_raster(data = RSF11_post_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "RSF11: Pre/Post Fire NDVI + Percent Cover at 1m",
       caption = "Predicted values based on post-fire NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/RSF11_2.png",
       plot = p7,
       dpi = 350,
       width = 5,
       height = 5,
       units = "in")

#**** RSF4----
p8 <- ggplot()+
  geom_raster(data = RSF4_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "RSF4:Percent Cover at 1m + Average NDVI")+
  scale_fill_viridis_c(name = "Probability", option = "mako")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/meeting_20230125/RSF4.png",
       plot = p8,
       dpi = 350,
       width = 7,
       height = 5,
       units = "in")

#**** RSF10 ----

p9 <- ggplot()+
  geom_raster(data = RSF10_pre_df, aes(x = x, y = y, fill = layer))+
  ggtitle("Probability of Herbivore Occurrence")+
  labs(x = "X", y = "Y", subtitle = "RSF10:Temporally Corresponding NDVI + Slope + Percent Cover at 1m")+
  scale_fill_gradientn(name = "Probability", colors = pal2)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/RSF10.png",
       plot = p9,
       dpi = 350,
       width = 7,
       height = 5,
       units = "in")
#****Observed and Random Point Map----
HiP <- read_sf("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/HiP_Boundary/boundary_edited.shp")

pts <- data.rsf %>%
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

