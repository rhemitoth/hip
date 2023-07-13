## ****************************************************
## Title: HiP Prey Catchability RSF
## Author: Rhemi Toth
## Date Created: 02/13/2023
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

source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/Prey_Catchability_RSF_data.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/clrs.R")
# Correlation Matrix ----

data.cor <- na.omit(data.rsf[,6:8])
colnames(data.cor) <- c("Cover1m", "Rivers", "Slope")

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

# Fitting RSF ----

#**** RSF1 Slope, Veg1m----

#Fitting RSF
RSF1.fit <- glm(Used ~ veg1m + slope,
                data = data.rsf,
                family = binomial(link = "logit"))


coef(RSF1.fit)%>%exp()

hab <- stack(slope, veg1m)
names(hab) <- c("slope", "veg1m")

#RSF Prediction Map
RSF1.predict <- terra::predict(object = hab, model = RSF1.fit, type = "response")
plot(RSF1.predict, main = "RSF1 Prediction (Slope, Veg1m)")
points(xy.obs, cex = 0.25, col = "darkblue")


#**** RSF2 Slope----

RSF2.fit <- glm(Used ~ slope,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF2.fit)%>%exp()

hab <- stack(slope)
names(hab) <- c("slope")

#RSF Prediction Map
RSF2.predict <- terra::predict(object = hab, model = RSF2.fit, type = "response")
plot(RSF2.predict, main = "RSF2 Prediction (Slope)")
points(xy.obs, cex = 0.25, col = alpha("darkblue", 0.3))

#**** RSF3 Veg1m----

RSF3.fit <- glm(Used ~ veg1m,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF3.fit)%>%exp()

hab <- stack(veg1m)
names(hab) <- c("veg1m")

#RSF Prediction Map
RSF3.predict <- terra::predict(object = hab, model = RSF3.fit, type = "response")
plot(RSF3.predict, main = "RSF3 Prediction (Veg1m)")
points(xy.obs, cex = 0.25, col = "darkblue")


#**** RSF4 Slope, Veg1m, Rivers ----

#Fitting RSF
RSF4.fit <- glm(Used ~ veg1m + slope + rivers,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF4.fit)%>%exp()

vif(RSF4.fit)

hab <- stack(slope, veg1m, rivers)
names(hab) <- c("slope", "veg1m", "rivers")

#RSF Prediction Map
RSF4.predict <- terra::predict(object = hab, model = RSF4.fit, type = "response")
plot(RSF4.predict, main = "RSF4 Prediction (Slope, Veg1m, Rivers)")
points(xy.obs, cex = 0.25, col = "darkblue")


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

#Plots ----

ks_pts <- ks_raw %>%
  st_as_sf(coords = c("LONG_UTM","LAT_UTM"), crs = 32736 )%>%
  st_cast("POINT")




rsf2_spdf <- rasterToPoints(x = RSF2.predict, spatial = TRUE)
rsf2_df <- data.frame(rsf2_spdf)

#Plot params
min <- 0.2914387
max <- 0.727854
pal <- pal2

#**** RSF1----
pc_map <- pc_map %>% mask(hip)
rsf1_spdf <- rasterToPoints(x = pc_map, spatial = TRUE)
rsf1_df <- data.frame(rsf1_spdf)

p1 <- ggplot()+
  geom_raster(data = rsf1_df, aes(x = x, y = y, fill = layer))+
  labs(title = "Probability of Catching Prey",
       subtitle = "RSF1:Slope + Percent Cover at 1m",
       x = "X",
       y = "Y")+
  scale_fill_gradientn(name = "Probability", colors = pal)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/Prey_Catchability/RSF1.png",
       plot = p1,
       dpi = 350,
       width = 6,
       height = 5,
       units = "in")


#**** RSF2 ----

rsf2_spdf <- rasterToPoints(x = RSF2.predict, spatial = TRUE)
rsf2_df <- data.frame(rsf2_spdf)

p2 <- ggplot()+
  geom_raster(data = rsf2_df, aes(x = x, y = y, fill = layer))+
  labs(title = "Probability of Catching Prey",
       subtitle = "RSF2:Slope",
       x = "X",
       y = "Y")+
  scale_fill_gradientn(name = "Probability", colors = pal, limits = c(min, max))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/Prey_Catchability/RSF2.png",
       plot = p2,
       dpi = 350,
       width = 6,
       height = 5,
       units = "in")

#**** RSF3 ----

rsf3_spdf <- rasterToPoints(x = RSF3.predict, spatial = TRUE)
rsf3_df <- data.frame(rsf3_spdf)

p3 <- ggplot()+
  geom_raster(data = rsf3_df, aes(x = x, y = y, fill = layer))+
  labs(title = "Probability of Catching Prey",
       subtitle = "RSF3: Percent Cover at 1m",
       x = "X",
       y = "Y")+
  scale_fill_gradientn(name = "Probability", colors = pal, limits = c(min, max))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/Prey_Catchability/RSF3.png",
       plot = p3,
       dpi = 350,
       width = 7,
       height = 5,
       units = "in")

#**** RSF4 ----

rsf4_spdf <- rasterToPoints(x = RSF4.predict, spatial = TRUE)
rsf4_df <- data.frame(rsf4_spdf)

p4 <- ggplot()+
  geom_raster(data = rsf4_df, aes(x = x, y = y, fill = layer))+
  labs(title = "Probability of Catching Prey",
       subtitle = "RSF4: Percent Cover at 1m",
       x = "X",
       y = "Y")+
  scale_fill_gradientn(name = "Probability", colors = pal, limits = c(min, max))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.4))

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/Prey_Catchability/RSF4.png",
       plot = p4,
       dpi = 350,
       width = 6,
       height = 4,
       units = "in")

