## ****************************************************
## Title: HiP Prey Catchability RSF Data
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
pcks <- list("tmap","sp","car","rstatix","corrplot","Hmisc","terra", "scico","sf", "raster", "rgdal","bbmle", "rgeos", "ggmap", "adehabitatHR", "sjPlot", "tidyverse", "lme4","AICcmodavg")
sapply(pcks, require, char = TRUE)
##
## ****************************************************
# Loading Data ----

#****HiP----
hip <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/HiP_Boundary/boundary_edited.shp")
hip_sf <- read_sf("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/HiP_Boundary/boundary_edited.shp")

#****Dam Mask----
dam <- shapefile ("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dam/dam.shp")
#****Vegetation Openness 1m----
veg1m <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/percent_cover/percent_cover/percent_cover_1m.tif")%>%
  mask(mask = dam, inverse = TRUE)

#****Slope----
slope <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/slope/slope.tif")%>%
  mask(mask = dam, inverse = TRUE)

#****Distance to Major Rivers----
rivers <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_major_rivers/dist_major_rivers.tif")%>%
  mask(mask = dam, inverse = TRUE)

#****Kill Sites----
ks_raw <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_raw/lion_kills/lion_kills_forR_UTM.csv")%>%
  filter(UseYN == 1)

#****Kill Site Buffers----
ks_buffer <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/kill_buffers/kill_buffers.shp")

#Kill Sites Map----
pts <- ks_raw %>%
  st_as_sf(coords = c("LONG_UTM","LAT_UTM"), crs = 32736 )%>%
  st_cast("POINT")

basemap <- tm_shape(hip_sf)+
  tm_fill(col =
            "#a3b18a")

map <- basemap +
  tm_shape(pts)+
  tm_dots(size = 0.2, col = "#ffb703", )+
  tm_compass(type = "4star", size = 2, position = c("right", "top"))+
  tm_scale_bar(breaks = c(0, 3, 6), text.size = 1)+
  tm_layout(main.title = "Location of Lion Kill Sites in HiP",
            fontface = "italic",
            fontfamily = "serif",
            legend.position = c("left", "top"),
            legend.text.size = 1,
            legend.title.size = 1.5,
            legend.frame = TRUE,
            bg.color = "#606c38")

#Assigning k-folds----

#Randomly assigning CV id to herbivore observations

ids <- 1:nrow(ks_raw)
#set.seed(872436)
ids_rand <- sample(ids)
ks_raw$cvID <- ids_rand

#Assigning each herbivore observation to a fold
#Using 10 folds

ks_raw$fold <- NA

fold_assignments <- tibble(cvID = seq(10,nrow(ks_raw),10),
                           fold = seq(1,10,1))

for (i in 1:length(ks_raw$cvID)){
  if(ks_raw$cvID[i] <= fold_assignments$cvID[1]){
    ks_raw$fold[i] <- 1
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[2]){
    ks_raw$fold[i] <- 2
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[3]){
    ks_raw$fold[i] <- 3
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[4]){
    ks_raw$fold[i] <- 4
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[5]){
    ks_raw$fold[i] <- 5
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[6]){
    ks_raw$fold[i] <- 6
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[7]){
    ks_raw$fold[i] <- 7
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[8]){
    ks_raw$fold[i] <- 8
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[9]){
    ks_raw$fold[i] <- 9
  }else if (ks_raw$cvID[i] <= fold_assignments$cvID[10]){
    ks_raw$fold[i] <- 10
  }
}

#Summary of folds
fold_sum <- ks_raw %>%
  group_by(fold)%>%
  summarise(fold_count = n())

# Observed and Random Points ----

# Observed
xy.obs <- ks_raw[,c("LONG_UTM", "LAT_UTM", "Date", "fold", "Code")]%>%
  rename(X = LONG_UTM,
         Y = LAT_UTM,
         Date = Date,
         fold = fold,
         ks_id= Code)

# Random Points----

xy.random.df <- data.frame(x = double(),
                           y = double(),
                           fold = double(),
                           ks_id = character())

for (i in 1:length(xy.obs$X)){
  id <- xy.obs$ks_id[i]
  fld <- xy.obs$fold[i]
  buff <- ks_buffer[ks_buffer$Code == id,]
  rand_pt <- spsample(buff,
                      n = 20,
                      "random",
                      iter = 10)
  rand_pt_df <- as.data.frame(rand_pt)
  rand_pt_df$ks_id <- id
  rand_pt_df$fold <- fld
  colnames(rand_pt_df)[1] <- "x"
  colnames(rand_pt_df)[2] <- "y"
  colnames(rand_pt_df)[4] <- "fold"
  colnames(rand_pt_df)[3] <- "ks_id"
  xy.random.df <- rbind(xy.random.df, rand_pt_df)
}


xy.obs.df <- as.data.frame(xy.obs)
data.rsf <- data.frame(X = c(xy.obs.df[, 1], xy.random.df[, 1]),
                       Y = c(xy.obs.df[, 2], xy.random.df[, 2]),
                       Used = c(rep(TRUE, nrow(xy.obs.df)), rep(FALSE, nrow(xy.random.df))),
                       ks_id = c(xy.obs.df[,5], xy.random.df[,3]),
                       fold = c(xy.obs.df[,4], xy.random.df[,4]))

# Extract Covariates ----

# Vegetation Openness 1m
data.rsf$veg1m <- raster::extract(veg1m, data.rsf[,1:2])

# Slope
data.rsf$slope <- raster::extract(slope,data.rsf[,1:2])

#Distance to Major Rivers
data.rsf$rivers<- raster::extract(rivers, data.rsf[,1:2])

#Observed and random points csv---
write.csv(x = data.rsf,
          file = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/kills_rsf_data.csv")
# What time of day do kills occur----
# dat <- ks_raw%>%
#   mutate(DayNight = "")
# dat$DateTime <- as.POSIXct(dat$DateTime, format = "%m/%d/%Y %H:%M")
# dat$DateTime <- dat$DateTime %m+% years(2000)
# tz(dat$DateTime) <- "Africa/Johannesburg"
# dat$TimeChr <- format(dat$DateTime, format = "%H:%M")
# dat$Time <- sapply(strsplit(dat$TimeChr,":"),
#        function(x) {
#          x <- as.numeric(x)
#          x[1]+x[2]/60
#        }
# )
#
# dates <- seq(from = as.POSIXct("2015-02-19 00:00:00", format = "%Y/%m/%d %H:%M:%S"),
#              to = as.POSIXct("2015-09-09 00:00:00", format = "%Y/%m/%d %H:%M:%S"),
#              by = "days")
#
# sun_times <- tibble(Date = dates,
#                     Sunrise = as.POSIXct("1900-01-01 00:00:00", format="%Y-%m-%d"),
#                     Sunset = as.POSIXct("1900-01-01 00:00:00", format="%Y-%m-%d"))
#
# loc <- matrix(c(31.8945, -28.26156 ), nrow = 1)
# Loc <- SpatialPoints(loc, proj4string=CRS("+proj=longlat +datum=WGS84"))
#
#
# for(i in 1:length(dates)){
#   date = sun_times$Date[i]
#   tz(date) = "Africa/Johannesburg"
#   print(date)
#   rise <- crepuscule(loc, date, direction = "dawn",solarDep = 12, POSIXct.out = TRUE, proj4string=CRS("+proj=longlat +datum=WGS84"))
#   set <- crepuscule(loc, date, direction = "dusk",solarDep = 12, POSIXct.out = TRUE, proj4string=CRS("+proj=longlat +datum=WGS84"))
#
#   sun_times$Sunrise[i] <- rise$time
#   sun_times$Sunset[i] <- set$time
# }
#
# sun_times$SunriseChr <- format(sun_times$Sunrise, format = "%H:%M")
# sun_times$SunriseTime <- sapply(strsplit(sun_times$SunriseChr,":"),
#                    function(x) {
#                      x <- as.numeric(x)
#                      x[1]+x[2]/60
#                    }
# )
#
# sun_times$SunsetChr <- format(sun_times$Sunset, format = "%H:%M")
# sun_times$SunsetTime <- sapply(strsplit(sun_times$SunsetChr,":"),
#                                 function(x) {
#                                   x <- as.numeric(x)
#                                   x[1]+x[2]/60
#                                 }
# )
#
# min_sunrise <- min(sun_times$SunriseTime)
# max_sunrise <- max(sun_times$SunriseTime)
# min_sunset <- min(sun_times$SunsetTime)
# max_sunset <- max(sun_times$SunsetTime)
#
# ks_track <- make_track(tbl = dat,
#                        .x = LONG_UTM,
#                        .y = LAT_UTM,
#                        .t = DateTime,
#                        Time,
#                        TimeChr,
#                        crs = st_crs(32736),
#                        id = Code)
# ks_track <- time_of_day(x = ks_track,include.crepuscule = FALSE)
#
# p <- ggplot(ks_track, aes(x = Time))+
#   geom_histogram(bins = 24)+
#   labs(x = "Time of Day (Hours)",
#        y = "Number of Kills",
#        title = "Timing of Lion Kills")
#
# ks_sum <- ks_track %>%
#   group_by(tod_)%>%
#   summarise(count = n())
#
# p <- ggplot(ks_track, aes(x = tod_, fill = tod_))+
#   geom_bar()+
#   labs(x = "Time of Day",
#        y = "Number of Kills",
#        title = "Timing of Lion Kills")
#
#
# p <- ggplot(ks_track, aes(x = Time))+
#   geom_histogram(bins = 24)+
#   geom_vline(xintercept = min_sunrise)+
#   geom_vline(xintercept = max_sunrise)+
#   geom_vline(xintercept = min_sunset)+
#   geom_vline(xintercept = max_sunset)+
#   labs(x = "Time of Day (Hours)",
#        y = "Number of Kills",
#        title = "Timing of Lion Kills")
#
#
#
#
# #ks_map----
# pts <- data.rsf %>%
#   st_as_sf(coords = c("X","Y"), crs = 32736 )%>%
#   st_cast("POINT")
#
# basemap <- tm_shape(hip_sf)+
#   tm_fill(col =
#             "#a3b18a")
#
# map <- basemap +
#   tm_shape(pts)+
#   tm_dots(size = 0.2, col = "Used" )+
#   tm_compass(type = "4star", size = 2, position = c("right", "top"))+
#   tm_scale_bar(breaks = c(0, 3, 6), text.size = 1)+
#   tm_layout(main.title = "Location of Lion Kill Sites in HiP",
#             fontface = "italic",
#             fontfamily = "serif",
#             legend.position = c("left", "top"),
#             legend.frame = TRUE,
#             bg.color = "#606c38")
#
