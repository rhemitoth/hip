## ****************************************************
## Title: HiP Herbivore RSF Data
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
pcks <- list("sp","car","rstatix","corrplot","Hmisc","terra", "scico","sf", "raster", "rgdal","bbmle", "rgeos", "ggmap", "adehabitatHR", "sjPlot", "tidyverse", "lme4","AICcmodavg")
sapply(pcks, require, char = TRUE)
##
## ****************************************************
# Loading Data ----

#****Herbivore Data----
herbivores_raw <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/herbivore_census/TransectData_2014_cleaned_errors removed_UTM36S.csv")%>%
  filter(tblSpecies == "Buffalo" |
           tblSpecies == "Wildebeeste" |
           tblSpecies == "Nyala" |
           tblSpecies == "Zebra" |
           tblSpecies == "Kudu" |
           tblSpecies == "Impala" |
           tblSpecies == "Giraffe")%>%
  mutate(ObsDate = as.Date("7/22/2014", format = "%m/%d/%Y"))



#Summary of herbivore data
herbs_sum <- herbivores_raw %>%
  group_by(tblSpecies)%>%
  summarise(count = n())%>%
  arrange(count)

write.csv(herbs_sum,
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/herbs_count.csv")

for (i in 1:length(herbivores_raw$OBJECTID)){
  date <- herbivores_raw$Date[i]
  year <- "2014"
  month <- strsplit(date,"/")[[1]][2]
  day <- strsplit(date,"/")[[1]][1]
  if (str_length(day) < 2){
    day <- paste("0", day,sep = "")
  }
  new_date <- paste(month,day,year,sep = "/")
  herbivores_raw$ObsDate[i] <- as.Date(new_date, format = "%m/%d/%Y")
}
herbivores <- SpatialPoints(herbivores_raw[,c("Animal_lon_UTM", "Animal_lat_UTM")],
                            proj4string = CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs"))

#****HiP----
hip <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/HiP_Boundary/boundary_edited.shp")

#****Dam Mask----
dam <- shapefile ("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dam/dam.shp")
#****Vegetation Openness 1m----
veg1m <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/percent_cover/percent_cover/percent_cover_1m.tif")%>%
  mask(mask = dam, inverse = TRUE)
#****Vegetation Openness 1.5m----
veg1.5m <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/percent_cover/percent_cover/percent_cover_15m.tif")%>%
  mask(mask = dam, inverse = TRUE)

#****Slope----
slope <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/slope/slope.tif")%>%
  mask(mask = dam, inverse = TRUE)

#****Distance to Major Rivers----
rivers <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_major_rivers/dist_major_rivers.tif")%>%
  mask(mask = dam, inverse = TRUE)

#****NDVI----
ndvi <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/NDVI/2014/2014_average_ndvi.tif")%>%
  mask(mask = dam, inverse = TRUE)

ndvi_20140722 <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI/LC08_167080_20140722.tif")%>%
  mask(mask = dam, inverse = TRUE)

ndvi_20140722 <- resample(ndvi_20140722, slope)

ndvi_20140823 <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI/LC08_167080_20140823.tif")%>%
  mask(mask = dam, inverse = TRUE)

ndvi_20140823 <- resample(ndvi_20140823, slope)


#****100m Transect Buffers----
transects <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/transects_100m/transect_buffers_100m.shp")


#buffers <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/buffers/transect_buffers_100m.shp")

# Assigning Cross Validaiton Folds

#Assigning k-folds----

#Randomly assigning CV id to herbivore observations

ids <- 1:nrow(herbivores_raw)
#set.seed(872436)
ids_rand <- sample(ids)
herbivores_raw$cvID <- ids_rand

#Assigning each herbivore observation to a fold
#Using 10 folds

herbivores_raw$fold <- NA

fold_assignments <- tibble(cvID = seq(180.4,nrow(herbivores_raw),180.4),
                           fold = seq(1,10,1))

for (i in 1:length(herbivores_raw$cvID)){
  if(herbivores_raw$cvID[i] <= fold_assignments$cvID[1]){
    herbivores_raw$fold[i] <- 1
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[2]){
    herbivores_raw$fold[i] <- 2
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[3]){
    herbivores_raw$fold[i] <- 3
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[4]){
    herbivores_raw$fold[i] <- 4
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[5]){
    herbivores_raw$fold[i] <- 5
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[6]){
    herbivores_raw$fold[i] <- 6
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[7]){
    herbivores_raw$fold[i] <- 7
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[8]){
    herbivores_raw$fold[i] <- 8
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[9]){
    herbivores_raw$fold[i] <- 9
  }else if (herbivores_raw$cvID[i] <= fold_assignments$cvID[10]){
    herbivores_raw$fold[i] <- 10
  }
}

#Summary of folds
fold_sum <- herbivores_raw %>%
  group_by(fold)%>%
  summarise(fold_count = n())
