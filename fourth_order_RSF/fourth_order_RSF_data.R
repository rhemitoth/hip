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
clusters <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/clusters_forR.csv")%>%
  filter(FID != 0 & FID!= 532)

#****Prey Catchability Map----
pc_map <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/PreyCatchability/PreyCatchability.tif")


#****Prey Abundance----
directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/PreyAbundance"
files <- list.files(directory, pattern = "tif")
numfiles <- length(files)
pa_maps <- c()

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  pa_map <- paste(str_replace(files[i],".tif",""))
  year <- substr(pa_map,4,7)
  month <- substr(pa_map,8,9)
  day <- substr(pa_map,10,11)
  key <- paste(year,month,day,sep = "-")
  #Reading in raster
  dat <- raster(file_path)%>%
    mask(mask = dam, inverse = TRUE)
  assign(key,dat)
  pa_maps[key] <- pa_map
}

#****NDVI----
directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI"
files <- list.files(directory, pattern = "tif")
numfiles <- length(files)
ndvi_images <- c()

for (i in 1:numfiles){
  print(i)
  file_path <- paste(directory,"/",files[i],sep = "")
  #Reading in raster
  dat <- raster(file_path)%>%
    mask(mask = dam, inverse = TRUE)
  dat <-  resample(dat, slope)
  #Assigning raster to variable and storing it in a dictionary
  var_name1 <- str_sub(files[i], start= -12)
  var_name2 <- paste(str_replace(var_name1,".tif",""))
  var_name3 <- paste("ndvi_",var_name2,sep="")
  assign(var_name3, dat)
  year <- str_sub(var_name2, start = 1, end = 4)
  month <- str_sub(var_name2, start = 5, end = 6)
  day <- str_sub(var_name2, start = 7, end = 8)
  key <- paste(year,month,day,sep = "-")
  ndvi_images[key] <- var_name3
}

#Assigning k-folds----

#Randomly assigning CV id to herbivore observations

ids <- 1:nrow(clusters)
set.seed(872436)
ids_rand <- sample(ids)
clusters$cvID <- ids_rand

#Assigning each herbivore observation to a fold
#Using 10 folds

clusters$fold <- NA

fold_assignments <- tibble(cvID = seq(63.1,nrow(clusters),63.1),
                           fold = seq(1,10,1))

for (i in 1:length(clusters$cvID)){
  if(clusters$cvID[i] <= fold_assignments$cvID[1]){
    clusters$fold[i] <- 1
  }else if (clusters$cvID[i] <= fold_assignments$cvID[2]){
    clusters$fold[i] <- 2
  }else if (clusters$cvID[i] <= fold_assignments$cvID[3]){
    clusters$fold[i] <- 3
  }else if (clusters$cvID[i] <= fold_assignments$cvID[4]){
    clusters$fold[i] <- 4
  }else if (clusters$cvID[i] <= fold_assignments$cvID[5]){
    clusters$fold[i] <- 5
  }else if (clusters$cvID[i] <= fold_assignments$cvID[6]){
    clusters$fold[i] <- 6
  }else if (clusters$cvID[i] <= fold_assignments$cvID[7]){
    clusters$fold[i] <- 7
  }else if (clusters$cvID[i] <= fold_assignments$cvID[8]){
    clusters$fold[i] <- 8
  }else if (clusters$cvID[i] <= fold_assignments$cvID[9]){
    clusters$fold[i] <- 9
  }else{
    clusters$fold[i] <- 10
  }
}

#Summary of folds
fold_sum <- clusters %>%
  group_by(fold)%>%
  summarise(fold_count = n())

#Data for RSF----
data.rsf <- clusters %>%
  dplyr::select(c('easting','northing','FID','Date','Lion','Lion_sex', 'Species','PreySex','Kill','fold','cvID'))

#NDVI Date----
ndvi_dates <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI/date_index.csv")%>%
  arrange(ImageDate)
data.rsf$ndviDate <- NA
for(j in 1:nrow(data.rsf)){
  data.rsf$ndviDate[j] <- as.Date("1970-01-01", format = "%Y%m%d")
  dateObs <- data.rsf$Date[j]
  dateList <- ndvi_dates$ImageDate
  index <- which.min(abs(dateList - dateObs))
  data.rsf$ndviDate[j] <- dateList[index]
}
# Extract Covariates ----

# Vegetation Openness 1m
data.rsf$veg1m <- raster::extract(veg1m, data.rsf[,1:2])

# Slope
data.rsf$slope <- raster::extract(slope,data.rsf[,1:2])

#Distance to Major Rivers
data.rsf$rivers<- raster::extract(rivers, data.rsf[,1:2])

#PreyCatchability

data.rsf$PreyCatchability <- raster::extract(pc_map, data.rsf[,1:2])

#NDVI and prey abundance----
data.rsf$ndvi <- 0.001
data.rsf$PreyAbundance <- 0.001
for(i in 1:nrow(data.rsf)){
  date <- data.rsf$ndviDate[i]
  index <- which(ndvi_dates == date)
  ndviImg <- get(ndvi_images[index])
  pa_map <- get(pa_maps[index])
  data.rsf$ndvi[i] <- raster::extract(ndviImg, data.rsf[i,1:2])
  data.rsf$PreyAbundance[i]  <- raster::extract(pa_map, data.rsf[i,1:2])
}


