library(raster)
library(readr)
library(readxl)
library(sf)
library(maps)
library(spData)
library(tidyverse)

#Loading data----
print("Loading Data")
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

fold_assignments <- tibble(cvID = seq(188.6,nrow(herbivores_raw),188.6),
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
  }else {
    herbivores_raw$fold[i] <- 10
  }
}

#Reformatting Dates
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

#****HiP----
hip <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/boundary_edited/boundary_edited.shp")

#****Raster Extent----
raster_extent <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/raster_extent/raster_extent.shp")

#****100m Transect Buffers----
transects <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/transects_100m/transect_buffers_100m.shp")

#****Dam Mask----
dam <- shapefile ("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dam/dam.shp")

#****Vegetation Openness 1m----
veg1m <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/percent_cover/percent_cover/percent_cover_1m.tif")%>%
  mask(mask = dam, inverse = TRUE)
names(veg1m) <- "Veg1m"

#****Slope----
slope <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/slope/slope.tif")%>%
  mask(mask = dam, inverse = TRUE)
names(slope) <- "Slope"

#****NDVI----
ndvi_20140722 <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI/LC08_167080_20140722.tif")%>%
  mask(mask = dam, inverse = TRUE)

ndvi_20140722 <- resample(ndvi_20140722, slope)

ndvi_20140823 <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI/LC08_167080_20140823.tif")%>%
  mask(mask = dam, inverse = TRUE)

ndvi_20140823 <- resample(ndvi_20140823, slope)


#Prey Abundance RSF----
print("Fitting prey abundance RSF")
#****Observed and Random Points ----

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

#****Random Points----

xy.random.df <- data.frame(x = double(),
                           y = double(),
                           Transect = character(),
                           ndviDate = as.Date(x = integer(0), origin = "1970-01-01"),
                           fold = double())

for (i in 1:length(xy.obs$X)){
  trans_name <- toString(xy.obs$Transect[i])
  ndvi_date <- xy.obs$ndviDate[i]
  trans <- transects[transects$TransectID == trans_name,]
  f <- xy.obs$fold[i]
  rand_pt <- spsample(trans,
                      n = 1,
                      "random",
                      iter = 10)
  #Checking if random point is in HiP Boundary
  rand_pt_df <- as.data.frame(rand_pt)
  names(rand_pt_df) <- c("x","y")
  rand_pt_sp <- as.data.frame(rand_pt_df)
  coordinates(rand_pt_sp) <- ~x + y
  crs.geo <- CRS("+init=epsg:32736")  # looks up UTM 33N
  proj4string(rand_pt_sp) <- crs.geo
  intersect <- rand_pt_sp[hip,]
  intersect_df <- as.data.frame(intersect)
  rand_pt_df$in_bounds <- do.call(paste0,rand_pt_df) %in% do.call(paste0, intersect_df)
  iter <- 1
  while(rand_pt_df$in_bounds == FALSE){
    rand_pt <- spsample(trans,
                        n = 1,
                        "random",
                        iter = 10)
    #Checking if random point is in HiP Boundary
    rand_pt_df <- as.data.frame(rand_pt)
    names(rand_pt_df) <- c("x","y")
    rand_pt_sp <- as.data.frame(rand_pt_df)
    coordinates(rand_pt_sp) <- ~x + y
    crs.geo <- CRS("+init=epsg:32736")  # looks up UTM 33N
    proj4string(rand_pt_sp) <- crs.geo
    intersect <- rand_pt_sp[hip,]
    intersect_df <- as.data.frame(intersect)
    rand_pt_df$in_bounds <- do.call(paste0,rand_pt_df) %in% do.call(paste0, intersect_df)
    iter <- iter+1
    print(iter)
  }
  rand_pt_df$Transect <- trans_name
  rand_pt_df$ndviDate <- ndvi_date
  rand_pt_df$fold <- f
  colnames(rand_pt_df)[1] <- "x"
  colnames(rand_pt_df)[2] <- "y"
  colnames(rand_pt_df)[4] <- "Transect"
  colnames(rand_pt_df)[5] <- "ndviDate"
  colnames(rand_pt_df)[6] <- "fold"
  xy.random.df <- rbind(xy.random.df, rand_pt_df)
}


xy.obs.df <- as.data.frame(xy.obs)
data.rsf <- data.frame(X = c(xy.obs.df[, 1], xy.random.df[, 1]),
                       Y = c(xy.obs.df[, 2], xy.random.df[, 2]),
                       Used = c(rep(TRUE, nrow(xy.obs.df)), rep(FALSE, nrow(xy.random.df))),
                       Transect = c(xy.obs.df[,4], xy.random.df[,4]),
                       ndviDate = c(xy.obs.df[,7], xy.random.df[,5]),
                       fold = c(xy.obs.df[,6], xy.random.df[,6]))

#****Extract Covariates ----

# Vegetation Openness 1m
data.rsf$veg1m <- raster::extract(veg1m, data.rsf[,1:2])

# Slope
data.rsf$slope <- raster::extract(slope,data.rsf[,1:2])

# Temporally Coressponding NDVI

data.rsf$ndvi <- NA

for (i in 1:length(data.rsf$X)){
  date <- data.rsf$ndviDate[i]
  if (date == "2014-07-22"){
    data.rsf$ndvi[i] <- raster::extract(ndvi_20140722, data.rsf[i,1:2])
  } else if (date == "2014-08-23"){
    data.rsf$ndvi[i] <- raster::extract(ndvi_20140823, data.rsf[i,1:2])
  }
}

# Remove rows containing NA

#data.rsf <- na.omit(data.rsf)

#Saving Used and Random Points----
write.csv(data.rsf,
          file = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyAb_RSF_points.csv")

#****Fitting RSF----
RSF10.fit <- glm(Used ~ veg1m + slope + ndvi,
                 data = data.rsf,
                 family = binomial(link = "logit"))



names(ndvi_20140823) <- "ndvi"
names(veg1m) <- "veg1m"
names(slope) <- "slope"
hab <- stack(veg1m, slope, ndvi_20140823)
pa_map <- terra::predict(object = hab, model = RSF10.fit, type = "response")
plot(pa_map%>%mask(hip), col = pal2)
summary(RSF10.fit)
