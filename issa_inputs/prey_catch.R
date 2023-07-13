## Loading Packages
# for loading our data
library(raster)
library(readr)
library(readxl)
library(sf)
library(maps)
library(spData)
library(tidyverse)
##
## ****************************************************

#Loading data----
print("Loading Data")

#****Kill Sites----
ks_raw <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_raw/lion_kills/lion_kills_forR_UTM.csv")%>%
  filter(UseYN == 1)

#****Kill Site Buffers----
ks_buffer <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/kill_buffers/kill_buffers.shp")

#****HiP----
hip <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/boundary_edited/boundary_edited.shp")

#****Raster Extent----
raster_extent <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/raster_extent/raster_extent.shp")

#****Dam Mask----
dam <- shapefile ("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dam/dam.shp")

#****Vegetation Openness 1m----
veg1m <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/percent_cover/percent_cover/percent_cover_1m.tif")
names(veg1m) <- "Veg1m"

#****Slope----
slope <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/slope/slope.tif")
names(slope) <- "Slope"

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

#Prey Catchability RSF----
#****Observed and Random Points ----

# Observed
# Observed
xy.obs <- ks_raw[,c("LONG_UTM", "LAT_UTM", "Date","Code", "fold")]%>%
  rename(X = LONG_UTM,
         Y = LAT_UTM,
         Date = Date,
         ks_id= Code)

# Random Points----

xy.random.df <- data.frame(x = double(),
                           y = double(),
                           ks_id = character(),
                           fold = double())

for (i in 1:length(xy.obs$X)){
  id <- xy.obs$ks_id[i]
  buff <- ks_buffer[ks_buffer$Code == id,]
  f <- xy.obs$fold[i]
  print(id)
  rand_pt <- spsample(buff,
                      n = 20,
                      "random",
                      iter = 10)
  rand_pt_df <- as.data.frame(rand_pt)
  rand_pt_df$ID <- seq(1,20,1)
  rand_pt_df$ks_id <- id
  rand_pt_df$fold <- f

  #Checking if Random Point is in HiP
  names(rand_pt_df) <- c("x","y","pt_id","ks_id","fold")
  rand_pt_sp <- as.data.frame(rand_pt_df)
  coordinates(rand_pt_sp) <- ~x + y
  crs.geo <- CRS("+init=epsg:32736")  # looks up UTM 33N
  proj4string(rand_pt_sp) <- crs.geo
  intersect <- rand_pt_sp[hip,]
  intersect_df <- as.data.frame(intersect)
  rand_pt_df$in_bounds <- do.call(paste0,rand_pt_df) %in% do.call(paste0, intersect_df)
  iter <- 1
  out_of_bounds <- rand_pt_df%>%
    filter(in_bounds == FALSE)
  #Regenerating Out of Bounds Points
  if(nrow(out_of_bounds) > 0){
    for(j in 1:nrow(out_of_bounds)){
      print(paste("Correcting Point", j, "/",nrow(out_of_bounds)))
      ID <- out_of_bounds$pt_id[j]
      bad_pts <- rand_pt_df %>%
        filter(pt_id == ID)
      for(k in 1:nrow(bad_pts)){
        new_rand_pt <- spsample(buff,
                            n = 1,
                            "random",
                            iter = 10)
        new_rand_pt_df <- as.data.frame(new_rand_pt)
        new_rand_pt_df$ID <- bad_pts$pt_id[k]
        new_rand_pt_df$ks_id <- bad_pts$ks_id[k]
        new_rand_pt_df$fold <- bad_pts$fold[k]
        names(new_rand_pt_df) <- c("x","y","pt_id","ks_id","fold")
        new_rand_pt_sp <- as.data.frame(new_rand_pt_df)
        coordinates(new_rand_pt_sp) <- ~x + y
        crs.geo <- CRS("+init=epsg:32736")  # looks up UTM 33N
        proj4string(new_rand_pt_sp) <- crs.geo
        int <- new_rand_pt_sp[hip,]
        iter <- 1
        while(nrow(int) != 1){
          new_rand_pt <- spsample(buff,
                                  n = 1,
                                  "random",
                                  iter = 10)
          new_rand_pt_df <- as.data.frame(new_rand_pt)
          new_rand_pt_df$ID <- bad_pts$pt_id[k]
          new_rand_pt_df$ks_id <- bad_pts$ks_id[k]
          new_rand_pt_df$fold <- bad_pts$fold[k]
          names(new_rand_pt_df) <- c("x","y","pt_id","ks_id","fold")
          new_rand_pt_sp <- as.data.frame(new_rand_pt_df)
          coordinates(new_rand_pt_sp) <- ~ x + y
          crs.geo <- CRS("+init=epsg:32736")  # looks up UTM 33N
          proj4string(new_rand_pt_sp) <- crs.geo
          int <- new_rand_pt_sp[hip,]
          iter <- 1
          iter <- iter + 1
        }
        rowid <- bad_pts[k,]$pt_id
        rand_pt_df[rowid,] <- new_rand_pt_df

      }
  }
  }
  colnames(rand_pt_df)[1] <- "x"
  colnames(rand_pt_df)[2] <- "y"
  colnames(rand_pt_df)[3] <- "pt_id"
  colnames(rand_pt_df)[4] <- "ks_id"
  colnames(rand_pt_df)[5] <- "fold"

  xy.random.df <- rbind(xy.random.df, rand_pt_df)
}

xy.obs.df <- as.data.frame(xy.obs)
data.rsf <- data.frame(X = c(xy.obs.df[, 1], xy.random.df[, 1]),
                       Y = c(xy.obs.df[, 2], xy.random.df[, 2]),
                       Used = c(rep(TRUE, nrow(xy.obs.df)), rep(FALSE, nrow(xy.random.df))),
                       ks_id = c(xy.obs.df[,4], xy.random.df[,4]),
                       fold = c(xy.obs.df[,5], xy.random.df[,5]))

# Extract Covariates ----

# Vegetation Openness 1m
data.rsf$veg1m <- raster::extract(veg1m, data.rsf[,1:2])

# Slope
data.rsf$slope <- raster::extract(slope,data.rsf[,1:2])
#****Fitting RSF----

#Fitting RSF

RSF1.fit <- glm(Used ~ veg1m + slope,
                data = data.rsf,
                family = binomial(link = "logit"))

hab <- stack(slope, veg1m)
names(hab) <- c("slope", "veg1m")


#RSF Prediction Map
data.rsf <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyCatchability.csv")
data.rsf <- data.rsf[,-1]
pc_map <- terra::predict(object = hab, model = RSF1.fit, type = "response")
names(pc_map) <- "PreyCatchability"

write.csv(data.rsf,
          file = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyCatchability.csv")
writeRaster(pc_map, "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyCatchability.tif",
            format = "GTiff",
            overwrite = TRUE)

summary(RSF1.fit)

plot(pc_map)
