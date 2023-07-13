## ****************************************************
## Title: iSSA Covariates
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
library(maps)
library(spData)
library(tidyverse)
##
## ****************************************************

#Loading data----
print("Loading Data")
#****Herbivore Data----
herbivores_raw <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/herbivore_census/TransectData_2014_cleaned_errors removed_UTM36S.csv")%>%
  filter(tblSpecies == "Buffalo" |
           tblSpecies == "Wildebeest" |
           tblSpecies == "Nyala" |
           tblSpecies == "Zebra" |
           tblSpecies == "Kudu" |
           tblSpecies == "Impala" |
           tblSpecies == "Giraffe")%>%
  mutate(ObsDate = as.Date("7/22/2014", format = "%m/%d/%Y"))

#****Kill Sites----
ks_raw <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_raw/lion_kills/lion_kills_forR_UTM.csv")%>%
  filter(UseYN == 1)

#****Kill Site Buffers----
ks_buffer <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/kill_buffers/kill_buffers.shp")

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
directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI"
files <- list.files(directory, pattern = "tif")
numfiles <- length(files)
ndvi_images <- c()

for (i in 1:numfiles){
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

#Prey Abundance RSF----
print("Fitting prey abundance RSF")
#****Observed and Random Points ----

# Observed
xy.obs <- herbivores_raw[,c("Animal_lon_UTM", "Animal_lat_UTM", "Date", "TransectNa", "ObsDate")]%>%
  rename(X = Animal_lon_UTM,
         Y = Animal_lat_UTM,
         ObsDate = ObsDate,
         Transect = TransectNa)%>%
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
                           ndviDate = as.Date(x = integer(0), origin = "1970-01-01"))

for (i in 1:length(xy.obs$X)){
  trans_name <- toString(xy.obs$Transect[i])
  ndvi_date <- xy.obs$ndviDate[i]
  trans <- transects[transects$TransectID == trans_name,]
  rand_pt <- spsample(trans,
                      n = 1,
                      "random",
                      iter = 10)
  rand_pt_df <- as.data.frame(rand_pt)
  rand_pt_df$Transect <- trans_name
  rand_pt_df$ndviDate <- ndvi_date
  colnames(rand_pt_df)[1] <- "x"
  colnames(rand_pt_df)[2] <- "y"
  colnames(rand_pt_df)[3] <- "Transect"
  colnames(rand_pt_df)[4] <- "ndviDate"
  xy.random.df <- rbind(xy.random.df, rand_pt_df)
}


xy.obs.df <- as.data.frame(xy.obs)
data.rsf <- data.frame(X = c(xy.obs.df[, 1], xy.random.df[, 1]),
                       Y = c(xy.obs.df[, 2], xy.random.df[, 2]),
                       Used = c(rep(TRUE, nrow(xy.obs.df)), rep(FALSE, nrow(xy.random.df))),
                       Transect = c(xy.obs.df[,4], xy.random.df[,3]),
                       ndviDate = c(xy.obs.df[,6], xy.random.df[,4]))

#****Extract Covariates ----

# Vegetation Openness 1m
data.rsf$veg1m <- raster::extract(veg1m, data.rsf[,1:2])

# Remove rows containing NA

data.rsf <- na.omit(data.rsf)

#****Fitting RSF----
RSF5.fit <- glm(Used ~ veg1m,
                 data = data.rsf,
                 family = binomial(link = "logit"))

hab <- stack(veg1m)
names(hab) <- "veg1m"
pa_map <- terra::predict(object = hab, model = RSF5.fit, type = "response")

writeRaster(pa_map, "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/Sensitivity_Test/PreyAbundance.tif",
            format = "GTiff",
            overwrite = TRUE)
#Prey Catchability RSF----
#****Observed and Random Points ----

# Observed
xy.obs <- ks_raw[,c("LONG_UTM", "LAT_UTM", "Date",  "Code")]%>%
  rename(X = LONG_UTM,
         Y = LAT_UTM,
         Date = Date,
         ks_id= Code)

# Random Points

xy.random.df <- data.frame(x = double(),
                           y = double(),
                           ks_id = character())

for (i in 1:length(xy.obs$X)){
  id <- xy.obs$ks_id[i]
  buff <- ks_buffer[ks_buffer$Code == id,]
  rand_pt <- spsample(buff,
                      n = 1,
                      "random",
                      iter = 10)
  rand_pt_df <- as.data.frame(rand_pt)
  rand_pt_df$ks_id <- id
  colnames(rand_pt_df)[1] <- "x"
  colnames(rand_pt_df)[2] <- "y"
  colnames(rand_pt_df)[3] <- "ks_id"
  xy.random.df <- rbind(xy.random.df, rand_pt_df)
}


xy.obs.df <- as.data.frame(xy.obs)
data.rsf <- data.frame(X = c(xy.obs.df[, 1], xy.random.df[, 1]),
                       Y = c(xy.obs.df[, 2], xy.random.df[, 2]),
                       Used = c(rep(TRUE, nrow(xy.obs.df)), rep(FALSE, nrow(xy.random.df))),
                       ks_id = c(xy.obs.df[,4], xy.random.df[,3]))

#****Extract Covariates ----

# Vegetation Openness 1m
data.rsf$veg1m <- raster::extract(veg1m, data.rsf[,1:2])

# Slope
data.rsf$slope <- raster::extract(slope,data.rsf[,1:2])

#****Fitting RSF----

#Fitting RSF
RSF2.fit <- glm(Used ~ slope,
                data = data.rsf,
                family = binomial(link = "logit"))

hab <- stack(slope)
names(hab) <- c("slope")

#RSF Prediction Map
pc_map <- terra::predict(object = hab, model = RSF2.fit, type = "response")
names(pc_map) <- "PreyCatchability"

writeRaster(pc_map, "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/Sensitivity_Test/PreyCatchability.tif",
            format = "GTiff",
            overwrite = TRUE)

#Lion encounter risk----

babe_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Babe_AKDE_100_Dist_to_Edge_30m.tif")

fanbelt_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Fanbelt_AKDE_100_Dist_to_Edge_30m.tif")

ihlane_distEdge <- raster('/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/iHlane_AKDE_100_Dist_to_Edge_30m.tif')

koku_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Koku_AKDE_100_Dist_to_Edge_30m.tif")

madonna_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Madonna_AKDE_100_Dist_to_Edge_30m.tif")

mellon_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Mellon_AKDE_100_Dist_to_Edge_30m.tif")

murph_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Murph_AKDE_100_Dist_to_Edge_30m.tif")

ntombi_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Ntombi_AKDE_100_Dist_to_Edge_30m.tif")

pokejr_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/PokeJr_AKDE_100_Dist_to_Edge_30m.tif")

roxy_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Roxy_AKDE_100_Dist_to_Edge_30m.tif")

stud_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Stud_AKDE_100_Dist_to_Edge_30m.tif")

thembi_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Thembi_AKDE_100_Dist_to_Edge_30m.tif")

zulu_distEdge <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dist_akde100_edge/Zulu_AKDE_100_Dist_to_Edge_30m.tif")

#Dictionary used to call variable when fitting issa
distEdges <- c("Babe" = babe_distEdge,
               "Fanbelt" = fanbelt_distEdge,
               "iHlane" = ihlane_distEdge,
               "Koku" = koku_distEdge,
               "Madonna" = madonna_distEdge,
               "Mellon" = mellon_distEdge,
               "Murph" = murph_distEdge,
               "Ntombi" = ntombi_distEdge,
               "PokeJr" = pokejr_distEdge,
               "Roxy" = roxy_distEdge,
               "Stud" = stud_distEdge,
               "Thembi" = thembi_distEdge,
               "Zulu" = zulu_distEdge)

#Lion Home Ranges----
babe_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Babe.shp")
fanbelt_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Fanbelt.shp")
fluffy_hr <- hip #Fluffy never formed home range, but will use hip boundary to constrain random steps
ihlane_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/iHlane.shp")
koku_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Koku.shp")
madonna_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Madonna.shp")
mellon_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Mellon.shp")
murph_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Murph.shp")
ntombi_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Ntombi.shp")
pokejr_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/PokeJr.shp")
roxy_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Roxy.shp")
stud_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Stud.shp")
thembi_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Thembi.shp")
zulu_hr <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/AKDE_100_raster_to_poly/Zulu.shp")

homeranges <- c("Babe" = babe_hr,
                "Fanbelt" = fanbelt_hr,
                "Fluffy" = fluffy_hr,
                "iHlane" = ihlane_hr,
                "Koku" = koku_hr,
                "Madonna" = madonna_hr,
                "Mellon" = mellon_hr,
                "Murph" = murph_hr,
                "Ntombi" = ntombi_hr,
                "PokeJr" = pokejr_hr,
                "Roxy" = roxy_hr,
                "Stud" = stud_hr,
                "Thembi" = thembi_hr,
                "Zulu" = zulu_hr)

#Habitat Stack


