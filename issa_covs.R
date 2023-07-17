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

#****NDVI----
directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI"
files <- list.files(directory, pattern = "tif")
numfiles <- length(files)
ndvi_images <- c()

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  #Reading in raster
  dat <- raster(file_path)
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

#****Prey Abundance RSF data ----
data.rsf <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyAb_RSF_points.csv")


#Prey Abundance RSF----
print("Fitting prey abundance RSF")
#****Observed and Random Points ----

#****Fitting RSF----
RSF10.fit <- glm(Used ~ veg1m + slope + ndvi,
                 data = data.rsf,
                 family = binomial(link = "logit"))

#****Generating Predictive Maps----
print("Generating prey abundance maps")
pa_maps <- c()
numfiles <- length(ndvi_images)
for (i in 1:numfiles){
  img_name <- ndvi_images[[i]]
  ndvi_date <- names(ndvi_images)[i]
  ndvi_date <- gsub('-','',ndvi_date)
  ndvi <- get(toString(img_name))
  hab <- stack(ndvi,slope, veg1m)
  names(hab) <- c("ndvi",  "slope", "veg1m")
  #RSF Prediction Map
  pa_map <- terra::predict(object = hab, model = RSF10.fit, type = "response")
  names(pa_map) <- paste("PA_",ndvi_date,sep = "")
  #Assigning raster to variable and storing it in a dictionary
  var_name1 <- str_sub(ndvi_images[[i]], start= -8)
  var_name2 <- paste("pa_",var_name1,sep="")
  assign(var_name2, pa_map)
  year <- str_sub(var_name1, start = 1, end = 4)
  month <- str_sub(var_name1, start = 5, end = 6)
  day <- str_sub(var_name1, start = 7, end = 8)
  key <- paste(year,month,day,sep = "-")
  pa_maps[key] <- var_name2
  writeRaster(pa_map, paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/PreyAbundance/",
                            var_name2,
                            ".tif",
                            sep = ""),
              format = "GTiff",
              overwrite = TRUE)
}

#Lion encounter risk----

#****Normalized distEdge----
directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/Normalized_Distance_to_Edge"
files <- list.files(directory, pattern = "tif")
numfiles <- length(files)

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  #Reading in raster
  dat <- raster(file_path)
  #Assigning raster to variable and storing it in a dictionary
  var_name1 <- files[i]
  var_name2 <- paste(str_replace(var_name1,".tif",""))
  var_name3 <- paste(var_name2,"_NormDistEdge",sep="")
  assign(var_name3, dat)
}

#Dictionary used to call variable when fitting issa
NormDistEdges <- c("Babe" = Babe_NormDistEdge,
               "Fanbelt" = Fanbelt_NormDistEdge,
               "iHlane" = iHlane_NormDistEdge,
               "Koku" = Koku_NormDistEdge,
               "Madonna" = Madonna_NormDistEdge,
               "Mellon" = Mellon_NormDistEdge,
               "Murph" = Murph_NormDistEdge,
               "Ntombi" = Ntombi_NormDistEdge,
               "PokeJr" = PokeJr_NormDistEdge,
               "Roxy" = Roxy_NormDistEdge,
               "Stud" = Stud_NormDistEdge,
               "Thembi" = Thembi_NormDistEdge,
               "Zulu" = Zulu_NormDistEdge)

#****distEdge

directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/Distance_to_HR_Edge"
files <- list.files(directory, pattern = "tif")
numfiles <- length(files)

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  #Reading in raster
  dat <- raster(file_path)
  #Assigning raster to variable and storing it in a dictionary
  var_name1 <- files[i]
  var_name2 <- paste(str_replace(var_name1,".tif",""))
  var_name3 <- paste(var_name2,"_distEdge",sep="")
  assign(var_name3, dat)
}

#Dictionary used to call variable when fitting issa
distEdges <- c("Babe" = Babe_distEdge,
                   "Fanbelt" = Fanbelt_distEdge,
                   "iHlane" = iHlane_distEdge,
                   "Koku" = Koku_distEdge,
                   "Madonna" = Madonna_distEdge,
                   "Mellon" = Mellon_distEdge,
                   "Murph" = Murph_distEdge,
                   "Ntombi" = Ntombi_distEdge,
                   "PokeJr" = PokeJr_distEdge,
                   "Roxy" = Roxy_distEdge,
                   "Stud" = Stud_distEdge,
                   "Thembi" = Thembi_distEdge,
                   "Zulu" = Zulu_distEdge)
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

#Prey Catchability----
#****PC RSF Points----
pc_rsf <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyCatchability.csv")

RSF1.fit <- glm(Used ~ veg1m + slope,
                data = pc_rsf,
                family = binomial(link = "logit"))

#****PC Map----
pc_map <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_inputs/PreyCatchability.tif")
names(pc_map) <- "PreyCatchability"


