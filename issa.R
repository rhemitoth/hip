## ****************************************************
## Title: Issa Prep
## Author: Rhemi Toth
## Date Created: 10/26/2022
## Date Last Updated: 10/26/2022
## Email: rhemitoth@g.harvard.edu
##
## ****************************************************
##
## Notes:
## -Script adapted from:https://github.com/eco4cast/Statistical-Methods-Seminar-Series/blob/main/avgar-smith_issa/extra/quick_issa.R
##
## ****************************************************
##
## Loading Packages
library(tidyverse)
library(raster)
library(amt)
library(lubridate)
library(maptools)
library(ggplot2)
library(cowplot)
library(survival)
library(geosphere)
##
## ****************************************************

#source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/data_processing.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/issa_covs.R")

#Loading processed lion GPS data----
#Looping through csv files and importing them as a batch
#Each csv is assigned to a variable corresponding with the name of the Lion (e.g. Fluffy.csv is stored as Fluffy)
#Also created a list "lion_names" that can be used to loop through lion datasets later on
directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa"
files <- list.files(directory, pattern = "csv")
numfiles <- length(files)
lions <- list()

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  lion_name <- paste(str_replace(files[i],".csv",""))
  #Reading in csv
  dat <- read_csv(file_path)%>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y" ))
  #Cleaning up date field and adding a time stamp Field
  #dat$Date <- dat$Date %m+% years(2000)
  dat$TimeStamp <- as.POSIXct(paste(dat$Date, dat$Time), format="%Y-%m-%d %H:%M")
  #dat$TimeStamp <- dat$TimeStamp %m+% years(2000)
  #Setting time zone to South African Standard Time
  tz(dat$TimeStamp) <- "Africa/Johannesburg"
  #Assigning dataframe to a variable that is the lions name
  dat <- dat %>%
    distinct(.keep_all = TRUE)
  dat <- dat[is.na(dat$TimeStamp) == FALSE,]
  assign(lion_name, dat)
  #Creating a list of lion names that can be called later
  lions <- append(lions, lion_name)

}

numlions <- length(lions)

#Filtering For Relocations in Home Range Extent----
for(i in 1:numlions){
  lion_name <- lions[[i]]
  dat <- get(lion_name)
  hr <- homeranges[[lion_name]]
  coordinates(dat) <- ~ Easting + Northing
  crs.geo <- CRS("+init=epsg:32736")
  proj4string(dat) <- crs.geo
  filtered_dat <- dat[hr,]
  filtered_dat_df <- as.data.frame(filtered_dat)
  tz(filtered_dat_df$TimeStamp) <- "Africa/Johannesburg"
  assign(lions[[i]], filtered_dat_df)
}


#Random Steps ----

num_rand_steps <- 20
for (i in 1:numlions){
  lion_name <- lions[[i]]
  print(paste("Generating Random Steps for",lion_name))
  dat <- get(toString(lion_name))
  boundary <- homeranges[[lion_name]]
  #Fitting issa
  stps <- dat %>%
    arrange(TimeStamp)%>%
    make_track(.y = Northing,
               .x = Easting,
               .t = TimeStamp,
               Sex,
               ndvi_date,
               #Slope,
               #Veg1m,
               #PreyCatch,
               #PreyAb,
               #distEdge,
               id = LID,
               crs = 32736)%>%
    track_resample(rate = hours(1),tolerance = minutes(5))%>%
    steps_by_burst(keep_cols = "end")%>%
    random_steps(n_control = num_rand_steps)%>%
    mutate(log_sl_ = log(sl_),
           cos_ta_ = cos(ta_)) %>%
    time_of_day(where = "both", solar.dep = 0)

  sl_dist <- fit_distr(stps$sl_, "gamma")
  ta_dist <- fit_distr(stps$ta_, "vonmises")
  stps$rowID <- seq.int(nrow(stps))

  #Calculating initial intersect between raster extent and step ends
  stps_sp <- as.data.frame(stps)
  coordinates(stps_sp) <- ~x2_ + y2_
  crs.geo <- CRS("+init=epsg:32736")  # looks up UTM 33N
  proj4string(stps_sp) <- crs.geo
  intersect <- stps_sp[boundary,]

  #Regenerating random steps until all step ends are in the raster extent
  intersect_df <- as.data.frame(intersect)
  stps_df <- as.data.frame(stps)
  stps_df$in_bounds <- do.call(paste0,stps_df) %in% do.call(paste0, intersect_df)

  out_of_bounds_steps <- stps_df %>%
    select(step_id_, in_bounds)%>%
    filter(in_bounds == FALSE)%>%
    unique()%>%
    select(step_id_)

  if(nrow(out_of_bounds_steps) > 0){
    for(j in 1:nrow(out_of_bounds_steps)){
      print(paste("Correcting step ", toString(j), "/", toString(nrow(out_of_bounds_steps))))
      stpID <- out_of_bounds_steps$step_id_[j]
      bad_steps <- stps %>%
        filter(step_id_ == stpID & case_ == FALSE)

      obs_step <- stps %>%
        filter(step_id_ == stpID & case_ == TRUE)

      obs_step_sp <- as.data.frame(obs_step)
      coordinates(obs_step_sp) <- ~x1_ + y1_
      proj4string(obs_step_sp) <- crs.geo
      rand_sl <- random_numbers(sl_dist, 1)
      rand_ta <- random_numbers(ta_dist, 1)
      obs_step_sp_latlon <- spTransform(obs_step_sp, CRS("+proj=longlat +datum=WGS84") )
      dest_point_latlon <- as.data.frame(destPoint(obs_step_sp_latlon, rand_sl, rand_ta))
      coordinates(dest_point_latlon) <- ~ lon + lat
      proj4string(dest_point_latlon) <- CRS("+proj=longlat +datum=WGS84")
      dest_point_UTM <- as.data.frame(spTransform(dest_point_latlon,CRS("+init=epsg:32736")))

      for(k in 1:num_rand_steps){
        new_step <- bad_steps[k,]
        new_step$x2_ <- dest_point_UTM$lon
        new_step$y2_ <- dest_point_UTM$lat
        new_step_sp <- new_step
        coordinates(new_step_sp) <- ~x2_ + y2_
        crs.geo <- CRS("+init=epsg:32736")
        proj4string(new_step_sp) <- crs.geo
        int <- new_step_sp[boundary,]
        iter <- 1
        while(nrow(int) != 1){
          #print(paste("Iteration:", iter))
          rand_sl <- random_numbers(sl_dist, 1)
          rand_ta <- random_numbers(ta_dist, 1)
          obs_step_sp_latlon <- spTransform(obs_step_sp, CRS("+proj=longlat +datum=WGS84") )
          dest_point_latlon <- as.data.frame(destPoint(obs_step_sp_latlon, rand_sl, rand_ta))
          coordinates(dest_point_latlon) <- ~ lon + lat
          proj4string(dest_point_latlon) <- CRS("+proj=longlat +datum=WGS84")
          dest_point_UTM <- as.data.frame(spTransform(dest_point_latlon,CRS("+init=epsg:32736")))
          new_step <- bad_steps[k,]
          new_step$x2_ <- dest_point_UTM$lon
          new_step$y2_ <- dest_point_UTM$lat
          new_step_sp <- new_step
          coordinates(new_step_sp) <- ~x2_ + y2_
          crs.geo <- CRS("+init=epsg:32736")
          proj4string(new_step_sp) <- crs.geo
          int <- new_step_sp[boundary,]
          iter <- iter + 1
        }
        rowid <- bad_steps[k,]$rowID
        stps[rowid,] <- new_step
      }

    }
  }

  #Creating variable for steps
  assign(paste(toString(lion_name),"steps",sep="_"),stps)
  filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/steps/",
                    lion_name,
                    ".csv",
                    sep = "")
  write.csv(stps,filename, row.names = FALSE)
}

# Extracting covariates----

#****Slope, Veg1m,distEdge, and Prey Catch----
for(i in 1:numlions){
  lion_name <- lions[[i]]
  print(paste("Extracting slope, vegetation, prey catchability, and distance to edge for",lion_name))
  dat <- get(paste(toString(lion_name),"steps",sep = "_"))
  start <- dat %>% select("x1_","y1_")
  end <- dat %>% select("x2_", "y2_")
  dat$Slope_start <- raster::extract(slope,start)
  dat$Slope_end <- raster::extract(slope,end)
  dat$Veg1m_start <- raster::extract(veg1m, start)
  dat$Veg1m_end <- raster::extract(veg1m, end)
  dat$PreyCatchability_end <- raster::extract(pc_map,end)
  if(lion_name != "Fluffy"){
    d <- distEdges[[lion_name]]
    names(d) <- "distEdge"
    dat$distEdge_start <- raster::extract(d, start)
    dat$distEdge_end <- raster::extract(d, end)
  }
  print(lion_name)
  print("Count NAs")
  print(sum(is.na(dat$Slope_start)))
  print(sum(is.na(dat$Slope_end)))
  print(sum(is.na(dat$Veg1m_start)))
  print(sum(is.na(dat$Veg1m_end)))
  print(sum(is.na(dat$PreyCatchability_end)))
  if(lion_name != "Fluffy"){
    print(sum(is.na(dat$distEdge_start)))
    print(sum(is.na(dat$distEdge_end)))
  }
  assign(paste(toString(lion_name),"issa",sep="_"),dat)
}

#****Prey Abundance----
for(i in 1:numlions){
  lion_name <- lions[[i]]
  print(paste("Assigning temporally corresponding prey abundance for",lion_name))
  dat <- get(paste(lion_name,"issa",sep = "_"))
  dat$PreyAbundance_end <- 0.001
  coords <- dat%>%select(x2_,y2_)
  dat$pa_20140401 <- raster::extract(get(pa_maps[[1]]), coords)
  dat$pa_20140503 <- raster::extract(get(pa_maps[[2]]), coords)
  dat$pa_20140519 <- raster::extract(get(pa_maps[[3]]), coords)
  dat$pa_20140604 <- raster::extract(get(pa_maps[[4]]), coords)
  dat$pa_20140620 <- raster::extract(get(pa_maps[[5]]), coords)
  dat$pa_20140722 <- raster::extract(get(pa_maps[[6]]), coords)
  dat$pa_20140823 <- raster::extract(get(pa_maps[[7]]), coords)
  dat$pa_20150607 <- raster::extract(get(pa_maps[[8]]), coords)
  dat$pa_20150623 <- raster::extract(get(pa_maps[[9]]), coords)
  dat$pa_20150709 <- raster::extract(get(pa_maps[[10]]), coords)
  dat$pa_20150927 <- raster::extract(get(pa_maps[[11]]), coords)
  dat$pa_20151013 <- raster::extract(get(pa_maps[[12]]), coords)
  dat$pa_20151130 <- raster::extract(get(pa_maps[[13]]), coords)
  dat$pa_20151216 <- raster::extract(get(pa_maps[[14]]), coords)
  dat$pa_20160202 <- raster::extract(get(pa_maps[[15]]), coords)
  dat$pa_20160305 <- raster::extract(get(pa_maps[[16]]), coords)
  dat$pa_20160524 <- raster::extract(get(pa_maps[[17]]), coords)
  dat$pa_20160609 <- raster::extract(get(pa_maps[[18]]), coords)
  dat$pa_20160625 <- raster::extract(get(pa_maps[[19]]), coords)
  dat$pa_20160711 <- raster::extract(get(pa_maps[[20]]), coords)
  dat$pa_20160727 <- raster::extract(get(pa_maps[[21]]), coords)
  dat$pa_20160812 <- raster::extract(get(pa_maps[[22]]), coords)
  dat$pa_20160828 <- raster::extract(get(pa_maps[[23]]), coords)
  dat$pa_20161015 <- raster::extract(get(pa_maps[[24]]), coords)
  dat$pa_20170119 <- raster::extract(get(pa_maps[[25]]), coords)
  dat$pa_20170204 <- raster::extract(get(pa_maps[[26]]), coords)
  dat$pa_20170511 <- raster::extract(get(pa_maps[[27]]), coords)

  for(j in 1:length(ndvi_images)){

    print(paste(j,"/27",sep=""))
    date <- names(ndvi_images)[j]

    if(lion_name == "Fluffy"){
      col_index <- 27 + (j-1)
    }else{
      col_index <- 29 + (j-1)
    }

    dat <- dat %>%
      rowwise()%>%
      mutate(PreyAbundance_end = ifelse(toString(ndvi_date) == date,
                             cur_data()[[col_index]],
                             PreyAbundance_end))%>%
      ungroup()
  }

  if(lion_name == "Fluffy"){
    dat <- dat[,1:26]
  }else{
    dat <- dat[,1:28]
  }

  assign(toString(lion_name),dat)
  assign(paste(toString(lion_name),"issa",sep="_"),dat)
}

#Saving data----

for(i in 1:numlions){
  lion_name <- lions[[i]]
  dat <- get(paste(toString(lion_name),"issa",sep = "_"))
  filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_complete/",
                    lion_name,
                    ".csv",
                    sep ="")
  write.csv(dat,filename, row.names = FALSE)
}

#compare missing points
plot_points <- function(dat, hr){
  dat <- dat %>% filter(is.na(Slope_end)==TRUE)
  dat_sp <- dat
  coordinates(dat_sp) <- ~x2_ + y2_
  crs.geo <- CRS("+init=epsg:32736")  # looks up UTM 33N
  proj4string(dat_sp) <- crs.geo
  plot(dat_sp, col = "red", add = TRUE)
  plot(hr, add=TRUE)
}
