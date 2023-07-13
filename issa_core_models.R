## ****************************************************
## Title: iSSA Core Model Competition
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
library(sf)
##
## ****************************************************

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
  #Setting time zone to South African Standard Time
  tz(dat$TimeStamp) <- "Africa/Johannesburg"
  #Assigning dataframe to a variable that is the lions name
  dat <- dat %>%
    distinct(.keep_all = TRUE)
  assign(lion_name, dat)
  #Creating a list of lion names that can be called later
  lions <- append(lions, lion_name)
}

numlions <- length(lions)

#Loading Covariates ----
#****HiP----
hip <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/HiP_Boundary/boundary_edited.shp")

#****Dam Mask----
dam <- shapefile ("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/dam/dam.shp")

#****Slope----
slope <- raster("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/slope/slope.tif")%>%
  mask(mask = hip)%>%
  mask(mask = dam, inverse = TRUE)

hab <- stack(slope)
names(hab) <- c("slope")

#Fitting iSSA ----

for (i in 1:numlions){
  print(paste("Fitting iSSA for",lion_names[i]))
  dat <- get(toString(lion_names[i]))
  #ESPG code for UTM 36S is 32736
  issa <- dat %>%
    make_track(Easting,
               Northing,
               TimeStamp,
               Hour,
               Sex,
               timetotwilight,
               id = LID,
               crs = st_crs(32736))%>%
    track_resample(rate = hours(1),tolerance = minutes(5))%>%
    steps_by_burst(keep_cols = "end")%>%
    random_steps(n_control = 20)%>%
    mutate(log_sl_ = log(sl_),
    cos_ta_ = cos(ta_))%>%
    extract_covariates(hab)%>%
    time_of_day(where="both",
                solar.dep = 12,
                include.crepuscule = FALSE)
  assign(paste(toString(lion_names[i]),"_issa",sep = ""), issa)
}

# Core Model 1----
for (i in 1:numlions){
  print(paste("Fitting Model 1 for",lion_names[i]))
  dat_name <- paste(toString(lion_names[i]),"_issa",sep="")
  dat <- get(dat_name)
  # Fitting Model
  m1 <- fit_issf(dat,
                 #Response
                 case_ ~
                   #Movement
                   sl_ + cos_ta_ +
                   #Stratum
                   strata(step_id_),
                 model = TRUE)
  assign(paste("m1_",toString(lion_names[i]),sep = ""), m1)

}

# Core Model 2----
for (i in 1:numlions){
  print(paste("Fitting Model 2 for",lion_names[i]))
  dat_name <- paste(toString(lion_names[i]),"_issa",sep="")
  dat <- get(dat_name)
  # Fitting Model
  m2 <- fit_issf(dat,
                 #Response
                 case_ ~
                   #Movement
                 log_sl_ + cos_ta_ +
                   #Stratum
                   strata(step_id_),
                 model = TRUE)
  assign(paste("m2_",toString(lion_names[i]),sep = ""), m2)
}

# Core Model 3----
for (i in 1:numlions){
  print(paste("Fitting Model 3 for",lion_names[i]))
  dat_name <- paste(toString(lion_names[i]),"_issa",sep="")
  dat <- get(dat_name)
  # Fitting Model
  m3 <- fit_issf(dat,
                 #Response
                 case_ ~
                   #Movement
                   sl_ + log_sl_ + cos_ta_ +
                 #Stratum
                 strata(step_id_),
                 model = TRUE)
  assign(paste("m3_",toString(lion_names[i]),sep = ""), m3)
}

# Core Model 4----
for (i in 1:numlions){
  print(paste("Fitting Model 4 for",lion_names[i]))
  dat_name <- paste(toString(lion_names[i]),"_issa",sep="")
  dat <- get(dat_name)
  # Fitting Model
  m4 <- fit_issf(dat,
                 #Response
                 case_ ~
                   #Movement
                   log_sl_ + cos_ta_ + log_sl_:timetotwilight +
                 #Stratum
                 strata(step_id_),
                 model = TRUE)
  assign(paste("m4_",toString(lion_names[i]),sep = ""), m4)
}


# Core Model 5----
for (i in 1:numlions){
  print(paste("Fitting Model 5 for",lion_names[i]))
  dat_name <- paste(toString(lion_names[i]),"_issa",sep="")
  dat <- get(dat_name)
  # Fitting Model
  m5 <- fit_issf(dat,
                 #Response
                 case_ ~
                   #Movement
                   log_sl_ + cos_ta_ + log_sl_:timetotwilight + slope:log_sl_+
                   #Stratum
                   strata(step_id_),
                 model = TRUE)
  assign(paste("m5_",toString(lion_names[i]),sep = ""), m5)
}

# Core Model 6----
for (i in 1:numlions){
  print(paste("Fitting Model 6 for",lion_names[i]))
  dat_name <- paste(toString(lion_names[i]),"_issa",sep="")
  dat <- get(dat_name)
  # Fitting Model
  m6 <- fit_issf(dat,
                 #Response
                 case_ ~
                   #Movement
                   log_sl_ + cos_ta_ + log_sl_:timetotwilight + slope:log_sl_+ slope:cos_ta_+
                   #Stratum
                   strata(step_id_),
                 model = TRUE)
  assign(paste("m6_",toString(lion_names[i]),sep = ""), m6)
}

#AIC----

babeAIC <- AIC(m1_Babe$model,
    m2_Babe$model,
    m3_Babe$model,
    m4_Babe$model,
    m5_Babe$model,
    m6_Babe$model)%>%
  arrange(AIC)

FanbeltAIC <- AIC(m1_Fanbelt$model,
               m2_Fanbelt$model,
               m3_Fanbelt$model,
               m4_Fanbelt$model,
               m5_Fanbelt$model,
               m6_Fanbelt$model)%>%
  arrange(AIC)

FluffyAIC <- AIC(m1_Fluffy$model,
                  m2_Fluffy$model,
                  m3_Fluffy$model,
                  m4_Fluffy$model,
                  m5_Fluffy$model,
                  m6_Fluffy$model)%>%
  arrange(AIC)

ZuluAIC <- AIC(m1_Zulu$model,
                 m2_Zulu$model,
                 m3_Zulu$model,
                 m4_Zulu$model,
                 m5_Zulu$model,
                 m6_Zulu$model)%>%
  arrange(AIC)
