## ****************************************************
## Title: Home Range Estiamte Data Processing Script
## Author: Rhemi Toth
## Date Created: 10/17/2022
## Date Last Updated: 10/18/2022
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
library(tidyverse)
library(raster)
library(lubridate)
library(maptools)
library(ctmm)
library(move)
##
## ****************************************************

#Loading Data----

directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_raw/converted_coords"
files <- list.files(directory, pattern = "csv")
numfiles <- length(files)
lion_names <- list()

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  lion_name <- paste(str_replace(files[i],".csv",""))
  #Reading in csv
  dat <- read_csv(file_path)%>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y" ))
  #Cleaning up date field and adding a time stamp Field
  #dat$Date <- dat$Date %m+% years(2000)
  dat$TimeStamp <- as.POSIXct(paste(dat$Date, dat$Time), format="%Y-%m-%d %H:%M:%S")
  #Setting time zone to South African Standard Time
  tz(dat$TimeStamp) <- "Africa/Johannesburg"
  #Assigning dataframe to a variable that is the lions name
  dat <- dat %>%
    distinct(.keep_all = TRUE)
  assign(lion_name, dat)
  #Creating a list of lion names that can be called later
  lion_names <- append(lion_names, lion_name)
}

numlions <- length(lion_names)

#Filtering Zulu data so that Zulu is a range resident
Zulu <- Zulu %>%
  filter(TimeStamp >= as.Date("2014-10-16 00:00:00") & TimeStamp <= as.Date("2015-06-20 00:00:00"))

#Filtering Fluffy data so that Fluffy is a range resident



#Duplicate Timestamp Check----
#
#Will get a message in the console with the lion name and duplicate timestamps (if they exist)

for (i in 1:numlions){
  dat <- get(toString(lion_names[i]))%>%
    arrange(TimeStamp)%>%
    mutate(DuplicateTimeStampYN = FALSE)
  for(j in 1:length(dat$LID)){
    if (j > 1){
      if(dat$TimeStamp[j] == dat$TimeStamp[j-1]){
        print(paste("Duplicate time stamp:",dat$LID[j],dat$TimeStamp[j]))
        dat$DuplicateTimeStampYN[j] <- TRUE
      }
    }
  }
  dat <- dat%>%
    filter(DuplicateTimeStampYN == FALSE)
  assign(toString(lion_names[i]), dat)
}

#Repeated Locations Check----
#
#Check if there are repeated coordinates at consecutive relocations
#(i.e. animal did not move between relocations)

for (i in 1:numlions){
  dat <- get(toString(lion_names[i]))%>%
    arrange(TimeStamp)%>%
    mutate(LionMovedYN = TRUE)
  for (j in 1:length(dat$TimeStamp)){
    if (j > 1){
      if(dat$Latitude[j] == dat$Latitude[j-1] & dat$Longitude[j] == dat$Longitude[j-1]){
        #print(paste(lion_names[i],"remained at",dat$Latitude[j],dat$Longitude[j],"between",dat$TimeStamp[j-1],dat$TimeStamp[j]))
        dat$LionMovedYN[j] <- FALSE
      }
    }
  }
  dat <- dat%>%
    filter(LionMovedYN == TRUE)
  assign(toString(lion_names[i]), dat)
}

#Remove Records with Extraneous Dates----

for (i in 1:numlions){
  dat <- get(toString(lion_names[i]))
  new_dat <- dat[dat$TimeStamp > '2014-01-01 00:0:00',]
  assign(toString(lion_names[i]), new_dat)
}

#Making telemetry object----
#1. Use ctmm compatible column names
#2. Convert df to track
#3. Convert track to movebank object
#4. Convert movebank object to telemtry object
for (i in 1:numlions){
  dat <- get(toString(lion_names[i]))
  dat <- dat%>%
    filter(LionMovedYN == TRUE,
           DuplicateTimeStampYN == FALSE)
  animal_name <- dat$LID[1]
  print(animal_name)
  new_dat <- dat%>%
    dplyr::select(c("LID","TimeStamp", "Easting", "Northing"))%>%
    rename("ID" = "LID",
           "timestamp" = "TimeStamp",
           "UTM.Easting" = "Easting",
           "UTM.Northing" = "Northing")%>%
    mutate("UTM.zone" = "36S")
  move_objct <- move(x = new_dat$UTM.Easting,
                     y = new_dat$UTM.Northing,
                     time = as.POSIXct(new_dat$timestamp, format="%Y-%m-%d %H:%M:%S", tz="Africa/Johannesburg"),
                     data = new_dat,
                     proj = CRS("+init=epsg:32736"),
                     animal = animal_name,
                     sensor = "GPS")
  telem_objct <- as.telemetry(move_objct)
  new_name <- paste(toString(lion_names[i]),"telem", sep = "_")
  assign(new_name, telem_objct)
}



#Filtering Fluffy data so that Fluffy is a range resident
