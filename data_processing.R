## ****************************************************
## Title: HiP Lion iSSA Data Processing Script
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
library(amt)
library(lubridate)
library(maptools)
##
## ****************************************************
# Loading Data ----
#
#Looping through csv files and importing them as a batch
#Each csv is assigned to a variable corresponding with the name of the Lion (e.g. Fluffy.csv is stored as Fluffy)
#Also created a list "lion_names" that can be used to loop through lion datasets later on

print(paste("Loading data"))
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
  #Hour of Day Column
  dat$TimeChr <- format(dat$TimeStamp, format = "%H:%M")
  dat$Hour <- sapply(strsplit(dat$TimeChr,":"),
                     function(x) {
                       x <- as.numeric(x)
                       x[1]
                     }
  )
  #Assigning dataframe to a variable that is the lions name
  dat <- dat %>%
    distinct(.keep_all = TRUE)
  assign(lion_name, dat)
  #Creating a list of lion names that can be called later
  lion_names <- append(lion_names, lion_name)
}

numlions <- length(lion_names)

#Duplicate Timestamp Check----
#
#Will get a message in the console with the lion name and duplicate timestamps (if they exist)

for (i in 1:numlions){
  print(paste("Checking for duplicate timestamps",lion_names[i]))
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
  print(paste("Identifying repeat locaitons for",lion_names[i]))
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

#Time to Twilight----

 for(i in 1:numlions){
   print(paste("Calculating time to twilight for",lion_names[i]))
   dat <- get(toString(lion_names[i]))%>%
     arrange(TimeStamp)%>%
     mutate(DayNight = "")
   dat$nautical_dawn <- as.POSIXct(paste(dat$Date, dat$Time), format="%Y-%m-%d %H:%M:%S")
   tz(dat$nautical_dawn) <- "Africa/Johannesburg"
   dat$nautical_dusk <- as.POSIXct(paste(dat$Date, dat$Time), format="%Y-%m-%d %H:%M:%S")
   tz(dat$nautical_dusk) <- "Africa/Johannesburg"
   dat$timetotwilight <- 0.001
   records <- nrow(dat)
   for(j in 1:length(dat$LID)){
     if((j/records*100)%%10 == 0){
       print(paste(toString(j/records*100),"%"))
     }

     coords_raw <- matrix(c(dat$Longitude[j],dat$Latitude[j]),
                          nrow = 1)
     coords <- SpatialPoints(coords_raw,
                             proj4string=CRS("+proj=longlat +datum=WGS84"))

     relocation_date <- dat$TimeStamp[j]
     print(relocation_date)
     nautical_dawn <- crepuscule(coords,
                                 relocation_date,
                                 solarDep=12,
                                 direction="dawn",
                                 POSIXct.out=TRUE)
     dawn <- nautical_dawn$time

     nautical_dusk <- crepuscule(coords,
                                     relocation_date,
                                     solarDep=12,
                                     direction="dusk",
                                     POSIXct.out=TRUE)
     dusk <- nautical_dusk$time

     dat$nautical_dusk[j] <- dusk
     dat$nautical_dawn[j] <- dawn

     timetodusk <- abs(as.numeric(dusk - relocation_date, units = "secs"))
     timetodawn <- abs(as.numeric(dawn - relocation_date, units = "secs"))
     times <- c(timetodawn,timetodusk)
     index <- which.min(times)
     ttt <- times[index]
     dat$timetotwilight[j] <- ttt/60/60

   }
   assign(toString(lion_names[i]), dat)
 }

#Sex of Lion Column----
sex <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_raw/lion_sexes.csv")
for(i in 1:numlions){
  print(paste("Adding sex of lion column for",lion_names[i]))
  dat1 <- get(toString(lion_names[i]))
  dat2 <- merge(x = dat1,
                y = sex,
                by = "LID",
                all.x=TRUE)
  assign(toString(lion_names[i]), dat2)
}

#NDVI Date Match----

ndvi_dates <- read_csv("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/GEE_NDVI/date_index.csv")%>%
  arrange(ImageDate)
for(i in 1:numlions){
  print(paste("Matching dates for",lion_names[i]))
  dat <- get(toString(lion_names[i]))%>%
    mutate(ndvi_date = as.Date("1900-01-01", format = "%Y-%m-%d"),
           ndviImage = "")
  for(j in 1:nrow(dat)){
    dateObs <- dat$Date[j]
    dateList <- ndvi_dates$ImageDate
    index <- which.min(abs(dateList - dateObs))
    dat$ndvi_date[j] <- dateList[index]
    dat$ndviImage[j] <- ndvi_dates$URL[index]
  }
  assign(toString(lion_names[i]), dat)
}

#Removing records with incorrect dates----
for(i in 1:numlions){
  print(paste("Removing records with incorrect dates for",lion_names[i]))
  dat <- get(toString(lion_names[i]))
  dat_filtered = dat[dat$Date >= '2014-05-01',]
  assign(toString(lion_names[i]), dat_filtered)
}


#Exporting processed data----
for(i in 1:numlions){
  print(paste("Exporting data for",lion_names[i]))
  dat <- get(toString(lion_names[i]))%>%
    arrange(TimeStamp)
  lion_name <- lion_names[i]
  filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa/",
                    lion_name,
                    ".csv",
                    sep = "")
  write.csv(dat,filename, row.names = FALSE)
}

print("Done")


