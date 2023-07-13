## ****************************************************
## Title: Data Exploration
## Author: Rhemi Toth
## Date Created: 10/18/2022
## Date Last Updated: 10/26/2022
## Email: rhemitoth@g.harvard.edu
##
## ****************************************************
##
## Notes:
## -run issa.R before this script
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
library(circular)
library(move)
library(basemaps)
library(mapedit)
library(ggmap)
library(pals)
##
## ****************************************************

#Loading Data from issa.R----
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/issa.R")

#Number of bursts per lion----
burst_summary <- tibble(Index = 1:numlions,
                        Lion = "NA",
                        num_bursts = 0,
                        min_burst_len = 0,
                        max_burst_len = 0,
                        mean_burst_len = 0,
                        median_burst_len = 0,
                        Units = "Hours")

for(i in 1:numlions){
  #Burst summary for each lion
  file <- paste(lion_names[i],
                "_trk",
                sep = "")
  dat <- get(file)
  bursts <- dat %>%
    group_by(burst_)%>%
    summarize(start = min(t_),
              stop = max(t_))%>%
    mutate(LengthHours = round(as.numeric(difftime(stop,start,units = "hours"),units = "hours"), digits = 1))
  filename <- paste(lion_names[i],"_bursts",sep="")
  file_path <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/burst_summary/",
                     filename,
                     ".csv",sep="")
  assign(filename,bursts)
  write.csv(bursts,file_path, row.names = FALSE)

  #Burst summary for all lions
  burst_summary$Lion[i] <- toString(lion_names[i])
  burst_summary$num_bursts[i] <- length(bursts$burst_)
  burst_summary$min_burst_len[i] <- min(bursts$LengthHours)
  burst_summary$max_burst_len[i] <- max(bursts$LengthHours)
  burst_summary$mean_burst_len[i] <- round(mean(bursts$LengthHours),digits = 1)
  burst_summary$median_burst_len[i] <- median(bursts$LengthHours)
}

write.csv(as.data.frame(burst_summary),
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/burst_summary/burst_summary.csv",
          row.names = FALSE)

#Step Length Histograms----

for(i in 1:numlions){
  file <- paste(lion_names[i],
                "_stp",
                sep = "")
  dat <- get(toString(file))
#splitting data into day and night combined and night only
  daynight <- dat
  night <- dat%>%
    filter(DayNight == "Night")
  day <- dat %>%
    filter(DayNight == "Day")
#Generating x bounds for plot axes
  max_sl <- max(dat$log(sl_))
  min_sl <- min(dat$log(sl_))

#generating y bounds for histograms
  bin_width = 100
  p_daynight <- ggplot(daynight, aes(x = log(sl_)))+
    geom_histogram(binwidth = bin_width, boundary = 0)+
    xlim(NA,max_sl)
  p_daynight_build <- ggplot_build(p_daynight)
  y_max <- max(p_daynight_build$data[[1]]$count)*1.1
 #Generating Histograms
  p_daynight <- ggplot(daynight, aes(x = log(sl_)))+
    geom_histogram(binwidth = bin_width, boundary = 0)+
    xlim(NA,max_sl)+
    ylim(0,y_max)
  p_night <- ggplot(night, aes(x = log(sl_)))+
    geom_histogram(binwidth = bin_width, boundary = 0)+
    xlim(NA,max_sl)+
    ylim(0,y_max)
  p_day <- ggplot(day, aes(x = log(sl_)))+
    geom_histogram(binwidth = bin_width, boundary = 0)+
    xlim(NA,max_sl)+
    ylim(0,y_max)
  p <- plot_grid(p_daynight, p_day,p_night, ncol = 1, nrow = 3, labels = c("Day and Night", "Day Only","Night Only"), label_x = 0.5 )
  filepth <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/step_length_dist/"
  filenm <- paste(filepth,lion_names[i], "_step_length_dist.jpeg", sep = "")
  ggsave(filename = filenm,
         plot = p,
         device = "jpeg")
}

#Parameterized Gamma Distribution Step Length Graphs----
for(i in 1:numlions){
  file <- paste(lion_names[i],
                "_randomstps",
                sep = "")
  dat <- get(toString(file))
  filepth <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/parameterized_step_length_dist/"
  filenm <- paste(filepth,lion_names[i], "_parameterized_step_length_dist.jpeg", sep = "")
  jpeg(file = filenm, width = 500, height = 500)
  p <- plot_sl((dat))
  dev.off()
}

#Turning Angle Histograms----
for(i in 1:numlions){
  file <- paste(lion_names[i],
                "_randomstps",
                sep = "")
  dat <- get(toString(file))
  p <- plot_ta(dat)
  filepth <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/turning_angle_dist/"
  filenm <- paste(filepth,lion_names[i], "_turning_angle_dist.jpeg", sep = "")
  ggsave(filename = filenm,
         plot = p,
         device = "jpeg")
}

#Parameterized Von Mises Distribution Turning Angle Graphs----
for(i in 1:numlions){
  file <- paste(lion_names[i],
                "_stp",
                sep = "")
  dat <- get(toString(file))
  #splitting data by night and day and night
  daynight <- dat
  night <- dat%>%
    filter(DayNight == "Night")
  #fitting gamma distributions to data
  vm_daynight <- fit_distr(daynight$ta_, "vonmises")
  vm_night <- fit_distr(night$ta_,"vonmises")
  #generating plots
  p <- ggplot()+
    geom_line(data = daynight,
              aes(x = ta_,
                  y = dvonmises(ta_,
                             kappa = as.numeric(vm_daynight$params[1]),
                             mu = as.numeric(vm_daynight$params[2]))),
              color = "orange")+
    geom_line(data = night,
              aes(x = ta_,
                  y = dvonmises(ta_,
                             kappa = as.numeric(vm_night$params[1]),
                             mu = as.numeric(vm_night$params[2]))),
              color = "blue")+
    labs(x = "Turning Angle (degrees)",
         y = "Density")+
    ggtitle(paste(lion_names[i], "Parameterized Turning Angle Distribution"))
  filepth <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/parameterized_turning_angle_dist/"
  filenm <- paste(filepth,lion_names[i], "_parameterized_turning_angle_dist.jpeg", sep = "")
  ggsave(filename = filenm,
         plot = p,
         device = "jpeg")
}

#Step Length vs Time of Day Plots----
for(i in 1:numlions){
  file <- paste(lion_names[i],
                "_stp",
                sep = "")
  dat <- get(toString(file))
  p <- dat %>%
    ggplot(aes(x = log(sl_), fill = DayNight))+
    geom_density(alpha = 0.5)+
    labs(x = "Step Length (m)",
         y = "Density")+
    ggtitle(paste(lion_names[i], "Step Length PDF"))
  filepth <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/step_length_tod/"
  filenm <- paste(filepth,lion_names[i], "_step_length.jpeg", sep = "")
  ggsave(filename = filenm,
         plot = p,
         device = "jpeg")
}

#Turning Angle vs Time of Day Plots----
for(i in 1:numlions){
  file <- paste(lion_names[i],
                "_stp",
                sep = "")
  dat <- get(toString(file))
  p <- dat %>%
    ggplot(aes(x = ta_, fill = DayNight))+
    geom_density(alpha = 0.5)+
    labs(x = "Turning Angle (Radians)",
         y = "Density")+
    ggtitle(paste(lion_names[i], "Turning Angle PDF"))
  filepth <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/turning_angle_tod/"
  filenm <- paste(filepth,lion_names[i], "_turning_angle.jpeg", sep = "")
  ggsave(filename = filenm,
         plot = p,
         device = "jpeg")
}

#Steps by burst plot ----
for(i in 1:numlions){
  file <- paste(lion_names[i],
                "_stp",
                sep = "")
  dat <- get(toString(file))
  n_bursts <- max(dat$burst_)
  p <- ggplot()+
    geom_segment(data = dat,
                 aes(x = x1_,
                     y = y1_,
                     xend = x2_,
                     yend = y2_,
                     color = burst_))+
    labs(x = "UTM Easting",
         y = "UTM Northing")+
    ggtitle(paste(lion_names[i], "Steps By Burst"))
  filepth <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/steps_by_burst/"
  filenm <- paste(filepth,lion_names[i], "_steps_by_burst.jpeg", sep = "")
  ggsave(filename = filenm,
         plot = p,
         device = "jpeg")
}

#Mapping temporal and spatial overlap of relocations----

#Assigning wet/dry season category to relocation dates
#Dry season = April - September, Wet Season = October - March
relocation_dates_list <- seq(as.Date("2014-05-28"), as.Date("2017-04-26"), by = "days")

relocation_dates <- tibble(Date = relocation_dates_list)

relocation_dates <- relocation_dates%>%
  mutate(DateClass = "some text")

for(i in 1:length(relocation_dates_list)){
  if(relocation_dates$Date[i] <= as.Date("2014-09-30")){
    res <- "2014 Dry Season"
  }
  else if (relocation_dates$Date[i] <= as.Date("2015-03-31")){
    res <- "2014-2015 Wet Season"
  }
  else if (relocation_dates$Date[i] <= as.Date("2015-09-30")){
    res <- "2015 Dry Season"
  }
  else if (relocation_dates$Date[i] <= as.Date("2016-03-31")){
    res <- "2015-2016 Wet Season"
  }
  else if (relocation_dates$Date[i] <= as.Date("2016-09-30")){
    res <- "2016 Dry Season"
  }
  else if (relocation_dates$Date[i] <= as.Date("2017-03-31")){
    res <- "2016-2017 Wet Season"
  }
  else if (relocation_dates$Date[i] <= as.Date("2017-09-30")){
    res <- "2017 Dry Season"
  }
  relocation_dates$DateClass[i] <- res
}

for(i in 1:numlions){
  file <- paste(lion_names[i])
  dat <- get(toString(file))
  dat <- merge(x = dat, y = relocation_dates, by = "Date", all = FALSE)
  assign(paste(lion_names[i],"_","plot",sep=""),dat)
}

#size of points on map
sz = 0.4

#Map of relocations by season
p <- ggplot()+
  geom_point(data = Babe_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Fanbelt_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Fluffy_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = iHlane_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Koku_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Madonna_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Mellon_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Murph_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Ntombi_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = PokeJr_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Roxy_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Stud_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Thembi_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  geom_point(data = Zulu_plot,aes(x = Easting, y = Northing, color = LID, shape = Sex),
             size = sz)+
  facet_wrap(~DateClass)+
  scale_color_manual(values = as.vector(polychrome()))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=5)),
         shape = guide_legend(override.aes = list(size=5)))+
  labs(title = "Relocations by Season", caption = "The dry season is defined as April - September and the wet season is defined as October - March")

ggsave(plot = p,
       filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/spatiotemporal_dynamics.jpeg",
       width = 11,
       height = 8,
       units = "in",
       dpi = 350)

# NDVI----
directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/NDVI"
files <- list.files(directory, pattern = "tiff")
numfiles <- length(files)
ndvi_imgs <- list()

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  img_name <- paste(str_replace(files[i],".tiff",""))
  #Reading in csv
  img <- raster(file_path)
  #Assigning dataframe to a variable that is the lions name
  assign(img_name, img)
  #Creating a list of lion names that can be called later
  ndvi_imgs <- append(ndvi_imgs, img_name)
}

num_imgs <- length(ndvi_imgs)

ndvi <- stack(ndvi20140722,ndvi20140807,ndvi20140823,ndvi20140908,ndvi20140924,average_ndvi_2014)

ndvi_fig <- plot(ndvi, zlim = c(-0.2,1))

ndvi_fig

# TOD vs Step Length----
for (i in 1:numlions){
  dat <- get(paste(toString(lion_names[i]),"_issa",sep = ""))
  dat_sum <- dat%>%
    mutate(log_sl = log(sl_))%>%
    group_by(Hour,id, Sex)%>%
    summarise(mean_log_sl = mean(log_sl))
  assign(paste(toString(lion_names[i]),"_issa_sum",sep = ""), dat_sum)
}
p <- ggplot()+
  geom_line(data = Babe_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Fanbelt_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Fluffy_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = iHlane_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Koku_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Madonna_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Mellon_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Murph_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Ntombi_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = PokeJr_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Roxy_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Stud_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Thembi_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  geom_line(data = Zulu_issa_sum,aes(x = Hour, y = mean_log_sl, color = id, linetype = Sex))+
  theme_classic()+
  labs(title = "Mean Log Step Length vs Time of Day",
       x = "Hour of Day",
       y = "Mean Step Length")

# TOD vs TA----
for (i in 1:numlions){
  dat <- get(paste(toString(lion_names[i]),"_issa",sep = ""))
  dat_sum <- dat%>%
    na.omit()%>%
    mutate(cos_ta = cos(ta_))%>%
    group_by(Hour,id, Sex)%>%
    summarise(mean_cos_ta = mean(cos_ta))
  assign(paste(toString(lion_names[i]),"_issa_sum",sep = ""), dat_sum)
}
p <- ggplot()+
  geom_line(data = Babe_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Fanbelt_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Fluffy_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = iHlane_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Koku_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Madonna_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Mellon_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Murph_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Ntombi_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = PokeJr_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Roxy_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Stud_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Thembi_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  geom_line(data = Zulu_issa_sum,aes(x = Hour, y = mean_cos_ta, color = id, linetype = Sex))+
  theme_classic()+
  labs(title = "Mean Cos(Turing Angle) vs Time of Day",
       x = "Hour of Day",
       y = "Mean Cos(Turning Angle)")

#Map of lion relocations
pts <- all_lions %>%
  filter(case_ == TRUE)%>%
  st_as_sf(coords = c("x1_","y1_"), crs = 32736 )%>%
  st_cast("POINT")

basemap <- tm_shape(hip)+
  tm_fill(col = "darkgrey")

map <- basemap +
  tm_compass(type = "4star", size = 2, position = c("right", "top"))+
  tm_scale_bar(breaks = c(0, 3, 6), text.size = 1)+
  tm_layout(main.title = "Used vs Available Points",
            fontface = "italic",
            fontfamily = "serif",
            legend.position = c("left", "top"),
            legend.text.size = 1,
            legend.title.size = 1.5,
            legend.frame = TRUE)

issa_design <- ggplot()+
  geom_raster(data = slope, aes(x = x, y = y, fill = Slope))+
  geom_point(data = all_lions[63,], aes(x = ))

#all lion plots----

directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_complete"
files <- list.files(directory, pattern = "csv")
numfiles <- length(files)
lions <- list()

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  lion_name <- paste(str_replace(files[i],".csv",""))
  #Reading in csv
  dat <- read_csv(file_path)
  var_name <- paste(lion_name,"_issa",sep = "")
  assign(var_name, dat)
  #Creating a list of lion names that can be called later
  lions <- append(lions, lion_name)
}
numlions <- length(lions)

Fluffy_issa <- Fluffy_issa %>%
  mutate(distEdge_start = NA,
         distEdge_end = NA)

all_lions <- rbind(Babe_issa,
                   Fanbelt_issa,
                   Fluffy_issa,
                   iHlane_issa,
                   Koku_issa,
                   Madonna_issa,
                   Mellon_issa,
                   Ntombi_issa,
                   Murph_issa,
                   PokeJr_issa,
                   Roxy_issa,
                   Stud_issa,
                   Thembi_issa,
                   Zulu_issa)
ggplot(data = all_lions, aes(x = x1_, y = y1_, color = id))+
  geom_point(size = 0.5)

write.csv(x = all_lions,
          file = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/lion_locs.csv")
