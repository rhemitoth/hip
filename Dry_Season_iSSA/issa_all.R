## ****************************************************
## Title: Issa All
## Author: Rhemi Toth
## Date Created: 04/11/2023
## Email: rhemitoth@g.harvard.edu
##
## ****************************************************
##
## Notes:
## Calls on scripts that fit models for all lions and makes summary tables
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

#Loading processed lion GPS data----
#Looping through csv files and importing them as a batch
#Each csv is assigned to a variable corresponding with the name of the Lion (e.g. Fluffy.csv is stored as Fluffy)
#Also created a list "lion_names" that can be used to loop through lion datasets later on
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

#Filtering dates for 2014 dry season
lions2014 <- list()
for(i in 1:numlions){
  lion_name <- lions[[i]]
  var_name <- paste(lion_name,"_issa",sep = "")
  dat <- get(var_name)
  start_date2014 <- as.POSIXct("2014-07-12 00:00:00",format="%Y-%m-%d %H:%M:%S")
  end_date2014 <- as.POSIXct("2014-09-19 00:00:00",format="%Y-%m-%d %H:%M:%S")
  start_date2015 <- as.POSIXct("2015-07-12 00:00:00",format="%Y-%m-%d %H:%M:%S")
  end_date2015 <- as.POSIXct("2015-09-19 00:00:00",format="%Y-%m-%d %H:%M:%S")
  start_date2016 <- as.POSIXct("2016-07-12 00:00:00",format="%Y-%m-%d %H:%M:%S")
  end_date2016 <- as.POSIXct("2016-09-19 00:00:00",format="%Y-%m-%d %H:%M:%S")
  dat <- dat %>%
    filter(t1_ > start_date2014 & t2_ < end_date2014 |
             t1_ > start_date2015 & t2_ < end_date2015|
             t1_ > start_date2016 & t2_ < end_date2016)
  if(nrow(dat) > 0){
    assign(var_name, dat)
    lions2014 <- append(lions2014,lion_name)
  }
}

Fluffy_issa <- Fluffy_issa %>%
  mutate(distEdge_end = NA,
         distEdge_start = NA)

#All lions except Fluffy (fluffy has no dist edge data)
all_lions <- rbind(Babe_issa,
                   iHlane_issa,
                   Fluffy_issa,
                   Murph_issa,
                   PokeJr_issa,
                   Thembi_issa,
                   Zulu_issa)

#Fitting models for each lion----


source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Babe_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Fanbelt_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Koku_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Madonna_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Ntombi_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/iHlane_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/PokeJr_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Thembi_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Zulu_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Murph_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Fluffy_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Roxy_issa.R")
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Stud_issa.R")

#All Lion AIC Table

directory <- "~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Model_Competition/"
files <- list.files(directory, pattern = "csv")
numfiles <- length(files)

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  lion_name <- paste(str_replace(files[i],"AIC.csv",""))
  lion_name <- gsub(" ","",lion_name)
  #Reading in csv
  dat <- read_csv(file_path)
  var_name <- paste(lion_name,"_AIC",sep = "")
  assign(var_name, dat)
  #Creating a list of lion names that can be called later
  lions <- append(lions, lion_name)
}

lion_aics <- rbind(Babe_AIC,
                   Fanbelt_AIC,
                   Fluffy_AIC,
                   iHlane_AIC,
                   Koku_AIC,
                   Madonna_AIC,
                   Murph_AIC,
                   Ntombi_AIC,
                   PokeJr_AIC,
                   Roxy_AIC,
                   Stud_AIC,
                   Thembi_AIC,
                   Zulu_AIC)

aic_pivot <- lion_aics%>%
  pivot_wider(
    names_from = lion,
    values_from = AIC
  )

aic_plot <- ggplot(data = lion_aics, aes(x = lion, y = AIC, color = Model))+
  geom_point()
write.csv(aic_pivot,
          file = "~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/AICS.csv",
          col.names = FALSE)

winning_models <- rbind(Babe_winner%>%as.data.frame(),
                        Fanbelt_winner%>%as.data.frame(),
                        iHlane_winner%>%as.data.frame(),
                        Koku_winner%>%as.data.frame(),
                        Madonna_winner%>%as.data.frame(),
                        Murph_winner%>%as.data.frame(),
                        Ntombi_winner%>%as.data.frame(),
                        PokeJr_winner%>%as.data.frame(),
                        Roxy_winner%>%as.data.frame(),
                        Stud_winner%>%as.data.frame(),
                        Thembi_winner%>%as.data.frame(),
                        Zulu_winner%>%as.data.frame(),
                        Fluffy_winner%>%as.data.frame())

winning_models_sum <- winning_models %>%
  group_by(Model) %>%
  summarize(count = n())

print(winning_models_sum)

winning_models_dict <- c("Babe" = "Babe_model",
                         "Fanbelt" = "Fanbelt_model",
                         "iHlane" = 'iHlane_model',
                         "Koku" = "Koku_model",
                         "Madonna" = "Madonna_model",
                         "Murph" = "Murph_model",
                         "Ntombi" = "Ntombi_model",
                         "PokeJr" = 'PokeJr_model',
                         "Roxy" = "Roxy_model",
                         "Stud" = "Stud_model",
                         "Thembi" = 'Thembi_model',
                         "Zulu" = 'Zulu_model',
                         "Fluffy" = 'Fluffy_model')


sex <- c("iHlane" = "Female",
         "PokeJr" = "Female",
         "Babe" = "Female",
         "Thembi" = "Female",
         "Ntombi" = "Female",
         "Roxy" = "Female",
         "Madonna" = "Female",
         "Zulu" = "Male",
         "Koku" = "Male",
         "Murph" = "Male",
         "Fluffy" = "Male",
         "Mellon" = "Male",
         "Stud" = "Male",
         "Fanbelt" = "Male")

#Beta coefficients----
for(i in 1:length(lions2014)){
  lion_name <- lions2014[[i]]
  model <- get(winning_models_dict[lion_name])
  dat_coefs <- as.data.frame(summary(model)$coefficients)
  dat_coefs$Variable <- row.names(dat_coefs)
  rownames(dat_coefs) <- NULL
  n_rows <- nrow(dat_coefs)

 dat_coefs_df <- tibble(Lion = rep(lion_name,n_rows),
                          Sex = rep(sex[[lion_name]],n_rows),
                          Variable = dat_coefs["Variable"],
                          Beta = dat_coefs["coef"],
                        High_CI = dat_coefs["coef"] + dat_coefs["se(coef)"],
                        Low_CI = dat_coefs["coef"] - dat_coefs["se(coef)"],
                          P_val = dat_coefs["Pr(>|z|)"])%>%
    mutate(Significant = case_when(P_val < 0.001 ~ "p < 0.001",
                                   P_val < 0.01 ~ "p < 0.01",
                                   P_val < 0.05 ~ "p < 0.05",
                                   P_val < 0.1 ~ "p < 0.1",
                                   P_val >= 0.1 ~ "n.s."),
           )
 var_name <- paste(lion_name,"_coefs_df",sep = "")
  assign(var_name,dat_coefs_df)

}



all_lion_coefs <- rbind(Babe_coefs_df,
                        iHlane_coefs_df,
                        PokeJr_coefs_df,
                        Ntombi_coefs_df,
                        Thembi_coefs_df,
                        Madonna_coefs_df,
                        Roxy_coefs_df,
                        Zulu_coefs_df,
                        Koku_coefs_df,
                        Murph_coefs_df,
                        Fluffy_coefs_df,
                        Stud_coefs_df,
                        Fanbelt_coefs_df)%>%
  mutate(Lion = factor(Lion, c("Babe",
                               "iHlane",
                               "PokeJr",
                               "Ntombi",
                               "Thembi",
                               "Madonna",
                               "Roxy",
                               "Zulu",
                               "Koku",
                               "Murph",
                               "Fluffy",
                               "Stud",
                               "Fanbelt")),
         Significant = factor(Significant, c("p < 0.001",
                                             "p < 0.01",
                                             "p < 0.05",
                                             "p < 0.1",
                                             "n.s.")))



