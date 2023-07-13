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
directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/Sensitivity_Test/issa_complete/"
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


#Fitting models for each lion----


source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Babe_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/iHlane_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/PokeJr_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Ntombi_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Thembi_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Madonna_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Roxy_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Zulu_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Koku_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Murph_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Fluffy_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Mellon_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Stud_issa.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/Fanbelt_issa.R")

#All Lion AIC Table

directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/Model_Competition"
files <- list.files(directory, pattern = "csv")
numfiles <- length(files)

for (i in 1:numfiles){
  file_path <- paste(directory,"/",files[i],sep = "")
  lion_name <- paste(str_replace(files[i],"AIC.csv",""))
  lion_name <- trimws(lion_name)
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
                   Mellon_AIC,
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
          file = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/Sensitivity_Test/AICS.csv")

winning_models <- rbind(Babe_winner%>%as.data.frame(),
                        iHlane_winner%>%as.data.frame(),
                        PokeJr_winner%>%as.data.frame(),
                        Ntombi_winner%>%as.data.frame(),
                        Thembi_winner%>%as.data.frame(),
                        Madonna_winner%>%as.data.frame(),
                        Roxy_winner%>%as.data.frame(),
                        Zulu_winner%>%as.data.frame(),
                        Koku_winner%>%as.data.frame(),
                        Murph_winner%>%as.data.frame(),
                        Fluffy_winner%>%as.data.frame(),
                        Mellon_winner%>%as.data.frame(),
                        Stud_winner%>%as.data.frame(),
                        Fanbelt_winner%>%as.data.frame())

winning_models_sum <- winning_models %>%
  group_by(Model) %>%
  summarize(count = n())

print(winning_models_sum)

winning_models_dict <- c("Babe" = "Babe_model",
                         "iHlane" = 'iHlane_model',
                         "PokeJr" = 'PokeJr_model',
                         "Ntombi" = 'Ntombi_model',
                         "Thembi" = 'Thembi_model',
                         "Madonna" = 'Madonna_model',
                         "Roxy" = 'Roxy_model',
                         "Zulu" = 'Zulu_model',
                         "Koku" = 'Koku_model',
                         "Murph" = 'Murph_model',
                         "Fluffy" = 'Fluffy_model',
                         "Mellon" = 'Mellon_model',
                         "Stud" = 'Stud_model',
                         "Fanbelt" = 'Fanbelt_model')

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
for(i in 1:length(lions)){
  lion_name <- lions[[i]] %>% trimws()
  model <- get(winning_models_dict[[lion_name]])
  dat_coefs <- as.data.frame(summary(model)$coefficients)
  dat_coefs$Variable <- row.names(dat_coefs)
  rownames(dat_coefs) <- NULL
  if(lion_name == "Fluffy"){
    n_rows <- 13
  }else{
    n_rows <- 16
  }
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
                        Mellon_coefs_df,
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
                               "Mellon",
                               "Stud",
                               "Fanbelt")),
         Significant = factor(Significant, c("p < 0.001",
                                             "p < 0.01",
                                             "p < 0.05",
                                             "p < 0.1",
                                             "n.s.")))



