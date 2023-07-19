### ****************************************************
## Title: Bootstrap Model Competition
## Author: Rhemi Toth
## Date Created: 04/11/2023
## Email: rhemitoth@g.harvard.edu
##
## ****************************************************
##
## Notes:
## For each lion, tally the instances of a model having the lowest AIC value from the 10,000 bootstrapped iterations
##
## ****************************************************
##
## Loading Packages
library(tidyverse)
library(amt)
##
## ****************************************************

#Extract coefs function----
extract_coefs <- function(mod){
  m <- get(mod)
  c <- summary(m)[7]$coefficients
  c_df <- data.frame(c)
  c_df$cov <- rownames(c_df)
  names(c_df) <- c("coef","exp_coef","se","z","p","cov")
  rownames(c_df) <- NULL
  c_df$model <- mod
  return(c_df)
}

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

niter <- 1
#Bootstrapped AIC calculations----
for(i in 1:1){

  #lion <- lions[[i]]
  lion <- "Fluffy"
  lion_data <- get(paste(lion,"issa",sep="_"))
  n <- nrow(lion_data)

  #Initialzing data table to tally AIC scores----
  aic_table_raw <- tibble(Lion = character(),
                          bootstrap = NA,
                          model = character(),
                          aic = NA,
                          weight = NA)

  #Initalizing coefs table----
  coef_table <- tibble(Lion = character(),
                       bootstrap = NA,
                       model = character(),
                       coef = NA,
                       exp_coef = NA,
                       z = NA,
                       p = NA,
                       cov = character()
  )

  for(j in 1:niter){

    print(paste("Starting bootstrap iteration ",j,"/",niter," for ",lion,sep = ""))

    print("-----Randomly sampling data")
    samp <- lion_data[sample(x = n,
                             size = n,
                             replace = TRUE),]

    print("-----Fitting random sample to models")
    #Core model (Model 0)
    m0 <- fit_issf(samp,
                   #Response
                   case_ ~
                     #Slope
                     Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                     #Vegetation
                     Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
                     #Time of Day
                     tod_start_:log_sl_+ tod_start_:sl_ + tod_start_:cos_ta_ +
                     #Stratum
                     strata(step_id_),
                   model = TRUE)

    # Lion Encounter Risk (Model 1)
    if(lion != "Fluffy"){
      m1 <- fit_issf(samp,
                     #Response
                     case_ ~
                       #Core Model
                       Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                       Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
                       tod_start_:log_sl_+ tod_start_:sl_  + tod_start_:cos_ta_ +
                       #Distance to Home Range Edge Effects on Movement
                       distEdge_start:log_sl_ + distEdge_start:cos_ta_+
                       #Distance to Home Range Edge Effects on Habitat Selection
                       distEdge_end +
                       #Stratum
                       strata(step_id_),
                     model = TRUE)
    }

    # Third Order Selection: Prey Abundance (Model 2)
    m2 <- fit_issf(samp,
                   #Response
                   case_ ~
                     #Core Model
                     Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                     Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
                     tod_start_:log_sl_+ tod_start_:sl_  + tod_start_:cos_ta_ +
                     #Prey Abundance Effects on Third Order Habitat Selection
                     PreyAbundance_end +
                     #Stratum
                     strata(step_id_),
                   model = TRUE)

    # Third Order Selection: Prey Catchability (Model 3)
    m3 <- fit_issf(samp,
                   #Response
                   case_ ~
                     #Core Model
                     Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                     Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
                     tod_start_:log_sl_+ tod_start_:sl_  + tod_start_:cos_ta_ +
                     #Prey Catchability Effects on Third Order Habitat Selection
                     PreyCatchability_end +
                     #Stratum
                     strata(step_id_),
                   model = TRUE)

    # Fourth Order Selection: Prey Abundance (Model 4)
    m4 <- fit_issf(samp,
                   #Response
                   case_ ~
                     #Core Model
                     Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                     Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
                     tod_start_:log_sl_+ tod_start_:sl_  + tod_start_:cos_ta_ +
                     #Prey Abundance Effects on Fourth Order Habitat Selection
                     PreyAbundance_end:tod_end_ +
                     #Stratum
                     strata(step_id_),
                   model = TRUE)

    # Fourth Order Selection: Prey Catchability (Model 5)
    m5 <- fit_issf(samp,
                   #Response
                   case_ ~
                     #Core Model
                     Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                     Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
                     tod_start_:log_sl_+ tod_start_:sl_  + tod_start_:cos_ta_ +
                     #Prey Catchability Effects on Fourth Order Habitat Selection
                     PreyCatchability_end:tod_end_ +
                     #Stratum
                     strata(step_id_),
                   model = TRUE)

    # Third and Fourth Order Habitat Selection (Model 6)
    m6 <- fit_issf(samp,
                   #Response
                   case_ ~
                     #Core Model
                     Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                     Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
                     tod_start_:log_sl_+ tod_start_:sl_  + tod_start_:cos_ta_ +
                     #Third Order Habitat Selection
                     PreyAbundance_end + PreyCatchability_end +
                     #Fourth Order Habitat Seleciton
                     PreyAbundance_end:tod_end_ + PreyCatchability_end:tod_end_ +
                     #Stratum
                     strata(step_id_),
                   model = TRUE)

    # Lion Encounter Risk, Third Order, and Fourth Order Habitat Selection (Model 7)
    if(lion != "Fluffy"){
      m7 <- fit_issf(samp,
                     #Response
                     case_ ~
                       #Core Model
                       Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                       Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
                       tod_start_:log_sl_+ tod_start_:sl_  + tod_start_:cos_ta_ +
                       #Distance to Home Range Edge Effects on Movement
                       distEdge_start:log_sl_ + distEdge_start:cos_ta_+
                       #Distance to Home Range Edge Effects on Habitat Selection
                       distEdge_end +
                       #Third Order Habitat Selection
                       PreyAbundance_end + PreyCatchability_end +
                       #Fourth Order Habitat Selection
                       PreyAbundance_end:tod_end_ + PreyCatchability_end:tod_end_ +
                       #Stratum
                       strata(step_id_),
                     model = TRUE)
    }

    #Extracting model coefficients----
    print("-----Extracting model coefficients")
    m0_coefs <- extract_coefs('m0') %>%
      mutate(Lion = lion)
    m2_coefs <- extract_coefs('m2')%>%
      mutate(Lion = lion)
    m3_coefs <- extract_coefs('m3')%>%
      mutate(Lion = lion)
    m4_coefs <- extract_coefs('m4')%>%
      mutate(Lion = lion)
    m5_coefs <- extract_coefs('m5')%>%
      mutate(Lion = lion)
    m6_coefs <- extract_coefs('m6')%>%
      mutate(Lion = lion)

    if(lion != "Fluffy"){
      m1_coefs <- extract_coefs('m1')%>%
        mutate(Lion = lion)
      m7_coefs <- extract_coefs('m7')%>%
        mutate(Lion = lion)

      coefs <- rbind(m0_coefs,
                     m1_coefs,
                     m2_coefs,
                     m3_coefs,
                     m4_coefs,
                     m5_coefs,
                     m6_coefs,
                     m7_coefs)%>%
        mutate(bootstrap = j)
    }else{
      coefs <- rbind(m0_coefs,
                     m2_coefs,
                     m3_coefs,
                     m4_coefs,
                     m5_coefs,
                     m6_coefs)%>%
        mutate(bootstrap = j)
    }
    coefs_table <- rbind(coef_table,coefs)

    #Calculating AICS and Akaike weights
    print("-----Computing AICs and Akaike weights")
    if(lion != "Fluffy"){
      aics <- tibble(Lion = lion,
                     bootstrap = j,
                     model = c("m0","m1","m2","m3","m4","m5","m6","m7"),
                     aic = NA,
                     weight = NA)

      for(k in 1:nrow(aics)){
        m <- aics$model[k]
        aics$aic[k] <- AIC(get(m))
        aics$weight[k] <- exp(-0.5*aics$aic[k])
      }
    }else{
      aics <- tibble(Lion = lion,
                     bootstrap = j,
                     model = c("m0","m2","m3","m4","m5","m6"),
                     aic = NA,
                     weight = NA)

      for(k in 1:nrow(aics)){
        m <- aics$model[k]
        aics$aic[k] <- AIC(get(m))
        aics$weight[k] <- exp(-0.5*aics$aic[k])
      }
    }

    aic_table_raw <- rbind(aic_table_raw, aics)
  }
  print("-----Assigning results to variable and saving to csv")

  assign(paste(lion,"aic_table",sep = "_"),aic_table_raw)
  f <- paste("/Users/rhemitoth/Documents/Lion_Movement/Results/bootstrapping/aic/",
                    lion,
                    ".csv",
                    sep = "")
  write.csv(aic_table_raw,
            file = f)

  assign(paste(lion,"coefs",sep = "_"),coefs_table)
  f <- paste("/Users/rhemitoth/Documents/Lion_Movement/Results/bootstrapping/coefs/",
             lion,
             ".csv",
             sep = "")
  write.csv(coefs_table,
            file = f)

}

