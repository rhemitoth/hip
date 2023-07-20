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
library(foreach)
library(doParallel)
library(amt)
library(survival)
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

#Parallel Settings----

#Allocating cores
n.cores <- 2

#creating cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)

#register cluster to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)


#Loading processed lion GPS data----
#Looping through csv files and importing them as a batch
#Each csv is assigned to a variable corresponding with the name of the Lion (e.g. Fluffy.csv is stored as Fluffy)
#Also created a list "lion_names" that can be used to loop through lion datasets later on

#directory <- "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/issa_complete"
directory <- "C:/Users/moorcroftlab/OneDrive - Harvard University/Documents/Rhemi/hip_lion_issa/R/scripts/bootstrapping"
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

niter <- 2000

#Bootstrapped AIC Calculations----

directory <- "C:/Users/moorcroftlab/OneDrive - Harvard University/Documents/Rhemi/hip_lion_issa/R/scripts/model_selection/"

start.time <- Sys.time()

for(i in 1:numlions){
  lion <- lions[[i]]
  print(lion)
  lion_data <- get(paste(lion,"issa",sep="_"))
  n <- nrow(lion_data)
  
  samps <- foreach(
    j = 1:niter
  )%dopar%{
    #randomly sampling data
    samp <- lion_data[sample(x = n,
                             size = n,
                             replace = TRUE),]
    return(samp)
  }
  
  #m0----
  print("m0")
  m0s <- foreach(
    j = 1:niter,
    .packages = c("amt","tidyverse"),
    .combine = 'rbind'
  )%dopar%{
    #fitting data to model
    m0 <- fit_issf(samps[[j]],
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
    
    #extracting model coefficients
    coefs <- extract_coefs('m0') %>%
      mutate(Lion = lion)
    
    #Calculating AIC and Akaike Weight
    aics <- tibble(Lion = lion,
                   bootstrap = j,
                   model = "m0",
                   aic = NA,
                   weight = NA)
    for(k in 1:nrow(aics)){
      m <- aics$model[k]
      aics$aic[k] <- AIC(get(m))
      aics$weight[k] <- exp(-0.5*aics$aic[k])
    }
    
    res <- merge(aics,coefs,by=c('model','Lion'),no.dups = TRUE)
    return(res)
  }

  f <- paste(directory,
             lion,
             "/m0.csv",
             sep = "")
  write.csv(m0s,
            file = f)
 
  
  #m1----
  print("m1")
  if(lion != "Fluffy"){
    
    m1s <- foreach(
      j = 1:niter,
      .packages = c("amt","tidyverse"),
      .combine = 'rbind'
    )%dopar%{
      #fitting data to model
      m1 <- amt::fit_issf(samps[[j]],
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
      
      #extracting model coefficients
      coefs <- extract_coefs('m1') %>%
        mutate(Lion = lion)
      
      #Calculating AIC and Akaike Weight
      aics <- tibble(Lion = lion,
                     bootstrap = j,
                     model = "m1",
                     aic = NA,
                     weight = NA)
      for(k in 1:nrow(aics)){
        m <- aics$model[k]
        aics$aic[k] <- AIC(get(m))
        aics$weight[k] <- exp(-0.5*aics$aic[k])
      }
      
      res <- merge(aics,coefs,by=c('model','Lion'),no.dups = TRUE)
      return(res)
    }
    
    f <- paste(directory,
               lion,
               "/m1.csv",
               sep = "")
    write.csv(m1s,
              file = f)
   
  }
  
  #m2----
  print("m2")
  m2s <- foreach(
    j = 1:niter,
    .packages = c("amt","tidyverse"),
    .combine = 'rbind'
  )%dopar%{
    #fitting data to model
    m2 <- amt::fit_issf(samps[[j]],
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
    
    #extracting model coefficients
    coefs <- extract_coefs('m2') %>%
      mutate(Lion = lion)
    
    #Calculating AIC and Akaike Weight
    aics <- tibble(Lion = lion,
                   bootstrap = j,
                   model = "m2",
                   aic = NA,
                   weight = NA)
    for(k in 1:nrow(aics)){
      m <- aics$model[k]
      aics$aic[k] <- AIC(get(m))
      aics$weight[k] <- exp(-0.5*aics$aic[k])
    }
    
    res <- merge(aics,coefs,by=c('model','Lion'),no.dups = TRUE)
    return(res)
  }
  
  f <- paste(directory,
             lion,
             "/m2.csv",
             sep = "")
  write.csv(m2s,
            file = f)
 
  
  #m3----
  print("m3")
  m3s <- foreach(
    j = 1:niter,
    .packages = c("amt","tidyverse"),
    .combine = 'rbind'
  )%dopar%{
    #fitting data to model
    m3 <- amt::fit_issf(samps[[j]],
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
    
    #extracting model coefficients
    coefs <- extract_coefs('m3') %>%
      mutate(Lion = lion)
    
    #Calculating AIC and Akaike Weight
    aics <- tibble(Lion = lion,
                   bootstrap = j,
                   model = "m3",
                   aic = NA,
                   weight = NA)
    for(k in 1:nrow(aics)){
      m <- aics$model[k]
      aics$aic[k] <- AIC(get(m))
      aics$weight[k] <- exp(-0.5*aics$aic[k])
    }
    
    res <- merge(aics,coefs,by=c('model','Lion'),no.dups = TRUE)
    return(res)
  }
  
  f <- paste(directory,
             lion,
             "/m3.csv",
             sep = "")
  write.csv(m3s,
            file = f)
 
  
  #m4----
  print("m4")
  m4s <- foreach(
    j = 1:niter,
    .packages = c("amt","tidyverse"),
    .combine = 'rbind'
  )%dopar%{
    #fitting data to model
    m4 <- amt::fit_issf(samps[[j]],
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
    
    #extracting model coefficients
    coefs <- extract_coefs('m4') %>%
      mutate(Lion = lion)
    
    #Calculating AIC and Akaike Weight
    aics <- tibble(Lion = lion,
                   bootstrap = j,
                   model = "m4",
                   aic = NA,
                   weight = NA)
    for(k in 1:nrow(aics)){
      m <- aics$model[k]
      aics$aic[k] <- AIC(get(m))
      aics$weight[k] <- exp(-0.5*aics$aic[k])
    }
    
    res <- merge(aics,coefs,by=c('model','Lion'),no.dups = TRUE)
    return(res)
  }
  
  f <- paste(directory,
             lion,
             "/m4.csv",
             sep = "")
  write.csv(m4s,
            file = f)
 
  
  #m5----
  
  print("m5")
  m5s <- foreach(
    j = 1:niter,
    .packages = c("amt","tidyverse"),
    .combine = 'rbind'
  )%dopar%{
    #fitting data to model
    m5 <- amt::fit_issf(samps[[j]],
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
    
    #extracting model coefficients
    coefs <- extract_coefs('m5') %>%
      mutate(Lion = lion)
    
    #Calculating AIC and Akaike Weight
    aics <- tibble(Lion = lion,
                   bootstrap = j,
                   model = "m5",
                   aic = NA,
                   weight = NA)
    for(k in 1:nrow(aics)){
      m <- aics$model[k]
      aics$aic[k] <- AIC(get(m))
      aics$weight[k] <- exp(-0.5*aics$aic[k])
    }
    
    res <- merge(aics,coefs,by=c('model','Lion'),no.dups = TRUE)
    return(res)
  }
  
  f <- paste(directory,
             lion,
             "/m5.csv",
             sep = "")
  write.csv(m5s,
            file = f)
 
  
  #m6----
  
  print("m6")
  m6s <- foreach(
    j = 1:niter,
    .packages = c("amt","tidyverse"),
    .combine = 'rbind'
  )%dopar%{
    #fitting data to model
    m6 <- amt::fit_issf(samps[[j]],
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
    
    #extracting model coefficients
    coefs <- extract_coefs('m6') %>%
      mutate(Lion = lion)
    
    #Calculating AIC and Akaike Weight
    aics <- tibble(Lion = lion,
                   bootstrap = j,
                   model = "m6",
                   aic = NA,
                   weight = NA)
    for(k in 1:nrow(aics)){
      m <- aics$model[k]
      aics$aic[k] <- AIC(get(m))
      aics$weight[k] <- exp(-0.5*aics$aic[k])
    }
    
    res <- merge(aics,coefs,by=c('model','Lion'),no.dups = TRUE)
    return(res)
  }
  
  f <- paste(directory,
             lion,
             "/m6.csv",
             sep = "")
  write.csv(m6s,
            file = f)
 
  
  #m7----
  
  print("M7")
  if(lion != "Fluffy"){
    
    m7s <- foreach(
      j = 1:niter,
      .packages = c("amt","tidyverse"),
      .combine = 'rbind'
    )%dopar%{
      #fitting data to model
      m7 <- amt::fit_issf(samps[[j]],
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
      
      #extracting model coefficients
      coefs <- extract_coefs('m7') %>%
        mutate(Lion = lion)
      
      #Calculating AIC and Akaike Weight
      aics <- tibble(Lion = lion,
                     bootstrap = j,
                     model = "m7",
                     aic = NA,
                     weight = NA)
      for(k in 1:nrow(aics)){
        m <- aics$model[k]
        aics$aic[k] <- AIC(get(m))
        aics$weight[k] <- exp(-0.5*aics$aic[k])
      }
      
      res <- merge(aics,coefs,by=c('model','Lion'),no.dups = TRUE)
      return(res)
    }
   
    f <- paste(directory,
               lion,
               "/m7.csv",
               sep = "")
    write.csv(m7s,
              file = f)
   
  }
  
}

parallel::stopCluster(cl = my.cluster)

end.time <- Sys.time()

total_time <- round(end.time - start.time, 2)

print(total_time)
