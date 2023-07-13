## ****************************************************
## Title: iHlane issa
## Author: Rhemi Toth
## Date Created: 04/03/2022
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
library(ggplot2)
library(cowplot)
library(survival)
##
## ****************************************************

#Loading Data----

lion_name <- "iHlane"
#Loading Data----

#Core model (Model 0) ----
m0 <- fit_issf(iHlane_issa,
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

# Lion Encounter Risk (Model 1)----
m1 <- fit_issf(iHlane_issa,
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

# Third Order Selection: Prey Abundance (Model 2) ----
m2 <- fit_issf(iHlane_issa,
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

# Third Order Selection: Prey Catchability (Model 3) ----
m3 <- fit_issf(iHlane_issa,
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

# Fourth Order Selection: Prey Abundance (Model 4) ----
m4 <- fit_issf(iHlane_issa,
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

# Fourth Order Selection: Prey Catchability (Model 5) ----
m5 <- fit_issf(iHlane_issa,
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

# Third and Fourth Order Habitat Selection (Model 6) ----
m6 <- fit_issf(iHlane_issa,
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

# Lion Encounter Risk, Third Order, and Fourth Order Habitat Selection (Model 7)----
m7 <- fit_issf(iHlane_issa,
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

#AIC----
models <- c('m0','m1','m2','m3','m4','m5','m6','m7')

aic_scores <- AIC(m0$model,
                  m1$model,
                  m2$model,
                  m3$model,
                  m4$model,
                  m5$model,
                  m6$model,
                  m7$model)

aic_table <- tibble(Model = models,
                    AIC = aic_scores$AIC,
                    lion = lion_name)%>%
  arrange(desc(AIC))

write.csv(aic_table,
          row.names = FALSE,
          file = paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/Dry_Season_iSSA/Model_Competition/",
                       lion_name,
                       "AIC.csv"))

index <- which.min(aic_table$AIC)
iHlane_winner <- aic_table[index,]
iHlane_model <- get(iHlane_winner$Model[[1]])
