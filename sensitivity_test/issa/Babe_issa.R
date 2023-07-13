## ****************************************************
## Title: Babe issa
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
lion_name <- "Babe"
#Loading Data----

#Core model (Model 0) ----
m0 <- fit_issf(Babe_issa,
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
m1 <- fit_issf(Babe_issa,
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
m2 <- fit_issf(Babe_issa,
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
m3 <- fit_issf(Babe_issa,
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
m4 <- fit_issf(Babe_issa,
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
m5 <- fit_issf(Babe_issa,
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
m6 <- fit_issf(Babe_issa,
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
m7 <- fit_issf(Babe_issa,
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
          file = paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/Model_Competition/",
                       lion_name,
                       "AIC.csv",
                       sep = ""))

index <- which.min(aic_table$AIC)
Babe_winner <- aic_table[index,]
Babe_model <- get(Babe_winner$Model[[1]])

# Updating Tentative Step Length and Turning Angle Distributions
# mintch = cellStats(hab$CHM, "min")
# maxtch = cellStats(hab$CHM, "max")
# meantch = cellStats(hab$CHM, "mean")
#
# # RSS
# x1 <- data.frame(CHM = seq(mintch, maxtch, length.out = 10),
#                  DEM = 200,
#                  sl_ = 189, log_sl_ = log(189),
#                  cos_ta_ = 0)
#
# x2 <- data.frame(CHM = meantch,
#                  DEM = 200,
#                  sl_ = 189, log_sl_ = log(189),
#                  cos_ta_ = 0)
#
# logRSS <- log_rss(m1,x1,x2,ci="se",ci_level = 0.95)
# ggplot(logRSS$df, aes(x = CHM_x1, y = exp(log_rss),
#                       ymin = exp(lwr), ymax = exp(upr))) +
#   geom_ribbon(color = "black", fill = "gray80", linetype = "dashed")+
#   geom_line()+
#   geom_hline(yintercept = 1, color = "red", linetype = "dashed")+
#   xlab("CHM at x1")+
#   ylab("RSS vs. Mean CHM")+
#   theme_bw()
#
# # Model 2 ----
#
# m2 <- fit_issf(Babe_issa,
#                #Response
#                case_ ~
#                  #Habitat
#                  tod_start_ : DEM + tod_start_ : CHM +
#                  #Movement
#                  sl_ + log_sl_ + cos_ta_ +
#                  tod_start_ : sl_ + tod_start_ : log_sl_ + tod_start_ : cos_ta_ +
#                  #Stratum
#                  strata(step_id_),
#                model = TRUE)
#
# # RSS
# x1_day <- data.frame(CHM = seq(mintch, maxtch, length.out = 10),
#                      DEM = 200,
#                      sl_ = 189, log_sl_ = log(189),
#                      cos_ta_ = 0,
#                      tod_start_ = factor("day", levels = c("day", "night")),
#                      tod_end_ = factor("day", levels = c("day", "night")))
#
# x2_day <- data.frame(CHM = meantch,
#                      DEM = 200,
#                      sl_ = 189, log_sl_ = log(189),
#                      cos_ta_ = 0,
#                      tod_start_ = factor("day", levels = c("day", "night")),
#                      tod_end_ = factor("day", levels = c("day", "night")))
#
# logRSS_day <- log_rss(m2,x1_day,x2_day,ci="se",ci_level = 0.95)
#
# x1_night <- data.frame(CHM = seq(mintch, maxtch, length.out = 10),
#                        DEM = 200,
#                        sl_ = 189, log_sl_ = log(189),
#                        cos_ta_ = 0,
#                        tod_start_ = factor("night", levels = c("day", "night")),
#                        tod_end_ = factor("night", levels = c("day", "night")))
#
# x2_night <- data.frame(CHM = meantch,
#                        DEM = 200,
#                        sl_ = 189, log_sl_ = log(189),
#                        cos_ta_ = 0,
#                        tod_start_ = factor("night", levels = c("day", "night")),
#                        tod_end_ = factor("night", levels = c("day", "night")))
#
# logRSS_night <- log_rss(m2,x1_night,x2_night,ci="se",ci_level = 0.95)
#
# ggplot() +
#   geom_ribbon(data = logRSS_day$df,
#               aes(x = CHM_x1, y = exp(log_rss),
#                   ymin = exp(lwr), ymax = exp(upr)),
#               color = "orange", fill = "yellow", linetype = "dashed",
#               alpha = 0.5)+
#   geom_line(data = logRSS_day$df,
#             aes(x = CHM_x1, y = exp(log_rss)),
#             color = "orange")+
#   geom_ribbon(data = logRSS_night$df,
#               aes(x = CHM_x1, y = exp(log_rss),
#                   ymin = exp(lwr), ymax = exp(upr)),
#               color = "blue", fill = "purple", linetype = "dashed",
#               alpha = 0.5)+
#   geom_line(data = logRSS_night$df,
#             aes(x = CHM_x1, y = exp(log_rss)),
#             color = "blue")+
#   geom_hline(yintercept = 1, color = "red", linetype = "dashed")+
#   xlab("CHM at x1")+
#   ylab("RSS vs. Mean CHM")+
#   theme_bw()
