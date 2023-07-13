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
## ****************************************************

m7_all_lions <- fit_issf(all_lions,
               #Response
               case_ ~
                 #Core Model
                 Slope_end + Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                 Veg1m_end + Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
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

m7_females <- fit_issf(females,
                         #Response
                         case_ ~
                           #Core Model
                           Slope_end + Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                           Veg1m_end + Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
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

m7_males <- fit_issf(males,
                       #Response
                       case_ ~
                         #Core Model
                         Slope_end + Slope_start:log_sl_+ Slope_start:sl_ + Slope_start:cos_ta_ +
                         Veg1m_end + Veg1m_start:log_sl_+ Veg1m_start:sl_ + Veg1m_start:cos_ta_ +
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
