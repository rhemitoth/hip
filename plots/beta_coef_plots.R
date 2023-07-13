## ****************************************************
## Title: Beta Coef Plots
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
library(cowplot)
library(ggpubr)
library(scales)
##
## ****************************************************

#source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/issa/issa_all.R")

plot_theme <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                    axis.title.x = element_text(face = "bold",size = 20, color = "grey15"),
                    axis.title.y = element_text(face = "bold",size = 23, color = "grey15"),
                    axis.text = element_text(size = 23),
                    legend.title = element_text(face = "bold", size = 23),
                    legend.text = element_text(size = 23),
                    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
                    plot.subtitle = element_text(size = 20, hjust = 0),
                    plot.background = element_rect(fill='transparent', color=NA),
                    panel.grid.major = element_blank(), #remove major gridlines
                    panel.grid.minor = element_blank(), #remove minor gridlines
                    panel.background = element_rect(fill = "#fefae0"),
                    legend.key = element_rect(colour = NA, fill = NA),
                    legend.background = element_rect(fill = "#fefae0"))
                    #,legend.position = "none")#transparent legend panel



male_clr <- "cornflowerblue"
female_clr <- "tan1"

pnt_size <- 4
pnt_w <- 2

#Slope Graph----
# slope_betas <- all_lion_coefs %>%
#   filter(Variable == "Slope_end")
#
# slope_betas_plot <- ggplot(data= slope_betas)+
#   geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
#   geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
#   labs(title = "Variation in Selection for Slope")+
#   geom_hline(yintercept = 0, linetype = 2)+
#   xlab("Lion ID")+
#   ylab("Selection for Slope")+
#   theme_bw()+
#   #scale_shape_manual(values = c(21,22,4)) +
#   plot_theme
#
#
# filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/slope.png")
# ggsave(filename = filename,
#        device = "png",
#        plot = slope_betas_plot,
#        width = 10,
#        height = 7,
#        units = "in")

#Veg1m----
# veg_betas <- all_lion_coefs %>%
#   filter(Variable == "Veg1m_end")
#
# veg_betas_plot <- ggplot(data= veg_betas)+
#   geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
#   geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
#   labs(title = "Variation in Selection for Vegetation Cover")+
#   geom_hline(yintercept = 0, linetype = 2)+
#   xlab("Lion ID")+
#   ylab("Selection for Vegetation Cover")+
#   theme_bw()+
#   plot_theme +
#   scale_shape_manual(values = c(21,22,23,4))
#
# filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/veg_betas.png")
# ggsave(filename = filename,
#        device = "png",
#        plot = veg_betas_plot,
#        width = 10,
#        height = 7,
#        units = "in")

#distEdge----
distEdge_betas <- all_lion_coefs %>%
  filter(Variable == "distEdge_end")

distEdge_betas_plot <- ggplot(data= distEdge_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3, linewidth = pnt_w)+
  labs(title = "Selection for Distance to Home Range Edge")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,23,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", scales::math_format(10^.x)))

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/distEdge_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = distEdge_betas_plot,
       width = 7,
       height = 7,
       units = "in")

#PreyAbundance----

PreyAbundance_betas <- all_lion_coefs %>%
  filter(Variable == "PreyAbundance_end")

PreyAbundance_betas_plot <- ggplot(data= PreyAbundance_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3, linewidth = pnt_w)+
  labs(title = "Selection for Prey Abundance")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw()+
  plot_theme +
  scale_shape_manual(values = c(21,22,23,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/PreyAbundance_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = PreyAbundance_betas_plot,
       width = 7,
       height = 7,
       units = "in")

#PreyCatchability----

PreyCatchability_betas <- all_lion_coefs %>%
  filter(Variable == "PreyCatchability_end")

PreyCatchability_betas_plot <- ggplot(data= PreyCatchability_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3, linewidth = pnt_w)+
  labs(title = "Selection for Prey Catchability")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw()+
  scale_shape_manual(values = c(21,22,23,4))+
  plot_theme+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))


filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/PreyCatchability_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = PreyCatchability_betas_plot,
       width = 7,
       height = 7,
       units = "in")

#PreyAbundanceNight----
PreyAbundanceNight_betas <- all_lion_coefs %>%
  filter(Variable == "PreyAbundance_end:tod_end_night")

PreyAbundanceNight_betas_plot <- ggplot(data= PreyAbundanceNight_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3, linewidth = pnt_w)+
  labs(title = "Prey Abundance x ToD",
       subtitle = "Reference level is day")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw()+
  plot_theme +
 scale_shape_manual(values = c(21,23,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/PreyAbundanceNight_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = PreyAbundanceNight_betas_plot,
       width = 7,
       height = 7,
       units = "in")

#PreyCatchabilityNight----
PreyCatchabilityNight_betas <- all_lion_coefs %>%
  filter(Variable == "PreyCatchability_end:tod_end_night")

PreyCatchabilityNight_betas_plot <- ggplot(data= PreyCatchabilityNight_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3, linewidth = pnt_w)+
  labs(title = "Prey Catchability x ToD",
       subtitle = "Reference level is day")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw()+
  scale_shape_manual(values = c(21,22,23,4))+
  plot_theme+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/PreyCatchabilityNight_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = PreyCatchabilityNight_betas_plot,
       width = 7,
       height = 7,
       units = "in")

#Poster PA PC----
removex <- theme(axis.text.x = element_blank(),
                 axis.title.x = element_blank())

removey <- theme(axis.title.y = element_blank())

papc <- ggarrange(PreyAbundance_betas_plot+ removex, PreyCatchability_betas_plot + removey + removex,
                 PreyAbundanceNight_betas_plot,  PreyCatchabilityNight_betas_plot +removey,
                 nrow =2,
                 ncol=2,
                 common.legend = TRUE,
                 align = "v",
                 heights = c(0.75,1))

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/PreyAbPreCatch.png")
ggsave(filename = filename,
       device = "jpeg",
       plot = papc,
       width = 20,
       height = 14,
       units = "in",
       limitsize = FALSE)

#Poster Encounter Risk----
encounter_risk <- ggarrange(distEdge_sl_ta + theme(legend.position = "none"),
                            distEdge_betas_plot + theme(legend.position = "none"),
                            nrow = 2,
                            heights = c(1.5,1))
filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/beta_coefficients/encounter_risk.png")
ggsave(filename = filename,
       device = "png",
       plot = encounter_risk,
       width = 13,
       height = 13,
       units = "in",
       limitsize = FALSE)

