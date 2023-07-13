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

##
## ****************************************************

#source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/issa/issa_all.R")

plot_theme <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                    axis.title.x = element_text(face = "bold",size = 14),
                    axis.title.y = element_text(face = "bold",size = 14),
                    axis.text = element_text(size = 12),
                    legend.title = element_text(face = "bold", size = 14),
                    legend.text = element_text(size = 12),
                    plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

#Slope Graph----
slope_betas <- all_lion_coefs %>%
  filter(Variable == "Slope_end")

slope_betas_plot <- ggplot(data= slope_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
  labs(title = "ST Slope Coefs")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Selection for Slope")+
  theme_bw()+
  #scale_shape_manual(values = c(21,22,4)) +
  plot_theme

filename <- paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/plots/slope.png")
ggsave(filename = filename,
       device = "png",
       plot = slope_betas_plot,
       width = 6,
       height = 5.625,
       units = "in")

#Veg1m----
veg_betas <- all_lion_coefs %>%
  filter(Variable == "Veg1m_end")

veg_betas_plot <- ggplot(data= veg_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
  labs(title = "ST for Vegetation Cover")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Selection for Vegetation Cover")+
  theme_bw()+
  plot_theme +
  scale_shape_manual(values = c(21,22,23,4))

filename <- paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/plots/veg_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = veg_betas_plot,
       width = 6,
       height = 5.625,
       units = "in")

#distEdge----
distEdge_betas <- all_lion_coefs %>%
  filter(Variable == "distEdge_end")

distEdge_betas_plot <- ggplot(data= distEdge_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
  labs(title = "ST Distance to Home Range Edge")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Selection for Distance to Edge")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,23,24))

filename <- paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/plots/distEdge_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = distEdge_betas_plot,
       width = 6,
       height = 5.625,
       units = "in")

#PreyAbundance----

PreyAbundance_betas <- all_lion_coefs %>%
  filter(Variable == "PreyAbundance_end")

PreyAbundance_betas_plot <- ggplot(data= PreyAbundance_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
  labs(title = "ST Probability of Encountering Prey")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Selection for Probability of Encountering Prey")+
  theme_bw()+
  plot_theme +
  scale_shape_manual(values = c(21,22,23,24,4))

filename <- paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/plots/PreyAbundance_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = PreyAbundance_betas_plot,
       width = 6,
       height = 5.625,
       units = "in")

#PreyCatchability----

PreyCatchability_betas <- all_lion_coefs %>%
  filter(Variable == "PreyCatchability_end")

PreyCatchability_betas_plot <- ggplot(data= PreyCatchability_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
  labs(title = "Variation in Selection for Probability of Catching Prey")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Selection for Probability of Catching Prey")+
  theme_bw()+
  scale_shape_manual(values = c(21,22,23,4))+
  plot_theme


filename <- paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/plots/PreyCatchability_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = PreyCatchability_betas_plot,
       width = 6,
       height = 5.625,
       units = "in")

#PreyAbundanceNight----
PreyAbundanceNight_betas <- all_lion_coefs %>%
  filter(Variable == "PreyAbundance_end:tod_end_night")

PreyAbundanceNight_betas_plot <- ggplot(data= PreyAbundanceNight_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
  labs(title = "ST PreyAb x TOD",
       subtitle = "Reference level is day")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw()+
  scale_shape_manual(values = c(21,24,4))+
  plot_theme


filename <- paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/plots/PreyAbundanceNight_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = PreyAbundanceNight_betas_plot,
       width = 6,
       height = 5.625,
       units = "in")

#PreyCatchabilityNight----
PreyCatchabilityNight_betas <- all_lion_coefs %>%
  filter(Variable == "PreyCatchability_end:tod_end_night")

PreyCatchabilityNight_betas_plot <- ggplot(data= PreyCatchabilityNight_betas)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = 2.5)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3)+
  labs(title = "ST PreyCatch x TOD",
       subtitle = "Reference level is day")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  scale_shape_manual(values = c(21,22,23,24,4))+
  theme_bw()+
  plot_theme


filename <- paste("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/plots/PreyCatchabilityNight_betas.png")
ggsave(filename = filename,
       device = "png",
       plot = PreyCatchabilityNight_betas_plot,
       width = 6,
       height = 5.625,
       units = "in")

