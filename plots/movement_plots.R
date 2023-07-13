## ****************************************************
## Title: Movement Plots
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
library(ggpubr)
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
                    plot.background = element_rect(fill='transparent', color=NA),
                    panel.grid.major = element_blank(), #remove major gridlines
                    panel.grid.minor = element_blank(), #remove minor gridlines
                    panel.background = element_rect(fill = "#fefae0"),
                    legend.key = element_rect(colour = NA, fill = NA),
                    legend.background = element_rect(fill = "#fefae0"))

Fluffy_issa_for_plot <- Fluffy_issa %>%
  mutate(distEdge_start = NA,
         distEdge_end = NA)

all_lions <- rbind(Babe_issa,
                   Fanbelt_issa,
                   iHlane_issa,
                   Koku_issa,
                   Madonna_issa,
                   Mellon_issa,
                   Murph_issa,
                   Ntombi_issa,
                   PokeJr_issa,
                   Roxy_issa,
                   Stud_issa,
                   Thembi_issa,
                   Zulu_issa)%>%
  filter(case_ == TRUE)%>%
  mutate(rounded_sl = round(sl_, digits = 0),
         rounded_slope = round(Slope_start, digits = 0),
         rounded_veg = round(Veg1m_start, digits = 0),
         rounded_distEdge = round(distEdge_start, digits = 0),
         year = format(t1_, format = "%Y"))


all_lions_sum <- all_lions %>%
  group_by(Sex, tod_start_, id)%>%
  summarize(mean_sl = mean(sl_),
            mean_cos_ta = mean(cos_ta_, na.rm = TRUE),
            sd_sl = sd(sl_),
            sd_cos_ta = sd(cos_ta_, na.rm = TRUE),
            count = n())%>%
  ungroup()%>%
  mutate(highCI_sl = mean_sl + 2 * sd_sl,
         lowCI_sl = mean_sl - 2 * sd_sl,
         highCI_cos_ta = mean_cos_ta + 2* sd_cos_ta,
         lowCI_cos_ta = mean_cos_ta - 2 * sd_cos_ta)

#StepLengths Slope----

slope_sl_sum <- all_lions %>%
  group_by(Slope_start, Sex) %>%
  summarize(mean_sl = mean(sl_))%>%
  ungroup()

slope_sl_plot <- ggplot()+
  geom_smooth(data = slope_sl_sum, aes(x = Slope_start, y = mean_sl, color = Sex), method = 'loess')+
  labs(title = "Mean Movement Rate vs. Slope")+
  xlab("Slope")+
  ylab("Mean Movement Rate (m/hr)")+
  theme_bw()+
  plot_theme

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/Slope_Step_Length.png")
ggsave(filename = filename,
       device = "png",
       plot = slope_sl_plot,
       width = 10,
       height = 5.625,
       units = "in")

#Step Length Veg
veg_sl_sum <- all_lions %>%
  group_by(Veg1m_start, Sex) %>%
  summarize(mean_sl = mean(sl_))%>%
  ungroup()

veg_sl_plot <- ggplot()+
  geom_smooth(data = veg_sl_sum, aes(x = Veg1m_start, y = mean_sl, color = Sex), method = 'loess')+
  labs(title = "Mean Movement Rate vs. Vegetation")+
  xlab("Percent Cover at 1m")+
  ylab("Mean Movement Rate (m/hr)")+
  theme_bw()+
  plot_theme

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/Veg_Step_Length.png")
ggsave(filename = filename,
       device = "png",
       plot = veg_sl_plot,
       width = 10,
       height = 5.625,
       units = "in")

#Step lenght Dist Edge----
distEdge_sl_sum <- all_lions %>%
  group_by(distEdge_start, Sex) %>%
  summarize(mean_sl = mean(sl_))%>%
  ungroup()

distEdge_sl_plot <- ggplot()+
  geom_smooth(data = distEdge_sl_sum, aes(x = distEdge_start, y = mean_sl, color = Sex), method = 'loess')+
  labs(title = "Step Length vs. Encounter Risk")+
  xlab("Distance to Edge (m)")+
  ylab("Mean Step Length (m)")+
  theme_bw()+
  plot_theme+
  scale_color_manual(values = c(female_clr,male_clr))

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/distEdge_Step_Length.png")
ggsave(filename = filename,
       device = "png",
       plot = distEdge_sl_plot,
       width = 5.5,
       height = 4,
       units = "in")

#Step Length day night----
tod_sl_sum <- all_lions %>%
  group_by(tod_start_, Sex) %>%
  summarize(mean_sl = mean(sl_),
            sd = sd(sl_),
            count = n())%>%
  ungroup()%>%
  mutate(se = sd/sqrt(count),
         highCI = mean_sl + se,
         lowCI = mean_sl - se)

tod_sl_plot <- ggplot(tod_sl_sum, aes(fill = tod_start_))+
  geom_bar(aes(x = Sex, y = mean_sl), stat = "identity", position = "dodge")+
  geom_errorbar(position = position_dodge(0.9),
                aes(x = Sex, ymin = lowCI, ymax = highCI), width = 0.2, size = 1)+
  labs(title = "Mean Movement Rate during the Day and Night")+
  xlab("Sex")+
  ylab("Mean Movement Rate (m/hr)")+
  scale_fill_manual(values = c("#F2AD00","#7294D4"), name = "Time of Day")+
  theme_bw()+
  plot_theme

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/tod_Step_Length.png")
ggsave(filename = filename,
       device = "png",
       plot = tod_sl_plot,
       width = 10,
       height = 5.625,
       units = "in")

#Turing Angle Slope----

slope_ta_sum <- all_lions %>%
  group_by(Slope_start, Sex) %>%
  summarize(mean_ta = mean(cos_ta_))%>%
  ungroup()

slope_ta_plot <- ggplot()+
  geom_smooth(data = slope_ta_sum, aes(x = Slope_start, y = mean_ta, color = Sex), method = 'loess')+
  geom_hline(yintercept = 0, linetype = 2)+
  labs(title = "Mean Cos(Turing Angle) vs. Slope")+
  xlab("Slope")+
  ylab("Mean Cos(Turing Angle)")+
  theme_bw()+
  plot_theme

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/Slope_ta.png")
ggsave(filename = filename,
       device = "png",
       plot = slope_ta_plot,
       width = 10,
       height = 5.625,
       units = "in")

#Veg Ta----

veg_ta_sum <- all_lions %>%
  group_by(Veg1m_start, Sex) %>%
  summarise(mean_ta = mean(cos_ta_, na.rm = TRUE))%>%
  ungroup()

veg_ta_plot <- ggplot()+
  geom_smooth(data = veg_ta_sum, aes(x = Veg1m_start, y = mean_ta, color = Sex), method = 'loess')+
  geom_hline(yintercept = 0, linetype = 2)+
  labs(title = "Mean Cos(Turning Angle) vs. Vegetation")+
  xlab("Percent Cover at 1m")+
  ylab("Mean Cos(Turning Angle)")+
  theme_bw()+
  plot_theme

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/Veg_ta.png")
ggsave(filename = filename,
       device = "png",
       plot = veg_ta_plot,
       width = 10,
       height = 5.625,
       units = "in")

#distEdge ta----

distEdge_ta_sum <- all_lions %>%
  group_by(distEdge_start, Sex) %>%
  summarize(mean_ta = mean(cos_ta_))%>%
  ungroup()

distEdge_ta_plot <- ggplot()+
  geom_smooth(data = distEdge_ta_sum, aes(x = distEdge_start, y = mean_ta, color = Sex), method = 'loess')+
  geom_hline(yintercept = 0, linetype = 2)+
  labs(title = "Turn Angle vs. Encounter Risk")+
  xlab("Distance to Edge (m)")+
  ylab("Mean cosTA")+
  theme_bw()+
  plot_theme+
  scale_color_manual(values = c(female_clr,male_clr))

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/distEdgeta.png")
ggsave(filename = filename,
       device = "png",
       plot = distEdge_ta_plot,
       width = 5.5,
       height = 4,
       units = "in")

#tod turning angle----
tod_ta_sum <- all_lions %>%
  group_by(tod_start_, Sex) %>%
  summarize(mean_ta = mean(cos_ta_, na.rm = TRUE),
            sd = sd(cos_ta_ ,na.rm = TRUE),
            count = n())%>%
  ungroup()%>%
  mutate(se = sd/sqrt(count),
         highCI = mean_ta + se,
         lowCI = mean_ta - se)

tod_ta_plot <- ggplot(tod_ta_sum, aes(fill = tod_start_))+
  geom_bar(aes(x = Sex, y = mean_ta), stat = "identity", position = "dodge")+
  geom_errorbar(position = position_dodge(0.9),
                aes(x = Sex, ymin = lowCI, ymax = highCI), width = 0.2, size = 1)+
  labs(title = "Mean Cos(Turning Angle) during the Day and Night")+
  xlab("Sex")+
  ylab("Mean Cos(Turning Angle)")+
  scale_fill_manual(values = c("#F2AD00","#7294D4"), name = "Time of Day")+
  theme_bw()+
  plot_theme

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/tod_ta.png")
ggsave(filename = filename,
       device = "png",
       plot = tod_ta_plot,
       width = 10,
       height = 5.625,
       units = "in")

#Lion locs----
# lion_locs <- ggplot()+
#   geom_polygon(data = hip, aes(x = long, y = lat))+
#   geom_point(data = all_lions, aes(x = x1_, y=y1_, color = id), size = 0.5)+
#   labs(title = "Lion Relocations")+
#   xlab("Easting")+
#   ylab("Northing")+
#   scale_fill_manual(values = c("#F2AD00","#7294D4"), name = "Time of Day")+
#   theme_bw()+
#   plot_theme +
#   theme(axis.text.x = element_text(angle = 90),
#         legend.position = "none")+
#   facet_wrap(~year)
#
# filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/lion_locs.png")
# ggsave(filename = filename,
#        device = "png",
#        plot = lion_locs,
#        width = 5.625,
#        height = 5.625,
#        units = "in")

#Dist Edge SL TA----
removex <- theme(axis.text.x = element_blank(),
                 axis.title.x = element_blank())

removey <- theme(axis.title.y = element_blank())

distEdge_sl_ta <- ggarrange(distEdge_sl_plot + removex + theme(legend.position = "none"),
                            distEdge_ta_plot + theme(legend.position = "none") ,
                            ncol=1,
                            align = "v",
                            heights = c(1,1.25))

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/movement/distEdge_sl_ta.png")
ggsave(filename = filename,
       device = "png",
       plot = distEdge_sl_ta,
       width = 10,
       height = 10,
       units = "in")

