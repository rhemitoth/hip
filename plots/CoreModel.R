#Core Model Figures


library(scales)
library(tidyverse)

#source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/sensitivity_test/issa/issa_all.R")
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/plots/theme.R")
#All Lions Dataset----
#Combined dataset
Fluffy_issa_all <- Fluffy_issa %>%
  mutate(distEdge_start = 0.001,
         distEdge_end = 0.001,
         NormDistEdge_start = 0.001,
         NormDistEdge_end = 0.001)

all_data <- rbind(Babe_issa,
                  iHlane_issa,
                  PokeJr_issa,
                  Ntombi_issa,
                  Thembi_issa,
                  Madonna_issa,
                  Roxy_issa,
                  Zulu_issa,
                  Koku_issa,
                  Murph_issa,
                  Fluffy_issa_all,
                  Mellon_issa,
                  Stud_issa,
                  Fanbelt_issa)%>%
  filter(case_ == TRUE)

all_data_sum <- all_data%>%
  group_by(id)%>%
  summarise(minSL = min(sl_),
                          maxSL = max(sl_))%>%
  arrange(id)


all_data <- all_data %>%
  mutate(nSL = ifelse(id == "SAT1372 Zulu", sl_/all_data_sum$maxSL[1],
                      ifelse(id == "SAT1372_Koku ", sl_/all_data_sum$maxSL[2],
                             ifelse(id == "SAT1374 Murph", sl_/all_data_sum$maxSL[3],
                                    ifelse(id == "SAT1375 Fluffy", sl_/all_data_sum$maxSL[4],
                                           ifelse(id == "SAT1376 Mellon", sl_/all_data_sum$maxSL[5],
                                                  ifelse(id == "SAT1376 Stud", sl_/all_data_sum$maxSL[6],
                                                         ifelse(id == "SAT1377 Fanbelt", sl_/all_data_sum$maxSL[7],
                                                                ifelse(id == " SAT1378 ihlane ", sl_/all_data_sum$maxSL[8],
                                                                       ifelse(id == "SAT1379 PokeJr", sl_/all_data_sum$maxSL[9],
                                                                              ifelse(id == "SAT1380 Babe", sl_/all_data_sum$maxSL[10],
                                                                                     ifelse(id == "SAT1381 Thembi", sl_/all_data_sum$maxSL[11],
                                                                                            ifelse(id == "SAT1382 Ntombi", sl_/all_data_sum$maxSL[12],
                                                                                                   ifelse(id == "SAT1383 Roxy", sl_/all_data_sum$maxSL[13],
                                                                                                          ifelse(id == "SAT1679 Madonna", sl_/all_data_sum$maxSL[14],1)))))))))))))))

folder <- "/Users/rhemitoth/Documents/Lion_Movement/Manuscript/Figures/Core_Model/"

#CosTA x ToD----

cosTA_ToD <- core_coefs %>%
  filter(Variable == "cos_ta_:tod_start_night")

cosTA_ToD_sum <- cosTA_ToD %>%
  summarise(avg = mean(Beta$coef),
            sd = sd(Beta$coef),
            n = n(),
            se = sd/sqrt(n))

cosTA_ToD_plot <- ggplot(data= cosTA_ToD)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = bar_w, linewidth = pnt_w)+
  #labs(title = "cosTA:ToD Interaction",
       #subtitle = "Reference level is day")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste(folder, "cosTA_ToD.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = cosTA_ToD_plot,
       width = 10,
       height = 7,
       units = "in")

#CosTA x Veg ----

cosTA_veg <- core_coefs %>%
  filter(Variable == "cos_ta_:Veg1m_start")

cosTA_veg_plot <- ggplot(data= cosTA_veg)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = bar_w, linewidth = pnt_w)+
  labs(title = "CosTA:Vegetation Interaction")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste(folder, "cosTA_Veg.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = cosTA_veg_plot,
       width = 10,
       height = 7,
       units = "in")

#LogSL x ToD----

logSL_ToD <- core_coefs %>%
  filter(Variable == "log_sl_:tod_start_night")

logSL_ToD_plot <- ggplot(data= logSL_ToD)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3, linewidth = pnt_w)+
  labs(title = "logSL x ToD")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,23,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste(folder, "logSL_ToD.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = logSL_ToD_plot,
       width = 10,
       height = 7,
       units = "in")

#logSL x Veg----

logSL_veg <- core_coefs %>%
  filter(Variable == "log_sl_:Veg1m_start")

logSL_veg_plot <- ggplot(data= logSL_veg)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3, linewidth = pnt_w)+
  labs(title = "logSL x Veg")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste(folder, "logSL_veg.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = logSL_veg_plot,
       width = 10,
       height = 7,
       units = "in")

#SL x ToD----

sl_ToD<- core_coefs %>%
  filter(Variable == "sl_:tod_start_night")

sl_ToD_plot <- ggplot(data= sl_ToD)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = bar_w, linewidth = pnt_w)+
  labs(title = "SL:ToD Interaction")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,23,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste(folder, "SL_ToD.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = sl_ToD_plot,
       width = 10,
       height = 7,
       units = "in")

#SL x Veg----

sl_veg<- core_coefs %>%
  filter(Variable == "sl_:Veg1m_start")

sl_veg_plot <- ggplot(data= sl_veg)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = bar_w, linewidth = pnt_w)+
  labs(title = "SL:Vegetation Interaction")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,23,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste(folder, "SL_Veg.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = sl_veg_plot,
       width = 10,
       height = 7,
       units = "in")

#Vegetation Interaction Plot----
veg_int_plot <- ggplot(data = all_data, aes(x = Veg1m_start, y = nSL, color = id))+
  geom_point()

#CosTA x Slope----

cosTA_slope<- core_coefs %>%
  filter(Variable == "Slope_start:cos_ta_")

cosTA_slope_plot <- ggplot(data = cosTA_slope)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = bar_w, linewidth = pnt_w)+
  labs(title = "cosTA:Slope Interaction")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste(folder, "cosTA_slope.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = cosTA_slope_plot,
       width = 10,
       height = 7,
       units = "in")

#logSL x Slope----

logSL_slope<- core_coefs %>%
  filter(Variable == "Slope_start:log_sl_")

logSL_slope_plot <- ggplot(data = logSL_slope)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = 0.3, linewidth = pnt_w)+
  labs(title = "logSL x Slope")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,23,24,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))

filename <- paste(folder, "logSL_slope.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = logSL_slope_plot,
       width = 10,
       height = 7,
       units = "in")

#SL x Slope----

sl_slope<- core_coefs %>%
  filter(Variable == "Slope_start:sl_")

sl_slope_plot <- ggplot(data = sl_slope)+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = bar_w, linewidth = pnt_w)+
  labs(title = "SL:Slope Interaction")+
  geom_hline(yintercept = 0, linetype = 2)+
  xlab("Lion ID")+
  ylab("Coefficient")+
  theme_bw() +
  plot_theme +
  scale_shape_manual(values = c(21,22,23,24,4))+
  scale_color_manual(values = c(female_clr,male_clr))+
  scale_fill_manual(values = c(female_clr,male_clr))+
  scale_y_continuous(breaks = c(-9e-05, -6e-05, -3e-05, 0),
                     label = c(expression(paste("-9 x",10^{-5})),
                               expression(paste("-3 x",10^{-5})),
                               expression(paste("-6 x",10^{-5})),
                               "0"))

filename <- paste(folder, "SL_slope.png", sep = "")

ggsave(filename = filename,
       device = "png",
       plot = sl_slope_plot,
       width = 10,
       height = 7,
       units = "in")

coplot()

#All Core Coefs Plot----

costa_dat <- core_coefs %>%
  filter(Variable$Variable == "cos_ta_:tod_start_night"|
           Variable$Variable == "cos_ta_:Veg1m_start"|
           Variable$Variable == "Slope_start:cos_ta_")

costa_plot <- ggplot(costa_dat, aes(x = Lion, y = Beta$Coef))+
  geom_point(aes(x = Lion, y = Beta$coef, color = Sex, shape = Significant, fill = Sex), size = pnt_size)+
  geom_errorbar(aes(x = Lion, ymin = Low_CI$coef, ymax = High_CI$coef, color = Sex), width = bar_w, linewidth = pnt_w)+
  facet_wrap(~Variable$Variable)+
  plot_theme

#Core Model Stats----
core_stats <- core_coefs %>%
  group_by(Variable$Variable)%>%
  summarise(mean = mean(Beta$coef),
            sd = sd(Beta$coef),
            n = n())%>%
  mutate(se = sd/sqrt(n))

write.csv(core_stats, file = "/Users/rhemitoth/Documents/Lion_Movement/Manuscript/Tables/core_model_summary.csv")


