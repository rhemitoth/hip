plot_theme <- theme(axis.text.x = element_text(hjust = 0.5),
                    axis.title.x = element_text(face = "bold",size = 14),
                    axis.title.y = element_text(face = "bold",size = 14),
                    axis.text = element_text(size = 12),
                    legend.title = element_text(face = "bold", size = 14),
                    legend.text = element_text(size = 12),
                    plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
#Plot Data----
slope_spdf <- rasterToPoints(x = slope, spatial = TRUE)
slope_df <- data.frame(slope_spdf)

veg1m_spdf <- rasterToPoints(x = veg1m, spatial = TRUE)
veg1m_df <- data.frame(veg1m_spdf)

veg1.5m_spdf <- rasterToPoints(x = veg1.5m, spatial = TRUE)
veg1.5m_df <- data.frame(veg1.5m_spdf)

rivers_spdf <- rasterToPoints(x = rivers, spatial = TRUE)
rivers_df <- data.frame(rivers_spdf)

avg_ndvi_spdf <- rasterToPoints(x = ndvi%>%mask(hip), spatial = TRUE)
avg_ndvi_df <- data.frame(avg_ndvi_spdf)%>% mutate(measure = "Average NDVI")

ndvi_20140722_spdf <- rasterToPoints(ndvi_20140722%>%mask(hip), spatial = TRUE)
ndvi_20140722_df <- data.frame(ndvi_20140722_spdf)%>% mutate(measure = "07/22/2014 NDVI")
names(ndvi_20140722_df) <- c("Band_1", "x", "y", "optional", "measure")

ndvi_20140823_spdf <- rasterToPoints(ndvi_20140823%>%mask(hip), spatial = TRUE)
ndvi_20140823_df <- data.frame(ndvi_20140823_spdf)%>%mutate(measure = "08/23/2014 NDVI")
names(ndvi_20140823_df) <- c("Band_1", "x", "y", "optional", "measure")
ndvi_plot_dat <- rbind(avg_ndvi_df,ndvi_20140722_df,ndvi_20140823_df)

#ndvi plot----
ndvi_plot <- ggplot(data = ndvi_plot_dat, aes(x = x, y = y, fill = Band_1))+
  geom_raster()+
  scale_fill_gradientn(name = "NDVI", colors = ndvi_pal)+
  facet_wrap(~measure)+
  labs(title = "NDVI")+
  xlab("Easting")+
  ylab("Northing")+
  theme_bw()+
  plot_theme+
  theme(axis.text.x = element_text(angle = 90))
filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/presentation/ndvi.png")
ggsave(filename = filename,
       device = "png",
       plot = ndvi_plot,
       width = 10,
       height = 4,
       units = "in")

#kill sites----
ks_plot <- ggplot()+
  geom_polygon(data = hip, aes(x = long, y = lat), fill = "#a3b18a")+
  geom_point(data = data.rsf, aes(x = X, y = Y, color = Used))+
  scale_color_manual(values = c("#ffb703","#023047"), labels = c("Available", "Used"), size = )+
  labs(title = "Used and Available Kill Site Locations")+
  xlab("Easting")+
  ylab("Northing")+
  theme_bw()+
  plot_theme

hip_plot <- ggplot()+

filename <- paste("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/figures/presentation/ks_locs.png")
ggsave(filename = filename,
       device = "png",
       plot = ks_plot,
       width = 7,
       height = 5.625,
       units = "in")

#Home Ranges
par(mfrow=c(2,2))
plot(koku_distEdge)
plot(babe_distEdge)
plot(fanbelt_distEdge)
plot(madonna_distEdge)

#Hip
HiP <- ggplot()+
  geom_polygon(data = hip, aes(x = long, y = lat), fill = "#fefae0")+
  labs(title = "HiP")+
  xlab("Easting")+
  ylab("Northing")+
  theme_bw()+
  plot_theme+
  theme(panel.background = element_rect(fill = '#606c38', color = '#606c38'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#All L
