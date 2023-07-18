#Plot Theme
plot_theme <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
                    axis.text.y = element_text(size = 15),
                    axis.title.x = element_text(face = "bold",size = 18, color = "grey15"),
                    axis.title.y = element_text(face = "bold",size = 18, color = "grey15"),
                    legend.title = element_text(face = "bold", size = 18),
                    legend.text = element_text(size = 15),
                    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
                    plot.subtitle = element_text(size = 18, hjust = 0),
                    panel.grid.major = element_blank(), #remove major gridlines
                    panel.grid.minor = element_blank(), #remove minor gridlines,
                    legend.key = element_rect(colour = NA, fill = NA))

#Plot Params----
male_clr <- "cornflowerblue"
female_clr <- "tan1"

pnt_size <- 3 #size of points
pnt_w <- 1 #horizontal length of error bar
bar_w <- 0.4 #thickness of error bar

#Plot Size----
w <- 10
h <- 7
