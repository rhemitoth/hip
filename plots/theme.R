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
male_clr <- "#5BBCD6"
female_clr <- "#F2AD00"

day_clr <- "#E2D200"
night_clr <- "#899DA4"

pnt_size <- 3 #size of points
pnt_w <- 1 #horizontal length of error bar
bar_w <- 0.4 #thickness of error bar

#Plot Size----
w <- 10
h <- 7

#Wes anderson palette----
wes_palettes <- list(
  BottleRocket1 = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707"),
  BottleRocket2 = c("#FAD510", "#CB2314", "#273046", "#354823", "#1E1E1E"),
  Rushmore1 = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
  Rushmore = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
  Royal1 = c("#899DA4", "#C93312", "#FAEFD1", "#DC863B"),
  Royal2 = c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089"),
  Zissou1 = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),
  Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
  Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
  Chevalier1 = c("#446455", "#FDD262", "#D3DDDC", "#C7B19C"),
  FantasticFox1 = c("#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20"),
  Moonrise1 = c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A"),
  Moonrise2 = c("#798E87", "#C27D38", "#CCC591", "#29211F"),
  Moonrise3 = c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B"),
  Cavalcanti1 = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15"),
  GrandBudapest1 = c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236"),
  GrandBudapest2 = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4"),
  IsleofDogs1 = c("#9986A5", "#79402E", "#CCBA72", "#0F0D0E", "#D9D0D3", "#8D8680"),
  IsleofDogs2 = c("#EAD3BF", "#AA9486", "#B6854D", "#39312F", "#1C1718"),
  FrenchDispatch = c("#90D4CC", "#BD3027", "#B0AFA2", "#7FC0C6", "#9D9C85")
)

