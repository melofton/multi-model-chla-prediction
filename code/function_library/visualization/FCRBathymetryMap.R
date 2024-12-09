# FCR Bathymetry Map in R
# Author: Mary Lofton
# Date: 09DEC24

# load packages
library(sf)
library(ggplot2)
library(ggmap)
library(ggspatial)

# read in shapefiles
layer1 <- read_sf("./data/data_processed/FCR_bathymetry_shapefiles/506_9.shp")
layer2 <- read_sf("./data/data_processed/FCR_bathymetry_shapefiles/504_9.shp")
layer3 <- read_sf("./data/data_processed/FCR_bathymetry_shapefiles/502_9.shp")
layer4 <- read_sf("./data/data_processed/FCR_bathymetry_shapefiles/500_9.shp")
layer5 <- read_sf("./data/data_processed/FCR_bathymetry_shapefiles/498_9.shp")
inf <- read_sf("./data/data_processed/FCR_bathymetry_shapefiles/inf2.shp")

# set x and y lims
xlim <- c(602890, 603300)
ylim <- c(4129087, 4129825)

# make data frame of sampling sites
sampling_sites <- data.frame(site_name = c("inflow weir","water quality","met station"),
                             x = c(603243.949, 603042.080, 603072.080), 
                             y = c(4129741.320, 4129147.407, 4129090.407))

# make map
map <- ggplot() +
  geom_sf(data = inf, aes(color = "Tunnel Branch")) +
  geom_sf(data = layer1, aes(fill = "0-2 m"), color = "white") +
  geom_sf(data = layer2, aes(fill = "2-4 m"), color = "white") +
  geom_sf(data = layer3, aes(fill = "4-6 m"), color = "white") +
  geom_sf(data = layer4, aes(fill = "6-8 m"), color = "white") +
  geom_sf(data = layer5, aes(fill = "8-10 m"), color = "white") +
  geom_point(data = sampling_sites, aes(x = x, y = y, shape = site_name), 
             color = "black", size = 3)+
  scale_fill_manual(values = c("8-10 m" = "#008ECE","6-8 m" = "#00A9E0",
                              "4-6 m" = "#59C7EB", "2-4 m" = "#A6E1F4",
                              "0-2 m" = "#CCEEF9"),
                    name = "Depth (m)")+
  scale_color_manual(values = c("Tunnel Branch" = "darkgray"), name = "")+
  scale_shape_manual(values = c("inflow weir" = 15,"met station" = 16,
                                "water quality" = 17), name = "")+
  xlab("")+
  ylab("")+
  ggtitle("Falling Creek Reservoir, Vinton, VA, USA")+
  coord_sf(xlim = xlim, ylim = ylim)+
  theme_classic()+
  ggspatial::annotation_scale(location = "br", bar_cols = c("grey60", "white"))+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(2.5, "in"),
    style = ggspatial::north_arrow_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    height = unit(0.3, "in"),
    width = unit(0.3, "in")
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

# save map
ggsave(map, file = "./figures/FCRBathymetryMap.png",device = "png")
