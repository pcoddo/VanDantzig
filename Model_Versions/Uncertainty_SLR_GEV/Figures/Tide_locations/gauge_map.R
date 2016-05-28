###################################
# file: gauge_map.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Plots sites of Hook van Holland and
# Delfzijl tide gauges
#################################### 

# Set working directory:
#setwd("~/Documents/Grad/SCRiM/vanDantzig/Model_Versions/Uncertainty_SLR_GEV/Figures/Tide_locations")

# Load libraries
library(ggplot2)
library(ggmap)
source("../../Scripts/mycolors.R")

# Read in locations of tide gauges
gauges = read.table("../../Storm_Surge_Module/Data/gauges.txt", header = TRUE, sep = '\t')

pdf("gauge_map.pdf", width = 6, height = 6)
mylocation = c(3, 51, 8, 54)
map_layer <- get_map(mylocation,
                     source = "stamen",
                     maptype = "watercolor",
                     crop = FALSE)
ggmap(map_layer, extent = "panel", legend = "none", crop = FALSE) +
  labs(x = "Longitude [degrees]", y = "Latitude [degrees]") +
  theme(axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12)) +
  
  geom_point(data = gauges[1,], aes(x = longitude, y = latitude, size = 10), pch = 21, bg = myred) +
  geom_point(data = gauges[2,], aes(x = longitude, y = latitude, size = 10), pch = 21, bg = myblue) +
  annotate('text', x=gauges[1,2], y=gauges[1,3]-0.1, label = 'Hook van Holland', col = "black", face = "bold") +
  annotate('text', x=gauges[1,2], y=gauges[1,3]-0.25, label = 'van Dantzig [1956]', col = "black", fontface = 3, size = 3.5) +
  annotate('text', x=gauges[2,2], y=gauges[2,3]-0.1, label = 'Delfzijl', col = "black", face = "bold") +
  annotate('text', x=gauges[2,2], y=gauges[2,3]-0.25, label = 'This study', col = "black", fontface = 3, size = 3.5) +
  
  theme(legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

dev.off()
