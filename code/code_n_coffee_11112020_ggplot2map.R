# Creating a simple thematic map using ggplot2 in R
# Code and Coffee, 11 November 2020
# Started 2020-09-07

library(sf)
library(rgdal)
library(tidyverse)

####################################################################

# Where to find shapefiles:

# Montreal Open Data Portal: http://donnees.ville.montreal.qc.ca/
# Statistics Canada (all Canada shapefiles): https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm 
# Strategic googling...
# McGill Geographic Information Centre: https://gic.geog.mcgill.ca/

#####################################################################

# Load Forward Sortation Areas shapefile (ie first 3 digits of postal code)
URL <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gfsa000b11a_e.zip"
download.file(URL, destfile="./data/gfsa000b11a_e.zip")
unzip("./data/gfsa000b11a_e.zip", overwrite=F, exdir="./data/gfsa000b11a_e")
fsa <-  readOGR("./data/gfsa000b11a_e") # for all of Canada! do not try to plot, it will be slow

fsa_trans <- spTransform(fsa, CRS("+proj=aea +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
fsa <- st_as_sf(fsa_trans)
head(fsa)

fsaqc <- fsa %>% filter(PRNAME == "Quebec / Québec")

# Add data we want to map over the shapefile
moosedata <- read.csv("./data/code_n_coffee_11112020_ggplot2map_data.csv")
head(moosedata)

# Merge simulated data with shapefile
fsaqc_merged <- merge(fsaqc, moosedata, by="CFSAUID") # Unique IDs for Forward Sortation Areas 

ggplot() + 
  geom_sf(data = fsaqc_merged, alpha=0.8, aes(fill=moose)) + 
  scale_fill_distiller(palette = "Reds", name="Mooses per capita", direction=1) +
  ggtitle("Moose Hazard Status in Quebec") + 
  # Below are just formatting parameters to make it look nicer
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_rect(),
        plot.title = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        panel.grid = element_blank())

# To make this a better map I'd add a background shapefile for the rest of Canada, ocean/water shapefiles, etc.
