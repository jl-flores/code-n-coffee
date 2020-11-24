# Presenters: Susannah Ripley & Martha Lee

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



#############
# Part 2 - spatial interpolation

# download data

Df <- data.frame() # empty dataframe
# make seq of dates for part of 2017
Dates <- format(seq(as.Date("2017-01-01"), as.Date("2017-02-28"), by="days"), format="%Y%m%d")

for(i in 1:length(Dates)) { # download data from website
  url <- paste("https://quotsoft.net/air/data/beijing_all_", Dates[i], ".csv", sep = "") # complete the url
  x = read.csv(file=url, encoding="UTF-8", stringsAsFactors=FALSE) # read in the data
  Df <- rbind(Df, x)
}
# check that is load in correctly
tail(Df)
head(Df)

# average the PM2.5 by station
Df$Year <- substr(as.character(Df$date), 1, 4) # the year, the first 4 characters of the date
m.ag <- aggregate(Df[4:38], data.frame(Df$Year), mean,na.rm = T) # sum columns (stations) by year

# transpose the rows and column
Data <- m.ag %>% 
  gather(station, value, -Df.Year) %>% 
  spread(Df.Year, value)
Data

################################
# spatial interpolation of air pollution Beijing
###############################

library(dplyr); library(tidyverse); library(sf); library(rgdal); library(spatstat); library(maptools); library(raster); library(tmap)

# used these sources for help
# https://rspatial.org/raster/analysis/4-interpolation.html
# https://andrewpwheeler.com/2016/11/02/some-inverse-distance-weighting-hacks-using-r-and-spatstat/
# https://mgimond.github.io/Spatial/interpolation-in-r.html

#####################
# add in the stations
Lines <- "Site  NameSite   Long   Lat   Type  
东四	东城东四	116.417	39.929	Urban
天坛	东城天坛	116.407	39.886	Urban
官园	西城官园	116.339	39.929	Urban
万寿西宫	西城万寿西宫	116.352	39.878	Urban
奥体中心	朝阳奥体中心	116.397	39.982	Urban
农展馆	朝阳农展馆	116.461	39.937	Urban
万柳	海淀万柳	116.287	39.987	Urban
北部新区	海淀北部新区	116.174	40.09	Urban
植物园	海淀北京植物园	116.207	40.002	Urban
丰台花园	丰台花园	116.279	39.863	Urban
云岗	丰台云岗	116.146	39.824	Urban
古城	石景山古城	116.184	39.914	Urban
房山	房山良乡	116.136	39.742	Suburban
大兴	大兴黄村镇	116.404	39.718	Suburban
亦庄	亦庄开发区	116.506	39.795	Suburban
通州	通州新城	116.663	39.886	Suburban
顺义	顺义新城	116.655	40.127	Suburban
昌平	昌平镇	116.23	40.217	Suburban
门头沟	门头沟龙泉镇	116.106	39.937	Suburban
平谷	平谷镇	117.1	40.143	Suburban
怀柔	怀柔镇	116.628	40.328	Suburban
密云	密云镇	116.832	40.37	Suburban
延庆	延庆镇	115.972	40.453	Suburban
定陵	昌平定陵	116.22	40.292	Background
八达岭	京西北八达岭，京西北区域点	115.988	40.365	Background
密云水库	京东北密云水库，京东北区域点	116.911	40.499	Background
东高村	京东东高村，京东区域点	117.12	40.1	Background
永乐店	京东南永乐店，京东南区域点	116.783	39.712	Background
榆垡	京南榆垡，京南区域点	116.3	39.52	Background
琉璃河	京西南琉璃河，京西南区域点	116	39.58	Background
前门	前门东大街，前门交通点	116.395	39.899	Traffic
永定门内	永定门内大街，永定门交通点	116.394	39.876	Traffic
西直门北	西直门北大街，西直门交通点	116.349	39.954	Traffic
南三环	南三环西路，南三环交通点	116.368	39.856	Traffic
东四环	东四环北路，东四环交通点	116.483	39.939	Traffic  "
DF2 <- read.table(text = Lines, check.names = FALSE, header = T) # set first line as headers 
m.Df <- merge(Data, DF2, by.x = "station", by.y = "Site") 
coordinates(m.Df) <- ~ Long + Lat # make a spatialpointdataframe, sf doesn't seem to work well with the idw functions right now

######################
# Make tessellated surfaces of PM2.5 (proximity interpolation)
#######################
# Assign to all unsampled locations the value of the closest sampled location. 
# This generates a tessellated surface whereby lines that split the midpoint between each sampled location are 
# connected thus enclosing an area. Each area ends up enclosing a sample point whose value it inherits.

shape <- readOGR(dsn = "~/OneDrive - McGill University/McGill/SnowGlobe/Coal Ban/CHN_adm/CHN_adm1.shp")
China <- shape[shape$NAME_1 == "Beijing",] # have to get only one polygon cause multiple polygons confuse it - think it is was of the bbox extent

m.Df@bbox <- China@bbox #forces the rectangular extent of the Beijing map onto the point data object

# Create a tessellated surface
# note from source (https://mgimond.github.io/Spatial/interpolation-in-r.html)
# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th  <-  as(dirichlet(as.ppp(m.Df)), "SpatialPolygons")  # Computes the Dirichlet tessellation of a spatial point patter
proj4string(th) <- proj4string(m.Df)

th.z     <- over(th, m.Df, fn=mean) # consistent spatial overlay for points, grids and polygons
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
th.clp   <- raster::intersect(China,th.spdf) # cliping to Beijing

# Map the data
# quartz()
tm_shape(th.clp) + 
  tm_polygons(col="X2017", auto.palette.mapping=FALSE,
              title=expression(paste('Predicted PM'[2.5]," (",mu,"g/",m^3, ")"))) +
  tm_legend(legend.outside=TRUE) +
  tm_shape(m.Df) +
  tm_symbols(col = "2017", scale = .5, legend.col.show = FALSE)
# the points are the same colour as the polygons since the surface values are only based on assigned based on the closest monitor

######################
# Make IDW surfaces of PM2.5 - deterministic interpolation method
#######################

# The IDW technique computes an average value for unsampled locations using values from nearby weighted 
# locations. The weights are proportional to the proximity of the sampled points to the unsampled 
# location and can be specified by the IDW power coefficient. 
#  J = (z1/dist1^p + z2/dist2^p)/(1/dist1^p + 1/dist2^p)

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(m.Df, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(grd) <- proj4string(m.Df)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
# The exponent of distance. Controls the significance of surrounding points on the interpolated value. A higher power results in less influence from distant points. It can be any real number greater than 0, but the most reasonable results will be obtained using values from 0.5 to 3. The default is 2.
# idw parameters (distance decay and number of neighbours)
P.idw <- gstat::idw(X2017 ~ 1, m.Df, newdata=grd, idp=2.0)
# in this example the maxmium distance is set to infinit 

# Convert to raster object then clip to Beijing
r       <- raster(P.idw)
r.m     <- mask(r, China)

# it is best of optimize the relations to find the right parameters (distance decay and number of neighbours (i.e. the number of obsveration you use))
# can use RMSE (https://rspatial.org/raster/analysis/4-interpolation.html) or LOO (https://mgimond.github.io/Spatial/interpolation-in-r.html)

# plot using tmap
# The tmap package mapping is closer to ggplot. Use tm_shape to add in another layer on the map
tm_shape(r.m) + 
  tm_raster(n=10, auto.palette.mapping = FALSE,
            title=expression(paste('2017 Predicted PM'[2.5]," (",mu,"g/",m^3, ")")))  +
  tm_legend(legend.outside=TRUE) +
  tm_shape(m.Df) +
  tm_symbols(n=10, col = "2017", scale = .5, legend.col.show = FALSE)
