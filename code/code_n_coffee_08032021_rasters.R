# BASICS OF RASTERS IN R

# Required packages
library(raster)
library(rgdal)
library(sp)

# Based on content from these 2 tutorials:
# https://www.neonscience.org/resources/learning-hub/tutorials/raster-res-extent-pixels-r
# https://www.neonscience.org/resources/learning-hub/tutorials/raster-data-r

# Terms: 
# Rasters = series of pixels
# Spatial resolution = the size of the area on the surfact that each pixel covers, e.g. if 1m spatial resolution, then each pixel represents a 1m x 1m area
# Spatial Extent = x,y coordinates of the corners of the raster on a spatial grid (within a given CRS), e.g. left bottom corner: x min, y min etc.

#### CREATING AND ASSIGNING VALUES TO A RASTER #### 

# Create a blank raster 
# Create a raster from a matrix - a "blank" raster of 4x4 (4 rows 4 columns, i.e. 16 cells)
rast1 <- raster(nrow=4, ncol=4)
rast1
# assign values to raster: 1 to n based on the number of cells in the raster (so in this case 1 to 16)
rast1[]<- 1:ncell(rast1)
# You'll now see a new output row with "values":
rast1

# plotting the raster
plot(rast1, useRaster=FALSE, main="Plot of Raster")

# Making a higher res raster: 
# 64 pixel raster
rast2 <- raster(nrow=8, ncol=8)
rast2[]<- 1:ncell(rast2)
# plot 
plot(rast2, useRaster=FALSE, main="Higher Res Raster")

#### LOADING AN EXISTING RASTER INTO R ####

# raster as R object
DEM_raster <- raster("DigitalTerrainModel/SJER2013_DTM.tif") 

# view raster attributes
DEM_raster
# view the extent of a raster: 
DEM_raster@extent # can also use @ to view other attributes, e.g. CRS

#### MAKING A HISTOGRAM OF DISTR OF RASTER VALUES  #### 

# make a histogram of the distribution of values in the raster
hist(DEM_raster, main="distr of values in raster")
# default max. of pixels is 100,000 but can plot more using maxpixels=

#### PLOTTING A RASTER ####
# plot function (used above for rast1 & 2) renders a limited number of pixels (100,000)
# so image function better for rendering larger rasters:
image(DEM_raster, main="Another plot")
# you can also specify the range of values you want to plot
# e.g. plot pixels between 250 and 300 (in this case meters because these data are about elevation)
image(DEM_raster, zlim=c(250,300), main="250-300m elevation only")

# Change plot colors: 
# specify # of colors in brackets. can also use heat.colors, or rainbow
col1 <- terrain.colors(5)
plot(DEM_raster, main="Aaand another", col=col1)
# color breaks: by default, colors will be assigned uniformly, but you can specify where color breaks should occur: 
brks <- c(250, 300, 350, 400, 450, 500)
plot(DEM_raster, col=col1, breaks=brks, main="diff breaks")

#### DEFINING THE EXTENT OF A RASTER ####

# e.g., create a raster with bottom left coordinates at: xmin=254570, ymin=4107302
# create 10x20 matrix with values 1-8. 
matrix1 <- (matrix(1:8, nrow = 10, ncol = 20))
# convert matrix to raster
matrix1_rast <- raster(matrix1)
matrix1_rast
# define the extent by defining bottom left corner (xmin, ymin)
ymin <- 4107302
xmin <- 254570
# define resolution: 
res <- 1
# add number of cols and rows in the raster to the xy corner location. This gives the bounds of the raster extent.
xmax <- xmin + (matrix1_rast@ncols * res)
ymax <- ymin + (matrix1_rast@nrows * res)
# create raster extent
ras_ext <- extent(xmin, xmax, ymin, ymax)
ras_ext 
# apply extent to raster 
matrix1_rast@extent <- ras_ext
# check 
matrix1_rast # extent values now correspond to the x and y values specified.
