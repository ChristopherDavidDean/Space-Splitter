# Grid Selection from Shape Data

# Packages
library("sp")
library("maptools")
library("maps")
library("rgdal")
library("dismo")
library("XML")


## PLAN ##
# 1. Input dataframe of gridpoints and shapefile in the same co-ordinate system
# 2. use inpolygon to check each corner of a grid square to see if it exists inside a polygon.
# 3. Record this information, and how many corners are present within it.


# Loading Data (using sp)
load("grid_frame.RData")
xy <- grid_frame[,c(2,3)]
spdf <- SpatialPointsDataFrame(coords = xy, data = grid_frame,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


data_projected <- readOGR("0_Data/WYgeol_dd", "wygeol_dd_polygon")





P4S.latlon <- CRS("+proj=longlat +datum=WGS84")

####### A simple way to print out a list of coordinates for each polygon in your shapefile: #######
# Path and filename of polygon shapefile

testfile <- '0_Data/WYgeol_dd/wygeol_dd_polygon.shp'

# Read in polygon shapefile using handy maptools function
test <- readShapePoly(testfile)
# Extract the list of Polygons objects
polys <- slot(test,"polygons")

# Within each Polygons object
#    Extract the Polygon list (assuming just one per Polygons)
#    And Extract the coordinates from each Polygon
for (i in 1:length(polys)) {
  print(paste("Polygon #",i))
  print(slot(slot(polys[[i]],"Polygons")[[1]],"coords"  )) 
}

