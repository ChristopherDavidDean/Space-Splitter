# Grid Selection from Shape Data

# Packages
library("sp")
library("maptools")
library("maps")
library("rgdal")
library("dismo")
library("XML")

# Loading Data
load("grid_frame.RData")
data_projected <- readOGR("0_Data/WYgeol_dd", "wygeol_dd_polygon")


# A simple way to print out a list of coordinates for each polygon in your shapefile:
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
