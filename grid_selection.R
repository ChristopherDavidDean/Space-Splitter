# Grid Selection from Shape Data

# Packages
library("sp")
library("maptools")
library("maps")
library("rgdal")
library("dismo")
library("XML")
library("ggplot2")

## PLAN ##
# 1. Input dataframe of gridpoints and shapefile in the same co-ordinate system
# 2. Extract all coordinates for polygons of outcrop
# 3. For each square of grid, check what polygons fall within it (order will be important here) and record the relevant IDs
# 4. Then use inpolygon to check each corner of a grid square to see if it exists inside a polygon.
# 5. Record this information, and compare with results from step 3.
# 6. Compile final list of all relevant Grid Squares.

## STEP 1. ##

# Loading Data (using sp)
load("grid_frame.RData")
xy <- grid_frame[,c(2,3)]
xy[ , c(1,2)] <- xy[ , c(2,1)]
spdf <- SpatialPointsDataFrame(coords = xy, data = grid_frame, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
data_projected <- readOGR("0_Data/WYgeol_dd", "wygeol_dd_polygon")
data_projected@proj4string
data_projected <- spTransform(data_projected, CRS("+proj=longlat +datum=WGS84"))

# Testing whether data has plotted correctly
map("usa")
plot(spdf, add = TRUE)
plot(data_projected, add= TRUE)


## STEP 2. ##

dataprojected2@polygons[[2]]@Polygons[[1]]@coords



############################################ JUNK ############################################################

x = GridTopology(c(0,0), c(1,1), c(5,4))
class(x)
x
summary(x)


####### A simple way to print out a list of coordinates for each polygon in your shapefile: #######
# Path and filename of polygon shapefile

testfile <- '0_Data/WYgeol_dd/wygeol_dd_polygon.shp'

# Read in polygon shapefile using handy maptools function
test <- readShapePoly(testfile)

ob <- SpatialPolygons(spp@polygons,proj4string=spp@proj4string)

# Extract the list of Polygons objects
polys <- slot(test,"polygons")

# Within each Polygons object
#    Extract the Polygon list (assuming just one per Polygons)
#    And Extract the coordinates from each Polygon
for (i in 1:length(polys)) {
  print(paste("Polygon #",i))
  print(slot(slot(polys[[i]],"Polygons")[[1]],"coords"  )) 
}

### define SpatialGrid object
bb <- bbox(shp) # this is finding dimensions of the original shapefile. Don't want this. Need to use grid_frame

cs <- c(3.28084, 3.28084)*6000  # cell size 6km x 6km (for illustration)
# 1 ft = 3.28084 m

cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
grd
# cellcentre.offset 923018 129964
# cellsize           19685  19685
# cells.dim              8      8

sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(shp)))
summary(sp_grd)

sapply(slot(testfile, 'polygons'), function(i) slot(i, 'area')) 
