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
map('state', region = "wyoming")
plot(spdf, add = TRUE)
plot(data_projected, add= TRUE)


x = as(SpatialPixelsDataFrame(xy, xy@data, tolerance=.00086),
       "SpatialPolygonsDataFrame")

## STEP 2. ##

data_projected[[10]][[8]]

sapply(slot(data_projected, "polygons"), function(x) slot(x, "ID"))

data_projected@polygons[[1]]@Polygons[[1]]@coords # line to extract all coordinates for single polygon.



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


#### Test idea

test_polys <- c()
temp_checker <- 0 # to check whether ID has already been covered
for(i in 1:length(grid_frame[[1]])){ # Loop through grid_frame
    for(r in 1:length(unique(grid_frame[[1]]))) { # Then loop through unique values in grid_frame
      if (grid_frame[[1]][i] == r){ # If current ID is the same as ID in frid_frame
        if(temp_checker == r) next # if it's been covered before, skip it
        else { # otherwise
          temp_poly <- Polygon(cbind(c(grid_frame[[i,2]],grid_frame[[(i+1),2]], grid_frame[[(i+2),2]], grid_frame[[(i+3),2]] ),
                                     c(grid_frame[[i,3]], grid_frame[[(i+1),3]], grid_frame[[(i+2),3]], grid_frame[[(i+3),3]]))) # Make polygon out of 4 coordinates for grid
          temp_list <- Polygons(list(temp_poly), r) # add that polygon to a list, complete with ID ref
          temp_checker <- as.numeric(grid_frame[[1]][i]) # update temp_checker
          }
        }
    }
}

temp_poly <- Polygon(cbind(c(grid_frame[[i,2]],grid_frame[[(i+1),2]], grid_frame[[(i+2),2]], grid_frame[[(i+3),2]] ),
                           c(grid_frame[[i,3]], grid_frame[[(i+1),3]], grid_frame[[(i+2),3]], grid_frame[[(i+3),3]])))
temp_list <- Polygons(list(temp_poly), r)
temp_list2 <- temp_list
