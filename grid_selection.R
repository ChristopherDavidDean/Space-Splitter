# Grid Selection from Shape Data

# Packages
library("sp")
library("maptools")
library("maps")
library("rgdal")
library("dismo")
library("XML")
library("ggplot2")
library("rgeos")

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
grid_frame <- grid_frame[c("id","lon","lat")]
xy <- grid_frame[,c(2,3)]
spdf <- SpatialPointsDataFrame(coords = xy, data = grid_frame, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Projecting Data
data_projected <- readOGR("0_Data/WYgeol_dd", "wygeol_dd_polygon")
data_projected@proj4string
data_projected <- spTransform(data_projected, CRS("+proj=longlat +datum=WGS84"))

# Testing whether data has plotted correctly
map('state', region = "wyoming")
plot(spdf, add = TRUE)
plot(data_projected, add= TRUE)

## STEP 2. ##

# Make grid points into unique polygons for use in sp(over)

comp_polys <- list()
temp_checker <- 0 # to check whether ID has already been covered
for(i in 1:length(grid_frame[[1]])){ # Loop through grid_frame
  for(r in 1:length(unique(grid_frame[[1]]))) { # Then loop through unique values in grid_frame
    if (grid_frame[[1]][i] == r){ # If current ID is the same as ID in frid_frame
      if(temp_checker == r) next # if it's been covered before, skip it
      else { # otherwise
        temp_poly <- Polygon(cbind(c(grid_frame[[i,2]], grid_frame[[(i+1),2]], grid_frame[[(i+2),2]], grid_frame[[(i+3),2]] ),
                                   c(grid_frame[[i,3]], grid_frame[[(i+1),3]], grid_frame[[(i+3),3]], grid_frame[[(i+2),3]]))) # Make polygon out of 4 coordinates for grid
        temp_list <- Polygons(list(temp_poly), r) # add that polygon to a list, complete with ID ref
        comp_polys[[r]] <- temp_list
        temp_checker <- as.numeric(grid_frame[[1]][i]) # update temp_checker
      }
    }
  }
}

spatial_comp_polys <- SpatialPolygons(comp_polys, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

results <- over(spatial_comp_polys, data_projected) # checking which gridsquares contain oputcrop

results$ID # full results

na.omit(results) # results with NAs omitted

rownames(results) # IDs of relevant grid squares