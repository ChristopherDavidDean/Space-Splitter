###########################################################################################
#   _____ _____  _____ _____     _____ _____  _      _____ _______ _______ ______ _____   #
#  / ____|  __ \|_   _|  __ \   / ____|  __ \| |    |_   _|__   __|__   __|  ____|  __ \  #
# | |  __| |__) | | | | |  | | | (___ | |__) | |      | |    | |     | |  | |__  | |__) | #
# | | |_ |  _  /  | | | |  | |  \___ \|  ___/| |      | |    | |     | |  |  __| |  _  /  #
# | |__| | | \ \ _| |_| |__| |  ____) | |    | |____ _| |_   | |     | |  | |____| | \ \  #
#  \_____|_|  \_\_____|_____/  |_____/|_|    |______|_____|  |_|     |_|  |______|_|  \_\ #
#                                                                                         #
###########################################################################################
# Written by Christopher Dean and Dominic Bennett, 2016. 
# General Public License 2
# R script for splitting grid space

# Variables:
# lats = vector of lists for the max and min values of each grid in lat-space
# lons = vector of lists for the max and min values of each grid in lon-space
# grid = matrix of indexes pointing to the lists in lats and lons


# Steps:
# 1. Create lists of the min and max values for each lon and lat (calls them lats and lons)
# 2. construct a grid using the expand.grid function that will find all possible combinations
#   for the lats and lons
# 3. Loop through each element in the grid and determine which observations are in which grid square
# 4. Produce a vector of each grid element index for each observation [RESULT]

## FUNCTIONS ##
genLonsandLats <- function(data, gsize=0.5) {
  # create list of list of list of grid cells
    lat <- data$Lat_Pub
    lon <- data$Long_Pub
    allLats <- sort(lat, na.last = NA)
    min_lat <- allLats[1]
    max_lat <- allLats[length(allLats)]
    allLongs <- sort(lon, na.last = NA)
    min_lon <- allLongs[1]
    max_lon <- allLongs[length(allLongs)]
    max_lat <- max_lat + 0.5
    max_lon <- max_lon - 0.5
    # LATS!
    lats_values <- seq(from=min_lat, to=max_lat, by=gsize) #Makes the lats grid spaces
    lons_values <- seq(from=min_lon, to=max_lon, by=gsize)
      if (length(lats_values)<length(lons_values)){ # This section checks to see if list lengths are even and if not makes sure they are.
        len <- length(lons_values)-length(lats_values)
        for(i in 1:len){
          lats_values <- c(lats_values, lats_values[length(lats_values)]+0.5)
          }
        }
      if (length(lons_values)<length(lats_values)){
        len <- length(lats_values)-length(lons_values)
        for(i in 1:len){
          lons_values <- c(lons_values, lons_values[length(lons_values)]-0.5) # Minus is in here as longitude of WIS is negative.
          }
        }
    
    lats_list <- list() # Make a list
      for(i in 2:length(lats_values)) { #Start from 2nd position, to length of the values of lats_values
        lats_list[[i-1]] <- list(min=lats_values[i-1], max=lats_values[i]) #at point number 1 in lats list, make a list of the min and max lat values for that grid square
      }
    
    # LONS!
    lons_list <- list()
      for(i in 2:length(lons_values)) {
        lons_list[[i-1]] <- list(min=lons_values[i-1], max=lons_values[i])
      }
    
    latslons <- list("lats"=lats_list,
                     "lons"=lons_list) # Make a list of the other two lists of lists
    
    return(latslons) #return the final list
}


## INPUT AND RUNNING LATLONG FUNCTION ##
usgs <- read.csv("0_Data/R__Data_1.22.2.16.csv", stringsAsFactors=FALSE) # input your own data here.
latslons <- genLonsandLats(usgs)

## CREATE GRID-SPACE ##
lats <- latslons[["lats"]]  # unpack lats and lons
lons <- latslons[["lons"]]  
grid <- expand.grid(1:length(lats),1:length(lons)) # make grid using lats and lons make from your selections
colnames(grid) <- c("lat", "lon")
save(lats, lons, grid, file="grid.RData")

## FINDING OCCS FOR EACH GRID SQUARE - STILL NOT WORKING INSIDE OF LOOP PROPERLY ##

# Setup 
usgs$near_far <- sample(2, size = 100, replace = TRUE) # Makes random near (1) and far(2) counts for rows
gridlist <- list() # the grid list
nflist <- list() # Near/Far list
the_failed <- vector()

for(r in 1:nrow(usgs)) {
  cat('... [', r, ']\n', sep="")
  cur_lat <- usgs$Lat_Pub[r] # lat for the current row
  cur_lon <- usgs$Long_Pub[r] # long for the current row
  temprec <- vector() # temproary record of the lat and long numbers for reference to the grid.
  tempgridrec <- 0 # temporary record of appropriate grid number
  
  for(i in 1:length(lats)){ # add the latitude grid number if current lat falls within its bounds
    if(as.numeric(lats[[i]][1]) <= cur_lat && cur_lat <= as.numeric(lats[[i]][2])){
      temprec[1] <- i
    }
  }
  for(i in 1:length(lons)){  # add the longitude grid number if current lat falls within its bounds
    if(as.numeric(lons[[i]][1]) <= cur_lon && cur_lon <= as.numeric(lons[[i]][2])){
      temprec[2] <- i
    } 
  }
  
  # temp solution, skip if temprec is less than 2
  if(length(temprec) < 2 | is.na(temprec[1])) {
    the_failed <- c(r, the_failed)
    next
  }
  
  for(g in 1:nrow(grid)){ # go through rows in the grid dataframe, if both temp recs match then record to tempgridrec
    if(temprec[1] == grid$lat[g] && temprec[2] == grid$lon[g]){
    tempgridrec <- g
    }
  }
  
  
  sp <- usgs$species[r] # sets species for the current row
  nf <- usgs$near_far[r] # sets near or far for current row
  if(length(gridlist) < tempgridrec) { # if list isn't as long as the grid square it's to be added to
    gridlist[[tempgridrec]] <- sp # just add the species
    nflist[[tempgridrec]] <- nf
  } else {
    gridlist[[tempgridrec]] <- c(gridlist[[tempgridrec]], sp) # otherwise add it on to the end (stops overwriting)
   nflist[[tempgridrec]] <- c(nflist[[tempgridrec]], nf)
  }
}

# [RESULT]

occs <- unlist(lapply(gridlist, function (x) length(x))) # gives vector of total species numbers in the lists.
sum(occs)
cat(sum(occs),'/',nrow(usgs), ' ', 'results recorded. Percentage of total: ', ((sum(occs)/nrow(usgs))*100),'%', sep="")


## GRID SELECTION FROM SHAPEFILE DATA ##

# Packages
library("sp")
library("maptools")
library("maps")
library("rgdal")
library("dismo")
library("XML")
library("ggplot2")
library("rgeos")

# PLAN #
# 1. Input dataframe of gridpoints and shapefile in the same co-ordinate system
# 2. Extract all coordinates for polygons of outcrop
# 3. For each square of grid, check what polygons fall within it (order will be important here) and record the relevant IDs
# 4. Then use inpolygon to check each corner of a grid square to see if it exists inside a polygon.
# 5. Record this information, and compare with results from step 3.
# 6. Compile final list of all relevant Grid Squares.

# STEP 1. #

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

# STEP 2. #

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


## STATISTICS ##

occs[420:625] <- 0 # fill in spaces with no species up to max grid square number

grid$occs <- occs

nflist <- (lapply(nflist, mean))
nflist <- (lapply(nflist, round)) # currently set at average - however, could be made to be 70% or something similar.
nflist[420:625] <- NA
grid$NF <- nflist

nf <- grid$NF
n <- length(which(nf == 1)) # number of grid squares classified as NEAR
f <- length(which(nf == 2)) # number of grid squares classified as FAR

Noccs <- 0
Foccs <- 0
grid[is.na(grid)]  <- 0

for(i in 1:nrow(grid)){ # calculates totals of species for both near and far.
  if(grid$NF[i] == 1){
    Noccs <- Noccs + grid$occs[i]
  }
  if(grid$NF[i] == 2){
    Foccs <- Foccs + grid$occs[i]
  }
}
PercNOccs <- (Noccs/(Noccs+Foccs))*100 # percentage of total species in Near environment.

## SHUFFLING ##

# First need to remove all zero values (just so it vaguely works)

newgrid <- subset(grid, grid$NF == 1 |grid$NF == 2)

# Then randomise!

NSpecsMod <- c()
times <- 10000
for(n in 1:times){
tempgrid <- (transform(newgrid, NF = sample(NF) )) # worked!
tempNoccs <- 0
tempFoccs <- 0
for(i in 1:nrow(tempgrid)){ # calculates totals of species for both near and far.
  if(tempgrid$NF[i] == 1){
    tempNoccs <- tempNoccs + tempgrid$occs[i]
  }
  if(tempgrid$NF[i] == 2){
    tempFoccs <- tempFoccs + tempgrid$occs[i]
  }
}
NSpecsMod <- c(NSpecsMod,(tempNoccs/(tempNoccs+tempFoccs))*100)
}

p <- sum((abs(NSpecsMod)<=PercNOccs))/times

hist(NSpecsMod, col='black', breaks=100,main=NULL)
arrows(PercNOccs,times/100,PercNOccs,0,col="red",lwd=2)
text(PercNOccs,times/100,pos=3, paste("dmean =",round(PercNOccs,2)))
mtext(paste(type, "( iterations =", times, ")"),line=1)
mtext(paste("p =",p))

# Works!!!!!

############ DEFUNCT ##############

# DESIGNATE
#res <- rep(NA, length(obs_lons))
#for (i in 1:nrow(grid)) {
#  lon <- lons[[grid[i, "lon"]]]
#  lat <- lats[[grid[i, "lat"]]]
  #   cat("Corrds lon:\n")
  #   cat("... min:", coords_lon$min, "\n")
  #   cat("... max:", coords_lon$max, "\n")
  #   cat("Corrds lat:\n")
  #   cat("... min:", coords_lat$min, "\n")
  #   cat("... max:", coords_lat$max, "\n")
#  res_bool <- obs_lons >= lon$min & obs_lons < lon$max &
#    obs_lats >= lat$min & obs_lats < lat$max
#  res_is <- which(res_bool)
#  res[res_is] <- i
#}

# constructObs <- function(lat, lat) {
#   # combine two separate vectors into a single list of $lon and $lat
#   obs <- list()
#   for(i in 1:length(lon)) {
#     obs[[i]] <- list(lon=lon[i], lat=lat[i])
#   }
#   obs
# }
#
# Write function to find max/min Lats and Longs - DONE AND PUT STRAIGHT INTO ORIGINAL ONE!
#MaxMinLatLong <- function(data) {
#  lat <- data$Lat_Pub
#  lon <- data$Long_Pub
#  allLats <- sort(lat, na.last = NA)
#  min_lat <- allLats[1]
#  max_lat <- allLats[length(allLats)]
# allLongs <- sort(lon, na.last = NA)
#  min_lon <- allLongs[1]
#  max_lon <- allLongs[length(allLongs)]
#}
#
# This is fake data for testing
#obs_lons <- runif(min=95, max=115, 100)
#obs_lats <- runif(min=30, max=50, 100)
# TODO Read in real data, make sure the two vectors are "paired", same length and identities
