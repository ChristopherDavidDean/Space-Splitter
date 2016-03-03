
# Quick file to convert grid from list to data.frame
load("grid.RData")
grid_frame <- data.frame("id"=NA, "lat"=NA, "lon"=NA)
for(i in 1:nrow(grid)) {
  temp_lats <- unlist(lats[[grid[i, "lat"]]])
  temp_lons <- unlist(lons[[grid[i, "lon"]]])
  temp_gf <- expand.grid(temp_lats, temp_lons)
  temp_gf[ ,'id'] <- i
  names(temp_gf) <- c("lat", 'lon', 'id')
  grid_frame <- rbind(grid_frame, temp_gf)
}
grid_frame <- grid_frame[-1, ]
save('grid_frame', file="grid_frame.RData")