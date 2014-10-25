#### intensity function


est_intensity <- function(locs, radius){
  intensity <- rep(0, nrow(locs))
  for(i in 1:nrow(locs)){
    loc <- locs[i, c('locx', 'locy')]
    in_x <- (locs$locx < loc$locx + radius & locs$locx > loc$locx - radius)
    in_y <- (locs$locy < loc$locy + radius & locs$locy > loc$locy - radius)
    intensity[i] <- sum(in_x*in_y)
  }
  locs$intensity <- intensity
  return(locs)
}