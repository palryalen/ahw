

# Remove ties by adding noise
addNoiseAtEventTimes <- function(frame,idName,startTimeName,stopTimeName){
  ids <- unique(frame[,idName][frame[,startTimeName] >=  frame[,stopTimeName] |
                                 duplicated(frame[,stopTimeName]) ])
  
  eps <- min(abs(diff( c(frame[,stopTimeName],frame[,startTimeName] ))))
  noiseapply <- function(idds){
    subfr <- frame[frame[,idName] == idds,]
    allTimes <- subfr[,stopTimeName]
    allTimes <- allTimes + 1e-2*eps*runif(length(allTimes))
    
    subfr[,startTimeName] <- c(subfr[1,startTimeName],allTimes[-length(allTimes)])
    subfr[,stopTimeName] <- allTimes
    return(subfr)
  }
  
  noiseFrame <- lapply(ids,noiseapply)
  noiseFrame <- do.call(rbind,noiseFrame)
  frame[frame$id %in% ids,] <- noiseFrame
  return(frame)
}
