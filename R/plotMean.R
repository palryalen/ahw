
plotMean <- function(weights, startTime, stopTime, pTimes) 
{  
   n <- length(pTimes)
   y <- rep(0,n)
   for (j in 1:n)
   {
      J <- (startTime <= pTimes[j]) & (stopTime >= pTimes[j])
      y[j] <- mean(weights[J],na.rm=T)
   }
   lines(pTimes,y,col='red')
}


