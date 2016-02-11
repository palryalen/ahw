
aalenRes <- function(aFit) {
   #Should check if residuals have been calculated
   #Should also be replaced by an predict.aalen function
   dM <- as.vector(aFit$residuals$dM)
   time <- rep(aFit$residuals$time,ncol(aFit$residuals$dM))
   id <- rep(0:(ncol(aFit$residuals$dM)-1),
             rep(length(aFit$residuals$time), ncol(aFit$residuals$dM))) 
   return(data.frame(dM,time,id))
}


