
# Assumes fr is data.table
addNoiseAtEventTimes <- function(fr,idName,startTimeName,stopTimeName){
        
        fr$noise <- 1e-4*runif(nrow(fr))
        fr[,noise:=cumsum(noise),by=id]
        fr$noiseFrom <- fr[,startTimeName,with=F]
        fr$noiseTo <- fr[,stopTimeName,with=F]
        fr[,noiseFrom:=noiseFrom + c(0,noise[-length(noise)]),by=id]
        fr[,noiseTo := noiseTo + noise,by=id]
        fr[,startTimeName] <- fr$noiseFrom
        fr[,stopTimeName] <- fr$noiseTo
        
        fr <- subset(fr,select = !(names(fr) %in% c("noise","noiseFrom","noiseTo") ))
        
        
        return(fr)

}
