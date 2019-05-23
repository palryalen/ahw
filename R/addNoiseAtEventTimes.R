
# Assumes fr is data.table
addNoiseAtEventTimes <- function(fr){
        

        fr$noise <- 1e-4*runif(nrow(fr))
        fr[,noise:=cumsum(noise),by="id"]
        fr$noiseFrom <- fr[,"from",with=F]
        fr$noiseTo <- fr[,"to",with=F]
        fr[,noiseFrom:=noiseFrom + c(0,noise[-length(noise)]),by="id"]
        fr[,noiseTo := noiseTo + noise,by="id"]
        fr[,"from"] <- fr$noiseFrom
        fr[,"to"] <- fr$noiseTo
        
        fr <- subset(fr,select = !(names(fr) %in% c("noise","noiseFrom","noiseTo") ))
        
        
        return(fr)

}
