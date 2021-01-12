
# Assumes fr is data.table on long format with "id", "from", and "to" in the column names.
addNoiseAtEventTimes <- function(fr){
        
        if(!all(c("id","from","to") %in% names(fr)) | !("data.table" %in% class(fr))){
                stop("Error! Try again with input data as data.table on long format 
                     with 'id', 'from', and  'to' in the column names.")
        }
                
        fr[,incrementTimes:=diff(c(0,to)),by="id"]
        fr[,nrws:=.N,by="id"]
        maxRows = max(fr$nrws)
        minIncrement = min(fr$incrementTimes)
        
        
        fr$noise <- runif(nrow(fr)) * minIncrement/(maxRows * 2)
        fr[,noise:=cumsum(noise),by="id"]
        fr$noiseFrom <- fr$from
        fr$noiseTo <- fr$to
        fr[,noiseFrom:=noiseFrom + c(0,noise[-length(noise)]),by="id"]
        fr[,noiseTo := noiseTo + noise,by="id"]
        fr$from <- fr$noiseFrom
        fr$to <- fr$noiseTo
        
        fr <- subset(fr,select = !(names(fr) %in% c("noise","noiseFrom","noiseTo","incrementTimes","nrws") ))
        
        
        return(fr)

}
