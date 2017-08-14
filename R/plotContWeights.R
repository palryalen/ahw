
# Plotting all the individual weight trajectories in fr, along with the mean
plotContWeights <- function(fr,stopTimeName="to",startStatusName="from.state",endStatusName="to.state",idName="id"){
        
        namesMatch <- match(c(startStatusName,endStatusName,stopTimeName,idName),names(fr))
        names(fr)[namesMatch] <- c("from.state","to.state","to","id")
        fr <- as.data.table(fr)
        tms <- sort(unique(fr$to))
        tmx <- max(tms)

        idNs <- unique(fr$id)
        numIds <- length(idNs)
        fr[,weights:=c(weights[-1],weights[length(weights)]),by=id]
        wtIn <- fr$weights[fr$id==idNs[1]]
        wt <- rep(NA,length(tms));wt[1]<-1
        wt[match(fr[id==idNs[1]]$to,tms)] <- wtIn
        
        wt <- naReplace(wt)
        # wt <- na.locf0(wt)
        wpr <- wt
        
        ylm <- c(max(c(0, min(fr$weights))), min(c(5, max(fr$weights))))
        plot(tms,wt,type="l",xlim=c(0,tmx),ylim=ylm,col="grey",xlab="time",ylab="weights")
        for(i in 2:numIds){

                wtIn <- fr$weights[fr$id==idNs[i]]
                wt <- rep(NA,length(tms));wt[1]<-1
                wt[match(fr[id==idNs[i]]$to,tms)] <- wtIn
                
                wt <- naReplace(wt)
                # wt <- na.locf0(wt)
                lines(tms,wt,col="grey")
                if(any(is.na(wt)))
                        break
                wpr <- wpr + wt
        }
        wpr <- wpr/numIds
        lines(tms,wpr,col="red")
        cat('Mean weights: ',mean(wpr))
}
