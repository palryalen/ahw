




# fPred <- pft;cfPred <- cpft;ids <- unique(dataFr[,idName]);eventTimes <- wtFrame$to[wtFrame$to.state %in% eventState];eventIds <- wtFrame$id[wtFrame$to.state %in% eventState]
hazPredict <- function(fPred,cfPred,wtFrame,ids,eventTimes,eventIds,b){
        
        # willPlotWeights <- match.arg(willPlotWeights,choices=willPlotWeights,several.ok = T)
        # Creating data.table with the individual predicted
        # cumulative hazard increments
        fPredTimes <- fPred$time;cfPredTimes <- cfPred$time
        
        totTimes <- sort(unique(c(fPredTimes,cfPredTimes,eventTimes)))
        nTimes <- length(totTimes)
        lagtimes <- totTimes-b;lagtimes[lagtimes<0] <- 0
        lagInds <- sapply(lagtimes,function(tm)max(which(tm >= totTimes)))
        
        sortedEventTimes <- sort(eventTimes)
        
        mtf <- match(fPredTimes,totTimes)
        mtcf <- match(cfPredTimes,totTimes)
        mtf <- rep(mtf)
        
        dA_f <- as.vector(apply(fPred$S0,1,function(rw)-diff(c(0,log(rw)))))
        dA_cf <- as.vector(apply(cfPred$S0,1,function(rw)-diff(c(0,log(rw)))))
        
        
        predTable <- data.table(rowNum=rep(1:nrow(wtFrame),each=nTimes),
                                id=rep(wtFrame$id,each=nTimes),
                                to=rep(totTimes,nrow(wtFrame)),dA_f=0,dA_cf=0,
                                fEvent=rep(1*(1:nTimes %in% mtf),nrow(wtFrame)),
                                cfEvent=rep(1*(1:nTimes %in% mtcf),nrow(wtFrame)),
                                lagInd=rep(lagInds,nrow(wtFrame)))
        
        ##
        # predTable[,fEvent:=1*(1:nTimes %in% mtf),by=id]
        # predTable[,cfEvent:=1*(1:nTimes %in% mtcf),by=id]
        predTable[fEvent==1]$dA_f <- dA_f
        predTable[cfEvent==1]$dA_cf <- dA_cf
        ##
        
        # fInds <- unlist(lapply(0:(nrow(wtFrame)-1),function(i)i*length(totTimes) + mtf))
        # cfInds <- unlist(lapply(0:(nrow(wtFrame)-1),function(i)i*length(totTimes) + mtcf))
        # predTable$dA_f[fInds] <- dA_f
        # predTable$dA_cf[cfInds] <- dA_cf
        
        
        # If there are several at risk states
        numRepId <- as.numeric(table(wtFrame$id))
        
        wtFrame$rowNum <- 1:nrow(wtFrame)
        
        predTable[,numIdRow:= rep(cumsum(numRepId),times=nTimes*numRepId)]
        predTable[,keep:=1*(to>= wtFrame$from[rowNum[1]==wtFrame$rowNum] & 
                                    to< wtFrame$to[rowNum[1]==wtFrame$rowNum]),by=rowNum]
        predTable[keep==0 & numIdRow==rowNum,keep:=1*(to>= max(wtFrame$to[id[1]==wtFrame$id])),by=id]
        predTable[to==0,keep:=1]
        
        predTable <- predTable[keep==1]
        
        predTable[,EventTimes:=0]
        predTable[,event:=0]
        predTable[,EventId:=1*(id%in%eventIds),by=id]
        predTable[EventId==1,EventTimes:=eventTimes[eventIds==id],by=id]
        predTable[EventId==1,event:= 1*(EventTimes == to)]
        predTable[,takeOut:=1*(to%in%eventTimes)]
        predTable[to==0,takeOut:=1]
        
        
        # Evaluating predicted cumulative hazards at (lagged) event times
        predTable <- predTable[,c("A_f","A_cf") := .(cumsum(dA_f),cumsum(dA_cf)),by=id]
        
        
        predTable[,c("A_f_lag","A_cf_lag") := .(A_f[lagInd],A_cf[lagInd]),by=id]
        
        # idds <- idds+1
        # plot(predTable[id==idds]$to,predTable[id==idds]$A_f,type="l")
        # lines(predTable[id==idds]$to,predTable[id==idds]$A_f_lag,col=2)
        
        # Calculating treatment contribution on the weights
        predTable[,jumpTerm:= (A_cf - A_cf_lag)/(A_f - A_f_lag)]
        
        # Convex modification for small times
        predTable[to<b,jumpTerm:=(b-to)/b + (to/b)*jumpTerm]
        
        predTable[,jumpTerm:=jumpTerm - 1]
        predTable[event==0,jumpTerm:=0]
        
        numNaIds <- nrow(predTable[jumpTerm %in% c(NA,NaN),])
        # Hack for dealing with possible "invalid" terms (e.g. 0/0)
        if(numNaIds != 0)
                cat('Warning: b is small for', numNaIds, 'individuals \n')
                
        predTable[jumpTerm %in% c(NA,NaN),jumpTerm:= 0]
        
        # Calculating the weights
        predTable[,preweight := 1 + dA_f - dA_cf + jumpTerm]
        predTable[,weights:=cumprod(preweight),by=id]
        
        predTable <- predTable[takeOut==1,]
        predTable <- predTable[,names(predTable) %in% c("id","to","weights"),with=F]
        
        # weight Plot (error?)
        # if(willPlotWeights == T){
        #         meanWeights <- sapply(sortedEventTimes,function(tm)mean(predTable[to==tm]$weights))
        #         plot(sortedEventTimes,meanWeights,col=2,lwd=2,type="s",xlab="time",ylab="weights")
        #         for(idds in ids[1:20]){
        #                 lines(c(0,sortedEventTimes),predTable[id==idds]$weights,type="l",col="gray",ann=F,bty="n")
        #         }
        # }
        
        # Estimators evaluate weights in the left limit:
        names(predTable)[names(predTable)=="to"] <- "from"
        
        return(predTable)
}


