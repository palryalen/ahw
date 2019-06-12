

# Creating data.table with the individual predicted cumulative hazard increments.
# Estimates weights at the provided event times.
weightPredict <- function(fPred,cfPred,wtFrame,ids,eventTimes,eventRowNums,b){
        
        # willPlotWeights <- match.arg(willPlotWeights,choices=willPlotWeights,several.ok = T)
        
        fPredTimes <- fPred$time;cfPredTimes <- cfPred$time
        
        totTimes <- sort(unique(c(fPredTimes,cfPredTimes,eventTimes)))
        nTimes <- length(totTimes)
        lagtimes <- totTimes-b;lagtimes[lagtimes<0] <- 0
        lagtimes2 <- totTimes-2*b;lagtimes2[lagtimes2<0] <- 0
        lagtimes3 <- totTimes-3*b;lagtimes3[lagtimes3<0] <- 0
        lagInds <- sapply(lagtimes,function(tm)max(which(tm >= totTimes)))
        lagInds2 <- sapply(lagtimes2,function(tm)max(which(tm >= totTimes)))
        lagInds3 <- sapply(lagtimes3,function(tm)max(which(tm >= totTimes)))
        
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
                                lagInd=rep(lagInds,nrow(wtFrame)),
                                lagInd2=rep(lagInds2,nrow(wtFrame)),
                                lagInd3=rep(lagInds3,nrow(wtFrame)))
        
        
        
        predTable <- getJumpTerm(predTable,eventTimes,sortedEventTimes, totTimes, eventRowNums)
        
        
        predTable$atRiskFrom <- rep(wtFrame$from,each=nTimes)
        predTable$atRiskTo <- rep(wtFrame$to,each=nTimes)
        
        predTable[EventRowNum==0,keep := 1*(to >= atRiskFrom & to < atRiskTo)]
        
        predTable <- predTable[keep == 1]
        
        # If there are several at risk states for each individual
        # numRepId <- as.numeric(table(wtFrame$id))
        
        
        
        
        # to <- predTable[id==idds]$to
        # predTable[,numIdRow:= rep(cumsum(numRepId),times=nTimes*numRepId)]
        # predTable[,keep:=1*(to>= wtFrame$from[rowNum[1]==wtFrame$rowNum] & 
        #                             to< wtFrame$to[rowNum[1]==wtFrame$rowNum]),by=rowNum]
        # predTable[keep==0 & numIdRow==rowNum,keep:=1*(to>= max(wtFrame$to[id[1]==wtFrame$id])),by=id]
        # 
        # # NB! Not keep every row that starts at to=0!
        # # predTable[to==0,keep:=1]
        # predTable[to==0 & numIdRow != rowNum,keep:=1]
        # 
        # 
        # predTable <- predTable[keep==1]
        # 
        # predTable[,EventTimes:=0]
        # predTable[,event:=0]
        # predTable[,EventId:=1*(id%in%eventIds),by=id]
        # 
        # # predTable[EventId==1,EventTimes:=eventTimes[eventIds==id],by=id]
        # predTable[EventId==1]$EventTimes <- rep(eventTimes)
        # 
        # predTable[EventId==1,event:= 1*(EventTimes == to)]
        # predTable[,takeOut:=1*(to%in%eventTimes)]
        # predTable[to==0,takeOut:=1]
        
        # Evaluating predicted cumulative hazards at (lagged) event times
        
        predTable[event==0,jumpTerm:=0]
                
        # Weight calculation; solving the SDE
        predTable[,preweight := 1 + dA_f - dA_cf + jumpTerm]
        
        predTable[,weights:=cumprod(preweight),by=id]
        
        # predTable <- predTable[takeOut==1,]
        predTable <- predTable[,names(predTable) %in% c("id","to","weights"),with=F]
        
        # Estimators evaluate weights in the left limit:
        names(predTable)[names(predTable)=="to"] <- "from"
        
        return(predTable)
}


