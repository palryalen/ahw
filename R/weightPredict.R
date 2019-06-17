

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
        
        predTable[fEvent==1]$dA_f <- dA_f
        predTable[cfEvent==1]$dA_cf <- dA_cf
        
        
        predTable <- getJumpTerm(predTable,eventTimes,sortedEventTimes,totTimes, eventRowNums)
        
        predTable$rowNumFrom <- rep(wtFrame$from,each=length(totTimes))
        predTable$rowNumTo <- rep(wtFrame$to,each=length(totTimes))
        

        
        # Other approach: remove rows using rowNumFrom & rowNumTo
        predTable <- predTable[to >= rowNumFrom & to <= rowNumTo]
        
        predTable[event==0,jumpTerm:=0]
        
        # Checking for "invalid" terms (e.g. 0/0)
        numNaIds <- length(unique(predTable[jumpTerm %in% c(NA,NaN)]$id))
        if(numNaIds != 0)
          cat('Warning: b is small for', numNaIds, 'individuals. Consider increasing b. \n')
                
        # Weight calculation; solving the SDE
        predTable[,preweight := 1 + dA_f - dA_cf + jumpTerm]
        
        predTable[,weights:=cumprod(preweight),by=id]
        
        predTable <- predTable[,names(predTable) %in% c("id","to","weights","rowNum"),with=F]
        
        # Estimators evaluate weights in the left limit:
        names(predTable)[names(predTable)=="to"] <- "from"
        
        return(predTable)
}


