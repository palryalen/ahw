
makeContWeights <- function(faFit,cfaFit,dataFr,atRiskState,eventState,stopTimeName,startStatusName,endStatusName,idName,b,weightRange = c(0,10),willPlotWeights=T){
        
        if(class(faFit) != "aalen" | class(cfaFit) != "aalen")
          stop("The survival fits must be of type aalen.",call. = F)
        # if(!requireNamespace("data.table",quietly = T))
        #   stop("The data.table package is needed for this function to work. Please install it.",call. = F)
  
        # Making new names for convenience
        namesMatch <- match(c(startStatusName,endStatusName,stopTimeName,idName),names(dataFr))
        saveNames <- names(dataFr)[namesMatch]
        names(dataFr)[namesMatch] <- c("from.state","to.state","to","id")
        
        dataFr <- as.data.table(dataFr)

        # Add noise to tied times
        dataFr <- addNoiseAtEventTimes(dataFr,"id","from","to")
        
        # data frame to get predictions along
        wtFrame <- dataFr[dataFr$from.state %in% atRiskState,]
        
        
        pft <- predict(faFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        cpft <- predict(cfaFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        
        ids <- unique(dataFr[,idName,with=F])
        eventIds <- wtFrame$id[wtFrame$to.state %in% eventState]
        
        # Times we want to estimate the weights at
        eventTimes <- wtFrame$to[wtFrame$to.state %in% eventState]
        sortedEventTimes <- sort(eventTimes)
        
        # Obtain estimated weights
        pft -> fPred; cpft -> cfPred;
        weightFrame <- weightPredict(pft,cpft,wtFrame,ids,eventTimes,eventIds,b)

        # Refining the data frame for individuals at risk
        Table <- refineTableAllTimes(dataFr,atRiskState,eventState)
        
        Table <- merge(Table,weightFrame,by=c("id","from"),all.x=T)
        
        Table[isAtRisk!=1]$to <- baseTable[!(from.state %in% atRiskState)]$to
        
        # Individuals weight constant after time of treatment
        Table[to > eventTime,weights := weights[1],by=id]
        
        
        ##
        # Table[isAtRisk==0, weights:=tail(weights,1) ,by=id]
        ##
        
        ##
        # Table[,tail(to,1):=tail(to,1)+1e-2*runif(1),by=id]
        ##
        
        # idds <- 0
        # idds <- idds+1
        # Table[id==idds & from > eventTimes[idds+1]-1]
        # plot(Table[id==idds]$to,Table[id==idds]$weights,type="l")
        
        Table <- subset(Table,select= !(names(Table) %in% c("rowNumber","numRep","putEventTimes","isAtRisk","u","eventTime")))
        
        Table[,weights:=naReplace(weights),by=id]

        # Truncate weights that are outside a given range
        Table$weights[Table$weights < weightRange[1]] <- weightRange[1]
        Table$weights[Table$weights > weightRange[2]] <- weightRange[2]
        
        # Optional plot of the weight trajectories
        if(willPlotWeights == T)
                plotContWeights(Table)
        
        # Switching names back
        namesMatch <- match(c("from.state","to.state","to","id"),names(Table))
        names(Table)[namesMatch] <- saveNames
        
        return(Table)
        
}
