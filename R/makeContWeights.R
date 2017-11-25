
makeContWeights <- function(faFit,cfaFit,dataFr,atRiskState,eventState,stopTimeName,startStatusName,endStatusName,idName,b,weightRange = c(0,10),willPlotWeights=T){
        
        if(class(faFit) != "aalen" | class(cfaFit) != "aalen")
          stop("The survival fits must be of type aalen.",call. = F)
        # if(!requireNamespace("data.table",quietly = T))
        #   stop("The data.table package is needed for this function to work. Please install it.",call. = F)
  
        # Making new names for convenience
        namesMatch <- match(c(startStatusName,endStatusName,stopTimeName,idName),names(dataFr))
        saveNames <- names(dataFr)[namesMatch]
        names(dataFr)[namesMatch] <- c("from.state","to.state","to","id")
        
        # Add noise to tied times
        dataFr <- addNoiseAtEventTimes(dataFr,"id","from","to")
        
        # data frame to get predictions along
        wtFrame <- dataFr[dataFr$from.state %in% atRiskState,]
        
        
        pft <- predict(faFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        cpft <- predict(cfaFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        
        ids <- unique(dataFr[,idName])
        eventIds <- wtFrame$id[wtFrame$to.state %in% eventState]
        
        # Times we want to estimate the weights at
        eventTimes <- wtFrame$to[wtFrame$to.state %in% eventState]
        sortedEventTimes <- sort(eventTimes)
        
        # Obtain estimated weights
        weightFrame <- weightPredict(pft,cpft,wtFrame,ids,eventTimes,eventIds,b)
        
        # Refining the data frame for individuals at risk
        Table <- refineTable(dataFr,atRiskState,eventTimes)
        
        Table <- merge(Table,weightFrame,by=c("id","from"),all.x=T)
        
        Table <- subset(Table,select= !(names(Table) %in% c("rowNumber","numRep","putEventTimes","isAtRisk")))
        
        
        # Truncate weights that are outside a given range
        removeIds <- unique(Table$id[ Table$weights < weightRange[1] | Table$weights > weightRange[2]])
        Table <- Table[!(Table$id %in% removeIds),]
        
        
        if(length(removeIds) > 0 & length(removeIds) < 20)
                cat('Removed ',idName,"'s: ",sep="");cat(removeIds,"\n")
        if(length(removeIds) >= 20)
                cat('Removed ',length(removeIds)," ",idName,"'s","\n",sep="")
        
        # Optional plot of the weights
        if(willPlotWeights == T)
                plotContWeights(Table)
        
        # Switching names back
        namesMatch <- match(c("from.state","to.state","to","id"),names(Table))
        names(Table)[namesMatch] <- saveNames
        
        return(Table)
        
}
