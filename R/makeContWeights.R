
makeContWeights <- function(faFit,cfaFit,dataFr,atRiskState,eventState,startTimeName,stopTimeName,startStatusName,endStatusName,idName,b,weightRange = c(0,10),willPlotWeights=T){
        
        if(class(faFit) != "aalen" | class(cfaFit) != "aalen")
          stop("The survival fits must be of type aalen.",call. = F)
        if(!requireNamespace("data.table",quietly = T))
          stop("The data.table package is needed for this function to work. Please install it.",call. = F)
  
        # Making new names for convenience
        namesMatch <- match(c(startStatusName,endStatusName,startTimeName,stopTimeName,idName),names(dataFr))
        saveNames <- names(dataFr)[namesMatch]
        names(dataFr)[namesMatch] <- c("from.state","to.state","from","to","id")
        
        dataFr <- as.data.table(dataFr)

        # data frame to get predictions along
        wtFrame <- dataFr[dataFr$from.state %in% atRiskState,]
        
        # Times we want to estimate the weights at
        eventTimes <- wtFrame$to[wtFrame$to.state %in% eventState]
        
        # Checking whether there are ties in the input data
        if(length(unique(eventTimes)) != length(eventTimes)){
          stop("Error! Remove ties from the data. Apply function addNoiseAtEventTimes() to dataFr and try again.
               Run example(makeContWeights) for an example.")
        }

        
        
        pft <- predict(faFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        cpft <- predict(cfaFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        
        # ids <- unique(dataFr[,idName,with=F])
        ids <- unique(dataFr$id)
        eventIds <- wtFrame$id[wtFrame$to.state %in% eventState]
        
        
        
        # Obtain estimated weights
        fPred<- pft;cfPred  <- cpft
        weightFrame <- weightPredict(pft,cpft,wtFrame,ids,eventTimes,eventIds,b)

        # Refining the data.frame for individuals at risk
        Table <- refineTable(dataFr,atRiskState,eventState)
        
        # Merge so that Table includes the weight estimates 
        Table <- merge(Table,weightFrame,by=c("id","from"),all.x=T)
        
        # Individuals weight constant after time of treatment
        Table[isAtRiskForTreatment != 1,weights := weights[1],by=id]
        
        
        
        Table <- subset(Table,select= !(names(Table) %in% c("rowNumber","numRep","putEventTimes","isAtRiskForTreatment","eventTime")))
        
        Table[,weights:=naReplace(weights),by=id]

        # Truncate weights that are outside a given range
        Table$weights[Table$weights < weightRange[1]] <- weightRange[1]
        Table$weights[Table$weights > weightRange[2]] <- weightRange[2]
        
        # Optional plot of the weight trajectories
        if(willPlotWeights == T)
                plotContWeights(Table)
        
        # Switching names back
        namesMatch <- match(c("from.state","to.state","from","to","id"),names(Table))
        names(Table)[namesMatch] <- saveNames
        
        return(Table)
        
}
