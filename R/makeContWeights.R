
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
        wtFrame$rowNum <- 1:nrow(wtFrame)
        
        
        pft <- predict(faFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        cpft <- predict(cfaFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        
        ids <- unique(dataFr[,idName,with=F])

        eventRowNums <- wtFrame$rowNum[wtFrame$to.state %in% eventState]
        
        # Times we want to estimate the weights at
        eventTimes <- wtFrame$to[wtFrame$to.state %in% eventState]
        sortedEventTimes <- sort(eventTimes)
        
        # Obtain estimated weights
        fPred<- pft;cfPred  <- cpft
        weightFrame <- weightPredict(pft,cpft,wtFrame,ids,eventTimes,eventRowNums,b)

        # Refining the data.table for individuals at risk
        Table <- refineTable(dataFr,atRiskState,eventState)
        
        # Merge so that Table includes the weight estimates 
        Table <- merge(Table,weightFrame,by=c("id","from"),all.x=T)

        # Individuals weight constant when subjects are not at risk for treatment
        Table[isAtRiskForTreatment != 1,weights := weights[1],by=rowNum]
        
        Table <- subset(Table,select= !(names(Table) %in% c("rowNum","isAtRiskForTreatment")))
        
        # The time-dependent weights have initial value 1
        Table[,weights := c(1,weights[-1]),by=id]
        Table[,weights:=naReplace(weights),by=id]

        # Truncate weights that are outside the given range weightRange
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
