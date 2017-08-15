
# Refining dataFr to include all the event times for the individuals at risk. Converts to data.table.
refineTable <- function(dataFr,atRiskState,eventTimes){
        sortedEventTimes <- sort(eventTimes)
        baseTable <- as.data.table(dataFr)
        
        baseTable$rowNumber <- 1:nrow(baseTable)
        baseTable[,isAtRisk:=1*(from.state %in% atRiskState)]
        
        baseTable[,numRep := 0]
        baseTable[isAtRisk==1,numRep := as.numeric(sum(from <= eventTimes & to > eventTimes)),by=rowNumber]
        
        baseTable[,numRep :=numRep+1]
        
        # To be improved.?
        baseTable[,to.state:=as.character(to.state)]
        
        Table <- baseTable[rep(1:nrow(baseTable),times=baseTable[,numRep]),]
        
        # Insert event times where the frame is refined
        Table[,putEventTimes := 0]
        Table[numRep>1,putEventTimes := c(sortedEventTimes[sortedEventTimes>=from[1] & sortedEventTimes<to[1]],to[1]),by=id]
        Table[numRep==1,putEventTimes := to]
        
        Table[,to := putEventTimes]
        Table[,from := c(from[1],to[-length(to)]),by=id]
        
        Table[,to.state := c(rep(0,length(to)-1),baseTable$to.state[rowNumber[1]]),by=rowNumber]
        return(Table)
}