
# Refining dataFr to include all the event times for the individuals at risk. Converts to data.table.
refineTableAllTimes <- function(dataFr,atRiskState,eventState){
        #sortedEventTimes <- sort(eventTimes)
        baseTable <- as.data.table(dataFr)
        
        # Refine at all times after being at risk
        ##
        fineRefineTimes <- c(0,baseTable$to[baseTable$from.state %in% c(atRiskState,eventState)])
        ##
        
        baseTable$rowNumber <- 1:nrow(baseTable)
        
        ##
        # baseTable[,isAtRisk:=1*(from.state %in% atRiskState)]
        ##
        
        baseTable[,isAtRisk:=1*(from.state %in% c(atRiskState,eventState))]
        
        
        baseTable[,numRep := 0]
        
        ##
        # baseTable[isAtRisk==1,numRep := as.numeric(sum(from <= eventTimes & to > eventTimes)),by=rowNumber]
        ##
        
        baseTable[isAtRisk==1,numRep := as.numeric(sum(from <= fineRefineTimes & to > fineRefineTimes)),by=rowNumber]
        
        baseTable[,eventTime := to[from.state %in%atRiskState & to.state %in% eventState],by=id]
        
        # To be improved.?
        baseTable[,to.state:=as.character(to.state)]
        
        ##
        sortedFineRefineTimes <- sort(fineRefineTimes)
        # baseTable[from.state==atRiskState,numRep :=numRep+1]
        ##
        
        
        # baseTable[,numRep :=numRep+1]
        # baseTable[from.state == atRiskState,numRep :=numRep+1]
        # baseTable[!(from.state == atRiskState),numRep :=numRep-1]
        
        Table <- baseTable[rep(1:nrow(baseTable),times=baseTable[,numRep]),]
        
        # Insert event times where the frame is refined
        # Table[,putEventTimes := 0]
        # Table[numRep>1,putEventTimes := c(sortedFineRefineTimes[sortedFineRefineTimes>=from[1] & sortedFineRefineTimes<to[1]],to[1]),by=id]
        # Table[numRep==1,putEventTimes := to]
        
        ## NB! Check the following line: Is the putEventTimes correct?
        Table[,putEventTimes := c(sortedFineRefineTimes[sortedFineRefineTimes>from[1] & sortedFineRefineTimes<to[1]],to[1]),by=rowNumber]
        # Table[from.state %in% atRiskState,putEventTimes := c(sortedFineRefineTimes[sortedFineRefineTimes>from[1] & sortedFineRefineTimes<to[1]],to[1]),by=rowNumber]
        # Table[!(from.state %in% atRiskState),putEventTimes := c(sortedFineRefineTimes[sortedFineRefineTimes>from[1] & sortedFineRefineTimes<to[1]],to[1]),by=rowNumber]
        # Table[from.state==atRiskState,putEventTimes := c(sortedFineRefineTimes[sortedFineRefineTimes>=from[1] & sortedFineRefineTimes<to[1]],to[1]),by=rowNumber]
        
        ##
        # from <- Table[rowNumber==2]$from[1]
        # to <- Table[rowNumber==2]$to[1]
        ##
        
        # Table[,tmpCol:= 1*(from.state=="diagnose")]
        # plot(Table[id==0][,c("to","tmpCol")]);abline(v = to[1],col="red")
        
        ##
        
        Table[,to := putEventTimes]
        Table[,from := c(from[1],to[-length(to)]),by=id]
        
        Table[,isAtRisk := 1*(from.state %in% atRiskState)]
        
        # Table[,eventTime := to[from.state%in%atRiskState & to.state %in% eventState],by=id]
        
        Table[,to.state := c(rep(0,length(to)-1),baseTable$to.state[rowNumber[1]]),by=rowNumber]
        return(Table)
}
