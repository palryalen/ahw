# Refining dataFr to include all the event times for the individuals at risk. Converts to data.table.
refineTable <- function(dataFr,atRiskState,eventState){
  baseTable <- as.data.table(dataFr)
  
  # Refine at all event times in the population when at risk
  fineRefineTimes <- c(0,baseTable$to[baseTable$from.state %in% c(atRiskState)])
  
  # Defining rownumber
  baseTable$rowNumber <- 1:nrow(baseTable)
  
  # Defining indicators for being at risk for treatment
  baseTable[,isAtRiskForTreatment:=1*(from.state %in% c(atRiskState))]
  
  # numRep is the number of times a given row should be replicated
  baseTable[,numRep := 1]
  
  # When the subjects are at risk of being treated, they should have one row for treatment time in the population:
  baseTable[isAtRiskForTreatment==1,numRep := as.numeric(sum(from < fineRefineTimes & to >= fineRefineTimes)),by=rowNumber]
  
  # to.state hack (CHECK IF IT CAN BE IMPROVED)
  baseTable[,to.state:=as.character(to.state)]
  
  sortedFineRefineTimes <- sort(fineRefineTimes)
  
  # Expanding the data.table
  Table <- baseTable[rep(1:nrow(baseTable),times=baseTable[,numRep]),]

  # Prepearing the times when the subject is at risk
  Table[isAtRiskForTreatment==1,putTreatmentTimes := c(sortedFineRefineTimes[sortedFineRefineTimes>from[1] & sortedFineRefineTimes<to[1]],to[1]),by=rowNumber]
  
  # Setting the new event times
  Table[,to := putTreatmentTimes]
  Table[,from := c(from[1],to[-length(to)]),by=rowNumber]
  
  # Setting to.state = 0 as long as subjects don't have events
  Table[isAtRiskForTreatment==1,to.state := c(rep(0,length(to)-1),baseTable$to.state[rowNumber[1]]),by=rowNumber]
  
  # The long format is the same when subjects are not at risk for being treated
  Table[isAtRiskForTreatment!=1]$to <- baseTable[isAtRiskForTreatment!=1]$to
  Table[isAtRiskForTreatment!=1]$from <- baseTable[isAtRiskForTreatment!=1]$from
  
  # Removing redundant columns
  Table <- subset(Table,select = !(names(Table) %in% c("numRep","rowNumber","putTreatmentTimes")))
  return(Table)
}
