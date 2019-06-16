
##
Table <- predTable
##

getJumpTerm <- function(Table, eventTimes, sortedEventTimes, totTimes, eventRowNums){
  
  WhichEvents <- sapply(eventTimes,function(tm)which(tm==totTimes)) + length(totTimes)*(0:(length(eventTimes)-1))
  
  # EventRowNum: does the row have an event?
  Table[,EventRowNum:=1*(rowNum%in%eventRowNums),by=id]
  
  # event: if the row has an event, at what event time point?
  Table[,event := 0]
  Table[EventRowNum==1]$event[WhichEvents] <- 1
  # Table[EventRowNum==1,eventTimePos := cumsum(eventTimePos)]
  # Table[,eventTimePos := eventTimePos + 1]
  
  #
  # Table[EventRowNum==1 & eventTimePos == 0,eventTimePos := 1]
  #
  
  # Table[EventId==1 & eventTimePos == 0,eventTimePos := 1]
  
  # event: 
  # Table[,event:=0]
  # Table[eventTimePos==1,event:=1]
  # 
  # Table[,nextEventTime := -1]
  # Table[EventRowNum==1,nextEventTime := eventTimes[eventTimePos]]
  # Table[EventRowNum==1,event:=1*(to == nextEventTime)]
  
  # Table[fEvent==1]$dA_f <- dA_f
  # Table[cfEvent==1]$dA_cf <- dA_cf
  
  
  
  # Table[EventRowNum==1, keep := 1*(to <= nextEventTime),by=rowNum]
  
  
  Table[,c("A_f","A_cf") := .(cumsum(dA_f),cumsum(dA_cf)),by=id]
  Table[,c("A_f_lag","A_cf_lag") := .(A_f[lagInd],A_cf[lagInd]),by=id]
  Table[,c("A_f_lag2","A_cf_lag2") := .(A_f[lagInd2],A_cf[lagInd2]),by=id]
  Table[,c("A_f_lag3","A_cf_lag3") := .(A_f[lagInd3],A_cf[lagInd3]),by=id]
  
  # Calculating jump term - contribution from the treatment
  # Using difference method based on 0, 1*b, 2*b, and 3*b steps back in time:
  Table[,jumpTerm:= (11/2*A_cf - 3*A_cf_lag - 3/2*A_cf_lag2 - A_cf_lag3)/(11/2*A_f - 3*A_f_lag - 3/2*A_f_lag2 - A_f_lag3)]
  
  # Convex modification for small times
  # Table[to<3*b,jumpTerm:=(3*b-to)/(3*b) + (to/(3*b))*jumpTerm]
  
  # Modification for small times
  Table[to<3*b,jumpTerm:=1 + to^2* sqrt(abs(jumpTerm-1)/(9*b))]
  
  
  Table[,jumpTerm:=jumpTerm - 1]
  
  
  return(Table)
}