# eventState <- 3;atRiskState <- 1;dataFr <- fFrame

# faFit <- fFitRP;cfaFit <- cfFitRP;dataFr <- RPFr; eventState <- "RP";atRiskState<-"diagnose";endStatusName <- "to.state"
# faFit <- fFitRad;cfaFit <- cfFitRP;dataFr <- straaleFr; eventState <- "straaling";atRiskState<-"diagnose";endStatusName <- "to.state"


# dataFr <- fFrame;faFit <- aaft1;cfaFit <- aaft2;atRiskState<-0;eventState<-1;stopTimeName<-"to";endStatusName <-"to.state";startTimeName <- "from";startStatusName <-"from.state";idName<-"id";b<-230;
MakeContWeights <- function(faFit,cfaFit,dataFr,atRiskState,eventState,stopTimeName,startStatusName,endStatusName,idName,b,weightCutoff = c(0,10),willPlotWeights=T){
        
        namesMatch <- match(c(startStatusName,endStatusName,stopTimeName,idName),names(dataFr))
        saveNames <- names(dataFr)[namesMatch]
        names(dataFr)[namesMatch] <- c("from.state","to.state","to","id")
        wtFrame <- dataFr[dataFr$from.state%in%atRiskState,]
        
        eventTimes <- wtFrame$to[wtFrame$to.state %in% eventState]
        pft <- predict(faFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        cpft <- predict(cfaFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        
        ids <- unique(dataFr[,idName])
        eventIds <- wtFrame$id[wtFrame$to.state %in% eventState]
        sortedEventTimes <- sort(eventTimes)
        
        hazFrame <- hazPredict(pft,cpft,wtFrame,ids,eventTimes,eventIds,b)
        
        Table <- refineTable(dataFr,atRiskState,eventTimes)
        
        Table <- merge(Table,hazFrame,by=c("id","from"),all.x=T)
        
        # Possible weight hacks
        # Frame[,weights:=na.locf0(weights),by=id]
        # Frame$weights[is.na(Frame$weights)] <- 1
        # Frame[,weights:=c(1,weights[-length(weights)]),by=id]
        
        Table <- subset(Table,select= !(names(Table) %in% c("rowNumber","numRep","putEventTimes","isAtRisk")))
        
        # idds <- idds+1
        # plot(frame$to[frame$id==idds],frame$weights[frame$id==idds],type="l",xlim=c(0,500))
        # lines(Frame$to[Frame$id==idds],Frame$weights[Frame$id==idds],col=2)
        removeIds <- unique(Table$id[ Table$weights < weightCutoff[1] | Table$weights > weightCutoff[2]])
        Table <- Table[!(Table$id %in% removeIds),]
        if(length(removeIds) > 0 & length(removeIds) < 20)
                cat('Removed ',idName,"'s: ",sep="");cat(removeIds,"\n")
        if(length(removeIds) >= 20)
                cat('Removed ',length(removeIds)," ",idName,"'s","\n",sep="")
        
        if(willPlotWeights == T)
                plotContWeights(Table)
        
        namesMatch <- match(c("from.state","to.state","to","id"),names(Table))
        names(Table)[namesMatch] <- saveNames
        
        return(Table)
        
}
