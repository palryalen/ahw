predict.aareg <-
function(fit, newData, idName, type=c('cumHaz','dCumHaz'), rightObsName=NULL ) {
  #Predicts hazard based on Aalen's additive model.
  type <- match.arg(type)
  ut <- unique(fit$times) 
  if (ut[1] < 0)
    stop('Negative times are not allowed')
  C <- lfit$call[[2]][[2]] #skal dette være fit - ikke lfit?   
  if(length(C) == 4) {
    intStartName <- as.character(C[[2]])
    intStopName <- as.character(C[[3]])
  } else {
    stop('Not a valid call to Surv.')
  }
  endStatusName <- C[[length(C)]]
  if (!is.name(endStatusName)) {
    endStatusName <- deparse(C[[length(C)]])
    newData[ ,endStatusName]<- eval(C[[length(C)]], envir=newData)
  }
  if (! (endStatusName %in% rightObsName))
    rightObsName <- c(endStatusName,rightObsName)      
  newData <- RefineTimeScale(newData, intStartName, intStopName, ut,
                             rightObsName)
  H <- model.matrix(as.formula(fit$call$formula), data=newData)
  W <- rowsum(diff(rbind(rep(0, ncol(fit$coefficient)), fit$coefficient)), fit$time)  
  J <- match(as.vector(newData[, intStartName]), as.vector(ut))
  dB <- matrix(nrow=nrow(newData), ncol=ncol(W))
  dB[!is.na(J), ] <- W[J[!is.na(J)],]
  dB[is.na(J), ] <- rep(0, ncol(W))
  print(dim(dB))
  print(dim(H))
  dCumHaz <- (dB * H) %*% rep(1, ncol(dB)) 
  dCumHaz[is.na(dCumHaz)] <- 0
  print(cbind(colnames(H), colnames(fit$coefficient))) 
  #TODO: Why are the columns compatible?
  #B <- ave(dB, newData[,idName], FUN=cumsum) 
  #Slow - should be implemented in C
    if (type=='cumHaz') {
      cumHaz <- cumSumGroup(dCumHaz,newData[,idName])
      newData <- cbind(newData, cumHaz)
    } else if (type=='dCumHaz') {
      newData <- cbind(newData, dCumHaz)
    }
   return(newData) 
}
