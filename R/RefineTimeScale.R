RefineTimeScale <-
function(A, startTimeName, stopTimeName,
                            intTimes, endStatusName, intStatusName=NULL) {
  # Refines each interval between startTime and stopTime. 
  N <- nrow(A)
  q <- N*(length(intTimes) + 1)
  r <- .C('intersect', as.double(A[,startTimeName]), as.double(A[,stopTimeName]),
          as.integer(N), as.double(intTimes), as.integer(length(intTimes)) , 
          newIntStart=double(q), newIntStop=double(q), newOLine=integer(q),
          newNRows=integer(1))
  W <- data.frame(r$newOLine, r$newIntStart, r$newIntStop)
  if (r$newNRows < q)
    W <- W[-((r$newNRows+1):q), ]
  names(W) <- c('imputation', startTimeName, stopTimeName)
  A <- A[, !(names(A) %in% c(startTimeName, stopTimeName))]
  A <- cbind(A, imputation=1:N)
  A <- merge(W, A, by.x='imputation', by.y='imputation', all.x=T,  sort=FALSE)
  A <- A[order(A$imputation,A[,startTimeName]),] 
  J <- duplicated(A$imputation, fromLast=TRUE)

  if (is.null(intStatusName)) {
    A[J, names(A) %in% as.character(endStatusName)] <- FALSE
  } else {
    if (length(intStatusName) != length(endStatusName))
      stop('Right-observed columns must be equipped with interval 
           observations. The value FALSE is imputed if 
           interval-observations are unspecified.')
    intStatus <- A[,intStatusName]
    A[J, names(A) %in% 
      as.character(endStatusName)] <- intStatus[A$imputation,][J,]
  }

  #skal egentlig imputere 
  A <- subset(A, select=-imputation)
  return(A) 
}

