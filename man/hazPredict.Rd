\name{hazPredict}
\alias{hazPredict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hazPredict(fPred, cfPred, wtFrame, ids, eventTimes, eventIds, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fPred}{
%%     ~~Describe \code{fPred} here~~
}
  \item{cfPred}{
%%     ~~Describe \code{cfPred} here~~
}
  \item{wtFrame}{
%%     ~~Describe \code{wtFrame} here~~
}
  \item{ids}{
%%     ~~Describe \code{ids} here~~
}
  \item{eventTimes}{
%%     ~~Describe \code{eventTimes} here~~
}
  \item{eventIds}{
%%     ~~Describe \code{eventIds} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fPred, cfPred, wtFrame, ids, eventTimes, eventIds, 
    b) 
{
    fPredTimes <- fPred$time
    cfPredTimes <- cfPred$time
    totTimes <- sort(unique(c(fPredTimes, cfPredTimes, eventTimes)))
    nTimes <- length(totTimes)
    lagtimes <- totTimes - b
    lagtimes[lagtimes < 0] <- 0
    lagInds <- sapply(lagtimes, function(tm) max(which(tm >= 
        totTimes)))
    sortedEventTimes <- sort(eventTimes)
    mtf <- match(fPredTimes, totTimes)
    mtcf <- match(cfPredTimes, totTimes)
    mtf <- rep(mtf)
    dA_f <- as.vector(apply(fPred$S0, 1, function(rw) -diff(c(0, 
        log(rw)))))
    dA_cf <- as.vector(apply(cfPred$S0, 1, function(rw) -diff(c(0, 
        log(rw)))))
    predTable <- data.table(rowNum = rep(1:nrow(wtFrame), each = nTimes), 
        id = rep(wtFrame$id, each = nTimes), to = rep(totTimes, 
            nrow(wtFrame)), dA_f = 0, dA_cf = 0, fEvent = rep(1 * 
            (1:nTimes \%in\% mtf), nrow(wtFrame)), cfEvent = rep(1 * 
            (1:nTimes \%in\% mtcf), nrow(wtFrame)), lagInd = rep(lagInds, 
            nrow(wtFrame)))
    predTable[fEvent == 1]$dA_f <- dA_f
    predTable[cfEvent == 1]$dA_cf <- dA_cf
    numRepId <- as.numeric(table(wtFrame$id))
    wtFrame$rowNum <- 1:nrow(wtFrame)
    predTable[, `:=`(numIdRow, rep(cumsum(numRepId), times = nTimes * 
        numRepId))]
    predTable[, `:=`(keep, 1 * (to >= wtFrame$from[rowNum[1] == 
        wtFrame$rowNum] & to < wtFrame$to[rowNum[1] == wtFrame$rowNum])), 
        by = rowNum]
    predTable[keep == 0 & numIdRow == rowNum, `:=`(keep, 1 * 
        (to >= max(wtFrame$to[id[1] == wtFrame$id]))), by = id]
    predTable[to == 0, `:=`(keep, 1)]
    predTable <- predTable[keep == 1]
    predTable[, `:=`(EventTimes, 0)]
    predTable[, `:=`(event, 0)]
    predTable[, `:=`(EventId, 1 * (id \%in\% eventIds)), by = id]
    predTable[EventId == 1, `:=`(EventTimes, eventTimes[eventIds == 
        id]), by = id]
    predTable[EventId == 1, `:=`(event, 1 * (EventTimes == to))]
    predTable[, `:=`(takeOut, 1 * (to \%in\% eventTimes))]
    predTable[to == 0, `:=`(takeOut, 1)]
    predTable <- predTable[, `:=`(c("A_f", "A_cf"), .(cumsum(dA_f), 
        cumsum(dA_cf))), by = id]
    predTable[, `:=`(c("A_f_lag", "A_cf_lag"), .(A_f[lagInd], 
        A_cf[lagInd])), by = id]
    predTable[, `:=`(jumpTerm, (A_cf - A_cf_lag)/(A_f - A_f_lag))]
    predTable[to < b, `:=`(jumpTerm, (b - to)/b + (to/b) * jumpTerm)]
    predTable[, `:=`(jumpTerm, jumpTerm - 1)]
    predTable[event == 0, `:=`(jumpTerm, 0)]
    numNaIds <- nrow(predTable[jumpTerm \%in\% c(NA, NaN), ])
    if (numNaIds != 0) 
        cat("Warning: b is small for", numNaIds, "individuals \n")
    predTable[jumpTerm \%in\% c(NA, NaN), `:=`(jumpTerm, 0)]
    predTable[, `:=`(preweight, 1 + dA_f - dA_cf + jumpTerm)]
    predTable[, `:=`(weights, cumprod(preweight)), by = id]
    predTable <- predTable[takeOut == 1, ]
    predTable <- predTable[, names(predTable) \%in\% c("id", "to", 
        "weights"), with = F]
    names(predTable)[names(predTable) == "to"] <- "from"
    return(predTable)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
