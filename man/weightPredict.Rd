\name{weightPredict}
\alias{weightPredict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Continuous time weight estimation based on \code{\link{predict.aalen}}.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Extracts cumulative hazard estimates for each individual. Each individual receives a weight process evaluated at pre-specified time points. The weight process is estimated as a cumulative product involving estimated cumulative hazard increments, and a hazard ratio estimated using a smoothing parameter \code{b}.
}
\usage{
weightPredict(fPred, cfPred, wtFrame, ids, eventTimes, eventIds, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
fPred, cfPred, wtFrame, ids, eventTimes, eventIds, b)
  \item{fPred}{
%%     ~~Describe \code{dataFr} here~~
\code{\link{predict.aalen}} object of the factual fit
}
\item{cfPred}{
%%     ~~Describe \code{dataFr} here~~
\code{\link{predict.aalen}} object of the counterfactual fit
}
\item{wtFrame}{
%%     ~~Describe \code{dataFr} here~~
\code{\link{data.frame}} or \code{\link{data.table}} for the at risk individuals
}
\item{ids}{
%%     ~~Describe \code{dataFr} here~~
All individuals in the data set
}
  \item{eventTimes}{
%%     ~~Describe \code{eventTimes} here~~
Observed event times 
}
\item{eventIds}{
%%     ~~Describe \code{dataFr} here~~
Individuals that experience the event
}
\item{b}{
%%     ~~Describe \code{dataFr} here~~
Smoothing parameter
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
data.table
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Pål Christie Ryalen <p.c.ryalen@medisin.uio.no>
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
function (dataFr, atRiskState, eventTimes) 
{
    sortedEventTimes <- sort(eventTimes)
    baseTable <- as.data.table(dataFr)
    baseTable$rowNumber <- 1:nrow(baseTable)
    baseTable[, `:=`(isAtRisk, 1 * (from.state \%in\% atRiskState))]
    baseTable[, `:=`(numRep, 0)]
    baseTable[isAtRisk == 1, `:=`(numRep, as.numeric(sum(from <= 
        eventTimes & to > eventTimes))), by = rowNumber]
    baseTable[, `:=`(numRep, numRep + 1)]
    baseTable[, `:=`(to.state, as.character(to.state))]
    Table <- baseTable[rep(1:nrow(baseTable), times = baseTable[, 
        numRep]), ]
    Table[, `:=`(putEventTimes, 0)]
    Table[numRep > 1, `:=`(putEventTimes, c(sortedEventTimes[sortedEventTimes >= 
        from[1] & sortedEventTimes < to[1]], to[1])), by = id]
    Table[numRep == 1, `:=`(putEventTimes, to)]
    Table[, `:=`(to, putEventTimes)]
    Table[, `:=`(from, c(from[1], to[-length(to)])), by = id]
    Table[, `:=`(to.state, c(rep(0, length(to) - 1), baseTable$to.state[rowNumber[1]])), 
        by = rowNumber]
    return(Table)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
