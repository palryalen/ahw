\name{refineTable}
\alias{refineTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Expands data.table
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Refines dataFr so that each individual at risk get a row for each of the provided event times
}
\usage{
refineTable(dataFr, atRiskState, eventTimes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataFr}{
%%     ~~Describe \code{dataFr} here~~
data.frame or data.table on long format
}
  \item{atRiskState}{
%%     ~~Describe \code{atRiskState} here~~
At risk state(s) 
}
  \item{eventTimes}{
%%     ~~Describe \code{eventTimes} here~~
Observed event times 
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
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
