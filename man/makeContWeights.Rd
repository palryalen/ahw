\name{makeContWeights}
\alias{makeContWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Continuous time weight estimation based on aalen.predict
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Refines data longitudinally in order to do estimate parameters(e.g. Nelson-Aalen or Kaplan-Meier) using continuous time weights. Weight assessment is biult in throug plotting options and removal of extreme(very largs or negative) weights.
}
\usage{
function (faFit, cfaFit, dataFr, atRiskState, eventState, stopTimeName, 
    startStatusName, endStatusName, idName, b, weightRange = c(0, 
        10), willPlotWeights = T) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{faFit}{
The \code{\link[timereg]{aalen}} fit for the factual hazard,
}
  \item{cfaFit}{
The  \code{\link[timereg]{aalen}} fit for the counterfactual hazard,
}
  \item{dataFr}{
\code{\link{data.frame}} or \code{\link{data.table}} on long format,
}
 \item{atRiskState}{
At risk state for the event,
}
\item{eventState}{
State for the event,
}
 \item{stopTimeName}{
Name of column with stop time of the at risk period,
}
\item{startStatusName}{
Name of the variable that contains the name of start state for each at-risk interval,
}
\item{endStatusName}{
Name of the variable that contains the name of end state for each at-risk interval,
}
  \item{idName}{
Name of column in \code{dataFr} that identifies individuals,
}
\item{b}{
Smoothing bandwidth parameter,
}
\item{weightRange}{
Range for allowable weights,
}
\item{willPlotWeights}{
Plot indicator,
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Individuals whos weights lie outside the weightRange are considered outliers and are removed.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Longitudinally refined \code{\link{data.table}} of the initial \code{dataFr} with \code{weights} column added.
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
