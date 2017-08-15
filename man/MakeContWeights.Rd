\name{MakeContWeights}
\alias{MakeContWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MakeContWeights(faFit, cfaFit, dataFr, atRiskState, eventState, stopTimeName, startStatusName, endStatusName, idName, b, weightRange = c(0, 10), willPlotWeights = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{faFit}{
%%     ~~Describe \code{faFit} here~~
}
  \item{cfaFit}{
%%     ~~Describe \code{cfaFit} here~~
}
  \item{dataFr}{
%%     ~~Describe \code{dataFr} here~~
}
  \item{atRiskState}{
%%     ~~Describe \code{atRiskState} here~~
}
  \item{eventState}{
%%     ~~Describe \code{eventState} here~~
}
  \item{stopTimeName}{
%%     ~~Describe \code{stopTimeName} here~~
}
  \item{startStatusName}{
%%     ~~Describe \code{startStatusName} here~~
}
  \item{endStatusName}{
%%     ~~Describe \code{endStatusName} here~~
}
  \item{idName}{
%%     ~~Describe \code{idName} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{weightRange}{
%%     ~~Describe \code{weightRange} here~~
}
  \item{willPlotWeights}{
%%     ~~Describe \code{willPlotWeights} here~~
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
function (faFit, cfaFit, dataFr, atRiskState, eventState, stopTimeName, 
    startStatusName, endStatusName, idName, b, weightRange = c(0, 
        10), willPlotWeights = T) 
{
    namesMatch <- match(c(startStatusName, endStatusName, stopTimeName, 
        idName), names(dataFr))
    saveNames <- names(dataFr)[namesMatch]
    names(dataFr)[namesMatch] <- c("from.state", "to.state", 
        "to", "id")
    wtFrame <- dataFr[dataFr$from.state \%in\% atRiskState, ]
    pft <- predict(faFit, newdata = wtFrame, n.sim = 0, se = F, 
        resample.iid = 0)
    cpft <- predict(cfaFit, newdata = wtFrame, n.sim = 0, se = F, 
        resample.iid = 0)
    ids <- unique(dataFr[, idName])
    eventIds <- wtFrame$id[wtFrame$to.state \%in\% eventState]
    eventTimes <- wtFrame$to[wtFrame$to.state \%in\% eventState]
    sortedEventTimes <- sort(eventTimes)
    hazFrame <- hazPredict(pft, cpft, wtFrame, ids, eventTimes, 
        eventIds, b)
    Table <- refineTable(dataFr, atRiskState, eventTimes)
    Table <- merge(Table, hazFrame, by = c("id", "from"), all.x = T)
    Table <- subset(Table, select = !(names(Table) \%in\% c("rowNumber", 
        "numRep", "putEventTimes", "isAtRisk")))
    removeIds <- unique(Table$id[Table$weights < weightRange[1] | 
        Table$weights > weightRange[2]])
    Table <- Table[!(Table$id \%in\% removeIds), ]
    if (length(removeIds) > 0 & length(removeIds) < 20) 
        cat("Removed ", idName, "'s: ", sep = "")
    cat(removeIds, "\n")
    if (length(removeIds) >= 20) 
        cat("Removed ", length(removeIds), " ", idName, "'s", 
            "\n", sep = "")
    if (willPlotWeights == T) 
        plotContWeights(Table)
    namesMatch <- match(c("from.state", "to.state", "to", "id"), 
        names(Table))
    names(Table)[namesMatch] <- saveNames
    return(Table)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
