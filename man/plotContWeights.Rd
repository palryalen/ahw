\name{plotContWeights}
\alias{plotContWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotContWeights(fr, stopTimeName = "to", startStatusName = "from.state", endStatusName = "to.state", idName = "id")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fr}{
%%     ~~Describe \code{fr} here~~
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
function (fr, stopTimeName = "to", startStatusName = "from.state", 
    endStatusName = "to.state", idName = "id") 
{
    namesMatch <- match(c(startStatusName, endStatusName, stopTimeName, 
        idName), names(fr))
    names(fr)[namesMatch] <- c("from.state", "to.state", "to", 
        "id")
    fr <- as.data.table(fr)
    tms <- sort(unique(fr$to))
    tmx <- max(tms)
    idNs <- unique(fr$id)
    numIds <- length(idNs)
    fr[, `:=`(weights, c(weights[-1], weights[length(weights)])), 
        by = id]
    wtIn <- fr$weights[fr$id == idNs[1]]
    wt <- rep(NA, length(tms))
    wt[1] <- 1
    wt[match(fr[id == idNs[1]]$to, tms)] <- wtIn
    wt <- naReplace(wt)
    wpr <- wt
    ylm <- c(max(c(0, min(fr$weights))), min(c(5, max(fr$weights))))
    plot(tms, wt, type = "l", xlim = c(0, tmx), ylim = ylm, col = "grey", 
        xlab = "time", ylab = "weights")
    for (i in 2:numIds) {
        wtIn <- fr$weights[fr$id == idNs[i]]
        wt <- rep(NA, length(tms))
        wt[1] <- 1
        wt[match(fr[id == idNs[i]]$to, tms)] <- wtIn
        wt <- naReplace(wt)
        lines(tms, wt, col = "grey")
        if (any(is.na(wt))) 
            break
        wpr <- wpr + wt
    }
    wpr <- wpr/numIds
    lines(tms, wpr, col = "red")
    cat("Mean weights: ", mean(wpr))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
