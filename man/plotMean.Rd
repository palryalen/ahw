\name{plotMean}
\alias{plotMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots mean of time continous-time weights}
\description{
Preliminary plot of mean of individual weight-trajectories
}
\usage{
plotMean(weights, startTime, stopTime, pTimes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weights}{
%%     ~~Describe \code{weights} here~~
}
  \item{startTime}{
Start of at-risk interval
  }
  \item{stopTime}{
Stop of at-ris interval}
  \item{pTimes}{
Times where the mean should be plotted. 
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
function (weights, startTime, stopTime, pTimes) 
{
    n <- length(pTimes)
    y <- rep(0, n)
    for (j in 1:n) {
        J <- (startTime <= pTimes[j]) & (stopTime >= pTimes[j])
        y[j] <- mean(weights[J], na.rm = T)
    }
    lines(pTimes, y, col = "red")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
