\name{RefineTimeScale}
\alias{RefineTimeScale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subdivide interval data. 
}
\description{
Subdivides interval data on long-format into shorter intervals such that  
every value in \code{intTimes} is a boundary point of the
subdivided intervals. 
}
\usage{
RefineTimeScale(A, startTimeName, stopTimeName, intTimes, endStatusName, intStatusName=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
Data frame with interval data on 'long-format'. 
}
  \item{startTimeName}{
Name of column in \code{A} that contains the left boundaries for the intervals.
}
  \item{stopTimeName}{
Name of column in \code{A} that contains the right boundaries for the intervals.
}
  \item{intTimes}{
The time points that should subdivide the intervals in \code{A}.
}
  \item{endStatusNames}{
The names of the columns in \code{A} that contain the states that are observed at the 
right boundaries. 
}
  \item{intStatusNames}{ The names of the columns in \code{A} that contain the
interval states, i.e. the states at the interior and left boundaries for each
interval. If this is left \code{NULL}, then the interval states are assumed to
take the value \code{FALSE}.  
  }
}
\details{
Each row represents a transition, that occurs at the end of the interval, from the interval
state to the right boundary state. If \code{endStatusName} is \code{NULL} then
the state at the imputed  right boundaries are set to \code{FALSE}, otherwise
they are set to the corresponding interval states. The remaining observations
are assumed to occur at the left boundaries. 

}
\value{
Data frame with subdivided intervals.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Kjetil Røysland <kjetil.roysland@medisin.uio.no>
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
function (A, startTimeName, stopTimeName, intTimes, endStatusName) 
{
    N <- nrow(A)
    q <- N * (length(intTimes) + 1)
    r <- .C("intersect", as.double(A[, startTimeName]), as.double(A[, 
        stopTimeName]), as.integer(N), as.double(intTimes), as.integer(length(intTimes)), 
        newIntStart = double(q), newIntStop = double(q), newOLine = integer(q), 
        newNRows = integer(1))
    W <- data.frame(r$newOLine, r$newIntStart, r$newIntStop)
    if (r$newNRows < q) 
        W <- W[-((r$newNRows + 1):q), ]
    names(W) <- c("imputation", startTimeName, stopTimeName)
    A <- A[, !(names(A) \%in\% c(startTimeName, stopTimeName))]
    A <- cbind(A, imputation = 1:N)
    A <- merge(W, A, by.x = "imputation", by.y = "imputation", 
        sort = FALSE)
    A$imputation <- duplicated(A$imputation, fromLast = TRUE)
    A[A$imputation, names(A) \%in\% as.character(endStatusName)] <- FALSE
    A <- subset(A, select = -imputation)
    return(A)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
