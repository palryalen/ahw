\name{cumSumGroup}
\alias{cumSumGroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grouped cumulative sums
}
\description{
Computes cumulative sums for each set of consecutively identical group levels.
}
\usage{
cumSumGroup(x, group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Vector with doubles that should be cumulatively summed. 
}
  \item{group}{
    Vector with integers that identify group levels. 
}
}
\details{
Assumes that all entries, affiliated to a particular group level, are consecutive. 
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
Kjetil Røysland <kjetil.roysland@medisin.uio.no>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ave}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, group) 
{
    if (length(x) != length(group)) 
        stop("Length of x and group differs")
    r <- .C("cumSumGroup", w = as.double(x), as.integer(length(x)), 
        as.integer(group))
    return(r$w)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
