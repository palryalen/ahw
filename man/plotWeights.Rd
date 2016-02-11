\name{plotWeights}
\alias{plotWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots time-dependent weight-trajectories for several individuals}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotWeights(w, t, g, n = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
Weights
}
  \item{t}{
Time}
  \item{g}{
Id 
  }
  \item{n}{
Number of trajectories to plot, defaults to 100
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
function (w, t, g, n = 100) 
{
    ug <- unique(g)[1:n]
    plot(NULL, xlim = c(min(t), min(max(t), 2990)), ylim = c(0.4, 
        3), xlab = "time", ylab = "weights")
    for (v in ug) {
        J <- which(v == g)
        lines(t[J], w[J], type = "s", col = "#00000022")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
