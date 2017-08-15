\name{MakeCensWeights}
\alias{MakeCensWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Additive hazard weights for censoring.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MakeCensWeights(fFit, cfFit, data, startTimeName, stopTimeName,
                             endStatusName, intStatusName, idName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fFormula}{
The \code{\link[survival]{aalen}} fit for the graph hazard.
}
  \item{cfFormula}{
The  \code{\link[survival]{aalen}} fit for the counterfactual hazard.
}
  \item{data}{
Data frame on long format,
}
 \item{startTimeName}{
Name of column with start-time of the at-risk period,
}
 \item{stopTimeName}{
Name of column with stop-time of the at-risk period,
}

\item{endStatusName}{
Name of the variable that contains the name of end state for each at-risk interval,
}
\item{intStatusName}{
Name of the variable that contains the initial state for each at-risk interval,
}
  \item{idName}{
Name of column in \code{data} that identifies individuals.
}
}
\details{
Uses \code{\link[survival]{aalen}} to perform the two additive hazard models, and then
\code{\link{RefineTimeScale}} to subdivide \code{data} such that the
subdivision is compatible with every change of the weights.
The weights are then computed as a stochastic exponential (product integral), i.e.
}
\value{
 Sufficiently expanded version of the data frame \code{data} where the weights
      are appended.
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
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
