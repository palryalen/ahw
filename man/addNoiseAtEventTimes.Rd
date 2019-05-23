\name{addNoiseAtEventTimes}
\alias{addNoiseAtEventTimes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
    Tie removal
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    Removes ties from by adding noise at tied times
}
\usage{
  addNoiseAtEventTimes(frame,idName,startTimeName,stopTimeName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frame}{
    Data on long format
    }
     \item{idName}{
    Name of column in \code{dataFr} that identifies individuals,
}
    \item{stopTimeName}{
    Name of column with stop time of the at risk period,
}
\item{startStatusName}{
    Name of the variable that contains the name of start state for each at-risk interval,
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
    Pål Christie Ryalen <p.c.ryalen@medisin.uio.no>
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line