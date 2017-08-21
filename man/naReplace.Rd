\name{naReplace}
\alias{naReplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
    Replaces NA-values in \code{vec} with last non-NA value.
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    Assumes first element is non-NA
}
\usage{
  naReplace(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
    Vector of any type
    %%     ~~Describe \code{vec} here~~
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
\examples{
  naReplace(c(1,2,3,NA,NA,4))
  naReplace(c("text",NA,NA))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line