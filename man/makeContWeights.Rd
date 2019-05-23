\name{makeContWeights}
\alias{makeContWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Continuous time weight estimation based on \code{aalen.predict}
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Refines data longitudinally in order to do estimate parameters(e.g. Nelson-Aalen or Kaplan-Meier) using continuous time weights. The weights can be assessed using the plot option.
}
\usage{
function (faFit, cfaFit, dataFr, atRiskState, eventState, stopTimeName, 
    startStatusName, endStatusName, idName, b, weightRange = c(0, 10),
    willPlotWeights = T) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{faFit}{
The \code{\link[timereg]{aalen}} fit for the factual hazard,
}
  \item{cfaFit}{
The  \code{\link[timereg]{aalen}} fit for the hypothetical hazard,
}
  \item{dataFr}{
\code{\link{data.frame}} or \code{\link{data.table}} on long format,
}
 \item{atRiskState}{
At risk state for the event of interest,
}
\item{eventState}{
State for the event of interest,
}
 \item{stopTimeName}{
Name of column with stop time of the at risk period,
}
\item{startStatusName}{
Name of the column that contains the starting state for each interval,
}
\item{endStatusName}{
Name of the column that contains the end state for each interval,
}
  \item{idName}{
Name of column in \code{dataFr} that identifies individuals,
}
\item{b}{
Smoothing bandwidth parameter,
}
\item{weightRange}{
Truncates weights outside this interval,
}
\item{willPlotWeights}{
Plot indicator,
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
Longitudinally refined \code{\link{data.table}} of the initial \code{dataFr} with \code{weights} column added.
}
\references{
%% ~put references to the literature/web site here ~
\url{https://arxiv.org/abs/1802.01946}
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


data(fr1)
data(Tmat)

# fr1 is a longitudinal data set with subjects that are diagnosed at time 0, and may
# be treated as time evolves. Subjects can die before receiving treatment:
head(fr1)

# Time to treatment and death are confounded by the baseline variable L. We want to
# mimic a scenario where time to treatment is randomized (and does not depend on L):
fFit <- aalen(Surv(from,to,to.state =="treat")~1 + L,data=fr1[fr1$from.state == "diag",])
cfFit <- aalen(Surv(from,to,to.state =="treat")~1,data=fr1[fr1$from.state == "diag",])

# We calculate and plot the weights
frame1 <- makeContWeights(fFit, cfFit, fr1, "diag", "treat", "from", "to",
                         "from.state", "to.state", "id", b = 0.4,
                         weightRange = c(0,5)) 

# We fit a weighted model for the outcome. A is a treatment indicator (A=1 means treated).
a1 <- aalen(Surv(from,to,to.state =="death") ~ 1 + A,data=frame1,weights = frame1$weights)

# We plot the A coefficient from the weighted regression,
# and compare with the true hypothetical coefficient
plot(a1$cum[,c(1,3)],type="s",ylim=c(-1.2,0.5),xlim=c(0,5),
main="Weighted additive hazard treatment coefficient")
lines(Tmat,col=2)
legend("bottomleft",c("weighted estimate","hypothetical treatment coef")
,lty=1,col=c(1,2),bty="n")



data(fFrame)

# Next we consider an example with dependent censoring.
# Subjects are censored depending on a baseline variable u. We wish to mimic the
# cumulative hazard for death we would have seen if the censoring were independent.

faFit <- aalen(Surv(from,to,to.state=="Censored") ~ 1 + u, data = fFrame)
cfaFit <- aalen(Surv(from,to,to.state=="Censored") ~ 1, data = fFrame)

frame <- makeContWeights(faFit,cfaFit,fFrame,"Alive","Censored","from","to","from.state",
"to.state","id",100)

fMod <- aalen(Surv(from,to,to.state=="Dead")~1,data=fFrame)
wMod <- aalen(Surv(from,to,to.state=="Dead")~1,data=frame,weights = frame$weights)

plot(fMod$cum,type="s",main="Nelson-Aalen for death",ylab="")
lines(wMod$cum,type="s",col="red")
legend("topleft",c("factual","weighted factual"),lty=1,col=c(1,"red"),bty="n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
