# Treat init randomization - 27.11.2018
# fr1 <- data.frame(id=1:1000,from=rep(0,1000),to = c(rexp(500,1),rexp(500,.6)),L = c(rep(0,500),rep(1,500)),from.state="diag",to.state="treat")
# fr1 <- as.data.table(fr1)
# fr1 <- fr1[rep(1:nrow(fr1),each=2),]
# fr1[2*(1:1000)]$from <-fr1[2*(1:1000)-1]$to
# fr1[2*(1:1000)]$from.state <-fr1[2*(1:1000)-1]$to.state
# fr1[2*(1:1000)]$to.state <- "death"
# fr1[2*(1:1000)]$to <- fr1[2*(1:1000)]$to + c(rexp(500,.25),rexp(500,5))
# 
# ss1 <- rep(sample(2*(1:1000)),each=2);ss1[2*(1:1000)-1] <- ss1[2*(1:1000)]-1
# fr1 <- fr1[ss1,]
# fr1$id <- rep(1:1000,each=2)

# fr1 <- fr1[1:100,]


# take II
xx <- seq(0,15,length.out = 20)
transitionMatrix2 <- vector("list",length = 4)
transitionMatrix2[[1]]$smoothspline[[1]] <- list(NULL)
transitionMatrix2[[1]]$smoothspline[[2]] <- list(NULL)
transitionMatrix2[[1]]$smoothspline[[3]] <- list(smooth.spline(xx,seq(0.6,0.6,length.out=length(xx))))
transitionMatrix2[[1]]$smoothspline[[4]] <- list(NULL)
transitionMatrix2[[1]]$neighbours <- c(3)
transitionMatrix2[[1]]$baseline <- c(1)
transitionMatrix2[[2]]$smoothspline[[1]] <- list(NULL)
transitionMatrix2[[2]]$smoothspline[[2]] <- list(NULL)
transitionMatrix2[[2]]$smoothspline[[3]] <- list(smooth.spline(xx,seq(1,1,length.out=length(xx))))
transitionMatrix2[[2]]$smoothspline[[4]] <- list(NULL)
transitionMatrix2[[2]]$neighbours <- c(3)
transitionMatrix2[[2]]$baseline <- c(1)
transitionMatrix2[[3]]$smoothspline[[1]] <- list(NULL)
transitionMatrix2[[3]]$smoothspline[[2]] <- list(NULL)
transitionMatrix2[[3]]$smoothspline[[3]] <- list(NULL)
transitionMatrix2[[3]]$smoothspline[[4]] <- list(smooth.spline(xx,seq(0.3,0.3,length.out=length(xx))))
transitionMatrix2[[3]]$neighbours <- c(4)
transitionMatrix2[[3]]$baseline <- c(1)
transitionMatrix2[[4]]$smoothspline[[1]] <- list(NULL)
transitionMatrix2[[4]]$smoothspline[[2]] <- list(NULL)
transitionMatrix2[[4]]$smoothspline[[3]] <- list(NULL)
transitionMatrix2[[4]]$smoothspline[[4]] <- list(NULL)
transitionMatrix2[[4]]$neighbours <- NULL
transitionMatrix2[[4]]$baseline <- c(1)

n <- 1e3
fr1 <- generateFrame(n,15,transitionMatrix2,c(1,2),1 )
fr1$L <- 1
fr1$L[fr1$from.state == 2] <- 0
fr1$L[fr1$id %in% fr1$id[fr1$L==0]] <- 0
fr1 <- fr1[fr1$from.state != 4,]
fr1$from.state[fr1$from.state %in% c(1,2)] <- "diag"
fr1$from.state[fr1$from.state %in% c(3)] <- "treat"
fr1$to.state[fr1$to.state %in% c(4)] <- "death"
fr1$to.state[fr1$to.state %in% c(3)] <- "treat"

# write.table(fr1,file="fr1.txt",sep= " ",col.names = T)

# Models for time to treatment:
faFit <- aalen(Surv(from,to,to.state == "treat") ~ 1 + L, data=fr1[fr1$from.state == "diag",])
cfaFit <- aalen(Surv(from,to,to.state == "treat") ~ 1, data=fr1[fr1$from.state == "diag",])


##
# plot(faFit$cum[,1],exp(faFit$cum[,2]) - cfaFit$cum[spp,2],type="s",ylim=c(0.5,1.5),xlim=c(0,5))
# lines(faFit$cum[,1],exp(0.2*faFit$cum[,1]))
##

dataFr <- fr1
atRiskState <- "diag"
eventState <- "treat"
stopTimeName  <- "to"
startStatusName <- "from.state"
endStatusName <- "to.state"
idName <- "id"
b <- 0.3


frame <- makeContWeights(faFit, cfaFit, dataFr, atRiskState, eventState, stopTimeName,
                         startStatusName, endStatusName, idName, b, willPlotWeights = F) 



###

frame[,w:=1]
frame[L==0 & from.state == "diag",w:=exp(to *(1- 0.8 )) ]
frame[L==1 & from.state == "diag",w:=exp(to *(0.6- 0.8 )) ]
frame[L==0 & from.state == "diag" & to.state == "treat",w:= w* 0.8/1]
frame[L==1 & from.state == "diag" & to.state == "treat",w:= w* 0.8/0.6]
frame[(from.state == "diag" & to.state == "treat") | from.state == "treat",w := cumprod(w),by=id]

idds <- idds+1
plot(frame[id==idds]$to,frame[id==idds]$weights,type="s",ylim=c(0,2.2),xlim=c(0,4))
lines(frame[id==idds]$to,frame[id==idds]$w,type="l",lty=2)

plotId <- function(tb,idds){
  subTb <- tb[id==idds]
  plot(subTb$to,subTb$weights,type="l",xlim=c(0,5),ylim=c(0.5,1.5))
  lines(subTb$to,exp(-0.2*subTb$to),type="l",lty=3,col="grey")
  lines(subTb$to,exp(0.2*subTb$to),type="l",lty=3,col="grey")
  
}




###


# write.table(head(frame,20),file="frame1.txt",sep= " ",col.names = T)



# Weighted outcome rergession
outMod <- aalen(Surv(from,to,to.state == "death")~1,data=frame,weights = frame$weights)

param <- pluginEstimate(1000,matrix(diff(c(0,outMod$cum[,2])),nrow=1),function(X)matrix(-X,nrow=1,ncol=1),list(function(X)matrix(-1,nrow=1,ncol=1)),matrix(1,nrow=1,ncol=1),matrix(0,nrow=1,ncol=1))


plot(outMod$cum[,1],param$X[1,],type="s",xlim=c(0,8),xlab="t",ylab="")
