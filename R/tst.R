# Treat init randomization - 27.11.2018
fr1 <- data.frame(id=1:1000,from=rep(0,1000),to = c(rexp(500,1),rexp(500,.6)),L = c(rep(0,500),rep(1,500)),from.state="diag",to.state="treat")
fr1 <- as.data.table(fr1)
fr1 <- fr1[rep(1:nrow(fr1),each=2),]
fr1[2*(1:1000)]$from <-fr1[2*(1:1000)-1]$to
fr1[2*(1:1000)]$from.state <-fr1[2*(1:1000)-1]$to.state
fr1[2*(1:1000)]$to.state <- "death"
fr1[2*(1:1000)]$to <- fr1[2*(1:1000)]$to + c(rexp(500,.25),rexp(500,5))

ss1 <- rep(sample(2*(1:1000)),each=2);ss1[2*(1:1000)-1] <- ss1[2*(1:1000)]-1
fr1 <- fr1[ss1,]
fr1$id <- rep(1:1000,each=2)

write.table(fr1,file="fr1.txt",sep= " ",col.names = T)

# Models for time to treatment:
faFit <- aalen(Surv(from,to,to.state == "treat") ~ 1 + L, data=fr1)
cfaFit <- aalen(Surv(from,to,to.state == "treat") ~ 1, data=fr1)


dataFr <- fr1
atRiskState <- "diag"
eventState <- "treat"
stopTimeName  <- "to"
startStatusName <- "from.state"
endStatusName <- "to.state"
idName <- "id"
b <- 0.3


frame <- makeContWeights(faFit, cfaFit, dataFr, atRiskState, eventState, stopTimeName,
                         startStatusName, endStatusName, idName, b, willPlotWeights = T) 




write.table(head(frame,20),file="frame1.txt",sep= " ",col.names = T)


