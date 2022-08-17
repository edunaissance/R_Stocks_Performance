library(scales)
library(TTR)
library(zoo)
library(xts)
library(ggplot2)
library(quantmod)
library(tseries)
library(PerformanceEstimation)
library(PerformanceAnalytics)
library(timeSeries)
library(quadprog)
library(Hmisc)
library(corrplot)
library(plotly)

homeuser="{working directory}"
#homeuser is set as my working directory, you should set it as yours. 
setwd(paste(homeuser,"/an example",sep=""))

GOOGLE <-getSymbols("GOOG",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
RHHVF <-getSymbols("RHHVF",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
MRK <-getSymbols("MRK",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
ABT <-getSymbols("ABT",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
MDT <-getSymbols("MDT",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
TMO <-getSymbols("TMO",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
SNY <-getSymbols("SNY",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
SNYNF <-getSymbols("SNYNF",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
LLY <-getSymbols("LLY",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)
BMY <-getSymbols("BMY",from =  "2017-01-01",to = "2018-12-31",src = "yahoo",auto.assign=FALSE)

GOOGLE <- GOOGLE[,c(6)]
RHHVF <- RHHVF[,c(6)]
MRK <- MRK[,c(6)]
ABT <- ABT [,c(6)]
MDT <- MDT [,c(6)]
TMO <- TMO [,c(6)]
SNY <- SNY [,c(6)]
SNYNF <- SNYNF [,c(6)]
LLY <- LLY [,c(6)]
BMY <- BMY [,c(6)]

colnames (GOOGLE) <-c("GOOGLEPrice")
colnames (ABT) <-c("ABTPrice")
colnames (BMY) <-c("BMYPrice")
colnames (LLY) <-c("LLYPrice")
colnames (MDT) <-c("MDTPrice")
colnames (MRK) <-c("MRKPrice")
colnames (RHHVF) <-c("RHHVFPrice")
colnames (SNY) <-c("SNYPrice")
colnames (SNYNF) <-c("SNYNFPrice")
colnames (TMO) <-c("TMOPrice")

StocksPrice <- cbind(GOOGLE, ABT, BMY, LLY, MDT, MRK, RHHVF, SNY, SNYNF, TMO)

StocksPrice.xts <- as.xts(StocksPrice)

write.table(StocksPrice.xts, file="StocksPrice.csv",sep=",",row.names=TRUE)

StocksPrice_Return <- diff (log (StocksPrice))
StocksPrice_Return <- na.omit(StocksPrice_Return)
Mean_SD_Data <- StocksPrice_Return [c(1),]
write.csv(Mean_SD_Data, file =  "Mean_SD.csv", row.names = F, quote = F)
Mean_SD<-read.table('Mean_SD.csv',sep=',',header = TRUE)
Mean_SD[c(1),] <- colMeans(StocksPrice_Return)
Mean_SD[c(2),] <- colSds(StocksPrice_Return)

write.csv(Mean_SD, file =  "Mean_SD.csv", row.names = F, quote = F)
x <- as.numeric ( Mean_SD[c(1),])
y <- as.numeric ( Mean_SD[c(2),])
plot (usgas,cagas ,xlab = "Date",ylab = "Price",main = "US-Canada Gas Price", ylim = c(0, 1.4))
lm.ab<-lm(y ~ x)
abline(lm.ab)
dev.print(pdf, file = "Means_SD")




res <- StocksPrice_Return [,c(1:10)]
res <- cor(as.matrix (res))
write.csv(res, file =  "Correlation Coefficient.csv", row.names = F, quote = F)
round(res, 2)
res <- data.frame(res)
res_2 <- rcorr(as.matrix (StocksPrice[,c(1:10)]))

corrplot(cor(as.matrix (StocksPrice_Return[,c(1:10)])), type = "upper", order = "hclust", method = "ellipse")
dev.print(pdf, file = "CorrlationPlot")
chart.Correlation(StocksPrice[,c(1:10)], histogram=TRUE, pch=19)
dev.print(pdf, file = "Corr_Coef")

ER_VAR_SD_SR <- matrix(NA,23,4)
colnames(ER_VAR_SD_SR) <- c("ExpectedReturn", "Variance", "StandardDeviation", "SharpeRatio")
row.names(ER_VAR_SD_SR) <- c("MaxSharpePort","MaxSharpePort_Noshort","MaxExpectedReturnPort","MinVariancePort",
                             "ExpecctedReturn1","ExpecctedReturn2","ExpecctedReturn3","ExpecctedReturn4","ExpecctedReturn5",
                             "ExpecctedReturn6","ExpecctedReturn7","ExpecctedReturn8","ExpecctedReturn9","ExpecctedReturn10",
                             "ExpecctedReturn11","ExpecctedReturn12","ExpecctedReturn13","ExpecctedReturn14","ExpecctedReturn15",
                             "ExpecctedReturn16","ExpecctedReturn17","ExpecctedReturn18","ExpecctedReturn19")
risk.free <- Mean_SD [1,12]
#risk.p <- 0.5

Dmat <- cov(StocksPrice_Return[,c(1:10)]) 
N <- ncol(Dmat)
dvec <- as.numeric(Mean_SD[1,1:10])
meq <- 1
Amat <- matrix(1, nrow=nrow(Dmat))
bvec <- 1
Max_Sharpe <- solve.QP(2*Dmat, dvec, Amat, bvec, meq)

Max_Sharpe$solution
ER_VAR_SD_SR [1,1] <- dvec%*%Max_Sharpe$solution
ER_VAR_SD_SR [1,2] <- t(Max_Sharpe$solution)%*%Dmat%*%Max_Sharpe$solution
ER_VAR_SD_SR [1,3] <- sqrt(ER_VAR_SD_SR [1,2])
ER_VAR_SD_SR [1,4] <- (ER_VAR_SD_SR [1,1]-risk.free)/sqrt(ER_VAR_SD_SR [1,2])

Amat <- cbind(1,diag(nrow(Dmat)))
bvec <- c(1,rep(0,nrow(Dmat)))
Max_Sharpe_NoShorts <- solve.QP(Dmat, dvec, Amat, bvec, meq)

Max_Sharpe_NoShorts$solution
ER_VAR_SD_SR [2,1] <- dvec%*%Max_Sharpe_NoShorts$solution
ER_VAR_SD_SR [2,2] <- t(Max_Sharpe_NoShorts$solution)%*%Dmat%*%Max_Sharpe_NoShorts$solution
ER_VAR_SD_SR [2,3] <- sqrt(ER_VAR_SD_SR [2,2])
ER_VAR_SD_SR [2,4] <- (ER_VAR_SD_SR [2,1]-risk.free)/sqrt(ER_VAR_SD_SR [2,2])

ER_VAR_SD_SR [3,1] <- max(Mean_SD[1,1:10])
ER_VAR_SD_SR [3,2] <- (Mean_SD [2,which.max(Mean_SD[1,1:10])])^2
ER_VAR_SD_SR [3,3] <- Mean_SD [2,which.max(Mean_SD[1,1:10])]
ER_VAR_SD_SR [3,4] <- (ER_VAR_SD_SR [3,1]-risk.free)/ER_VAR_SD_SR [3,3]

dvec <- rep.int(0, N)
Amat <- cbind(rep(1,N), diag(1,N))
bvec <- c(1, rep(0,N))
Min_Var <- solve.QP(2*Dmat, dvec, Amat, bvec, meq)
Min_Var$solution
ER_VAR_SD_SR [4,1] <- as.numeric(Mean_SD[1,1:10])%*%Min_Var$solution
ER_VAR_SD_SR [4,2] <- t(Min_Var$solution)%*%Dmat%*%Min_Var$solution
ER_VAR_SD_SR [4,3] <- sqrt(ER_VAR_SD_SR [4,2])
ER_VAR_SD_SR [4,4] <- (ER_VAR_SD_SR [4,1]-risk.free)/sqrt(ER_VAR_SD_SR [4,2])

Amat <- cbind(1,diag(nrow(Dmat)),-1 * diag(nrow(Dmat)))
dvec <- as.numeric(Mean_SD[1,1:10])
bvec <- c(1, rep(0, nrow(Dmat)), rep(-1, nrow(Dmat)))
j <- 5
for( i in seq(0.05,0.95,0.05)){
  ret <- solve.QP(2*Dmat, dvec*i, Amat, bvec, meq, factorized=FALSE)
  ret$solution[abs(ret$solution) <= 1e-7] <- 0
  ER_VAR_SD_SR [j,1] <- dvec %*% ret$solution
  ER_VAR_SD_SR [j,2] <- t(ret$solution)%*%Dmat%*%ret$solution
  ER_VAR_SD_SR [j,3] <- sqrt(ER_VAR_SD_SR [j,2])
  ER_VAR_SD_SR [j,4] <- (ER_VAR_SD_SR [j,1]-risk.free)/ER_VAR_SD_SR [j,3]
  j = j+1
  if (j > 21) break
  
}

eff <- ER_VAR_SD_SR [3:21,c(1,3)]
eff <- eff [, c("StandardDeviation","ExpectedReturn")]
plot(eff, ylab = "Ecpected Return",xlab = "StDev",main = "Effctive Frontier", xlim = c(0.007, 0.013), ylim = c(0.0006, 0.0012))
dev.print(pdf, file = "Effective Frontier")

charts.PerformanceSummary(ROC(StocksPrice[,c(1:10)], n = 1, type = "discrete"), main="Daily Performance")
dev.print(pdf, file = "Performance")
write.csv(eff, file =  "Effctive Frontier.csv", row.names = F, quote = F)
