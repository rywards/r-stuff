# Quick Stock data analysis
library("readr")
library("plyr")
setwd("D:\Documents\School Documents\Spring 2021\Con Stat\Research Project\Istanbul Stock")
getwd()
stockInfo=read.csv("data_akbilgic.csv")

head(stockInfo)
attach(stockInfo)

#average daily returns
100*mean(ISE.USD)
100*mean(DAX)
100*mean(SP)
100*mean(FTSE)
100*mean(NIKKEI)
100*mean(BOVESPA)
100*mean(EU)
100*mean(EM)

#total return
100*sum(ISE.USD)
100*sum(DAX)
100*sum(SP)
100*sum(FTSE)
100*sum(NIKKEI)
100*sum(BOVESPA)
100*sum(EU)
100*sum(EM)

# Finding total positive and negative days
# with(stockInfo,count(sign(DAX)))
# we can use the individual data for later if we want

### Positives ###
posISE=nrow(stockInfo[stockInfo$ISE.USD>0,])
posDAX=nrow(stockInfo[stockInfo$DAX>0,])
posSP=nrow(stockInfo[stockInfo$SP>0,])
posFTSE=nrow(stockInfo[stockInfo$FTSE>0,])
posNIKKEI=nrow(stockInfo[stockInfo$NIKKEI>0,])
posBOVESPA=nrow(stockInfo[stockInfo$BOVESPA>0,])
posEU=nrow(stockInfo[stockInfo$EU>0,])
posEM=nrow(stockInfo[stockInfo$EM>0,])

### Negatives ###
negISE=nrow(stockInfo[stockInfo$ISE.USD<0,])
negDAX=nrow(stockInfo[stockInfo$DAX<0,])
negSP=nrow(stockInfo[stockInfo$SP<0,])
negFTSE=nrow(stockInfo[stockInfo$FTSE<0,])
negNIKKEI=nrow(stockInfo[stockInfo$NIKKEI<0,])
negBOVESPA=nrow(stockInfo[stockInfo$BOVESPA<0,])
negEU=nrow(stockInfo[stockInfo$EU<0,])
negEM=nrow(stockInfo[stockInfo$EM<0,])

# Totals
totalPos=(posISE+posDAX+posSP+posFTSE+posNIKKEI+posBOVESPA+posEU+posEM)
totalNeg=(negISE+negDAX+negSP+negFTSE+negNIKKEI+negBOVESPA+negEU+negEM)
totalPos
totalNeg

# regression
x=stockInfo$ISE.USD
y=stockInfo$SP

out=lm(x~y)

stock_plot=plot(x,y,xlab="USD Based ISE ADR",ylab="SP ADR")
stock_plot
abline(out)

# correlation of the variables
corr=cor(x,y)
corr

# confidence interval
confint(out,level=0.95)

x=stockInfo$ISE.USD
y=stockInfo$EM

out=lm(x~y)

stock_plot=plot(x,y,xlab="USD Based ISE ADR",ylab="EM ADR")
stock_plot
abline(out)

# correlation of the variables
corr=cor(x,y)
corr




