#' @title <factorforest>
#'
#' @description <Performing and cross-validating random forest models on each asset included in a data.frame object, given a matrix of risk factors obtained with PCA>
#' @param factors Data.frame object containing time series of the estimated factors.
#' @param data Data.frame object containing prices time series of each asset in the market.
#' @param ntr Integer, number of observations of the training dataset.
#' @param window Integer, number of observations to see in the diagnostic plots.
#' @export

factorforest<-function(factors,data,ntr=800,window=100) {
library(randomForest)
  rForests<-list()
  pred.values<-list()
  pred.os<-list()
  for (i in 1:ncol(data)) {
    Y<-data[,i]
  Y[1:nrow(factors)]->Y

cbind.data.frame(factors,Y)->dft #dataframe con variabile esplicativa e pca
dft[1:ntr,]->pidtrain
dft[(ntr+1):nrow(dft),]->pidtest

factors[1:ntr,]->ftrain
factors[(ntr+1):nrow(dft),]->ftest #dataset train/test con soli pca

suppressWarnings(suppressMessages(attach(pidtrain)))

pidtrain$Y->Yt

randomForest(Yt ~ .,ftrain,ntree=1000)->for1
for1$predicted->preds
  preds->pred.values[[i]]
plot(preds[1:window],type='l',col="blue")
lines(Yt[1:window],type='l',col="red")
for1->rForests[[i]]


pidtest$Y->Yt2
predict(for1,ftest)->preds2
 preds2->pred.os[[i]]
plot(preds2,type='l',col="blue")
lines(Yt2,type='l',col="red")

print(for1)
print(i)
  }
  as.data.frame(pred.os)->pred.os
  as.data.frame(pred.values)->pred.values
  colnames(pred.os)<-colnames(data)
    colnames(pred.values)<-colnames(data)
  return=list(out.of.sample.preds=pred.os,in.sample.preds=pred.values,rForests=rForests)
}
