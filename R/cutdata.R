#' @title <cutdata>
#'
#' @description <removes columns in a data.frame if the total number of missing values exceed a certain treshold>
#' @param df A data.frame object.
#' @param nas Integer, maximum number of NA's to tolerate at each column.
#' @export


cutdata<-function(df,nas=300) {

  library(zoo)
  as.data.frame(df)->df
k<-1
i<-1
j<-1
dfupd<-list()
incomplete<-list()
namescomp<-vector()
namesincomp<-vector()
for (i in 1:ncol(df)) {
  df[,i]->tempvect
  if (length(which(is.na(tempvect)))<nas) {
    tempvect->dfupd[[k]]
    colnames(df)[i]->namescomp[k]
    k<-k+1
    i<-i+1
  } else {

    tempvect->incomplete[[j]]
    colnames(df)[i]->namesincomp[j]
    j<-j+1
    i<-i+1
  }

  print(c(i,k,j))
}
as.data.frame(dfupd)->dfc
as.data.frame(incomplete)->dfpost
colnames(dfc)<-namescomp
colnames(dfpost)<-namesincomp
library(Amelia)
missmap(dfc, main = "Missing values vs observed")

return=list(dfc=dfc,dfpost=dfpost)
}
