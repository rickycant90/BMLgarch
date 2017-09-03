#' @title <imputeBBridge>
#'
#' @description <imputes a brownian bridge for each missing value in a dataset>
#' @param df
#' @export


imputeBBridge<-function(df,meanwindow=90,verbose=FALSE) {
  df[colSums(is.na(df)) > 0]->missing
  colnames(df[colSums(is.na(df)) > 0])->nams
  i<-1
  complete<-list()
  imputed<-list()
  for (i in 1:ncol(missing)) {
    missing[,i]->colum
    which(is.na(colum))->nanni

    j<-0
    h<-1
    count1<-0
    for (h in 1:length(nanni)) {

      if (is.na(colum[nanni[h]-j])==TRUE) {
        repeat {
          if (nanni[h]==1) { colum[1]<-round(mean(colum[1:meanwindow],na.rm=TRUE),5)
          if (verbose==TRUE) {print(c("Initial value of BBridge",colum[1])) }
          break }
          else if (is.na(colum[nanni[h]-j]==TRUE)) {
            count1<-count1+1


            j<-j+1
          }  else if (is.na(colum[nanni[h]-j])==FALSE) {
            colum[nanni[h]-j]->x0

            break }
        }
      }
      if (is.na(colum[nanni[h]-j])==FALSE) {
        colum[nanni[h]-j]->x0
        if (verbose==TRUE) {print("Initial value of BBridge")
          print(x0)}
      }
      j<-0
      count2<-0

      if (is.na(colum[nanni[h]-j])==TRUE) {
        repeat {
          if (nanni[h]==1) { colum[1]<-round(mean(colum,na.rm=TRUE),5)

          break }
          else if (is.na(colum[nanni[h]-j]==TRUE)) {
            count2<-count2+1


            j<-j+1
          }  else if (is.na(colum[nanni[h]-j])==FALSE) {
            colum[nanni[h]-j]->xT

            break }
        }
      }
      if (is.na(colum[nanni[h]-j])==FALSE) {
        colum[nanni[h]-j]->xT
        if (verbose==TRUE) {print("final value of BBridge")
          print(x0)}
      }
      library(sde)
      BBridge(x=x0,y=xT,t0=1,T=2,N=100)->complete[[h]]
      mean(complete[[h]])->colum[nanni[h]]

    }
    colum->imputed[[i]]
  }
  as.data.frame(imputed)->fixed
  colnames(fixed)<-nams
  i<-1

  for (i in 1:ncol(fixed)) {


    fixed[which(fixed[,i]<0),i]<-abs(fixed[which(fixed[,i]<0),i])
    df[,-which(colnames(df) %in% nams)]->purged
    cbind.data.frame(purged,fixed)->bridged


  }
  return=list(completeObs=bridged,bridges=complete)
}
