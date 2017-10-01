#' @title <logreturns>
#'
#' @description <Compute log-returns on a data.frame object>
#' @param df A data.frame object.
#' @export


logreturns<-function(df) {
i<-1
lrdf<-list()
for (i in 1:ncol(df)) {

  diff(log(df[,i]))->lrdf[[i]]
  i<-1+i
}
as.data.frame(lrdf)->lrdf
colnames(lrdf)<-colnames(df)

return=lrdf
}
