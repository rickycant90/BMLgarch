
PCAvariables<-function(df,n=1,minexpl=0.15) {
library(missMDA)
library(foreach)
library(FactoMineR)






  if (length(which(is.na(df))!=0)){
    imputePCA(df,n)->dfc
    dfc$completeObs->dfc
    colnames(dfc)<-colnames(df)
    foreach(i=1:ncol(dfc),.combine='cbind') %do% as.numeric(dfc[,i])->dfc
    colnames(dfc)<-colnames(df)
  } else {
    df<-df

  }






  if  (is.numeric(df)==FALSE) {

    foreach(i=1:ncol(df),.combine='cbind') %do% as.numeric(df[,i])->dfn
    colnames(dfn)<-colnames(df)

  } else {

    dfn<-df
  }


  if (length(which(is.na(dfn))!=0)){
  imputePCA(dfn,n)->dfc
  dfc$completeObs->dfc
  colnames(dfc)<-colnames(df)
  foreach(i=1:ncol(dfc),.combine='cbind') %do% as.numeric(dfc[,i])->dfc
   colnames(dfc)<-colnames(df)
  } else {
    dfc<-df
  }
  dfn->df



  as.data.frame(df)->df
  suppressMessages(suppressWarnings(attach(df)))


res.pca3 <- PCA(df,ncp=10, graph = F)
plot(res.pca3)
res.pca3$var$cos2->capturedvar
res.pca3$eig->eigenvalues
foreach(i=1:nrow(capturedvar),.combine='c') %do% sum(capturedvar[i,])->vars
names(vars)<-rownames(capturedvar)
sort(vars)->vars
vars->completevars

if (vars[1]<minexpl) {

  repeat {

    vars[-1]->vars

    if (vars[1] > minexpl) {
      break
    }
  }
}

    vars->sel

    names(sel)->tickers


    foreach(i=1:length(tickers),.combine='cbind') %do% df[,which(colnames(df)==tickers[i])]->selected

     colnames(selected)<-tickers

     for(i in 1:ncol(selected)) {

       selected[which(abs(selected[,i])>1),i]<-median(selected[,i],na.rm=TRUE)
     }



    return(list(selected.variables=selected,cos2=capturedvar,eigenvalues=eigenvalues,sorted.variables=completevars,sel.tickers=selected))

    }





