PCAportfolios<-function(selection,Center=T,Scale=F,ncomp=6) {


prcomp(selection,center=Center,scale=Scale)->PCAfinal
  PCAfinal$x[,1:ncomp]->PCs
PCAfinal$rotation[,1:ncomp]->portfoliosf

i<-1
sportf<-list()
for (i in 1:ncol(portfoliosf)) {

  sort(portfoliosf[,i])->sportf[[i]]

  sportf[[i]]/sum(sportf[[i]])->sportf[[i]]

  print(paste('Most correlated/uncorrelated variables PCA',i, sep='i'))

  print(cbind(round(head(sportf[[i]]),4),cbind(names(tail(sportf[[i]])),round(tail(sportf[[i]]),4))))

  #print(paste('Most anticorrelated variables PCA',i, sep='i'))
  #print(cbind(names(tail(sportf[[i]])),head(sportf[[i]])))

  i<-i+1
   }
return=list(Component.portfolios=sportf,PCAfinal=PCAfinal,plot(PCAfinal), summary(PCAfinal),unsorted=portfoliosf,factors=PCs)
}

