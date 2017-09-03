rollingEWMA<-function(y,x,dates,span=1000,l=0.25,sds=3) {
  
  
  
  i<-1
  
  window <- span #3 months rolling window 
  pos<-window+1
  
  
  
  EWMAs<-list()
  while (i<=(nrow(y)-(window+1))) {
    
    for (pos in (window+1):nrow(y)) {
      
      
      pos-window->roll
      y[[j]][(roll):pos]->ty
      groups1 <- qcc.groups(x[1:length(ty)], ty)
      q <- ewma(ty, lambda=l, nsigmas=sds,plot=FALSE)
      q->EWMAs[[i]]
      print(c(i,pos))
      
      if (i==1) {
        
        plot(dates,y[[j]],type="l",xlab="Time",ylab="Logreturns",col="red",main="EWMA smoother")
        lines(ewmaSmooth(dates,y[[j]],lambda=l), col="green")
        
      }
      
      i+1->i
      
      pos+1->pos
    }
  }
  
  names(EWMAs)<-index(EWMAs)
  plot(EWMAs[[length(EWMAs)]])
  
  return=list(rollingEWMA=EWMAs,lambda=l,sds=sds)
}
