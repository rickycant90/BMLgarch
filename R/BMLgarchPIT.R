BMLgarch<-function(X,dates,k=2,show.spec=TRUE,burns=800,mcmc=1700,Dic=-5000,maxht=2,fortime=7,ndraws=700,sds=3,pred.diag=FALSE) {
  
  if (k==3) {
    
    
    
    
    
    
    
    repeat {
      
      set.seed(sample(c(1:100),1))
      nmods<-1
      a<-sample(c(1:4),nmods)
      sample(c(1:nrow(dists3)),nmods)->b
      sample(c(1:nrow(fits3)),nmods)->c
      
      spec.X<-ALLMODELS3[[a]][[b]][[c]]
      
      if (show.spec==TRUE) {
        print(spec.X) 
      }
      
      #garchXl<-fit.mle(spec.X,X,ctr=list())
      garchXc<-fit.bayes(spec.X,X,ctr=list(N.burn=burns,N.mcmc=mcmc))
      ht(garchXc,garchXc$theta,X)->hhat
      hhat[2:length(X),1,1]->h1
      hhat[2:length(X),1,2]->h2
      hhat[2:length(X),1,3]->h3
      Pstate(garchXc,garchXc$theta,X)->shat
      shat[2:length(X),1,1]->s1
      shat[2:length(X),1,2]->s2
      shat[2:length(X),1,3]->s3
      cbind(s1,s2,s3)->filtpX
      foreach(i=1:length(h1),.combine='c') %do% max(filtpX[i,])->states
      htX0<-c(rep(0,length(h1)))
      h1[which((states==s1))]->htX0[which((states==s1))]
      h2[which((states==s2))]->htX0[which((states==s2))]
      h3[which((states==s3))]->htX0[which((states==s3))]
      cbind(h1,h2,h3)->regimevariances
      
      
      s1*h1+s2*h2+s3*h3->ehXc #expected conditional variance
      htX0[1]<-min(h1[1],h2[1], h3[1])
      ehXc[1]<-min(h1[1],h2[1], h3[1])
      
      
      max(ehXc)->upperb
      DIC(garchXc)->dic
      print(dic[[1]])
      print(upperb)
      if (dic[[1]] < Dic & upperb<maxht) {
        break
      }
    }
    
    sqrt(ehXc)->volXc
    sqrt(htX0)->volXreal
    smapX<-c(rep(0,length(h1)))
    1->smapX[which((states==s1))]
    2->smapX[which((states==s2))]
    3->smapX[which((states==s3))]
    
    
    Xts<-xts(X,order.by=dates[1:length(X)])
    Xts[-length(Xts)]->Xts
    ehXts<-xts(ehXc,order.by=dates[1:length(ehXc)])
    
    
    #volXc[which(volXc>1)]=0
    #ehXc[which(ehXc>1)]=0
    
    cbind.xts(ehXts,volXc,smapX*0.1)->tsX1
    cbind.xts(Xts,smapX*0.01)->tsX2
    X[1:length(ehXc)]->Xt
    
    
    
    cbind.data.frame(ehXc,htX0,h1,h2,h3,s1,s2,s3,filtpX,Xt)->verifystatesX
    colnames(tsX1)=c("ehXts","volXc","smapX")
    colnames(tsX2)=c("Xts","smapX")
    
    
    par(mfrow=c(3,1))
    tsRainbow <- rainbow(ncol(tsX1))
    # Plot the overlayed series
    plot(x = as.zoo(tsX1), ylab = "conditional variance", main = "Garch estimates", col = tsRainbow, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c(colnames(tsX1)), lwd=0.1,lty = 2,col = tsRainbow)
    
    plot(x = as.zoo(tsX2), ylab = "log-returns", main = "Time series", col = c("orange","brown"), screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c(colnames(tsX2)), lwd=0.1,lty = 2,col = c("orange","brown"))
    
    
    plot(x = as.zoo(cbind(s1,s2,s3)), ylab = "Filtered Probabilities", main = "Markov-chain path", col = c("blue","red","violet"), screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c(colnames(cbind(s1,s2,s3))), lwd=0.1,lty = 2,col = c("blue","red","violet"))
    
    BIC(garchXc)
    
    
    
    suppressMessages(invisible(attach(verifystatesX)))
    
    
    
    ALLMODELS3[[a]][[b]][[c]]
    
    repeat {
      
      simahead(garchXc,fortime,ndraws)->drawsX
      drawsX$draws->forecastsX
      drawsX$state->forecastedstatesX
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% mean(forecastsX[which(forecastedstatesX[,i]==0),i])->state0Xmeans
      foreach(i=1:ncol(forecastsX),.combine='c') %do% mean(forecastsX[which(forecastedstatesX[,i]==1),i])->state1Xmeans
      foreach(i=1:ncol(forecastsX),.combine='c') %do% mean(forecastsX[which(forecastedstatesX[,i]==2),i])->state2Xmeans
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% median(forecastsX[which(forecastedstatesX[,i]==0),i])->state0Xmeds
      foreach(i=1:ncol(forecastsX),.combine='c') %do% median(forecastsX[which(forecastedstatesX[,i]==1),i])->state1Xmeds
      foreach(i=1:ncol(forecastsX),.combine='c') %do% median(forecastsX[which(forecastedstatesX[,i]==2),i])->state2Xmeds
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% length(which(forecastedstatesX[,i]==0))/length(forecastedstatesX[,i])->weightspredX0
      foreach(i=1:ncol(forecastsX),.combine='c') %do% length(which(forecastedstatesX[,i]==1))/length(forecastedstatesX[,i])->weightspredX1
      foreach(i=1:ncol(forecastsX),.combine='c') %do% length(which(forecastedstatesX[,i]==2))/length(forecastedstatesX[,i])->weightspredX2
      
      
      foreach(i=1:fortime,.combine='c') %do% state0Xmeans[i]*weightspredX0[i]+state1Xmeans[i]*weightspredX1[i]+state2Xmeans[i]*weightspredX2[i]->volpathXMEAN
      foreach(i=1:fortime,.combine='c') %do% mean(drawsX$draws[,i])->volpathXmean
      foreach(i=1:fortime,.combine='c') %do% median(drawsX$draws[,i])->volpathXmedian
      cbind(volpathXMEAN,volpathXmean,volpathXmedian)->volpaths
      
      max(abs(volpathXMEAN))->predb1
      max(abs(volpathXmean))->predb2
      max(abs(volpathXmedian))->predb3
      print(predb1)
      print(predb2)
      print(predb3)
      if (is.nan(predb1)==F & is.finite(predb1)==T & predb1<mean(X)+sds*sd(X)|is.nan(predb2)==F & is.finite(predb2)==T & predb2<mean(X)+sds*sd(X)|is.nan(predb3)==F & is.finite(predb3)==T & predb3<mean(X)+sds*sd(X)) {
        break
      }
      else{
        return("Non converging forecasts!, run me again please")
      }
    }
    
    
    c(t(X),t(volpathXMEAN))->extendefortimeXcMEAN
    c(t(X),t(volpathXmean))->extendefortimeXcmean
    c(t(X),t(volpathXmedian))->extendefortimeXcmedian
    
    cbind(extendefortimeXcMEAN,extendefortimeXcmean,extendefortimeXcmedian)->extXlr
    
    par(mfrow=c(4,1))
    
    
    
    # Plot the overlayed series
    plot(x = as.zoo(extendefortimeXcMEAN[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcMEAN)]), ylab="extended series", xlab = "200d window", main = "Simulated process", col ="violet", lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = "predicted w/weigths", lwd=0.1,lty = 2,col = "violet")
    
    #Plot the overlayed series
    plot(x = as.zoo(extendefortimeXcmean[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmean)]), ylab="extended series", xlab = "200d window", main = "Simulated process (mean)", col ="green", lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = "predicted 1 (mean)", lwd=0.1,lty = 2,col = "green")
    
    plot(x = as.zoo(extendefortimeXcmedian[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmedian)]), ylab="extended series", xlab = "200d window", main = "Simulated process (median)", col = "blue",lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c("predicted 2 (median)"), lwd=0.1,lty = 2,col = "blue")
    
    plot(x = as.zoo(X[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmean)]), ylab="in-sample series", xlab = "200d window", main = "true series", col = "red",lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c("log returns X"), lwd=0.1,lty = 2,col = "red")
    

    if (pred.diag==TRUE) {
      
      pit(garchXc,drawsX$draws[,1],do.its=FALSE)->pit1
      plot(pit1)
      return(list(summary(garchXc),plot(drawsX),plot(pit1),PIT=pit1,draws=forecastsX,extended.series=extXlr,predictions=volpaths,verify=verifystatesX,fittedv=verifystatesX[,1],model=garchXc,spec=spec.X))
      
    } else {
      
      return(list(summary(garchXc),draws=forecastsX,extended.series=extXlr,predictions=volpaths,verify=verifystatesX,fittedv=verifystatesX[,1],model=garchXc,spec=spec.X))
        
    }
   

    
  }
  
  
  if (k==2) {
    
    
    
    
    
    
    
    
    
    
    
    repeat {
      
      set.seed(sample(c(1:100),1))
      nmods<-1
      a<-sample(c(1:4),nmods)
      sample(c(1:nrow(dists2)),nmods)->b
      sample(c(1:nrow(fits2)),nmods)->c
      
      spec.X<-ALLMODELS2[[a]][[b]][[c]]
      
      if (show.spec==TRUE) {
        print(spec.X) 
      }
      
      #garchXl<-fit.mle(spec.X,X,ctr=list())
      garchXc<-fit.bayes(spec.X,X,ctr=list(N.burn=burns,N.mcmc=mcmc))
      ht(garchXc,garchXc$theta,X)->hhat
      hhat[2:length(X),1,1]->h1
      hhat[2:length(X),1,2]->h2
      
      Pstate(garchXc,garchXc$theta,X)->shat
      shat[2:length(X),1,1]->s1
      shat[2:length(X),1,2]->s2
      
      cbind(s1,s2)->filtpX
      foreach(i=1:length(h1),.combine='c') %do% max(filtpX[i,])->states
      htX0<-c(rep(0,length(h1)))
      h1[which((states==s1))]->htX0[which((states==s1))]
      h2[which((states==s2))]->htX0[which((states==s2))]
      
      cbind(h1,h2)->regimevariances
      
      
      s1*h1+s2*h2->ehXc #expected conditional variance
      htX0[1]<-min(h1[1],h2[1])
      ehXc[1]<-min(h1[1],h2[1])
      
      
      max(ehXc)->upperb
      DIC(garchXc)->dic
      print(dic[[1]])
      print(upperb)
      if (dic[[1]] < Dic & upperb<maxht) {
        break
      }
    }
    
    sqrt(ehXc)->volXc
    sqrt(htX0)->volXreal
    smapX<-c(rep(0,length(h1)))
    1->smapX[which((states==s1))]
    2->smapX[which((states==s2))]
    
    
    
    Xts<-xts(X,order.by=dates[1:length(X)])
    Xts[-length(Xts)]->Xts
    ehXts<-xts(ehXc,order.by=dates[1:length(ehXc)])
    
    
    #volXc[which(volXc>1)]=0
    #ehXc[which(ehXc>1)]=0
    
    cbind.xts(ehXts,volXc,smapX*0.1)->tsX1
    cbind.xts(Xts,smapX*0.01)->tsX2
    X[1:length(ehXc)]->Xt
    
    
    
    cbind.data.frame(ehXc,htX0,h1,h2,s1,s2,filtpX,Xt)->verifystatesX
    colnames(tsX1)=c("ehXts","volXc","smapX")
    colnames(tsX2)=c("Xts","smapX")
    
    
    par(mfrow=c(3,1))
    
    tsRainbow <- rainbow(ncol(tsX1))
    # Plot the overlayed series
    plot(x = as.zoo(tsX1), ylab = "conditional variance", main = "Garch estimates", col = tsRainbow, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c(colnames(tsX1)), lwd=0.1,lty = 2,col = tsRainbow)
    
    plot(x = as.zoo(tsX2), ylab = "log-returns", main = "Time series", col = c("orange","brown"), screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c(colnames(tsX2)), lwd=0.1,lty = 2,col = c("orange","brown"))
    
    
    plot(x = as.zoo(cbind(s1,s2)), ylab = "Filtered Probabilities", main = "Markov-chain path", col = c("blue","violet"), screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c(colnames(cbind(s1,s2))), lwd=0.1,lty = 2,col = c("blue","violet"))
    
    BIC(garchXc)
    
    
    
    suppressMessages(invisible(attach(verifystatesX)))
    
    
    
    ALLMODELS2[[a]][[b]][[c]]
    
    repeat {
      
      simahead(garchXc,fortime,ndraws)->drawsX
      drawsX$draws->forecastsX
      drawsX$state->forecastedstatesX
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% mean(forecastsX[which(forecastedstatesX[,i]==0),i])->state0Xmeans
      foreach(i=1:ncol(forecastsX),.combine='c') %do% mean(forecastsX[which(forecastedstatesX[,i]==1),i])->state1Xmeans
      
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% median(forecastsX[which(forecastedstatesX[,i]==0),i])->state0Xmeds
      foreach(i=1:ncol(forecastsX),.combine='c') %do% median(forecastsX[which(forecastedstatesX[,i]==1),i])->state1Xmeds
      
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% length(which(forecastedstatesX[,i]==0))/length(forecastedstatesX[,i])->weightspredX0
      foreach(i=1:ncol(forecastsX),.combine='c') %do% length(which(forecastedstatesX[,i]==1))/length(forecastedstatesX[,i])->weightspredX1
      
      
      
      foreach(i=1:fortime,.combine='c') %do% state0Xmeans[i]*weightspredX0[i]+state1Xmeans[i]*weightspredX1[i]->volpathXMEAN
      foreach(i=1:fortime,.combine='c') %do% mean(drawsX$draws[,i])->volpathXmean
      foreach(i=1:fortime,.combine='c') %do% median(drawsX$draws[,i])->volpathXmedian
      cbind(volpathXMEAN,volpathXmean,volpathXmedian)->volpaths
      
      max(abs(volpathXMEAN))->predb1
      max(abs(volpathXmean))->predb2
      max(abs(volpathXmedian))->predb3
      print(predb1)
      print(predb2)
      print(predb3)
      if (is.nan(predb1)==F & is.finite(predb1)==T & predb1<mean(X)+sds*sd(X)|is.nan(predb2)==F & is.finite(predb2)==T & predb2<mean(X)+sds*sd(X)|is.nan(predb3)==F & is.finite(predb3)==T & predb3<mean(X)+sds*sd(X)) {
        break
      }
      else{
        return("Non converging forecasts!, run me again please")
      }
    }
    
    
    c(t(X),t(volpathXMEAN))->extendefortimeXcMEAN
    c(t(X),t(volpathXmean))->extendefortimeXcmean
    c(t(X),t(volpathXmedian))->extendefortimeXcmedian
    
    cbind(extendefortimeXcMEAN,extendefortimeXcmean,extendefortimeXcmedian)->extXlr
    
    par(mfrow=c(4,1))
    
    
    
    # Plot the overlayed series
    plot(x = as.zoo(extendefortimeXcMEAN[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcMEAN)]), ylab="extended series", xlab = "200d window", main = "Simulated process", col ="violet", lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = "predicted w/weigths", lwd=0.1,lty = 2,col = "violet")
    
    #Plot the overlayed series
    plot(x = as.zoo(extendefortimeXcmean[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmean)]), ylab="extended series", xlab = "200d window", main = "Simulated process (mean)", col ="green", lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = "predicted 1 (mean)", lwd=0.1,lty = 2,col = "green")
    
    plot(x = as.zoo(extendefortimeXcmedian[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmedian)]), ylab="extended series", xlab = "200d window", main = "Simulated process (median)", col = "blue",lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c("predicted 2 (median)"), lwd=0.1,lty = 2,col = "blue")
    
    plot(x = as.zoo(X[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmean)]), ylab="in-sample series", xlab = "200d window", main = "true series", col = "red",lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c("log returns X"), lwd=0.1,lty = 2,col = "red")
    
    if (pred.diag==TRUE) {
      
      pit(garchXc,drawsX$draws[,1],do.its=FALSE)->pit1
      plot(pit1)
      return(list(summary(garchXc),plot(drawsX),plot(pit1),PIT=pit1,draws=forecastsX,extended.series=extXlr,predictions=volpaths,verify=verifystatesX,fittedv=verifystatesX[,1],model=garchXc,spec=spec.X))
      
    } else {
      
      return(list(summary(garchXc),draws=forecastsX,extended.series=extXlr,predictions=volpaths,verify=verifystatesX,fittedv=verifystatesX[,1],model=garchXc,spec=spec.X))
      
    }
    
    
    

    
    
  }
  
  if(k==1) {
    
    
    
    
    
    
    
    
    
    
    
    repeat {
      
      set.seed(sample(c(1:10),1))
      nmods<-1
      a<-sample(c(1:2),nmods)
      sample(c(1:nrow(dists1)),nmods)->b
      sample(c(1:nrow(fits1)),nmods)->c
      
      spec.X<-ALLMODELS1[[a]][[b]][[c]]
      
      if (show.spec==TRUE) {
        print(spec.X) 
      }
      
      #garchXl<-fit.mle(spec.X,X,ctr=list())
      garchXc<-fit.bayes(spec.X,X,ctr=list(N.burn=burns,N.mcmc=mcmc))
      ht(garchXc,garchXc$theta,X)->hhat
      hhat[2:length(X)]->h1
      
      
      
      h1->regimevariances
      
      
      h1->ehXc #expected conditional variance
      
      
      max(ehXc)->upperb
      DIC(garchXc)->dic
      print(dic[[1]])
      print(upperb)
      if (dic[[1]] < Dic & upperb<maxht) {
        break
      }
    }
    
    sqrt(ehXc)->volXc
    
    
    
    Xts<-xts(X,order.by=dates[1:length(X)])
    Xts[-length(Xts)]->Xts
    ehXts<-xts(ehXc,order.by=dates[1:length(ehXc)])
    
    
    #volXc[which(volXc>1)]=0
    #ehXc[which(ehXc>1)]=0
    
    cbind.xts(ehXts,volXc)->tsX1
    Xts->tsX2
    X[1:length(ehXc)]->Xt
    
    
    
    cbind.data.frame(ehXc,h1,Xt)->verifystatesX
    colnames(tsX1)=c("ehXts","volXc")
    colnames(tsX2)=c("Xts")
    
    
    par(mfrow=c(2,1))
    tsRainbow <- rainbow(ncol(tsX1))
    # Plot the overlayed series
    plot(x = as.zoo(tsX1), ylab = "conditional variance", main = "Garch estimates", col = tsRainbow, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c(colnames(tsX1)), lwd=0.1,lty = 2,col = tsRainbow)
    
    plot(x = as.zoo(tsX2), ylab = "log-returns", main = "Time series", col = c("orange","brown"), screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c(colnames(tsX2)), lwd=0.1,lty = 2,col = c("orange","brown"))
    
    
    BIC(garchXc)
    
    
    
    suppressMessages(invisible(attach(verifystatesX)))
    
    
    
    ALLMODELS1[[a]][[b]][[c]]
    
    repeat {
      
      simahead(garchXc,fortime,ndraws)->drawsX
      drawsX$draws->forecastsX
      drawsX$state->forecastedstatesX
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% mean(forecastsX[which(forecastedstatesX[,i]==0),i])->state0Xmeans
      
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% median(forecastsX[which(forecastedstatesX[,i]==0),i])->state0Xmeds
      
      
      foreach(i=1:ncol(forecastsX),.combine='c') %do% length(which(forecastedstatesX[,i]==0))/length(forecastedstatesX[,i])->weightspredX0
      
      
      foreach(i=1:fortime,.combine='c') %do% mean(drawsX$draws[,i])->volpathXmean
      foreach(i=1:fortime,.combine='c') %do% median(drawsX$draws[,i])->volpathXmedian
      cbind(volpathXmean,volpathXmedian)->volpaths
      
      
      max(abs(volpathXmean))->predb2
      max(abs(volpathXmedian))->predb3
      
      print(predb2)
      print(predb3)
      if (is.nan(predb2)==F & is.finite(predb2)==T & predb2<mean(X)+sds*sd(X)|is.nan(predb3)==F & is.finite(predb3)==T & predb3<mean(X)+sds*sd(X)) {
        break
      }
      else{
        return(print("Non converging forecasts!, run me again please"))
      }
    }
    
    
    c(t(X),t(volpathXmean))->extendefortimeXcmean
    c(t(X),t(volpathXmedian))->extendefortimeXcmedian
    
    cbind(extendefortimeXcmean,extendefortimeXcmedian)->extXlr
    
    par(mfrow=c(3,1))
    
    
    
    
    #Plot the overlayed series
    plot(x = as.zoo(extendefortimeXcmean[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmean)]), ylab="extended series", xlab = "200d window", main = "Simulated process (mean)", col ="green", lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = "predicted 1 (mean)", lwd=0.1,lty = 2,col = "green")
    
    plot(x = as.zoo(extendefortimeXcmedian[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmedian)]), ylab="extended series", xlab = "200d window", main = "Simulated process (median)", col = "blue",lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c("predicted 2 (median)"), lwd=0.1,lty = 2,col = "blue")
    
    plot(x = as.zoo(X[(length(extendefortimeXcMEAN)-200):length(extendefortimeXcmean)]), ylab="in-sample series", xlab = "200d window", main = "true series", col = "red",lwd=2, screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c("log returns X"), lwd=0.1,lty = 2,col = "red")
    
    if (pred.diag==TRUE) {
      
      pit(garchXc,drawsX$draws[,1],do.its=FALSE)->pit1
      plot(pit1)
      return(list(summary(garchXc),plot(pit1),draws=forecastsX,PIT=pit1,extended.series=extXlr,predictions=volpaths,verify=verifystatesX,fittedv=verifystatesX[,1],model=garchXc,spec=spec.X))
      
    } else {
      
      return(list(summary(garchXc),draws=forecastsX,extended.series=extXlr,predictions=volpaths,verify=verifystatesX,fittedv=verifystatesX[,1],model=garchXc,spec=spec.X))
      
    }
    
  }
  
  
  
}
