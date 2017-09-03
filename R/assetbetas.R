assetbetas<-function(specm,wspecm=list(),dates,Y,k=2,p=1,npar,df,wdf,Aic=-1500) {

  library(xts)
  library(MSwM)
  library(foreach)

  datest<-dates[1:length(Y)]
  Yts<-xts(Y,order.by=datest)
  Yts[-length(Yts)]->Yts
  lag(Yts,1)->Y_1
  Y_1[-1]->Y_1

  if (k==3|length(wspecm)==3) {

    Ylrt<-Y[1:nrow(df)]

    #s1[2:length(s1)]->s1
    #s2[2:length(s1)]->s2
    #s3[2:length(s1)]->s3

    s1*h1+s2*h2+s3*h3->ehY
    sqrt(ehY)->  volY

    sqrt(h1)->vol1
    sqrt(h2)->vol2
    sqrt(h3)->vol3

    wreg<-cbind.data.frame(df,wdf,vol1,vol2,vol3)
    print("estimating weighted glms...")

    glm(wspecm[[1]], family = gaussian, wreg, weights=s1)->Ym1
    glm(wspecm[[2]], family = gaussian, wreg, weights=s2)->Ym2
    glm(wspecm[[3]], family = gaussian, wreg, weights=s3)->Ym3



    list(glm1=Ym1,glm2=Ym2,glm3=Ym3)->glms

    print("estimating MSM...")

    repeat  {


      mYc=msmFit(specm,k=k,p=p,sw=c(rep(T,npar)),df,control=list(parallel=T))


      AIC(mYc)->aic
      print(aic[1])
      if (aic[1] < Aic) {
        break
      }
    }



    #mY0@model$fitted.values->fitmeanY0
    mYc@model$fitted.values->fitmeanYc
    mYc@model$residuals->resYc
    #mY0@model$residuals->resY0

    #list(mY0,mYc)->mY
    list(mYc)->mY
    #cbind.data.frame(fitmeanY0,resY0,htY0[1:length(fitmeanY0)])->fitplot1
    cbind.data.frame(fitmeanYc,resYc,volY[(1+p):length(volY)])->fitplot2




    #plot(x = as.zoo(fitplot1), ylab = "Diagnostics mean", main = "sqrt(h(t))", col = rainbow(ncol(fitplot1)), screens = 1)
    #legend(x = "bottomleft", legend = c(colnames(fitplot1)), lwd=0.1,lty = 2,col = rainbow(ncol(fitplot1)))

    par(mfrow=c(1,1))
    plot(x = as.zoo(fitplot2), ylab = "Diagnostics mean", main = "Conditional Mean+variance", col = rainbow(ncol(fitplot2)), screens = 1)
    legend(x = "bottomleft", legend = c("Fitted Mean Y","Residuals Y","Fitted Garch"), lwd=0.1,lty = 2,col = rainbow(ncol(fitplot2)))

    fitplot2->graphic.diags
    #c(mY0@Fit@logLikel,mYc@Fit@logLikel)->llY

    #plotDiag(mY[[which(llY==min(llY))]])
    plotDiag(mY[[1]])
    #mY[[which(llY==min(llY))]]@Fit@filtProb->filtpmeanY
    mY[[1]]@Fit@filtProb->filtpmeanY

    print("mapping states...")

    foreach(i=1:nrow(filtpmeanY),.combine='c') %do% max(filtpmeanY[i,])->statesmean
    statesY<-c(rep(0,nrow(filtpmeanY)))
    1->statesY[which((statesmean==filtpmeanY[,1]))]
    2->statesY[which((statesmean==filtpmeanY[,2]))]
    3->statesY[which((statesmean==filtpmeanY[,3]))]

    par(mfrow=c(1,1))
    as.zoo(cbind(fitmeanYc[1:length(statesY)],Y[1:length(statesY)],statesY*0.01),datest)->statesvsY
    rainbow(ncol(statesvsY))->colori
    plot(x = statesvsY[], ylab = "States", main = "Fit vs log-returns", col = colori,type='l', lwd=c(2,1.6,1.6),screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c("fitted mean","logreturns","regimes map"), lwd=0.1,lty = 2,col = colori)

print(summary(Ym1))
print(summary(Ym2))
print(summary(Ym3))

    print("%regime1")
    print(length(which(statesY==1))/length(Y))
    print("%regime2")
    print(length(which(statesY==2))/length(Y))
    print("%regime3")
    print(length(which(statesY==3))/length(Y))

    print(summary(mYc))

    return(list(summary(mYc),summary(Ym1),summary(Ym2),summary(Ym3),model=mYc,weighted.models=glms, graphic.diags=graphic.diags,states.map=statesY))->output



  }








  if (k==2 | length(wspecm)==2) {

    suppressMessages(suppressWarnings(attach(wdf)))
    Ylrt<-Y[1:nrow(df)]

    h1*s1+h2*s2->ehY

    sqrt(ehY)->volY


    sqrt(h1)->vol1
    sqrt(h2)->vol2

    wreg<-cbind.data.frame(df,wdf,vol1,vol2)

    print("estimating weighted glms...")

    glm(wspecm[[1]], family = gaussian, wreg, weights=s1)->Ym1
    glm(wspecm[[2]], family = gaussian, wreg, weights=s2)->Ym2


    list(glm1=Ym1,glm2=Ym2)->glms

    print("estimating MSM...")

    repeat  {

      mYc=msmFit(specm,k=k,p=p,sw=c(rep(T,npar)),df,control=list(parallel=T))


      AIC(mYc)->aic
      print(aic[1])
      if (aic[1] < Aic) {
        break
      }
    }



    #mY0@model$fitted.values->fitmeanY0
    mYc@model$fitted.values->fitmeanYc
    mYc@model$residuals->resYc
    #mY0@model$residuals->resY0

    #list(mY0,mYc)->mY
    list(mYc)->mY
    #cbind.data.frame(fitmeanY0,resY0,htY0[1:length(fitmeanY0)])->fitplot1
    cbind.data.frame(fitmeanYc,resYc,volY[(1+p):length(volY)])->fitplot2




    #plot(x = as.zoo(fitplot1), ylab = "Diagnostics mean", main = "sqrt(h(t))", col = rainbow(ncol(fitplot1)), screens = 1)
    #legend(x = "bottomleft", legend = c(colnames(fitplot1)), lwd=0.1,lty = 2,col = rainbow(ncol(fitplot1)))

    par(mfrow=c(1,1))
    plot(x = as.zoo(fitplot2), ylab = "Diagnostics mean", main = "Conditional Mean+variance", col = rainbow(ncol(fitplot2)), screens = 1)
    legend(x = "bottomleft", legend = c("Fitted Mean Y","Residuals Y","Fitted Garch"), lwd=0.1,lty = 2,col = rainbow(ncol(fitplot2)))

    fitplot2->graphic.diags
    #c(mY0@Fit@logLikel,mYc@Fit@logLikel)->llY

    #plotDiag(mY[[which(llY==min(llY))]])
    plotDiag(mY[[1]])
    #mY[[which(llY==min(llY))]]@Fit@filtProb->filtpmeanY
    mY[[1]]@Fit@filtProb->filtpmeanY

    print("mapping states...")

    foreach(i=1:nrow(filtpmeanY),.combine='c') %do% max(filtpmeanY[i,])->statesmean
    statesY<-c(rep(0,nrow(filtpmeanY)))
    1->statesY[which((statesmean==filtpmeanY[,1]))]
    2->statesY[which((statesmean==filtpmeanY[,2]))]

    par(mfrow=c(1,1))
    as.zoo(cbind(fitmeanYc[1:length(statesY)],Y[1:length(statesY)],statesY*0.01),datest)->statesvsY
    rainbow(ncol(statesvsY))->colori
    plot(x = statesvsY[], ylab = "States", main = "Fit vs log-returns", col = colori,type='l', lwd=c(2,1.6,1.6),screens = 1)
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "bottomleft", legend = c("fitted mean","logreturns","regimes map"), lwd=0.1,lty = 2,col = colori)

    print(summary(Ym1))
    print(summary(Ym2))

    print("%regime1")
    print(length(which(statesY==1))/length(Y))
    print("%regime2")
    print(length(which(statesY==2))/length(Y))


    return(list(summary(mYc),model=mYc,weighted.models=glms, graphic.diags=graphic.diags,states.map=statesY))->output

  }


}
