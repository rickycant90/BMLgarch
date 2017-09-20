#' @title <BacktestPORTFOLIOS>
#'
#' @description <Rolling window backtesting of optimized portfolios>
#' @param scenario.set A data.frame object containing all the assets in the market.
#' @param cor.matrices A list of conditional correlation matrices.
#' @param nroll Integer, width of the rolling window.
#' @param q real number, quantile for VaRc estimates.
#' @param shortsell Logic, if TRUE negative proportions are allowed in the optimized portfolios.
#' @param max.weight real number, total capital to invest
#' @export



BacktestPORTFOLIOS<-function(scenario.set,cor.matrices=list(),nroll=90,q=0.025,shortsell=FALSE,max.weight=1) {
  #############################################################################
  ##### 1. Setup & Data

  library(fPortfolio)
  library(quantmod)
  library(PerformanceAnalytics)
  library(magrittr)
  library(fractalrock)
  library(PortfolioAnalytics)
  library(ROI)
  library(matrixcalc)
  library(corpcor)
  library(scenportopt)
  library(tseries)
  library(zoo)
  scenario.set <- as.matrix(scenario.set)

  assets <- ncol(scenario.set)
  scenarios <- nrow(scenario.set)




















  ### 2.2. Easy Backtesting

  window <- nroll #3 months rolling window
  pos<-window+1
  backtestMARKOWITZ<-list()
  i<-1
  end.a <- FALSE
  while (i<=(nrow(scenario.set)-(window+1))) {
    for (pos in (window+1):nrow(scenario.set)) {


      if (is.positive.definite(cor.matrices[[window]])==FALSE) {

        make.positive.definite(cor.matrices[[window]])->cor.matrices[[window]]
      }
      tempport<-portfolio.optim(scenario.set[(pos-window):pos,],covmat=cor.matrices[[window]],shorts=shortsell)


      tempport$pw->tempport
      names(tempport)<-colnames(scenario.set)
      round(tempport,4)->backtestMARKOWITZ[[i]]
      i<-i+1

      pos<-pos+1
      print(c(i,pos-window))




    }
  }

  scenario.set->returns
  names(backtestMARKOWITZ)<-index(backtestMARKOWITZ)
  backtestMARKOWITZ->portfolioWeights
  i<-1
  matrweights<-matrix(,length(portfolioWeights),ncol(returns))

  for (i in 1:length(portfolioWeights)) {

    portfolioWeights[[i]]->matrweights[i,]

  }
  colnames(matrweights)<-names(portfolioWeights[[1]])


  # Convert to xts object
  portfolioWeights2 <- xts(matrweights,order.by=zoo::as.Date(index(matrweights)))


  # Remove leading NA value
  portfolioWeights2 <- portfolioWeights2[complete.cases(portfolioWeights2)]
  # Add column names
  names(portfolioWeights2) <- colnames(returns)

  foreach(i=1:nrow(portfolioWeights2),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2[i,]),8)->Xt
  rowSums(Xt)->portfolio.returns
  # Calculate the portfolio returns of the strategy
  #portfolio.returns <- xts(rowSums(returns[index(portfolioWeights2),]*portfolioWeights2),
  #xts(portfolio.returns,order.by=zoo::as.Date(index(portfolio.returns)))->portfolio.returns
  xts(portfolio.returns,order.by=zoo::as.Date(index(backtestMARKOWITZ)))->portfolio.returns
  # Add returns and weights to a list


  #############################################################
  ###### PLOTTING                                       #######
  #############################################################

  charts.PerformanceSummary(portfolio.returns,main="Markowitz performance")





















  # initialize portfolio
  init.portfolio <- portfolio.spec(assets = colnames(scenario.set))


  init.portfolio <- add.constraint(portfolio = init.portfolio, type = "full_investment")
  init.portfolio <- add.constraint(portfolio = init.portfolio, type = "long_only")
  minSD.portfolio <- add.objective(portfolio=init.portfolio, type="risk", name="StdDev")
  meanES.portfolio <- add.objective(portfolio=init.portfolio, type="risk", name="ES")


  #"full_investment"
  #"long_only"

  ### 2.2. Easy Backtesting

  window <- nroll #3 months rolling window
  pos<-window+1
  backtestSD<-list()
  backtestES<-list()
  i<-1

  while (i<=(nrow(scenario.set)-(window+1))) {
    for (pos in (window+1):nrow(scenario.set)) {




      as.xts(scenario.set[(pos-window):pos,],order.by=zoo::as.Date(index(scenario.set[(pos-window):pos,])))->tscenario.set
      tminSD <- optimize.portfolio(R = tscenario.set, portfolio = minSD.portfolio, optimize_method = "ROI", trace = TRUE)
      tmeanES <- optimize.portfolio(R = tscenario.set, portfolio = meanES.portfolio, optimize_method = "ROI", trace = TRUE)



      tminSD$weights->tempport1
      round(tempport1,4)->backtestSD[[i]]
      tmeanES$weights->tempport2
      round(tempport2,4)->backtestES[[i]]
      i<-i+1

      pos<-pos+1
      print(c(i,pos-window))




    }
  }

  scenario.set->returns
  names(backtestSD)<-index(backtestSD)
  names(backtestES)<-index(backtestES)

  backtestSD->portfolioWeightsA
  backtestES->portfolioWeightsB
  i<-1
  matrweightsA<-matrix(,length(portfolioWeightsA),ncol(returns))
  matrweightsB<-matrix(,length(portfolioWeightsB),ncol(returns))

  for (i in 1:length(portfolioWeightsA)) {

    portfolioWeightsA[[i]]->matrweightsA[i,]
    portfolioWeightsB[[i]]->matrweightsB[i,]
  }
  colnames(matrweightsA)<-names(portfolioWeightsA[[1]])
  colnames(matrweightsB)<-names(portfolioWeightsB[[1]])

  # Convert to xts object
  portfolioWeights2a <- xts(matrweightsA,order.by=zoo::as.Date(index(matrweightsA)))
  portfolioWeights2b <- xts(matrweightsB,order.by=zoo::as.Date(index(matrweightsB)))

  # Remove leading NA value
  portfolioWeights2a <- portfolioWeights2b[complete.cases(portfolioWeights2a)]
  portfolioWeights2b <- portfolioWeights2b[complete.cases(portfolioWeights2b)]
  # Add column names
  names(portfolioWeights2a) <- colnames(returns)
  names(portfolioWeights2b) <- colnames(returns)

  foreach(i=1:nrow(portfolioWeights2a),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2a[i,]),8)->Xta
  rowSums(Xta)->portfolio.returns1

  foreach(i=1:nrow(portfolioWeights2b),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2b[i,]),8)->Xtb
  rowSums(Xtb)->portfolio.returns2
  # Calculate the portfolio returns of the strategy
  #portfolio.returns <- xts(rowSums(returns[index(portfolioWeights2),]*portfolioWeights2),
  #xts(portfolio.returns,order.by=zoo::as.Date(index(portfolio.returns)))->portfolio.returns
  xts(portfolio.returns1,order.by=zoo::as.Date(index(portfolio.returns1)))->portfolio.returnsSD
  xts(portfolio.returns2,order.by=zoo::as.Date(index(portfolio.returns2)))->portfolio.returnsES
  # Add returns and weights to a list


  #############################################################
  ###### PLOTTING                                       #######
  #############################################################

  charts.PerformanceSummary(portfolio.returnsSD,main="Minimum SD performance")

  charts.PerformanceSummary(portfolio.returnsES,main="Mean Expected Shortfall performance")
















  ### 2.2. Easy Backtesting

  window <- nroll #3 months rolling window
  pos<-window+1
  backtestcVAR<-list()
  backtestMAD<-list()

  i<-1


  while (i<=(nrow(scenario.set)-(window+1))) {
    for (pos in (window+1):nrow(scenario.set)) {




      ### 6.1. Comparison of risk measures
      scenario.set[(pos-window):pos,]->tscenario.set
      modelport <- portfolio.model(tscenario.set)
      #optimal.portfolio(modelport)
      cvar95 <- optimal.portfolio(alpha(modelport, q))
      mad <- optimal.portfolio(objective(modelport, "mad"))



      cvar95$portfolio$x->tempport1
      round(tempport1,4)->backtestcVAR[[i]]
      mad$portfolio$x->tempport2
      round(tempport2,4)->backtestMAD[[i]]
      i<-i+1

      pos<-pos+1
      print(c(i,pos-window))




    }
  }

  scenario.set->returns
  names(backtestcVAR)<-index(backtestcVAR)
  names(backtestMAD)<-index(backtestMAD)

  backtestcVAR->portfolioWeightsA
  backtestMAD->portfolioWeightsB
  i<-1
  matrweightsA<-matrix(,length(portfolioWeightsA),ncol(returns))
  matrweightsB<-matrix(,length(portfolioWeightsB),ncol(returns))

  for (i in 1:length(portfolioWeightsA)) {

    portfolioWeightsA[[i]]->matrweightsA[i,]
    portfolioWeightsB[[i]]->matrweightsB[i,]
  }
  colnames(matrweightsA)<-names(portfolioWeightsA[[1]])
  colnames(matrweightsB)<-names(portfolioWeightsB[[1]])

  # Convert to xts object
  portfolioWeights2a <- xts(matrweightsA,order.by=zoo::as.Date(index(matrweightsA)))
  portfolioWeights2b <- xts(matrweightsB,order.by=zoo::as.Date(index(matrweightsB)))

  # Remove leading NA value
  portfolioWeights2a <- portfolioWeights2b[complete.cases(portfolioWeights2a)]
  portfolioWeights2b <- portfolioWeights2b[complete.cases(portfolioWeights2b)]
  # Add column names
  names(portfolioWeights2a) <- colnames(returns)
  names(portfolioWeights2b) <- colnames(returns)

  foreach(i=1:nrow(portfolioWeights2a),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2a[i,]),8)->Xta
  rowSums(Xta)->portfolio.returns1

  foreach(i=1:nrow(portfolioWeights2b),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2b[i,]),8)->Xtb
  rowSums(Xtb)->portfolio.returns2
  # Calculate the portfolio returns of the strategy
  #portfolio.returns <- xts(rowSums(returns[index(portfolioWeights2),]*portfolioWeights2),
  #xts(portfolio.returns,order.by=zoo::as.Date(index(portfolio.returns)))->portfolio.returns
  xts(portfolio.returns1,order.by=zoo::as.Date(index(portfolio.returns1)))->portfolio.returnscVAR

  xts(portfolio.returns2,order.by=zoo::as.Date(index(portfolio.returns2)))->portfolio.returnsMAD
  # Add returns and weights to a list


  #############################################################
  ###### PLOTTING                                       #######
  #############################################################

  charts.PerformanceSummary(portfolio.returnscVAR,main="VaR performance")

  charts.PerformanceSummary(portfolio.returnsMAD,main="Mean Absolute Deviation performance")










  nrAssets <- ncol(returns)
  library(quadprog)
  library(rrcov)
  library(matrixcalc)
  library(corpcor)

  i<-1
  j<-1
  portfoliot<-list()

  while (i <= nrow(returns)) {
    for (j in 1:length(cor.matrices))
    {

      as.matrix(cor.matrices[[j]])->matrt
      colnames(matrt)<-colnames(returns)
      rownames(matrt)<-colnames(returns)
      nrMarginals <- ncol(matrt)
      # no linear components in target function
      dvec <- rep.int(0, nrMarginals)

      # Fully Invested (equality constraint)
      a <- rep.int(1, nrMarginals)
      b <- 1
      longOnly<-shortsell
      if(longOnly==TRUE)
      {
        # Long only (>= constraint)
        a2 <- diag(nrMarginals)
        b2 <- rep.int(0, nrMarginals)

        a <- rbind(a, a2)
        b <- c(b, b2)
      }

      # Weights greater than -max.weight
      a3 <- diag(nrMarginals)
      b3 <- rep.int(-max.weight, nrMarginals)

      # Weights smaller than max.weight
      a4 <- -diag(nrMarginals)
      b4 <- rep.int(-max.weight, nrMarginals)

      Amat <- t(rbind(a, a3, a4)) # This matrix will be transposed again
      bvec <- c(b, b3, b4)

      if (is.positive.definite(matrt)==FALSE) {

        make.positive.definite(matrt)->matrt
      }
      colnames(matrt)<-colnames(returns)
      rownames(matrt)<-colnames(returns)
      # Solve the quadratic problem
      gmv <- solve.QP(Dmat=matrt, dvec=dvec, Amat=Amat,
                      bvec=bvec, meq=1) # meq = 1 equality constraint


      gmv$solution->portfoliot[[i]]

      names(portfoliot[[i]])<-colnames(matrt)

      print(c(i,j))

      i<-i+1

    }

  }
  names(portfoliot)<-index(portfoliot)


  portfoliot->portfolioWeights

  library(fractalrock)



  #############################################################
  ###### POSTPROCESSING of Results                      #######
  #############################################################

  i<-1
  matrweights<-matrix(,length(portfolioWeights),ncol(returns))

  for (i in 1:length(portfolioWeights)) {

    portfolioWeights[[i]]->matrweights[i,]

  }
  colnames(matrweights)<-names(portfolioWeights[[1]])


  # Convert to xts object
  portfolioWeights2 <- xts(matrweights,order.by=zoo::as.Date(index(matrweights)))


  # Remove leading NA value
  portfolioWeights2 <- portfolioWeights2[complete.cases(portfolioWeights2)]
  # Add column names
  names(portfolioWeights2) <- names(returns)

  foreach(i=1:nrow(portfolioWeights2),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2[i,]),8)->Xt
  rowSums(Xt)->portfolio.returns
  # Calculate the portfolio returns of the strategy
  #portfolio.returns <- xts(rowSums(returns[index(portfolioWeights2),]*portfolioWeights2),
  #xts(portfolio.returns,order.by=zoo::as.Date(index(portfolio.returns)))->portfolio.returns
  xts(portfolio.returns,order.by=zoo::as.Date(index(portfolio.returns)))->portfolio.returnsGMV
  # Add returns and weights to a list
  portfolioWeights2->xGMV

  #############################################################
  ###### PLOTTING                                       #######
  #############################################################

  charts.PerformanceSummary(portfolio.returnsGMV,main="Global Minimum Variance performance")































  return=list(portfolios=list(xMV=portfolioWeights,xMinSD=backtestSD,xES=backtestES,xcVAR=backtestcVAR,xMAD=backtestMAD,xGMV=xGMV),
              BACKTESTperformance=list(minSD=portfolio.returnsSD,ES=portfolio.returnsES,MV=portfolio.returns,cVAR=portfolio.returnscVAR,MAD=portfolio.returnsMAD,GMV=portfolio.returnsGMV))
}
