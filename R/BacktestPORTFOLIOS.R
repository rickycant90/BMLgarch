#' @title <BacktestPORTFOLIOS>
#'
#' @description <Rolling window backtesting of optimized portfolios>
#' @param scenario.set A data.frame object containing all the assets in the market.
#' @param cor.matrices A list of conditional correlation matrices.
#' @param nroll Integer, width of the rolling window.
#' @param q real number, quantile for VaRc estimates.
#' @param shortsell Logic, if TRUE negative proportions are allowed in the optimized portfolios.
#' @param max.weight real number, total capital to invest
#' @param box logical, indicating whether to add or not a box constraint, minbox and maxbox are the quantities associated to that constraint.
#' @param active logical, indicating whether to compose a dollar-neutral (0-sum) portfolio or not.
#' @param full logical, indicating whether to add or not a full-investment constraint.
#' @param poslim logical, indicating whether to add or not a position limit constraint, maxpos is the quantity associated to that constraint.
#' @param diversification logical, indicating whether to add or not a diversification constraint, div is the percentage quantity associated to that constraint.
#' @param turnover logical, indicating whether to add or not a turnover constraint, turn is the quantity associated to that constraint.
#' @param target.return logical, indicating whether to add or not a target return constraint, ret.target is the quantity associated to that constraint.
#' @export



BacktestPORTFOLIOS<-function(scenario.set,cor.matrices=list(),box=TRUE,active=FALSE,full=TRUE,poslim=TRUE,diversification=FALSE,turnover=FALSE,target.return=FALSE,div=0.8,turn=0.2,p=0.05,ret.target=0.05,shortsell=FALSE,max.weight=1,maxrisk=0.3,maxpos=4,minbox=-0.2,maxbox=0.2) {
  #############################################################################
  ##### 1. Setup & Data

  nroll=90
  q=0.025
  library(PortfolioAnalytics)
  library(DEoptim)
  library(ROI)
  require(ROI.plugin.glpk)
  require(ROI.plugin.quadprog)
  suppressWarnings(suppressMessages(invisible(library(fPortfolio))))
  suppressWarnings(suppressMessages(invisible(library(quantmod))))
  suppressWarnings(suppressMessages(invisible(library(PerformanceAnalytics))))
  suppressWarnings(suppressMessages(invisible(library(magrittr))))
  suppressWarnings(suppressMessages(invisible(library(fractalrock))))
  suppressWarnings(suppressMessages(invisible(library(PortfolioAnalytics))))
  suppressWarnings(suppressMessages(invisible(library(ROI))))
  suppressWarnings(suppressMessages(invisible(library(matrixcalc))))
  suppressWarnings(suppressMessages(invisible(library(corpcor))))
  suppressWarnings(suppressMessages(invisible(library(scenportopt))))
  suppressWarnings(suppressMessages(invisible(library(tseries))))




  scenario.set <- as.matrix(scenario.set)

  assets <- ncol(scenario.set)
  scenarios <- nrow(scenario.set)




















  ### MARKOWITZ

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
  portfolioWeights2 <- xts(matrweights,order.by=as.Date(index(matrweights)))


  # Remove leading NA value
  portfolioWeights2 <- portfolioWeights2[complete.cases(portfolioWeights2)]
  # Add column names
  names(portfolioWeights2) <- colnames(returns)

  foreach(i=1:nrow(portfolioWeights2),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2[i,]),8)->Xt
  rowSums(Xt)->portfolio.returns
  # Calculate the portfolio returns of the strategy
  #portfolio.returns <- xts(rowSums(returns[index(portfolioWeights2),]*portfolioWeights2),
  #xts(portfolio.returns,order.by=as.Date(index(portfolio.returns)))->portfolio.returns
  xts(portfolio.returns,order.by=as.Date(index(backtestMARKOWITZ)))->portfolio.returns
  # Add returns and weights to a list


  #############################################################
  ###### PLOTTING                                       #######
  #############################################################

  charts.PerformanceSummary(portfolio.returns,main="Markowitz performance")

















  ##################################PORTFOLIOANALYTICS PART##############################################



  # initialize portfolio
  pspec <- portfolio.spec(assets = colnames(scenario.set))


  pspec<- add.constraint(portfolio = pspec, type = "full_investment")


  if (shortsell==FALSE) {
    pspec <- add.constraint(portfolio = pspec, type = "long_only")
  }
  if (diversification==TRUE) {
    pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=div)
  }
  #turnover
  if (turnover==TRUE) {
    pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=turn)
  }
  #target return
  if (target.return==TRUE) {
    pspec <- add.constraint(portfolio=pspec, type="return", return_target=ret.target)
  }

  # Add box constraint such that no asset can have a weight of greater than
  # 20% or less than -20%
  if (box==TRUE) {
    pspec <- add.constraint(pspec, type="box", min=minbox, max=maxbox)
  }
  if (poslim==TRUE) {
    # Add constraint such that we have at most n positions
    pspec <- add.constraint(pspec, type="position_limit", max_pos=maxpos)
  }
  #if (beta=TRUE) {
  # Add constraint such that the portfolio beta is between -0.25 and 0.25
  #betas <- t(CAPM.beta(scenario.set, market, Rf))
  #pspec <- add.constraint(pspec, type="factor_exposure", B=betas,lower=-0.25, upper=0.25)

  #}



  minSD.portfolio <- add.objective(portfolio=pspec, type="risk", name="StdDev",multiplier=0)
  meanES.portfolio <- add.objective(portfolio=pspec, type="risk", name="ES",multiplier=0)
  RB.portfolio <- add.objective(portfolio=pspec, type="risk_budget", name="ETL", arguments=list(p=0.95), max_prisk=maxrisk,multiplier=0)


  #"full_investment"
  #"long_only"

  ### 2.2. Easy Backtesting

  window <- nroll #3 months rolling window
  pos<-window+1
  backtestSD<-list()
  backtestES<-list()
  backtestRB<-list()

  i<-1

  while (i<=(nrow(scenario.set)-(window+1))) {
    for (pos in (window+1):nrow(scenario.set)) {




      as.xts(scenario.set[(pos-window):pos,],order.by=as.Date(index(scenario.set[(pos-window):pos,])))->tscenario.set
      tminSD <- optimize.portfolio(R = tscenario.set, portfolio = minSD.portfolio, optimize_method = "ROI", trace = TRUE)
      tmeanES <- optimize.portfolio(R = tscenario.set, portfolio = meanES.portfolio, optimize_method = "ROI", trace = TRUE)
      tmeanRB <- optimize.portfolio(R = tscenario.set, portfolio = RB.portfolio, optimize_method = "ROI", trace = TRUE)


      tminSD$weights->backtestSD[[i]]

      tmeanES$weights->backtestES[[i]]

      tmeanRB$weights->backtestRB[[i]]



      i<-i+1

      pos<-pos+1
      print(c(i,pos-window))




    }
  }

  scenario.set->returns
  names(backtestSD)<-index(backtestSD)
  names(backtestES)<-index(backtestES)
  names(backtestRB)<-index(backtestRB)

  backtestSD->portfolioWeightsA
  backtestES->portfolioWeightsB
  backtestRB->portfolioWeightsC

  i<-1
  matrweightsA<-matrix(,length(portfolioWeightsA),ncol(returns))
  matrweightsB<-matrix(,length(portfolioWeightsB),ncol(returns))
  matrweightsC<-matrix(,length(portfolioWeightsC),ncol(returns))


  for (i in 1:length(portfolioWeightsA)) {

    portfolioWeightsA[[i]]->matrweightsA[i,]
    portfolioWeightsB[[i]]->matrweightsB[i,]
    portfolioWeightsB[[i]]->matrweightsC[i,]

  }
  colnames(matrweightsA)<-names(portfolioWeightsA[[1]])
  colnames(matrweightsB)<-names(portfolioWeightsB[[1]])
  colnames(matrweightsC)<-names(portfolioWeightsC[[1]])


  # Convert to xts object
  portfolioWeights2a <- xts(matrweightsA,order.by=as.Date(index(matrweightsA)))
  portfolioWeights2b <- xts(matrweightsB,order.by=as.Date(index(matrweightsB)))
  portfolioWeights2c <- xts(matrweightsC,order.by=as.Date(index(matrweightsC)))

  # Remove leading NA value
  portfolioWeights2a <- portfolioWeights2b[complete.cases(portfolioWeights2a)]
  portfolioWeights2b <- portfolioWeights2b[complete.cases(portfolioWeights2b)]
  portfolioWeights2c <- portfolioWeights2c[complete.cases(portfolioWeights2c)]

  # Add column names
  names(portfolioWeights2a) <- colnames(returns)
  names(portfolioWeights2b) <- colnames(returns)
  names(portfolioWeights2c) <- colnames(returns)

  foreach(i=1:nrow(portfolioWeights2a),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2a[i,]),8)->Xta
  rowSums(Xta)->portfolio.returns1

  foreach(i=1:nrow(portfolioWeights2b),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2b[i,]),8)->Xtb
  rowSums(Xtb)->portfolio.returns2

  foreach(i=1:nrow(portfolioWeights2c),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2c[i,]),8)->Xtc
  rowSums(Xtc)->portfolio.returns3
  # Calculate the portfolio returns of the strategy
  #portfolio.returns <- xts(rowSums(returns[index(portfolioWeights2),]*portfolioWeights2),
  #xts(portfolio.returns,order.by=as.Date(index(portfolio.returns)))->portfolio.returns
  xts(portfolio.returns1,order.by=as.Date(index(portfolio.returns1)))->portfolio.returnsSD
  xts(portfolio.returns2,order.by=as.Date(index(portfolio.returns2)))->portfolio.returnsES
  xts(portfolio.returns3,order.by=as.Date(index(portfolio.returns3)))->portfolio.returnsRB

  # Add returns and weights to a list


  #############################################################
  ###### PLOTTING                                       #######
  #############################################################

  charts.PerformanceSummary(portfolio.returnsSD,main="Minimum SD performance")

  charts.PerformanceSummary(portfolio.returnsES,main="Mean Expected Shortfall performance")

  charts.PerformanceSummary(portfolio.returnsRB,main="Risk budget performance")



  Return.cumulative(portfolio.returnsSD)->cumret1
  print("MinSD portfolio cumulative return at time T")
  print(cumret1)
  Return.cumulative(portfolio.returnsES)->cumret2
  print("ES portfolio cumulative return at time T")
  print(cumret2)
  Return.cumulative(portfolio.returnsRB)->cumret3
  print("RB portfolio cumulative return at time T")








  ######################################SCENPORTOPT######################################

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



      cvar95$portfolio$x->tempport1
      round(tempport1,4)->backtestcVAR[[i]]

      i<-i+1

      pos<-pos+1
      print(c(i,pos-window))




    }
  }

  scenario.set->returns
  names(backtestcVAR)<-index(backtestcVAR)

  backtestcVAR->portfolioWeightsA

  i<-1
  matrweightsA<-matrix(,length(portfolioWeightsA),ncol(returns))


  for (i in 1:length(portfolioWeightsA)) {

    portfolioWeightsA[[i]]->matrweightsA[i,]

  }
  colnames(matrweightsA)<-names(portfolioWeightsA[[1]])


  # Convert to xts object
  portfolioWeights2a <- xts(matrweightsA,order.by=as.Date(index(matrweightsA)))


  # Remove leading NA value
  portfolioWeights2a <- portfolioWeights2b[complete.cases(portfolioWeights2a)]

  # Add column names
  names(portfolioWeights2a) <- colnames(returns)


  foreach(i=1:nrow(portfolioWeights2a),.combine='rbind') %do% round(returns[i,]*as.numeric(portfolioWeights2a[i,]),8)->Xta
  rowSums(Xta)->portfolio.returns1


  # Calculate the portfolio returns of the strategy
  #portfolio.returns <- xts(rowSums(returns[index(portfolioWeights2),]*portfolioWeights2),
  #xts(portfolio.returns,order.by=as.Date(index(portfolio.returns)))->portfolio.returns
  xts(portfolio.returns1,order.by=as.Date(index(portfolio.returns1)))->portfolio.returnscVAR


  # Add returns and weights to a list


  #############################################################
  ###### PLOTTING                                       #######
  #############################################################

  charts.PerformanceSummary(portfolio.returnscVAR,main="VaR performance")









  Return.cumulative(portfolio.returnsSD)->cumret1
  print("MinSD portfolio cumulative return at time T")
  print(cumret1)
  Return.cumulative(portfolio.returnsES)->cumret2
  print("ES portfolio cumulative return at time T")
  print(cumret2)

  Return.cumulative(portfolio.returnsRB)->cumret3
  print("RB portfolio cumulative return at time T")


  Return.cumulative(portfolio.returnscVAR)->cumret5
  print("cVAR portfolio cumulative return at time T")
  print(cumret5)






















  return=list(cumulative.returns=list(minSD=cumret1,ES=cumret2,RB=cumret3,cVAR=cumret5),portfolios=list(minSD.portfolio=minSD.portfolio,meanES.portfolio=meanES.portfolio,RB.portfolio=RB.portfolio),weights=list(xMV=portfolioWeights,xMinSD=backtestSD,xES=backtestES,xRB=backtestRB,xcVAR=backtestcVAR),
              BACKTESTperformance=list(minSD=portfolio.returnsSD,ES=portfolio.returnsES,RB=portfolio.returnsRB,MV=portfolio.returns,cVAR=portfolio.returnscVAR))
}




