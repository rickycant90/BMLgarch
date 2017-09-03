#' @title <yuimaCOGARCH>
#'
#' @description <estimation and simulation of a COGARCH(1,1) model on input price data>
#' @param X
#' @param p
#' @param q
#' @param term
#' @param num
#' @param gamma
#' @export



yuimaCOGARCH<-function(X,p=1,q=1,term=1600,num=24000,gamma=TRUE) {

library(yuima)


setData(X)->datacogarch
mod.cp <- setCogarch(p = p, q = q, work = FALSE,measure=list(intensity="lambda",
df=list("dnorm(z,eta,sig2)")),measure.type = "CP", Cogarch.var = "g", V.var = "v", Latent.var="x", XinExpr=TRUE)

param.cp <- list(a1 = 0.03, b1 = 0.05, a0 = 0.04/0.05,lambda = 1, eta=0, sig2=1, x01 = X[length(X)])
samp.cp <- setSampling(Terminal=Term, n=num)
sim.cp <- simulate(mod.cp, true.parameter=param.cp, sampling=samp.cp, method="euler")

print("Parameters estimation 1...")
res.cp <- gmm(sim.cp,datacogarch, start=param.cp, objFun = "L2", Est.Incr = "IncrPar",aggregation=FALSE)

par(mfrow=c(2,1))
plot(res.cp, main = "Compound Poisson Increments of the COGARCH model")
plot(X,type="l")
print("Simulation 1...")
traj.cp<- simulate(res.cp)

res.cp@coef->param.cpt
c(param.cpt,x01=X[length(X)])->param.cpt
as.list(param.cpt)->param.cpt
sim.cpt <- simulate(mod.cp, true.parameter=param.cpt, sampling=samp.cp, method="euler")
plot(sim.cpt, main = "simulated COGARCH(1,1) model driven by a Compound Poisson process")
res.cp <- gmm(sim.cpt,datacogarch, start=param.cpt, objFun = "L2", Est.Incr = "IncrPar",aggregation=FALSE)
print(summary(res.cp))
traj.cp<- simulate(res.cp)
plot(traj.cp, main = "estimated COGARCH(1,1) driven by compound poisson process")

  param.VG <- list(a1 = 0.06,  b1 =  0.1, a0 = 0.06/0.05,lambda = 1, alpha = sqrt(2), beta = 0, mu = 0,
                   x01 = X[length(X)])

  cog.VG <- setCogarch(p = 1, q = 1, work = FALSE,
                       measure=list(df="rvgamma(z, lambda, alpha, beta, mu)"),
                       measure.type = "code",  Cogarch.var = "y", V.var = "v", Latent.var="x",XinExpr=TRUE)


  samp.VG <- setSampling(Terminal=Term, n=num)
  sim.VG <- simulate(cog.VG, true.parameter=param.VG, sampling=samp.VG, method="euler")
  plot(sim.VG, main = "simulated COGARCH(1,1) model driven by a Variance Gamma process")
  print("Parameters estimation 2...")
  res.VG <- gmm(sim.VG,datacogarch, start=param.VG, objFun = "L2", Est.Incr = "IncrPar",aggregation=FALSE)

  par(mfrow=c(2,1))
  plot(res.VG, main = "Variance Gamma Increments of the COGARCH model")
  plot(X,type="l")
  print("Simulation 2...")
  traj.VG<- simulate(res.VG)



  plot(traj.VG, main = "estimated COGARCH(1,1) driven by Variance Gamma process")
  print(summary(res.VG))
  return=list(cogarchPOI=res.cp,cogarchGAM=res.VG,sim.cp=sim.cp,sim.VG=sim.VG)


}
