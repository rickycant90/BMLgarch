DynamicCOR<-function(htfactors,rotated) {

library(foreach)

  rotated->Z
  Z%*%t(Z)->unc.corr

  foreach(i=1:ncol(htfactors),.combine='cbind') %do% c(scale(htfactors[,i]))->stand.variances
foreach(i=1:nrow(stand.variances)) %do% diag(stand.variances[i,])->diags

i<-1
conditional.correlation<-list(rep(0,length(diags)))

for (i in 1:length(diags)) {

as.matrix(diags[[i]])->matrt
Z%*%matrt%*%t(Z)->conditional.correlation[[i]]
cor(conditional.correlation[[i]])->conditional.correlation[[i]]
}

return=list(CORt=conditional.correlation,uncond.correlation=unc.corr)
}

