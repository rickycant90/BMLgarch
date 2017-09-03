
allspecs<-function(garchmodels=c("sGARCH",   "eGARCH",   "gjrGARCH", "GAS"),distributions=c("norm", "std",  "ged")) {
  
  
  
  
  
  
  suppressWarnings(suppressMessages(invisible(library("MSwM"))))
  suppressWarnings(suppressMessages(invisible(library("gtools"))))
  suppressWarnings(suppressMessages(invisible(library("foreach"))))
  suppressWarnings(suppressMessages(invisible(library("xts"))))
  suppressWarnings(suppressMessages(invisible(library("MSGARCH"))))
  gtools::combinations(length(garchmodels),3,garchmodels,repeats=T)->tofit3
  gtools::combinations(length(distributions),3,distributions,repeats=T)->todist3
  
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[1,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[2,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[3,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[4,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist4A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[5,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist5A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[6,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist6A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[7,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist7A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[8,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist8A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[9,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist9A
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[10,]), do.skew = c(T,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist10A
  
  
  list(MODELSdist1A, MODELSdist2A, MODELSdist3A, MODELSdist4A, MODELSdist5A, MODELSdist6A, MODELSdist7A, MODELSdist8A, MODELSdist9A, MODELSdist10A)->msg31
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[1,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[2,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[3,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[4,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist4B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[5,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist5B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[6,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist6B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[7,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist7B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[8,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist8B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[9,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist9B
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[10,]), do.skew = c(F,T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist10B
  
  
  list(MODELSdist1B, MODELSdist2B, MODELSdist3B, MODELSdist4B, MODELSdist5B, MODELSdist6B, MODELSdist7B, MODELSdist8B, MODELSdist9B, MODELSdist10B)->msg32
  
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[1,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[2,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[3,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[4,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist4C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[5,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist5C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[6,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist6C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[7,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist7C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[8,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist8C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[9,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist9C
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[10,]), do.skew = c(F,F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist10C
  
  
  list(MODELSdist1C, MODELSdist2C, MODELSdist3C, MODELSdist4C, MODELSdist5C, MODELSdist6C, MODELSdist7C, MODELSdist8C, MODELSdist9C, MODELSdist10C)->msg33
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[1,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[2,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[3,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[4,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist4D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[5,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist5D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[6,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist6D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[7,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist7D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[8,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist8D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[9,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist9D
  foreach(i=1:nrow(tofit3)) %do% create.spec(model = c(tofit3[i,]), distribution = c(todist3[10,]), do.skew = c(F,F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist10D
  
  
  list(MODELSdist1D, MODELSdist2D, MODELSdist3D, MODELSdist4D, MODELSdist5D, MODELSdist6D, MODELSdist7D, MODELSdist8D, MODELSdist9D, MODELSdist10D)->msg34
  
  list(msg31,msg32,msg33,msg34)->ALLMODELS3
  
  
  gtools::combinations(length(garchmodels),2,garchmodels,repeats=T)->tofit2
  gtools::combinations(length(distributions),2,distributions,repeats=T)->todist2
  
  
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[1,]), do.skew = c(T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1A
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[2,]), do.skew = c(T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2A
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[3,]), do.skew = c(T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3A
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[4,]), do.skew = c(T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist4A
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[5,]), do.skew = c(T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist5A
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[6,]), do.skew = c(T,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist6A
  
  
  list(MODELSdist1A, MODELSdist2A, MODELSdist3A, MODELSdist4A, MODELSdist5A, MODELSdist6A)->msg21
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[1,]), do.skew = c(F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1B
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[2,]), do.skew = c(F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2B
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[3,]), do.skew = c(F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3B
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[4,]), do.skew = c(F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist4B
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[5,]), do.skew = c(F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist5B
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[6,]), do.skew = c(F,T), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist6B
  
  
  list(MODELSdist1B, MODELSdist2B, MODELSdist3B, MODELSdist4B, MODELSdist5B, MODELSdist6B)->msg22
  
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[1,]), do.skew = c(T,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1C
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[2,]), do.skew = c(T,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2C
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[3,]), do.skew = c(T,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3C
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[4,]), do.skew = c(T,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist4C
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[5,]), do.skew = c(T,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist5C
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[6,]), do.skew = c(T,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist6C
  
  
  list(MODELSdist1C, MODELSdist2C, MODELSdist3C, MODELSdist4C, MODELSdist5C, MODELSdist6C)->msg23
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[1,]), do.skew = c(F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1D
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[2,]), do.skew = c(F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2D
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[3,]), do.skew = c(F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3D
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[4,]), do.skew = c(F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist4D
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[5,]), do.skew = c(F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist5D
  foreach(i=1:nrow(tofit2)) %do% create.spec(model = c(tofit2[i,]), distribution = c(todist2[6,]), do.skew = c(F,F), do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist6D
  
  
  list(MODELSdist1D, MODELSdist2D, MODELSdist3D, MODELSdist4D, MODELSdist5D, MODELSdist6D)->msg24
  
  list(msg21,msg22,msg23,msg24)->ALLMODELS2
  
  gtools::combinations(length(garchmodels),1,garchmodels,repeats=T)->tofit1
  gtools::combinations(length(distributions),1,distributions,repeats=T)->todist1
  
  
  
  foreach(i=1:nrow(tofit1)) %do% create.spec(model = c(tofit1[i,]), distribution = c(todist1[1,]), do.skew = T, do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1A
  foreach(i=1:nrow(tofit1)) %do% create.spec(model = c(tofit1[i,]), distribution = c(todist1[2,]), do.skew = T, do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2A
  foreach(i=1:nrow(tofit1)) %do% create.spec(model = c(tofit1[i,]), distribution = c(todist1[3,]), do.skew = T, do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3A
  
  
  
  list(MODELSdist1A, MODELSdist2A, MODELSdist3A)->msg11
  
  foreach(i=1:nrow(tofit1)) %do% create.spec(model = c(tofit1[i,]), distribution = c(todist1[1,]), do.skew = F, do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist1B
  foreach(i=1:nrow(tofit1)) %do% create.spec(model = c(tofit1[i,]), distribution = c(todist1[2,]), do.skew = F, do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist2B
  foreach(i=1:nrow(tofit1)) %do% create.spec(model = c(tofit1[i,]), distribution = c(todist1[3,]), do.skew = F, do.mix = FALSE, do.shape.ind = FALSE)->MODELSdist3B
  
  
  list(MODELSdist1B, MODELSdist2B, MODELSdist3B)->msg12
  
  list(msg11,msg12)->ALLMODELS1
  
  suppressMessages(invisible(rm(i,MODELSdist1D, MODELSdist2D, MODELSdist3D, MODELSdist4D, MODELSdist5D, MODELSdist6D, MODELSdist7D, MODELSdist8D, MODELSdist9D, MODELSdist10D,
                                MODELSdist1C, MODELSdist2C, MODELSdist3C, MODELSdist4C, MODELSdist5C, MODELSdist6C, MODELSdist7C, MODELSdist8C, MODELSdist9C, MODELSdist10C,
                                MODELSdist1B, MODELSdist2B, MODELSdist3B, MODELSdist4B, MODELSdist5B, MODELSdist6B, MODELSdist7B, MODELSdist8B, MODELSdist9B, MODELSdist10B,
                                MODELSdist1A, MODELSdist2A, MODELSdist3A, MODELSdist4A, MODELSdist5A, MODELSdist6A, MODELSdist7A, MODELSdist8A, MODELSdist9A, MODELSdist10A,msg31,msg32,msg33,msg34,
                                msg21,msg22,msg23,msg24,msg11,msg12)))
  
  return(list(ALLMODELS3=ALLMODELS3,fits3=tofit3,dists3=todist3,ALLMODELS2=ALLMODELS2,fits2=tofit2,dists2=todist2,ALLMODELS1=ALLMODELS1,fits1=tofit1,dists1=todist1))->outt
  suppressMessages(attach(outt))
}
