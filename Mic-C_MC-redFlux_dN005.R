### main code running base and MC simulations
### Dr. Annette Dathe, Cornell University, June 1st 2026

setwd("C:/Users/ad273/Documents/R-scripts/Mic-C_redFlux")
setwd("C:/Users/Your/Path/Here")

# load libraries
library(deSolve) # library for solving differential equations
library(FME)
library(xlsx)
library(data.table)

pars <- list(muO2=0.09989, kO2C=30.08, rg = 2.522e-1, rm = 3.102E-04, re=0.1005,
             d=3.919E-04, rateEnz=0.02371, rateExu=7.205E-04, rateWaste=7.444E-04,
             fNBac=0.1685, fNEnz=0.2144, fNExu=0.2530, fNWaste=0, BN_0=0.01685)

cinit <- c(C=100, B=0.1, CO2=0, Enz=0, Exu=0, Waste=0, deadB=0, N=5,  
           BN=pars[[14]], Nof=0, NEnz=0, NExu=0, NWaste=0, deadNB=0, rBN=1,
           sumC=100.1, sumN=5+pars[[14]], fEx=0)

rxnrate = function(t, cinit,pars, positive = TRUE){  
  with (as.list(c(cinit, pars)), {
    
    dExu=rateExu*B           # C portion extracellular exudate production
    if (C<=0.5) {            # Exudate flux reduced  
      dExu=rateExu/10*B
    }
    dNExu=fNExu*dExu         # N portion extracellular exudate production
    
    dWaste=rateWaste*B       # C portion extracellular waste production
    # if (dB/B <= -0.001) {  # doesn't run b/c dB is not calculated yet
    if (C<=0.5) {            # Waste flux reduced
      dWaste=rateWaste/10*B
    }
    dNWaste=fNWaste*dWaste   # N portion extracellular waste production
    
    ddeadB=d*B
    ddeadNB=fNBac*ddeadB

      if (N>0) {
      dC=  -muO2 *C/(kO2C+C)*B             # uptake of C into biomass
      dN=0.05*dC             # N/C=0.05: hard coded for now, uptake of N into biomass
      
      dEnz=rateEnz*-dC       # C portion extracellular enzyme production
      dNEnz=fNEnz*dEnz       # N portion extracellular enzyme production  
      
      rBN=fNBac/(BN/B)
      
      if (rBN>=1) {
        dNof=0
        dCO2g=-(rg*dC)
        dCO2m=rm*B
        dCO2e=re*(dEnz+dExu)
        dCO2o=B-BN/fNBac
        dCO2=dCO2g+dCO2m+dCO2e+dCO2o
      } 
      else {
        dNof=BN-fNBac*B
        dCO2g=-(rg*dC)
        dCO2m=rm*B
        dCO2e=re*(dEnz+dExu)
        dCO2=dCO2g+dCO2m+dCO2e
      }
      
      dB=       -dC -d*B       -dCO2   -dEnz       -dExu       -dWaste # dB/dt
      dBN=-dN -fNBac*d*B -dNof   -fNEnz*dEnz -fNExu*dExu -fNWaste*dWaste
    }
    
    else {
      dC=  0*B                 # uptake of C into biomass
      dN=0.05*dC               # doesn't matter because dN gets 0 anyway
 
      dEnz=0
      dNEnz=0
      
      rBN=fNBac/(BN/B)
      
      if (rBN>=1) {
        dNof=0
        dCO2g=-(rg*dC)
        dCO2m=rm*B
        dCO2e=re*(dEnz+dExu)
        dCO2o=B-BN/fNBac
        dCO2=dCO2g+dCO2m+dCO2e+dCO2o
       } 
      else {
        dNof=BN-fNBac*B
        dCO2g=-(rg*dC)
        dCO2m=rm*B
        dCO2e=re*(dEnz+dExu)
        dCO2=dCO2g+dCO2m+dCO2e
       }
      
      dB=                 -d*B -dCO2   -dEnz       -dExu       -dWaste # dB/dt
      dBN=-dN -fNBac*d*B -dNof   -fNEnz*dEnz -fNExu*dExu -fNWaste*dWaste
    }
    
    sumC=C+B+CO2+Enz+Exu+Waste+deadB
    sumN=N+BN+Nof+NEnz+NExu+NWaste+deadNB
    fEx=ddeadB/(dEnz+dExu+dWaste+ddeadB)*100
    
    return(list(c(dC,dB,dCO2,dEnz,dExu,dWaste,ddeadB,
                  dN,dBN,dNof,dNEnz,dNExu,dNWaste,ddeadNB,rBN,
                  sumC,sumN,fEx)))
  })
}  

# create a timeline
t=seq(0,72*24,by=1)

bactModel=function(pars,t){
  
  P<-pars
  P["muO2"]  <-pars[1]
  P["kO2C"]  <-pars[2]
  P["rg"]    <-pars[3]
  P["rm"]    <-pars[4]
  P["re"]    <-pars[5]
  P["d"]     <-pars[6]
  P["rateEnz"]     <-pars[7]
  P["rateExu"]     <-pars[8]
  P["rateWaste"]   <-pars[9]
  P["fNBac"]  <-pars[10]
  P["fNEnz"]  <-pars[11]
  P["fNExu"]  <-pars[12]
  P["fNWaste"]<-pars[13]
  P["BN_0"]   <-pars[14]

  ## ode solves the model by integration ...
  return(as.data.frame(ode(y = cinit, times = t, func = rxnrate,
                           parms=P)))
  }

out <- bactModel(as.numeric(pars),t)

outdf <- (out[,c(1:15)])
df_fEx<-diff(out$fEx)     # this writes fEx between two timesteps
df_rBN<-diff(out$rBN)
df_Nof<-diff(out$Nof)
df_sumC <- diff(out$sumC)
df_sumN <- diff(out$sumN)
NC_Bac <- (out$BN/out$B)
CUE<- diff(out$B)/-diff(out$C)

outdf$df_sumC <- c(0,t(df_sumC))
outdf$df_sumN <- c(0,t(df_sumN))
outdf$df_fEx <- c(t(out$deadB/(out$Enz+out$Exu+out$Waste+out$deadB))*100)
outdf$df_rBN <- c(0,t(df_rBN))
outdf$df_Nof <- c(0,t(df_Nof))
outdf$NC_Bac <- c(t(NC_Bac))
outdf$CUE <- c(0,t(CUE))

# write.xlsx(outdf, "outdf.xlsx")
head(outdf)
tail(outdf)

# source("graphs_base-sim_days.R")
source("graphs_base-sim.R")

### estimate max biomass and write out pNecro for respective hours
maxB=max(outdf$B)
rowN <- which(outdf$B == maxB)
hourMaxB = rowN -1   # because time starts with 0

pNecroMB = outdf[rowN,"deadB"]/
  (outdf[rowN,"Enz"]+outdf[rowN,"Exu"]+outdf[rowN,"Waste"]+outdf[rowN,"deadB"])*100
pNecroEnd = outdf[1729,"deadB"]/
  (outdf[1729,"Enz"]+outdf[1729,"Exu"]+outdf[1729,"Waste"]+outdf[1729,"deadB"])*100

out_maxB_base <-as.data.frame(cbind(maxB, hourMaxB, pNecroMB, pNecroEnd),
                   col.names=c("maxB", "hourMaxB", "necroMaxB", "necroEnd"))

filename = paste0('necMaxB_redF_dN005.xlsx')
write.xlsx(out_maxB_base,filename)


### now with fEx, rBN and Nof as return 
bactModel2 = function(pars, t = seq(0,72*24,by=1)){

  P <- pars
  P["muO2"]  <-pars[1]
  P["kO2C"]  <-pars[2]
  P["rg"]    <-pars[3]
  P["rm"]    <-pars[4]
  P["re"]    <-pars[5]
  P["d"]     <-pars[6]
  P["rateEnz"]     <-pars[7]
  P["rateExu"]     <-pars[8]
  P["rateWaste"]   <-pars[9]
  P["fNBac"]  <-pars[10]
  P["fNEnz"]  <-pars[11]
  P["fNExu"]  <-pars[12]
  P["fNWaste"]<-pars[13]
  P["BN_0"]   <-pars[14]

  res = (as.data.frame(ode(y = cinit, times = t, func = rxnrate, parms=P)))
  return(list(rBN = tail(diff(res$rBN),1), Nof = tail(diff(res$Nof),1), 
         Enz=tail(res$Enz,1), Exu=tail(res$Exu,1), Waste=tail(res$Waste,1),
         fEx = tail(diff(res$fEx),1), necro=tail(res$deadB,1), end=tail(res$time,1)))
}

out2 <- bactModel2(as.numeric(pars), t)

# runs the MC simulations and gives values for fEx for each set of parameters 
load("parh.df")
parh.df[, 15:22] = rbindlist(apply(parh.df, 1 , function(x) bactModel2(t(x), 
                                 t=seq(0,72*24,by=1))))
saveRDS(parh.df, file = "parhdf2_redFlux_dN005.RData")

### for MC output over time
bactModel3 = function(pars,  t=seq(0,72*24,by=1)) {

  P <- pars
  P["muO2"]  <-pars[1]
  P["kO2C"]  <-pars[2]
  P["rg"]    <-pars[3]
  P["rm"]    <-pars[4]
  P["re"]    <-pars[5]
  P["d"]     <-pars[6]
  P["rateEnz"]     <-pars[7]
  P["rateExu"]     <-pars[8]
  P["rateWaste"]   <-pars[9]
  P["fNBac"]  <-pars[10]
  P["fNEnz"]  <-pars[11]
  P["fNExu"]  <-pars[12]
  P["fNWaste"]<-pars[13]
  P["BN_0"]   <-pars[14]
  
  res = (as.data.frame(ode(y = cinit, times = t, func = rxnrate, parms=P)))
  return(list(res$C, res$B, res$CO2, res$Enz, res$Exu, res$Waste, res$deadB,res$fEx,
              res$N, res$BN, res$Nof, res$NEnz, res$NExu, res$NWaste, res$deadNB))
}

out3 <- bactModel3(as.numeric(pars), t)
# FEx is integrated, we need the differences!
df_fEx<-diff(out3[[8]])

# load the parameter distribution
load("parh.df")

# run this for full output
Fout_1728hours <- as.array(apply(parh.df, 1 , function(x) bactModel3(t(x), 
                            t=seq(0,72*24,by=1))))
saveRDS(Fout_1728hours, file = "fout_redFlux_dN005.RData")


# this creates mean, min, max, and sd of fEx and OC pools
out_fEx<-c(mean(parh.df3$fEx),(min(parh.df3$fEx)),(max(parh.df3$fEx)),(sd(parh.df3$fEx))) 
out_Enz<-c(mean(parh.df3$Enz),(min(parh.df3$Enz)),(max(parh.df3$Enz)),(sd(parh.df3$Enz)))
out_Exu<-c(mean(parh.df3$Exu),(min(parh.df3$Exu)),(max(parh.df3$Exu)),(sd(parh.df3$Exu)))
out_Waste<-c(mean(parh.df3$Waste),(min(parh.df3$Waste)),(max(parh.df3$Waste)),(sd(parh.df3$Waste)))
out_necro<-c(mean(parh.df3$necro),(min(parh.df3$necro)),(max(parh.df3$necro)),(sd(parh.df3$necro)))
minmax_df3 <-as.data.frame((cbind(out_fEx,out_Enz,out_Exu,out_Waste,out_necro)),
                           row.names=c("mean","min","max","sd"))

write.xlsx(minmax_df3,"minmax_df3h.xlsx")
