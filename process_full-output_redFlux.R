### analysis of MC simulations
### Dr. Annette Dathe, Cornell University, June 1st 2026

setwd("C:/Users/Your/Path/Here")

library(xlsx)

x = readRDS('fout_redFlux_dN005.RData')
# convert to an array, with dimensions (time, variable, iteration)
z = array(unlist(x),dim=c(
  length(x[[1]][[1]]),
  length(x[[1]]),
  length(x)
  ))
# dim(z)
# [1]    1729    15 10000

names = c("C", "B", "CO2", "Enz", "Exu", "Waste", "deadB",
          "sumfEx", "N", "BN", "Nof", "NEnz", "NExu", "NWaste", "deadNB")

# Keep in mind that the output starts at 0, always add one hour!
hour = 1729

# Now plotting pNecro=proportion necromass - integrated up to time chosen
# hour 11+1 is max microbial activity in MC-mean simulation dN=0.05
# hour 329+1 is max biomass in MC-mean simulation dN=0.05
# hour 10+1 is max microbial activity in MC-mean simulation dN=0.1
# hour 175+1 is max biomass in MC-mean simulation dN=0.1

### histograms % necromass - Fig. 3
# calculate pNecro before plotting the histogram!

# for dN = 0.1
pNecro11=z[11,7,]/(z[11,4,]+z[11,5,]+z[11,6,]+z[11,7,])*100
pNecro176=z[176,7,]/(z[176,4,]+z[176,5,]+z[176,6,]+z[176,7,])*100
pNecro1729=z[1729,7,]/(z[1729,4,]+z[1729,5,]+z[1729,6,]+z[1729,7,])*100
# for dN = 0.05
pNecro12=z[12,7,]/(z[12,4,]+z[12,5,]+z[12,6,]+z[12,7,])*100
pNecro330=z[330,7,]/(z[330,4,]+z[330,5,]+z[330,6,]+z[330,7,])*100
pNecro1729=z[1729,7,]/(z[1729,4,]+z[1729,5,]+z[1729,6,]+z[1729,7,])*100

# calculate distinct distributions for consecutive times / histograms 
p1 <- pNecro12
p2 <- (z[330,7,]-z[12,7,]) / ((z[330,4,]-z[12,4,]) + (z[330,5,]-z[12,5,]) 
                           + (z[330,6,]-z[12,6,])   + (z[330,7,]-z[12,7,]))*100
p3 <- (z[1729,7,]-z[330,7,]) / ((z[1729,4,]-z[330,4,]) + (z[1729,5,]-z[330,5,]) 
                           + (z[1729,6,]-z[330,6,]) + (z[1729,7,]-z[330,7,]))*100

# PNAS Fig. 3
# colors below are colorblind-friendly. 
# settings for PNAS Fig. 3 submission were ylim=c(0,1500, by=2.5)
hist(pNecro12, col="#FF000080",xlim=c(0,100), ylim=c(0,1200), main=NULL,
     breaks = seq(from=0, to=100, by=2), cex.lab=1.2, cex.axis=1.2,
     xlab="Necromass (% of microbial organic products)")
hist(pNecro330, col="#00FF0080", xlim=c(0,100),
     breaks = seq(from=0, to=100, by=2),  add=T)
hist(pNecro1729, col="#0000FF80", xlim=c(0,100),
     breaks = seq(from=0, to=100, by=2),  add=T)

# PNAS Fig. S2
# second histogram for distinct phases 1, 2, 3
hist(p1, col="#FF000080",xlim=c(0,100), ylim=c(0,1200), main=NULL,
     breaks = seq(from=0, to=100, by=2), cex.lab=1.2, cex.axis=1.2,
     xlab="Necromass (% of microbial organic products)")
hist(p2, col="#00FF0080", xlim=c(0,100),
     breaks = seq(from=0, to=100, by=2),  add=T)
hist(p3, col="#0000FF80", xlim=c(0,100),
     breaks = seq(from=0, to=100, by=2),  add=T)

# estimating mean, median, min, max, sd, CI95
# save output for cumulative histograms
out_p1<-c(mean(pNecro12),median(pNecro12),min(pNecro12),max(pNecro12),
          sd(pNecro12),quantile((pNecro12),c(.025,.975)))
out_p1n2<-c(mean(pNecro330),median(pNecro330),min(pNecro330),max(pNecro330),
            sd(pNecro330),quantile((pNecro330),c(.025,.975)))
out_p1n2n3<-c(mean(pNecro1729),median(pNecro1729),min(pNecro1729),max(pNecro1729),
              sd(pNecro1729),quantile((pNecro1729),c(.025,.975)))

out_pNec_Cum <- as.data.frame((cbind(out_p1, out_p1n2, out_p1n2n3)),
                               row.names=c("mean","median","min","max","sd",
                                           "CI95l","CI95u"))
filename_p = paste0('3pHisto_Cum_redFlux','_CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(out_pNec_Cum,filename_p)

# estimating mean, median, min, max, sd, CI95
# save output for consecutive histograms
out_p1<-c(mean(p1),median(p1),min(p1),max(p1),sd(p1),quantile((p1),c(.025,.975)))
out_p2<-c(mean(p2),median(p2),min(p2),max(p2),sd(p2),quantile((p2),c(.025,.975)))
out_p3<-c(mean(p3),median(p3),min(p3),max(p3),sd(p3),quantile((p3),c(.025,.975)))

out_pNec_Dist <- as.data.frame((cbind(out_p1, out_p2, out_p3)),
                              row.names=c("mean","median","min","max","sd",
                                          "CI95l","CI95u"))
filename_pd = paste0('3pHisto_Dist_redFlux','_CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(out_pNec_Dist,filename_pd)


### main output processing
# calculating % of Enz, Exu, Waste, and deadB for specific hours/days
# hour = 1729
# hour = 330
# hour = 12

# fEx2 is equal to diff_fEx'hour' from rxn output - d/dt
d_fEx <- diff(z[(hour-1):hour,8,],1)
d_Enz <- diff(z[(hour-1):hour,4,],1)
d_Exu <- diff(z[(hour-1):hour,5,],1)
d_Waste <- diff(z[(hour-1):hour,6,],1)
d_deadB <- diff(z[(hour-1):hour,7,],1)
fEx2=d_deadB/(d_Enz+d_Exu+d_Waste+d_deadB)*100

median(fEx2, na.rm=TRUE)
sum(is.na(fEx2))

# calculating percentages for cumulative C-sinks! 
pEnz=z[hour,4,]/(z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])*100
pExu=z[hour,5,]/(z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])*100
pWaste=z[hour,6,]/(z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])*100
pNecro=z[hour,7,]/(z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])*100

# get median, min and max, 95% CI for hour specified above
out_fEx<-c(mean(fEx2, na.rm=T),median(fEx2, na.rm=T),min(fEx2, na.rm=T),max(fEx2, na.rm=T),
           quantile((fEx2),c(.025,.975), na.rm=T),sd(fEx2, na.rm=T))
out_Enz<-c(mean(pEnz),median(pEnz),min(pEnz),max(pEnz),quantile((pEnz),c(.025,.975)),sd((pEnz)))
out_Exu<-c(mean(pExu),median(pExu),min(pExu),max(pExu),quantile((pExu),c(.025,.975)),sd((pExu)))
out_Waste<-c(mean(pWaste),median(pWaste),min(pWaste),max(pWaste),quantile((pWaste),c(.025,.975)),sd((pWaste)))
out_necro<-c(mean(pNecro),median(pNecro),min(pNecro),max(pNecro),quantile((pNecro),c(.025,.975)),sd((pNecro)))

# estimate C used, microbial, and organic product yields
Cused <- (100-z[hour,1,])
Biom <- z[hour,2,]
OC <- (z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])

# estimate proportions
pOC_Cused <- OC/Cused
pBiom_Cused <- Biom/Cused
out_Cused <- c(mean(Cused),median(Cused),min(Cused),max(Cused),quantile((Cused),c(.025,.975)),sd((Cused)))
out_Biom <- c(mean(Biom),median(Biom),min(Biom),max(Biom),quantile((Biom),c(.025,.975)),sd((Biom)))
out_OC <- c(mean(OC),median(OC),min(OC),max(OC),quantile((OC),c(.025,.975)),sd((OC)))
out_pOC_Cused <- c(mean(pOC_Cused),median(pOC_Cused),min(pOC_Cused),max(pOC_Cused),quantile((pOC_Cused),c(.025,.975)),sd((pOC_Cused)))
out_pBiom_Cused <- c(mean(pBiom_Cused),median(pBiom_Cused),min(pBiom_Cused),max(pBiom_Cused),
                     quantile((pBiom_Cused),c(.025,.975)),sd((pBiom_Cused)))

minmax <-as.data.frame((cbind(out_fEx,out_Enz,out_Exu,out_Waste,out_necro,
                        out_Cused,out_Biom,out_OC,out_pOC_Cused,out_pBiom_Cused)),
                        row.names=c("median","min","max","CI95l","CI95u","sd"))
filename = paste0('minmax_redFlux_', hour, '_CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(minmax,filename)


### estimate CI over all 0-72 days for specific parameters
# PNAS Fig 2

m_C     <- apply(z[1:1729,1,],1,mean)
q_C     <- apply(z[1:1729,1,],1,quantile, probs=c(.025,.975)) 
m_B     <- apply(z[1:1729,2,],1,mean)
q_B     <- apply(z[1:1729,2,],1,quantile, probs=c(.025,.975))
m_CO2   <- apply(z[1:1729,3,],1,mean)
q_CO2   <- apply(z[1:1729,3,],1,quantile, probs=c(.025,.975))
m_Enz   <- apply(z[1:1729,4,],1,mean)
q_Enz   <- apply(z[1:1729,4,],1,quantile, probs=c(.025,.975))
m_Exu   <- apply(z[1:1729,5,],1,mean)
q_Exu   <- apply(z[1:1729,5,],1,quantile, probs=c(.025,.975))
m_Waste <- apply(z[1:1729,6,],1,mean)
q_Waste <- apply(z[1:1729,6,],1,quantile, probs=c(.025,.975))
m_deadB <- apply(z[1:1729,7,],1,mean)
q_deadB <- apply(z[1:1729,7,],1,quantile, probs=c(.025,.975))

q_hourly <- cbind(m_C, q_C[1,], q_C[2,], m_B, q_B[1,], q_B[2,], m_CO2, q_CO2[1,], q_CO2[2,], 
                  m_Enz, q_Enz[1,], q_Enz[2,],m_Exu, q_Exu[1,], q_Exu[2,], 
                  m_Waste, q_Waste[1,], q_Waste[2,], m_deadB, q_deadB[1,], q_deadB[2,])
# write.xlsx(q_hourly,'mean_q_CN20_hourly.xlsx')

filename1 = paste0('meanC_redFlux_', 'CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(q_hourly,filename1)

# obtaining data for PNAS Fig. S2, CI for N 

m_N      <- apply(z[1:1729,9,],1,mean)
q_N      <- apply(z[1:1729,9,],1,quantile, probs=c(.025,.975))
m_BN     <- apply(z[1:1729,10,],1,mean)
q_BN     <- apply(z[1:1729,10,],1,quantile, probs=c(.025,.975))
m_Nof    <- apply(z[1:1729,11,],1,mean)
q_Nof    <- apply(z[1:1729,11,],1,quantile, probs=c(.025,.975))
m_NEnz   <- apply(z[1:1729,12,],1,mean)
q_NEnz   <- apply(z[1:1729,12,],1,quantile, probs=c(.025,.975))
m_NExu   <- apply(z[1:1729,13,],1,mean)
q_NExu   <- apply(z[1:1729,13,],1,quantile, probs=c(.025,.975))
m_NWaste <- apply(z[1:1729,14,],1,mean)
q_NWaste <- apply(z[1:1729,14,],1,quantile, probs=c(.025,.975))
m_deadNB <- apply(z[1:1729,15,],1,mean)
q_deadNB <- apply(z[1:1729,15,],1,quantile, probs=c(.025,.975))

q_Nhourly <- cbind(m_N, q_N[1,], q_N[2,], m_BN, q_BN[1,], q_BN[2,],m_Nof, q_Nof[1,], q_Nof[2,], 
                   m_NEnz, q_NEnz[1,], q_NEnz[2,], m_NExu, q_NExu[1,], q_NExu[2,],
                   m_NWaste, q_NWaste[1,], q_NWaste[2,], m_deadNB, q_deadNB[1,], q_deadNB[2,])
# write.xlsx(q_Nhourly,'meanN_q_CN20_hourly.xlsx')

filename2 = paste0('meanN_redFlux_', 'CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(q_Nhourly,filename2)

hour=1729
### estimate mean, median, min, max, sd of pools at the chosen hour
s_C     <- c(mean(z[hour,1,]),median(z[hour,1,]),min(z[hour,1,]),max(z[hour,1,]),sd(z[hour,1,]),
             quantile((z[hour,1,]),c(.025,.975)), quantile((z[hour,1,]),c(.005,.995)))     
s_B     <- c(mean(z[hour,2,]),median(z[hour,2,]),min(z[hour,2,]),max(z[hour,2,]),sd(z[hour,2,]),
            quantile((z[hour,2,]),c(.025,.975)), quantile((z[hour,2,]),c(.005,.995))) 
s_CO2   <- c(mean(z[hour,3,]),median(z[hour,3,]),min(z[hour,3,]),max(z[hour,3,]),sd(z[hour,3,]),
            quantile((z[hour,3,]),c(.025,.975)), quantile((z[hour,3,]),c(.005,.995)))
s_Enz   <- c(mean(z[hour,4,]),median(z[hour,4,]),min(z[hour,4,]),max(z[hour,4,]),sd(z[hour,4,]),
            quantile((z[hour,4,]),c(.025,.975)), quantile((z[hour,4,]),c(.005,.995)))
s_Exu   <- c(mean(z[hour,5,]),median(z[hour,5,]),min(z[hour,5,]),max(z[hour,5,]),sd(z[hour,5,]),
            quantile((z[hour,5,]),c(.025,.975)), quantile((z[hour,5,]),c(.005,.995)))
s_Waste <- c(mean(z[hour,6,]),median(z[hour,6,]),min(z[hour,6,]),max(z[hour,6,]),sd(z[hour,6,]),
            quantile((z[hour,6,]),c(.025,.975)), quantile((z[hour,6,]),c(.005,.995)))
s_deadB <- c(mean(z[hour,7,]),median(z[hour,7,]),min(z[hour,7,]),max(z[hour,7,]),sd(z[hour,7,]),
            quantile((z[hour,7,]),c(.025,.975)), quantile((z[hour,7,]),c(.005,.995))) 

minmax_pools <- as.data.frame((cbind(s_C, s_B, s_CO2, s_Enz, s_Exu, s_Waste, s_deadB)),
                           row.names=c("mean","median","min","max","sd",
                                       "CI95l","CI95u","CI99l","CI99u"))
filename3 = paste0('minmax_pools_redFlux_', hour, '_CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(minmax_pools,filename3)


### estimate enzyme production as function of biomass at hour 24
hour=25
enz_prod <- c(median(z[hour,4,]/z[hour,2,]),quantile((z[hour,4,]/z[hour,2,]),c(.025,.975)))
write.xlsx(enz_prod,"~/enz-prod_redFlux_24h_dN005.xlsx")

hour=330
excr_prod <- c(median((z[hour,4,]-z[hour-24,4,]+z[hour,5,]-z[hour-24,5,]+z[hour,6,]-z[hour-24,6,])/z[hour,2,]),
            quantile(((z[hour,4,]-z[hour-24,4,]+z[hour,5,]-z[hour-24,5,]+z[hour,6,]-z[hour-24,6,])/z[hour,2,]),c(.025,.975)),
            quantile(((z[hour,4,]-z[hour-24,4,]+z[hour,5,]-z[hour-24,5,]+z[hour,6,]-z[hour-24,6,])/z[hour,2,]),c(.005,.995)))
# write.xlsx(excr_prod,"max-excreta_329_dN005_test.xlsx")
 

### estimate waste production per standing biomass at time of biomass growth
# hour 271 +1 for line, in base simulation 
hour=272 #choose hour! Then take waste +- 6 hours and divide by biomass
waste_biom = c(median((z[hour+6,6,]-z[hour-6,6,])/z[hour,2,]), 
            quantile(((z[hour+6,6,]-z[hour-6,6,])/z[hour,2,]),c(.025,.975)))
filename4 = paste0('waste_biom_CI_', hour, '_CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(waste_biom,filename4)


### estimate % enzymes, exudates, and waste incl. 95% CI
hour = 1729
org_subs_3 = c(median(z[hour,4,]+z[hour,5,]+z[hour,6,]),
            quantile((z[hour,4,]+z[hour,5,]+z[hour,6,]),c(.025,.975)))
filename5 = paste0('org_subs_woNecro_CI_', hour, '_CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(org_subs,filename5)


### estimate enzymes, exudates, waste, and necromass in % of organic substrate
### this is ratio of medians or means
hour = 1729
OC <- (z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])
ppEnz=median(pEnz)*median(OC)/(median(pEnz)+median(pExu)+median(pWaste)+median(pNecro))
ppExu=median(pExu)*median(OC)/(median(pEnz)+median(pExu)+median(pWaste)+median(pNecro))
ppWaste=median(pWaste)*median(OC)/(median(pEnz)+median(pExu)+median(pWaste)+median(pNecro))
ppNecro=median(pNecro)*median(OC)/(median(pEnz)+median(pExu)+median(pWaste)+median(pNecro))
checkSum=ppEnz+ppExu+ppWaste+ppNecro

ppEnz2=mean(pEnz)*mean(OC)/(mean(pEnz)+mean(pExu)+mean(pWaste)+mean(pNecro))
ppExu2=mean(pExu)*mean(OC)/(mean(pEnz)+mean(pExu)+mean(pWaste)+mean(pNecro))
ppWaste2=mean(pWaste)*mean(OC)/(mean(pEnz)+mean(pExu)+mean(pWaste)+mean(pNecro))
ppNecro2=mean(pNecro)*mean(OC)/(mean(pEnz)+mean(pExu)+mean(pWaste)+mean(pNecro))
checkSum2=ppEnz2+ppExu2+ppWaste2+ppNecro2

row1 <- c(ppEnz, ppExu, ppWaste, ppNecro, checkSum)
row2 <- c(ppEnz2, ppExu2, ppWaste2, ppNecro2, checkSum2)

OC_pprod2 = as.data.frame(rbind(row1, row2), rownames=c("median", "mean"))  
filename6 = paste0('OC_pprod2_', hour, '_CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(OC_pprod2,filename6)


### Now calculate mean and median of ratios

mean_O_rat <- function(i=1:10000) {
  OC <- (z[hour,4,i]+z[hour,5,i]+z[hour,6,i]+z[hour,7,i])
  meEnz=z[hour,4,i]*OC[i]/(z[hour,4,i]+z[hour,5,i]+z[hour,6,i]+z[hour,7,i])
  meExu=z[hour,5,i]*OC[i]/(z[hour,4,i]+z[hour,5,i]+z[hour,6,i]+z[hour,7,i])
  meWaste=z[hour,6,i]*OC[i]/(z[hour,4,i]+z[hour,5,i]+z[hour,6,i]+z[hour,7,i])
  meNecro=z[hour,7,i]*OC[i]/(z[hour,4,i]+z[hour,5,i]+z[hour,6,i]+z[hour,7,i])
  
  mEnz=c(mean(meEnz), median(meEnz), sd(meEnz), quantile((meEnz),c(.025,.975)))
  mExu=c(mean(meExu), median(meExu), sd(meExu), quantile((meExu),c(.025,.975)))
  mWaste=c(mean(meWaste), median(meWaste), sd(meWaste), quantile((meWaste),c(.025,.975)))
  mNecro=c(mean(meNecro), median(meNecro), sd(meNecro), quantile((meNecro),c(.025,.975)))

  return(as.data.frame(cbind(mEnz,mExu,mWaste, mNecro)))
}
outm2 <- mean_O_rat()

outmOC2 = as.data.frame(outm2)
rownames(outmOC2) = c("mean", "median", "sd", "CI95l", "CI95u")
filename8 = paste0('outmOC2_', hour, '_CN', z[1,1,1]/z[1,9,1], '.xlsx')
write.xlsx(outmOC2,filename8)

