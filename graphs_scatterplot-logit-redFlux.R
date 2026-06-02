### printing scatterplot - influence on %necromass
# PNAS Fig. 4
setwd("C:/Users/Your/Path/Here")

library(car)    #includes function logit

# read data
# we need both datasets: parhdf2* has parameter values for each MC, 
#         and fout* hourly results to calculate % necromass p (of OC released)

x = readRDS('fout_redFlux_dN005.RData')
y = readRDS('parhdf2_redFlux_dN005.RData')
# dim(y)

# convert to an array, with dimensions (time, variable, iteration)
z = array(unlist(x),dim=c(
length(x[[1]][[1]]),
length(x[[1]]),
length
(x)))
dim(z)

names = c("C", "B", "CO2", "Enz", "Exu", "Waste", "deadB",
          "sumfEx", "N", "BN", "Nof", "NEnz", "NExu", "NWaste", "deadNB")

# added 1 to hour, they start at 0
# hour=1729
# hour=330  # hour=176
# hour=12   # hour=11
pNecro = z[hour,7,]/(z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])*100
p = array(unlist(pNecro))

# choose the parameter sets you like to plot
 zz <- cbind(y$muO2, y$d, y$rateEnz, y$rateExu, y$rateWaste, p)
# zz <- cbind(y$kO2C,y$rm, y$fNBac, y$fNEnz, y$fNExu, p)

# first set of graphs for day 72, with x-axis titles
# check y-axis min and max, for reduced flux
 min(logit(zz[,6]))    # -7.0(dN01), -6.5(dN005)
 max(logit(zz[,6]))    #  3.5(dN01),  3.0(dN005)

dev.off()
# plot.new()
 
par(mfrow = c(1,5))
par(mar=c(6, 5, 2, 2) + 0.1)

plot(log(zz[,1]), logit(zz[,6]),
    xlab = "", ylab = "Necromass logit(% of organic products)",
    xaxt = 'n',
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis = 2.2, type = "p")
    axis(1, mgp=c(3, 1.5, 0), cex.axis = 2.2)
    mtext (text = expression(paste("Maximum growth rate ln(hour"^-1,")")),
    # mtext (text = expression(paste("Half saturation constant ln(hour"^-1,")")),    
             line = 4.5, cex = 1.5, side = 1)
    #lines(lowess(zz[,1], logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,1]))
    abline((lmz), col = "red", lwd = 2)

plot(log(zz[,2]),logit(zz[,6]),
    xlab = "", ylab = "",
    xaxt ='n', yaxt ='n', 
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis = 2.2, type = "p")
    axis(1, mgp=c(3, 1.5, 0), cex.axis = 2.2)
    mtext (text = expression(paste("Death rate ln(hour"^-1,")")),
    # mtext (text = expression(paste("Maintenance respiration ln(hour"^-1,")")),
             line = 4.5, cex = 1.5, side = 1)
    #lines(lowess(zz[,2],logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,2]))
    abline((lmz), col = "red", lwd = 2)

plot(log(zz[,3]),logit(zz[,6]), 
    xlab = "", ylab = "",
    xaxt ='n', yaxt ='n',
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis = 2.2, type = "p")
    axis(1, mgp=c(3, 1.5, 0), cex.axis = 2.2)
    mtext (text = expression(paste("Enzyme production rate ln(hour"^-1,")")),
    # mtext (text = expression(paste("N:C bacteria (mol mol"^-1,")")),
             line = 4.5, cex = 1.5, side = 1)
    #lines(lowess(zz[,3],logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,3]))
    abline((lmz), col = "red", lwd = 2)

plot(log(zz[,4]),logit(zz[,6]),
    xlab = "", ylab = "",
    xaxt ='n', yaxt ='n', 
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis = 2.2, type = "p")
    axis(1, mgp=c(3, 1.5, 0), cex.axis = 2.2)
    mtext (text = expression(paste("Exudate production rate ln(hour"^-1,")")),
    # mtext (text = expression(paste("N:C enzymes (mol mol"^-1,")")),       
             line = 4.5, cex = 1.5, side = 1)
    #lines(lowess(log(zz[,4]),logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,4]))
    abline((lmz), col = "red", lwd = 2)

plot(log(zz[,5]),logit(zz[,6]),
    xlab = "", ylab = "",
    xaxt='n', yaxt='n',
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis = 2.2, type= "p")
    axis(1, mgp=c(3, 1.5, 0), cex.axis = 2.2)
    mtext (text = expression(paste("Waste production rate ln(hour"^-1,")")),
    # mtext (text = expression(paste("N:C exudates (mol mol"^-1,")")),       
             line = 4.5, cex = 1.5, side = 1)
    #lines(lowess(log(zz[,5]),logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,5]))
    abline((lmz), col = "red", lwd = 2)

# second set of graphs without x-axis label (already 1 hour added)
# dN = 0.05: hour = 330, hour = 12 : day 14, day 1
# dN = 0.1:  hour = 176, hour = 11 : day 7,  day 1    
# hour=1729
pNecro = z[hour,7,]/(z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])*100 
p = array(unlist(pNecro))
zz <- cbind(y$muO2, y$d, y$rateEnz, y$rateExu, y$rateWaste, p)
# zz <- cbind(y$kO2C, y$rm, y$fNBac, y$fNEnz, y$fNExu, p)
     
par(mfrow = c(1,5))
par(mar=c(2, 5, 2, 2) + 0.1)    

plot(log(zz[,1]), logit(zz[,6]),
#    main = expression(paste("Maximum growth rate (hour"^-1,")")),
    xlab = "", ylab = "Necromass logit(% of organic products)",
    xaxt ='n',
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis = 2.2, cex.main = 2.5, type = "p")
    axis(1, labels=FALSE)
    # lines(lowess(zz[,1], logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,1]))
    abline((lmz), col = "red", lwd = 2)

plot(log(zz[,2]), logit(zz[,6]),
#    main = expression(paste("Death rate (hour"^-1,")")), 
    xlab = "", ylab = "", 
    xaxt ='n', yaxt ='n',
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis = 2.2, cex.main = 2.5, type= "p")
    axis(1, labels=FALSE)
    # lines(lowess(zz[,2], logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,2]))
    abline((lmz), col = "red", lwd = 2)

plot(log(zz[,3]), logit(zz[,6]),
#    main = expression(paste("Enzyme production rate (hour"^-1,")")), 
    xlab = "", ylab = "",
    xaxt ='n', yaxt ='n',
    ylim = c(-7, 3.5),
    cex.lab = 2.2, cex.axis = 2.2, cex.main = 2.5, type= "p")
    axis(1, labels=FALSE)
    # lines(lowess(zz[,3], logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,3]))
    abline((lmz), col = "red", lwd = 2)

plot(log(zz[,4]), logit(zz[,6]),
#    main = expression(paste("Exudate production rate log(hour"^-1,")")), 
    xlab = "", ylab = "",
    xaxt ='n', yaxt ='n',
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis = 2.2, cex.main = 2.5, type= "p")
    axis(1, labels=FALSE)
    # lines(lowess(log(zz[,4]), logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,4]))
    abline((lmz), col = "red", lwd = 2)

plot(log(zz[,5]), logit(zz[,6]),
#    main = expression(paste("Waste production rate log(hour"^-1,")")), 
    xlab = "", ylab = "",
    xaxt ='n', yaxt ='n', 
    ylim = c(-6.5, 3),
    cex.lab = 2.2, cex.axis=2.2, cex.main = 2.5, type = "p")
    axis(1, labels=FALSE)
    # lines(lowess(log(zz[,5]), logit(zz[,6])), col = "red", lwd = 2)
    lmz <- lm(logit(zz[,6]) ~ log(zz[,5]))
    abline((lmz), col = "red", lwd = 2)
    