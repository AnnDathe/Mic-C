### printing the base simulation
### Dr. Annette Dathe, Cornell University, May/June 2026

old.par = par(no.readonly=TRUE)
par(mfrow = c(4,4))
par(mar=c(3.1, 3, 1.5, 0.1) + 0.1)      # only use on small screen

plot(out$time/24, out$C, main = "Carbon Source", mgp = c(2, 0.8, 0),
     xaxt = 'n', xlab = "", ylab = "Carbon (mol)", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
     axis(1, labels = FALSE)

plot(out$time/24, out$B, main = "C Living Bacteria", mgp = c(2, 0.8, 0),
      xaxt = 'n', xlab = "", ylab = "", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
     axis(1, labels = FALSE)

plot(out$time/24, out$deadB, main = "C Dead Bacteria", mgp = c(2, 0.8, 0),
     xaxt = 'n', xlab = "", ylab = "", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
     axis(1, labels = FALSE)

plot(out$time/24, out$CO2, main = "Carbon Dioxide", mgp = c(2, 0.8, 0),
     xlab = "time (days)", ylab = "", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)

plot(out$time/24, out$Enz, main = "C Exo-Enzymes", mgp = c(2, 0.8, 0),
     xaxt = 'n', xlab = "", ylab = "Carbon (mol)", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
     axis(1, labels = FALSE)

plot(out$time/24, out$Exu, main = "C Exudates", mgp = c(2, 0.8, 0),
     xaxt = 'n', xlab = "", ylab = "", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
     axis(1, labels = FALSE)

plot(out$time/24, out$Waste, main = "C Waste", mgp = c(2, 0.8, 0),
     xaxt = 'n', xlab = "", ylab = "", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
     axis(1, labels = FALSE)

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

#plot(out$time, outdf$df_rBN, main = "rBN factor",
#     xlab = "time, (hours)", ylab = "1", type = "l", lwd = 2)

plot(out$time/24, out$N, main = "Nitrogen Source", mgp = c(2, 0.8, 0),
     xaxt = 'n', xlab = "", ylab = "Nitrogen (mol)", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
     axis(1, labels = FALSE)

plot(out$time/24, out$BN, main = "N Living Bacteria", mgp = c(2, 0.8, 0),
    xaxt='n', xlab = "", ylab = "", 
    cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
    axis(1, labels = FALSE)

plot(out$time/24, out$deadNB, main = "N Dead Bacteria", mgp = c(2, 0.8, 0),
     xlab = "time (days)", ylab = "", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)

plot(out$time/24, out$Nof, main = "N Gases", mgp = c(2, 0.8, 0),
     xaxt = 'n', xlab = "", ylab = "", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)
     axis(1, labels = FALSE)

plot(out$time/24, out$NEnz, main = "N Exo-Enzymes", mgp = c(2, 0.8, 0),
     xlab = "time (days)", ylab = "Nitrogen (mol)", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)

plot(out$time/24, out$NExu, main = "N Exudates", mgp = c(2, 0.8, 0),
     xlab = "time (days)", ylab = "", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)

#plot(out$time, out$NWaste, main = "N Waste",
#     xlab = "time, (hours)", ylab = "mol N", type = "l", lwd = 2)

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

#plot(outdf$time/24, outdf$df_fEx, main = "% Necromass", mgp = c(2, 0.8, 0),
plot(outdf[2:1729,1]/24, outdf[2:1729,18], main = "% Necromass", mgp = c(2, 0.8, 0),     
     xlab = "time (days)", ylab = "Carbon (% mol)", 
     cex.axis = 1.1, cex.lab = 1.1, type = "l", lwd = 2)

par(old.par)

