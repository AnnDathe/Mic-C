### script to plot Mic-C hourly simulations +/- sd
### http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
### additional colors from above: black"#000000,reddish purple"#CC79A7"
### Dr. Annette Dathe, Cornell University, June 1st 2026

setwd("C:/Users/Your/Path/Here")

library(data.table)
library(xlsx)
library(plyr)

d <- read.delim("~/R-scripts/Mic-C_redFlux/meanC_redFlux_CN20.txt",
               header=TRUE)

# scale time to days
d$hour <- d$hour/24

# first plot for C, B, CO2
par(mfcol=c(1,1),ps=20,lwd=3,bty='l' ,mar=c(2,5,3,1) + 0.1 ) #one graph with large text

plot(x = d$hour, y = d$m_C, xlab = "", ylab = "",
ylim = c(0, 100), xlim = c(0, 72), xaxs = "i", yaxs = "i",
cex.lab = 1, cex.axis = 1, type = "l", lwd = 2, axes = FALSE)
axis(side = 2, ylim = c(0, 100), cex.axis = 1, lwd.ticks = 2)
mtext(text = expression(paste("Carbon (mol)")), line = 3, cex = 1, side = 2)
axis(side = 1, ylim = c(0, 72), labels=FALSE, lwd.ticks = 2)
      legend( 40,100,        
      legend = c("Carbon input", "Carbon dioxide release", "Bacterial carbon"),
      col = c("#009E73","#D55E00","#0072B2"),
      bty ='n',lty = 1, cex = 0.9, xjust=0, y.intersp=0.75)  

with(d, polygon(c(hour, rev(hour)), c(d$l_C,rev(d$u_C)),border=NA, col=adjustcolor("#009E73", alpha=.25)))
lines(d$m_C ~ d$hour, type = "l", lwd = 3, col = "#009E73")
#bluish green
with(d, polygon(c(hour, rev(hour)), c(d$l_B,rev(d$u_B)),border=NA, col=adjustcolor("#0072B2", alpha=.25)))
lines(d$m_B ~ d$hour, type = "l", lwd = 3, col = "#0072B2")
#blue
with(d, polygon(c(hour, rev(hour)), c(d$l_CO2,rev(d$u_CO2)),border=NA, col=adjustcolor("#D55E00", alpha=.25)))
lines(d$m_CO2 ~ d$hour, type = "l", lwd = 3, col = "#D55E00")
#vermillion
box()

# second plot for waste, necro, exu, enz
par(mfcol=c(1,1),ps=20,lwd=3,bty='l' ,mar=c(5,5,3,1) + 0.1 ) #one graph with large text

# for dN=0.1  use ylim = c(0,45)
# for dN=0.05 use ylim = c(0,25)
plot(x = d$hour, y = d$m_Enz, xlab = "", ylab = "",
     ylim = c(0, 25), xlim = c(0, 72), xaxs = "i", yaxs = "i",
     cex.lab = 1, cex.axis = 1, type = "l", lwd = 2, axes = FALSE)
axis(side = 2, ylim = c(0, 25), cex.axis = 1, lwd.ticks = 2)
mtext(text = expression(paste("Carbon (mol)")), line = 3, cex = 1, side = 2)
axis(side = 1, ylim = c(0, 72), cex.axis = 1, lwd.ticks = 2)
mtext(text = expression(paste("Time (days)")), line = 3.8, cex = 1, side = 1)
        legend( 1,25,
#        legend( 1,45,  
        legend = c("Waste products carbon", "Necromass carbon", "Exudate carbon", "Exoenzyme carbon"),
#        col = c("#E69F00", "#CC79A7", "#56B4E9",  "#888888"),
        col = c("#E69F00", "#CC79A7", "#56B4E9","#009E73"),
        bty ='n',lty = 1, cex = 0.9, xjust=0, y.intersp=0.75) 

with(d, polygon(c(hour, rev(hour)), c(d$l_Enz,rev(d$u_Enz)),border=NA, 
                col=adjustcolor("#009E73", alpha=.25)))
lines(d$m_Enz ~ d$hour, type = "l", lwd = 3, col = "#009E73")
#green

with(d, polygon(c(hour, rev(hour)), c(d$l_deadB,rev(d$u_deadB)),border=NA, 
                col=adjustcolor("#CC79A7", alpha=.25)))
lines(d$m_deadB ~ d$hour, type = "l", lwd = 3, col = "#CC79A7")
#reddish purple

with(d, polygon(c(hour, rev(hour)), c(d$l_Waste,rev(d$u_Waste)),border=NA, 
                col=adjustcolor("#E69F00", alpha=.25)))
lines(d$m_Waste ~ d$hour, type = "l", lwd = 3, col = "#E69F00")
#orange

with(d, polygon(c(hour, rev(hour)), c(d$l_Exu,rev(d$u_Exu)),border=NA, 
                col=adjustcolor("#56B4E9", alpha=.25)))
lines(d$m_Exu ~ d$hour, type = "l", lwd = 3, col = "#56B4E9")
#sky blue

box()

### do the same for N pools
dN <- read.delim("~/R-scripts/Mic-C_redFlux/meanN_redFlux_CN20.txt",
                 header=TRUE)

# scale time to days
dN$hour <- dN$hour/24

# first plot for N, BN, Nof
par(mfcol=c(1,1),ps=20,lwd=3,bty='l' ,mar=c(2,5,3,1) + 0.1 ) #one graph with large text

# for dN=0.05 use ylim = c(0,5)
# for dN=0.1  use ylim = c(0,10)
plot(x = dN$hour, y = dN$m_N, xlab = "", ylab = "",
     xlim = c(0, 72), ylim = c(0, 5), xaxs = "i", yaxs = "i",
     cex.lab = 1, cex.axis = 1, type = "l", lwd = 2, axes = FALSE)
axis(side = 2, ylim = c(0, 5), cex.axis = 1, lwd.ticks = 2)
mtext(text = expression(paste("Nitrogen (mol)")), line = 3, cex = 1, side = 2)
axis(side = 1, xlim = c(0, 72), labels=FALSE, lwd.ticks = 2)
#      legend( 46,10, # for CN=10
      legend( 40,5, # for CN=20        
        legend = c("Nitrogen input", "Nitrogen release", "Bacterial nitrogen"),
        col = c("#009E73","#D55E00","#0072B2"),
        bty ='n',lty = 1, cex = 0.9, xjust=0,y.intersp=0.75)  

with(dN, polygon(c(hour, rev(hour)), c(dN$l_N,rev(dN$u_N)),border=NA, col=adjustcolor("#009E73", alpha=.25)))
lines(dN$m_N ~ dN$hour, type = "l", lwd = 3, col = "#009E73")
#bluish green
with(dN, polygon(c(hour, rev(hour)), c(dN$l_BN,rev(dN$u_BN)),border=NA, col=adjustcolor("#0072B2", alpha=.25)))
lines(dN$m_BN ~ dN$hour, type = "l", lwd = 3, col = "#0072B2")
#blue
with(dN, polygon(c(hour, rev(hour)), c(dN$l_Nof,rev(dN$u_Nof)),border=NA, col=adjustcolor("#D55E00", alpha=.25)))
lines(dN$m_Nof ~ dN$hour, type = "l", lwd = 3, col = "#D55E00")
#vermillion
box()

par(mfcol=c(1,1),ps=20,lwd=3,bty='l' ,mar=c(5,5,3,1) + 0.1 ) #one graph with large text

# for dN=0.005 use ylim = c(0,3.5)
# for dN=0.01  use ylim = c(0,7)
plot(x = dN$hour, y = dN$m_NEnz, xlab = "", ylab = "",
     ylim = c(0, 3.5), xlim = c(0, 72), xaxs = "i", yaxs = "i",
     cex.lab = 1, cex.axis = 1, type = "l", lwd = 2, axes = FALSE)
axis(side = 2, ylim = c(0, 25), cex.axis = 1, lwd.ticks = 2)
mtext(text = expression(paste("Nitrogen (mol)")), line = 3, cex = 1, side = 2)
axis(side = 1, xlim = c(0, 72), cex.axis = 1, lwd.ticks = 2)
mtext(text = expression(paste("Time (days)")), line = 3.8, cex = 1, side = 1)
#        legend( 1,7, # CN=10
        legend( 1,3.5, # CN=20,  
        legend = c("Exudate nitrogen", "Necromass nitrogen", "Exoenzyme nitrogen"),
        col = c("#CC79A7", "#56B4E9", "#009E73"),
        bty ='n',lty = 1, cex = 0.9, xjust=0, y.intersp=0.75) 

with(dN, polygon(c(hour, rev(hour)), c(dN$l_NEnz,rev(dN$u_NEnz)),border=NA, col=adjustcolor("#009E73", alpha=.40)))
lines(dN$m_NEnz ~ dN$hour, type = "l", lwd = 3, col = "#009E73")
#grey
with(dN, polygon(c(hour, rev(hour)), c(dN$l_NExu,rev(dN$u_NExu)),border=NA, col=adjustcolor("#CC79A7", alpha=.25)))
lines(dN$m_NExu ~ dN$hour, type = "l", lwd = 3, col = "#CC79A7")
#reddish purple
with(dN, polygon(c(hour, rev(hour)), c(dN$l_deadNB,rev(dN$u_deadNB)),border=NA, col=adjustcolor("#56B4E9", alpha=.40)))
lines(dN$m_deadNB ~ dN$hour, type = "l", lwd = 3, col = "#56B4E9")
#sky blue
box()
