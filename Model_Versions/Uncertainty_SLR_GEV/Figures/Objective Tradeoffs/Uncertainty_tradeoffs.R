###################################
# file: Uncertainty_tradeoffs.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Produces manually adjusted pairwise scattter plots 
# for van Dantzig (1956) Objectives
# Includes uncertainty bounds, EUM and Ideal points
#################################### 

source("../../Scripts/mycolors.R")

library(fields)
library(KernSmooth)
library(RColorBrewer)
library(MASS)
library(sfsmisc)
set.seed(101)


#define palette color
mycols <- colorRampPalette(c("blue","yellow","red"),space = "Lab")
k <- 11

#EV_p_exceed_transient[which(EV_p_exceed_transient==0)] <- min(EV_p_exceed_transient.base)#min(EV_p_exceed_transient[which(EV_p_exceed_transient != 0)]) #
#EV_p_exceed_transient.v[which(EV_p_exceed_transient.v==0)] <- min(EV_p_exceed_transient.base)#NA#min(EV_p_exceed_transient[which(EV_p_exceed_transient != 0)])

# Identify outer bounds of distributions for polygon shading
max.costs <- apply(costs, 1, max)
min.costs <- apply(costs, 1, min)
range.costs <- c(1, max(costs/1e+06))

max.total_costs <- apply(total_costs, 1, max)
min.total_costs <- apply(total_costs, 1, min)
range.total_costs <- c(min(total_costs/1e+06), max(total_costs/1e+06))

max.losses <- apply(NPV_expected_losses, 1, max)
min.losses <- apply(NPV_expected_losses, 1, min)
range.losses <- c(min(NPV_expected_losses/1e+06), max(NPV_expected_losses/1e+06))

max.reliability <- apply(EV_p_exceed_transient, 1, max)
min.reliability <- apply(EV_p_exceed_transient, 1, min)
range.reliability <- c(min(EV_p_exceed_transient), max(EV_p_exceed_transient))

# Create data frames for baseline, uncertainty, and max/min
df <- data.frame(total_costs.v/1e+06, NPV_expected_losses.v/1e+06, EV_p_exceed_transient.v, costs.v/1e+06)
df.base <- data.frame(total_costs.base/1e+06, NPV_expected_losses.base/1e+06, EV_p_exceed_transient.base, costs.base/1e+06)
df.max <- data.frame(max.total_costs/1e+06, max.losses/1e+06, max.reliability, max.costs/1e+06)
df.min <- data.frame(min.total_costs/1e+06, min.losses/1e+06, min.reliability, min.costs/1e+06)

# Set parameter names / units
param.names=c("Discounted total costs",
              "Discounted damages",
              "Reliability",
              "Investment costs")

param.units=c("[million Guilders]",
              "[million Guilders]",
              "[1/yr]",
              "[million Guilders]")

# Baseline curve panel plot (pairs):

#df <- df[sample(nrow(df[indexes,]), 150000), ]

#pdf("test5.pdf", width = 7.5, height = 7.5)
png("test5.png", width = 7.5, height = 7.5, units = 'in', res = 600)
par( oma=c(8,8,5,2),mfrow=c(3,3),mar=c(1,1,0,0))
d=ncol(df)

for(i in 1:(d-1))
{
  for(j in 1:(d-1))
  {
    if(i<((d+1)-j))
    {
      panel=data.frame(cbind(df[,(d+1)-j],df[,i]))
#       if( i == 3)
#       {
#         plot(panel$X1, panel$X2, pch = NA, axes = FALSE, log = 'y')
#         polygon(x = c(df.max[,(d+1)-j], rev(df.min[,(d+1)-j])), y = c(df.max[,i], rev(df.min[,i])),
#                 col = mybluealpha1, border = myblue)
#         lines(df.base[,(d+1)-j], df.base[,i])
#         points(0, min(range(df[,i], na.rm = T)), col = rgb(1, 102/255, 102/255, 1), pch = 8, cex = 1.5)
#         points(df.base[,(d+1)-j][min_ind], df.base[,i][min_ind], pch = 20, cex = 1.5)
#         box(lwd = 2)
#         axis(side = 2, at=10^(seq(0, -8, by = -2)), tck = -.045, tck = -.045, labels = NA, lwd = 2)
#         axis(side = 1, tck = -.045, labels = NA, lwd = 2)
#       }
#       if((d+1)-j==3)#i==2 && ((d+1)-j)==3 )
#       {
#         plot(panel$X1, panel$X2, pch = NA, axes = FALSE, log = 'x')
#         polygon(x = c(df.max[,(d+1)-j], rev(df.min[,(d+1)-j])), y = c(df.max[,i], rev(df.min[,i])),
#                col = mybluealpha1, border = myblue)
#         lines(df.base[,(d+1)-j], df.base[,i])
#         points(min(range(df[,(d+1)-j], na.rm = T)), 0, col = rgb(1, 102/255, 102/255, 1), pch = 8, cex = 1.5)
#         points(df.base[,(d+1)-j][min_ind], df.base[,i][min_ind], pch = 20, cex = 1.5)
#         box(lwd = 2)
#         axis(side = 1, at=10^(seq(-8, 0, by = 2)), tck = -.045, tck = -.045, labels = NA, lwd = 2)
#         axis(side = 2, tck = -.045, labels = NA, lwd = 2)
#       }
      
#       else 
#       {
#         if(i != 3)
#         {
      #smoothScatter(panel$X1, panel$X2, axes = FALSE, xlab = NA, ylab = NA, 
                    #nrpoints = 0, colramp = mycols, yaxs = 'i', xaxs = 'i')
      plot(panel$X1, panel$X2, pch = 21, axes = FALSE, bg = "gray", col = NA)
        #plot(panel$X1, panel$X2, pch = NA, axes = FALSE)
                #polygon(x = c(df.max[,(d+1)-j], rev(df.min[,(d+1)-j])), y = c(df.max[,i], rev(df.min[,i])),
         #       col = mybluealpha1, border = myblue)
#         polygon(x = c(df.max[,(d+1)-j], rev(df.min[,(d+1)-j])), y = c(df.max[,i], rev(df.min[,i])),
#                col = "gray", border = NA)
        points(df.min[,(d+1)-j], df.min[,i], pch = 21, bg = "gray", col = NA)
        points(df.max[,(d+1)-j], df.max[,i], pch = 21, bg = "gray", col = NA)
        lines(df.base[,(d+1)-j], df.base[,i], col = "black", lwd = 1.5)
        points(0, 0, col = rgb(1, 102/255, 102/255, 1), pch = 8, cex = 1.5)
        points(df.base[,(d+1)-j][min_ind], df.base[,i][min_ind], pch = 20, col = "black")
        box(lwd = 2)
        eaxis(side = 1, tck = -.045, labels = FALSE, lwd = 2)
        eaxis(side = 2, tck = -.045, labels = FALSE, lwd = 2)
        #}
      #}
    } else
    {
      plot.new()
    }
    if(i==(d-1)&((d+1)-j)==2)
    {
      legend("bottomright", 
             c("van Dantzig (1956) baseline", "Storm surge and parametric uncertainty", "Minimum expected NPV of Total cost", "Ideal solution"),
             lty = c(1, NA, NA, NA),
             pch = c(NA, 22, 20, 8),
             lwd = c(1.5, NA, NA, NA),
             col = c("black", myblue, "black", myred ),
             pt.bg = c(NA, mybluealpha1, NA, NA),
             pt.cex = c(NA, 3, 1.5, 1.5),
             cex = 1.4, box.lwd = 1.5,
             inset = c(0.1, 0),
             xpd = NA)
    }
#     if( ((i+j)==d)&((d+1)-j)==2 | ((i+j)==d)&((d+1)-j)==4 ){
#       axis( 1, cex.axis=1.5)
#       mtext( line=3.5, side=1,param.names[(d+1)-j], font = 2 )
#       mtext( line=5, side=1,param.units[(d+1)-j] )}
    
    if(((d+1)-j)==4 && i == 3){
      #axis( 2, cex.axis=1.5, at=10^(seq(0, -8, by = -2)), label=parse(text=paste("10^", seq(0, -8, by = -2), sep="")), las =1)
      mtext( line=7, side=2,param.names[i], font = 2 )
      mtext( line=5.5, side=2,param.units[i] )}

    if( ((i+j)==d)){
    #if( ((i+j)==d) &&((d+1)-j)==3 ){
      #axis( 1, cex.axis=1.5, at=10^(seq(-8, 0, by = 2)), label=parse(text=paste("10^", seq(-8, 0, by = 2), sep="")))
      eaxis( 1, cex.axis=1.5, las = 1)
      mtext( line=3.5, side=1,param.names[(d+1)-j], font = 2 )
      mtext( line=5, side=1,param.units[(d+1)-j] )} 

    #if( i != 3 && ((d+1)-j)==d ){
      if( ((d+1)-j)==d ){
        
      eaxis( 2, cex.axis=1.5, las = 1)
      mtext( line=7, side=2,param.names[i], font = 2)
      mtext( line=5.5, side=2,param.units[i])}
  }
}

dev.off()