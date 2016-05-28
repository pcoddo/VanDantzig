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
mycols <- colorRampPalette(c("blue", "yellow", "red"), space = "rgb", bias = 2.2)
#mycols <- colorRampPalette(c("white", "blue", "yellow", "red"), space = "rgb", bias = 1.1)

k <- 11

# Create data frames for baseline, uncertainty, and max/min
df <- data.frame(total_costs.v/1e+06, NPV_expected_losses.v/1e+06, EV_p_exceed_transient.v, costs.v/1e+06)
#df <- data.frame(total_costs.v, NPV_expected_losses.v, EV_p_exceed_transient.v, costs.v)
#df.base <- data.frame(total_costs.base, NPV_expected_losses.base, EV_p_exceed_transient.base, costs.base)
df.base <- data.frame(total_costs.base/1e+06, NPV_expected_losses.base/1e+06, EV_p_exceed_transient.base, costs.base/1e+06)
df.lims <- data.frame(50000, 50000, 0.05, 200)

# Set parameter names / units
param.names=c("Discounted total costs",
              "Discounted damages",
              "Flood Probability",
              "Investment costs")

param.units=c("[million Guilders]",
              "[million Guilders]",
              "[1/yr]",
              "[million Guilders]")

# Baseline curve panel plot (pairs):

#pdf("test8.pdf", width = 7.5, height = 7.5)
png("test9.pleasework.png", width = 7.5, height = 7.5, units = 'in', res = 600)
#pdf("test9.1.pdf", width = 7.5, height = 7.5)
par( oma=c(8,8,5,2),mfrow=c(3,3),mar=c(1,1,0,0))
d=ncol(df)

for(i in 1:(d-1))
{
  for(j in 1:(d-1))
  {
    if(i<((d+1)-j))
    {
      panel=data.frame(cbind(df[,(d+1)-j],df[,i]))
      y.lim = df.lims[,i]
      x.lim = df.lims[,(d+1)-j]
      
      plot(panel$X1, panel$X2, type = 'n', axes = FALSE, 
           xlim = c(min(panel$X1), x.lim),
           ylim = c(min(panel$X2), y.lim))
      rect(xleft = -10000, ybottom = -10000, xright = max(panel$X1)*1.25, ytop = max(panel$X2)*1.25,
           col = "blue", border = "blue")
      smoothScatter(panel$X1, panel$X2, nbin = 500, axes = FALSE, xlab = NA, ylab = NA, 
                    xlim = c(min(panel$X1), x.lim),
                    ylim = c(min(panel$X2), y.lim),
                    nrpoints = 0, colramp = mycols, add = T, transformation = function(x) x^0.2)#, yaxs = 'i', xaxs = 'i')
      box(lwd = 1.5)
      symbols(0, 0, stars = matrix(c(rep(c(2,0.75),8)), ncol = 16), bg = "green", inches = 0.15, add = TRUE)
      lines(df.base[,(d+1)-j], df.base[,i], col = "black", lwd = 3.2)
      #points(0, 0, col = myred, pch = 8, cex = 2.5, lwd = 2)
      points(df.base[,(d+1)-j][min_ind], df.base[,i][min_ind], pch = 20, col = "black", cex = 2.5)
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
#       legend("bottomright", 
#              c("van Dantzig (1956) baseline", "Storm surge and parametric uncertainty", "Minimum expected NPV of Total cost", "Ideal solution"),
#              lty = c(1, NA, NA, NA),
#              pch = c(NA, 22, 20, 8),
#              lwd = c(1.5, NA, NA, NA),
#              col = c("black", myblue, "black", myred ),
#              pt.bg = c(NA, mybluealpha1, NA, NA),
#              pt.cex = c(NA, 3, 1.5, 1.5),
#              cex = 1.4, box.lwd = 1.5,
#              inset = c(0.1, 0),
#              xpd = NA, 
#       )
      image.plot(zlim=c(0,1),legend.only=TRUE, horizontal=TRUE,axex=TRUE, legend.mar = 5,legend.width=3, 
                 col = (designer.colors(500, c("blue", "yellow", "red"))), smallplot = c(0, .75, 0.05, 0.15),
                 legend("bottomright", 
                        c("van Dantzig (1956) baseline", "Minimum expected NPV of total costs", 
                          "Hypothetical ideal solution", "Density", "", "", ""),
                        lty = c(1, NA, NA, NA, NA, NA, NA),
                        pch = c(NA, 20, 8, NA, NA, NA, NA),
                        lwd = c(2, NA, 1.5, NA, NA, NA, NA),
                        col = c("black", "black", mygreen, NA, NA, NA, NA),
                        pt.bg = c(NA, NA, NA, NA, NA, NA, NA),
                        pt.cex = c(NA, 1.5, 2.2, NA, NA, NA, NA),
                        cex = 1.4, box.lwd = 1.5,
                        inset = c(0.1, -0.3),
                        xpd = NA, bty='n' 
                 ))
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
      eaxis( 1, cex.axis=1.4)
      mtext( line=3.5, side=1,param.names[(d+1)-j], font = 2 )
      mtext( line=5, side=1,param.units[(d+1)-j] )} 
    
    #if( i != 3 && ((d+1)-j)==d ){
    if( ((d+1)-j)==d ){
      
      eaxis( 2, cex.axis=1.4)
      mtext( line=7, side=2,param.names[i], font = 2)
      mtext( line=5.5, side=2,param.units[i])}
  }
}

dev.off()