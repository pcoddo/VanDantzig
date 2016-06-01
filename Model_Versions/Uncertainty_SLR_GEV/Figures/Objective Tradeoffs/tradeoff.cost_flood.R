###################################
# file: Uncertainty_tradeoffs.costs_flood.R
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

# load in tradeoff data
load("../../SLR_GEV.RData")

#define palette color
mycols <- colorRampPalette(c("blue", "yellow", "red"), space = "rgb", bias = 2.2)

# Create data frames for baseline, uncertainty, and max/min
# Convert from 1953 guilders to 1953 US dollars (multiply by 0.263)
  # http://fxtop.com/en/currency-converter-past.php?A=1&C1=NLG&C2=USD&DD=&MM=&YYYY=1953&B=1&P=&I=1&btnOK=Go%21
# Adjust for inflation from 1953-2016 using average annual consumer price index (CPI)
  # 2016 cost = 1953 cost * (CPI_2016 / CPI_1953)
  # 8.92 = 1 guilder * (238.132 / 26.7)
    # http://www.usinflationcalculator.com
    # http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
df <- data.frame(EV_p_exceed_transient.v, costs.v*0.263*8.92/1e+06)
df.base <- data.frame(EV_p_exceed_transient.base, costs.base*0.263*8.92/1e+06)

# Axis limits to zoom in on distribution
df.lims <- data.frame(0.05, 800)

# Create data frame for expected (mean) tradeoffs
EV_p_exceed_transient.mean <- apply(EV_p_exceed_transient, 1, mean)
costs.mean <- apply(costs, 1, mean)
df.mean <- data.frame(EV_p_exceed_transient.mean, costs.mean*0.263*8.92/1e+06)

# Set parameter names / units
param.names=c("Flood Probability",
              "Investment costs")

param.units=c("[1/yr]",
              "[million USD]")


# Plot
png("tradeoff.cost_flood.png", width = 7.5, height = 7.5, units = 'in', res = 600)
par( oma=c(8,8,5,2),mfcol=c(2,2),mar=c(1,1,0,0))
d=ncol(df)

      panel=data.frame(cbind(df[,2],df[,1]))
      y.lim = df.lims[,1]
      x.lim = df.lims[,2]
      
      plot(panel$X1, panel$X2, type = 'n', axes = FALSE,
           xlim = c(min(panel$X1), x.lim),
           ylim = c(min(panel$X2), y.lim))
      rect(xleft = -1000, ybottom = 1e-06, xright = max(panel$X1)*1.25, ytop = max(panel$X2)*1.25,
           col = "blue", border = "blue")
      smoothScatter(panel$X1, panel$X2, nbin = 1000, axes = FALSE, xlab = NA, ylab = NA, 
                    xlim = c(min(panel$X1), x.lim),
                    ylim = c(min(panel$X2), y.lim),
                    nrpoints = 0, colramp = mycols, add = T, transformation = function(x) x^0.2)
      box(lwd = 1.5)
      symbols(0, 0, stars = matrix(c(rep(c(1,0.25),5)), ncol = 10), bg = "black", inches = 0.05, add = TRUE)
      lines(df.base[,2], df.base[,1], col = "black", lwd = 3.2, lty = 2)
      lines(df.mean[,2], df.mean[,1], col = "black", lwd = 3.2, lty = 1)

      points(df.base[,2][min_ind], df.base[,1][min_ind], pch = 20, col = "black", cex = 2.5)
      eaxis(side = 1, tck = -.045, labels = TRUE, lwd = 2)
      eaxis(side = 2, tck = -.045, labels = TRUE, lwd = 2)

      #axis( 2, cex.axis=1.5, at=10^(seq(0, -8, by = -2)), label=parse(text=paste("10^", seq(0, -8, by = -2), sep="")), las =1)
      mtext( line=5, side=2,param.names[1], font = 2 )
      mtext( line=4, side=2,param.units[1] )
    
      mtext( line=2.5, side=1,param.names[2], font = 2 )
      mtext( line=3.5, side=1,param.units[2] ) 

      # Add colorbar
      plot.new()
      image.plot(zlim=c(0,1),legend.only=TRUE, horizontal=TRUE,axex=TRUE, legend.mar = 5,legend.width=3, 
                 col = (designer.colors(500, c("blue", "yellow", "red"))), smallplot = c(0, .75, 0.05, 0.15),
                 legend("bottomright", 
                        c("van Dantzig (1956) baseline", "Minimum expected NPV of total costs", 
                          "Expected tradeoff with updated structural \nand parametric uncertainties",
                          "Hypothetical ideal solution", "Density (unitless)", "", "", "", ""),
                        lty = c(2, NA, 1, NA, NA, NA, NA, NA, NA),
                        pch = c(NA, 20, NA, 8, NA, NA, NA, NA, NA),
                        lwd = c(2, NA, 2, 1.5, NA, NA, NA, NA, NA),
                        col = c("black", "black", "black", "black", NA, NA, NA, NA, NA),
                        pt.bg = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
                        pt.cex = c(NA, 1.5, NA, 2.2, NA, NA, NA, NA, NA),
                        cex = 1.1, box.lwd = 1.5,
                        inset = c(0.2, -0.3),
                        xpd = NA, bty='n')) 

dev.off()
            
# Add ideal point and change legend spacing in Photoshop
            
            