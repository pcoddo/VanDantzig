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

# Load data
load("../../SLR_GEV.RData")

# Load plotting libraries
library(fields)
library(KernSmooth)
library(RColorBrewer)
library(MASS)
library(sfsmisc)
source("../../Scripts/mycolors.R")

set.seed(101)


#define palette color
mycols <- colorRampPalette(c("blue", "yellow", "red"), space = "rgb", bias = 1.8)
#mycols <- colorRampPalette(c("white", "blue", "yellow", "red"), space = "rgb", bias = 1.1)

k <- 11

# Create data frames for baseline, uncertainty, and max/min
df <- data.frame(costs.v/1e+06, total_costs.v/1e+06, NPV_expected_losses.v/1e+06, EV_p_exceed_transient.v)#*0.56*7.91/1e+06)
df.base <- data.frame(costs.base/1e+06, total_costs.base/1e+06, NPV_expected_losses.base/1e+06, EV_p_exceed_transient.base)
df.lims <- data.frame(200, 5000, 5000, max(EV_p_exceed_transient.v))

# Create data frame for expected tradeoffs
costs.mean <- apply(costs, 1, mean)
total_costs.mean <- apply(total_costs, 1, mean)
NPV_expected_losses.mean <- apply(NPV_expected_losses, 1, mean)
EV_p_exceed_transient.mean <- apply(EV_p_exceed_transient, 1, mean)
df.mean <- data.frame(costs.mean/1e+06, total_costs.mean/1e+06, NPV_expected_losses.mean/1e+06, EV_p_exceed_transient.mean)#*0.56*7.91/1e+06)

# Set parameter names / units
param.names=c("Investment costs",
              "Discounted total costs",
              "Discounted damages",
              "Flood probability")

param.units=c("[million Guilders]",
              "[million Guilders]",
              "[million Guilders]",
              "[1/yr]")

# Baseline curve panel plot (pairs):

png("Fig4.png", width = 3, height = 8.5, units = 'in', res = 600)
#pdf("tradeoff_newbins.pdf", width = 7.5, height = 7.5)#, units = 'in', res = 600)
par( oma=c(4,6,1,1),mfcol=c(4,1), mar=c(1.25,1,0,0))

# Panel 1 - costs vs. total costs
panel=data.frame(cbind(df[,1],df[,2]))
x.lim = df.lims[,1]
y.lim = df.lims[,2]

plot(panel$X1, panel$X2, type = 'n', axes = FALSE, 
     xlim = c(min(panel$X1), x.lim),
     ylim = c(min(panel$X2), y.lim))
rect(xleft = -1000, ybottom = -1000, xright = max(panel$X1)*1.25, ytop = max(panel$X2)*1.25,
     col = "blue", border = "blue")
smoothScatter(panel$X1, panel$X2, nbin = 1000, axes = FALSE, xlab = NA, ylab = NA,# bandwidth = c(3,1)/3, #nbin15500
              xlim = c(min(panel$X1), x.lim),
              ylim = c(min(panel$X2), y.lim),
              nrpoints = 0, colramp = mycols, add = T)#, transformation = function(x) x^0.2)
box(lwd = 1.5)
symbols(0, 0, stars = matrix(c(rep(c(1,0.25),5)), ncol = 10), bg = "black", inches = 0.05, add = TRUE)
lines(df.base[,1], df.base[,2], col = "black", lwd = 3.2, lty = 2)
lines(df.mean[,1], df.mean[,2], col = "black", lwd = 3.2, lty = 1)

points(df.base[,1][min_ind], df.base[,2][min_ind], pch = 20, col = "black", cex = 2.5)
points(df.mean[,1][min_ind_mean], df.mean[,2][min_ind_mean], pch = 20, col = "black", cex = 2.5)
eaxis(side = 1, tck = -.045, labels = FALSE, lwd = 2)
eaxis(side = 2, tck = -.045, labels = NULL, lwd = 2)
#eaxis(2, at = axt, labels = pretty10exp(1^axt, drop.1=TRUE, sub10 = c(-6,-1)))
#axis(2, lwd = 1.5, at=10^(seq(-6, -1, by = 1)), label=parse(text=paste("10^", seq(-6,-1, by = 1), sep="")), las = 1)

mtext( line=5.5, side=2,param.names[2], font = 2, cex = 0.85)
mtext( line=4, side=2,param.units[2], cex = 0.85)

#mtext( line=2.5, side=1,param.names[1], font = 2 )
#mtext( line=3.5, side=1,param.units[1] ) 

# Panel 2 - costs vs. damages
panel=data.frame(cbind(df[,1],df[,3]))
x.lim = df.lims[,1]
y.lim = df.lims[,3]

plot(panel$X1, panel$X2, type = 'n', axes = FALSE, 
     xlim = c(min(panel$X1), x.lim),
     ylim = c(min(panel$X2), y.lim))
rect(xleft = -1000, ybottom = -1000, xright = max(panel$X1)*1.25, ytop = max(panel$X2)*1.25,
     col = "blue", border = "blue")
smoothScatter(panel$X1, panel$X2, nbin = 1000, axes = FALSE, xlab = NA, ylab = NA,# bandwidth = c(3,1)/3, #nbin15500
              xlim = c(min(panel$X1), x.lim),
              ylim = c(min(panel$X2), y.lim),
              nrpoints = 0, colramp = mycols, add = T)#, transformation = function(x) x^0.2)
box(lwd = 1.5)
symbols(0, 0, stars = matrix(c(rep(c(1,0.25),5)), ncol = 10), bg = "black", inches = 0.05, add = TRUE)
lines(df.base[,1], df.base[,3], col = "black", lwd = 3.2, lty = 2)
lines(df.mean[,1], df.mean[,3], col = "black", lwd = 3.2, lty = 1)

points(df.base[,1][min_ind], df.base[,3][min_ind], pch = 20, col = "black", cex = 2.5)
points(df.mean[,1][min_ind_mean], df.mean[,3][min_ind_mean], pch = 20, col = "black", cex = 2.5)
eaxis(side = 1, tck = -.045, labels = FALSE, lwd = 2)
eaxis(side = 2, tck = -.045, labels = NULL, lwd = 2)
#eaxis(2, at = axt, labels = pretty10exp(1^axt, drop.1=TRUE, sub10 = c(-6,-1)))
#axis(2, lwd = 1.5, at=10^(seq(-6, -1, by = 1)), label=parse(text=paste("10^", seq(-6,-1, by = 1), sep="")), las = 1)

mtext( line=5.5, side=2,param.names[3], font = 2, cex = 0.85)
mtext( line=4, side=2,param.units[3], cex = 0.85)

#mtext( line=2.5, side=1,param.names[1], font = 2 )
#mtext( line=3.5, side=1,param.units[1] ) 

# Panel 3 - costs vs. flood probability
panel=data.frame(cbind(df[,1],df[,4]))
y.lim = df.lims[,4]
x.lim = df.lims[,1]

plot(panel$X1, panel$X2, type = 'n', axes = FALSE, log = 'y', yaxs = 'i',
     xlim = c(min(panel$X1), x.lim),
     ylim = c(1e-05, y.lim))
rect(xleft = -1000, ybottom = 1e-07, xright = max(panel$X1)*1.25, ytop = 1,
     col = "blue", border = "blue")
smoothScatter(panel$X1, panel$X2, nbin = 15500, axes = FALSE, xlab = NA, ylab = NA,# bandwidth = c(3,1)/3, #nbin15500
              xlim = c(min(panel$X1), x.lim),
              ylim = c(min(panel$X2), y.lim),
              nrpoints = 0, colramp = mycols, add = T)#, transformation = function(x) x^0.2)
box(lwd = 1.5)
symbols(0, 0, stars = matrix(c(rep(c(1,0.25),5)), ncol = 10), bg = "black", inches = 0.05, add = TRUE)
lines(df.base[,1], df.base[,4], col = "black", lwd = 3.2, lty = 2)
lines(df.mean[,1], df.mean[,4], col = "black", lwd = 3.2, lty = 1)

points(df.base[,1][min_ind], df.base[,4][min_ind], pch = 20, col = "black", cex = 2.5)
points(df.mean[,1][min_ind_mean], df.mean[,4][min_ind_mean], pch = 20, col = "black", cex = 2.5)
eaxis(side = 1, tck = -.045, labels = TRUE, lwd = 2)
eaxis(side = 2, tck = -.045, labels = NULL, lwd = 2)
#eaxis(2, at = axt, labels = pretty10exp(1^axt, drop.1=TRUE, sub10 = c(-6,-1)))
#axis(2, lwd = 1.5, at=10^(seq(-6, -1, by = 1)), label=parse(text=paste("10^", seq(-6,-1, by = 1), sep="")), las = 1)

mtext( line=5.5, side=2,param.names[4], font = 2, cex = 0.85)
mtext( line=4, side=2,param.units[4], cex = 0.85)

mtext( line=3, side=1,param.names[1], font = 2, cex = 0.85)
mtext( line=4.5, side=1,param.units[1], cex = 0.85)

# Add colorbar
par(mar=c(0,1.25,2,0))
plot.new()
image.plot(zlim=c(0,1), legend.only=T, horizontal=TRUE, 
           axis.args = list( at = c(0,1), labels = c("Min", "Max")), col = (designer.colors(500, c("blue", "yellow", "red"))), smallplot = c(0.05, 0.85, 0.05, 0.15),
           legend("bottomright", 
                  c("Hypothetical ideal solution within considered range",
                    "van Dantzig (1956) baseline", 
                    "Expected tradeoff with updated structural \nand parametric uncertainties",
                    "Minimum expected NPV of total costs", 
                    "Relative density of states of the world (unitless)"),#, "", "", "", ""),
                  lty = c(NA, 3, 1, NA, NA, NA, NA, NA, NA),
                  pch = c(8, 20, NA, 20, NA, NA, NA, NA, NA),
                  lwd = c(1.5, 2, 2, NA, NA, NA, NA, NA, NA),
                  col = c("black", "black", "black", "black", NA, NA, NA, NA, NA),
                  pt.bg = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
                  pt.cex = c(0.5, 1.5, NA, 1.5, NA, NA, NA, NA, NA),
                  cex = 1, box.lwd = 1.5,
                  inset = c(0, 0.15),
                  xpd = NA, bty='n')) 

dev.off()

# Add ideal point and change legend spacing in Photoshop



png("Fig5.png", width = 8, height = 4, units = 'in', res = 300)

par(oma = c(4.5,5,0,0)+0.5, mar = c(1,3,1,1), mfrow = c(1,2))
plot(costs/1e+06, EV_p_exceed_transient, pch = 20, col = "gray", cex = 0.25, axes = FALSE,log = 'y',
     yaxs = 'i', xaxs = 'i',
     xlim = c(0, 200), xlab = "", ylab = "")
     #ylim = c(1e-4, max(EV_p_exceed_transient.v)), ylab = "")

segments(x0 = 100, y0 = 1e-04, x1 = 100, y1 = 1e-9, lty = 1, lwd = 2, col = "dodgerblue3")
segments(x0 = -1e6, y0 = 1e-04, x1 = 100, y1 = 1e-04, lty = 1, lwd = 2, col = "dodgerblue3")

lines(df.base[,1], df.base[,4], col = "black", lwd = 1.5, lty = 2)
points(df.base[,1][min_ind], df.base[,4][min_ind], pch = 20, col = "black", cex = 1)

eaxis(side = 1, tck = -.045, labels = NULL, lwd = 1.5)
eaxis(side = 2, tck = -.045, labels = TRUE, lwd = 1.5)

mtext( line=2.5, side=1,param.names[1], font = 2, cex = 1)
mtext( line=3.5, side=1,param.units[1], cex = 1)

mtext( line=5, side=2,param.names[4], font = 2, cex = 1)
mtext( line=4, side=2,param.units[4], cex = 1)

legend("bottomleft", "(A)", bty = 'n', inset = c(-0.08,0))
box(lwd = 1.5)


satisfy <- subset(Objectives, costs.v <= 10^8 & EV_p_exceed_transient.v <= 1e-4)
robust <- subset(Objectives, costs.v <= 10^8 & EV_p_exceed_transient.v <= 1e-4 & NPV_expected_losses <= 10^6)

### Zoomed in plot
plot(satisfy$costs.v/1e+06, satisfy$EV_p_exceed_transient.v, pch = 20, cex = 0.25, col = "gray", log = 'y',
     axes = F, xlab = "", ylab = "",
     yaxs = 'i', xaxs = 'i',
     ylim = c(1e-9, 1e-4), xlim = c(40, 100))
eaxis(side = 1, tck = -.045, labels = TRUE, lwd = 1.5)
eaxis(side = 2, tck = -.045, labels = TRUE, lwd = 1.5)

lines(df.base[,1], df.base[,4], col = "black", lwd = 1.5, lty = 2)
points(df.base[,1][min_ind], df.base[,4][min_ind], pch = 20, col = "black", cex = 1)

points(robust$costs.v/1e+06, robust$EV_p_exceed_transient.v, pch = 20, cex = 0.25, col = "darkgreen")

mtext( line=2.5, side=1,param.names[1], font = 2, cex = 1)
mtext( line=3.5, side=1,param.units[1], cex = 1)

mtext( line=4.5, side=2,param.names[4], font = 2, cex = 1)
mtext( line=3.5, side=2,param.units[4], cex = 1)
legend("bottomleft", "(B)", bty = 'n', inset = c(-0.08,0))
box(lwd = 4, col = "dodgerblue3")
box(lwd = 1.5)
dev.off()