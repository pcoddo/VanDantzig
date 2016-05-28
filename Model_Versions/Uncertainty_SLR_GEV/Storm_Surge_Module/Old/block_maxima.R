###################################
# file: block_maxima.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Generates survival functions for tide gauge data
# Monthly/Annual Block Maxima & Wemelsfelder plots
#################################### 

setwd("~/Documents/Grad/SCRiM/Tide Gauge")
source("plot_sf.r")

monthly <- read.table("monthly_block_maxima.txt", header = T, sep = '\t')
annual <- read.table("annual_block_maxima.txt", header = T, sep = '\t')
wemels <- read.table("wemelsfelder.txt", header = T, sep = '\t')

# Wemelsfelder recreation:
# Isolate CCDF values
wemels.data <- plot.sf(wemels$detrend, make.plot = F)

# Generate regression line
wemels.line <- lm(log10(wemels.data[1:50])~wemels$detrend[1:50])

# Plot figure
source("put_fig_letter.r")
source("mycolors.R")

pdf("wemelsfelder.pdf", width = 6.5, height = 4, family="Times", pointsize=12)
par(oma = c(0,0,0,0), mar = c(4,4,0,0)+0.25)

plot.sf(wemels$detrend, pch = 20, cex = 0.75,
  xlab = "Maximum tide (cm)",
  ylab = "1 - CDF",
  xlim = c(200, 850),
  ylim = c(0.0001, 1))
axis(2, at=10^(seq(-4,-2, by = 1)), label=parse(text=paste("10^", seq(-4,-2, by = 1), sep="")), las=1)
box(lwd = 1.5)
points(wemels$detrend[51], wemels.data[51], pch = 21, col = "black", bg = myred, cex = 0.85)
put.fig.letter("1953", location="topleft", offset=c(0.35, -0.37), cex = 0.90)
legend("topright", inset = c(0.01, 0.01),
       pch = c(21, 20, NA, NA, NA),
       col = c("black", "black", "black", "black", myblue),
       pt.bg = c(myred, NA, NA, NA, NA),
       lty = c(NA, NA, 1, 2, 1),
       lwd = c(NA, NA, 2, 2, 1.5),
       c("North Sea flood", "Wemelsfelder (1961)", "Linear fit", "Future projection", "1/10,000 year flood"),
       cex = 0.90,
       bty = 'n')

abline(h = 1/10000, col = myblue, lwd = 2)
clip(200,380, 1, 0.0001)
abline(wemels.line)
clip(380,800, 1, 0.0001)
abline(wemels.line, lty = 2, lwd = 1.5)

dev.off()

#################################### 
# Monthly:
mbm <- plot.sf(monthly$detrend, make.plot = F)
mbm.line <- lm(log10(mbm) ~ monthly$detrend)

pdf("monthly_block.pdf", width = 6.5, height = 4)
par(oma = c(0,0,0,0), mar = c(4,4,0,0)+0.25)

plot.sf(monthly$detrend, pch = 20, cex = 0.75,
        xlab = "Maximum tide (cm)",
        ylab = "1 - CDF",
        xlim = c(100, 700),
        ylim = c(0.0001, 1))
abline(mbm.line)
axis(2, at=10^(seq(-4,-4, by = 1)), label=parse(text=paste("10^", seq(-4,-4, by = 1), sep="")), las=1)
box(lwd = 1.5)

dev.off()

#################################### 
# Annual:
abm <- plot.sf(annual$detrend, make.plot = F)
abm.line <- lm(log10(abm) ~ annual$detrend)

pdf("annual_block.pdf", width = 6.5, height = 4)
par(oma = c(0,0,0,0), mar = c(4,4,0,0)+0.25)

plot.sf(annual$detrend, pch = 20, cex = 0.75,
        xlab = "Maximum tide (cm)",
        ylab = "1 - CDF",
        xlim = c(200, 850),
        ylim = c(0.0001, 1))
abline(abm.line)
axis(2, at=10^(seq(-4,-3, by = 1)), label=parse(text=paste("10^", seq(-4,-3, by = 1), sep="")), las=1)
box(lwd = 1.5)

dev.off()
#################################### 

