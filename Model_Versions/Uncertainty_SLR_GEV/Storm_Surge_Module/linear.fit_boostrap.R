###################################
# file: linear.fit_boostrap.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Bootstraps fit parameters for linear fit
# of annual block maxima and flood frequencies
#################################### 

# Set working directory
setwd("/Users/puo112/Documents/Grad/SCRiM/vanDantzig/Model_Versions/Uncertainty_SLR_GEV/Storm_Surge_Module")

# Clear variables
rm(list = ls())

# Source scripts and packags
source("Scripts/plot_sf.r")
library(fExtremes)
library(zoo)
library(DEoptim)

# Load in annual block maxima tide gauge
load("MCMC_coredata.RData")
year.res.max <- MCMC_coredata

year.res.max.fit2 <- gevFit(coredata(year.res.max))

q = seq(0,1,length.out=10^4+1)  # quantile array

# Find closed-form solution of GEV fit
fit_q_year = qgev(q, year.res.max.fit2@fit$par.ests[1], year.res.max.fit2@fit$par.ests[2], year.res.max.fit2@fit$par.ests[3])
fit_q_year = fit_q_year[fit_q_year< max(fit_q_year)]
q = seq(0,1, length.out = 10^4)

# Find linear regression through annual block maxima
year.res.data <- plot.sf(coredata(year.res.max), make.plot = F)
year.res.line <- lm(log10(year.res.data)~coredata(year.res.max))

# Define optimization function based on linear fit through block maxima [y = ax + b]
optim_fn = function(x, year.res.max, year.res.data) 
{
  a = x[1]
  b = x[2]

  est_anom = a * year.res.max + b
  residual = log10(year.res.data) - est_anom
  rsme = sqrt(mean(residual^2))
}

# Optimize each data set using DEoptim and optimize function
lower_bound = c(-1, -10)
upper_bound = c(1, 10)

lm_optim = DEoptim(optim_fn, lower_bound, upper_bound, coredata(year.res.max), year.res.data, control = DEoptim.control(itermax = 1000))

# Assign parameters from best fit equation to a variable for each data set
tide_eq = c(lm_optim$optim$bestmem[1], lm_optim$optim$bestmem[2])

# Generate tide gauge fit estimates based on optimized fit
tide_est = tide_eq[1] * year.res.max + tide_eq[2]

# Generate projection lines based on optimized fit
tide_proj = tide_eq[1] * 0:1000 + tide_eq[2]

################################################################
# Bootstrapping residuals
n_repeat = 500

# Calculate residuals from estimated return levels
tide_resid = year.res.data - tide_est

# Initialize arrays for parameter bootstraps
resid_a = rep(NA, n_repeat)
resid_b = rep(NA, n_repeat)

# Bootstrap residuals using DEoptim
# Return function parameters into variables for "a" and "b"

for (i in 1: n_repeat) 
{
  resid_boot = sample(tide_resid, length(tide_resid), replace = TRUE)
  tide_boot = tide_est + resid_boot
  optim_boot = DEoptim(optim_fn, lower_bound, upper_bound, coredata(year.res.max)[which(tide_boot>0)], tide_boot[which(tide_boot>0)], control = DEoptim.control(itermax = 100))
  resid_a[i] = optim_boot$optim$bestmem[1]
  resid_b[i] = optim_boot$optim$bestmem[2]
}

# Initialize matrix and assign bootstrapped residuals
bootmat = matrix(NA, length(tide_proj), n_repeat)

for(i in 1:n_repeat) 
{
  bootmat[, i] = resid_a[i]* 0:1000 + resid_b[i]
}

# Find 90% confidence interval
lower <- sapply(1:length(bootmat[,1]), function(x) {
  as.numeric(quantile(bootmat[x,], 0.1))
})

upper <- sapply(1:length(bootmat[,1]), function(x) {
  as.numeric(quantile(bootmat[x,], 0.9))
})

# Plot results
source("../Scripts/log.grid.R")
source("../Scripts/mycolors.R")

pdf("Figures/linear.fit_bootstrap.pdf", width = 6.5, height = 5)
par(mar = c(4,4,1,1)+0.1, oma = c(0,0,0,0)+0.1)
plot(coredata(year.res.max), year.res.data, xlim = c(200, 1000), ylim = c(10^-4, 1), 
     log = 'y', yaxt = 'n', xaxt = 'n', type = 'n', yaxs = 'i',
     ylab = "1-Cumulative Frequency", xlab = "Return level [meters]")
log.grid(10^-4, col = rgb(0,0,0,0.2))
magaxis(side = 2, las = 1)
axis(side = 1, at = seq(200, 1000, by = 200), labels = seq(2, 10, by = 2))
#matlines(0:1000, 10^(bootmat), col = "black", lty = 1)
polygon(x = c(0:1000, 1000:0), y = c(10^upper, rev(10^lower)), col = "gray")
points(coredata(year.res.max), year.res.data, pch = 21, bg = "white", lwd = 1.5, cex = 0.85)
abline(lm(log10(year.res.data)~ coredata(year.res.max)), col = mygreen, lwd = 2, lty = 1)
lines(fit_q_year, 1-q, col = myblue, lwd = 2)

legend("topright",
       c("Annual block maxima", "GEV fit", "Linear best fit", "Linear bootstrapped", "90% confidence interval"),
       pch = c(21, NA, NA, NA, 22),
       pt.bg = c("white", NA, NA, NA, "gray"),
       col = c("black", myblue, mygreen, "black", "black"),
       lwd = c(NA, 2, 2, 1, NA),
       pt.lwd = c(1.5, NA, NA, NA, 1.5),
       pt.cex = c(1.5, NA, NA, NA, 1.5),
       bg = "white")
box(lwd = 1.5)

dev.off()

source("../Scripts/plotutils.R")
pdf("Figures/linear.fit_marginals.pdf", width = 4, height = 4)
par(mar = c(0,0,0,0)+0.1, oma = c(0,0,0,0)+0.1)
plot.pairs(data.frame(resid_a, resid_b))
dev.off()


pdf("Figures/linear_parameters.pdf", width = 3, height = 5)
par(mfcol = c(2, 1), mar = c(4, 4, 1, 1), oma = c(0,0,0,0) + 0.1)
plot(density(resid_a), main = NA, lwd = 2, xaxt = 'n')
box(lwd = 1.5)
plot(density(resid_b), main = NA, lwd = 2)
box(lwd = 1.5)
dev.off()
