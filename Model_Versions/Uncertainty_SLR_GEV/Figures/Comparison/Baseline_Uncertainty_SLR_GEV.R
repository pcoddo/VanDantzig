###################################
# file: Baseline_Uncertainty_SLR_GEV.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Shows baseline van Dantzig (1956) model vs. effects of 
# (1) storm suge and parametric uncertainties, 
# (2) updated sea level rise module, and
# (3) updated storm surge module
#################################### 

# Set working directory:
#setwd("~vanDantzig/Model_Versions/Uncertainty_SLR_GEV/Figures/Comparison")

source("../../Scripts/put_fig_letter.r")
source("../../Scripts/mycolors.R")
library(extrafont)

# Baseline vs Uncertainty Figure
load("../../../Parametric_Uncertainty/Parametric_Uncertainty.RData")

# (A) Baseline plot
png("fig3.png", width = 9, height = 5.5, unit = "in", res = 600)
#pdf("Baseline_Uncertainty_SLR_GEV2.pdf", width = 9.5, height = 5.5)
par(mfrow = c(2,2), mar = c(0, 1, 2, 0.75)+0.1, oma = c(3.5, 3, 0, 0.5)+0.1)

plot(X, (NPV_expected_losses.base/1e+06), type = 'l', col = myred, 
     xlab = NA,
     ylab = expression(bold('Expected cost [million Guilders]')),
     lwd = 3,
     las = 1,
     ylim = c(0,600),
     xaxs="i",
     xaxt='n')
lines(X, (costs.base/1e+06), col = myblue , lwd = 3)
lines(X, (total_costs.base/1e+06), col = 'black', lwd = 4)
points(min_cost_X, (total_costs.base[min_ind]/1e+06), cex = 1.5, pch = 19)
axis(side = 1, tck = -.045, labels = NA, lwd = 1.5)
axis(side = 2, tck = -.045, labels = NA, lwd = 1.5)
mtext('Expected cost [million Guilders]', side = 2, line = 3, cex = 0.9)

legend("topright", 
       lty = c(1, 1, 1, NA),
       pch = c(NA, NA, NA, 20),
       lwd = 3,
       c("Discounted damages", "Investment costs", "Discounted total costs", "Minimum NPV total costs"),
       col = c(myred, myblue, "black", "black"),
       cex = 1,
       bg = "white")
box(lwd = 1.3)
mtext("(A) van Dantzig (1956) baseline model structure", side = 3, line = 0.15, at = 0, adj = c(0,0), cex = 0.9)
text(min_cost_X, 200, labels = min_cost_X)

# (B) Parametric Uncertainty plot
# Load environment from van Dantzig analysis with parametric uncertainty
load("../../../Parametric_Uncertainty/Parametric_Uncertainty.RData")

matplot(X, (NPV_expected_losses/1e+06), type = 'l', col = myredalpha05,
     xlab = expression(bold("Dike height increase [m]")), 
     ylab = expression(bold('Expected cost [million Guilders]')),
     las = 1,
     lty = 1,
     ylim = c(0,600),
     xaxs="i",
     xaxt = 'n',
     yaxt = 'n')
lines(X, (mean_NPV_expected_losses/1e+06), col = "dark red", lwd = 3)

matlines(X, costs/1e+06, col = mybluealpha05, type = 'l', lty = 1)
lines(X, (mean_costs/1e+06), col = "blue", lwd = 3)
matlines(X, total_costs/1e+06, col = rgb(0, 0, 0, 0.02), lty = 1)
lines(X, (mean_total_costs/1e+06), col = "dark gray", lwd = 3)

points(min_cost_X_mean, (mean_total_costs[min_ind_mean]/1e+06), pch = 19, cex = 1.5, col = "dark gray")
box(lwd = 1.5)
axis(side = 1, tck = -.045, labels = NA, lwd = 1.5)
axis(side = 2, tck = -.045, labels = NA, lwd = 1.5)
mtext("(B) Addition of parametric uncertainties", side = 3, line = 0.15, at = 0, adj = c(0,0), cex = 0.9)
text(min_cost_X_mean, 200, labels = min_cost_X_mean)

# (C) Sea level rise module plot
# Load environment from van Dantzig analysis with sea level rise module
load("../../../Uncertainty_SLR/Uncertainty_SLR.RData")

matplot(X, (NPV_expected_losses/1e+06), type = 'l', col = myredalpha05,
        xlab = NA,
        ylab = expression(bold('Expected cost (million Guilders)')),
        las = 1,
        lty = 1,
        ylim = c(0,600),
        xaxs="i")
lines(X, (mean_NPV_expected_losses/1e+06), col = "dark red", lwd = 3)

matlines(X, costs/1e+06, col = mybluealpha05, type = 'l', lty = 1)
lines(X, (mean_costs/1e+06), col = "blue", lwd = 3)
matlines(X, total_costs/1e+06, col = rgb(0, 0, 0, 0.02), lty = 1)
lines(X, (mean_total_costs/1e+06), col = "dark gray", lwd = 3)

points(min_cost_X_mean, (mean_total_costs[min_ind_mean]/1e+06), pch = 19, cex = 1.5, col = "dark gray")
box(lwd = 1.5)
axis(side = 1, tck = -.045, labels = NA, lwd = 1.5)
axis(side = 2, tck = -.045, labels = NA, lwd = 1.5)
mtext("Dike height increase [m]", side = 1, line = 2.5, cex = 0.9)
mtext('Expected cost [million Guilders]', side = 2, line = 3, cex = 0.9)
mtext("(C) Updated sea level rise model", side = 3, line = 0.15, at = 0, adj = c(0,0), cex = 0.9)
text(min_cost_X_mean, 225, labels = min_cost_X_mean)

# (D) Storm surge module plot
# Load environment from van Dantzig analysis with sea level rise module
load("../../SLR_GEV.RData")

matplot(X, (NPV_expected_losses/1e+06), type = 'l', col = myredalpha05,
        xlab = expression(bold("Dike height increase [m]")), 
        ylab = expression(bold('Expected cost [million Guilders]')),
        las = 1,
        lty = 1,
        ylim = c(0,600),
        xaxs="i",
        yaxt = 'n')
lines(X, (mean_NPV_expected_losses/1e+06), col = "dark red", lwd = 3)

matlines(X, costs/1e+06, col = mybluealpha05, type = 'l', lty = 1)
lines(X, (mean_costs/1e+06), col = "blue", lwd = 3)
matlines(X, total_costs/1e+06, col = rgb(0, 0, 0, 0.02), lty = 1)
lines(X, (mean_total_costs/1e+06), col = "dark gray", lwd = 3)

points(min_cost_X_mean, (mean_total_costs[min_ind_mean]/1e+06), pch = 19, cex = 1.5, col = "dark gray")
box(lwd = 1.5)
axis(side = 1, tck = -.045, labels = NA, lwd = 1.5)
axis(side = 2, tck = -.045, labels = NA, lwd = 1.5)
mtext("Dike height increase [m]", side = 1, line = 2.5, cex = 0.9)
mtext("(D) Updated storm surge model", side = 3, line = 0.15, at = 0, adj = c(0,0), cex = 0.9)
text(min_cost_X_mean, 300, labels = min_cost_X_mean)

dev.off()
