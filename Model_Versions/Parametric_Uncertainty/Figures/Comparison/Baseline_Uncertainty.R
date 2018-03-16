###################################
# file: Baseline_Uncertainty.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Shows baseline van Dantzig (1956) model vs. effects of 
# storm suge and parametric uncertainties
#################################### 

# Set working directory:
setwd("~/vanDantzig/Model_Versions/Parametric_Uncertainty/Figures/Comparison")

# Load environment from van Dantzig analysis with parametric uncertainty
load("../../Parametric_Uncertainty.RData")

source("../../Scripts/put_fig_letter.r")
source("../../Scripts/mycolors.R")
library(extrafont)

# Baseline vs Uncertainty Figure

# Baseline plot
#pdf(file = "../Figures/Fig1_Baseline_Uncertainty1.pdf", height = 8, width = 6, family = "Times", pointsize = 12)
png("Baseline_Uncertainty.png", width = 6, height = 8, unit = "in", res = 600)
par(mfcol = c(2,1), mar = c(3, 2, 1, 0), oma = c(2, 2, 1, 1), family="Times")

plot(X, (NPV_expected_losses.base/1e+06), type = 'l', col = myred, 
     xlab = NA,
     ylab = expression(bold('Expected cost (million Guilders)')),
     lwd = 3,
     las = 1,
     ylim = c(0,600),
     xaxs="i")
lines(X, (costs.base/1e+06), col = myblue , lwd = 3)
lines(X, (total_costs.base/1e+06), col = 'black', lwd = 4)
points(min_cost_X, (total_costs.base[min_ind]/1e+06), pch = 19)
axis(side = 1, tck = -.045, labels = NA, lwd = 1.5)
axis(side = 2, tck = -.045, labels = NA, lwd = 1.5)
mtext(expression(bold('Expected cost (million Guilders)')), side = 2, line = 2.5)

legend("topright", 
       lty = c(1, 1, 1, NA),
       pch = c(NA, NA, NA, 20),
       lwd = 3,
       c("Expected Losses", "Investment Costs", "Expected Total Costs", "Minimum Total Costs"),
       col = c(myred, myblue, "black", "black"),
       cex = 1,
       bg = "white")
box(lwd = 1.5)
mtext("A. Storm surge uncertainty", side = 3, line = 0.35, at = 0, adj = c(0,0))
text(min_cost_X, 200, labels = min_cost_X)

# Uncertainty plot
matplot(X, (NPV_expected_losses/1e+06), type = 'l', col = myredalpha05,
     xlab = expression(bold("Dike height increase (m)")), 
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

points(min_cost_X_mean, (mean_total_costs[min_ind_mean]/1e+06), pch = 19, col = "dark gray")
box(lwd = 1.5)
axis(side = 1, tck = -.045, labels = NA, lwd = 1.5)
axis(side = 2, tck = -.045, labels = NA, lwd = 1.5)
mtext(expression(bold("Dike height increase (m)")), side = 1, line = 2.5)
mtext(expression(bold('Expected cost (million Guilders)')), side = 2, line = 2.5)
mtext("B. Storm surge and parametric uncertainties", side = 3, line = 0.35, at = 0, adj = c(0,0))
text(min_cost_X_mean, 200, labels = min_cost_X_mean)#"2.65 m")

dev.off()
