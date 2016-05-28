###################################
# file: Parameter_hist.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Plots histogram distributions of Parameters with density overlay
#################################### 

# Set working directory
setwd("~/Documents/Grad/SCRiM/van Dantzig/Model Versions/Parametric Uncertainty/Figures/Parameter Histograms")

source("mycolors.R")
pdf("Parameter_hist.pdf", width = 8, height = 6)

par(mfrow = c(2,4), mar=c(4,4,1,1)+0.1, oma=c(0,0,0,0))

#bquote("p"[0]), expression(alpha), "V", expression(delta ~ "'"), "k", expression(eta), expression(phi))
hist(Parameters$p_zero_p, prob = T,
     xlab = expression(bold("Initial flood frequency,"~ p[0])),
     ylab = expression(bold("Frequency")),
     main = NULL,
     col = "light blue",
     border = "light blue",
     lwd = 1.5,
     cex.axis = 0.9,
     cex.lab = 1,
     las = 1,
     axes = T,
     font.axis = 1)
abline(v= p_zero_p)
lines(density(Parameters$p_zero_p), col = myred, lwd = 1)
segments(p_zero_p, 0, p_zero_p, max(density(Parameters$p_zero_p)$y), col = "dark gray", lwd = 4)
box(lwd = 1.5)

hist(Parameters$alpha_p, prob = T,
     xlab = expression(bold("Flood Frequency Rate,"~ alpha)),
     ylab = NULL,#expression(bold("Frequency")),
     main = NULL,
     col = "light blue",
     border = "light blue",
     lwd = 1.5,
     cex.axis = 0.9,
     cex.lab = 1,
     las = 1,
     axes = T,
     font.axis = 1)
lines(density(Parameters$alpha_p), col = myred, lwd = 1)
segments(alpha_p, 0, alpha_p, max(density(Parameters$alpha_p)$y), col = "dark gray", lwd = 4)
box(lwd = 1.5)

hist(Parameters$V_p, prob = T,
     xlab = expression(bold("Value of goods, V")),
     ylab = NULL,#expression(bold("Frequency")),
     main = NULL,
     col = "light blue",
     border = "light blue",
     lwd = 1.5,
     cex.axis = 0.9,
     cex.lab = 1,
     las = 1,
     axes = T,
     font.axis = 1)
lines(density(Parameters$V_p), col = myred, lwd = 1)
segments(V_p, 0, V_p, max(density(Parameters$V_p)$y), col = "dark gray", lwd = 4)
box(lwd = 1.5)

hist(Parameters$delta_prime_p, prob = T,
     xlab = expression(bold("Effective discount rate," ~ delta~"'")),
     ylab = NULL,#expression(bold("Frequency")),
     main = NULL,
     col = "light blue",
     border = "light blue",
     lwd = 1.5,
     cex.axis = 0.9,
     cex.lab = 1,
     las = 1,
     axes = T,
     font.axis = 1)
lines(density(Parameters$delta_prime_p), col = myred, lwd = 1)
segments(delta_prime_p, 0, delta_prime_p, max(density(Parameters$delta_prime_p)$y), col = "dark gray", lwd = 4)
box(lwd = 1.5)

hist(Parameters$k_p, prob = T,
     xlab = expression(bold("Heightening cost, k")),
     ylab = expression(bold("Frequency")),
     main = NULL,
     col = "light blue",
     border = "light blue",
     lwd = 1.5,
     cex.axis = 0.9,
     cex.lab = 1,
     las = 1,
     axes = T,
     font.axis = 1)
lines(density(Parameters$k_p), col = myred, lwd = 1)
segments(k_p, 0, k_p, max(density(Parameters$k_p)$y), col = "dark gray", lwd = 4)
box(lwd = 1.5)

hist(Parameters$subs_rate, prob = T,
     xlab = expression(bold("Subsidence rate,"~ eta)),
     ylab = NULL,#expression(bold("Frequency")),
     main = NULL,
     col = "light blue",
     border = "light blue",
     lwd = 1.5,
     cex.axis = 0.9,
     cex.lab = 1,
     las = 1,
     axes = T,
     font.axis = 1)
lines(density(Parameters$subs_rate), col = myred, lwd = 1)
segments(subs_rate, 0, subs_rate, max(density(Parameters$subs_rate)$y), col = "dark gray", lwd = 4)
box(lwd = 1.5)

hist(Parameters$sea_level_rate, prob = T,
     xlab = expression(bold("Sea level rate," ~ phi)),
     ylab = NULL,#expression(bold("Frequency")),
     main = NULL,
     col = "light blue",
     border = "light blue",
     lwd = 1.5,
     cex.axis = 0.9,
     cex.lab = 1,
     las = 1,
     axes = T,
     font.axis = 1)
lines(density(Parameters$sea_level_rate), col = myred, lwd = 1)
segments(sea_level_rate, 0, sea_level_rate, max(density(Parameters$sea_level_rate)$y), col = "dark gray", lwd = 4)
box(lwd = 1.5)

plot.new()
legend("center", 
       c("van Dantzig prior", "LHS distribution", "Density curve"),
       lty = c(1, NA, 1),
       pch = c(NA, 15, NA),
       lwd = c(3, 5, 1.5),
       col = c("dark gray", "lightblue", myred),
       xpd = NA,
       cex = 1.3,
       box.lwd = 1.5)

dev.off()
