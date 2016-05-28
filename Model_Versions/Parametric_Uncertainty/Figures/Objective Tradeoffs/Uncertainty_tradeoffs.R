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
df <- data.frame(total_costs.v/1e+06, costs.v/1e+06, NPV_expected_losses.v/1e+06, EV_p_exceed_transient.v)
df.base <- data.frame(total_costs.base/1e+06, costs.base/1e+06, NPV_expected_losses.base/1e+06, EV_p_exceed_transient.base)
df.max <- data.frame(max.total_costs/1e+06, max.costs/1e+06, max.losses/1e+06, max.reliability)
df.min <- data.frame(min.total_costs/1e+06, min.costs/1e+06, min.losses/1e+06, min.reliability)

# Set parameter names / units
param.names=c("NPV: Total Costs",
              "Investment Costs",
              "NPV: Damages",
              "Expected Annual Flood Frequency")

param.units=c("(million Guilders)",
              "(million Guilders)",
              "(million Guilders)",
              "(1/yr)")

# Baseline curve panel plot (pairs):
pdf("Uncertainty_tradeoffs2.pdf", width = 7, height = 7)#, family = "Times")
par( oma=c(8,8,5,1),mfrow=c(3,3),mar=c(1,1,0,0))
d=ncol(df)

for(i in 1:(d-1))
{
  for(j in 1:(d-1))
  {
    if(i<((d+1)-j))
    {
      panel=data.frame(cbind(df[,(d+1)-j],df[,i]))
        if(((d+1)-j)==4)
        {
        plot(panel$X1, panel$X2, pch = NA, axes = FALSE, log = 'x')
        polygon(x = c(df.max[,(d+1)-j], rev(df.min[,(d+1)-j])), y = c(df.max[,i], rev(df.min[,i])),
                col = mybluealpha1, border = myblue)
        lines(df.base[,(d+1)-j], df.base[,i])
        points(min(range(df[,(d+1)-j])), 0, col = rgb(1, 102/255, 102/255, 1), pch = 8, cex = 1.5)
        points(df.base[,(d+1)-j][min_ind], df.base[,i][min_ind], pch = 20, cex = 1.5)
        box(lwd = 2)
        axis(side = 1, at=10^(seq(-14, 0, by = 3)), tck = -.045, tck = -.045, labels = NA, lwd = 2)
        axis(side = 2, tck = -.045, labels = NA, lwd = 2)
        } else 
        {
        plot(panel$X1, panel$X2, pch = NA, axes = FALSE)
        polygon(x = c(df.max[,(d+1)-j], rev(df.min[,(d+1)-j])), y = c(df.max[,i], rev(df.min[,i])),
          col = mybluealpha1, border = myblue)
        lines(df.base[,(d+1)-j], df.base[,i])
        points(0, 0, col = rgb(1, 102/255, 102/255, 1), pch = 8, cex = 1.5)
        points(df.base[,(d+1)-j][min_ind], df.base[,i][min_ind], pch = 20)
        box(lwd = 2)
        axis(side = 1, tck = -.045, labels = NA, lwd = 2)
        axis(side = 2, tck = -.045, labels = NA, lwd = 2)
        }
    } else
    {
      plot.new()
    }
    if(i==(d-1)&((d+1)-j)==2)
    {
      legend("bottomright", 
             c("van Dantzig baseline", "Storm surge and parametric uncertainty", "EUM solution", "Ideal solution"),
             lty = c(1, NA, NA, NA),
             pch = c(NA, 22, 20, 8),
             lwd = c(1.5, NA, NA, NA),
             col = c("black", myblue, "black", myred ),
             pt.bg = c(NA, mybluealpha1, NA, NA),
             pt.cex = c(NA, 3, 1.5, 1.5),
             cex = 1.4, box.lwd = 1.5,
             inset = c(0.1, -0.1),
             xpd = NA)
    }
    if( ((i+j)==d)&((d+1)-j)==2 | ((i+j)==d)&((d+1)-j)==3 ){
      axis( 1, cex.axis=1.5)
      mtext( line=3.5, side=1,param.names[(d+1)-j], font = 2 )
      mtext( line=5, side=1,param.units[(d+1)-j] )}
    if( ((i+j)==d)&((d+1)-j)==4 ){
      axis( 1, cex.axis=1.5, at=10^(seq(-14, 0, by = 3)), label=parse(text=paste("10^", seq(-14, 0, by = 3), sep="")))
      mtext( line=3.5, side=1,param.names[(d+1)-j], font = 2 )
      mtext( line=5, side=1,param.units[(d+1)-j] )} 
    if( ((d+1)-j)==d ){
      axis( 2, cex.axis=1.5, las = 1)
      mtext( line=7, side=2,param.names[i], font = 2)
      mtext( line=5.5, side=2,param.units[i])}
  }
}
dev.off()

