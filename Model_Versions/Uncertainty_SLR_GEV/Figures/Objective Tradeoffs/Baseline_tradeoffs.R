###################################
# file: Baseline_tradeoffs.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Produces manually adjusted pairwise scattter plots 
# for van Dantzig (1956) Objectives
#################################### 

# Panel plots (Uncertainty and baseline models)
df <- data.frame(total_costs.v/1e+06, costs.v/1e+06, NPV_expected_losses.v/1e+06, EV_p_exceed_transient.v)
df2 <- data.frame(total_costs.base/1e+06, costs.base/1e+06, NPV_expected_losses.base/1e+06, EV_p_exceed_transient.base)

param.names=c("NPV: Total Costs",
              "Investment Costs",
              "NPV: Damages",
              "Flood Risk")

param.units=c("(million Guilders)",
              "(million Guilders)",
              "(million Guilders)",
              "(1/yr)")

# Baseline curve panel plot (pairs):
pdf("Baseline_tradeoffs.pdf", width = 7, height = 7)
par( oma=c(8,8,5,1),mfrow=c(3,3),mar=c(1,1,0,0))
d=ncol(df)

for(i in 1:(d-1))
{
  for(j in 1:(d-1))
  {
    if(i<((d+1)-j))
    {
      panel=data.frame(cbind(df[,(d+1)-j],df[,i]))
      panel2=data.frame(cbind(df2[,(d+1)-j],df2[,i]))
      plot(panel2$X1, panel2$X2, type = 'l', 
           lwd = 2,
           axes = FALSE)
      points(0, 0, col = rgb(1, 102/255, 102/255, 1), pch = 8, cex = 1.5)
      box(lwd = 2)
      axis(side = 1, tck = -.045, labels = NA, lwd = 2)
      axis(side = 2, tck = -.045, labels = NA, lwd = 2)
    
    } else
    {
      plot.new()
    }
    if(i==(d-1)&((d+1)-j)==2)
{
      legend("bottomright", 
           lty = c(1, NA),
           pch = c(NA, 8),
           lwd = c(2.5, 1),
           c("Storm Surge Uncertainty", "Ideal Solution"),
           col = c("black", rgb(1, 102/255, 102/255, 1)),
           pt.cex = c(NA, 1.5),
           cex = 1.4, box.lwd = 1.5,
           inset = c(0.1, -0.1),
           xpd = NA)
}
if( (i+j)==d ){
  axis( 1, cex.axis=1.5,)
  mtext( line=3.5, side=1,param.names[(d+1)-j], font = 2 )
  mtext( line=5, side=1,param.units[(d+1)-j] )}
if( ((d+1)-j)==d ){
  axis( 2, cex.axis=1.5, las = 1)
  mtext( line=7, side=2,param.names[i], font = 2)
  mtext( line=5.5, side=2,param.units[i])}
  }
}
dev.off()


# Baseline pairs on log scale
pdf("Baseline_tradeoffs_log.pdf", width = 6, height = 6)
par( oma=c(8,8,5,1),mfrow=c(3,3),mar=c(1,1,0,0))
d=ncol(df)

for(i in 1:(d-1))
{
  for(j in 1:(d-1))
  {
    if(i<((d+1)-j))
    {
      panel=data.frame(cbind(df[,(d+1)-j],df[,i]))
      panel2=data.frame(cbind(df2[,(d+1)-j],df2[,i]))
      plot(panel2$X1, panel2$X2, type = 'l', log = 'xy',
           lwd = 2,
           axes = FALSE)
      points(panel2$X1[min_ind], panel2$X2[min_ind], col = rgb(1, 102/255, 102/255, 1), pch = 8, cex = 1.5)
      box(lwd = 2)
      axis(side = 1, tck = -.045, labels = NA, lwd = 2)
      axis(side = 2, tck = -.045, labels = NA, lwd = 2)
      
    } else
    {
      plot.new()
    }
    if(i==(d-1)&((d+1)-j)==2)
    {
      legend("bottomright", 
             lty = c(1, NA),
             pch = c(NA, 8),
             lwd = c(2.5, 1),
             c("Storm Surge Uncertainty", "EUM Solution"),
             col = c("black", rgb(1, 102/255, 102/255, 1)),
             cex = 1.1)
    }
    if( ((i+j)==d)&((d+1)-j)==2 | ((i+j)==d)&((d+1)-j)==3 ){
      axis( 1, cex.axis=1.5,)
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

