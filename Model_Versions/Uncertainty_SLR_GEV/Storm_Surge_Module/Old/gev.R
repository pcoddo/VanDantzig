############################################
##  file: gev.R
##   R Example of performing basic stats on sample data
##   - Reads in hourly sea-level anomalies at Port of LA
##   - Fits a general extreme value (GEV) distribution to data
##   - Calculate emperical cumalitive distribution functions (CDF)
##      for data and fitted model
##   - Plot CDF curves and analyze flooding frequencies/risks
######################################################
##  authors: Ryan Sriver and Klaus Keller
##  copyright by the authors
##  distributed under the GNU general public license
##  no warranty
##################################################
# version: 1, Feb.12, 2012
#          2, March 16, 2015
##################################################
# how to run:
# - save the file in a directory
# - go to the directory with this file
# - open R
# - type 'source(gev.R)'
# - open the new pdf files to analyze the results
############################################


# read in formatted pola hourly sea level anomalies
data=read.table("monthly_block_maxima.txt", header = T)
  sla=data[,4] # sea level anomalies

print(summary(sla))
print(length(sla))

qlo=0.025
qhi=0.975

### Calculate and plot histogram
pdf(file="plot1.pdf")  # write to pdf, define a pdf file to write to
histogram=hist(sla, plot=TRUE, col="lightblue", main=("Histogram"),
  xlab=("Maximum hourly sea level anomaly [mm]")
)
  abline(v=mean(sla),lwd=3,col="red")
  abline(v=quantile(sla,qlo),lwd=3,col="red",lty="dashed")
  abline(v=quantile(sla,qhi),lwd=3,col="red",lty="dashed")
dev.off()


### Calculate and plot cdf
source("mycolors.R")
install.packages('fExtremes')  # install the fExtremes package if necessary
library(fExtremes) #load the fExtremes package
source("put_fig_letter.R")

pdf(file="gev.pdf", width = 6.5, height = 3.25, family="Times", pointsize=12)
png(file="gev.png", width = 6.5, height = 3.25, family="Times", pointsize=12, unit = "in", res = 600)
par(oma = c(0,0,0,0), mar = c(4,4,0,0)+0.25, mfcol = c(1,2)) 

ecdf_data=ecdf(sla)  # returns cdf function
quant_data=ecdf_data(sla)  # calculate quantiles
plot(sla[order(sla)],quant_data[order(sla)],type="l",col=myred, lwd=6,
  ylab="Probability",
  xlab="Monthly block maxima (cm)",
  las = 1)
put.fig.letter("A", location="bottomright", family = "Times", offset=c(-0.05, 0.3), cex = 1.5)

### Perform model fitting
fit=gevFit(sla, type="pwm") # estimates GEV parameters
  print(fit)
###########################################
### Estimated Parameters:
###          xi           mu         beta
###  0.08952961 182.80539958  1.28547819
###########################################

### fit model to given quantiles 
q=seq(0,1,length.out=1000000)  # quantile array
fit_q=qgev(q, xi=0.08952961, mu=182.80539958,  beta=1.28547819)

### add fitted model to plot
lines(fit_q, q, col="black", lwd=2)

### add a fitted normal model to the plot
lines(qnorm(q, mean(sla), sd(sla)), q, col=myblue, lwd=2)

### Add legend

# legend(x="bottomright",
#        inset=c(0.05,0.05),
#        legend=c("Observations","GEV Fit", "Normal Fit"),
#        col=c(myred,"black", myblue),
#        lty=1,
#        lwd=c(3,3,3),
#        bty = 'n',
#        cex = 0.85)
box(lwd = 1.5)
#dev.off()


############################################
### Plot survival function
### 1-cdf 
### emphasizes tail areas
############################################

#pdf(file="cdf_extreme.pdf", width = 3.25, height = 3.25, family="Times", pointsize=12)
#par(oma = c(0,0,0,0), mar = c(4,4,0,0)+0.25)
plot(sla[order(sla)],1-quant_data[order(sla)],
     type="l",
     col=myred,
     cex=1.5,log="y",
     ylim=c(0.001,1.), 
     xlim=c(180,200), 
     lwd=6,
     yaxt = 'n',
     ylab="1-CDF",xlab="Monthly block maxima (cm)")
#lines(fit_q,1-q,type="l",col="black",lwd=2)

axis(2, at=10^(seq(-3,1, by = 1)), label=parse(text=paste("10^", seq(-3,1, by = 1), sep="")), las=1)
#axis(2,at=c(1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1),
#  lab=c("","","","","","","",""))

###########################################
### Notice that fitted model does not do well in tails
### Tweak parameters for better fit
###
### Optimal GEV  Parameters:
###  shape: 	  xi=-0.3047
###  location:    mu=-175.735
###  scale:    	  beta=516.769
###########################################

opt_q=qgev(q, xi=0.08952961, mu=182.80539958,  beta=1.28547819)
lines(opt_q,1-q,type="l",col="black",lwd=2)

### What happens under conditions with larger mean sea level?
### Add 1 meter to mean
#lines(opt_q+5,1-q,type="l",col="green",lwd=2)

### What happens if we approximated the distribution as
### a normal distribution?
lines(qnorm(q, mean(sla), sd(sla)),1-q,type="l",col=myblue,lwd=2)

### Analyze flooding frequencies
#fl_50 = 1/50      # flood every 50 years
#fl_50 = fl_50 * 1/(365.25*24)    # convert to per-hour frequency

### Add reference lines
#abline(h=fl_50,lwd=3,col="black",lty="dashed")
#abline(v=1500,lwd=3,col="black",lty="dashed")

### Add legend
legend(x="topright",
       inset=c(0.05,0.03),
       bty = 'n',
       legend=c("Observations","GEV Fit","Normal Fit"),                   
       col=c(myred,"black", myblue),
       lty=1,
       lwd=3,
       cex = 0.9)
put.fig.letter("B", location="bottomright", family = "Times", offset=c(-0.05, 0.3), cex = 1.5)
box(lwd = 1.5)
dev.off()
