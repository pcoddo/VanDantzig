############################################
##  file: unif_delfzijl.R
##   - Application to sea level rise (SLR) projections 
#
######################################################
##  Authors: Klaus Keller (klaus@psu.edu) and Ryan Sriver 
##  copyright by the authors
######################################################
##  distributed under the GNU general public license
##  no warranty
##  This is PRELIMINARY RESEARCH CODE, USE AT YOUR OWN RISK AND ONLY
##  AFTER YOU CONVINCED YOURSELF THAT THIS IS OK TO USE FOR YOUR APPLICATION 
##################################################
# version: 4
# last changes:
# August 26 2015
# Perry Oddo (poddo@psu.edu)
##################################################
# how to run:
# - save the file in a directory
# - go to the directory with this file
# - open R
# - the file array_raw.txt is a table of the resulting model parameters for the prior 
#	- after rejection resampling to fit the chain to theoretical slr distribution
# - the entries are (index, a, b, c, tstar, cstar, projected slr in 2100 in mm)
############################################

# Set working directory
setwd("~/Documents/Grad/SCRiM/vanDantzig/Model_Versions/Uncertainty_SLR_GEV/SLR_Module")

# Compile
rm(list = ls())
graphics.off()

library(compiler)
enableJIT(3)

# Set seed for reproducibility 
set.seed(1)

# Load libraries
library(timeSeries)

# Set current year
year = 2015

# Define the number of bootstrap/mc samples
N=55000  # Used throughout analysis

# read in the local data from Delfzijl tide gauge
slr.global.data <- read.csv("Data/delfzijl.csv", header = T)

# Fit a simple polynominal model to the data and extract the year 2000 prediion
years.global=slr.global.data[,2]
years.global=years.global-(year)  # normalize time series to current year 2015
  nyears.global=length(years.global)
slr.global=slr.global.data[,3] * 10 # convert sea level to mm

# 2nd order polynomial ~ a + bx + cx^2
fit.global=lm(slr.global ~ years.global + I(years.global^2))

# scale the observations such that the slr anomalie is zero in the year 2000
predict.global.2000=predict(fit.global, newdata=data.frame(years.global=-15))
slr.global=slr.global-predict.global.2000		# data
predict.hind=predict(fit.global)-predict.global.2000	# poly fit

### Calculate Residuals during observed time series (data - polynomial fit)  ###
res=slr.global-predict.hind
res_stdev=sd(res[length(res)-50:length(res)]) # stdev for the final 50 years of data
print(res_stdev)

# write-out the residuals
residuals.annual=mat.or.vec(2,nyears.global) #(nr,nc)
residuals.annual[1,]=years.global+year
residuals.annual[2,]=res
write.table(residuals.annual,
            file="./Output/residuals.annual.ascii",
            row.names=FALSE,
            col.names=FALSE
            )
           

### Bootstrap the residuals ###
### confine to post-1900 portion of time series ###
white.boot = mat.or.vec(N, nyears.global) # create matrix (nr,nc)
white.boot_sd = rep(NA,N)

  for(i in 1:N) {
    white.boot[i,1:nyears.global] = sample(res[23:nyears.global],size=nyears.global,replace=TRUE)
    white.boot_sd[i] = sd(white.boot[i,])
  }

### create new residuals from bootstraps with original AR structure
pac <- pacf(res[23:nyears.global], lag.max=5, plot=FALSE)  # apply partial auto-correlation to determine correlation coefficients
  print(pac$acf)

res.boot=mat.or.vec(N, nyears.global) #(nr,nc)

  for(n in 1:N) {
    for(i in 2:nyears.global) {
        res.boot[n,i] = pac$acf[1]*res.boot[n,i-1] + rnorm(1,mean=0,sd=white.boot_sd[n]) 
    }
  }


### apply new AUTOCORRELATED residuals to polynomial fit
slr.boot=mat.or.vec(N, nyears.global) #(nr,nc)

  for(i in 1:N) {
    slr.boot[i,]=predict.hind+res.boot[i,]
  }

### plot observations and polynomial fit with superimposed autocorrelated bootstraps ###
source("Scripts/mycolors.R")

pdf(file="Figures_uniform/hindcast.pdf", height = 4.5, width = 6)  # write to pdf, define a pdf file to write to
par(oma = c(0,0,0,0)+0.1, mar = c(4,5,1,1)+0.1)
plot(years.global+year, slr.global/1000, type = 'n', 
     xlab=("Year"),
     ylab=("Mean sea-level anomaly [cm] \nwith respect to the year 2000"),
     xaxs = 'i',
     las = 1,
     xlim=c(1879,2015),
     ylim=c(-3.25, 1.50))

# add bootstrap samples
nexamp=50
nrand=sample(N, nexamp)
  for(i in 1:nexamp) {
    lines(years.global+year, slr.boot[nrand[i], 1:nyears.global]/100, col=mybluealpha1, lwd=0.5)
  }
points(years.global+year, slr.global/100, pch = 21, bg = myred, cex = 0.5)
lines(years.global+year, predict.hind/100, lty = 1, lwd = 2)
abline(h = 0, lty = 2)

legend(x="topleft",
       legend=c("Observations [Delfzijl tide gauge]", 
                "Polynomial best fit projection", 
                "Best fit + bootstrap residuals"),
       col = c("black", "black", myblue),
       pt.bg = c(myred, NA, NA),
       pch = c(21, NA, NA),
       lwd = c(NA, 2, 1),
       lty=c(NA,"solid","solid"),
       bty = 'n',
       inset = c(0.01,0))
box(lwd = 1.5)
dev.off()

# define the extrapolation, predict, and center on 2000
years.extra = seq(1879, 2100)
years.extra = years.extra - year
#years.extra=seq(-211,89, length=300) # using jevrejava annual data 1800-2100 (t0=2011)
n.years.extra=length(years.extra)
predict.global=predict(fit.global,newdata=data.frame(years.global=seq(-136,85,length=222)))-predict.global.2000


### calculate polynomial coefficients for projections ###
boot.fit_coef=mat.or.vec(N, 3)
boot.fit_predict=mat.or.vec(N, n.years.extra)
for(i in 1:N) {
 fit.new=lm(slr.boot[i,] ~ years.global + I(years.global^2))
  boot.fit_coef[i,1]=fit.new$coefficients[1]
  boot.fit_coef[i,2]=fit.new$coefficients[2]
  boot.fit_coef[i,3]=fit.new$coefficients[3]
  boot.fit_predict[i,]=predict(fit.new, newdata=data.frame(years.global=seq(-136,85,length=222))) - 
	predict(fit.new, newdata=data.frame(years.global=-15))
  }


# define the scenario models
slrmodel <- function(times, slr.normal, c, t.star)
  # this function adds the scenarios for unresolved processes using a
  # time t.star at which the change happen and an additional sea level
  # rate c after t.star
  # example call: foo=slrmodel(years.extra,predict.global,1,2050)
{
  n.points=length(times)
  slr.scenario=slr.normal
   for(i in 1:n.points)
     {
      if (times[i] > t.star)
        {
        slr.scenario[i]=slr.normal[i]+(times[i]-t.star)*c
        }
     }
    return (slr.scenario)
}

# produce an example scenario projections
scenario.global=slrmodel(years.extra, predict.global,17,4)


# produce many slr scenarios drawing from the prior
###################################################
c.sample=mat.or.vec(N,1) #(nr,nc)
t.star.sample=mat.or.vec(N,1)
scenario.mc=mat.or.vec(N,n.years.extra)
for(n in 1:N)
  {
  # draw random samples from a uniform distribution
  c.sample[n]<-runif(1, min=-15, max=35)
  t.star.sample[n]<-runif(1, min=0, max=89)
  # produce the scenario
  scenario.mc[n,]=slrmodel(years.extra,boot.fit_predict[n,],c.sample[n],t.star.sample[n])
  }

scenario.poly.only=scenario.mc   # define the polynomial projections (used as input for pdfs)


### calculate projected residuals ###
res.boot_proj=mat.or.vec(N, n.years.extra) #(nr,nc)

 for(n in 1:N) {
    for(i in 2:n.years.extra) {
        res.boot_proj[n,i] = pac$acf[1]*res.boot_proj[n,i-1]  + rnorm(1,mean=0,sd=white.boot_sd[n])
    }
  }



### superimpose residuals on polynomial fit projections ###
  for(i in 1:N) {
    scenario.mc[i,]=scenario.mc[i,]+res.boot_proj[i,]
  }



# cut the scenarios within the window
ub.2100=2508
lb.2100=255
# see notes Aug. 16th.
#(pfeffer + thermosteric + reg. uncert.) )

# define arrays for all parameters and slr time series
array=mat.or.vec(N,6)
colnames(array) = c("a","b","c","t.star","c.star","slr")
scenario.filter=mat.or.vec(N,n.years.extra)

for(n in 1:N)
  {
    if (scenario.poly.only[n,n.years.extra] < ub.2100 && scenario.poly.only[n,n.years.extra] > lb.2100)
      {
       array[n,1:3]=boot.fit_coef[n,]
       array[n,4]=t.star.sample[n] + year
       array[n,5]=c.sample[n]
       array[n,6]=scenario.poly.only[n,n.years.extra]

       scenario.filter[n,]=scenario.mc[n,]
      } 
    else 
     {
      array[n,]=NA	# set out of bound entries to missing values
      scenario.filter[n,]=NA
     }
  }  


array=removeNA(array)
scenario.filter=removeNA(scenario.filter)  # remove missing values
  print(array)


# projections figure
pdf(file="Figures_uniform/projections.pdf", height = 4.5, width = 6)  
par(oma = c(0,0,0,0)+0.1, mar = c(4,5,1,1)+0.1)
plot(years.global+year, slr.global, type="n",
     xlab=("Year"),
     ylab=("Mean sea-level anomaly [mm] \nwith respect to the year 2000"),
     xlim=c(1879,2100),
     ylim=c(-300,2600),
     xaxs = 'i')

# add projected bootstrap samples
nexamp=50
nrand=sample(N/2,nexamp)  # decrease sampling due to cutting off outliers
  for(i in 1:nexamp) {
    lines(years.extra+year, scenario.filter[nrand[i],], col = "#0080FF40", lwd=0.5)
  }
points(years.global+year, slr.global, pch = 21, bg = myred, cex = 0.5)
lines(years.extra+year,predict.global, col="black", lwd = 1.5)

# add CA scenarios
#arrows(2100,780,2100,1768,col="gray",lty="solid",code=3);
# add sriver et al version of the Pfeffer et al scenarios
#arrows(2100,lb.2100,2100,ub.2100,col="blue",lty="solid",code=3);

# add the zero line top guide the eye
abline(h=0, lty=2)

legend(x="topleft",
       legend=c("Observations [Delfzijl tide gauge]",	
                'Polynomial best fit projection',
                'Model scenarios'),
       pch = c(21, NA, NA),
       pt.bg = c(myred, NA, NA),
       col = c("black","black", myblue),
       lty = c(NA, 1, 1),
       lwd = c(NA, 2, 1),
       bty = 'n',
       inset = c(0.01, 0))
box(lwd = 1.5)
dev.off()

write.table(array,file="./Output/array_raw.txt",quote=FALSE,col.names=FALSE)

###############################################
### Thin the model output to fit theoretical
### distribution of 2100 slr
###   - using rejection sampling
################################################


###  beta distriution  ###

a=lb.2100 # define slr bounds
b=ub.2100
n.bin=50  # number of bins for rejection sampling

s1 = 2    # define shape parameters (yields good fit to expert priors -- pfeffer, sriver, etc.)
s2 = 3

xs <- seq(0, 1, length=n.bin)	# define standard beta function on interval [0,1]
xt <- seq(a, b, length=n.bin)   # transform distribution to slr bounds

beta_pdf<-dbeta(xs, s1, s2)
  print(beta_pdf)

### check that pdf integrates to 1 ###
beta_wt_test=beta_pdf*(xs[2]-xs[1])  # standard distribution
  print(sum(beta_wt_test))
#beta_wt=beta_pdf*(xt[2]-xt[1])/(b-a) # transformed distribution
beta_wt=beta_pdf/sum(beta_pdf)
  print(sum(beta_wt))

# get the uniform pdf
x.sample.CA=seq(0,2500,length=1000)
lb.CA=10
ub.CA=2060
uniform.fit.CA=dunif(x.sample.CA, min=lb.CA, max=ub.CA, log = FALSE)

################################################

### read in model output ###
array = read.table("./Output/array_raw.txt")
x.bin=n.bin-1

### read through slr and bin the values accordingly ###
nrows=length(array[,1])
bin.array=array(NA, dim=c(x.bin,nrows,7))

for (i in 1:x.bin)
{
 for(n in 1:nrows)                                         
  {
    if (array[n,7] > xt[i]  && array[n,7] < xt[i+1])
      {
       bin.array[i,n,1]=array[n,1]
       bin.array[i,n,2]=array[n,2]
       bin.array[i,n,3]=array[n,3]
       bin.array[i,n,4]=array[n,4]
       bin.array[i,n,5]=array[n,5]
       bin.array[i,n,6]=array[n,6]
       bin.array[i,n,7]=array[n,7]
      }
  }
}


print(bin.array[1,,])

### thin the binned data to fit the uniform distribution ###

### test on first sample ###
bin.sub=bin.array[1,,]
bin.sub=removeNA(bin.sub)
  print(bin.sub)

sample.size=uniform.fit.CA[1]*nrows
  print(sample.size)
sample.ind=sample(bin.sub[,1],sample.size+1,replace=TRUE)  # add one sample to ensure no empty bins 
  print(sample.ind)
unif.dist=array(NA, dim=c(length(sample.ind),6))
colnames(unif.dist) = c("a","b","c","t.star","c.star","slr")
  print(unif.dist)

for(i in 1:length(sample.ind))
  {
#   beta.dist[i,1]=array[sample.ind[i],1]
   unif.dist[i,1]=array[sample.ind[i],2]
   unif.dist[i,2]=array[sample.ind[i],3]
   unif.dist[i,3]=array[sample.ind[i],4]
   unif.dist[i,4]=array[sample.ind[i],5]
   unif.dist[i,5]=array[sample.ind[i],6]
   unif.dist[i,6]=array[sample.ind[i],7]
  }
  print(unif.dist)

### repeat for rest of chain ###
for (n in 2:x.bin)
  {
   bin.sub_tmp=bin.array[n,,]
   bin.sub_tmp=removeNA(bin.sub_tmp)
   sample.size_tmp=uniform.fit.CA[n]*nrows
   sample.ind_tmp=sample(bin.sub_tmp[,1],sample.size_tmp+1,replace=TRUE)
   unif.dist_tmp=array(NA, dim=c(length(sample.ind_tmp),6))

  for(i in 1:length(sample.ind_tmp))
    {
#     beta.dist_tmp[i,1]=array[sample.ind_tmp[i],1]
     unif.dist_tmp[i,1]=array[sample.ind_tmp[i],2]
     unif.dist_tmp[i,2]=array[sample.ind_tmp[i],3]
     unif.dist_tmp[i,3]=array[sample.ind_tmp[i],4]
     unif.dist_tmp[i,4]=array[sample.ind_tmp[i],5]
     unif.dist_tmp[i,5]=array[sample.ind_tmp[i],6]
     unif.dist_tmp[i,6]=array[sample.ind_tmp[i],7]
    }

   unif.dist=rbind(unif.dist,unif.dist_tmp)  #combines bins into single array
  }

print(unif.dist)

### plot pdf ###
d.array=hist(array[,7], breaks=xt,plot=FALSE)
d.unif.dist=hist(unif.dist[,6],breaks=xt,plot=FALSE) # use histograms for easier weighting

pdf(file="Figures_uniform/fitted_unif.pdf",height=10, width=6) 
par(mfcol=c(2,1))

# panel A: plot the beta distribution fit to Pfeffer

plot(xt,beta_wt,type="l",col="black",
     lwd=4,xlim=c(-50,3000),
     ylim=c(0,0.05),
     xlab="Projected Sea-Level Rise in 2100 [mm]",
     ylab="Probability density function", main="(a) Extended Scenario of Pfeffer et al (2008)")
# add the zero line
 lines(xt,beta_pdf*0,lwd=1,lty=2,col="gray")
# add an arrow for the range of the distribution
 y.loc.arrow.a=9.0e-4;
#arrows(lb.2100,y.loc.arrow.a,ub.2100,y.loc.arrow.a,col="black",lty="solid",code=3);
 abline(v=lb.2100,lwd=1,col="blue",lty=2)
 abline(v=ub.2100,lwd=1,col="blue",lty=2)

#lines(d.array$mids,d.array$counts/length(array[,7]),lwd=3,lty=2,col="blue")

# add normalized fitted beta distribution
  lines(d.unif.dist$mids,d.unif.dist$counts/length(unif.dist[,6]),lwd=3,lty=2,col="red")

# check that normalized distributions sum to one
  print(sum(d.array$counts)/length(array[,7]))
  print(sum(d.unif.dist$counts)/length(unif.dist[,6]))
  print(sum(beta_wt))



legend(1600,0.045, inset=.2,
c("bounds","Pdf fit","Model Approximation"),
   lwd=2, col=c("blue","black","red"), cex=.7)

# panel B: plot the uniform dist fit to the CA scenarios
plot(x.sample.CA,uniform.fit.CA,type="l",col="black", lwd=4,
     xlim=c(-50,3000),
     ylim=c(0,6e-4),
     xlab="Projected Sea-Level Rise in 2100 [mm]",
     ylab="Probability density function", main="(b) Extended Scenario of Co-CAT (2010)")
# add the zero line
lines(x.sample.CA,uniform.fit.CA*0,lwd=1,lty=2,col="gray")
#arrows(lb.2100,y.loc.arrow.a,ub.2100,y.loc.arrow.a,col="black",lty="solid",code=3);
abline(v=lb.CA,lwd=1,col="blue",lty=2)
abline(v=ub.CA,lwd=1,col="blue",lty=2)
legend(2100,5.5e-4, inset=.2,
c("bounds","Pdf fit"),
   lwd=2, col=c("blue","black"), cex=.7)

dev.off()   #write file


# Nathan's resources for marginals and pair plots
source("../Scripts/plotutils.R")

figure(pdf(file="Figures_uniform/marginals.pdf"))
plot.marginals(unif.dist) 
dev.off()

figure(pdf(file="Figures_uniform/pairs.pdf"))
plot.pairs(unif.dist)
dev.off()

write.table(unif.dist,file="./Output/array_uniform.txt", quote=FALSE, col.names=TRUE, row.names = FALSE)


# zoom into the joint distribution of t.star and c.star 
figure(pdf(file="Figures_uniform/cstar_tstar.pdf"))
plot(unif.dist[,4],unif.dist[,5],pch=16,cex=0.2,col="black",
     xlab="t* estimates [Years after 2011]",
     ylab="c* estimate [mm/a]", main=" ")
dev.off()

#############################
# Plot slr scenarios for rejection-sampled parameters

scenario.constrain=mat.or.vec(length(unif.dist[,1]), n.years.extra)
for(n in 1:length(unif.dist[,1]))
{
  # draw samples from beta distribution for slr model
  scenario.constrain[n,]=slrmodel(years.extra+2015, boot.fit_predict[n,],unif.dist[n,5],unif.dist[n,4])
}

scenario.constrain_poly = scenario.constrain

# Apply residual structure to polynomial projections
for(i in 1:length(unif.dist[,1])) {
  scenario.constrain[i,]=scenario.constrain[i,]+res.boot_proj[i,]
}

matplot(years.extra+2015, t(scenario.constrain[nrand,]), type = 'l', lty = 1, col = "black")
matlines(years.extra+2015, t(scenario.constrain_poly[nrand,]), type = 'l', lty = 1, col = myred, lwd = 0.5)

points(years.global+2015, slr.global)

# Parameters for adding marginal histogram of SLR at 2100
vals <- scenario.constrain[,222]
A <- hist(vals)

# Save output
save.image(file = "unif_sl.RData")

# Plot projectiosn with marginal pdf of SLR at 2100
pdf("Figures_uniform/projections_constrain.pdf", width = 6.5, height = 4.5)
par(oma = c(0,4.75,0,0.25), mar = c(4,0,0.5,0), mfcol = c(1,2))
layout(matrix(c(1,2), 1, 2, byrow = T), widths = c(5.25, 1.25), heights = c(4.5, 4.5))
plot(years.global+year, slr.global/100, type="n",
     xlab=("Year"),
     #ylab=("Mean sea-level anomaly [m] \nwith respect to the year 2000"),
     xlim=c(1879,2100),
     ylim=c(-300,2500)/1000,
     las = 1,
     xaxs = 'i')
mtext("Mean sea-level anomaly [meters] \nwith respect to the year 2000", side = 2, line = 2.5, outer = FALSE)

# Source van Dantzig (1956) priors
source("../Scripts/priors.R")

# add projected bootstrap samples
nexamp=100
nrand=sample(814/2,nexamp)  # decrease sampling due to cutting off outliers
high.proj = seq(500, length(scenario.constrain[,1])) # sample scenarios at high end
max.proj = which.max(scenario.constrain[,222]) # maximum projection at year 2100
nrand.high = sample(high.proj, 25)
for(i in 1:nexamp) {
  lines(years.extra+year, scenario.constrain[nrand[i],]/1000, col = "#0080FF40", lwd=0.75)
  lines(years.extra+year, scenario.constrain[nrand.high[i],]/1000, col = "#0080FF40", lwd=0.75)
}
lines(years.extra+year, scenario.constrain[max.proj,]/1000, col = "#0080FF40", lwd=0.75)
points(years.global+year, slr.global/1000, pch = 21, bg = myred, cex = 0.5)
lines(years.extra+year,predict.global/1000, col="black", lwd = 1.5)
lines(2015:2100, 1:86*priors$sea_level_rate, col = mygreen, lwd = 3)

# add the zero line top guide the eye
abline(h=0, lty=2)

# vertical line at present day
abline(v = 2015, lty = 3)

# text for hindcasts and projections
text(x=2000, y = 1.25, labels = "Hindcast", adj = c(1, 0.5), font = 3, cex = 0.9)
text(x=2030, y = 1.25, labels = "Projections", adj = c(0, 0.5), font = 3, cex = 0.9)

legend(x="topleft",
       legend=c("Observations",	
                'Polynomial best fit projection',
                'Rejection sample projections',
                'van Dantzig [1956] projection'),
       pch = c(21, NA, NA, NA),
       pt.bg = c(myred, NA, NA, NA),
       col = c("black","black", myblue, mygreen),
       lty = c(NA, 1, 1, 1),
       lwd = c(NA, 2, 1, 2),
       bty = 'n',
       inset = c(0.01, 0))
box(lwd = 1.5)

# Add slr marginals at year 2100

plot.marginals.2 <- function(chain, smoothing=1, truncate.kde=TRUE)
{
 
    x <- chain
    est <- if(truncate.kde){bkde(x,bandwidth=smoothing*dpik(x),range.x=range(x))} else {bkde(x,bandwidth=smoothing*dpik(x))} # from KernSmooth
    y <- est$y
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5*max(y)) )
    
    # plot pdf (smoothed kernel density estimate)
    plot.default(NA, ylim=c(-400, max(x)), xlim=c(0,max(y)), axes = FALSE, ylab = "", xlab = "")
    if(truncate.kde) {
      polygon(c(0,y,0), c(min(x),est$x,max(x)), col="gray")
    } else {
      polygon(est,col="gray")	
    }
      
}  


plot.marginals.2(scenario.constrain[,222])

dev.off()
