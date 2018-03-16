###################################
# file: Pairs_parameters.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Produces pairwise scattter plots for model Parameters
# Utilizes plotutils.R function written by:
# Nathan Urban (nurban@lanl.gov)
#################################### 

# Set working directory
setwd("~/vanDantzig/Model_Versions/Uncertainty_SLR_GEV/Figures/Pairs Plots/")

# Pairs plot for parameters
#source("../../SLR_Module/Rejection_Sampling/plotutils.R")

# Load in augmented plotting utility:
plot.pairs <- function(x, smooth=FALSE, smooth.hist=FALSE, smooth.scatter=FALSE, smoothing=1, truncate.kde=TRUE, qlo=0.025, qhi=0.975, bold=TRUE, ...)
{
  if(smooth) {
    smooth.hist = TRUE
    smooth.scatter = TRUE	
  }
  
  histpanel <- function(x,smooth,...) {panel.hist(x,smooth.hist,smoothing,truncate.kde,qlo,qhi,...)}
  scatterpanel <- function(x,y,smooth,...) {panel.scatter(x,y,smooth.scatter,smoothing,truncate.kde,...)}
  
  pairs(x, lower.panel=scatterpanel, diag.panel=histpanel, upper.panel=panel.cor, font.labels=ifelse(bold,2,1), ...)
}

panel.hist <- function(x, smooth, smoothing=1, truncate.kde=TRUE, qlo=0.025, qhi=0.975, ...)
{
  if(smooth) { # kernel density estimate
    est <- ifelse(truncate.kde, bkde(x,bandwidth=smoothing*dpik(x),range.x=range(x)), bkde(x,bandwidth=smoothing*dpik(x))) # from KernSmooth
    #est <- ifelse(truncate.kde, density(x,adjust=smoothing,cut=0), density(x,adjust=smoothing))
    y <- est$y
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.75*max(y)) )
    if(truncate.kde) {
      polygon(c(min(x),est$x,max(x)),c(0,y,0),col="gray")
    } else {
      polygon(est,col="gray")	
    }
  } else { # histogram
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$density
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.75*max(y)) )
    rect(breaks[-nB], 0, breaks[-1], y, border=NA, col="gray", ...)
    n <- nB-1
    outline.x0 <- rep(NA, 2*n-1)
    outline.y0 <- rep(NA, 2*n-1)
    outline.x1 <- rep(NA, 2*n-1)
    outline.y1 <- rep(NA, 2*n-1)
    for(i in 1:n) {
      outline.x0[2*i-1] <- breaks[i]
      outline.y0[2*i-1] <- y[i]
      outline.x1[2*i-1] <- breaks[i+1]
      outline.y1[2*i-1] <- y[i]
      if(i < n) {
        outline.x0[2*i] <- breaks[i+1]
        outline.y0[2*i] <- y[i]
        outline.x1[2*i] <- breaks[i+1]
        outline.y1[2*i] <- y[i+1]
      }
    }
    segments(outline.x0,outline.y0,outline.x1,outline.y1)
  }
  
  # plot mean and qlo/50/qhi quantiles
  lines(c(mean(x),mean(x)), c(0,1.05*max(y)), col="red")
  lines(c(median(x),median(x)), c(0,1.05*max(y)), col="blue", lty="dashed")
  lines(c(quantile(x,qlo),quantile(x,qlo)), c(0,1.05*max(y)), col="blue", lty="dotted")
  lines(c(quantile(x,qhi),quantile(x,qhi)), c(0,1.05*max(y)), col="blue", lty="dotted")
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r, col="blue")
}

panel.scatter <- function (x, y, smooth, smoothing=1, col = par("col"), bg = NA, pch = 20, cex = 0.5, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  if(smooth) { # 2D kernel density estimate
    maxpts <- min(50000,length(x))
    idx <- seq(1,maxpts,length=maxpts); x <- x[idx]; y <- y[idx]
    z <- cbind(x,y)
    est <- bkde2D(z, bandwidth=smoothing*c(dpik(x),dpik(y))) # from KernSmooth
    l <- contourLines(est$x1, est$x2, est$fhat, levels=seq(0.05,0.95,0.15)*max(est$fhat))
    for(i in 1:length(l)) {
      lines(l[[i]]$x,l[[i]]$y)
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
  } else { # scatterplot
    # subsample points to prevent scatterplot from being too crowded
    nsamp <- 5000
    maxpts <- min(nsamp, length(x))
    idx <- sample(1:max(length(x),maxpts), maxpts); xs <- x[idx]; ys <- y[idx]
    points(xs, ys, pch = pch, col = "black", bg = bg, cex = cex)
    # ... but compute smoothed curve using full data set
    ok <- is.finite(xs) & is.finite(ys)
    if (any(ok)) 
      lines(stats::lowess(xs[ok], ys[ok], f = span, iter = iter), 
            col = col.smooth, ...)	
  }
}

#################
# Plot economic parameters
pdf("econ_param2.pdf")
plot.pairs(Parameters[3:5], labels = c("V", expression(delta*"'"), "k"))
dev.off()

# Plot sea-level rise parameters
pdf("SLR_param2.pdf") #Figure S4
plot.pairs(c(Parameters[6], beta.dist[1:5]), labels = c(expression(eta), "a", "b", "c", "t*", "c*"))
dev.off()

# Plot sea-level rise parameters
pdf("GEV_param2.pdf")
plot.pairs(GEV_param, labels = c(expression(mu), expression(xi), expression(sigma)))
dev.off()



