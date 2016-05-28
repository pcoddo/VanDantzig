###  beta distriution  ###

a=255 	  # define slr bounds (mm)
b=2508

s1 = 2    # define shape parameters
s2 = 3

xs <- seq(0, 1, length=100)   # define standard beta function on interval [0,1]
xt <- seq(a, b, length=100)   # transform distribution to slr bounds

beta_pdf<-dbeta(xs, s1, s2)

plot(xt,beta_pdf,type="l",lwd=5)

data <- read.table("array_beta.txt")


