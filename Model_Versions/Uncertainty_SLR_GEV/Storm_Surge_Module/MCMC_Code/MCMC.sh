#!/usr/bin/env Rscript

#setwd("/home/scrim/skl5261/MCMCTest/9.22Test")

library(snow)
library(snowfall)
sfInit(parallel=TRUE, cpus=20, type="SOCK" )
sfSource("GEVMCMCSource.R")
load("MCMC_coredata.Rdata")

startparam<-c(10,10,10)
errvect<-c(20,0.2,0.2)
sdvect<-c(300,30,20)

sfExport("MCMC_coredata", "starparam", "errvect", "sdvect")
wrapper <- function(filetitle) {
result<-ALLVARnsGEVmh(dataset,100000,startparam,errvect,sdvect)
return(result)
}

result <- sfClusterApplyLB(1:20, wrapper)
save(result ,file="MCMC_parallel.RData")