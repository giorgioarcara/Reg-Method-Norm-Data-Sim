rm(list=ls())

source("R_functions/shadenormal.R")

dir.create("Figures/Others")

jpeg("Figures/Others/True_Score_SD.jpeg", res=250, width=600*3, height=600)
par(mfrow=c(1,3))
shadenormal(mean=0, sd=0.5, xlim=c(-5,5), xlab="True Adj Score", main="True Adj Score SD = 0.5")
shadenormal(mean=0, sd=1, xlim=c(-5,5), xlab="True Adj Score", main="True Adj Score SD = 1")
shadenormal(mean=0, sd=1.5, xlim=c(-5,5), xlab="True Adj Score",  main="True Adj Score SD = 1.5")
dev.off()

