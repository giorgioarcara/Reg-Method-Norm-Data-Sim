rm(list=ls())

source("R_functions/shadenormal.R")

dir.create("Figures/Others")

jpeg("Figures/Others/True_Adj_Score_SD.jpeg", res=250, width=600*3, height=600)
par(mfrow=c(1,3))
shadenormal(mean=0, sd=0.5, xlim=c(-5,5), xlab="True Adj Score", main="True Adj Score SD = 0.5")
shadenormal(mean=0, sd=1, xlim=c(-5,5), xlab="True Adj Score", main="True Adj Score SD = 1")
shadenormal(mean=0, sd=1.5, xlim=c(-5,5), xlab="True Adj Score",  main="True Adj Score SD = 1.5")
dev.off()


#sd effect
set.seed(100)
x = rnorm(200, mean=50, sd=10)
err_0.5 = rnorm(200, mean=0, sd=0.5)
err_1 = rnorm(200, mean=0, sd=1)
err_1.5 = rnorm(200, mean=0, sd=1.5)


y = -0.002*x^2 + 20
y_0.5 = -0.002*x^2 + 20 + err_0.5
y_1 = -0.002*x^2 + 20 + err_1
y_1.5 = -0.002*x^2 + 20 + err_1.5

dat = data.frame(x, y, y_0.5, y_1, y_1.5)

png("Figures/Others/True_Adj_Score_SD_regr.png",res=250, width=600*3, height=600)
par(mfrow=c(1,3))
plot(dat[order(dat$x), "x"], dat[order(dat$x), "y_0.5"], xlab="Predictor", ylab="Test Score", main="True Adj Score SD = 0.5", ylim=c(5,20))
lines(dat[order(dat$x), "x"], dat[order(dat$x), "y"])
plot(dat[order(dat$x), "x"], dat[order(dat$x), "y_1"], xlab="Predictor", ylab="Test Score",main="True Adj Score SD = 1", ylim=c(5,20))
lines(dat[order(dat$x), "x"], dat[order(dat$x), "y"])
plot(dat[order(dat$x), "x"], dat[order(dat$x), "y_1.5"], xlab="Predictor", ylab="Test Score", main="True Adj Score SD = 1.5", ylim=c(5,20))
lines(dat[order(dat$x), "x"], dat[order(dat$x), "y"])
dev.off()