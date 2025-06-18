library(ggplot2)

tab <- read.table("citi.txt")

# (a) Illustrate the price evolution over the trading day.
plot(tab$time,tab$trade,type="l")

ggplot(data, aes(x = time, y = trade)) +
  geom_point(size = 0.01)

# (b) determine the vector of price changes which are non-zero and illustrate their pattern.
# In the sequel, this vector is the time series of interest.

xx<-diff(tab$trade)

#Alternative code that works without the diff function
#n<-length(tab$trade)
#xx<-(tab$trade[2:n]-tab$trade[1:(n-1)])


x<-xx[which(abs(xx)>0)]
tx<-tab$time[which(abs(xx)>0)]
plot(tx,x)

# (c) Plot the empirical autocorrelations. We shall model the time series as an MA(1)
#process. Comment on reasons why this could be reasonable, or not.


acf(x,lag.max=15,main="")
acf(x,pl=FALSE)

# (d) Using the MA(1) model for the time series, determine an asymptotic 99% confidence
#interval for rho(2), i.e. the autocorrelation at lag 2. You can use exercise 8.

# acf[3] entspricht lag 2, weil acf[1] = rho(0).
acf(x,pl=FALSE)$acf[3] 
acf(x,pl=FALSE)$acf[3]-1/sqrt(length(x))*sqrt(1+2*(acf(x,pl=FALSE)$acf[2])^2)*qnorm(.995)
acf(x,pl=FALSE)$acf[3]+1/sqrt(length(x))*sqrt(1+2*(acf(x,pl=FALSE)$acf[2])^2)*qnorm(.995)

# (e) Using the MA(1) model for the time series, determine an asymptotic 99% confidence
#interval for rho(1), i.e. the autocorrelation at lag 1. You can use Example 1.24. Note,
#however, that we have set s2 = 1 in this example.

acf(x,pl=FALSE)$acf[2]
acf(x,pl=FALSE)$acf[2]-1/sqrt(length(x))*sqrt(1-3*(acf(x,pl=FALSE)$acf[2])^2+4*(acf(x,pl=FALSE)$acf[2])^4)*qnorm(.995)
acf(x,pl=FALSE)$acf[2]+1/sqrt(length(x))*sqrt(1-3*(acf(x,pl=FALSE)$acf[2])^2+4*(acf(x,pl=FALSE)$acf[2])^4)*qnorm(.995)

#acf(2*x,pl=FALSE)$acf[2]
#acf(2*x,pl=FALSE)$acf[2]-1/sqrt(length(x))*sqrt(1-3*(acf(2*x,pl=FALSE)$acf[2])^2+4*(acf(2*x,pl=FALSE)$acf[2])^4)*qnorm(.995)
#acf(2*x,pl=FALSE)$acf[2]+1/sqrt(length(x))*sqrt(1-3*(acf(2*x,pl=FALSE)$acf[2])^2+4*(acf(2*x,pl=FALSE)$acf[2])^4)*qnorm(.995)

# (f) How can we estimate the parameter theta Determine a point estimate of ?. Determine,
#moreover, a point estimate of s2.

thetahat<-(1-sqrt(1-4*(acf(x,pl=FALSE)$acf[2])^2))/(2*(acf(x,pl=FALSE)$acf[2])) # Momentestimator
sigmahat<-acf(x,pl=FALSE,type = c("covariance"))$acf[1]/(1+thetahat^2)

# (g) Determine an asymptotic 99% confidence interval for the mean mu of the time series.

muhat<-mean(x)
muhat-1/sqrt(length(x))*sqrt(acf(x,pl=FALSE,type = c("covariance"))$acf[1]+2*acf(x,pl=FALSE,type = c("covariance"))$acf[2])*qnorm(.995)
muhat+1/sqrt(length(x))*sqrt(acf(x,pl=FALSE,type = c("covariance"))$acf[1]+2*acf(x,pl=FALSE,type = c("covariance"))$acf[2])*qnorm(.995)

