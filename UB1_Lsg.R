# (a)
library(TSA)   #Install TSA and load data
data(airpass)


# Inspect the dataset


par(mfrow=c(1,2))
plot(airpass);acf(airpass)

# The data exhibit both a strong seasonal pattern and a nearly linear trend. 
# Since the variability of the data increases for larger values of  t, it may
# be appropriate to consider a logarithmic transformation of the data.


# (b)
logairpass<-log(airpass) # log transform


# Short comparison

par(mfrow=c(2,2))
plot(airpass);acf(airpass)
plot(logairpass);acf(logairpass)

# c) Removing the trend
# Method 1: 12-month moving average

par(mfrow=c(1,2))

trend_ma <- filter(logairpass, rep(1/12, 12), sides=1)
detrended_ma <- logairpass - trend_ma
plot(detrended_ma, main="Detrended (MA) Series")
acf(detrended_ma, na.action=na.pass,
    main="ACF of Detrended (MA) Series")

# Method 2: Linear regression

par(mfrow=c(2,2))

time <- time(logairpass)
fit<- lm(logairpass ~ time)
trend<- ts(fitted(fit), start=start(logairpass),frequency=frequency(logairpass))
detrended <- logairpass - trend
plot(logairpass);lines(trend,col="red");plot(detrended)
plot(detrended); acf(detrended)


# Method 3: Taking Differences

par(mfrow=c(1,2))
y<-diff(logairpass,differences=1)
plot(y);acf(y)


# d) Removing the seasonality: Difference with lag 12

z<-diff(y,lag=12)
plot(z);acf(z)

# Remark; Removing seasonality with a 12-period difference and removing trend by differencing
# can be done in either order they produce the same result.

par(mfrow=c(2,2))

x1<-diff(logairpass,lag=12)
x2<-diff(x1,differences=1)

plot(x2);acf(x2)
plot(z);acf(z)











