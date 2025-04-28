library(TSA)
library(ggplot2)
library(ggfortify)

data(package = "TSA")

data("airpass")
airpass

log_airpass <- log(airpass)
log_airpass

autoplot(log_airpass)


window_size <- 12
trend_component <- filter(log_airpass, rep(1/window_size, window_size), sides = 2)
log_airpass_detrended <- log_airpass - trend_component

View(log_airpass_detrended)
autoplot(log_airpass_detrended)

differenced_airpass <- diff(log_airpass, lag = 12)

autoplot(differenced_airpass)