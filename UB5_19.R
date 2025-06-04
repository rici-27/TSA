# Set seed for reproducibility
set.seed(975)


# Define AR(6) coefficients for a "normal" stationary process
phi <- c(2.4012, -2.7882, 2.3094, -1.4980, 0.6926, -0.1981)
p <- length(phi)
n <- 1000  # total length

# Generate normal AR(6) data
normalts <- arima.sim(n = n, model = list(ar = phi), sd = 1)
plot(normalts)

# Create anomaly data by adding a leak starting at t = 500

anomalyts <- normalts
leak_start <- 500
leak_length <- n - leak_start + 1

anomalyts <- normalts
anomalyts[leak_start:n] <- anomalyts[leak_start:n] + rnorm(leak_length, mean = 0, sd = 0.3)

#Optional: plot both series
ts.plot(normalts, col = "blue", lty = 2, main = "Synthetic Normal and Anomaly Data", ylab = "Signal")
lines(anomalyts, col = "red")
#legend("topright", legend = c("Normal", "Anomalous (with leak)"), col = c("blue", "red"), lty = c(2, 1))

# a)
yule_walker <- ar(normalts, aic=FALSE, order.max=6, method="yule-walker")
yw_coeff <- yule_walker[[2]]
yw_coeff

ols <- ar(normalts, aic=FALSE, order.max=6, method = "ols")
ols_coeff <- ols[[2]]

# b)

vorhersage <- predict(yule_walker, n.ahead=1)

v_yw <- rep(0, 1000)
v_ols <- rep(0, 1000)

for (i in 7:1000){
  v_yw[i] <- rev(yw_coeff) %*% anomalyts[(i-6):(i-1)]
  v_ols[i] <- rev(ols_coeff) %*% anomalyts[(i-6):(i-1)]
}

diff_yw <- (anomalyts - v_yw)**2
diff_ols <- (anomalyts - v_ols)**2

ts.plot(diff_yw[7:1000], col = "blue", main = "Pred Error Yule Walker", ylab = "Signal")
ts.plot(diff_ols[7:1000], col = "green", main = "Pred Error OLS", ylab = "Signal")




