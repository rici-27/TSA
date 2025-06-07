quakes = read.delim("quakes.txt")
View(quakes)

# Zeitreihen Objekt erstellen
quakes_ts <- ts(quakes$X13, start=1901)

# Lag1-Autocovariance und Autokorrelation
acf(quakes_ts, type="correlation")
acf(quakes_ts, type="covariance")
acf(quakes_ts, type="correlation", plot=FALSE)
acf(quakes_ts, type="covariance", plot=FALSE)
# könnte man auch selbst implementieren

# AR(1)-Model 
X <- quakes$X13[1:123]
Y <- quakes$X13[2:124]
ols_data <- data.frame(X, Y)
ols <- lm(X ~ Y, data=ols_data)
ols_coef <- ols[[1]]
a <- ols_coef["Y"]  # Steigung
b <- ols_coef["(Intercept)"]  # Achsenabschnitt

plot(quakes_ts)
years <- as.vector(time(quakes_ts)[1:123])
lines(years, a * X + b, col = "red")

pred <- a * quakes$X13[124] + b
pred
