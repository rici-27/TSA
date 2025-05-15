# MA(1)-Process with sigma = 1

sigma <- 1
n <- 300
epsilon <- rnorm(n+1, mean=0, sd=sigma) # erzeugt zufallsvariablen
theta <- 1
Y <- numeric(n+1)
?numeric
for(j in 1:n){
  Y[j+1] <- epsilon[j+1] + theta*epsilon[j]
}
plot(Y, type="b", col=4)

acf(Y, lag.max=15, main="")
acf(Y, pl=FALSE)
theta/(1+theta^2)