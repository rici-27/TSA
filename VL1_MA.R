# MA(1)-Process with sigma = 1

# zunächst simulieren wir MA(1) mithilfe normalverteiltem Noise
sigma <- 1
n <- 300
epsilon <- rnorm(n+1, mean=0, sd=sigma) # erzeugt zufallsvariablen
theta <- 1
Y <- numeric(n+1) # erzeugt leeren vektor der länge n+1
?numeric
for(j in 1:n){
  Y[j+1] <- epsilon[j+1] + theta*epsilon[j]
}
plot(Y, type="l", col=4) # tpye l macht linien, type b punkte

acf(Y, lag.max=15, main="")
acf(Y, pl=FALSE) # so kriegt man werte ausgegeben
theta/(1+theta^2)

# jetzt wollen wir eine Monte Carlo Simulation durchführen
N <- 10000
n <- 100
rhohat1 <- numeric(N);

muhat <- numeric(N) # vektoren anlegen für MC Sim
for (k in 1:N){
  epsilon <- rnorm(n+1, mean=0, sd=sigma)
  Y <- numeric(n)
  for (j in 1:n){
    Y[j+1] <- epsilon[j+1] + theta*epsilon[j]
  }
  muhat[k] <- mean(Y)
  rhohat1[k] <- acf(Y, pl=FALSE)$acf[2]
}

# die empirische Verteilung der Schätzer für sample Größe n kann mit 
# der theoretischen asymptotischen Grenzverteilung verglichen werden

hist(muhat, breaks=20)
plot(density(sqrt(n) * muhat/(1+theta)), main="")
grid <- seq(-3.5, 3.5, length=1000)
lines(grid, dnorm(grid), col=4, lwd=2)

qqnorm(sqrt(n)*muhat/(1+theta))
qqline(sqrt(n)*muhat/(1+theta))

# fertig machen !


