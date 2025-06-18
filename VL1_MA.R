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

# die asymptotische varianz von mu kann man sich auch anschauen (bsp 1.19)
mean(muhat)
var(muhat) * n
(1+theta)^2
# bsp 1.24 wir kennen die asymptotische varianz vom rho schätzer:
mean(rhohat1) # monte carlo average über hatrho1
# hat immer negativen bias!!!
# größere abweichung als muhat von mu
var(rhohat1)*n 
1-3*(theta/(1+theta^2))^2 + 4*(theta/(1+theta^2))^4

# die empirische Verteilung der Schätzer für sample Größe n kann mit 
# der theoretischen asymptotischen Grenzverteilung verglichen werden

hist(muhat, breaks=20)
plot(density(sqrt(n) * muhat/(1+theta)), main="") # rescaled emp mean
grid <- seq(-3.5, 3.5, length=1000)
lines(grid, dnorm(grid), col=4, lwd=2)
# unser clt sagt wir sind nah an einer normalverteilung

# wir vergleichen die quantile und damit die verteilung:
qqnorm(sqrt(n)*muhat/(1+theta)) # normalized estimators
qqline(sqrt(n)*muhat/(1+theta))
# quantile quantile plot macht es leichter die tails anzuschauen

# jetzt histogram von rhohat1
hist(rhohat1, breaks=20)
# nochmal kernel density
plot(density(sqrt(n)*(rhohat1-theta/(1+theta^2))/sqrt(1-3*(theta/(1+theta^2))^2+4*(theta/(1+theta^2))^4)),main="")
lines(grid,dnorm(grid),col=4,lwd=2)
# hier sieht man die verschiebung durch den negativen bias!
# finite sample bias! sieht man auch gut an:
hist(sqrt(n)*(rhohat1-theta/(1+theta^2))/sqrt(1-3*(theta/(1+theta^2))^2+4*(theta/(1+theta^2))^4),freq=F,breaks=30,main="",xlab="",ylab="")
lines(grid,dnorm(grid),col=4,lwd=2)
# es ist keine verzerrung sondern eine verschiebung
qqnorm(sqrt(n)*(rhohat1-theta/(1+theta^2))/sqrt(1-3*(theta/(1+theta^2))^2+4*(theta/(1+theta^2))^4))
qqline(sqrt(n)*(rhohat1-theta/(1+theta^2))/sqrt(1-3*(theta/(1+theta^2))^2+4*(theta/(1+theta^2))^4))
# effekte sieht man mit kleinerer sample size noch stärker


# jetzt machen wir noch das milk data example:

require(TSA)
data(milk)
plot(milk,xlab="",col="Blue",type="b")
# es gibt trend und yearly seasonality
# variance doesnt seem to depend on time
acf(milk)
# you can see trend and seansonality
# big values!
# jetzt filtern wir dies aus
x<-as.numeric(milk)
h<-21
filt<-filter(x,rep(1,h)/h) # linearer filter = moving average
filt # missing values wegen h
#mean(x[1:h]);mean(x[2:(h+1)]);mean(x[(length(x)-h+1):length(x)])
for(i in 1:((h-1)/2)){filt[i]<-mean(x[1:i])}
for(i in (length(x)-((h-1)/2)):(length(x))){filt[i]<-mean(x[i:(length(x))])}
plot(x,xlab="",type="l")
lines(filt,col=2,lwd=2)
# am anfang gibt es ein problem, trend estimate nicht gut
acf(x-filt) # form liegt an seasonality
# jetzt entfernen wir noch die jährliche seasonality
xdt<-x-filt
seasonmilk<-numeric(12)
for(j in 1:12){
  seasonmilk[j]<-mean(xdt[12*(0:11)+j])}
plot(xdt) 
lines(rep(seasonmilk,12),col=2) # jetzt sind wir 'weakly stationary'
#
acf(xdt-seasonmilk) # trotzdem nicht klar welches modell gewählt werden soll
# aber weakly stationary model macht jetzt sinn :-)

#Bemerke: xdt hat Länge 144, seasonmilk nur Länge 12, 
# macht aber genau was es soll, identisch xdt-rep(seasonmilk,12)







