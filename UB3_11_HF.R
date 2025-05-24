# Exercise 11
# Part(d) 


Bmotion<-function(n){                #Simuliert Pfade einer BB
res<-cumsum(c(0,rnorm(n,0,sqrt(1/n))))
}
 
n<-2000                              #Teste die vorherige Funktion
B<-Bmotion(n)
grid<-(0:n)/n                        #Zeitgrid
plot(grid,B,type="l")

GBB<-function(n,mu,sigma){
res<-exp(mu*(1/n)+sigma*Bmotion(n))
}
 
plot(grid,GBB(2000,-0.85,2),type="l")


logReturns<-function(n,mu,sigma){
return(mu*(1/n)+sigma*rnorm(n,0,sqrt(1/n)))
}

obs<-logReturns(2000,-0.85,2) 
plot(grid[1:n],obs,type="l")

# Da die Beobachtungen zu den Zeitpunkten i/n gesampled wurden, hat
# man mu/n und sigma^2/n geschätzt. Somit muss man die Ergbenisse nochmal
# durch 1/n teilen, was jeweils die Vorfaktoren 1/n und 1/(n-1) überflüssig 
# macht.

sigmaMLE<-function(obs){
return(sum((obs-mean(obs))^2))
}

sigmaUMVU<-function(obs){
n<-length(obs)
sum(n/(n-1)*(obs-mean(obs))^2)

}

muMLE<-function(obs){
sum(obs)
}

sigmaMLE(obs);sigmaUMVU(obs);muMLE(obs)

# Bemerkung: Der Schätzer für mu ist schlecht. Wenn man die Log-Returns
# aufsummiert erhält man eine Teleskopsumme. Somit ist der Schätzer für 
# mu einfach nur eine normalverteilte ZUfallsvariable.



# Monte Carlo

mu<-1.5
sigma<-2
n<-2000
m<-1000   # Anzahl an Monte-Carlo Simulationen

temp1<-matrix(rnorm(n*m,mu*1/n,(sigma/sqrt(n))),ncol=m,nrow=n)
r1<-apply(temp1,2,sigmaMLE)
r2<-apply(temp1,2,muMLE)


r1
r2

par(mfrow=c(2,1))
hist(r1);hist(r2)





