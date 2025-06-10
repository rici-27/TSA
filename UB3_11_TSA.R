# Exercise 11
# Part(d) 


Bmotion<-function(n){                #Simuliert Pfade einer BB
res<-cumsum(c(0,rnorm(n,0,1)))
}
 
n<-2000                              #Teste die vorherige Funktion
B<-Bmotion(n)
grid<-(0:n)                          #Zeitgrid
plot(grid,B,type="l")


logReturns<-function(n,mu,sigma){
  return(mu+sigma*rnorm(n,0,1))
}

obs<-logReturns(2000,-0.85,2) 
plot(grid[1:n],obs,type="l")


sigmaMLE<-function(obs){
  return(1/n*sum((obs-mean(obs))^2))
}

sigmaUMVU<-function(obs){
  n<-length(obs)
sum(1/(n-1)*(obs-mean(obs))^2)
}

muMLE<-function(obs){
  1/n*sum(obs)
}

sigmaMLE(obs);sigmaUMVU(obs);muMLE(obs)

# Monte Carlo

mu<-1.5
sigma<-2
n<-2000
m<-2000 # Anzahl an Monte-Carlo Simulationen

temp1<-matrix(rnorm(n*m,mu,(sigma)),ncol=m,nrow=n)
r1<-apply(temp1,2,sigmaMLE)
r2<-apply(temp1,2,muMLE)


r1
r2

par(mfrow=c(2,1))
hist(r1,breaks=20);hist(r2)



