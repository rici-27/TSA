#AR(1) process
n<-1000
mu<-0
z<-numeric(n+1)+mu
sigma<-1
phi<-0.5 # wir simulieren, also wählen den koeffizienten selbst
for(j in 1:n){
  z[j+1]<-phi*z[j]+rnorm(1,mean=0,sd=sigma)}
acf(z,lag.max=12)
points(0:12,phi^(0:12),col="red",lwd=2,pch=19) # phi^h ist acf (corr)

#
acf(z,lag.max=12,type="partial") # get partial acf
# at lag 1 pacf and acf are the same
# all the other acfs are nonsignificant / zero in theory!
#
pacf(z,lag.max = 12)
# implemented function for yule walker
ar.yw(z, aic = TRUE, order.max = 10) # wählt ordnung selbst, max 10
# Akaike Information Criterion
# hier wird Ordnung 2 gewählt, das ist falsch
# aber phi_2 ist sehr klein immerhin
ar.yw(z, aic = FALSE, order.max = 1)
acf(z,lag.max = 12)$acf[2]
pacf(z,lag.max = 12)$acf[1]
ar.yw(z, aic = FALSE, order.max = 2)
############################
#Monte Carlo simulation AR(1)
# yule walker says for order 1:
# we estimate phi_1 with the acf at lag 1
n<-1000
N<-1000
Phihat1<-numeric(N);Phihat2<-numeric(N);Phihat3<-numeric(N);Phihat<-array(0,dim=c(10,N))
for(k in 1:N){
  z<-numeric(n+1)
  for(j in 1:n){
    z[j+1]<-phi*z[j]+rnorm(1,mean=0,sd=sigma)}
  # wir betrachten phi_1 für verschiedene ordnungen
  Phihat1[k]<-ar.yw(z, aic = FALSE, order.max = 1)$ar
  Phihat2[k]<-ar.yw(z, aic = FALSE, order.max = 2)$ar[1]
  Phihat3[k]<-ar.yw(z, aic = FALSE, order.max = 10)$ar[1]
  Phihat[,k]<-ar.yw(z, aic = FALSE, order.max = 10)$ar
}
mean(Phihat1);sd(Phihat1)
mean(Phihat2);sd(Phihat2)
mean(Phihat3);sd(Phihat3)
# standard abweichung für größere modelle ist größer
#
par(mfrow=c(1,3))
hist(Phihat1,breaks=20,col="gray",freq=T)
hist(Phihat2,breaks=20,col="gray",freq=T)
hist(Phihat3,breaks=20,col="gray",freq=T)
#
# wir schauen uns KI an für phi bei ordnung 10
# man sieht alle koeff außer 1 sind nahe bei Null
dev.off()
plot(rowMeans(Phihat),col="blue",type="p",ylim=c(-.1,.6))
points(rowMeans(Phihat)+1.64*apply(Phihat, 1, sd, na.rm=TRUE),col="red")
points(rowMeans(Phihat)-1.64*apply(Phihat, 1, sd, na.rm=TRUE),col="red")
##############
#AR(2) process
n<-1000
mu<-0
z<-numeric(n+1)+mu
sigma<-1
phi<-c(0.4,0.5)
for(j in 2:n){
  z[j+1]<-phi[1]*z[j]+phi[2]*z[j-1]+rnorm(1,mean=0,sd=sigma)}
acf(z,lag.max = 10)
#theoretical values based on Yule-Walker equations
acf<-numeric(10)
acf[1]<-phi[1]/(1-phi[2])
acf[2]<-phi[1]*acf[1]+phi[2]
for(k in 3:10){
  acf[k]<-phi[1]*acf[k-1]+phi[2]*acf[k-2]}
acf
points(acf[1:10],col="red",lwd=2,pch=19)
#
pacf(z,lag.max = 10)
pacf(z,lag.max = 10)$acf[1:10]
##############################
#Monte Carlo simulation AR(2)
N<-1000
Phihat<-array(0,dim=c(2,N))
PhihatYW<-array(0,dim=c(2,N))
PhihatYWad<-array(0,dim=c(2,N))
order<-numeric(N)
for(k in 1:N){
  z<-numeric(n+1)
  for(j in 2:n){
    z[j+1]<-phi[1]*z[j]+phi[2]*z[j-1]+rnorm(1,mean=0,sd=sigma)}
  rhohat1<-acf(z,lag.max = 12,plot=FALSE)$acf[2]
  Phihat[,k]<-1/(1-rhohat1^2)*matrix(c(1,-rhohat1,-rhohat1,1),nrow= 2,ncol = 2,byrow=TRUE)%*%c(rhohat1,acf(z,lag.max = 12,plot=FALSE)$acf[3])
  PhihatYW[,k]<-ar.yw(z, aic = FALSE, order.max = 2)$ar
  PhihatYWad[,k]<-ar.yw(z, aic = FALSE, order.max = 10)$ar[1:2]
  order[k]<-ar.yw(z, aic = TRUE)$order
}
rowMeans(Phihat);apply(Phihat, 1, sd, na.rm=TRUE)
rowMeans(PhihatYW);apply(PhihatYW, 1, sd, na.rm=TRUE)
rowMeans(PhihatYWad);apply(PhihatYWad, 1, sd, na.rm=TRUE)
#
mean(order)
plot(order)
length(which(order==2))/N



########################################
#Forecasting and fitting AR example to GISS surface yearly temperature anomalies
#setwd("C:/Universität/Lehre/Zeitreihenanalyse/2025/Examples")
#
require(itsmr)
temp<-read.table("temperature.txt", sep = " ")
yearlytemp<-temp[,2]
year<-temp[,1]
plot(year,yearlytemp,type="l",xlab="",ylab="")
#
ytemp<-yearlytemp
acf(ytemp)
#
ts<-diff(ytemp,differences = 2)
plot(ts,type="l",xlab="",ylab="")
#
acf(ts)
pacf(ts)
ar.yw(ts, aic = TRUE, order.max = 10)
#Optimal linear prediction
c<-ar.yw(ts, aic = TRUE, order.max = 10)$ar

### aic fokusiert auf die besten forecasting ergebnisse
# macht so mehr sinn, dass so p öfter zu hoch gewählt wird

pred<-numeric(5)
pred[1]<-c%*%ts[(length(ts)-8):length(ts)]
pred[2]<-c%*%c(pred[1],ts[(length(ts)-7):length(ts)])
pred[3]<-c%*%c(pred[1],pred[2],ts[(length(ts)-6):length(ts)])
pred[4]<-c%*%c(pred[1],pred[2],pred[3],ts[(length(ts)-5):length(ts)])
pred[5]<-c%*%c(pred[1],pred[2],pred[3],pred[4],ts[(length(ts)-4):length(ts)])
pred
#
plot(c(1:length(ts)),ts,type="l",ylab="",xlab="",xlim=c(0,length(ts)+5))
points(c((length(ts)+1):(length(ts)+5)),pred,col=2,type="l")
#
temppred<-numeric(5)
temppred[1]<-2*ytemp[145]-ytemp[144]+pred[1]
temppred[2]<-2*temppred[1]-ytemp[145]+pred[2]
temppred[3]<-2*temppred[2]-temppred[1]+pred[3]
temppred[4]<-2*temppred[3]-temppred[2]+pred[4]
temppred[5]<-2*temppred[4]-temppred[3]+pred[5]
temppred
#
plot(year,yearlytemp,type="l",xlab="",ylab="",xlim=c(1880,2029),ylim=c(-1,2.8))
points(c(2024:2029),c(yearlytemp[length(yearlytemp)],temppred),col=2,type="l")