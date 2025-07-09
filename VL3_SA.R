# Beispiel 3.2

require(astsa)
par(mfrow=2:1)
tsplot(star, ylab="star magnitude", xlab="day", col=4)
Pstar = mvspec(star, col=5, xlim=c(0,.08), lwd=3, type="h", main=NA)
text(.05, 7000, "24 daycycle"); text(.027, 9000, "29 daycycle")
#
dev.off()
x1 = 2*cos(2*pi*1:100*6/100) + 3*sin(2*pi*1:100*6/100)
x2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
x = x1 + x2 + x3
tsplot(x1)
#
acf(x1)
#
tsplot(x, ylim=c(-16,16), main="sum", col=4, gg=TRUE)
#
per = Mod(fft(x)/sqrt(100))^2
P = (4/100)*per; Fr = 0:99/100
tsplot(Fr, P, type="h", lwd=3, xlab="frequency", ylab="scaled periodogram",col=4, gg=TRUE)
abline(v=.5, lty=5, col=8)


?fft
n<-100
X<-cos(2*pi*7*(0:(n-1)/n))+3*sin(2*pi*23*(0:(n-1)/n))
plot(X,type="b")
#
dft<-fft(X)/sqrt(n)
#
plot(Re(dft))
#
plot(Im(dft))
#
per<-abs(dft)^2
plot(2*pi*(0:(n-1)/n),per)
#
segments(2*pi*7/n,0,2*pi*7/n,250,col=2,lwd=2,lty=3)
segments(2*pi*23/n,0,2*pi*23/n,250,col=4,lwd=2,lty=3)
segments(2*pi*(n-7)/n,0,2*pi*(n-7)/n,250,col=2,lwd=2,lty=3)
segments(2*pi*(n-23)/n,0,2*pi*(n-23)/n,250,col=4,lwd=2,lty=3)

#smoothed periodogram: simulation
N<-128
n<-c(0:(N-1))
mu<-1
sigma<-1
epsilon<-rnorm(N,mean=mu,sd=sigma)
omega<-0.4
C<-0
sine<-C*sin(2*pi*omega*n)
ts<-sine+epsilon
plot(ts,type="l")
points(ts,cex=.5,lwd=2,pch=1,col=2)
points(sine,col=4)
lines(sine, col=4)

# dies sollte eine Mischung aus einem kontinuierlichen Spektrum
# und einem diskreten Spektrum in 0.4 (omega) sein
# hier muss man schauen, wo der punkt dann genau liegt
#
I<-(1/N)*abs(fft(ts))^2
# fft arbeitet auf 0 bis 2pi
I
plot(I)
# 
#
I<-I[1:(N/2)]
grid<-seq(0,2*pi-1/N,by=(2*pi)/N) # skalieren von beoachtungen auf pi
grid<-grid[1:(N/2)] # haben 128 Beobachtungen, wegen Symmetrie reicht hälfte
plot(grid,I,type="l")
# man sieht kleinen peak bei 0.4 * 2 pi = 2.51..
# großer peak ist circa bei 128 * mu = 128,
# macht sinn wegen theorem 3.15
# wenn wir C=0 setzen verschwindet der 2. peak
#
plot(grid[-1],I[-1],type="l")
# wir erwarten eigentlich eine konstante spektral density vom white noise
# jetzt smoothen wir:
plot(grid[-1],filter(I[-1]/(2*pi),rep(1,20)/20),type="l",ylim=c(0,max(I[-1])/(2*pi)))
abline(sigma^2/(2*pi),0)

#Monte Carlo white noise
smooth<-c(5,10,20,30,40) # different smoothing parameter 
# (number of parameters i smooth over)
fest<-array(0,dim=c(1000,5))
for(s in 1:5){
  m<-smooth[s]
  for(l in 1:1000){
    ts<-rnorm(N,mean=mu,sd=sigma)
    I<-1/(2*pi)*(1/N)*abs(fft(ts))^2
    fest[l,s]<-filter(I[-1],rep(1,m)/m)[64]}
}
colMeans(fest)-sigma^2/(2*pi)
apply(fest, 2, sd, na.rm=TRUE) # bias could get larger for larger m
# depends on if the spectral density is constant or not
apply(fest, 2, var, na.rm=TRUE) # var should get smaller for larger m
# sieht man für C\neq 0 !


#Monte Carlo simulation AR(1)
N<-128
smooth<-c(10,20,30,40,50)
fest<-array(0,dim=c(1000,5))
phi<-0.5
for(s in 1:5){
  m<-smooth[s]
  for(l in 1:1000){
    z<-numeric(N)
    for(j in 1:(N-1)){
      z[j+1]<-phi*z[j]+rnorm(1,mean=0,sd=sigma)} # model equation
    I<-1/(2*pi)*(1/N)*abs(fft(z))^2
    fest[l,s]<-filter(I[-1],rep(1,m)/m)[95]}
  # 95 is at pi/2
  # -> can calculate true spectral density at this point
}
colMeans(fest)-sigma^2/(2*pi)*1/1.25
apply(fest, 2, sd, na.rm=TRUE)





