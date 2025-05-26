require(itsmr)

#Level of Lake Huron, 1875 to 1972
plotc(lake)
y<-trend(lake,1)
lines(y)

acf(lake)

ar(lake-y,order.max=1)

acf(lake-y)
points(0:15,0.7616 ^(0:15))

ar(lake-y)


a<- 0.9714;b<- -0.2754 
rho<-numeric(20)
rho[1]<-1;rho[2]<-a/(1-b)
for(k in 3:20){rho[k]<-a*rho[k-1]+b*rho[k-2]}
rho
acf(lake-y)
points(0:19,rho)
points(0:15,0.7616 ^(0:15),col=2)