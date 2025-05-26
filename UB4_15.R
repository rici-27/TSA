require(itsmr)
# wirft false, falls das Paket nicht schon installiert ist


###Level of Lake Huron, 1875 to 1972
# lake muss nicht mehr geladent werden, ist direkt vorhanden

plotc(lake)
y<-trend(lake,1) # nähert trend als polynom 1. Ordnung
lines(y)

# betrachte die ursprüngliche sample autokorrelation
acf(lake)

# Detrend mit Order 1
ar(lake-y,order.max=1)
?ar
# Fit an autoregressive time series model to the data
# ergibt 0.7616 als Schätzer für phi

acf(lake-y)

points(0:15,0.7616 ^(0:15))


# Detrend mit Order 2 (automatisch gewählt)
ar(lake-y)
# hier kann man wieder die Koeffizienten auslesen

a<- 0.9714;b<- -0.2754 
rho<-numeric(20)
rho[1]<-1
rho[2]<-a/(1-b)


for(k in 3:20){rho[k]<-a*rho[k-1]+b*rho[k-2]}
rho

# neuer plot 
acf(lake-y)
points(0:19,rho)
points(0:15,0.7616 ^(0:15),col=2)