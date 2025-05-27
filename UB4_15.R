### Aufgabe: To find a suitable model for the Lake Huron data, we compare the empirical autocorrelation function
### of the data with the autocorr functions of a fitted AR(1) and AR(2) model.


require(itsmr) # wirft false, falls das Paket nicht schon installiert ist


### Level of Lake Huron, 1875 to 1972

plotc(lake) # lake muss nicht mehr geladent werden, ist direkt vorhanden
y<-trend(lake,1) # nähert trend als polynom 1. Ordnung
lines(y) # einfacher als alles mit ggplot 2, aber nicht so viele Optionen

acf(lake) # betrachte die ursprüngliche sample autokorrelation


## Detrend mit Order 1
ar(lake-y,order.max=1)
?ar # Fit an autoregressive time series model to the data
# ergibt 0.7616 als Schätzer für phi

acf(lake-y)
points(0:15,0.7616 ^(0:15)) # aus Beipiel 1.11 b)


## Detrend mit Order 2 (automatisch gewählt)
ar(lake-y)
# hier kann man wieder die Koeffizienten auslesen

a<- 0.9714
b<- -0.2754 
rho<-numeric(20)
rho[1]<-1
rho[2]<-a/(1-b)


for(k in 3:20){rho[k]<-a*rho[k-1]+b*rho[k-2]} # Korr wird rekursiv berechnet :-)
rho

# neuer plot zum Vergleich :-)
acf(lake-y)
points(0:19,rho)
points(0:15,0.7616 ^(0:15),col=2)