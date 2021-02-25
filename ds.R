## ----setOptions, echo=F, include=F, cache=F, purl=T----
# set some display options  for including R code and R output in document
opts_chunk$set(
 background=rep(0.93, 3),
 size='small',
 tidy=F,
 replace.assign=F,
 width=60,
 dev='png',
 cache=T,
 fig.pos='h!',
 fig.align='center',
 fig.width=6, fig.height=3,
 show.signif.stars=T,
 keep.blank.lines=T,
 echo=F) 


## ----AUTHOR, include=F-------------------------------
# written by: Mark McFarland, P.E.
#             December, 2020
#             Boulder, CO
#                   ~+~


## tree -fi data


## wc -l data/*.csv


## cat data/AZ_LincolnHeight_Dwntwn_Run1.txt


## ----------------------------------------------------
d1=read.table('data/AZ_LincolnHeight_Dwntwn_Run1.csv', sep=',', header=T) 
d1$city=as.factor('LincolnHeight')
d1$region=as.factor('downtown')
d1$run=as.factor(1)

d2=read.table('data/AZ_LincolnHeight_Dwntwn_Run2.csv', sep=',', header=T) 
d2$city=as.factor('LincolnHeight')
d2$region=as.factor('downtown')
d2$run=as.factor(2)

d3=read.table('data/AZ_LincolnHeight_Suburb_Run1.csv', sep=',', header=T) 
d3$city=as.factor('LincolnHeight')
d3$region=as.factor('suburb')
d3$run=as.factor(1)

d4=read.table('data/AZ_LincolnHeight_Suburb_Run2.csv', sep=',', header=T) 
d4$city=as.factor('LincolnHeight')
d4$region=as.factor('suburb')
d4$run=as.factor(2)

d5=read.table('data/AZ_LincolnHeight_Suburb_Run3.csv', sep=',', header=T) 
d5$city=as.factor('LincolnHeight')
d5$region=as.factor('suburb')
d5$run=as.factor(3)

d.lh=rbind(d1, d2, d3, d4, d5)


d1=read.table('data/AZ_OpenArms_Pima.csv', sep=',', header=T) 
d1$city=as.factor('OpenArms')
d1$region=as.factor('Pima')
d1$run=as.factor(1)

d2=read.table('data/AZ_OpenArms_Sub_Run1.csv', sep=',', header=T) 
d2$city=as.factor('OpenArms')
d2$region=as.factor('suburb')  # NOTE: originally 'sub'
d2$run=as.factor(1)

d3=read.table('data/AZ_OpenArms_Sub_Run2.csv', sep=',', header=T) 
d3$city=as.factor('OpenArms')
d3$region=as.factor('suburb')
d3$run=as.factor(2)

d.oa=rbind(d1, d2, d3)


d1=read.table('data/AZ_Phoenix_North_Dwntwn.csv', sep=',', header=T) 
d1$city=as.factor('PhoenixN')
d1$region=as.factor('downtown')
d1$run=as.factor(1)

d2=read.table('data/AZ_Phoenix_North_North.csv', sep=',', header=T) 
d2$city=as.factor('PhoenixN')
d2$region=as.factor('north')
d2$run=as.factor(1)

d3=read.table('data/AZ_Phoenix_South_Dwntwn.csv', sep=',', header=T) 
d3$city=as.factor('PhoenixS')
d3$region=as.factor('downtown')
d3$run=as.factor(1)

d4=read.table('data/AZ_Phoenix_South_South.csv', sep=',', header=T) 
d4$city=as.factor('PhoenixS')
d4$region=as.factor('south')
d4$run=as.factor(1)

d.p=rbind(d1, d2, d3, d4)

d=rbind(d.lh, d.oa, d.p)
names(d)[1:4]=c('time', 'lat', 'lon', 'pl.m')
d$pl.m=d$pl.m*-1
#levels(d$region)[4]='suburb'


## ----------------------------------------------------
dim(d)


## ----------------------------------------------------
replications(pl.m~city*region*run, data=d)


## ----------------------------------------------------
summary(d) 


## ----fig.height=6, fig.width=6, message=F------------
ggplot(d, aes(x=pl.m, fill=run)) +
  geom_histogram(position='identity', alpha=0.5) +
  #geom_histogram( position='dodge') +
  theme(legend.position = 'top') +
  facet_wrap(city~region, labeller = "label_both")

## ----b1.phoenix.hist, dpi=200, fig.height=6, fig.width=6, message=F, include=F----
# for Brad
ggplot(d, aes(x=pl.m, fill=run)) +
  geom_histogram(position='identity', alpha=0.5) +
  #geom_histogram( position='dodge') +
  labs(x='Measured Path Loss (dBm)') +
  theme(legend.position = 'top') +
  facet_wrap(city~region, labeller = "label_both")


## ----fig.height=5------------------------------------
ggplot(d, aes(x=run, y=pl.m)) +
  geom_boxplot(coef=1e3) +
  labs(y='Measured Path Loss (dBm)') +
  theme(legend.position = 'top') +
  facet_wrap(city~region, labeller='label_both')

## ----b1.phoenix.box, fig.height=5, dpi=200, include=F----
ggplot(d, aes(x=run, y=pl.m)) +
  geom_boxplot(coef=1e3) +
  labs(y='Measured Path Loss (dBm)') +
  theme(legend.position = 'top') +
  facet_wrap(city~region, labeller='label_both')


## ----fig.height=5, dpi=75----------------------------
ggplot(d, aes(x=time, y=pl.m, shape=run, colour=run)) +
  geom_line() +
  theme(legend.position = 'top') +
  facet_wrap(city~region) 
  #facet_grid(city~region) 

## ----b1.phoenix.ts, fig.height=5, dpi=200, include=F----
ggplot(d, aes(x=time, y=pl.m, shape=run, colour=run)) +
  geom_line() +
  labs(x='Time (s)', y='Measured Path Loss (dBm)') +
  theme(legend.position = 'top') +
  facet_wrap(city~region, labeller='label_both') 
  #facet_grid(city~region) 



## ----pmap, fig.height=6, fig.width=8, dpi=75, message=F----
library(ggmap)   
loc = c(lon=mean(d$lon), lat=mean(d$lat))
plMap.phx = get_map(location=loc, zoom=11, source='stamen', maptype='terrain', messaging=T)

txLoc=data.frame(lon=-108.2404, lat=38.99187)
 
ggmap(plMap.phx, darken=0.00, extent='panel') +
  geom_point(data=d[d$run=='1',], aes(x=lon, y=lat, colour=pl.m), size=0.50) +
  #geom_point(data=txLoc, aes(x=lon, y=lat), pch=4,colour='yellow', size=5) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL) + 
  facet_wrap(city~region, nrow=2, labeller = "label_both") +
  #theme(legend.position = 'bottom') +
  theme(line = element_blank()) + theme(axis.text = element_blank()) 

## ----b1.phoenix.map, fig.height=6, fig.width=8, dpi=200, include=F----
loc = c(lon=mean(d$lon), lat=mean(d$lat))
plMap.phx = get_map(location=loc, zoom=11, source='stamen', maptype='terrain', messaging=T)

txLoc=data.frame(lon=-108.2404, lat=38.99187)
 
ggmap(plMap.phx, darken=0.00, extent='panel') +
  geom_point(data=d[d$run=='1',], aes(x=lon, y=lat, colour=pl.m), size=0.50) +
  #geom_point(data=txLoc, aes(x=lon, y=lat), pch=4,colour='yellow', size=5) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL, color='Measured\nPath\nLoss\n(dBm)') + 
  facet_wrap(city~region, nrow=2, labeller = "label_both") +
  #theme(legend.position = 'bottom') +
  theme(line = element_blank()) + theme(axis.text = element_blank()) 


## ----acfPhoenix, fig.height=4, fig.width=6, dpi=75----
tmp=d[d$city=='LincolnHeight' & d$run=='1',]
dl=dlply(tmp, .(region)) 
par(mfrow=c(1,2))
acf(dl[[1]]$pl.m, main=names(dl)[1],  lag.max=1000)
acf(dl[[2]]$pl.m, main=names(dl)[2],  lag.max=1000)

#library(ggfortify)
#aut1oplot()


## ----acfP, fig.height=4, fig.width=6, dpi=75---------
dld=lapply(dl, function(x) x[seq(1, nrow(x), 25),])

par(mfrow=c(1,2)) 
acf(dld[[1]]$pl.m, main=names(dl)[1],  lag.max=1000)
acf(dld[[2]]$pl.m, main=names(dl)[2],  lag.max=1000)


## ----eval=F------------------------------------------
## fun=function(x) { ada=scale(x, scale=F, center=median(cltr)) }
## 
## 
## jcltr.ada=scale(d.e2$cltr, center=median(d.e2$cltr), scale=F)
## jj=ddply(d.e2, .(run, loc, obstr), mutate,
##          center=median(cltr),
##          cltr.ada=scale(cltr, center=median(cltr), scale=F))


## ----size='scriptsize'-------------------------------
d1=read.table('../tc/rfData/GrandJunction-GrandMesa/GrandMesa-to-Route1_1771-MHz_Jun0719_1_10.2_3_tbcorr_noheader.csv', sep=',', header=T) 
d1$route=as.factor(1)
d1$loc=as.factor('Grand Mesa')

d2=read.table('../tc/rfData/GrandJunction-GrandMesa/GrandMesa-to-Route2_1771-MHz_Jun0819_1_10.2_3_tbcorr_noheader.csv', sep=',', header=T) 
d2$route=as.factor(2)
d2$loc=as.factor('Grand Mesa')

d3=read.table('../tc/rfData/GrandJunction-GrandMesa/GrandMesa-to-Route3_1771-MHz_Jun0819_1_10.2_3_tbcorr_noheader.csv', sep=',', header=T) 
d3$route=as.factor(3)
d3$loc=as.factor('Grand Mesa')

d4=read.table('../tc/rfData/GrandJunction-GrandMesa/GrandMesa-to-Route4_1771-MHz_Jun0919_1_10.2_3_tbcorr_noheader.csv', sep=',', header=T) 
d4$route=as.factor(4)
d4$loc=as.factor('Grand Mesa')


d.g=rbind(d1, d2, d3, d4)
d.g=d.g[,-c(1,10:13)]

names(d.g)=c('time', 'lat', 'lon', 'pl.meas', 'pl.itm', 'fspl', 'dist', 'angle', 'route', 'location')
d.g$pl.meas=abs(d.g$pl.meas)  # assuming path gain is recorded
d.g$pl.itm=abs(d.g$pl.itm)  # assuming path gain is recorded
d.g$fspl=abs(d.g$fspl)  # assuming path gain is recorded

head(d.g)


## ----eval=F------------------------------------------
## d.g=d.g[d.g$lat>39.02,]


## ----------------------------------------------------
dim(d.g)


## ----fig.height=5, dpi=75, message=F-----------------
library(ggmap)   
loc = c(lon=mean(d.g$lon)+0.04000, lat=mean(d.g$lat-0.0400))
plMap.gm = get_map(location=loc, zoom=10, source='stamen', maptype='terrain', messaging=T)

txLoc=data.frame(lon=-108.2404, lat=38.99187)

ggmap(plMap.gm, darken=0.00, extent='panel') +
  geom_point(data=d.g, aes(x=lon, y=lat, colour=pl.meas), size=0.50) +
  geom_point(data=txLoc, aes(x=lon, y=lat), pch=4,colour='black', size=5) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL) + 
  facet_wrap(~route, nrow=2, labeller = "label_both") +
  #theme(legend.position = 'bottom') +
  theme(line = element_blank()) + theme(axis.text = element_blank()) 

## ----b1.gj.map, fig.height=5, dpi=200, include=F-----
library(ggmap)   
loc = c(lon=mean(d.g$lon)+0.04000, lat=mean(d.g$lat-0.0400))
plMap.gm = get_map(location=loc, zoom=10, source='stamen', maptype='terrain', messaging=T)

txLoc=data.frame(lon=-108.2404, lat=38.99187)

ggmap(plMap.gm, darken=0.00, extent='panel') +
  geom_point(data=d.g, aes(x=lon, y=lat, colour=pl.meas), size=0.50) +
  geom_point(data=txLoc, aes(x=lon, y=lat), pch='*', colour='black', size=7) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL, color='Measured\nPath\nLoss\n(dBm)') + 
  facet_wrap(~route, nrow=2, labeller = "label_both") +
  #theme(legend.position = 'bottom') +
  theme(line = element_blank()) + theme(axis.text = element_blank()) 


## ----acfGJ, fig.height=5, fig.width=6, dpi=75--------
d.gl=dlply(d.g, .(route))
par(mfrow=c(2,2))
acf(d.gl[[1]]$pl.m, main=names(d.gl)[1],  lag.max=1000)
acf(d.gl[[2]]$pl.m, main=names(d.gl)[2],  lag.max=1000)
acf(d.gl[[3]]$pl.m, main=names(d.gl)[3],  lag.max=1000)
acf(d.gl[[4]]$pl.m, main=names(d.gl)[4],  lag.max=1000)
#library(ggfortify)
#aut1oplot()


## ----j2, fig.height=5, fig.width=6, dpi=75-----------
d.gld=lapply(d.gl, function(x) x[seq(1, nrow(x), 25),])
par(mfrow=c(2,2))
acf(d.gld[[1]]$pl.m, main=names(d.gld)[1],  lag.max=1000)
acf(d.gld[[2]]$pl.m, main=names(d.gld)[2],  lag.max=1000)
acf(d.gld[[3]]$pl.m, main=names(d.gld)[3],  lag.max=1000)
acf(d.gld[[4]]$pl.m, main=names(d.gld)[4],  lag.max=1000)


## ----size='scriptsize'-------------------------------
d1=read.table('../tc/rfData/SaltLakeCity-CityCreek/CityCreek-to-South_1773-MHz_Jun1318_1_19.4_3_tbcorr_noheader.csv', sep=',', header=T) 
d1$route=as.factor(1)
d1$loc=as.factor('SLC')

d2=read.table('../tc/rfData/SaltLakeCity-CityCreek/CityCreek-to-SubWest_1773-MHz_Jun1118_1_19.4_3_tbcorr_noheader.csv', sep=',', header=T) 
d2$route=as.factor(2)
d2$loc=as.factor('SLC')

d3=read.table('../tc/rfData/SaltLakeCity-CityCreek/CityCreek-to-Urban_1773-MHz_Jun1218_1_19.4_3_tbcorr_noheader.csv', sep=',', header=T) 
d3$route=as.factor(3)
d3$loc=as.factor('SLC')

d.s=rbind(d1, d2, d3)
d.s=d.s[,-c(1,10:13)]

names(d.s)=c('time', 'lat', 'lon', 'pl.meas', 'pl.itm', 'fspl', 'dist', 'angle', 'route', 'location')
d.s$pl.meas=abs(d.s$pl.meas)  # assuming path gain is recorded
d.s$pl.itm=abs(d.s$pl.itm)  # assuming path gain is recorded
d.s$fspl=abs(d.s$fspl)  # assuming path gain is recorded

head(d.s)


## ----------------------------------------------------
dim(d.s)


## ----fig.height=5, dpi=75, message=F-----------------
library(ggmap)  
loc = c(lon=median(d.s$lon)+0.00000, lat=median(d.s$lat-0.0000))
plMap.slc = get_map(location=loc, zoom=12, source='stamen', maptype='terrain', messaging=T)

#txLoc=data.frame(lon=-108.2404, lat=38.99187) #GM
#txLoc=data.frame(lon=-108.2404, lat=39.089337) #LM
txLoc=data.frame(lon=-111.880944, lat=40.807189) #SLC

ggmap(plMap.slc, darken=0.00, extent='panel') +
  geom_point(data=d.s, aes(x=lon, y=lat, colour=pl.meas), size=0.50) +
  geom_point(data=txLoc, aes(x=lon, y=lat), pch='*',colour='black', size=7) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL, color='Measured\nPath\nLoss\n(dBm)') + 
  facet_wrap(~route, nrow=2)+#, labeller = "label_both") +
  #theme(legend.position = 'bottom') +
  theme(line = element_blank()) + theme(axis.text = element_blank()) 

## ----b1.slc.map, fig.height=5, dpi=200, messages=F, include=F----
library(ggmap)  
loc = c(lon=median(d.s$lon)+0.00000, lat=median(d.s$lat-0.0000))
plMap.slc = get_map(location=loc, zoom=12, source='stamen', maptype='terrain', messaging=T)

#txLoc=data.frame(lon=-108.2404, lat=38.99187) #GM
#txLoc=data.frame(lon=-108.2404, lat=39.089337) #LM
txLoc=data.frame(lon=-111.880944, lat=40.807189) #SLC

ggmap(plMap.slc, darken=0.00, extent='panel') +
  geom_point(data=d.s, aes(x=lon, y=lat, colour=pl.meas), size=0.50) +
  geom_point(data=txLoc, aes(x=lon, y=lat), pch='*',colour='black', size=7) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL, color='Measured\nPath\nLoss\n(dBm)') + 
  facet_wrap(~route, nrow=2)+#, labeller = "label_both") +
  #theme(legend.position = 'bottom') +
  theme(line = element_blank()) + theme(axis.text = element_blank()) 


## ----acfslc, fig.height=5, fig.width=6, dpi=75-------
d.sl=dlply(d.s, .(route))
par(mfrow=c(2,2))
acf(d.sl[[1]]$pl.m, main=names(d.sl)[1],  lag.max=500)
acf(d.sl[[2]]$pl.m, main=names(d.sl)[2],  lag.max=500)
acf(d.sl[[3]]$pl.m, main=names(d.sl)[3],  lag.max=500)
#library(ggfortify)
#aut1oplot()


## ----j3, fig.height=5, fig.width=6, dpi=75-----------
d.sld=lapply(d.sl, function(x) x[seq(1, nrow(x), 50),])
par(mfrow=c(2,2))
acf(d.sld[[1]]$pl.m, main=names(d.sld)[1],  lag.max=500)
acf(d.sld[[2]]$pl.m, main=names(d.sld)[2],  lag.max=500)
acf(d.sld[[3]]$pl.m, main=names(d.sld)[3],  lag.max=500)




## ----eval=F------------------------------------------
## install.packages('tmap')
## library(rgdal);library(maptools);library(dplyr);library(tidyr);library(tmap)
## 
## 

