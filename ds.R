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




## ----readData, eval=T, cache=T-----------------------
#file='data/spreadsheet_1760M_loant_hipwr_offpeak_20mph_final.csv'
#j = read.table(file='data/spreadsheet_1760M_loant_hipwr_offpeak_20mph_final.csv', header=T, sep=',')

library(plyr)
files=list.files(path='../martinAcres/data', pattern='spreadsheet', full.names=T)
d2=data.frame()
for (f in files) {
  #print(f)
  a=unlist(strsplit(f, '_'))
  freq=a[2]; txHeight=a[3]; txPwr=a[4]; traffic=a[5]; speed=a[6]
  #print(c(freq, ant, txPwr, traffic, speed))
  tmp = read.table(file=f, header=T, sep=',')
  tmp$freq      = as.factor(freq)
  tmp$txHeight = as.factor(txHeight)
  tmp$txPwr     = as.factor(txPwr)
  tmp$traffic   = as.factor(traffic)
  tmp$speed     = as.factor(speed)
  d2=rbind(d2, tmp)
}

# remove unnecessary columns:
#delete=c('elevation', 'sa.pwr.mw', 'sa.pwr.dBm', 0
names(d2)=tolower(names(d2))

# rename some:
names(d2)[3]='lon'
names(d2)[4]='elev'
names(d2)[8]='fspl'
names(d2)[9]='dist'
names(d2)[10]='pwr.vsa'
names(d2)[11]='bpl.vsa'
names(d2)[12]='k'
names(d2)[20]='txHeight'
names(d2)[21]='txPwr'


##################################################
### identify lashley and moorhead specimens:
library(sp)
# take lon lat values and create object for coord:
#dat = d[(d$txPwr=='high'& d$txHeight=='high' & d$traffic=='high' & d$speed=='30mph'),]
xy = d2[,3:2]

#pts.df=SpatialPointsDataFrame(xy, data=dat, 
#                           proj4string=CRS('+proj=longlat'))
#pts.df=SpatialPointsDataFrame(xy, data=dat) 
pts=SpatialPoints(xy) 

# create polygon - lashley:
poly.lashley=data.frame(lon=c(-105.260, -105.259, -105.256, -105.257),
                        lat=c(39.995,    39.9955,  39.9907, 39.990))
poly.lashley=rbind(poly.lashley, poly.lashley[1,])
poly1=Polygon(poly.lashley)
poly2=Polygons(list(poly1), ID='lashley')
p.lashley=SpatialPolygons(list(poly2))

# create polygon - moorehead:
poly.moorhead=data.frame(lon=c(-105.249, -105.255  , -105.25460, -105.2485),
                          lat=c( 39.9934,   39.9968,    39.99730,   39.994))
poly.moorhead=rbind(poly.moorhead, poly.moorhead[1,])
poly1=Polygon(poly.moorhead)
poly2=Polygons(list(poly1), ID='moorhead')
p.moorhead=SpatialPolygons(list(poly2))


# find points inside polygon:
#  over(SpatialPoints, SpatialPolygons)
in.lashley = over(pts, p.lashley)
in.moorhead = over(pts, p.moorhead)

# id lashley specimens in dataframe:
jl = as.numeric(names(na.omit(in.lashley)))
tmp.lashley = d2[as.numeric(names(na.omit(in.lashley))),]  ###
tmp.lashley$road=as.factor('lashley')
ntmp.lashley = d2[-as.numeric(names(na.omit(in.lashley))),]  ###
ntmp.lashley$road=as.factor('unclass')

# id moorhead specimens in dataframe:
jm = as.numeric(names(na.omit(in.moorhead)))
tmp.moorhead = d2[as.numeric(names(na.omit(in.moorhead))),]
tmp.moorhead$road=as.factor('moorhead')
ntmp.moorhead = d2[-as.numeric(names(na.omit(in.moorhead))),]
ntmp.moorhead$road=as.factor('unclass')

# lashley & moorhead in dd - organized
dd = rbind(tmp.lashley, tmp.moorhead)
# all of them:
dda = rbind(tmp.lashley, tmp.moorhead, ntmp.lashley, ntmp.moorhead)

# to make it work with code below
levels(dd$txHeight) = c('high', 'low')
levels(dd$txPwr) = c('high', 'low')
levels(dd$traffic) = c('low', 'high')

# remove second drive-bys
d.1=dd[dd$txPwr=='high' & dd$txHeight=='high' & dd$traffic=='low' & dd$speed=='20mph' & dd$road=='lashley',]  # nothing to do
d.2=dd[dd$txPwr=='high' & dd$txHeight=='high' & dd$traffic=='low' & dd$speed=='20mph' & dd$road=='moorhead' & dd$time<=284,]  # remove time>284

d.3=dd[dd$txPwr=='high' & dd$txHeight=='low' & dd$traffic=='low' & dd$speed=='20mph' & dd$road=='lashley',]  # nothing to do
d.4=dd[dd$txPwr=='high' & dd$txHeight=='low' & dd$traffic=='low' & dd$speed=='20mph' & dd$road=='moorhead' & dd$time<=271,]  # remove time>=271

d.5=dd[dd$txPwr=='low' & dd$txHeight=='high' & dd$traffic=='low' & dd$speed=='20mph' & dd$road=='lashley',]  # nothing
d.6=dd[dd$txPwr=='low' & dd$txHeight=='high' & dd$traffic=='low' & dd$speed=='20mph' & dd$road=='moorhead' & dd$time<=269,]  

d.7=dd[dd$txPwr=='low' & dd$txHeight=='low' & dd$traffic=='low' & dd$speed=='20mph' & dd$road=='lashley',]  # nothing
d.8=dd[dd$txPwr=='low' & dd$txHeight=='low' & dd$traffic=='low' & dd$speed=='20mph' & dd$road=='moorhead' & dd$time<=278,]  

#. row 2:
d.9= dd[dd$txPwr=='high' & dd$txHeight=='high' & dd$traffic=='low' & dd$speed=='30mph' & dd$road=='lashley' & dd$time<=86,]  
d.10=dd[dd$txPwr=='high' & dd$txHeight=='high' & dd$traffic=='low' & dd$speed=='30mph' & dd$road=='moorhead' & dd$time<=245,]  

d.11=dd[dd$txPwr=='high' & dd$txHeight=='low' & dd$traffic=='low' & dd$speed=='30mph' & dd$road=='lashley' & dd$time<=104,]  
d.12=dd[dd$txPwr=='high' & dd$txHeight=='low' & dd$traffic=='low' & dd$speed=='30mph' & dd$road=='moorhead' & dd$time<=261,]  

d.13=dd[dd$txPwr=='low' & dd$txHeight=='high' & dd$traffic=='low' & dd$speed=='30mph' & dd$road=='lashley' & dd$time<=76,]  
d.14=dd[dd$txPwr=='low' & dd$txHeight=='high' & dd$traffic=='low' & dd$speed=='30mph' & dd$road=='moorhead' & dd$time<=215,]  

d.15=dd[dd$txPwr=='low' & dd$txHeight=='low' & dd$traffic=='low' & dd$speed=='30mph' & dd$road=='lashley' & dd$time<=94,]  
d.16=dd[dd$txPwr=='low' & dd$txHeight=='low' & dd$traffic=='low' & dd$speed=='30mph' & dd$road=='moorhead' & dd$time<=255,]  

#. row 3:
d.17=dd[dd$txPwr=='high' & dd$txHeight=='high' & dd$traffic=='high' & dd$speed=='20mph' & dd$road=='lashley',]  
d.18=dd[dd$txPwr=='high' & dd$txHeight=='high' & dd$traffic=='high' & dd$speed=='20mph' & dd$road=='moorhead' & dd$time<=260,]  

d.19=dd[dd$txPwr=='high' & dd$txHeight=='low' & dd$traffic=='high' & dd$speed=='20mph' & dd$road=='lashley',]  
d.20=dd[dd$txPwr=='high' & dd$txHeight=='low' & dd$traffic=='high' & dd$speed=='20mph' & dd$road=='moorhead' & dd$time<=272,]  

d.21=dd[dd$txPwr=='low' & dd$txHeight=='high' & dd$traffic=='high' & dd$speed=='20mph' & dd$road=='lashley',]  
d.22=dd[dd$txPwr=='low' & dd$txHeight=='high' & dd$traffic=='high' & dd$speed=='20mph' & dd$road=='moorhead' & dd$time<=278,]  

d.23=dd[dd$txPwr=='low' & dd$txHeight=='low' & dd$traffic=='high' & dd$speed=='20mph' & dd$road=='lashley',]  
d.24=dd[dd$txPwr=='low' & dd$txHeight=='low' & dd$traffic=='high' & dd$speed=='20mph' & dd$road=='moorhead' & dd$time<=279,]  

#. row 4:
d.25=dd[dd$txPwr=='high' & dd$txHeight=='high' & dd$traffic=='high' & dd$speed=='30mph' & dd$road=='lashley' & dd$time<=86,]  
d.26=dd[dd$txPwr=='high' & dd$txHeight=='high' & dd$traffic=='high' & dd$speed=='30mph' & dd$road=='moorhead' & dd$time<=244,]  

d.27=dd[dd$txPwr=='high' & dd$txHeight=='low' & dd$traffic=='high' & dd$speed=='30mph' & dd$road=='lashley' & dd$time<=93,]  
d.28=dd[dd$txPwr=='high' & dd$txHeight=='low' & dd$traffic=='high' & dd$speed=='30mph' & dd$road=='moorhead' & dd$time<=255,]

d.29=dd[dd$txPwr=='low' & dd$txHeight=='high' & dd$traffic=='high' & dd$speed=='30mph' & dd$road=='lashley',]  
d.30=dd[dd$txPwr=='low' & dd$txHeight=='high' & dd$traffic=='high' & dd$speed=='30mph' & dd$road=='moorhead' & dd$time<=257,]

d.31=dd[dd$txPwr=='low' & dd$txHeight=='low' & dd$traffic=='high' & dd$speed=='30mph' & dd$road=='lashley',]  
d.32=dd[dd$txPwr=='low' & dd$txHeight=='low' & dd$traffic=='high' & dd$speed=='30mph' & dd$road=='moorhead' & dd$time<=262,]

dd = rbind(d.1,d.2,d.3,d.4,d.5,d.6,d.7,d.8,d.9,d.10,
          d.11,d.12,d.13,d.14,d.15,d.16,d.17,d.18,d.19,d.20,
          d.21,d.22,d.23,d.24,d.25,d.26,d.27,d.28,d.29,d.30,
          d.31,d.32)

rm(d.1,d.2,d.3,d.4,d.5,d.6,d.7,d.8,d.9,d.10, d.11,d.12,d.13,d.14,d.15,d.16,d.17,d.18,d.19,d.20, d.21,d.22,d.23,d.24,d.25,d.26,d.27,d.28,d.29,d.30, d.31,d.32)

# fix level names 
levels(dd$txHeight) = c('high', 'low')
levels(dd$txPwr) = c('47dBm', '37dBm')
levels(dd$traffic) = c('offPeak', 'peak')
names(dd)[24]='road'
levels(dd$road)=c('LOS', 'nonLOS')

# remove PL values higher than 140:
dd=dd[dd$bpl.vsa<=140,]

# remove traffic=peak and speed=30 and txPwr=47dBm
dd=dd[dd$traffic=='offPeak' & dd$speed=='20mph' & dd$txPwr=='47dBm',]
dd=dd[,-c(19,21,22,23)] # rm freq, txPwr, traffic, speed

dd = ddply(dd, .(txHeight, txPwr, road), mutate, idx=1:length(time))

# creade DF with uncategorized road data - ddn
# dda contained dd (los & nlos) and uncategorized data from other roads
# ddn contains uncategorized  road data - all other roads but lashley & moorh.
ddn=rbind(ntmp.lashley, ntmp.moorhead)
levels(ddn$txHeight)=c('high', 'low')
#dda$txPwr
levels(ddn$txPwr)=c('47dBm', '37dBm')
levels(ddn$traffic)=c('offPeak', 'peak')
ddn=ddn[ddn$traffic=='offPeak' & ddn$speed=='20mph' & ddn$txPwr=='47dBm',]

ddn = ddply(ddn, .(txHeight, txPwr, road), mutate, idx=1:length(time))
ddn=ddn[ddn$bpl.vsa!=0,]   # what about values >140 dB?
ddn=ddn[,-c(19,21,22,23)]


## ----------------------------------------------------
ggplot(ddn, aes(x=txHeight, y=bpl.vsa-fspl)) + 
       geom_boxplot(coef=1e10) + #ylab('Clutter Loss (dB)') +
       theme(legend.position = 'top') #+
       #facet_grid(.~txHeight, labeller=label_both) 
       #facet_grid(traffic+speed~txPwr+txHeight, labeller=label_both)  

# over time:
ggplot(ddn, aes(x=time, y=bpl.vsa-fspl, color=txHeight)) + 
#ggplot(ddn, aes(x=idx, y=bpl.vsa, color=txPwr)) + 
       geom_line() + #ylab('Clutter Loss (dB)') +
       theme(legend.position = 'top') #+
       #facet_grid(.~txHeight, labeller=label_both) 


## ----include=F---------------------------------------
# set up api key: 
#register_google(key='AIzaSyA7EB6miKS4BJHir0hLqlBEV90Bd9YhfTY', write=T)


## ----fig.height=4,dev='png',message=F----------------
loc=c(-105.26200, 39.99363)  # to center map    
loc=c(mean(ddn$lon), mean(ddn$lat)) 

#The lab rooftop GPS position is: 39.994832, -105.262199.
#The Green High GPS position is: 39.991778, -105.274669
lon=c(-105.262199, -105.274669) # tx1, tx2
lat=c(39.994832, 39.991778)

Location=c('low', 'high')
txLoc=data.frame(lon, lat, Location)

library(ggmap)
#plMap = get_map(location=loc, zoom=16, source='stamen', maptype='toner-lite', messaging=T)

# uncomment to force reload of map from internet
#plMap = get_map(location=loc, zoom=14, maptype='satellite', messaging=T)
#plMap = get_map(location=loc, zoom=15, source='stamen', maptype='terrain', messaging=T)
#loc = c(lon=mean(d.g$lon)+0.04000, lat=mean(d.g$lat-0.0400))




ggmap(plMap, darken=0.20, extent='panel') +
  geom_point(data=ddn, aes(x=lon, y=lat, colour=bpl.vsa-fspl), size=1.00) +
  #geom_point(data=txLoc, aes(x=lon, y=lat), pch=4,colour='orange', size=3) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL) + 
  facet_grid(.~txHeight, labeller = "label_both") +
  theme(legend.position = 'top') +
  theme(line = element_blank()) + theme(axis.text = element_blank())  


## ----fig.height=5------------------------------------
library(plyr)
d=rbind.fill(dd,ddn)
d$txPwr=NULL

ggmap(plMap, darken=0.20, extent='panel') +
  geom_point(data=d, aes(x=lon, y=lat, colour=bpl.vsa-fspl), size=1.00) +
  #geom_point(data=d, aes(x=lon, y=lat, colour=time), size=1.00) +
  #geom_point(data=txLoc, aes(x=lon, y=lat), pch=4,colour='orange', size=3) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL) + 
  facet_grid(txHeight~road, labeller = "label_both") +
  theme(legend.position = 'top') +
  theme(line = element_blank()) + theme(axis.text = element_blank()) 


## ----fig.height=5------------------------------------
ggplot(d, aes(x=time, y=bpl.vsa-fspl, linetype=txHeight, color=txHeight)) +
  geom_line() +
  #geom_point(size=1) +
  facet_grid(road~., labeller = "label_both") +
  theme(legend.position = 'top') 


## ----eval=F------------------------------------------
## ### remove repeated roads:
## 
## tmph=ddn[ddn$txHeight=='high',]   # low repeats 690, high 510
## tmph=tmph[1:690,]
## 
## # remove duplicate road data - high ant
## tmph=ddn[ddn$txHeight=='high',]   # low repeats 690, high 510
## tmph1=tmph[1:300,]  # first stretch
## tmph2=tmph[301:690,]  # first stretch
## #tmpl=ddn[ddn$txHeight=='low',]   # low repeats 690, high 510
## #tmpl=tmpl[1:510,]
## #plot(tmph$time)
## 
## ggmap(plMap, darken=0.20, extent='panel') +
##   #geom_point(data=d, aes(x=lon, y=lat, colour=bpl.vsa-fspl), size=1.00) +
##   geom_point(data=tmph2, aes(x=lon, y=lat, colour=time), size=1.00) +
##   scale_colour_gradientn(colours=c('blue', 'red', 'white')) +
##   labs(x=NULL, y=NULL) +
##   theme(line = element_blank()) + theme(axis.text = element_blank())
## 
## 
## # remove duplicate road data - low ant
## tmpl=ddn[ddn$txHeight=='low',]   # low repeats 690, high 510
## tmpl1=tmpl[50:105,]  # first stretch
## tmpl2=tmpl[270:800,]  # first stretch
## j=tmpl[c(50:105,270:800),]
## #tmpl2=tmpl[301:510,]  # first stretch
## 
## ggmap(plMap, darken=0.20, extent='panel') +
##   #geom_point(data=d, aes(x=lon, y=lat, colour=bpl.vsa-fspl), size=1.00) +
##   geom_point(data=j, aes(x=lon, y=lat, colour=time), size=1.00) +
##   scale_colour_gradientn(colours=c('blue', 'red', 'white')) +
##   labs(x=NULL, y=NULL) +
##   theme(line = element_blank()) + theme(axis.text = element_blank())


## ----------------------------------------------------
# create data set


### high ant data set: 
d.h=ddn[ddn$txHeight=='high',]    
d.h=d.h[301:690,]

### low ant data set: 
d.l=ddn[ddn$txHeight=='low',]    
d.l=d.l[c(50:105,270:800),]

# combine to one data set for analysis:
d=rbind(d.h, d.l) 


## ----fig.height=4------------------------------------
#plMap = get_map(location=loc, zoom=15, source='stamen', maptype='terrain', messaging=T)
ggmap(plMap, darken=0.20, extent='panel') +
  geom_point(data=d, aes(x=lon, y=lat, colour=time), size=1.00, alpha=0.25) +
  scale_colour_gradientn(colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL) + 
  facet_grid(.~txHeight) +
  theme(line = element_blank()) + theme(axis.text = element_blank()) +
  theme(legend.position = 'top') 


## ----eval=F, fig.width=10, fig.height=8, message=F, warning=F----
## #library(corrgram)
## library(GGally)
## tmp=d[,-c(1:3,5,6,7,10,16,17,18,20,21)]
## tmp$logK=log10(tmp$k)
## 
## # remove road & k
## ggpairs(tmp[,-c(5)])


## ----fig.height=3------------------------------------
ggplot(dd, aes(x=txHeight, y=(k))) + 
       geom_boxplot(coef=1e10) + #ylab('Clutter Loss (dB)') +
       theme(legend.position = 'top') +
       facet_grid(.~road)

#ggplot(dd, aes(x=txHeight, y=std.dbm)) + 
#       geom_boxplot(coef=1e10) + #ylab('Clutter Loss (dB)') +
#       theme(legend.position = 'top') +
#       facet_grid(.~road)


## ----fig.height=3------------------------------------
ggplot(dd, aes(x=(k), color=txHeight, fill=txHeight)) + 
       geom_histogram(bins=50, position='dodge') + 
       xlab('K (dB)') + ylab('Count') +
       theme(legend.position = 'top') +
       facet_grid(~road)


## ----fig.height=4------------------------------------
# uncomment to force reload of map from internet
#plMap = get_map(location=loc, zoom=14, maptype='satellite', messaging=T)
#plMap = get_map(location=loc, zoom=15, source='stamen', maptype='terrain', messaging=T)
#loc = c(lon=mean(d.g$lon)+0.04000, lat=mean(d.g$lat-0.0400))



ggmap(plMap, darken=0.20, extent='panel') +
  geom_point(data=dd, aes(x=lon, y=lat, colour=k), size=1.0) +
  #geom_point(data=txLoc, aes(x=lon, y=lat), pch=4,colour='orange', size=3) +
  #scale_colour_gradientn(name='Path Loss (dBm)', colours=c('blue', 'red', 'white')) +
  scale_colour_gradientn( colours=c('blue', 'red', 'white')) +
  labs(x=NULL, y=NULL) + 
  facet_grid(.~txHeight, labeller = "label_both") +
  theme(legend.position = 'top') +
  theme(line = element_blank()) + theme(axis.text = element_blank())  


## ----echo=T------------------------------------------
library(ltm)
biserial.cor(dd$k, dd$road)


## ----echo=T------------------------------------------
biserial.cor(dd$std.dbm, dd$road)


## ----echo=T------------------------------------------
cor.test(dd$std.dbm, dd$k, method='pearson')


## ----echo=T------------------------------------------
dd.aov=aov(k~txHeight*road, data=dd)
summary(dd.aov)


## ----echo=T------------------------------------------
library(effectsize)
omega_squared(dd.aov, partial=F, ci=0.95)


## ----eval=F------------------------------------------
## #par(mfrow=c(2,3))
## #plot(dd.aov, which=1:6)
## 
## 
## 
## library(ggstatsplot)
## #j=TukeyHSD(dd.aov)


## ----eval=F------------------------------------------
## install.packages('tmap')
## library(rgdal);library(maptools);library(dplyr);library(tidyr);library(tmap)
## 
## 

