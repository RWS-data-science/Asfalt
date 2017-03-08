##Laad WIM geaggregeerd van Elise

library(data.table)
wim_elise<- fread("data/Verwerkte_data/combined_wim_rafeling_week3.csv",header=T)

wim_elise$WEG.BAAN.STROOK.VAN<- paste(wim_elise$weg,wim_elise$baan,wim_elise$strook,wim_elise$van,sep=".")

#Geaggregeerde wim data naar r.int.tot

#Data zijn geaggregeerd over de maanden jan-jun 2015 en geextrapoleerd over de wegvakken waar de
#weegpunten liggen (tussen 2 knooppunten)
raf_tot$asdruk1_mean<- wim_elise$asdruk1_mean[match(raf_tot$WEG.BAAN.STROOK.VAN,wim_elise$WEG.BAAN.STROOK.VAN)]

summary(raf_tot$asdruk1_mean)

raf_tot$asdruk1_P_50<- wim_elise$asdruk1_P_50[match(raf_tot$WEG.BAAN.STROOK.VAN,wim_elise$WEG.BAAN.STROOK.VAN)]
raf_tot$asdruk1_P_84<- wim_elise$asdruk1_P_84[match(raf_tot$WEG.BAAN.STROOK.VAN,wim_elise$WEG.BAAN.STROOK.VAN)]
raf_tot$asdruk2_mean<- wim_elise$asdruk2_mean[match(raf_tot$WEG.BAAN.STROOK.VAN,wim_elise$WEG.BAAN.STROOK.VAN)]
raf_tot$asdruk2_P_50<- wim_elise$asdruk2_P_50[match(raf_tot$WEG.BAAN.STROOK.VAN,wim_elise$WEG.BAAN.STROOK.VAN)]
raf_tot$asdruk2_P_84<- wim_elise$asdruk2_P_84[match(raf_tot$WEG.BAAN.STROOK.VAN,wim_elise$WEG.BAAN.STROOK.VAN)]

#save(raf_tot,file="data/raf_tot.RData")

library(ggplot2)
library(meteo)
library(rgdal)
data("NLpol")
#proj4string(NLpol) <- proj4string(nwb.hectopunten) # RD
NLpol_f= fortify(NLpol)
coordinates(NLpol_f)<- ~long+lat
proj4string(NLpol_f)<- CRS("+init=epsg:4326")
proj_rd<-  CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
NLpol_f<- spTransform(NLpol_f,proj_rd)
NLpol_f<- as.data.frame(NLpol_f)

#rm NA's from r.int.wegdeel

#r.int.wegdeel.narm2<- r.int.wegdeel[which(is.na(r.int.wegdeel$ldr.verw.gem)==F),]

#png("plots/leeftijden_op_kaart.png",width = 720,height = 720,res=100)
ggplot(NLpol_f, aes(x = long, y = lat,group=group))+geom_polygon(colour="black",fill="white")+
  geom_point(data=raf_tot,aes(x=x,y=y,group=asdruk1_P_84,colour=asdruk1_P_84,na.rm=T),size=0.8)+
  scale_colour_gradientn(colours=c("blue","red"),na.value="lightgrey")
#dev.off()