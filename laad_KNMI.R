##Laad KNMI data

aws<- read.table(file="data/KNMI_20160101_daily.txt.tsv", sep = ",",dec=".",
                 col.names=c("STN","YYYYMMDD","DDVEC","FHVEC",   "FG",  "FHX", "FHXH",  "FHN", "FHNH",
                             "FXX", "FXXH",   "TG",   "TN",  "TNH",   "TX",  "TXH", "T10N","T10NH",   "SQ",
                             "SP",    "Q",   "DR",   "RH",  "RHX", "RHXH",   "PG",   "PX",  "PXH",   "PN",
                            "PNH",  "VVN", "VVNH",  "VVX", "VVXH",   "NG",   "UG",   "UX",  "UXH",   "UN",  "UNH", "EV24"))



# 
# YYYYMMDD = Datum (YYYY=jaar MM=maand DD=dag); 
# DDVEC    = Vectorgemiddelde windrichting in graden (360=noord, 90=oost, 180=zuid, 270=west, 0=windstil/variabel). Zie http://www.knmi.nl/kennis-en-datacentrum/achtergrond/klimatologische-brochures-en-boeken; 
# FHVEC    = Vectorgemiddelde windsnelheid (in 0.1 m/s). Zie http://www.knmi.nl/kennis-en-datacentrum/achtergrond/klimatologische-brochures-en-boeken; 
# FG       = Etmaalgemiddelde windsnelheid (in 0.1 m/s); 
# FHX      = Hoogste uurgemiddelde windsnelheid (in 0.1 m/s); 
# FHXH     = Uurvak waarin FHX is gemeten; 
# FHN      = Laagste uurgemiddelde windsnelheid (in 0.1 m/s); 
# FHNH     = Uurvak waarin FHN is gemeten; 
# FXX      = Hoogste windstoot (in 0.1 m/s); 
# FXXH     = Uurvak waarin FXX is gemeten; 
# TG       = Etmaalgemiddelde temperatuur (in 0.1 graden Celsius); 
# TN       = Minimum temperatuur (in 0.1 graden Celsius); 
# TNH      = Uurvak waarin TN is gemeten; 
# TX       = Maximum temperatuur (in 0.1 graden Celsius); 
# TXH      = Uurvak waarin TX is gemeten; 
# T10N     = Minimum temperatuur op 10 cm hoogte (in 0.1 graden Celsius); 
# T10NH    = 6-uurs tijdvak waarin T10N is gemeten; 6=0-6 UT, 12=6-12 UT, 18=12-18 UT, 24=18-24 UT
# SQ       = Zonneschijnduur (in 0.1 uur) berekend uit de globale straling (-1 voor <0.05 uur); 
# SP       = Percentage van de langst mogelijke zonneschijnduur; 
# Q        = Globale straling (in J/cm2); 
# DR       = Duur van de neerslag (in 0.1 uur); 
# RH       = Etmaalsom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm); 
# RHX      = Hoogste uursom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm); 
# RHXH     = Uurvak waarin RHX is gemeten; 
# PG       = Etmaalgemiddelde luchtdruk herleid tot zeeniveau (in 0.1 hPa) berekend uit 24 uurwaarden; 
# PX       = Hoogste uurwaarde van de luchtdruk herleid tot zeeniveau (in 0.1 hPa); 
# PXH      = Uurvak waarin PX is gemeten; 
# PN       = Laagste uurwaarde van de luchtdruk herleid tot zeeniveau (in 0.1 hPa); 
# PNH      = Uurvak waarin PN is gemeten; 
# VVN      = Minimum opgetreden zicht; 0: <100 m, 1:100-200 m, 2:200-300 m,..., 49:4900-5000 m, 50:5-6 km, 56:6-7 km, 57:7-8 km,..., 79:29-30 km, 80:30-35 km, 81:35-40 km,..., 89: >70 km)
# VVNH     = Uurvak waarin VVN is gemeten; 
# VVX      = Maximum opgetreden zicht; 0: <100 m, 1:100-200 m, 2:200-300 m,..., 49:4900-5000 m, 50:5-6 km, 56:6-7 km, 57:7-8 km,..., 79:29-30 km, 80:30-35 km, 81:35-40 km,..., 89: >70 km)
# VVXH     = Uurvak waarin VVX is gemeten; 
# NG       = Etmaalgemiddelde bewolking (bedekkingsgraad van de bovenlucht in achtsten, 9=bovenlucht onzichtbaar); 
# UG       = Etmaalgemiddelde relatieve vochtigheid (in procenten); 
# UX       = Maximale relatieve vochtigheid (in procenten); 
# UXH      = Uurvak waarin UX is gemeten; 
# UN       = Minimale relatieve vochtigheid (in procenten); 
# UNH      = Uurvak waarin UN is gemeten; 
# EV24     = Referentiegewasverdamping (Makkink) (in 0.1 mm); 



###Geocoding:

aws_loc<- read.csv(file = "data/knmi_aws_coord.csv")
aws_loc<- aws_loc[complete.cases(aws_loc),]

library(rgdal)
coordinates(aws_loc)<- ~LON.east. + LAT.north.
proj4string(aws_loc)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"
rd<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs" 

aws_loc<-spTransform(aws_loc,rd)
aws_loc<-as.data.frame(aws_loc)

aws$x<- aws_loc$LON.east.[match(aws$STN,aws_loc$STN)]
aws$y<- aws_loc$LAT.north.[match(aws$STN,aws_loc$STN)]

aws$Date<- as.Date(as.character(aws$YYYYMMDD),format="%Y%m%d")
#save(aws,file="data/aws.RData")
#load("data/aws.RData")



##Weer tijdens aanleg

#interpolatie nn
library(gstat)
#library(sp)
r.int.narm<- r.int.tot[which(!is.na(r.int.tot$x)),]


dates<- unique(r.int.narm$AANLEGDATUM)
dates<- dates[which(!format(dates,"%m-%d") %in% c("12-31","11-30","01-01") )] #deze data worden gebruikt als de exacte datum niet bekend is

list<- list()
for (i in 1:length(dates)){
  r.x<- as.data.frame(r.int.narm[which(r.int.narm$AANLEGDATUM == dates[i]),])
  a.x<- aws[which(aws$Date == dates[i]),]
  if(nrow(a.x)>0){
    a.x<- a.x[which(!(is.na(a.x$FG)|is.na(a.x$TG)|is.na(a.x$TN) )),]
    a.x<- a.x[which(!(is.na(a.x$RH) )),]
    coordinates(r.x)<- ~ x+y
    coordinates(a.x)<- ~x+y
    #krige met n=1 interpoleert kiest waarde van 1 nearest neighbor
    #idw doet inverse distance weighting met alle punten
    r.x$aanleg.FG = krige(FG  ~ 1, a.x, r.x, nmax = 1)$var1.pred/10 #delen door 10 ivm eenheid van KNMI data (0.1)
    #r.x$aanleg.FG = idw(FG  ~ 1, a.x, r.x)$var1.pred/10
    r.x$aanleg.FHX = krige(FHX ~ 1, a.x, r.x, nmax = 1)$var1.pred/10
    r.x$aanleg.TG = krige(TG ~ 1, a.x, r.x, nmax = 1)$var1.pred/10
    r.x$aanleg.TX = krige(TX ~ 1, a.x, r.x, nmax = 1)$var1.pred/10
    r.x$aanleg.TN = krige(TN ~ 1, a.x, r.x, nmax = 1)$var1.pred/10
    r.x$aanleg.RH = krige(RH ~ 1, a.x, r.x, nmax = 1)$var1.pred/10
    r.x<- as.data.frame(r.x)  
    
    list[[i]]<- r.x
  }
  cat(i/length(dates))
}

library(data.table)
r.int.knmi<- rbindlist(list)

###merge in r.int.tot
r.int.tot$WEG.BAAN.STROOK.VAN.JAAR<- paste(r.int.tot$WEG.BAAN.STROOK.VAN,r.int.tot$jaar,sep=".")
r.int.knmi$WEG.BAAN.STROOK.VAN.JAAR<- paste(r.int.knmi$WEG.BAAN.STROOK.VAN,r.int.knmi$jaar,sep=".")

r.int.tot$aanleg.FG<- r.int.knmi$aanleg.FG[match(r.int.tot$WEG.BAAN.STROOK.VAN.JAAR,r.int.knmi$WEG.BAAN.STROOK.VAN.JAAR)]
r.int.tot$aanleg.FHX<- r.int.knmi$aanleg.FHX[match(r.int.tot$WEG.BAAN.STROOK.VAN.JAAR,r.int.knmi$WEG.BAAN.STROOK.VAN.JAAR)]
r.int.tot$aanleg.TG<- r.int.knmi$aanleg.TG[match(r.int.tot$WEG.BAAN.STROOK.VAN.JAAR,r.int.knmi$WEG.BAAN.STROOK.VAN.JAAR)]
r.int.tot$aanleg.TX<- r.int.knmi$aanleg.TX[match(r.int.tot$WEG.BAAN.STROOK.VAN.JAAR,r.int.knmi$WEG.BAAN.STROOK.VAN.JAAR)]
r.int.tot$aanleg.TN<- r.int.knmi$aanleg.TN[match(r.int.tot$WEG.BAAN.STROOK.VAN.JAAR,r.int.knmi$WEG.BAAN.STROOK.VAN.JAAR)]
r.int.tot$aanleg.RH<- r.int.knmi$aanleg.RH[match(r.int.tot$WEG.BAAN.STROOK.VAN.JAAR,r.int.knmi$WEG.BAAN.STROOK.VAN.JAAR)]

#save(r.int.tot,file="data/r_int_tot.RData")



