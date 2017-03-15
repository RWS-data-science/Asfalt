##load filedata
rm(list=ls())
library(data.table)
files_2010<- fread("data/NIS_files/files_2010.csv",dec=",")
files_2011<- fread("data/NIS_files/files_2011.csv",dec=",")
files_2012<- fread("data/NIS_files/files_2012.csv",dec=",")
files_2013<- fread("data/NIS_files/files_2013.csv",dec=",")
files_2014<- fread("data/NIS_files/files_2014.csv",dec=",")
files_2015<- fread("data/NIS_files/files_2015.csv",dec=",")

files_tot<- rbind(files_2010,files_2011,files_2012,files_2013,files_2014,files_2015)
rm(files_2010,files_2011,files_2012,files_2013,files_2014,files_2015)

files_tot$baan<- ifelse(files_tot$Richting==1, "1HRL",ifelse(files_tot$Richting==2,"1HRR",NA)) ## aangenomen dat files altijd op de hoofdrijbaan plaatsvinden
files_tot$Hm<- as.numeric(sub(",", ".", files_tot$Hm))
files_tot$Filezwaarte<- as.numeric(sub(",", ".", files_tot$Filezwaarte))
files_tot$Filelengte<- as.numeric(sub(",", ".", files_tot$Filelengte))
files_tot$Fileduur<- as.numeric(sub(",", ".", files_tot$Fileduur))

files_tot$weg.van.baan<- paste(files_tot$Wegnummer,files_tot$Hm,files_tot$baan,sep=".")

#load nwb
library(rgdal)
nwb.wegvakken<-readOGR("data/GIS data/NWB_wegvakken.shp",layer="NWB_wegvakken")
nwb.hectopunten<-readOGR("data/GIS data/NWB_hectopunten.shp",layer="NWB_hectopunten")

wegvkn<- as.data.frame(nwb.wegvakken)
hectoptn<- as.data.frame(nwb.hectopunten)

hectoptn$WEG<- wegvkn$WEGNUMMER[match(hectoptn$WVK_ID,wegvkn$WVK_ID)]
hectoptn$WEG<- as.numeric(as.character(hectoptn$WEG))
hectoptn$VAN<- hectoptn$HECTOMTRNG/10

hectoptn$POS_TV_WOL<- wegvkn$POS_TV_WOL[match(hectoptn$WVK_ID,wegvkn$WVK_ID)]
hectoptn$WEG.VAN.BAAN<- paste(hectoptn$WEG,hectoptn$VAN,paste("1HR",hectoptn$POS_TV_WOL,sep=""),sep=".")

files_tot$WVK_ID<- hectoptn$WVK_ID[match(files_tot$weg.van.baan,hectoptn$WEG.VAN.BAAN)]

summary(files_tot$WVK_ID)

##totale filezwaarte per WVK_ID

agg<- aggregate(Filezwaarte ~ WVK_ID+Jaar,data=files_tot,FUN="sum")
saveRDS(agg,file="data/filezwaarte_wegvakken.RDS")

##Match aan r_int
load("data/r_int_tot.RData")
agg$WVK_jr<- paste0(agg$WVK_ID,agg$Jaar)

r.int.tot$WVK_jr<- paste0(r.int.tot$WVK_ID,r.int.tot$jaar)
r.int.tot$filezwaarte_jaar<- agg$Filezwaarte[match(r.int.tot$WVK_jr,agg$WVK_jr)]
aggtot<- aggregate(Filezwaarte ~ WVK_ID,data=files_tot,FUN="sum")

r.int.tot$filezwaarte_2010_2015<- aggtot$Filezwaarte[match(r.int.tot$WVK_ID,agg$WVK_ID)]

r.int.tot$WVK_jr<- NULL

#save(r.int.tot,file="data/r_int_tot.RData")


##plot locaties met veel files (testje)
wegvakken.file<- nwb.wegvakken[which(nwb.wegvakken$WVK_ID %in% files_tot$WVK_ID),]
aggtot<- aggregate(Filezwaarte ~ WVK_ID,data=files_tot,FUN="sum")
wegvakken.file$Filezwaarte<- aggtot$Filezwaarte[match(wegvakken.file$WVK_ID,aggtot$WVK_ID)]

library(leaflet)
wgs<- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj_rd<-  CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
proj4string(wegvakken.file)<- proj_rd
wegvakken.file<- spTransform(wegvakken.file,wgs)
wegvakken.file<- wegvakken.file[order(wegvakken.file$Filezwaarte),]

#qpal <- colorQuantile("Reds", wegvakken.file$Filezwaarte, n = 7)
pal <- colorNumeric(
  palette = "YlOrRd",
  wegvakken.file$Filezwaarte
)

m <- leaflet(wegvakken.file) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  #addProviderTiles("Stamen.Toner") %>%
  addPolylines(color = ~pal(Filezwaarte),weight=5) %>%
  addLegend("bottomright", pal = pal, values = ~Filezwaarte,
            title = "Filezwaarte",
            opacity = 1
  )
m

  