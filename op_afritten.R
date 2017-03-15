###in-uitvoegstroken

library(rgdal)
in_uit<- readOGR("data/BPS_in_uitvoegstroken/in-uitvoegstroken.shp")

in_uit$x <- getSpPPolygonsLabptSlots(in_uit)[,1]
in_uit$y <- getSpPPolygonsLabptSlots(in_uit)[,2]

in_uit<- as.data.frame(in_uit)

in_uit$baan_edit<- paste0(in_uit$BAANSOORT,in_uit$BAANPOSITI)


in_hrr<- in_uit[which(in_uit$baan_edit %in% "HRR"& in_uit$STROOKSOOR %in% "I-"),]
in_hrl<- in_uit[which(in_uit$baan_edit %in% "HRL"& in_uit$STROOKSOOR %in% "I-"),]
in_vwl<- in_uit[which(in_uit$baan_edit %in% "VWL"& in_uit$STROOKSOOR %in% "I-"),]
in_vwr<- in_uit[which(in_uit$baan_edit %in% "VWR"& in_uit$STROOKSOOR %in% "I-"),]

uit_hrr<- in_uit[which(in_uit$baan_edit %in% "HRR"& in_uit$STROOKSOOR %in% "U-"),]
uit_hrl<- in_uit[which(in_uit$baan_edit %in% "HRL"& in_uit$STROOKSOOR %in% "U-"),]
uit_vwl<- in_uit[which(in_uit$baan_edit %in% "VWL"& in_uit$STROOKSOOR %in% "U-"),]
uit_vwr<- in_uit[which(in_uit$baan_edit %in% "VWR"& in_uit$STROOKSOOR %in% "U-"),]


####
load("data/raf_tot.RData")

raf_tot$baanedit<- paste0(raf_tot$BAANSOORT,raf_tot$STROOK_POSITIE)
raf_tot$ID<- seq.int(nrow(raf_tot))

raf_hrr<- raf_tot[which(raf_tot$baanedit %in% "HRR"),]
raf_hrl<- raf_tot[which(raf_tot$baanedit %in% "HRL"),]
raf_vwr<- raf_tot[which(raf_tot$baanedit %in% "VWR"),]
raf_vwl<- raf_tot[which(raf_tot$baanedit %in% "VWL"),]

raf_hrr<- raf_hrr[complete.cases(raf_hrr$x),]
raf_hrl<- raf_hrl[complete.cases(raf_hrl$x),]
raf_vwr<- raf_vwr[complete.cases(raf_vwr$x),]
raf_vwl<- raf_vwl[complete.cases(raf_vwl$x),]



library(FNN)
raf_hrr$distance_invoeg<- knnx.dist(in_hrr[,c("x","y")], raf_hrr[,c("x","y")], k=1)[,1]
raf_hrl$distance_invoeg<- knnx.dist(in_hrl[,c("x","y")], raf_hrl[,c("x","y")], k=1)[,1]
raf_vwr$distance_invoeg<- knnx.dist(in_vwr[,c("x","y")], raf_vwr[,c("x","y")], k=1)[,1]
raf_vwl$distance_invoeg<- knnx.dist(in_vwl[,c("x","y")], raf_vwl[,c("x","y")], k=1)[,1]

raf_hrr$distance_uitvoeg<- knnx.dist(uit_hrr[,c("x","y")], raf_hrr[,c("x","y")], k=1)[,1]
raf_hrl$distance_uitvoeg<- knnx.dist(uit_hrl[,c("x","y")], raf_hrl[,c("x","y")], k=1)[,1]
raf_vwr$distance_uitvoeg<- knnx.dist(uit_vwr[,c("x","y")], raf_vwr[,c("x","y")], k=1)[,1]
raf_vwl$distance_uitvoeg<- knnx.dist(uit_vwl[,c("x","y")], raf_vwl[,c("x","y")], k=1)[,1]

raf_tot2<- rbind(raf_hrl,raf_hrr,raf_vwl,raf_vwr)

raf_tot$distance_invoeg<- raf_tot2$distance_invoeg[match(raf_tot$ID,raf_tot2$ID)]
raf_tot$distance_uitvoeg<- raf_tot2$distance_uitvoeg[match(raf_tot$ID,raf_tot2$ID)]


#save(raf_tot,file="data/raf_tot.RData")

raf_eerste_schade<- raf_tot[which(raf_tot$eerste.schade==1),]
library(ggplot2)
ggplot(raf_eerste_schade,aes(y=eerste.schade.leeftijd,x=distance_uitvoeg))+geom_point(alpha=0.3,size=0.1)
