###data 2015/2016
library(readxl) 
raf2004<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M05.xlsx")
raf2005<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M06.xlsx")
raf2006<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M07.xlsx")
raf2007<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M08.xlsx")
raf2008<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M09.xlsx")
raf2009<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M10.xlsx")
raf2010<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M11.xlsx")
raf2011<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M12.xlsx")
raf2012<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M13.xlsx")
raf2013<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M14.xlsx")
raf2014<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M15.xlsx")
raf2015<- read_excel("data/MJPV DATA 2004_2016/RAF_KRK_NL_M16.xlsx")

raf2004$jaar<- 2004;raf2005$jaar<- 2005;raf2006$jaar<- 2006;raf2007$jaar<- 2007;raf2008$jaar<- 2008;
raf2009$jaar<- 2009;raf2010$jaar<- 2010;raf2011$jaar<- 2011;raf2012$jaar<- 2012;raf2013$jaar<- 2013;
raf2014$jaar<- 2014;raf2015$jaar<- 2015;

raf_tot<- rbind(raf2004,raf2005,raf2006,raf2007,raf2008,raf2009,raf2010,raf2011,
                raf2012,raf2013,raf2014,raf2015)

#summary(raf_tot)
raf_tot$WEG<- raf_tot$WEGNUMMER

raf_tot$BAAN<- sub('NA$', '', paste(raf_tot$VOLGNUMMER_BAAN,raf_tot$BAANSOORT,raf_tot$BAANPOSITIE,sep=""))
raf_tot$STROOK<- paste(raf_tot$VOLGNUMMER_STROOK,raf_tot$SOORT_STROOK,raf_tot$STROOK_POSITIE,sep="")
raf_tot$WEG.BAAN.STROOK.VAN<- paste(raf_tot$WEG,raf_tot$BAAN,raf_tot$STROOK,raf_tot$VAN,sep=".")
raf_tot$WEG.BAAN.VAN<- paste(raf_tot$WEG,raf_tot$BAAN,raf_tot$VAN,sep=".")
raf_tot$AANLEGDATUM<- as.Date(raf_tot$AANLEGDATUM)



as.data.frame(table(raf_tot$DEKLAAGSOORT))
#raf_tot<- raf_tot[which(raf_tot$DEKLAAGSOORT %in% c("ZOAB","ZOABTW","ZOAB+")),]

raf_tot$INTERVENTIEJAAR_RAF<- raf_tot$IVJR_RAF
raf_tot$IVJR_RAF<-NULL
raf_tot$DKRNAAM<-NULL



#raf2015<- raf2015[,intersect(colnames(r.int.tot), colnames(raf2015))]
#raf2015


library(dplyr)
library(data.table)
#system.time(t<-split(raf_tot, raf_tot$WEG.BAAN.STROOK.VAN))
#save(t,file="t.RData")
#load("t.RData")

library(Hmisc)
raf_tot<- raf_tot[order(raf_tot$WEG.BAAN.STROOK.VAN,raf_tot$jaar),]
raf_tot$vervangen<- ifelse(raf_tot$WEG.BAAN.STROOK.VAN == Lag(raf_tot$WEG.BAAN.STROOK.VAN ) & 
                                      raf_tot$AANLEGDATUM > Lag(raf_tot$AANLEGDATUM),1,0)
raf_tot$vervangen.leeftijd<- ifelse(raf_tot$vervangen==1, (raf_tot$AANLEGDATUM - Lag(raf_tot$AANLEGDATUM))/365,NA)
raf_tot$verdacht<- ifelse(raf_tot$WEG.BAAN.STROOK.VAN == Lag(raf_tot$WEG.BAAN.STROOK.VAN ) & 
                               (raf_tot$AANLEGDATUM - Lag(raf_tot$AANLEGDATUM))/365< -1,1,0)

#weg.baan.strook.van verdacht
verd<- raf_tot$WEG.BAAN.STROOK.VAN[which(raf_tot$verdacht==1)]

raf_tot<- raf_tot[which(!raf_tot$WEG.BAAN.STROOK.VAN %in% verd),]

raf_tot$vvr<- ifelse(raf_tot$vervangen==1&Lag(raf_tot$INTERVENTIEJAAR_RAF-raf_tot$jaar<5),1,0)
raf_tot$leeftijd<- raf_tot$jaar-as.numeric(format(raf_tot$AANLEGDATUM,'%Y'))
raf_tot$jr.tot.interventie<- raf_tot$INTERVENTIEJAAR_RAF-raf_tot$jaar
raf_tot$jr.tot.interventie<- ifelse(raf_tot$jr.tot.interventie< (-10),NA,raf_tot$jr.tot.interventie)
raf_tot$jr.tot.interventie<- ifelse(raf_tot$jr.tot.interventie>5,6,raf_tot$jr.tot.interventie)


raf_tot$ldr.verw<- raf_tot$INTERVENTIEJAAR_RAF- as.numeric(format(raf_tot$AANLEGDATUM,'%Y'))
#verwijder negatieve waardes (voor interventiejaren 1900)
raf_tot$ldr.verw<- ifelse(raf_tot$ldr.verw>-10,raf_tot$ldr.verw,NA)




#####Laad bodemkaart######
library(rgdal)
bodemkaart<- readOGR("data/GIS data/bodemkaart_250.shp",layer="bodemkaart_250")
#bodemkaart<- as.data.frame(bodemkaart)

#####Laad weggeg#####

#weggeg.vmax<-readOGR("data/GIS data/weggeg_maxsnelhedenoverdag.shp",layer="weggeg_maxsnelhedenoverdag")
#weggeg.vmax<- as.data.frame(weggeg.vmax)

#r.int.tot$max.snelheid<- 


nwb.hectopunten<-readOGR("data/GIS data/NWB_hectopunten.shp",layer="NWB_hectopunten")
#coordinates(hectoptn)<- ~coords.x1 + coords.x2
nwb.hectopunten$bodem<-over(nwb.hectopunten, bodemkaart)$CODE2
#nwb.hectopunten$vmax<-over(nwb.hectopunten, weggeg.vmax)$OMSCHR
#nwb.hectopunten<- as.data.frame(nwb.hectopunten)

library(raster)
pgroen_density<- raster("data/GIS data_1910/pgroen_density.tif")
nwb.hectopunten$pgroen_density<- extract(pgroen_density,nwb.hectopunten,method="bilinear")

#####Link naar NWB gegevens voor verkrijgen van een wegvakID########
library(rgdal)
nwb.wegvakken<-readOGR("data/GIS data/NWB_wegvakken.shp",layer="NWB_wegvakken")
#nwb.hectopunten<-readOGR("data/GIS data/NWB_hectopunten.shp",layer="NWB_hectopunten")

wegvkn<- as.data.frame(nwb.wegvakken)
hectoptn<- as.data.frame(nwb.hectopunten)

hectoptn$WEG<- wegvkn$WEGNUMMER[match(hectoptn$WVK_ID,wegvkn$WVK_ID)]
hectoptn$WEG<- as.numeric(as.character(hectoptn$WEG))
hectoptn$BAANSUBSRT<- as.character(wegvkn$BAANSUBSRT[match(hectoptn$WVK_ID,wegvkn$WVK_ID)])

hectoptn$POS_TV_WOL<- wegvkn$POS_TV_WOL[match(hectoptn$WVK_ID,wegvkn$WVK_ID)]
hectoptn$baan<- ifelse(hectoptn$BAANSUBSRT %in% c("VBD","VBI","VBS","VBR","VBK","VBW"),"VW",hectoptn$BAANSUBSRT)

hectoptn$nwb.key<- paste(hectoptn$WEG,hectoptn$HECTOMTRNG,hectoptn$baan,hectoptn$POS_TV_WOL,sep=".")


##
raf_tot$nwb.key<- paste(raf_tot$WEG,(raf_tot$VAN*10), raf_tot$BAANSOORT, ifelse(raf_tot$BAANPOSITIE %in% "M",raf_tot$BAANPOSITIE,raf_tot$STROOK_POSITIE),sep="." )

# test2<- raf_tot[which(raf_tot$BAANSOORT=="HR"),c("BAANPOSITIE","STROOK_POSITIE")]
# test2$verschil<- ifelse(test2$BAANPOSITIE == test2$STROOK_POSITIE,0,1)

#hectoptn$VAN<- hectoptn$HECTOMTRNG/10

# test<- raf_tot[which(raf_tot$WEG==12 & raf_tot$VAN <62 & raf_tot$VAN >60),]
# 
# wv<- wegvkn[,c("WVK_ID","RPE_CODE","POS_TV_WOL")]
# wv$RPE_CODE<- as.character(wv$RPE_CODE);wv$POS_TV_WOL<- as.character(wv$POS_TV_WOL)
# wv$verschil<- ifelse(wv$RPE_CODE == wv$POS_TV_WOL,0,1)


#r.int.tot$WEG.VAN.BAAN<- paste(r.int.tot$WEG,r.int.tot$VAN,r.int.tot$BAAN,sep=".")

#write.csv(hectoptn,"data/hectoptn.csv")

raf_tot$x<- hectoptn$coords.x1[match(raf_tot$nwb.key,hectoptn$nwb.key)]
raf_tot$y<- hectoptn$coords.x2[match(raf_tot$nwb.key,hectoptn$nwb.key)]
raf_tot$WVK_ID<- hectoptn$WVK_ID[match(raf_tot$nwb.key,hectoptn$nwb.key)]
raf_tot$bodem<- hectoptn$bodem[match(raf_tot$nwb.key,hectoptn$nwb.key)]
raf_tot$pgroen_density<- hectoptn$pgroen_density[match(raf_tot$nwb.key,hectoptn$nwb.key)]


####BPS#######
#bps<- readOGR("data/GIS data/BPS.shp",layer="BPS")

#bps<- as.data.frame(bps)


#####wegcategorien######
wegcat<- readOGR("data/GIS data/categorien.shp",layer="categorien")
wegcat<-as.data.frame(wegcat)
raf_tot$cat<- wegcat$WEGTYPE[match(raf_tot$WVK_ID,wegcat$WVK_ID)]

#districtnamen
raf_tot$district<- wegcat$DISTRNAAM[match(raf_tot$WVK_ID,wegcat$WVK_ID)]
raf_tot$dienst<- wegcat$DIENSTCODE[match(raf_tot$WVK_ID,wegcat$WVK_ID)]

#asfalttype lag (om te kunnen bepalen welk asfaltsoort er lag voor vervanging)
raf_tot$deklaagsoort.lag<- lag(raf_tot$DEKLAAGSOORT)

##bermtype

weggeg.bermgeg<- readOGR("data/GIS data/weggeg_wegbermen.shp",layer="weggeg_wegbermen")
weggeg.bermgeg<- as.data.frame(weggeg.bermgeg)

raf_tot$bermtype<- weggeg.bermgeg$OMSCHR[match(raf_tot$WVK_ID,weggeg.bermgeg$WVK_ID)]
raf_tot$afst.obst<- weggeg.bermgeg$AFST_OBST[match(raf_tot$WVK_ID,weggeg.bermgeg$WVK_ID)]



##kunstwerken


library(rgdal )
weggeg.kunstinweg<- readOGR("data/GIS data/WEGGEG_kunstinwegt.shp",layer="WEGGEG_kunstinwegt")
weggeg.kunstinweg<- as.data.frame(weggeg.kunstinweg)

raf_tot$kunstwerk<- weggeg.kunstinweg$OMSCHR[match(raf_tot$WVK_ID,weggeg.kunstinweg$WVK_ID)]

summary(r.int.tot$kunstwerk)

#save(raf_tot,file="data/raf_tot.RData")

#verloop
library(Hmisc)

raf_tot<- raf_tot[order(raf_tot$WEG.BAAN.STROOK.VAN,raf_tot$jaar),]
#raf_tot$num.jr.int<- ifelse()
raf_tot$verschil_planjaar<- ifelse(raf_tot$WEG.BAAN.STROOK.VAN == Lag(raf_tot$WEG.BAAN.STROOK.VAN ) & Lag(raf_tot$jr.tot.interventie<= 5) &
                                       raf_tot$jaar - Lag(raf_tot$jaar) == 1, 
                                     raf_tot$jr.tot.interventie - Lag(raf_tot$jr.tot.interventie),NA)

raf_tot$verloop<- ifelse(raf_tot$verschil_planjaar== -1, "Normaal", ifelse(raf_tot$verschil_planjaar < (-1),"Snel",
                                                                               ifelse(raf_tot$verschil_planjaar == 0, "Langzaam",NA)))
raf_tot$schade<- ifelse(raf_tot$jr.tot.interventie<= 5,1,0)


##eerste keer schade
raf_tot$eerste.schade<- ifelse(raf_tot$WEG.BAAN.STROOK.VAN == Lag(raf_tot$WEG.BAAN.STROOK.VAN ) & Lag(raf_tot$jr.tot.interventie==6) &
                                   raf_tot$jr.tot.interventie<= 5,
                                 1,0)
raf_tot$eerste.schade.leeftijd<- ifelse(raf_tot$WEG.BAAN.STROOK.VAN == Lag(raf_tot$WEG.BAAN.STROOK.VAN ) & Lag(raf_tot$jr.tot.interventie==6) &
                                            raf_tot$jr.tot.interventie<= 5,
                                          raf_tot$leeftijd,NA)

eerste.schade.leeftijd<- aggregate(eerste.schade.leeftijd~WEG.BAAN.STROOK.VAN+DEKLAAGSOORT,raf_tot,FUN="mean")



ggplot(eerste.schade.leeftijd,aes(x=eerste.schade.leeftijd))+geom_histogram(binwidth = 1,color="black")+facet_wrap(~DEKLAAGSOORT,scales="free_y")

#save(raf_tot,file="data/raf_tot.RData")

####INweva
inweva_werkdagen_2012_2015<- readRDS(file="data/inweva_werkdagen_2012_2015.RDS")

inw2012<- inweva_werkdagen_2012_2015[[1]]
inw2013<- inweva_werkdagen_2012_2015[[2]]
inw2014<- inweva_werkdagen_2012_2015[[3]]
inw2015<- inweva_werkdagen_2012_2015[[4]]

inw2012$jaar<- 2012;inw2013$jaar<- 2013;inw2014$jaar<- 2014;inw2015$jaar<- 2015;

inw2012$WVK_ID_jaar<- paste(inw2012$WVK_ID,2012,sep="_")
inw2013$WVK_ID_jaar<- paste(inw2013$WVK_ID,2013,sep="_")
inw2014$WVK_ID_jaar<- paste(inw2014$WVK_ID,2014,sep="_")
inw2015$WVK_ID_jaar<- paste(inw2015$WVK_ID,2015,sep="_")


inwtot<- rbind(inw2012,inw2013,inw2014,inw2015)

#saveRDS(inwtot,file="data/inweva2012_2015.RDS")

inw_gem<- aggregate(cbind(Etmaal_AL,Etmaal_L1,Etmaal_L2,Etmaal_L3) ~ WVK_ID,inwtot,FUN="mean")

saveRDS(inw_gem,file="data/inweva_gem2012_2015.RDS")

##match with raf_tot

raf_tot$WVK_ID_jaar<- paste(raf_tot$WVK_ID,raf_tot$jaar,sep="_")

raf_tot$Inw_etm_AL<- inwtot$Etmaal_AL[match(raf_tot$WVK_ID_jaar,inwtot$WVK_ID_jaar)]
raf_tot$Inw_etm_L1<- inwtot$Etmaal_L1[match(raf_tot$WVK_ID_jaar,inwtot$WVK_ID_jaar)]
raf_tot$Inw_etm_L2<- inwtot$Etmaal_L2[match(raf_tot$WVK_ID_jaar,inwtot$WVK_ID_jaar)]
raf_tot$Inw_etm_L3<- inwtot$Etmaal_L3[match(raf_tot$WVK_ID_jaar,inwtot$WVK_ID_jaar)]

raf_tot$gem_Inw_etm_AL<- inw_gem$Etmaal_AL[match(raf_tot$WVK_ID,inw_gem$WVK_ID)]
raf_tot$gem_Inw_etm_L1<- inw_gem$Etmaal_L1[match(raf_tot$WVK_ID,inw_gem$WVK_ID)]
raf_tot$gem_Inw_etm_L2<- inw_gem$Etmaal_L2[match(raf_tot$WVK_ID,inw_gem$WVK_ID)]
raf_tot$gem_Inw_etm_L3<- inw_gem$Etmaal_L3[match(raf_tot$WVK_ID,inw_gem$WVK_ID)]


#save(raf_tot,file="data/raf_tot.RData")


###freeze counts



