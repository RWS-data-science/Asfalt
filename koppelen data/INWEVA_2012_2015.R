##INWEVA
library(readxl)

#2012
INWEVA2012_werkdag<- read_excel("data/INWEVA/INWEVA/INWEVA_2012/INWEVA 2012/A. Outputbestanden intensiteitsgegevens/1. Excel/definitief_INWEVA_2012_werkdag.xlsx",
                                sheet="INWEVA_2012_werkdag")

#split NwbId, zodat er een unieke rij bestaat voor iedere NwbId
s <- strsplit(INWEVA2012_werkdag$NwbId, split = ", ")
INWEVA_2012_werkdagen<- data.frame(Etmaal_AL=rep(INWEVA2012_werkdag$etmaal_AL, sapply(s, length)),
                                   Etmaal_L1=rep(INWEVA2012_werkdag$etmaal_L1, sapply(s, length)), 
                                   Etmaal_L2=rep(INWEVA2012_werkdag$etmaal_L2, sapply(s, length)), 
                                   Etmaal_L3=rep(INWEVA2012_werkdag$etmaal_L3, sapply(s, length)), 
                                   WVK_ID = unlist(s))

#2013
INWEVA2013_werkdag<- read_excel("data/INWEVA/INWEVA/INWEVA_2013/A. Outputbestanden/1. Excel/definitief01_INWEVA_2013_werkdag.xls",
                                sheet="INWEVA_2013_werkdag")

#split NwbId, zodat er een unieke rij bestaat voor iedere NwbId
s <- strsplit(INWEVA2013_werkdag$NwbId, split = ", ")
INWEVA_2013_werkdagen<- data.frame(Etmaal_AL=rep(INWEVA2013_werkdag$etmaal_AL, sapply(s, length)),
                                   Etmaal_L1=rep(INWEVA2013_werkdag$etmaal_L1, sapply(s, length)), 
                                   Etmaal_L2=rep(INWEVA2013_werkdag$etmaal_L2, sapply(s, length)), 
                                   Etmaal_L3=rep(INWEVA2013_werkdag$etmaal_L3, sapply(s, length)), 
                                   WVK_ID = unlist(s))




##2014
INWEVA2014_werkdag<- read_excel("data/INWEVA/INWEVA/INWEVA_2014/A. Outputbestanden intensiteitsgegevens/1. Excel/definitief_INWEVA_2014_werkdag.xlsx",
                                sheet="definitief_INWEVA_2014_werkdag")

#split NwbId, zodat er een unieke rij bestaat voor iedere NwbId
s <- strsplit(INWEVA2014_werkdag$NwbId, split = ", ")
INWEVA_2014_werkdagen<- data.frame(Etmaal_AL=rep(INWEVA2014_werkdag$etmaal_AL, sapply(s, length)),
                                   Etmaal_L1=rep(INWEVA2014_werkdag$etmaal_L1, sapply(s, length)), 
                                   Etmaal_L2=rep(INWEVA2014_werkdag$etmaal_L2, sapply(s, length)), 
                                   Etmaal_L3=rep(INWEVA2014_werkdag$etmaal_L3, sapply(s, length)), 
                                   WVK_ID = unlist(s))

##2015
INWEVA2015_werkdag<- read_excel("data/INWEVA/INWEVA/INWEVA_2015/A. Outputbestanden intensiteitsgegevens/1. Excel/definitief_INWEVA_2015_werkdag.xlsx",
                                sheet="def_INWEVA_2015_werkdag")

#split NwbId, zodat er een unieke rij bestaat voor iedere NwbId
s <- strsplit(INWEVA2015_werkdag$NwbId, split = ", ")
INWEVA_2015_werkdagen<- data.frame(Etmaal_AL=rep(INWEVA2015_werkdag$etmaal_AL, sapply(s, length)),
                                   Etmaal_L1=rep(INWEVA2015_werkdag$etmaal_L1, sapply(s, length)), 
                                   Etmaal_L2=rep(INWEVA2015_werkdag$etmaal_L2, sapply(s, length)), 
                                   Etmaal_L3=rep(INWEVA2015_werkdag$etmaal_L3, sapply(s, length)), 
                                   WVK_ID = unlist(s))

inweva_werkdagen_2012_2015<- list(INWEVA_2012_werkdagen,INWEVA_2013_werkdagen,INWEVA_2014_werkdagen,INWEVA_2015_werkdagen)

saveRDS(inweva_werkdagen_2012_2015,file="data/inweva_werkdagen_2012_2015.RDS")


r.int.tot$Inw2012_Werkdag_etmaal_al<- INWEVA_2012_werkdagen$Etmaal_AL[match(r.int.tot$WVK_ID,INWEVA_2012_werkdagen$WVK_ID)]
r.int.tot$Inw2012_Werkdag_etmaal_L1<- INWEVA_2012_werkdagen$Etmaal_L1[match(r.int.tot$WVK_ID,INWEVA_2012_werkdagen$WVK_ID)]
r.int.tot$Inw2012_Werkdag_etmaal_L2<- INWEVA_2012_werkdagen$Etmaal_L2[match(r.int.tot$WVK_ID,INWEVA_2012_werkdagen$WVK_ID)]
r.int.tot$Inw2012_Werkdag_etmaal_L3<- INWEVA_2012_werkdagen$Etmaal_L3[match(r.int.tot$WVK_ID,INWEVA_2012_werkdagen$WVK_ID)]

r.int.tot$Inw2013_Werkdag_etmaal_al<- INWEVA_2013_werkdagen$Etmaal_AL[match(r.int.tot$WVK_ID,INWEVA_2013_werkdagen$WVK_ID)]
r.int.tot$Inw2013_Werkdag_etmaal_L1<- INWEVA_2013_werkdagen$Etmaal_L1[match(r.int.tot$WVK_ID,INWEVA_2013_werkdagen$WVK_ID)]
r.int.tot$Inw2013_Werkdag_etmaal_L2<- INWEVA_2013_werkdagen$Etmaal_L2[match(r.int.tot$WVK_ID,INWEVA_2013_werkdagen$WVK_ID)]
r.int.tot$Inw2013_Werkdag_etmaal_L3<- INWEVA_2013_werkdagen$Etmaal_L3[match(r.int.tot$WVK_ID,INWEVA_2013_werkdagen$WVK_ID)]

r.int.tot$Inw2015_Werkdag_etmaal_al<- INWEVA_2015_werkdagen$Etmaal_AL[match(r.int.tot$WVK_ID,INWEVA_2015_werkdagen$WVK_ID)]
r.int.tot$Inw2015_Werkdag_etmaal_L1<- INWEVA_2015_werkdagen$Etmaal_L1[match(r.int.tot$WVK_ID,INWEVA_2015_werkdagen$WVK_ID)]
r.int.tot$Inw2015_Werkdag_etmaal_L2<- INWEVA_2015_werkdagen$Etmaal_L2[match(r.int.tot$WVK_ID,INWEVA_2015_werkdagen$WVK_ID)]
r.int.tot$Inw2015_Werkdag_etmaal_L3<- INWEVA_2015_werkdagen$Etmaal_L3[match(r.int.tot$WVK_ID,INWEVA_2015_werkdagen$WVK_ID)]

#save(r.int.tot,file="data/r_int_tot.RData")
