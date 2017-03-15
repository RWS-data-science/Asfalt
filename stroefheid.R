#stroefheid all

str2016<- read.table("data/export2016.dsv",sep=";",header=T)

str2016$MTG_METDAT<- as.Date(str2016$MTG_METDAT,format="%d-%B-%y")


str2011<- read.table("data/export2011.dsv",sep=";",header=T)

str2011$MTG_METDAT<- as.Date(str2011$MTG_METDAT,format="%d-%B-%y")


library(plyr)
strtot<- rbind.fill(str2011,str2016)

length(unique(strtot$MRT_ID))
summary(strtot$MTG_METDAT)
strtot<- strtot[!duplicated(strtot$MRT_ID),]

strtot$jaar<- as.numeric(format(strtot$MTG_METDAT,"%Y"))
table(strtot$jaar)
summary(strtot$MRT_WAARDE)

strtot$baan<- sub("NA","",paste0(strtot$VOLGNRBAN,strtot$SBN_CD,strtot$POSBAN))
strtot$strook<- sub("NA","",paste0(strtot$VOLGNRSTK1,strtot$SSK_CD1,strtot$POSSTK1))

strtot$WEG.BAAN.STROOK.VAN.JAAR<- sub('NA$', '',paste(strtot$WEG_NR,strtot$baan,strtot$strook,strtot$MRT_HECTVAN,strtot$jaar,sep="."))
length(unique(strtot$WEG.BAAN.STROOK.VAN.JAAR))

strtot$WEG.BAAN.VAN.JAAR<- sub('NA$', '',paste(strtot$WEG_NR,strtot$baan,strtot$MRT_HECTVAN,strtot$jaar,sep="."))
length(unique(strtot$WEG.BAAN.STROOK.VAN.JAAR))


strtot$nwb.key<- paste(strtot$WEG_NR,(strtot$MRT_HECTVAN)*10,strtot$SBN_CD,strtot$POSBAN,sep=".")
strtot$nwb.key.jaar<- paste(strtot$WEG_NR,(strtot$MRT_HECTVAN)*10,strtot$SBN_CD,strtot$POSBAN,strtot$jaar,sep=".")


#######
raf_tot$stroefheid<- strtot$MRT_WAARDE[match(raf_tot$WEG.BAAN.STROOK.VAN.JAAR,strtot$WEG.BAAN.STROOK.VAN.JAAR)]
summary(raf_tot$stroefheid)

#save(raf_tot,file="data/raf_tot.RData")




######koppel met hectopunten arjan

hec2004<- readRDS("data/hectopunten_2004.rds")
hec2005<- readRDS("data/hectopunten_2005.rds")
hec2006<- readRDS("data/hectopunten_2006.rds")
hec2007<- readRDS("data/hectopunten_2007.rds")
hec2008<- readRDS("data/hectopunten_2008.rds")
hec2009<- readRDS("data/hectopunten_2009.rds")
hec2010<- readRDS("data/hectopunten_2010.rds")
hec2011<- readRDS("data/hectopunten_2011.rds")
hec2012<- readRDS("data/hectopunten_2012.rds")
hec2013<- readRDS("data/hectopunten_2013.rds")
hec2014<- readRDS("data/hectopunten_2014.rds")
hec2015<- readRDS("data/hectopunten_2015.rds")


#aggregate
stragg_mean<- aggregate(MRT_WAARDE~ WEG.BAAN.VAN.JAAR +nwb.key.jaar ,strtot,FUN=mean)
stragg_max<- aggregate(MRT_WAARDE~WEG.BAAN.VAN.JAAR ,strtot,FUN=max)
stragg_min<- aggregate(MRT_WAARDE~WEG.BAAN.VAN.JAAR,strtot,FUN=min)
stragg<- stragg_mean; colnames(stragg)<- c("WEG.BAAN.VAN.JAAR","nwb.key.jaar","str.mean")
stragg$str.max<- stragg_max$MRT_WAARDE[match(stragg$WEG.BAAN.VAN.JAAR,stragg_max$WEG.BAAN.VAN.JAAR)]
stragg$str.min<- stragg_min$MRT_WAARDE[match(stragg$WEG.BAAN.VAN.JAAR,stragg_max$WEG.BAAN.VAN.JAAR)]
rm(stragg_max,stragg_mean,stragg_min)

# load("data/r_int_tot.RData")
# 
# r.int.tot$str.mean<- stragg$str.mean[match(r.int.tot$WEG.VAN.BAAN.JAAR,stragg$WEG.VAN.BAAN.JAAR)]
# r.int.tot$str.max<- stragg$str.max[match(r.int.tot$WEG.VAN.BAAN.JAAR,stragg$WEG.VAN.BAAN.JAAR)]
# r.int.tot$str.min<- stragg$str.min[match(r.int.tot$WEG.VAN.BAAN.JAAR,stragg$WEG.VAN.BAAN.JAAR)]
# #save(r.int.tot,file="data/r_int_tot.RData")
# 
# stroefheid<- r.int.tot[,c("WVK_ID","VAN","jaar","WEG.VAN.BAAN.JAAR","str.mean","str.max","str.min")]
# #save(stroefheid,file="data/stroefheid.RDS")

hec2004$nwb.key.jaar<- paste(hec2004$nwb.key,2004,sep=".")
hec2005$nwb.key.jaar<- paste(hec2005$nwb.key,2005,sep=".")
hec2006$nwb.key.jaar<- paste(hec2006$nwb.key,2006,sep=".")
hec2007$nwb.key.jaar<- paste(hec2007$nwb.key,2007,sep=".")
hec2008$nwb.key.jaar<- paste(hec2008$nwb.key,2008,sep=".")
hec2009$nwb.key.jaar<- paste(hec2009$nwb.key,2009,sep=".")
hec2010$nwb.key.jaar<- paste(hec2010$nwb.key,2010,sep=".")
hec2011$nwb.key.jaar<- paste(hec2011$nwb.key,2011,sep=".")
hec2012$nwb.key.jaar<- paste(hec2012$nwb.key,2012,sep=".")
hec2013$nwb.key.jaar<- paste(hec2013$nwb.key,2013,sep=".")
hec2014$nwb.key.jaar<- paste(hec2014$nwb.key,2014,sep=".")
hec2015$nwb.key.jaar<- paste(hec2015$nwb.key,2015,sep=".")


hec2004$str.mean<- stragg$str.mean[match(hec2004$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2005$str.mean<- stragg$str.mean[match(hec2005$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2006$str.mean<- stragg$str.mean[match(hec2006$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2007$str.mean<- stragg$str.mean[match(hec2007$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2008$str.mean<- stragg$str.mean[match(hec2008$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2009$str.mean<- stragg$str.mean[match(hec2009$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2010$str.mean<- stragg$str.mean[match(hec2010$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2011$str.mean<- stragg$str.mean[match(hec2011$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2012$str.mean<- stragg$str.mean[match(hec2012$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2013$str.mean<- stragg$str.mean[match(hec2013$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2014$str.mean<- stragg$str.mean[match(hec2014$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2015$str.mean<- stragg$str.mean[match(hec2015$nwb.key.jaar,stragg$nwb.key.jaar)]

hec2004$str.max<- stragg$str.max[match(hec2004$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2005$str.max<- stragg$str.max[match(hec2005$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2006$str.max<- stragg$str.max[match(hec2006$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2007$str.max<- stragg$str.max[match(hec2007$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2008$str.max<- stragg$str.max[match(hec2008$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2009$str.max<- stragg$str.max[match(hec2009$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2010$str.max<- stragg$str.max[match(hec2010$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2011$str.max<- stragg$str.max[match(hec2011$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2012$str.max<- stragg$str.max[match(hec2012$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2013$str.max<- stragg$str.max[match(hec2013$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2014$str.max<- stragg$str.max[match(hec2014$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2015$str.max<- stragg$str.max[match(hec2015$nwb.key.jaar,stragg$nwb.key.jaar)]

hec2004$str.min<- stragg$str.min[match(hec2004$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2005$str.min<- stragg$str.min[match(hec2005$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2006$str.min<- stragg$str.min[match(hec2006$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2007$str.min<- stragg$str.min[match(hec2007$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2008$str.min<- stragg$str.min[match(hec2008$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2009$str.min<- stragg$str.min[match(hec2009$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2010$str.min<- stragg$str.min[match(hec2010$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2011$str.min<- stragg$str.min[match(hec2011$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2012$str.min<- stragg$str.min[match(hec2012$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2013$str.min<- stragg$str.min[match(hec2013$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2014$str.min<- stragg$str.min[match(hec2014$nwb.key.jaar,stragg$nwb.key.jaar)]
hec2015$str.min<- stragg$str.min[match(hec2015$nwb.key.jaar,stragg$nwb.key.jaar)]

saveRDS(hec2004,file="stroefheid/hectopunten2004.RDS")
saveRDS(hec2005,file="stroefheid/hectopunten2005.RDS")
saveRDS(hec2006,file="stroefheid/hectopunten2006.RDS")
saveRDS(hec2007,file="stroefheid/hectopunten2007.RDS")
saveRDS(hec2008,file="stroefheid/hectopunten2008.RDS")
saveRDS(hec2009,file="stroefheid/hectopunten2009.RDS")
saveRDS(hec2010,file="stroefheid/hectopunten2010.RDS")
saveRDS(hec2011,file="stroefheid/hectopunten2011.RDS")
saveRDS(hec2012,file="stroefheid/hectopunten2012.RDS")
saveRDS(hec2013,file="stroefheid/hectopunten2013.RDS")
saveRDS(hec2014,file="stroefheid/hectopunten2014.RDS")
saveRDS(hec2015,file="stroefheid/hectopunten2015.RDS")
