####Laad boogstralen#####

library(readxl)    
#filename<- "data/nis_langsdwarsonvlakheid/nis_langsonvlakheid_edit.xlsx"
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

verkantingen <- read_excel_allsheets("data/NIS_verkantingen/NIS_verkantingen.xlsx")
verkantingen[[1]]<- NULL
verkantingen<- lapply(verkantingen,function(x) x[3:nrow(x),1:11])

library(data.table)
verkantingen<- rbindlist(verkantingen)
verkantingen<-as.data.frame(verkantingen)
colnames(verkantingen)<- c("Rijksweg","Baan","Strook","Buitenste_rijkstrook","Hecto_begin","Afstand_van","Hecto_eind","Afstand_tot","Meetdatum","Meetsoort","Meetwaarde")




verkantingen$Meetdatum<- as.Date(as.numeric(verkantingen$Meetdatum),origin="1900-01-01")
verkantingen$Hecto_begin<- sub(",",".",verkantingen$Hecto_begin)
verkantingen$Hecto_eind<- sub(",",".",verkantingen$Hecto_eind)
verkantingen$Hecto_begin<- as.numeric(verkantingen$Hecto_begin)
verkantingen$Hecto_eind<- as.numeric(verkantingen$Hecto_eind)
verkantingen$Meetwaarde<- as.numeric(verkantingen$Meetwaarde)
verkantingen$Afstand_van<- as.numeric(verkantingen$Afstand_van)
verkantingen$Afstand_tot<- as.numeric(verkantingen$Afstand_tot)
#verkantingen$Normwaarde<- as.numeric(verkantingen$Normwaarde)
#verkantingen$Verschil<- as.numeric(verkantingen$Verschil)
verkantingen$Jaar<- format(verkantingen$Meetdatum, "%Y")


#convert wegnamen naar nummrs
verkantingen$wegnummer<- as.numeric(substr(verkantingen$Rijksweg,3,5))
verkantingen$baan_edit<- substr(verkantingen$Baan,1,6)
verkantingen$baan_edit<- gsub(" ","",verkantingen$baan_edit)
verkantingen$strook_edit<- gsub(" ","",verkantingen$Strook)

verkantingen$weg.baan.strook.van<- paste(verkantingen$wegnummer,verkantingen$baan_edit,verkantingen$strook_edit,verkantingen$Hecto_begin,sep=".")
#verkantingen$weg.baan.strook.van<- paste(verkantingen$wegnummer,verkantingen$baan_edit,verkantingen$strook_edit,verkantingen$Hecto_begin,sep=".")
verkantingen$weg.baan.van<- paste(verkantingen$wegnummer,verkantingen$baan_edit,verkantingen$Hecto_begin,sep=".")

verkantingen$baan_edit2<- ifelse(verkantingen$baan_edit %in% "0HRM","HR.M",ifelse(verkantingen$baan_edit %in% "1HRL","HR.L", 
                                                                                ifelse(verkantingen$baan_edit %in% "1HRR", "HR.R","VW")))

verkantingen_br<- verkantingen[which(verkantingen$Buitenste_rijkstrook=="ja"),]

#Aggregeren
verkantingen_agg<- aggregate(Meetwaarde ~ weg.baan.van +wegnummer + Hecto_begin +baan_edit2, verkantingen_br, FUN = mean)
verkantingen_agg$verkanting.max<- aggregate(Meetwaarde ~ weg.baan.van , verkantingen_br, FUN = max)[,2]
verkantingen_agg$verkanting.min<- aggregate(Meetwaarde ~ weg.baan.van , verkantingen_br, FUN = min)[,2]
colnames(verkantingen_agg)[5]<- "verkanting.mean"

verkantingen_agg$nwb.key<- paste(verkantingen_agg$wegnummer,(verkantingen_agg$Hecto_begin*10),verkantingen_agg$baan_edit2,sep=".")



load("data/raf_tot.RData")
###match with r.int.tot
raf_tot$verkanting.mean<- verkantingen_agg$verkanting.mean[match(raf_tot$WEG.BAAN.VAN,verkantingen_agg$weg.baan.van)]
raf_tot$verkanting.min<- verkantingen_agg$verkanting.min[match(raf_tot$WEG.BAAN.VAN,verkantingen_agg$weg.baan.van)]
raf_tot$verkanting.max<- verkantingen_agg$verkanting.max[match(raf_tot$WEG.BAAN.VAN,verkantingen_agg$weg.baan.van)]

#save(raf_tot,file="data/raf_tot.RData")

sum(!is.na(match(verkantingen_agg$weg.baan.van,raf_tot$WEG.BAAN.VAN)))
sum(which(verkantingen$WEG.BAAN.VAN %in% raf_tot$WEG.BAAN.VAN))
length(unique(verkantingen$weg.baan.van))





##hectopunten arjan
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

verkantingen_agg$nwb.key<- paste(verkantingen_agg$wegnummer,(verkantingen_agg$Hecto_begin*10),verkantingen_agg$baan_edit2,sep=".")



#match
hec2004$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2004$nwb.key,verkantingen_agg$nwb.key)]
hec2005$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2005$nwb.key,verkantingen_agg$nwb.key)]
hec2006$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2006$nwb.key,verkantingen_agg$nwb.key)]
hec2007$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2007$nwb.key,verkantingen_agg$nwb.key)]
hec2008$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2008$nwb.key,verkantingen_agg$nwb.key)]
hec2009$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2009$nwb.key,verkantingen_agg$nwb.key)]
hec2010$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2010$nwb.key,verkantingen_agg$nwb.key)]
hec2011$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2011$nwb.key,verkantingen_agg$nwb.key)]
hec2012$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2012$nwb.key,verkantingen_agg$nwb.key)]
hec2013$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2013$nwb.key,verkantingen_agg$nwb.key)]
hec2014$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2014$nwb.key,verkantingen_agg$nwb.key)]
hec2015$verkanting.mean<- verkantingen_agg$verkanting.mean[match(hec2015$nwb.key,verkantingen_agg$nwb.key)]

hec2004$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2004$nwb.key,verkantingen_agg$nwb.key)]
hec2005$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2005$nwb.key,verkantingen_agg$nwb.key)]
hec2006$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2006$nwb.key,verkantingen_agg$nwb.key)]
hec2007$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2007$nwb.key,verkantingen_agg$nwb.key)]
hec2008$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2008$nwb.key,verkantingen_agg$nwb.key)]
hec2009$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2009$nwb.key,verkantingen_agg$nwb.key)]
hec2010$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2010$nwb.key,verkantingen_agg$nwb.key)]
hec2011$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2011$nwb.key,verkantingen_agg$nwb.key)]
hec2012$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2012$nwb.key,verkantingen_agg$nwb.key)]
hec2013$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2013$nwb.key,verkantingen_agg$nwb.key)]
hec2014$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2014$nwb.key,verkantingen_agg$nwb.key)]
hec2015$verkanting.max<- verkantingen_agg$verkanting.max[match(hec2015$nwb.key,verkantingen_agg$nwb.key)]

hec2004$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2004$nwb.key,verkantingen_agg$nwb.key)]
hec2005$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2005$nwb.key,verkantingen_agg$nwb.key)]
hec2006$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2006$nwb.key,verkantingen_agg$nwb.key)]
hec2007$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2007$nwb.key,verkantingen_agg$nwb.key)]
hec2008$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2008$nwb.key,verkantingen_agg$nwb.key)]
hec2009$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2009$nwb.key,verkantingen_agg$nwb.key)]
hec2010$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2010$nwb.key,verkantingen_agg$nwb.key)]
hec2011$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2011$nwb.key,verkantingen_agg$nwb.key)]
hec2012$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2012$nwb.key,verkantingen_agg$nwb.key)]
hec2013$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2013$nwb.key,verkantingen_agg$nwb.key)]
hec2014$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2014$nwb.key,verkantingen_agg$nwb.key)]
hec2015$verkanting.min<- verkantingen_agg$verkanting.min[match(hec2015$nwb.key,verkantingen_agg$nwb.key)]


summary(hec2015$verkanting.max)

saveRDS(hec2004,file="boog_verk/hectopunten2004.RDS")
saveRDS(hec2005,file="boog_verk/hectopunten2005.RDS")
saveRDS(hec2006,file="boog_verk/hectopunten2006.RDS")
saveRDS(hec2007,file="boog_verk/hectopunten2007.RDS")
saveRDS(hec2008,file="boog_verk/hectopunten2008.RDS")
saveRDS(hec2009,file="boog_verk/hectopunten2009.RDS")
saveRDS(hec2010,file="boog_verk/hectopunten2010.RDS")
saveRDS(hec2011,file="boog_verk/hectopunten2011.RDS")
saveRDS(hec2012,file="boog_verk/hectopunten2012.RDS")
saveRDS(hec2013,file="boog_verk/hectopunten2013.RDS")
saveRDS(hec2014,file="boog_verk/hectopunten2014.RDS")
saveRDS(hec2015,file="boog_verk/hectopunten2015.RDS")


##testje
hec2015$verk.test<- raf_tot$verkanting.mean[match(hec2015$nwb.key,raf_tot$nwb.key)]

hec2015$test<- hec2015$verkanting.mean - hec2015$verk.test
