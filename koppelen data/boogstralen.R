####Laad boogstralen#####

library(readxl)    
#filename<- "data/nis_langsdwarsonvlakheid/nis_langsonvlakheid_edit.xlsx"
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

boogstralen <- read_excel_allsheets("data/NIS_boogstralen/nis_boogstralen.xlsx")
boogstralen<- lapply(boogstralen, function(x) x[3:nrow(x),1:11])

library(data.table)
boogstralen<- rbindlist(boogstralen)
boogstralen<-as.data.frame(boogstralen)

colnames(boogstralen)<- c("Rijksweg","Baan","Strook","Buitenste_rijkstrook","Hecto_begin","Afstand_van","Hecto_eind","Afstand_tot","Meetdatum","Meetsoort","Meetwaarde")
boogstralen$Meetdatum<- as.Date(as.numeric(boogstralen$Meetdatum),origin="1900-01-01")
boogstralen$Hecto_begin<- sub(",",".",boogstralen$Hecto_begin)
boogstralen$Hecto_eind<- sub(",",".",boogstralen$Hecto_eind)
boogstralen$Hecto_begin<- as.numeric(boogstralen$Hecto_begin)
boogstralen$Hecto_eind<- as.numeric(boogstralen$Hecto_eind)
boogstralen$Meetwaarde<- as.numeric(boogstralen$Meetwaarde)
boogstralen$Afstand_van<- as.numeric(boogstralen$Afstand_van)
boogstralen$Afstand_tot<- as.numeric(boogstralen$Afstand_tot)
#boogstralen$Normwaarde<- as.numeric(boogstralen$Normwaarde)
#boogstralen$Verschil<- as.numeric(boogstralen$Verschil)
boogstralen$Jaar<- format(boogstralen$Meetdatum, "%Y")


#convert wegnamen naar nummrs
boogstralen$wegnummer<- as.numeric(substr(boogstralen$Rijksweg,3,5))
boogstralen$baan_edit<- substr(boogstralen$Baan,1,6)
boogstralen$baan_edit<- gsub(" ","",boogstralen$baan_edit)
boogstralen$strook_edit<- gsub(" ","",boogstralen$Strook)

boogstralen$weg.baan.strook.van<- paste(boogstralen$wegnummer,boogstralen$baan_edit,boogstralen$strook_edit,boogstralen$Hecto_begin,sep=".")
boogstralen$weg.baan.strook.van<- paste(boogstralen$wegnummer,boogstralen$baan_edit,boogstralen$strook_edit,boogstralen$Hecto_begin,sep=".")
boogstralen$weg.baan.van<- paste(boogstralen$wegnummer,boogstralen$baan_edit,boogstralen$Hecto_begin,sep=".")

boogstralen$baan_edit2<- ifelse(boogstralen$baan_edit %in% "0HRM","HR.M",ifelse(boogstralen$baan_edit %in% "1HRL","HR.L", 
                                                                                ifelse(boogstralen$baan_edit %in% "1HRR", "HR.R","VW")))

boogstralen_br<- boogstralen[which(boogstralen$Buitenste_rijkstrook=="ja"),]

#Aggregeren
boogstralen_agg<- aggregate(Meetwaarde ~ weg.baan.van +wegnummer + Hecto_begin +baan_edit2, boogstralen_br, FUN = mean)
boogstralen_agg$boogstraal.max<- aggregate(Meetwaarde ~ weg.baan.van , boogstralen_br, FUN = max)[,2]
boogstralen_agg$boogstraal.min<- aggregate(Meetwaarde ~ weg.baan.van , boogstralen_br, FUN = min)[,2]
colnames(boogstralen_agg)[5]<- "boogstraal.mean"

# boogstralen_agg<- aggregate(Meetwaarde ~ weg.baan.strook.van, boogstralen, FUN = mean)
# boogstralen_agg$boogstraal.max<- aggregate(Meetwaarde ~ weg.baan.strook.van, boogstralen, FUN = max)[,2]
# boogstralen_agg$boogstraal.min<- aggregate(Meetwaarde ~ weg.baan.strook.van, boogstralen, FUN = min)[,2]
# colnames(boogstralen_agg)[2]<- "boogstraal.mean" 



###match with r.int.tot
raf_tot$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(raf_tot$WEG.BAAN.VAN,boogstralen_agg$weg.baan.van)]
raf_tot$boogstraal.min<- boogstralen_agg$boogstraal.min[match(raf_tot$WEG.BAAN.VAN,boogstralen_agg$weg.baan.van)]
raf_tot$boogstraal.max<- boogstralen_agg$boogstraal.max[match(raf_tot$WEG.BAAN.VAN,boogstralen_agg$weg.baan.van)]

load("data/raf_tot.RData")
#save(raf_tot,file="data/raf_tot.RData")



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

boogstralen_agg$nwb.key<- paste(boogstralen_agg$wegnummer,(boogstralen_agg$Hecto_begin*10),boogstralen_agg$baan_edit2,sep=".")



#match
hec2004$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2004$nwb.key,boogstralen_agg$nwb.key)]
hec2005$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2005$nwb.key,boogstralen_agg$nwb.key)]
hec2006$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2006$nwb.key,boogstralen_agg$nwb.key)]
hec2007$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2007$nwb.key,boogstralen_agg$nwb.key)]
hec2008$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2008$nwb.key,boogstralen_agg$nwb.key)]
hec2009$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2009$nwb.key,boogstralen_agg$nwb.key)]
hec2010$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2010$nwb.key,boogstralen_agg$nwb.key)]
hec2011$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2011$nwb.key,boogstralen_agg$nwb.key)]
hec2012$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2012$nwb.key,boogstralen_agg$nwb.key)]
hec2013$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2013$nwb.key,boogstralen_agg$nwb.key)]
hec2014$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2014$nwb.key,boogstralen_agg$nwb.key)]
hec2015$boogstraal.mean<- boogstralen_agg$boogstraal.mean[match(hec2015$nwb.key,boogstralen_agg$nwb.key)]

hec2004$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2004$nwb.key,boogstralen_agg$nwb.key)]
hec2005$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2005$nwb.key,boogstralen_agg$nwb.key)]
hec2006$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2006$nwb.key,boogstralen_agg$nwb.key)]
hec2007$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2007$nwb.key,boogstralen_agg$nwb.key)]
hec2008$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2008$nwb.key,boogstralen_agg$nwb.key)]
hec2009$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2009$nwb.key,boogstralen_agg$nwb.key)]
hec2010$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2010$nwb.key,boogstralen_agg$nwb.key)]
hec2011$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2011$nwb.key,boogstralen_agg$nwb.key)]
hec2012$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2012$nwb.key,boogstralen_agg$nwb.key)]
hec2013$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2013$nwb.key,boogstralen_agg$nwb.key)]
hec2014$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2014$nwb.key,boogstralen_agg$nwb.key)]
hec2015$boogstraal.max<- boogstralen_agg$boogstraal.max[match(hec2015$nwb.key,boogstralen_agg$nwb.key)]

hec2004$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2004$nwb.key,boogstralen_agg$nwb.key)]
hec2005$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2005$nwb.key,boogstralen_agg$nwb.key)]
hec2006$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2006$nwb.key,boogstralen_agg$nwb.key)]
hec2007$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2007$nwb.key,boogstralen_agg$nwb.key)]
hec2008$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2008$nwb.key,boogstralen_agg$nwb.key)]
hec2009$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2009$nwb.key,boogstralen_agg$nwb.key)]
hec2010$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2010$nwb.key,boogstralen_agg$nwb.key)]
hec2011$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2011$nwb.key,boogstralen_agg$nwb.key)]
hec2012$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2012$nwb.key,boogstralen_agg$nwb.key)]
hec2013$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2013$nwb.key,boogstralen_agg$nwb.key)]
hec2014$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2014$nwb.key,boogstralen_agg$nwb.key)]
hec2015$boogstraal.min<- boogstralen_agg$boogstraal.min[match(hec2015$nwb.key,boogstralen_agg$nwb.key)]


library(leaflet)
library(rgdal)
hec2015<- as.data.frame(hec2015)
#hec_s<- hec2015[sample(nrow(hec2015),0.05*nrow(hec2015)),]
hec_s<- hec2015[which(hec2015$boogstraal.mean <=2000&hec2015$boogstraal.mean >= -2000),]

coordinates(hec_s)<- ~coords.x1+coords.x2
#qpal <- colorQuantile("Reds", wegvakken.file$Filezwaarte, n = 7)
pal <- colorNumeric(
  palette = "YlOrRd",
  hec_s$boogstraal.mean
)

m <- leaflet(hec_s) %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles("Stamen.Toner") %>%
  addCircles(color = ~pal(boogstraal.mean),weight=1,radius=10,popup= ~paste("waarde:", boogstraal.mean, "<br>",
                                                                            "nwb: ",nwb.key,"<br>")) %>%
  addLegend("bottomright", pal = pal, values = ~boogstraal.mean,
            title = "Boogstraal",
            opacity = 1
  )
m


