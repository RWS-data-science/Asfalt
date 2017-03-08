#GMS

#load("C:/Users/datalab1/Documents/R/Asfalt/data/gms_measurement_features_1501_20161106_781593.RData")

load("data/gms_measurement_features_1501_20161106_781593.RData")


gms_measurement_features<- gms_measurement_features[order(gms_measurement_features$date),]
gms_measurement_features<- as.data.frame(gms_measurement_features)
gms_measurement_features$freeze_count_1<- as.numeric(gms_measurement_features$freeze_count_1)
gms_measurement_features$freeze_count_2<- as.numeric(gms_measurement_features$freeze_count_2)
gms_measurement_features$freeze_count_3<- as.numeric(gms_measurement_features$freeze_count_3)
gms_measurement_features$melt_minutes<- as.numeric(gms_measurement_features$melt_minutes)
gms_measurement_features$freeze_pattern_count<- as.numeric(gms_measurement_features$freeze_pattern_count)

gms_agg<- aggregate(cbind(freeze_count_1, freeze_count_2, freeze_count_3, melt_minutes,freeze_pattern_count) ~  period+ loc_nr,
                data=gms_measurement_features,FUN='sum')

gms_loc<-read.csv("data/GMSstations.csv")

gms_agg$lat<- gms_loc$loc_lat[match(gms_agg$loc_nr,gms_loc$loc_nr)]
gms_agg$lon<- gms_loc$loc_lon[match(gms_agg$loc_nr,gms_loc$loc_nr)]
gms_agg$name<- gms_loc$loc_nm[match(gms_agg$loc_nr,gms_loc$loc_nr)]

gms_agg<- gms_agg[complete.cases(gms_agg),]



load("data/gms_locations_measurement_features_1582.RData")
gms_agg2<- gms_locations_measurement_features
gms_agg2$period<- as.numeric(gms_agg2$period)

library(rgdal)
rd<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs" 

coordinates(gms_agg2)<- ~loc_lon+loc_lat
proj4string(gms_agg2)<- rd
#wgs<- "+proj=longlat +ellps=WGS84 +datum=WGS84"
#proj4string(gms_agg)<- rd
#gms_agg2<-spTransform(gms_agg2,wgs)
#aws_loc<-as.data.frame(aws_loc)
gms_agg_df<- as.data.frame(gms_agg2)



load("data/raf_tot.RData")

#interpolatie nn
library(gstat)
#library(sp)
r.int.narm<- raf_tot[which(!is.na(raf_tot$x)),]


jaren<- unique(r.int.narm$jaar)
#dates<- dates[which(!format(dates,"%m-%d") %in% c("12-31","11-30","01-01") )] #deze data worden gebruikt als de exacte datum niet bekend is

list<- list()
for (i in jaren){
  if (i>2010){
  r.x<- r.int.narm[which(r.int.narm$jaar == i),]
  a.x<- gms_agg2[which(gms_agg2$period == i),]
  

    coordinates(r.x)<- ~ x+y
    proj4string(r.x)<- rd
    #coordinates(a.x)<- ~x+y
    
    r.x$freeze_count_1 = krige(freeze_count_1  ~ 1, a.x, r.x, nmax = 1)$var1.pred #delen door 10 ivm eenheid van KNMI data (0.1)
    r.x$freeze_count_2 = krige(freeze_count_2  ~ 1, a.x, r.x, nmax = 1)$var1.pred
    r.x$freeze_count_3 = krige(freeze_count_3  ~ 1, a.x, r.x, nmax = 1)$var1.pred
    r.x$melt_minutes = krige(melt_minutes  ~ 1, a.x, r.x, nmax = 1)$var1.pred
    r.x$freeze_pattern_count = krige(freeze_pattern_count  ~ 1, a.x, r.x, nmax = 1)$var1.pred

    r.x<- as.data.frame(r.x)  
    
    list[[i]]<- r.x
  }
  #cat(i/length(dates))
}

library(data.table)
raf_gms<- rbindlist(list)


raf_tot$freeze_count_1<- raf_gms$freeze_count_1[match(raf_tot$ID,raf_gms$ID)]
raf_tot$freeze_count_2<- raf_gms$freeze_count_2[match(raf_tot$ID,raf_gms$ID)]
raf_tot$freeze_count_3<- raf_gms$freeze_count_3[match(raf_tot$ID,raf_gms$ID)]
raf_tot$melt_minutes<- raf_gms$melt_minutes[match(raf_tot$ID,raf_gms$ID)]
raf_tot$freeze_pattern_count<- raf_gms$freeze_pattern_count[match(raf_tot$ID,raf_gms$ID)]



save(raf_tot,file="data/raf_tot.RData")
