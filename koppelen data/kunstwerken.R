##laad kunstwerken

library(rgdal )
weggeg.kunstinweg<- readOGR("data/GIS data/WEGGEG_kunstinwegt.shp",layer="WEGGEG_kunstinwegt")
weggeg.kunstinweg<- as.data.frame(weggeg.kunstinweg)

r.int.tot$kunstwerk<- weggeg.kunstinweg$OMSCHR[match(r.int.tot$WVK_ID,weggeg.kunstinweg$WVK_ID)]

summary(r.int.tot$kunstwerk)

#save(r.int.tot,file="data/r_int_tot.RData")
