###Sample dataset
library(data.table)
load("data/raf_tot.RData")
raf_tot$km<- round(raf_tot$VAN,0)
raf_tot$weg.baan.km.jaar<- paste(raf_tot$WEG,raf_tot$BAAN,raf_tot$km,raf_tot$jaar,sep=".")

#take sample of 1 for each 1km segment per year
sp <- split(raf_tot$ID, raf_tot$weg.baan.km.jaar)

samples <- lapply(sp, function(x) x[sample(1:length(x), 1, FALSE)])

raf_sample<- raf_tot[which(raf_tot$ID %in% samples),]
raf_sample$weg.baan.km.jaar<-NULL
raf_sample$km<-NULL

save(raf_sample,file="data/r_int_sample_mar17.RData")

