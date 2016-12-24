###Sample dataset

load("data/r_int_tot.RData")
r.int.tot$km<- round(r.int.tot$VAN,0)
r.int.tot$weg.baan.km.jaar<- paste(r.int.tot$WEG,r.int.tot$BAAN,r.int.tot$km,r.int.tot$jaar,sep=".")

#take sample of 1 for each 1km segment per year
sp <- split(r.int.tot, r.int.tot$weg.baan.km.jaar)

samples <- lapply(sp, function(x) x[sample(1:nrow(x), 1, FALSE),])
r.int.sample <- do.call(rbind, samples)
r.int.sample[,97:99]<-NULL

#save(r.int.sample,file="data/r_int_sample.RData")
