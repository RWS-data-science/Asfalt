load(file= "data/input/r_int_tot.RData")

r.int.tot<-r.int.tot[r.int.tot$WEG != 99,]

#OVERBODIG
#r.int.tot$WEG = NULL
#r.int.tot$BAAN = NULL
#r.int.tot$STROOK = NULL
#r.int.tot$VAN = NULL
r.int.tot$TOT= NULL
r.int.tot$AANLEGDATUM = NULL
r.int.tot$BUITENSTE_RIJSTROOK = NULL
r.int.tot$WEG.VAN.BAAN = NULL
r.int.tot$WEG.VAN.BAAN.JAAR = NULL
r.int.tot$WEG.VAN.BAAN = NULL
r.int.tot$WEG.BAAN.STROOK.VAN = NULL
r.int.tot$WEG.BAAN.STROOK.VAN.JAAR = NULL

#INDENTIFICEREND (Voor jr.interventie.raf)
r.int.tot$INTERVENTIEJAAR_RAF = NULL
r.int.tot$vervangen = NULL
r.int.tot$vervangen.leeftijd = NULL
r.int.tot$verdacht = NULL
r.int.tot$vvr = NULL
r.int.tot$ldr.verw = NULL
r.int.tot$verschil_planjaar = NULL
r.int.tot$jr.tot.interventie = NULL
r.int.tot$eerste.schade = NULL
r.int.tot$eerste.schade.leeftijd = NULL
#Mogelijk identificerend
r.int.tot$MPD_links2015 = NULL
r.int.tot$MPD_rechts2015 = NULL

# Many NA's weak prediction
r.int.tot$WVK_ID = NULL
r.int.tot$afst.obst = NULL

# TOO Many NA's
r.int.tot$aanleg.FG  = NULL
r.int.tot$aanleg.FHX = NULL
r.int.tot$aanleg.TG  = NULL
r.int.tot$aanleg.TX  = NULL
r.int.tot$aanleg.TN  = NULL
r.int.tot$aanleg.RH  = NULL

r.int.tot$MPD_midden2015  = NULL
r.int.tot$stroefheid2015  = NULL
r.int.tot$langsonvlakheid2015  = NULL
r.int.tot$dwarsonvlakheid2015  = NULL

r.int.tot$boogstraal.mean  = NULL
r.int.tot$boogstraal.min  = NULL
r.int.tot$boogstraal.max  = NULL

r.int.tot$asdruk1_mean  = NULL
r.int.tot$asdruk1_P_50  = NULL
r.int.tot$asdruk1_P_84  = NULL
r.int.tot$asdruk2_mean = NULL
r.int.tot$asdruk2_P_50  = NULL
r.int.tot$asdruk2_P_84  = NULL

#r.int.tot$kunstwerk = NULL

# HERE we make two different sets to train on
# ONE set for verloop and one set for jr.tot.interventie 
r.int.verloop = r.int.tot
r.int.schade = r.int.tot
r.int.schade$verloop = NULL
r.int.verloop$schade = NULL

r.int.verloop = r.int.verloop[complete.cases(r.int.verloop),]
r.int.schade = r.int.schade[complete.cases(r.int.schade),]

#now we make resampled versions of the two
#we remove all hectometers in the same kilometer, except one
#to reduce spacial dependence
r.int.verloop$km<- round(r.int.verloop$VAN,0)
r.int.verloop$weg.baan.km.jaar<-paste(r.int.verloop$WEG,r.int.verloop$BAAN,r.int.verloop$km,r.int.verloop$jaar,sep=".")
#take sample of 1 for each 1km segment per year
sp <- split(r.int.verloop, r.int.verloop$weg.baan.km.jaar)
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 1, FALSE),])
r.int.verloop.sampled <- do.call(rbind, samples)

r.int.schade$km<- round(r.int.schade$VAN,0)
r.int.schade$weg.baan.km.jaar<-paste(r.int.schade$WEG,r.int.schade$BAAN,r.int.schade$km,r.int.schade$jaar,sep=".")
#take sample of 1 for each 1km segment per year
sp <- split(r.int.schade, r.int.schade$weg.baan.km.jaar)
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 1, FALSE),])
r.int.schade.sampled <- do.call(rbind, samples)

rm(samples)
rm(sp)
r.int.schade.sampled$VAN <- r.int.schade.sampled$km <- r.int.schade.sampled$weg.baan.km.jaar <- NULL
r.int.schade$VAN <- r.int.schade$km <- r.int.schade$weg.baan.km.jaar <- NULL
r.int.verloop.sampled$VAN <- r.int.verloop.sampled$km <- r.int.verloop.sampled$weg.baan.km.jaar <- NULL
r.int.verloop$VAN <- r.int.verloop$km <- r.int.verloop$weg.baan.km.jaar <- NULL


for(year in c(2011, 2012, 2013, 2014, 2015)){
  #prepare train/testset
  cur.verloop.test = r.int.verloop[r.int.verloop$jaar == year,]
  cur.verloop.train = r.int.verloop[! r.int.verloop$jaar == year,]
  cur.schade.test = r.int.schade[r.int.schade$jaar == year,]
  cur.schade.train = r.int.schade[! r.int.schade$jaar == year,]
  
  cur.verloop.sampled.test = r.int.verloop.sampled[r.int.verloop.sampled$jaar == year,]
  cur.verloop.sampled.train = r.int.verloop.sampled[! r.int.verloop.sampled$jaar == year,]
  cur.schade.sampled.test = r.int.schade.sampled[r.int.schade.sampled$jaar == year,]
  cur.schade.sampled.train = r.int.schade.sampled[! r.int.schade.sampled$jaar == year,]
  
  write.csv(cur.verloop.test, file=paste("data/output/learning/csv/ver.test.h", year, ".csv",sep=""))
  write.csv(cur.verloop.train,file=paste("data/output/learning/csv/ver.train.h", year, ".csv",sep=""))
  write.csv(cur.schade.test, file=paste("data/output/learning/csv/sch.test.h", year, ".csv",sep=""))
  write.csv(cur.schade.train, file=paste("data/output/learning/csv/sch.train.h", year, ".csv",sep=""))
  write.csv(cur.verloop.sampled.test, file=paste("data/output/learning/csv/ver.test.h", year, ".resampled.csv",sep=""))
  write.csv(cur.verloop.sampled.train, file=paste("data/output/learning/csv/ver.train.h", year, ".resampled.csv",sep=""))
  write.csv(cur.schade.sampled.test, file=paste("data/output/learning/csv/sch.test.h", year, ".resampled.csv",sep=""))
  write.csv(cur.schade.sampled.train, file=paste("data/output/learning/csv/sch.train.h", year, ".resampled.csv",sep=""))
  
  save(cur.verloop.test, file=paste("data/output/learning/rdata/ver.test.h", year, ".RData",sep=""))
  save(cur.verloop.train,file=paste("data/output/learning/rdata/ver.train.h", year, ".RData",sep=""))
  save(cur.schade.test, file=paste("data/output/learning/rdata/sch.test.h", year, ".RData",sep=""))
  save(cur.schade.train, file=paste("data/output/learning/rdata/sch.train.h", year, ".RData",sep=""))
  save(cur.verloop.sampled.test, file=paste("data/output/learning/rdata/ver.test.h", year, ".resampled.RData",sep=""))
  save(cur.verloop.sampled.train, file=paste("data/output/learning/rdata/ver.train.h", year, ".resampled.RData",sep=""))
  save(cur.schade.sampled.test, file=paste("data/output/learning/rdata/sch.test.h", year, ".resampled.RData",sep=""))
  save(cur.schade.sampled.train, file=paste("data/output/learning/rdata/sch.train.h", year, ".resampled.RData",sep=""))
}

rm(cur.verloop.test,cur.verloop.train,cur.schade.test,cur.schade.train,cur.verloop.sampled.test,cur.verloop.sampled.train,cur.schade.sampled.test,cur.schade.sampled.train, year)