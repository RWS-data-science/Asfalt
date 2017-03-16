library(data.table)
prepare<- function(r){

  #r<-r[r$WEG != 99,]
  r.samp<- r
  r.samp<- r.samp[which(r.samp$DEKLAAGSOORT %in% c("ZOAB","ZOAB+","ZOABTW")),]
  
  r.samp[sapply(r.samp, is.character)] <- lapply(r.samp[sapply(r.samp, is.character)], 
                                         as.factor)
  
  overbodig<- c("WEGNUMMER","VOLGNUMMER_BAAN","HECTOMETRERINGSLETTER","VAN","TOT","AANLEGDATUM","WEG","WEG.BAAN.STROOK.VAN",
                "WEG.BAAN.VAN","vervangen","INTERVENTIEJAAR_RAF","vervangen.leeftijd","verdacht","vvr","ldr.verw","verschil_planjaar",
                "jr.tot.interventie","ldr.verw","verschil_planjaar","eerste.schade","eerste.schade.leeftijd","WVK_ID","WVK_ID_jaar",
                "ID","WEG.BAAN.VAN.JAAR","weg.baan.km.jaar","nwb.key","WEG.BAAN.STROOK.VAN.JAAR","afst.obst","BAANPOSITIE","deklaagsoort.lag")

  
  r.samp<- r.samp[,!(colnames(r.samp) %in% overbodig)]
  
  
  na_count <-sapply(r.samp, function(y) sum(is.na(y))/length(y))
  compl_columns<- names(na_count[na_count<0.4]) #only columns with >60% availability
  

  r.samp<- r.samp[,colnames(r.samp) %in% c(compl_columns,"verloop")]
  
  r.samp$schade<- as.factor(r.samp$schade)
  
  return(r.samp)
}