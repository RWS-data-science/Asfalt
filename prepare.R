prepare<- function(r){
  r<-r[r$WEG != 99,]
  r.samp<- r
  
  #OVERBODIG
  #r.samp$WEG = NULL
  r.samp$BAAN = NULL
  #r.samp$STROOK = NULL
  r.samp$VAN = NULL
  r.samp$TOT= NULL
  r.samp$AANLEGDATUM = NULL
  r.samp$BUITENSTE_RIJSTROOK = NULL
  r.samp$WEG.VAN.BAAN = NULL
  r.samp$WEG.VAN.BAAN.JAAR = NULL
  r.samp$WEG.VAN.BAAN = NULL
  r.samp$WEG.BAAN.STROOK.VAN = NULL
  r.samp$WEG.BAAN.STROOK.VAN.JAAR = NULL
  
  #INDENTIFICEREND (Voor jr.interventie.raf)
  r.samp$INTERVENTIEJAAR_RAF = NULL
  r.samp$vervangen = NULL
  r.samp$vervangen.leeftijd = NULL
  r.samp$verdacht = NULL
  r.samp$vvr = NULL
  r.samp$ldr.verw = NULL
  r.samp$verschil_planjaar = NULL
  r.samp$jr.tot.interventie = NULL
  r.samp$eerste.schade = NULL
  r.samp$eerste.schade.leeftijd = NULL
  #Mogelijk identificerend
  r.samp$MPD_links2015 = NULL
  r.samp$MPD_rechts2015 = NULL
  
  # Many NA's weak prediction
  r.samp$WVK_ID = NULL
  r.samp$afst.obst = NULL
  
  # TOO Many NA's
  r.samp$aanleg.FG  = NULL
  r.samp$aanleg.FHX = NULL
  r.samp$aanleg.TG  = NULL
  r.samp$aanleg.TX  = NULL
  r.samp$aanleg.TN  = NULL
  r.samp$aanleg.RH  = NULL
  
  r.samp$MPD_midden2015  = NULL
  r.samp$stroefheid2015  = NULL
  r.samp$langsonvlakheid2015  = NULL
  r.samp$dwarsonvlakheid2015  = NULL
  
  r.samp$boogstraal.mean  = NULL
  r.samp$boogstraal.min  = NULL
  r.samp$boogstraal.max  = NULL
  
  r.samp$asdruk1_mean  = NULL
  r.samp$asdruk1_P_50  = NULL
  r.samp$asdruk1_P_84  = NULL
  r.samp$asdruk2_mean = NULL
  r.samp$asdruk2_P_50  = NULL
  r.samp$asdruk2_P_84  = NULL
  
  r.samp$kunstwerk = NULL
  r.samp$verloop = NULL
  
  # HERE we make two different sets to train on
  # ONE set for verloop and one set for jr.tot.interventie 
  r.int.verloop = r.samp
  r.int.schade = r.samp
  r.int.schade$verloop = NULL
  r.int.verloop$jr.tot.interventie = NULL
  
  r.int.verloop = r.int.verloop[complete.cases(r.int.verloop),]
  r.int.schade = r.int.schade[complete.cases(r.int.schade),]
  
  #prepare train/testset
  r.int.verloop.test = r.int.verloop[r.samp$jaar == 2015,]
  r.int.verloop.train = r.int.verloop[! r.samp$jaar == 2015,]
  r.int.schade.test = r.int.schade[r.samp$jaar == 2015,]
  r.int.schade.train = r.int.schade[! r.samp$jaar == 2015,]
  
  
  
  # #write them to file
  # write.csv(r.int.verloop.test, file="r.int.verloop.test.csv")
  # write.csv(r.int.verloop.train, file="r.int.verloop.train.csv")
  # write.csv(r.int.schade.test, file="r.int.schade.test.csv")
  # write.csv(r.int.schade.train, file="r.int.schade.train.csv")
  
  return(r.samp)
}