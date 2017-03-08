#verloop
library(Hmisc)
load("data/r_int_tot.RData")

r.int.tot<- r.int.tot[order(r.int.tot$WEG.BAAN.STROOK.VAN,r.int.tot$jaar),]
#r.int.tot$num.jr.int<- ifelse()
r.int.tot$verschil_planjaar<- ifelse(r.int.tot$WEG.BAAN.STROOK.VAN == Lag(r.int.tot$WEG.BAAN.STROOK.VAN ) & Lag(r.int.tot$jr.tot.interventie<= 5) &
                                       r.int.tot$jaar - Lag(r.int.tot$jaar) == 1, 
                                     r.int.tot$jr.tot.interventie - Lag(r.int.tot$jr.tot.interventie),NA)

r.int.tot$verloop<- ifelse(r.int.tot$verschil_planjaar== -1, "Normaal", ifelse(r.int.tot$verschil_planjaar < (-1),"Snel",
                                                                               ifelse(r.int.tot$verschil_planjaar == 0, "Langzaam",NA)))
r.int.tot$schade<- ifelse(r.int.tot$jr.tot.interventie<= 5,1,0)


##eerste keer schade
r.int.tot$eerste.schade<- ifelse(r.int.tot$WEG.BAAN.STROOK.VAN == Lag(r.int.tot$WEG.BAAN.STROOK.VAN ) & Lag(r.int.tot$jr.tot.interventie==6) &
                                   r.int.tot$jr.tot.interventie<= 5,
                                 1,0)
r.int.tot$eerste.schade.leeftijd<- ifelse(r.int.tot$WEG.BAAN.STROOK.VAN == Lag(r.int.tot$WEG.BAAN.STROOK.VAN ) & Lag(r.int.tot$jr.tot.interventie==6) &
                                   r.int.tot$jr.tot.interventie<= 5,
                                 r.int.tot$leeftijd,NA)

eerste.schade.leeftijd<- aggregate(eerste.schade.leeftijd~WEG.BAAN.STROOK.VAN,r.int.tot,FUN="mean")

#save(r.int.tot,file="data/r_int_tot.RData")
#write.csv(r.int.tot,file="data/r_int_tot_export_dec2016.csv")

ggplot(eerste.schade.leeftijd,aes(x=eerste.schade.leeftijd))+geom_histogram(binwidth = 1,color="black")

library(rpart)
library(rpart.plot)
library(caret)
#Voorspel of een wegvak een normaal of snel verloop heeft 

xnam <- colnames(r.int.tot)
xnam<- xnam[! xnam %in% c("verloop","BAAN","STROOK","VAN","TOT","AANLEGDATUM","INTERVENTIEJAAR_RAF",
     "BUITENSTE_RIJSTROOK","jaar","WEG.BAAN.STROOK.VAN","vervangen","vervangen.leeftijd",
     "verdacht","vvr","ldr.verw","WEG.VAN.BAAN","x","y", "WVK_ID","district", "dienst")]
fmla <- as.formula(paste("verloop ~ ", paste(xnam, collapse= "+")))

tree<- rpart(fmla       ,r.int.tot,method = "class", cp = 0.002)

#png("plots/tree_2012.png",width = 1000,height = 1000,res=100)
rpart.plot(tree,cex=0.7,yesno=T,type=4,fallen.leaves=F)
#dev.off()

summary(tree)



##
r.int.tot<- as.data.frame(r.int.tot)
r.klein<- r.int.tot[,c("AANLEGDATUM","WEG.BAAN.STROOK.VAN","jaar","jr.tot.interventie","verschil_planjaar", "verloop","eerste.schade")]
       
r.int.schade<- raf_tot[raf_tot$schade==1,]

library(ggplot2)

ggplot(r.int.schade,aes(x=factor(verloop),y=aanleg.FG))+geom_boxplot()
ggplot(r.int.schade,aes(x=aanleg.FG,color=verloop))+geom_density()
ggplot(r.int.schade,aes(x=factor(verloop),y=aanleg.TG))+geom_boxplot()
ggplot(r.int.schade,aes(x=factor(verloop),y=aanleg.RH))+geom_boxplot()
ggplot(r.int.schade,aes(x=factor(verloop),y=Inw_etm_L3))+geom_boxplot()
ggplot(r.int.schade,aes(x=factor(verloop),y=pgroen_density))+geom_boxplot()+scale_y_log10()
ggplot(r.int.schade,aes(x=Inw_etm_L3,color=verloop))+geom_density()
ggplot(r.int.schade,aes(x=jaar,fill=verloop))+geom_histogram()

ggplot(r.int.schade,aes(x=jr.tot.interventie,fill=verloop))+geom_histogram()
                                                                               
ggplot(r.int.schade,aes(x=freeze_count_1,color=verloop))+geom_density()+xlab(">15 minute periods with T < 0 °C")+
  ylab("Relative density of road segments")  +scale_color_manual(labels = c("Slow", "Normal","Fast", "NA"), values = c("red","green","blue","grey")) +
  #theme_bw() +
  guides(color=guide_legend("Damage increase")) 


ggplot(r.int.schade,aes(x=verloop))+geom_histogram(stat="count")+facet_wrap(~dienst)

ggplot(r.int.schade,aes(x=freeze_count_2,color=verloop))+geom_density()+facet_wrap(~bodem)
ggplot(r.int.schade,aes(x=aanleg.TN,color=verloop))+geom_density()
ggplot(r.int.schade,aes(x=pgroen_density,color=verloop))+geom_density()+xlim(0,50)
ggplot(r.int.schade,aes(x=verloop,y=pgroen_density,fill=verloop))+geom_boxplot()+ylim(0,50)

ggplot(r.int.schade,aes(x=verloop,y=boogstraal.mean,fill=verloop))+geom_boxplot()
ggplot(r.int.schade,aes(x=boogstraal.mean,color=verloop))+geom_density()
ggplot(r.int.schade,aes(x=afst.in.uitvoeg,color=verloop))+geom_density()

ggplot(r.int.schade,aes(x=freeze_count_2,color=verloop))+geom_density()+facet_wrap(~bodem,scales="free_y")


r_verloop<- r.int.schade[complete.cases(r.int.schade$verloop),]
ggplot(r_verloop,aes(x=verloop))+geom_histogram(stat="count")+facet_wrap(~jaar)
