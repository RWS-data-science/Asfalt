##UDLS Stijn

load("data/raf_tot.RData")
load("data/UDLS_tot_clean_transformed.Rdata")

UDLS_tot_clean_transformed$hoofdrijbaan<- ifelse(UDLS_tot_clean_transformed$richting == "right", "1HRR",
                                                 ifelse(UDLS_tot_clean_transformed$richting == "left","1HRL",
                                                        ifelse(UDLS_tot_clean_transformed$richting == "middle","0HRM",NA)))
UDLS_tot_clean_transformed$wegnummerA<- as.numeric(substr(UDLS_tot_clean_transformed$weg,regexpr(pattern ='A',UDLS_tot_clean_transformed$weg)+1,
                                    regexpr(pattern ='A',UDLS_tot_clean_transformed$weg)+3))
UDLS_tot_clean_transformed$wegnummerN<- as.numeric(substr(UDLS_tot_clean_transformed$weg,regexpr(pattern ='N',UDLS_tot_clean_transformed$weg)+1,
                                                          regexpr(pattern ='N',UDLS_tot_clean_transformed$weg)+3)) 

UDLS_tot_clean_transformed$wegnummer<- ifelse(!is.na(UDLS_tot_clean_transformed$wegnummerA),UDLS_tot_clean_transformed$wegnummerA,
                                              UDLS_tot_clean_transformed$wegnummerN)
UDLS_tot_clean_transformed$wegnummerN<- NULL;UDLS_tot_clean_transformed$wegnummerA<- NULL

UDLS_tot_clean_transformed$WEG.BAAN.VAN.JAAR<- paste(UDLS_tot_clean_transformed$wegnummer,UDLS_tot_clean_transformed$hoofdrijbaan,
                                                     UDLS_tot_clean_transformed$hectometer,UDLS_tot_clean_transformed$period,sep=".")

UDLS_tot_clean_transformed$WEG.BAAN.VAN.Links.JAAR<- ifelse(UDLS_tot_clean_transformed$richting=="both",paste(UDLS_tot_clean_transformed$wegnummer,
                                                "1HRL",UDLS_tot_clean_transformed$hectometer,UDLS_tot_clean_transformed$period,sep="."),NA)
UDLS_tot_clean_transformed$WEG.BAAN.VAN.Rechts.JAAR<- ifelse(UDLS_tot_clean_transformed$richting=="both",paste(UDLS_tot_clean_transformed$wegnummer,
                                                "1HRR",UDLS_tot_clean_transformed$hectometer,UDLS_tot_clean_transformed$period,sep="."),NA)
UDLS_tot_clean_transformed$WEG.BAAN.VAN.hrm.JAAR<- ifelse(UDLS_tot_clean_transformed$richting=="both",paste(UDLS_tot_clean_transformed$wegnummer,
                                               "0HRM",UDLS_tot_clean_transformed$hectometer,UDLS_tot_clean_transformed$period,sep="."),NA)


##
raf_tot$WEG.BAAN.VAN.JAAR<- paste(raf_tot$WEG.BAAN.VAN,raf_tot$jaar,sep=".")

#dieren
raf_tot$dieren1<- UDLS_tot_clean_transformed$dieren[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.JAAR)]
raf_tot$dieren.R<- UDLS_tot_clean_transformed$dieren[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Rechts.JAAR)]
raf_tot$dieren.0<- UDLS_tot_clean_transformed$dieren[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.hrm.JAAR)]
raf_tot$dieren.L<- UDLS_tot_clean_transformed$dieren[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Links.JAAR)]
raf_tot$dieren<- ifelse(!is.na(raf_tot$dieren1),raf_tot$dieren1,ifelse(!is.na(raf_tot$dieren.R),raf_tot$dieren.R,
                            ifelse(!is.na(raf_tot$dieren.0),raf_tot$dieren.0,ifelse(!is.na(raf_tot$dieren.L),raf_tot$dieren.L,0))))
raf_tot$dieren1<-NULL;raf_tot$dieren.R<- NULL; raf_tot$dieren.0 <- NULL;raf_tot$dieren.L<- NULL

#ongeval
raf_tot$ongeval1<- UDLS_tot_clean_transformed$ongeval[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.JAAR)]
raf_tot$ongeval.R<- UDLS_tot_clean_transformed$ongeval[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Rechts.JAAR)]
raf_tot$ongeval.0<- UDLS_tot_clean_transformed$ongeval[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.hrm.JAAR)]
raf_tot$ongeval.L<- UDLS_tot_clean_transformed$ongeval[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Links.JAAR)]
raf_tot$ongeval<- ifelse(!is.na(raf_tot$ongeval1),raf_tot$ongeval1,ifelse(!is.na(raf_tot$ongeval.R),raf_tot$ongeval.R,
                                                                             ifelse(!is.na(raf_tot$ongeval.0),raf_tot$ongeval.0,ifelse(!is.na(raf_tot$ongeval.L),raf_tot$ongeval.L,0))))
raf_tot$ongeval1<-NULL;raf_tot$ongeval.R<- NULL; raf_tot$ongeval.0 <- NULL;raf_tot$ongeval.L<- NULL

#voorwerp
raf_tot$voorwerp1<- UDLS_tot_clean_transformed$voorwerp[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.JAAR)]
raf_tot$voorwerp.R<- UDLS_tot_clean_transformed$voorwerp[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Rechts.JAAR)]
raf_tot$voorwerp.0<- UDLS_tot_clean_transformed$voorwerp[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.hrm.JAAR)]
raf_tot$voorwerp.L<- UDLS_tot_clean_transformed$voorwerp[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Links.JAAR)]
raf_tot$voorwerp<- ifelse(!is.na(raf_tot$voorwerp1),raf_tot$voorwerp1,ifelse(!is.na(raf_tot$voorwerp.R),raf_tot$voorwerp.R,
                                                                             ifelse(!is.na(raf_tot$voorwerp.0),raf_tot$voorwerp.0,ifelse(!is.na(raf_tot$voorwerp.L),raf_tot$voorwerp.L,0))))
raf_tot$voorwerp1<-NULL;raf_tot$voorwerp.R<- NULL; raf_tot$voorwerp.0 <- NULL;raf_tot$voorwerp.L<- NULL

#voorwerp_op_rijstrook
raf_tot$voorwerp_op_rijstrook1<- UDLS_tot_clean_transformed$voorwerp_op_rijstrook[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.JAAR)]
raf_tot$voorwerp_op_rijstrook.R<- UDLS_tot_clean_transformed$voorwerp_op_rijstrook[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Rechts.JAAR)]
raf_tot$voorwerp_op_rijstrook.0<- UDLS_tot_clean_transformed$voorwerp_op_rijstrook[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.hrm.JAAR)]
raf_tot$voorwerp_op_rijstrook.L<- UDLS_tot_clean_transformed$voorwerp_op_rijstrook[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Links.JAAR)]
raf_tot$voorwerp_op_rijstrook<- ifelse(!is.na(raf_tot$voorwerp_op_rijstrook1),raf_tot$voorwerp_op_rijstrook1,ifelse(!is.na(raf_tot$voorwerp_op_rijstrook.R),raf_tot$voorwerp_op_rijstrook.R,
                                                                             ifelse(!is.na(raf_tot$voorwerp_op_rijstrook.0),raf_tot$voorwerp_op_rijstrook.0,ifelse(!is.na(raf_tot$voorwerp_op_rijstrook.L),raf_tot$voorwerp_op_rijstrook.L,0))))
raf_tot$voorwerp_op_rijstrook1<-NULL;raf_tot$voorwerp_op_rijstrook.R<- NULL; raf_tot$voorwerp_op_rijstrook.0 <- NULL;raf_tot$voorwerp_op_rijstrook.L<- NULL

#werk_in_uitvoering
raf_tot$werk_in_uitvoering1<- UDLS_tot_clean_transformed$werk_in_uitvoering[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.JAAR)]
raf_tot$werk_in_uitvoering.R<- UDLS_tot_clean_transformed$werk_in_uitvoering[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Rechts.JAAR)]
raf_tot$werk_in_uitvoering.0<- UDLS_tot_clean_transformed$werk_in_uitvoering[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.hrm.JAAR)]
raf_tot$werk_in_uitvoering.L<- UDLS_tot_clean_transformed$werk_in_uitvoering[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Links.JAAR)]
raf_tot$werk_in_uitvoering<- ifelse(!is.na(raf_tot$werk_in_uitvoering1),raf_tot$werk_in_uitvoering1,ifelse(!is.na(raf_tot$werk_in_uitvoering.R),raf_tot$werk_in_uitvoering.R,
                                                                             ifelse(!is.na(raf_tot$werk_in_uitvoering.0),raf_tot$werk_in_uitvoering.0,ifelse(!is.na(raf_tot$werk_in_uitvoering.L),raf_tot$werk_in_uitvoering.L,0))))
raf_tot$werk_in_uitvoering1<-NULL;raf_tot$werk_in_uitvoering.R<- NULL; raf_tot$werk_in_uitvoering.0 <- NULL;raf_tot$werk_in_uitvoering.L<- NULL#werk_in_uitvoering

#infraschade
raf_tot$infraschade1<- UDLS_tot_clean_transformed$infraschade[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.JAAR)]
raf_tot$infraschade.R<- UDLS_tot_clean_transformed$infraschade[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Rechts.JAAR)]
raf_tot$infraschade.0<- UDLS_tot_clean_transformed$infraschade[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.hrm.JAAR)]
raf_tot$infraschade.L<- UDLS_tot_clean_transformed$infraschade[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Links.JAAR)]
raf_tot$infraschade<- ifelse(!is.na(raf_tot$infraschade1),raf_tot$infraschade1,ifelse(!is.na(raf_tot$infraschade.R),raf_tot$infraschade.R,
                                                                             ifelse(!is.na(raf_tot$infraschade.0),raf_tot$infraschade.0,ifelse(!is.na(raf_tot$infraschade.L),raf_tot$infraschade.L,0))))
raf_tot$infraschade1<-NULL;raf_tot$infraschade.R<- NULL; raf_tot$infraschade.0 <- NULL;raf_tot$infraschade.L<- NULL

#schadelijke_stof
raf_tot$schadelijke_stof1<- UDLS_tot_clean_transformed$schadelijke_stof[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.JAAR)]
raf_tot$schadelijke_stof.R<- UDLS_tot_clean_transformed$schadelijke_stof[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Rechts.JAAR)]
raf_tot$schadelijke_stof.0<- UDLS_tot_clean_transformed$schadelijke_stof[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.hrm.JAAR)]
raf_tot$schadelijke_stof.L<- UDLS_tot_clean_transformed$schadelijke_stof[match(raf_tot$WEG.BAAN.VAN.JAAR,UDLS_tot_clean_transformed$WEG.BAAN.VAN.Links.JAAR)]
raf_tot$schadelijke_stof<- ifelse(!is.na(raf_tot$schadelijke_stof1),raf_tot$schadelijke_stof1,ifelse(!is.na(raf_tot$schadelijke_stof.R),raf_tot$schadelijke_stof.R,
                                                                             ifelse(!is.na(raf_tot$schadelijke_stof.0),raf_tot$schadelijke_stof.0,ifelse(!is.na(raf_tot$schadelijke_stof.L),raf_tot$schadelijke_stof.L,0))))
raf_tot$schadelijke_stof1<-NULL;raf_tot$schadelijke_stof.R<- NULL; raf_tot$schadelijke_stof.0 <- NULL;raf_tot$schadelijke_stof.L<- NULL


###
cor(raf_tot$dieren,raf_tot$t_num200,use="complete")

#save("r.int.tot",file="data/r_int_tot_2.RData")

r.int.schade<- raf_tot[raf_tot$schade==1,]

#UDLS
ggplot(r.int.schade,aes(x=dieren,fill=verloop))+geom_histogram()+scale_y_log10()
ggplot(r.int.schade,aes(x=ongeval,fill=verloop))+geom_histogram()+scale_y_log10()
ggplot(r.int.schade,aes(x=voorwerp,fill=verloop))+geom_histogram()+scale_y_log10()
ggplot(r.int.schade,aes(x=voorwerp_op_rijstrook,fill=verloop))+geom_histogram()+scale_y_log10()
ggplot(r.int.schade,aes(x=werk_in_uitvoering,fill=verloop))+geom_histogram()+scale_y_log10()
ggplot(r.int.schade,aes(x=infraschade,fill=verloop))+geom_histogram()+scale_y_log10()
ggplot(r.int.schade,aes(x=schadelijke_stof,fill=verloop))+geom_histogram()+scale_y_log10()
