###data 2015/2016
library(readxl) 
rsd_lvl2004<- read_excel("data/MJPV DATA 2004_2016/M05_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2005<- read_excel("data/MJPV DATA 2004_2016/M06_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2006<- read_excel("data/MJPV DATA 2004_2016/M07_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2007<- read_excel("data/MJPV DATA 2004_2016/M08_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2008<- read_excel("data/MJPV DATA 2004_2016/M09_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2009<- read_excel("data/MJPV DATA 2004_2016/M10_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2010<- read_excel("data/MJPV DATA 2004_2016/M11_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2011<- read_excel("data/MJPV DATA 2004_2016/M12_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2012<- read_excel("data/MJPV DATA 2004_2016/M13_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2013<- read_excel("data/MJPV DATA 2004_2016/M14_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2014<- read_excel("data/MJPV DATA 2004_2016/M15_Metingen_RSD_LVL_STR.xlsx")
rsd_lvl2015<- read_excel("data/MJPV DATA 2004_2016/M16_Metingen_RSD_LVL_STR.xlsx")

rsd_lvl2004$jaar<- 2004;rsd_lvl2005$jaar<- 2005;rsd_lvl2006$jaar<- 2006;rsd_lvl2007$jaar<- 2007;rsd_lvl2008$jaar<- 2008;
rsd_lvl2009$jaar<- 2009;rsd_lvl2010$jaar<- 2010;rsd_lvl2011$jaar<- 2011;rsd_lvl2012$jaar<- 2012;rsd_lvl2013$jaar<- 2013;
rsd_lvl2014$jaar<- 2014;rsd_lvl2015$jaar<- 2015;

rsd_lvl_tot<- rbind(rsd_lvl2004,rsd_lvl2005,rsd_lvl2006,rsd_lvl2007,rsd_lvl2008,rsd_lvl2009,rsd_lvl2010,rsd_lvl2011,
                rsd_lvl2012,rsd_lvl2013,rsd_lvl2014,rsd_lvl2015)


rsd_lvl_tot$WEG.BAAN.STROOK.VAN.JAAR<- paste(rsd_lvl_tot$WEG,rsd_lvl_tot$BAAN,rsd_lvl_tot$STROOK,rsd_lvl_tot$Van,rsd_lvl_tot$jaar,sep=".")

langs<- rsd_lvl_tot[which(rsd_lvl_tot$SCHADE=="LVL"),]
dwars<- rsd_lvl_tot[which(rsd_lvl_tot$SCHADE=="RSD"),]


raf_tot$langs<- langs$MEETWAARDE[match(raf_tot$WEG.BAAN.STROOK.VAN.JAAR,langs$WEG.BAAN.STROOK.VAN.JAAR)]
raf_tot$dwars<- dwars$MEETWAARDE[match(raf_tot$WEG.BAAN.STROOK.VAN.JAAR,langs$WEG.BAAN.STROOK.VAN.JAAR)]
 

#save(raf_tot,file="data/raf_tot.RData")



ggplot(raf_tot,aes(x=factor(jr.tot.interventie),y=langs))+geom_boxplot()
ggplot(raf_tot,aes(x=factor(jr.tot.interventie),y=dwars))+geom_boxplot()


