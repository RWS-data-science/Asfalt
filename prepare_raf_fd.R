### Prepare a dataset to perform regression analysis on. Regression is done on the age at which the 
### first raveling damage is observed, so only these years are selected. Variables that have cumulative 
### effects over the years are cumulated (e.g. weather or traffic)

load("data/raf_tot.RData")
raf_tot<- as.data.frame(raf_tot)

#cumulative for cumulative influences (e.g. weather)

cumSkipNA <- function(x, FUNC)
{
  d <- deparse(substitute(FUNC))
  funs <- c("max", "min", "prod", "sum")
  stopifnot(is.vector(x), is.numeric(x), d %in% funs)
  FUNC <- match.fun(paste0("cum", d))
  x[!is.na(x)] <- FUNC(x[!is.na(x)])
  x
}

library(data.table) 
library(dplyr)
#cumsum the yearly varying variables 
setDT(raf_tot)[, freeze_count_1_cum:=cumSkipNA(freeze_count_1,sum), 
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, freeze_count_2_cum:=cumSkipNA(freeze_count_2,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, freeze_count_3_cum:=cumSkipNA(freeze_count_3,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, melt_minutes_cum:=cumSkipNA(melt_minutes,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, freeze_pattern_count_cum:=cumSkipNA(freeze_pattern_count,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[,Inw_etm_AL_cum:=cumSkipNA(gem_Inw_etm_AL,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[,Inw_etm_L1_cum:=cumSkipNA(gem_Inw_etm_L1,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, Inw_etm_L2_cum:=cumSkipNA(gem_Inw_etm_L2,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, Inw_etm_L3_cum:=cumSkipNA(gem_Inw_etm_L3,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, dieren_cum:=cumSkipNA(dieren,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, ongeval_cum:=cumSkipNA(ongeval,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, voorwerp_cum:=cumSkipNA(voorwerp,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, voorwerp_op_rijstrook_cum:=cumSkipNA(voorwerp_op_rijstrook,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, werk_in_uitvoering_cum:=cumSkipNA(werk_in_uitvoering,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, infraschade_cum:=cumSkipNA(infraschade,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]
setDT(raf_tot)[, schadelijke_stof_cum:=cumSkipNA(schadelijke_stof,sum),
               by=list(WEG.BAAN.STROOK.VAN,AANLEGDATUM)]



head(raf_tot[,c("WEG.BAAN.STROOK.VAN","vervangen","AANLEGDATUM","freeze_count_1","freeze_count_1_cum","Inw_etm_L3_cum")],100)

##create set containing only the first damage occurrences

raf_fd<- as.data.frame(raf_tot[which(raf_tot$eerste.schade==1),])


# library(rgdal)
# raf_wgs<- raf_fd[complete.cases(raf_fd$x),]
# coordinates(raf_wgs)<- ~x+y
# wgs<- CRS("+init=epsg:4326")
# proj_rd<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +no_defs"
# 
# proj4string(raf_wgs)<- proj_rd
# raf_wgs<- spTransform(raf_wgs,wgs)
# raf_wgs<- as.data.frame(raf_wgs)
# 
# raf_fd$x.wgs<- raf_wgs$x[match(raf_fd$ID,raf_wgs$ID)]
# raf_fd$y.wgs<- raf_wgs$y[match(raf_fd$ID,raf_wgs$ID)]

#save files
#save(raf_fd,file="data/raf_fd.RData")
#save(raf_fd_prep,file="data/raf_fd_prep.RData")
#write.csv(raf_fd_prep,file="data/raf_fd_prep.csv")