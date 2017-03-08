###Geodan Bomen
# library(rgdal)
# #bomen<- readOGR("Maarten/linesegments3/linesegments3.shp",layer="linesegments3")
# bomen<- readOGR("data/rws_geodan_vegetation_roaddegradation/linesegments.shp")
# bomen<-  as.data.frame(bomen)

library(foreign)
trees_shadows<- read.dbf("data/rws_geodan_vegetation_roaddegradation/linesegments.dbf")

raf_tot$wegvanbaan<- paste(raf_tot$WEGNUMMER,raf_tot$VAN,raf_tot$BAAN,sep=".")

raf_tot$t_area50<- trees_shadows$t_area50[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_area100<- trees_shadows$t_area100[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_area200<- trees_shadows$t_area200[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_avg_height50<- trees_shadows$t_avg_high[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_avg_height100<- trees_shadows$t_avg_hi_1[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_avg_height200<- trees_shadows$t_avg_hi_2[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_num50<- trees_shadows$t_num50[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_num100<- trees_shadows$t_num100[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_num200<- trees_shadows$t_num200[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_num_10_50<- trees_shadows$t_num_10_5[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_num_10_100<- trees_shadows$t_num_10_1[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_num_10_200<- trees_shadows$t_num_10_2[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_volume50<- trees_shadows$t_volume50[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_volume100<- trees_shadows$t_volume10[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$t_volume200<- trees_shadows$t_volume20[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]


###shadows

raf_tot$gkn_shadow<- trees_shadows$gkn_shadow[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$tree_shadow<- trees_shadows$tree_shado[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]
raf_tot$tot_shadow<- trees_shadows$tot_shadow[match(raf_tot$wegvanbaan,trees_shadows$wegvanbaan)]

raf_tot$wegvanbaan<- NULL
#save(raf_tot,file="data/raf_tot.RData")





raf_schade<- raf_tot[which(raf_tot$schade==1),]

a1<-ggplot(r.int.schade,aes(x=t_area50,color=verloop))+geom_density()
a2<-ggplot(r.int.schade,aes(x=t_area100,color=verloop))+geom_density()
a3<-ggplot(r.int.schade,aes(x=t_area200,color=verloop))+geom_density()
a4<-ggplot(r.int.schade,aes(x=t_avg_height50,color=verloop))+geom_density()
a5<-ggplot(r.int.schade,aes(x=t_avg_height100,color=verloop))+geom_density()
a6<-ggplot(r.int.schade,aes(x=t_avg_height200,color=verloop))+geom_density()
a7<-ggplot(r.int.schade,aes(x=t_num50,color=verloop))+geom_density()
a8<-ggplot(r.int.schade,aes(x=t_num100,color=verloop))+geom_density()
a9<-ggplot(r.int.schade,aes(x=t_num200,color=verloop))+geom_density()
a10<-ggplot(r.int.schade,aes(x=t_num_10_50,color=verloop))+geom_density()
a11<-ggplot(r.int.schade,aes(x=t_num_10_100,color=verloop))+geom_density()
a12<-ggplot(r.int.schade,aes(x=t_num_10_200,color=verloop))+geom_density()
a13<-ggplot(r.int.schade,aes(x=t_volume50,color=verloop))+geom_density()
a14<-ggplot(r.int.schade,aes(x=t_volume100,color=verloop))+geom_density()
a15<-ggplot(r.int.schade,aes(x=t_volume200,color=verloop))+geom_density()

library(gridExtra)
grid.arrange(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,ncol=3)

ggplot(r.int.schade,aes(x=t_area50,fill=verloop))+geom_histogram(position="fill")

ggplot(bomen,aes(x=t_area100,y=t_area200))+geom_point()
r.int.schade$eerste.schade.leeftijd<- as.integer(r.int.schade$eerste.schade.leeftijd)
ggplot(r.int.schade,aes(y=t_area50,x=eerste.schade.leeftijd))+geom_hex()+geom_smooth()
ggplot(r.int.schade,aes(y=t_volume50,x=eerste.schade.leeftijd))+geom_bin2d()+geom_smooth()

ggplot(r.int.tot,aes(eerste.schade.leeftijd))+geom_histogram()

b1<-ggplot(r.int.schade,aes(y=t_area50,x=verloop,fill=verloop))+geom_boxplot()
b2<-ggplot(r.int.schade,aes(y=t_area100,x=verloop,fill=verloop))+geom_boxplot()
b3<-ggplot(r.int.schade,aes(y=t_area200,x=verloop,fill=verloop))+geom_boxplot()
b4<-ggplot(r.int.schade,aes(y=t_avg_height50,x=verloop,fill=verloop))+geom_boxplot()
b5<-ggplot(r.int.schade,aes(y=t_avg_height100,x=verloop,fill=verloop))+geom_boxplot()
b6<-ggplot(r.int.schade,aes(y=t_avg_height200,x=verloop,fill=verloop))+geom_boxplot()
b7<-ggplot(r.int.schade,aes(y=t_num50,x=verloop,fill=verloop))+geom_boxplot()
b8<-ggplot(r.int.schade,aes(y=t_num100,x=verloop,fill=verloop))+geom_boxplot()
b9<-ggplot(r.int.schade,aes(y=t_num200,x=verloop,fill=verloop))+geom_boxplot()
b10<-ggplot(r.int.schade,aes(y=t_num_10_50,x=verloop,fill=verloop))+geom_boxplot()
b11<-ggplot(r.int.schade,aes(y=t_num_10_100,x=verloop,fill=verloop))+geom_boxplot()
b12<-ggplot(r.int.schade,aes(y=t_num_10_200,x=verloop,fill=verloop))+geom_boxplot()
b13<-ggplot(r.int.schade,aes(y=t_volume50,x=verloop,fill=verloop))+geom_boxplot()
b14<-ggplot(r.int.schade,aes(y=t_volume100,x=verloop,fill=verloop))+geom_boxplot()
b15<-ggplot(r.int.schade,aes(y=t_volume200,x=verloop,fill=verloop))+geom_boxplot()

library(gridExtra)
grid.arrange(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,ncol=3)

c1<-ggplot(r.int.schade,aes(x=t_area50,fill=verloop))+geom_histogram(position="fill",bins=100)
c2<-ggplot(r.int.schade,aes(x=t_area100,fill=verloop))+geom_histogram(position="fill",bins=100)
c3<-ggplot(r.int.schade,aes(x=t_area200,fill=verloop))+geom_histogram(position="fill",bins=100)
c4<-ggplot(r.int.schade,aes(x=t_avg_height50,fill=verloop))+geom_histogram(position="fill",bins=100)
c5<-ggplot(r.int.schade,aes(x=t_avg_height100,fill=verloop))+geom_histogram(position="fill",bins=100)
c6<-ggplot(r.int.schade,aes(x=t_avg_height200,fill=verloop))+geom_histogram(position="fill",bins=100)
c7<-ggplot(r.int.schade,aes(x=t_num50,fill=verloop))+geom_histogram(position="fill",bins=100)
c8<-ggplot(r.int.schade,aes(x=t_num100,fill=verloop))+geom_histogram(position="fill",bins=100)
c9<-ggplot(r.int.schade,aes(x=t_num200,fill=verloop))+geom_histogram(position="fill",bins=100)
c10<-ggplot(r.int.schade,aes(x=t_num_10_50,fill=verloop))+geom_histogram(position="fill",bins=100)
c11<-ggplot(r.int.schade,aes(x=t_num_10_100,fill=verloop))+geom_histogram(position="fill",bins=100)
c12<-ggplot(r.int.schade,aes(x=t_num_10_200,fill=verloop))+geom_histogram(position="fill",bins=100)
c13<-ggplot(r.int.schade,aes(x=t_volume50,fill=verloop))+geom_histogram(position="fill",bins=100)
c14<-ggplot(r.int.schade,aes(x=t_volume100,fill=verloop))+geom_histogram(position="fill",bins=100)
c15<-ggplot(r.int.schade,aes(x=t_volume200,fill=verloop))+geom_histogram(position="fill",bins=100)

grid.arrange(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,ncol=3)


ggplot(r.int.tot,aes(x=t_num50,y=pgroen_density))+geom_hex()
cor(r.int.tot$t_num50,r.int.tot$pgroen_density,use="complete")


###Schadows


library(ggplot2)
ggplot(r.int.schade,aes(x=tot_shadow,color=verloop))+geom_density()+scale_y_log10()
