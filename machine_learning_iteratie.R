load(file= "data/r_int_sample_feb17.RData")

source("prepare.R")

r.samp<- prepare(r.int.sample)

na_count <-sapply(r.samp, function(y) sum(is.na(y)))

na_count/nrow(r.samp)

xnam <- colnames(r.samp)
xnam<- xnam[! xnam %in% c("schade","verloop")]
fmla <- as.formula(paste("schade ~ ", paste(xnam, collapse= "+")))



library(rpart)
library(rpart.plot)
library(caret)

#train <- r_2014[train_ind, ]
#test <- r_2014[-train_ind, ]
# train2011<- r.samp[r.samp$jaar != 2011,]
# test2011<- r.samp[r.samp$jaar == 2011,]
# train2012<- r.samp[r.samp$jaar != 2012,]
# test2012<- r.samp[r.samp$jaar == 2012,]
# train2013<- r.samp[r.samp$jaar != 2013,]
# test2013<- r.samp[r.samp$jaar == 2013,]
# train2014<- r.samp[r.samp$jaar != 2014,]
# test2014<- r.samp[r.samp$jaar == 2014,]
# train2015<- r.samp[r.samp$jaar != 2015,]
# test2015<- r.samp[r.samp$jaar == 2015,]


ml<- function(jaar){
  
train<- r.samp[r.samp$jaar != jaar,]
test<- r.samp[r.samp$jaar == jaar,]

###tree
tree<- rpart(fmla,train,method = "class")

#rpart.plot(tree,cex=0.7,yesno=T,type=2,compress=T)
test$pred_tree<- predict(tree,test,typ="class")

#confusionMatrix(test$schade,test$pred_tree)
tree_res<- as.data.frame(confusionMatrix(test$schade,test$pred_tree)[[3]]);colnames(tree_res)<- "tree"

###rf

library(randomForest)


train$schade<- as.factor(train$schade)

rf<- randomForest(fmla,data=train,ntree=100,importance=T,na.action = na.omit)


test$rf_res<- predict(rf,test,typ="class")

#confusionMatrix(test$schade,test$rf_res)
rf_res<- as.data.frame(confusionMatrix(test$schade,test$rf_res)[[3]]);colnames(rf_res)<- "rf"

#plot(rf)
#varImpPlot(rf,n.var=15,type=1)
#summary(rf)


##h2o deep neural en xgboost
library("h2o")
## start a local cluster with 1GB RAM
localH2O = h2o.init(max_mem_size = '6g', # use 6GB of RAM of *GB availabke on Kaggle
                    nthreads = -1) 

#df.train.com<- df.train[complete.cases(df.train),]
## import MNIST data as H2O
train_h2o = as.h2o(train)
test_h2o = as.h2o(test)
## set timer
#s <- proc.time()

model =
  h2o.deeplearning(x = c(1:30,33:nrow(train)),  # column numbers for predictors
                   y = 32,   # column number for label
                   training_frame = train_h2o, hidden=c(200)) # data in H2O format

model2 =
  h2o.gbm(x = c(1:30,33:nrow(train)),  # column numbers for predictors
                   y = 32,   # column number for label
                   training_frame = train_h2o) # data in H2O format

model3=
  h2o.naiveBayes(x = c(1:30,33:nrow(train)),  # column numbers for predictors
                 y = 32,   # column number for label
                 training_frame = train_h2o)

#model4=
#  h2o.

h2o_y_test <- h2o.predict(model, test_h2o)
h2o_y_test2 <- h2o.predict(model2, test_h2o)
h2o_y_test3 <- h2o.predict(model3, test_h2o)
#h2o.varimp_plot(model)
h2o.varimp_plot(model2,num_of_features = 15)
#h2o.varimp_plot(model3)


## convert H2O format into data frame and  save as csv
df_y_test = as.data.frame(h2o_y_test)
df_y_test = data.frame(ImageId = seq(1,length(df_y_test$predict)), Label = df_y_test$predict)

df_y_test2 = as.data.frame(h2o_y_test2)
df_y_test2 = data.frame(ImageId = seq(1,length(df_y_test2$predict)), Label = df_y_test2$predict)

df_y_test3 = as.data.frame(h2o_y_test3)
df_y_test3 = data.frame(ImageId = seq(1,length(df_y_test3$predict)), Label = df_y_test3$predict)

## shut down virutal H2O cluster
h2o.shutdown(prompt=F)


test$pred.h2o<- as.factor(df_y_test$Label)
test$pred.h2o.xg<- as.factor(df_y_test2$Label)
test$pred.h2o.nb<- as.factor(df_y_test3$Label)

#confusionMatrix(test$schade,test$pred.h2o)
h2o_res<- as.data.frame(confusionMatrix(test$schade,test$pred.h2o)[[3]]);colnames(h2o_res)<- "h2o.net"
#confusionMatrix(test$schade,test$pred.h2o.xg)
xgb_res<- as.data.frame(confusionMatrix(test$schade,test$pred.h2o.xg)[[3]]);colnames(xgb_res)<- "xgb.res"

nb_res<- as.data.frame(confusionMatrix(test$schade,test$pred.h2o.nb)[[3]]);colnames(nb_res)<- "nb.res"

res<- as.data.frame(rbind(t(tree_res),t(rf_res),t(h2o_res),t(xgb_res),t(nb_res)))
res$jaar<- jaar
res$model<- rownames(res)

return(res)
}


ml_res<- lapply(c(2011:2015),ml)

ml_res1<- rbindlist(t(ml_res))
#save(ml_res1,file="data/ml_res.RData")

ml_res1$net.acc<- ml_res1$Accuracy - ml_res1$AccuracyNull
#ggplot(ml_res1,aes(x=model,y=Accuracy))+geom_boxplot()+geom_point(aes(y=AccuracyNull))
#ggplot(ml_res1,aes(x=model,y=Kappa))+geom_boxplot()

#png("plots/model_accgain.png",height = 500, width = 720, res=100)
ggplot(ml_res1,aes(x=model,y=net.acc))+geom_boxplot()+xlab("Model")+ylab("Accuracy gain")+ 
  scale_x_discrete(labels= c("Neural Net","Naive Bayes", "Random Forest", "Decision tree","Gradient Boosting \n Machine"))
#dev.off()
#png("plots/model_accgainjaar.png",height = 500, width = 720, res=100)
ggplot(ml_res1,aes(x=factor(jaar),y=net.acc))+geom_boxplot()+xlab("Year")+ylab("Accuracy gain")
#dev.off()

ggplot(ml_res1,aes(x=factor(jaar),y=Kappa))+geom_boxplot()

#resultaten stijn
weka.res<- read.csv("data/WekaResults.ms.csv",sep=";",dec=",")


library(ggplot2)
sub2<- weka.res[weka.res$Model %in% c("Bayes on all features","J48 GPO baseline C 0,05","SMO", "ZeroR") & weka.res$Concept == "sch" & weka.res$Variant == "resampled",]

ggplot(sub2,aes(x=factor(Hold.Out.Year),y=Kappa))+geom_boxplot()
