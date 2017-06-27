

load("data/raf_fd.RData")


##prepare for machine learning (keep only relevant and complete features)
source("prepare_ML_regression.R")
raf_fd_prep<- prepare(raf_fd)

###ML procedure

sapply(raf_fd_prep, function(y) sum(is.na(y))/length(y)) #check NA's


xnam <- colnames(raf_fd_prep)
xnam<- xnam[! xnam %in% c("leeftijd")]
fmla <- as.formula(paste("leeftijd ~ ", paste(xnam, collapse= "+")))
raf_fd_prep<- raf_fd_prep[complete.cases(raf_fd_prep$leeftijd),]


library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(h2o)
library(gbm)
library(xgboost)
#library(mlbench)

control <- trainControl(method="cv", number=5)

# train the GBM model

modelGbm <- train(fmla, data=raf_fd_prep, method="gbm_h2o", trControl=control, verbose=T,
                  na.action = na.omit)

modelXgb<- train(fmla, data=raf_fd_prep,method="xgbTree", trControl=control, verbose=T,
                na.action = na.omit)

# train the glm
modelGLM<- train(fmla, data=raf_fd_prep, method="glm", trControl=control, na.action = na.omit)

# train the DT model

modelTree <- train(fmla, data=raf_fd_prep, method="rpart", trControl=control,na.action = na.omit)

# collect resamples
results <- resamples(list(xgb=modelXgb,GBM=modelGbm,GLM=modelGLM, RF= modelRF,Tree=modelTree))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)

plotObsVsPred(extractPrediction( modelXgb))

