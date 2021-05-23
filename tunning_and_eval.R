
############# following parts include 
############# 1) 2 tuning functions (optinal)
############# 2) function to compare performance with other models 
rm(list = ls())
######choose data for model#########
source("function.R")
load("processed_data.rds")

###### prepare data


head(com.dat)
com.dat$date%<>%as.Date()
com.dat$id <- apply(com.dat$id %>% as.matrix, 1, same_length)
#check data set

cut_date<-"2019-4-1"
com.dat$result[com.dat$date>=cut_date]%>%table()%>%prop.table()
com.dat$result[com.dat$date<cut_date]%>%table()%>%prop.table()

#com.dat$gender%<>%as.numeric();com.dat$hepatitis.B%<>%as.numeric()
model.variables <- com.dat %>% 
  select(-c(edu,result,plan)) %>% 
  cbind(model.matrix(~edu-1, 
                     model.frame(~edu-1, com.dat, na.action=na.pass)))%>%
  select(-c(eduedu.1))
model.y <- com.dat$result %>% as.numeric()

data.train.x<-model.variables[model.variables$date<cut_date,] %>% 
  select(-c(id,date,Duration,days_dif))
data.train.y<-model.y[model.variables$date<cut_date]
data.validat.x<-model.variables[model.variables$date>=cut_date,] %>% 
  select(-c(id,date,Duration,days_dif))
data.validat.y<-model.y[model.variables$date>=cut_date]

###### prepare train/test data set #########

feature_select <- read.csv("feature_select.csv")[,2]%>%as.character()

dtrain <- xgb.DMatrix(data = data.train.x[,feature_select]%>%as.matrix(),
                      label= data.train.y%>%as.matrix())
dtest <- xgb.DMatrix(data = data.validat.x[,feature_select]%>%as.matrix(),
                     label= data.validat.y%>%as.matrix())


######### model (without Tuning) ##########
#single tree
model <- xgboost(data = dtrain,          
                 #max.depth = 3, 
                 nround = 1,
                 #gamma = 1,
                 eval_metric = "logloss",
                 objective = "binary:logistic", 
                 verbose = 0)

#results
save(model, file = "model.rds")
pred <- predict(model, dtest)
xgbpred <- ifelse (pred >= 0.5,1,0)
caret::confusionMatrix (xgbpred%>%as.factor(), data.validat.y%>%as.factor())
xgb.plot.tree(model = model)

pred.o <- predict(model, dtrain)
xgbpred.o <- ifelse (pred.o >= 0.5,1,0)
caret::confusionMatrix (xgbpred.o%>%as.factor(), data.train.y%>%as.factor())

###### Bayesian Tuning (optional) #########

set.seed(7654321)
folds <- createFolds(factor(data.train.y), k = 10, list = T)
bounds <- list(  
  subsample = c(0.3, 1)
)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
clusterExport(cl,c('folds','data.train.x',
                   'data.train.y','feature_select'))
clusterEvalQ(cl,expr= {
  library(xgboost)
  library(tidyverse)
})

tWithPar <- system.time(
  optObj <- bayesOpt(
    FUN = scoringFunction
    , bounds = bounds
    , initPoints = 100
    , iters.n = 32
    , iters.k = 8
    , parallel = TRUE
  )
)
stopCluster(cl)
registerDoSEQ()
getBestPars(optObj)

Pars <- list( 
  booster = "gbtree"
  , eta = 0.3
  , max_depth = 3
  , subsample = getBestPars(optObj)%>%unlist()
  , objective = "binary:logistic"
  , eval_metric = "auc")

set.seed(19960911)
seee<-NULL
for(i in 1:10000){
  xgb <- xgb.train(
    params = Pars
    , data = dtrain
    , nround = 1
    , verbose = 0)
  
  #saveRDS(xgb,file = "xgb.rds")
  #xgb <- readRDS("xgb.rds")
  
  pred <- predict(xgb, dtest)
  xgbpred <- ifelse (pred >= 0.5,1,0)
  see<-confusionMatrix(xgbpred%>%as.factor(), data.validat.y%>%as.factor())
  xgb.plot.tree(model = xgb)
  seee<-c(see$overall[1],seee)
  print(i/10000)
}
seee%>%summary()


###### alternative grid Tuning method (optional 2)#####
df1<-data.frame(y=data.train.y%>%as.factor(),data.train.x[,feature_select])
trainTask <- makeClassifTask(data = df1,target = "y", positive = 1)
df2<-data.frame(y=data.validat.y%>%as.factor(),data.validat.x[,feature_select])
testTask <- makeClassifTask(data = df2,target = "y")

set.seed(199609)
# Create an xgboost learner that is classification based and outputs
# labels (as opposed to probabilities)
xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "response",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 1,
    max_depth = 3,
    eta = 0.3
  )
)


xgb_model <- train(xgb_learner, task = trainTask)

getParamSet("classif.xgboost")
xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  #makeIntegerParam("nrounds", lower = 1, upper = 1),
  # number of splits in each tree
  #makeIntegerParam("max_depth", lower = 2, upper = 3),
  # "shrinkage" - prevents overfitting
  #makeNumericParam("eta", lower = .1, upper = .4),
  makeNumericParam("gamma", lower = 0, upper = 10),
  makeNumericParam("min_child_weight", lower = 1, upper = 25),
  makeNumericParam("subsample", lower = .5, upper = 1)
)

control <- makeTuneControlGrid(resolution = 30)

# Create a description of the resampling plan
resample_desc <- makeResampleDesc("CV",stratify = T,iters = 10)

tuned_params <- tuneParams(
  learner = xgb_learner,
  task = trainTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)

# Create a new model using tuned hyperparameters
xgb_tuned_learner <- setHyperPars(
  learner = xgb_learner,
  par.vals = tuned_params$x
)
set.seed(10e7)
# Re-train parameters using tuned hyperparameters (and full training set)
xgb_model <- train(xgb_tuned_learner, trainTask)
# Make a new prediction
pred <- predict(xgb_model, testTask)
caret::confusionMatrix (pred$data$response, data.validat.y%>%as.factor())


########---compare auc---##########
library(mice)
library(ROCR)
library(e1071)
library("pROC")
library(caret)
miss <- function(x){sum(is.na(x))/length(x)*100}
na_table <- apply(com.dat,2,miss)
write.csv(na_table,"na_table.csv")

data.train.x<-model.variables[model.variables$date<cut_date,] %>% 
  select(-c(id,date,Duration,days_dif))
data.train.y<-model.y[model.variables$date<cut_date]
data.validat.x<-model.variables[model.variables$date>=cut_date,] %>% 
  select(-c(id,date,Duration,days_dif))
data.validat.y<-model.y[model.variables$date>=cut_date]

data.train.x<-rbind(data.train.x,data.validat.x)
data.train.y<-c(data.train.y,data.validat.y)

#data for glm
tempData <- mice(data.train.x,m=5,maxit=5,meth='pmm')
imputed <- complete(tempData)

A<-NULL;B<-NULL;C<-NULL;D<-NULL;E<-NULL;Ff<-NULL
for(i in 0){
  #print(paste0(i*2,"%"))
  set.seed(1e7-i)
  folds <- createFolds(factor(data.train.y), k = 10, list = T)
  
  set.seed(1e7-i)
  a<-lapply(folds, function(x){
    dtrain <- data.frame(data = imputed[-x,]%>%as.matrix(),
                         label= data.train.y[-x]%>%as.matrix())
    dtest <- data.frame(data = imputed[x,]%>%as.matrix(),
                        label= data.train.y[x]%>%as.matrix())
    test_labels <- data.train.y[x]
    m<-glm(label ~.,data = dtrain, family = "binomial")
    p<-predict(m,newdata=dtest,type="response")
    return(data.frame(p,test_labels))})
  a<-Reduce("rbind",a)
  A<-rbind(A,a)
  
  set.seed(1e7-i)
  b<-lapply(folds, function(x){
    dtrain <- xgb.DMatrix(data = data.train.x[-x,feature_select]%>%as.matrix(),
                          label= data.train.y[-x]%>%as.matrix())
    dtest <- xgb.DMatrix(data = data.train.x[x,feature_select]%>%as.matrix(),
                         label= data.train.y[x]%>%as.matrix())
    test_labels <- data.train.y[x]
    model <- xgboost(data = dtrain,          
                     #max.depth = 3, 
                     nround = 1, 
                     eval_metric = "logloss",
                     objective = "binary:logistic", 
                     verbose = 0)
    pred <- predict(model, dtest)
    return(data.frame(pred,test_labels))})
  b<-Reduce("rbind",b)
  B<-rbind(B,b)
  
  set.seed(1e7-i)
  c<-lapply(folds, function(x){
    dtrain <- xgb.DMatrix(data = data.train.x[-x,]%>%as.matrix(),
                          label= data.train.y[-x]%>%as.matrix())
    dtest <- xgb.DMatrix(data = data.train.x[x,]%>%as.matrix(),
                         label= data.train.y[x]%>%as.matrix())
    test_labels <- data.train.y[x]
    model <- xgboost(data = dtrain,          
                     #max.depth = 3, 
                     nround = 100, 
                     eval_metric = "logloss",
                     objective = "binary:logistic", 
                     verbose = 0)
    pred <- predict(model, dtest)
    return(data.frame(pred,test_labels))})
  c<-Reduce("rbind",c)
  C<-rbind(C,c)
  
  set.seed(1e7-i)
  d<-lapply(folds, function(x){
    dtrain <- data.frame(data = imputed[-x,]%>%as.matrix(),
                         label= data.train.y[-x]%>%as.matrix())
    dtest <- data.frame(data = imputed[x,]%>%as.matrix(),
                        label= data.train.y[x]%>%as.matrix())
    test_labels <- data.train.y[x]
    m <- randomForest(label ~  .,
                      data = dtrain,
                      ntree =1,
                      mtry=3,
                      importance=TRUE ,
                      proximity=TRUE)
    p<-predict(m,newdata=dtest,type="response")
    return(data.frame(p,test_labels))})
  d<-Reduce("rbind",d)
  D<-rbind(D,d)
  
  set.seed(1e7-i)
  e<-lapply(folds, function(x){
    dtrain <- data.frame(data = imputed[-x,]%>%as.matrix(),
                         label= data.train.y[-x]%>%as.matrix())
    dtest <- data.frame(data = imputed[x,]%>%as.matrix(),
                        label= data.train.y[x]%>%as.matrix())
    test_labels <- data.train.y[x]
    m <- randomForest(label ~  .,
                      data = dtrain,
                      ntree =300,
                      mtry=3,
                      importance=TRUE ,
                      proximity=TRUE)
    p<-predict(m,newdata=dtest,type="response")
    return(data.frame(p,test_labels))})
  e<-Reduce("rbind",e)
  E<-rbind(E,e)
  
  'set.seed(1e7-i)
  f<-lapply(folds, function(x){
    dtrain <- xgb.DMatrix(data = data.train.x[-x,feature_select]%>%as.matrix(),
                          label= data.train.y[-x]%>%as.matrix())
    dtest <- xgb.DMatrix(data = data.train.x[x,feature_select]%>%as.matrix(),
                         label= data.train.y[x]%>%as.matrix())
    test_labels <- data.train.y[x]
    model <- xgboost(data = dtrain,          
                     max.depth = 3, 
                     nround = 1, 
                     eta = 0.3,
                     objective = "binary:logistic", 
                     gamma = 4.482759,
                     min_child_weight = 1.827586,
                     subsample = 0.9655172,
                     verbose = 0)
    pred <- predict(model, dtest)
    return(data.frame(pred,test_labels))})
  f<-Reduce("rbind",f)
  Ff<-rbind(Ff,f)'}

rocglm<-roc(A$test_labels,A$p,smooth=T)
rocxgs<-roc(B$test_labels,B$p,smooth=T)
rocxg<-roc(C$test_labels,C$p,smooth=T)
rocrfs<-roc(D$test_labels,D$p,smooth=T)
rocrf<-roc(E$test_labels,E$p,smooth=T)
#rocxgst<-roc(Ff$test_labels,Ff$p)

ci.auc(A$test_labels,A$p)
ci.auc(B$test_labels,B$p)
ci.auc(C$test_labels,C$p)
ci.auc(D$test_labels,D$p)
ci.auc(E$test_labels,E$p)

tiff("compare_model.tiff",width = 1000,height = 1000,units = "px", pointsize = 22)
plot(rocglm,col = "dark blue",lty=1,lwd=2)
plot(rocxg,add = TRUE,col = "orange",lty=1,lwd=2)
plot(rocxgs,add = TRUE,col = "dark red",lty=1,lwd=2)
plot(rocrfs,add = TRUE,col = "pink",lty=1,lwd=2)
plot(rocrf,add = TRUE,col = "dark green",lty=1,lwd=2)
legend("bottomright",legend=c("XGBoost (AUC:0.940 (0.924,0.956))",
                              "Single tree XGBoost (AUC:0.912 (0.890,0.935))",
                              "Random forests (AUC:0.876 (0.852,0.901))",
                              "Generalized linear model (AUC:0.861 (0.842,893))",
                              "Single tree random forests (AUC:0.783 (0.738,0.803))"),
       col=c("orange","dark red","dark green","dark blue","pink"),
       lty=1,lwd=2,cex=0.9,bty="n")
dev.off()
