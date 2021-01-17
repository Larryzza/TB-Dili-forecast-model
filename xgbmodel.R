#rm(list = ls())
######choose data for model#########
source("function.R")

#com.dat <- read.csv("com.dat.v2.csv")[,-1]
com.dat <- read.csv("com.dat.v4.csv")[,-1]
head(com.dat)

com.dat$date%<>%as.Date()
#check data set

cut_date<-"2019-4-1"
com.dat$result[com.dat$date>=cut_date]%>%table()%>%prop.table()
com.dat$result[com.dat$date<cut_date]%>%table()%>%prop.table()

#com.dat$gender%<>%as.numeric();com.dat$hepatitis.B%<>%as.numeric()
model.variables <- com.dat %>% 
  select(-c(edu,result,plan)) %>% 
  cbind(model.matrix(~edu-1, model.frame(~edu-1, com.dat, na.action=na.pass)))%>%
  select(-c(eduedu.1))
model.y <- com.dat$result %>% as.numeric()

data.train.x<-model.variables[model.variables$date<cut_date,] %>% select(-c(id,date,Duration,days_dif))
data.train.y<-model.y[model.variables$date<cut_date]
data.validat.x<-model.variables[model.variables$date>=cut_date,] %>% select(-c(id,date,Duration,days_dif))
data.validat.y<-model.y[model.variables$date>=cut_date]


# Folds are created on the basis of target variable

###### get importance #########

importance_combine <- NULL
for(i in 1:500){
  set.seed(1e7-i)
  folds <- createFolds(factor(data.train.y), k = 10, list = T)
  importance_combine <- rbind(importance_combine,
                              model_function_Total(importance=T,auc=F))
  print(paste0((i/5)%>%round(),"%"))
}

importance_combine %>%
  group_by(Feature) %>%
  summarise(mean=mean(Gain)) %>%
  arrange(desc(mean)) -> importance_sum

write.csv(importance_sum,"importance_sum.csv")
imp_plot <- read.csv("importance_sum.csv")[,-1]
imp_plot <- imp_plot[1:10,]
imp_plot$Feature%<>%as.factor()

getPalette = colorRampPalette(brewer.pal(10, "Set1"))
ggplot(data=imp_plot,mapping=aes(x=reorder(Feature, -mean),y=mean,fill=Feature))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette = "RdBu") +
  #scale_fill_manual(values =wes_palette(10, name = "Royal1", type = "continuous"))+
  xlab(NULL)+
  ylab("Relative Rmpotortance")+
  theme_few()
ggsave("rr.tiff",dpi=300,scale=1.3)
###### feature select #########

feature_select<-NULL;auc.comb<-NULL

for(f in 1:10) {
  auc_value_combine<-NULL
  feature_select<-importance_sum$Feature[1:f]
  data.new <- data.train.x[,feature_select]%>%as.data.frame()
  
  for(i in 1:100){
    set.seed(1e7-i)
    folds <- createFolds(factor(data.train.y), k = 10, list = T)
    auc_value_combine <- model_function_Total(importance=F,auc=T)
    print(paste0(f," variables - ",i,"%"))
  }
  auc_new <- auc_value_combine %>% mean()
  auc_sd <- auc_value_combine %>% sd()
  auc.comb <- rbind(auc.comb,data.frame(auc_new,auc_sd,model=f))
}
which.max(auc.comb$auc_new)
write.csv(auc.comb,"auc.comb.csv")
write.csv(feature_select[1:which.max(auc.comb$auc_new)],"feature_select.csv")


###### prepare train/test data set #########
feature_select <- read.csv("feature_select.csv")[,2]%>%as.character()

dtrain <- xgb.DMatrix(data = data.train.x[,feature_select]%>%as.matrix(),
                      label= data.train.y%>%as.matrix())
dtest <- xgb.DMatrix(data = data.validat.x[,feature_select]%>%as.matrix(),
                     label= data.validat.y%>%as.matrix())

######### model ref without Tuning##########
#single tree
model <- xgboost(data = dtrain,          
                 #max.depth = 4, 
                 nround = 1,
                 #gamma = 1,
                 objective = "binary:logistic", 
                 verbose = 0)
pred <- predict(model, dtest)
xgbpred <- ifelse (pred >= 0.5,1,0)
caret::confusionMatrix (xgbpred%>%as.factor(), data.validat.y%>%as.factor())
rocxg<-roc(data.validat.y,xgbpred)
plot(rocxg,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), 
     max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)
xgb.plot.tree(model = model)

pred.o <- predict(model, dtrain)
xgbpred.o <- ifelse (pred.o >= 0.5,1,0)
caret::confusionMatrix (xgbpred.o%>%as.factor(), data.train.y%>%as.factor())
###### model evaluation #########
data.frame(xgbpred,data.validat.y) %>%
  mutate(Y_N=ifelse(xgbpred==1&data.validat.y==1,1,0)) -> result_eva
model.variables$id[model.variables$date>=cut_date] %>% .[result_eva$Y_N==1] -> eva
eva <- apply(eva%>%as.matrix, 1, same_length)
early_pred <- NULL
day_back <- 0

while (length(eva)>0){
  print(length(eva))
  ALT_result <- lapply(eva%>%as.list(),function(x){data3%>%
      filter(id==x)%>%select(c(id,alt_value,alt_yn,report_date,start_time))->
      out;return(out)})
  first_time <- lapply(eva%>%as.list(),function(x){data4%>%
      filter(登记号...1==x)%>%select(c(登记号...1,日期))%>%arrange(日期)->
      out;return(out[1,])}) 
  first_time_results <- Reduce("rbind",first_time)
  names(first_time_results) <- c("id","start_time")
  last_time <- lapply(ALT_result, dig_time_lable)
  last_time_results <- Reduce("rbind",last_time)
  last_time_results<-left_join(last_time_results[,-5],first_time_results,by="id") 
  
  day_back <- day_back+1
  last_time_results$report_date <- last_time_results$report_date-day_back
  
  cut_off <- 1
  ALT_results <- lapply(ALT_result, dig_alts)
  ALT_results <- Reduce("rbind",ALT_results)
  
  ALT_results %<>%
    filter(is.na(id)==F) %<>%
    filter(is.na(near)==F) %<>%
    select(-test_date)
  colnames(ALT_results)<-c("id","alt_updated","alt_rate")
  
  data.frame(id=eva) %>%
    left_join(ALT_results,by="id") %>%
    left_join(com.dat.v2,by="id") -> eva.dat
  
  med_dose <- lapply(eva%>%as.list(), get_dose)
  
  exact_dose_list <- lapply(med_dose, get_exact_dose_list)
  
  med_dose_sort <-lapply(exact_dose_list,sort_med)
  med_dose_sorts <- Reduce("rbind",med_dose_sort)
  
  med_dose_sorts<-med_dose_sort[[1]]
  if(length(med_dose_sort)>1){
    for(i in 1:(length(med_dose_sort)-1)){
      med_dose_sorts<-full_join(med_dose_sorts,med_dose_sort[[i+1]],by = "name")
    }
  }
  #dim(med_dose_sorts)
  med_dose_exact<-t(med_dose_sorts)%>%as.data.frame()
  med_dose_exact<-med_dose_exact[-1,]
  colnames(med_dose_exact)<-med_dose_sorts[,1]%>%as.character()
  rownames(med_dose_exact)<-1:dim(med_dose_exact)[1]
  
  med_dose_exact <- apply(med_dose_exact%>%as.matrix, 2, 
                          function(x){x[which(is.na(x)==T)]<-0;return(x)})
  if(length(med_dose_sort)==1)med_dose_exact<-t(med_dose_exact)
  med_dose_exact%<>%as.data.frame()
  
  eva.dat.v2<-merge_dose(med_dose_exact)
  eva.dat.v2$id<-apply(eva.dat.v2$id%>%as.matrix,1,same_length)
  eva.dat.v2<-left_join(eva.dat.v2,eva.dat[,-c(13:16)],by="id")
  early.end<-which(apply(eva.dat.v2%>%select(c(PZA,RFP,EMB,INH)),1,sum)==0)%>%length()
  if(early.end>0){
    eva<-eva[-which(apply(eva.dat.v2%>%select(c(PZA,RFP,EMB,INH)),1,sum)==0)]
    eva.dat.v2<-eva.dat.v2[-which(apply(eva.dat.v2%>%select(c(PZA,RFP,EMB,INH)),1,sum)==0),]
  }
  if(eva%>%length()>0){
    dtest <- xgb.DMatrix(data = eva.dat.v2[,feature_select]%>%as.matrix(),
                         label= eva.dat.v2$result%>%as.matrix())
    pred <- predict(model, dtest)
    xgbpred <- ifelse (pred >= 0.5,1,0)
    early_pred<-c(early_pred,rep(day_back,(which(xgbpred==0)%>%length()+early.end)))
  }else{
    early_pred<-c(early_pred,rep(day_back,early.end))
  }
  if(which(xgbpred==0)%>%length()>0)eva <- eva[-which(xgbpred==0)]
}
tiff("pred_horizon.tiff",width = 1000,height = 700,units = "px", pointsize = 22)
hist(early_pred, breaks=25, xlim=c(0,100), col=rgb(1,0,0,0.5), ylim = c(0,15),
     xlab="days of early warning",ylab="number of TB-DILI cases", main=NULL)
dev.off()
summary(early_pred)
length(early_pred)

rocxg<-roc(data.validat.y,xgbpred)
plot(rocxg,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), 
     max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)

importance_matrix <- xgb.importance(model = model) 
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")

feature_values <- data.validat.x[,feature_select] %>%
  as.data.frame() %>%
  mutate_all(scale) %>%
  gather(feature, feature_value) %>% 
  pull(feature_value)
shap_df <- model %>%
  predict(newdata = dtest, predcontrib = TRUE) %>%
  as.data.frame() %>%
  select(-BIAS) %>%
  gather(feature, shap_value) %>%
  mutate(feature_value = feature_values) %>%
  group_by(feature) %>%
  mutate(shap_importance = mean(abs(shap_value)))
ggplot(shap_df, 
       aes(x = shap_value, 
           y = reorder(feature, shap_importance))) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, 
                               size = 0.4, alpha = 0.25) +
  xlab("SHAP value") +
  ylab(NULL)



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
    max_depth = 4,
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

#control <- makeTuneControlRandom(maxit = 5000)
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




set.seed(9994040)
#single tree
model <- xgboost(data = dtrain,          
                 max.depth = 4, 
                 nround = 1, 
                 eta = 0.3666667,
                 objective = "binary:logistic", 
                 gamma = 1.111111,
                 subsample = 0.9444444,
                 verbose = 0)
pred <- predict(model, dtest)
xgbpred <- ifelse (pred >= 0.5,1,0)
caret::confusionMatrix (xgbpred%>%as.factor(), data.validat.y%>%as.factor())
rocxg<-roc(data.validat.y,xgbpred)
plot(rocxg,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), 
     max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)
xgb.plot.tree(model = model)





########---compare auc---##########
library(mice)
library(ROCR)
library(e1071)
library("pROC")
library(caret)
miss <- function(x){sum(is.na(x))/length(x)*100}
apply(data.train.x,2,miss)

data.train.x<-model.variables[model.variables$date<cut_date,] %>% select(-c(id,date,Duration,days_dif))
data.train.y<-model.y[model.variables$date<cut_date]
data.validat.x<-model.variables[model.variables$date>=cut_date,] %>% select(-c(id,date,Duration,days_dif))
data.validat.y<-model.y[model.variables$date>=cut_date]

data.train.x<-rbind(data.train.x,data.validat.x)
data.train.y<-c(data.train.y,data.validat.y)

#data for glm
tempData <- mice(data.train.x,m=5,maxit=5,meth='pmm')
imputed <- complete(tempData)

A<-NULL;B<-NULL;C<-NULL;D<-NULL;E<-NULL;Ff<-NULL
for(i in 1:300){
  print(paste0(i,"%"))
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
                     max.depth = 4, 
                     nround = 1, 
                     objective = "binary:logistic", 
                     verbose = 0)
    pred <- predict(model, dtest)
    return(data.frame(pred,test_labels))})
  b<-Reduce("rbind",b)
  B<-rbind(B,b)
  
  set.seed(1e7-i)
  c<-lapply(folds, function(x){
    dtrain <- xgb.DMatrix(data = data.train.x[-x,feature_select]%>%as.matrix(),
                          label= data.train.y[-x]%>%as.matrix())
    dtest <- xgb.DMatrix(data = data.train.x[x,feature_select]%>%as.matrix(),
                         label= data.train.y[x]%>%as.matrix())
    test_labels <- data.train.y[x]
    model <- xgboost(data = dtrain,          
                     max.depth = 4, 
                     nround = 100, 
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
  
  set.seed(1e7-i)
  f<-lapply(folds, function(x){
    dtrain <- xgb.DMatrix(data = data.train.x[-x,feature_select]%>%as.matrix(),
                          label= data.train.y[-x]%>%as.matrix())
    dtest <- xgb.DMatrix(data = data.train.x[x,feature_select]%>%as.matrix(),
                         label= data.train.y[x]%>%as.matrix())
    test_labels <- data.train.y[x]
    model <- xgboost(data = dtrain,          
                     max.depth = 4, 
                     nround = 1, 
                     eta = 0.3,
                     objective = "binary:logistic", 
                     gamma = 3.448276,
                     min_child_weight = 1.827586,
                     subsample = 0.8275862,
                     verbose = 0)
    pred <- predict(model, dtest)
    return(data.frame(pred,test_labels))})
  f<-Reduce("rbind",f)
  Ff<-rbind(Ff,f)}

rocglm<-roc(A$test_labels,A$p)
rocxgs<-roc(B$test_labels,B$p)
rocxg<-roc(C$test_labels,C$p)
rocrfs<-roc(D$test_labels,D$p)
rocrf<-roc(E$test_labels,E$p)
rocxgst<-roc(Ff$test_labels,Ff$p)

plot(rocglm,col = "dark blue",lty=1,lwd=2)
plot(rocxg,add = TRUE,col = "orange",lty=1,lwd=2)
plot(rocxgs,add = TRUE,col = "dark red",lty=1,lwd=2)
plot(rocrfs,add = TRUE,col = "pink",lty=1,lwd=2)
plot(rocrf,add = TRUE,col = "dark green",lty=1,lwd=2)
legend("bottomright",legend=c("xgboost(aic:0.9274)",
                              "single tree xgboost(aic:0.9009)",
                              "GLM(aic:0.8679)",
                              "random forests(aic:0.8796)",
                              "single tree random forests(aic:0.7553)"),
       col=c("orange","dark red","dark blue","dark green","pink"),
       lty=1,lwd=2,cex=0.8,bty="n")
dev.off()
