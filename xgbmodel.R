rm(list = ls())
######choose data for model#########
source("function.R")
load("processed_data.rds")

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

# Folds are created on the basis of target variable

###### get order of importance #########

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

ggplot(data=imp_plot,
       mapping=aes(x=reorder(Feature, -mean),y=mean,fill=Feature))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette = "RdBu") +
  #scale_fill_manual(values =wes_palette(10, name = "Royal1", type = "continuous"))+
  xlab(NULL)+
  ylab("Relative importancee")+
  theme_few()
ggsave("rr.tiff",dpi=300,scale=1.9)

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

###### model evaluation #########

####ROC
rocxg<-roc(data.validat.y,xgbpred)
plot(rocxg,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), 
     max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)

####variables importance
importance_matrix <- xgb.importance(model = model) 
xgb.plot.importance(importance_matrix, 
                    rel_to_first = TRUE, xlab = "Relative importance")

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

###### evaluate the early warning duration

data.frame(xgbpred,data.validat.y) %>%
  mutate(Y_N=ifelse(xgbpred==1&data.validat.y==1,1,0)) -> result_eva
model.variables$id[model.variables$date>=cut_date] %>%
  .[result_eva$Y_N==1] -> eva
eva <- apply(eva%>%as.matrix, 1, same_length)
data3$id <- apply(data3$id %>% as.matrix, 1, same_length)
data4$id <- apply(data4$id %>% as.matrix, 1, same_length)
early_pred <- NULL
day_back <- 0
set_window <- 300

dose_ref <- data.frame(EMB = model.variables$EMB/model.variables$Duration,
                       INH = model.variables$INH/model.variables$Duration,
                       PZA = model.variables$PZA/model.variables$Duration,
                       RFP = model.variables$RFP/model.variables$Duration,
                       SS = model.variables$SS/model.variables$Duration,
                       id = model.variables$id)

while (length(eva)>0){
  print(length(eva))
  ALT_result <- lapply(eva%>%as.list(),function(x){data3%>%
      filter(id==x)%>%select(c(id,alt_value,alt_yn,report_date,start_time))->
      out;return(out)})
  first_time <- lapply(eva%>%as.list(),function(x){data4%>%
      filter(id==x)%>%select(c(id,date))%>%arrange(date)->
      out;return(out[1,])}) 
  first_time_results <- Reduce("rbind",first_time)
  names(first_time_results) <- c("id","start_time")
  last_time <- lapply(ALT_result, dig_time_lable)
  last_time_results <- Reduce("rbind",last_time)
  last_time_results<-left_join(last_time_results[,-5],
                               first_time_results,by="id") 
  
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
    left_join(com.dat[,-c(1:5,21,22)],by="id") -> eva.dat
  
  med_dose <- lapply(eva%>%as.list(), get_dose)
  
  exact_dose_list <- lapply(med_dose, get_exact_dose_list)
  
  med_dose_sort <-lapply(exact_dose_list,sort_med)
  med_dose_sorts <- Reduce("rbind", med_dose_sort)
  
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
  
  eva.dat.v2 <- merge_dose(med_dose_exact) %>% 
    relocate(id) %>% select(-days_dif)
  col_temp <- names(eva.dat.v2)[2:dim(eva.dat.v2)[2]]
  eva.dat.v2[,col_temp] <- eva.dat.v2[,col_temp] -
    dose_ref[which(dose_ref$id%>%as.numeric %in% eva.dat.v2$id),
             col_temp]*day_back
  eva.dat.v2$id<-apply(eva.dat.v2$id%>%as.matrix,1,same_length)
  eva.dat.v2<-left_join(eva.dat.v2,eva.dat,by="id")
  early.end<-which((eva.dat.v2$Duration-day_back)<=0)%>%length()
  #print(eva.dat.v2$days_dif.x)
  if(early.end>0){
    eva<-eva[-which((eva.dat.v2$Duration-day_back)<=0)]
    eva.dat.v2<-eva.dat.v2[-which((eva.dat.v2$Duration-day_back)<=0),]
  }
  if(eva%>%length()>0){
    dtest <- xgb.DMatrix(data = eva.dat.v2[,feature_select]%>%as.matrix(),
                         label= eva.dat.v2$result%>%as.matrix())
    pred <- predict(model, dtest)
    xgbpred <- ifelse (pred >= 0.5,1,0)
    early_pred<-c(early_pred,rep(day_back,
                                 (which(xgbpred==0)%>%length()+early.end)))
  }else{
    early_pred<-c(early_pred,rep(day_back,early.end))
  }
  if(which(xgbpred==0)%>%length()>0)eva <- eva[-which(xgbpred==0)]
  #print(eva.dat.v2[1:5,1:4])
}
summary(early_pred)

tiff("pred_horizon.tiff",width = 1000,height = 700,units = "px", pointsize = 22)
hist(early_pred, breaks=25, xlim=c(0,70), col=rgb(1,0,0,0.5), ylim = c(0,15),
     xlab="days of early warning",ylab="number of TB-DILI cases", main=NULL)
dev.off()
summary(early_pred)
length(early_pred)

### visualize decision tree

grViz("
digraph box_and_circles {
graph [layout = dot;splines=line;
overlap = false;nodesep=0.3;rankdir=LR]

node [shape=box;penwidth=1.5;fixedsize = true;width = 1.2]
11 [label='ALT_updated']
22 [label='EMB']
21 [label='ALT_rate']
34 [label='ALT_updated']
33 [label='ALT_updated']
32 [label='PZA']
31 [label='ALT_updated']
41 [label='ALTt_rate']
42 [label='ALT_rate']
43 [label='ALT_updated']
44 [label='ALT_rate']
46 [label='PZA']
52 [label='EMB']
54 [label='EMB']
56 [label='PZA']
65 [label='EMB']
66 [label='ALT_updated']

node [shape = circle;fixedsize = true;width = 0.5]
45 [label='Yes' color=red]
47 [label='No']
48 [label='Yes'color=red]
51 [label='No']
53 [label='Yes'color=red]
55 [label='No']
57 [label='No']
58 [label='Yes'color=red]
59 [label='Yes'color=red]
510 [label='No']
61 [label='No']
62 [label='Yes'color=red]
63 [label='No']
64 [label='Yes'color=red]
71 [label='Yes'color=red]
72 [label='No']
73 [label='No']
74 [label='Yes'color=red]

11->21[label='< 25.5' color=gray];11->22[label='\u2265 25.5']
21->31[label='< 0.26' color=gray];21->32[label='\u2265 0.26']
22->33[label='< 90.38'];22->34[label='\u2265 90.38' color=gray]
31->41[label='< 19.5'color=gray];31->42[label='\u2265 19.5']
32->43[label='< 26.25'];32->44[label='\u2265 26.25'color=gray]
33->45[label='< 26.5'];33->46[label='\u2265 26.5'color=gray]
34->47[label='< 27.5'color=gray];34->48[label='\u2265 27.5']
41->51[label='< 0.16'color=gray];41->52[label='\u2265 0.16']
52->61[label='\u2265 81.34'color=gray];52->62[label='< 81.34']
42->53[label='< -0.11'color=gray];42->54[label='\u2265 -0.11']
54->63[label='\u2265 17.60'color=gray];54->64[label='< 17.60']
43->55[label='< 11.5'color=gray];43->56[label='\u2265 11.5']
56->65[label='\u2265 3.40'color=gray];56->66[label='< 3.40']
65->71[label='\u2265 1.58'color=gray];65->72[label='< 1.58']
66->73[label='< 22'color=gray];66->74[label='\u2265 22']
44->57[label='\u2265 0.28'color=gray];44->58[label='< 0.28']
46->59[label='\u2265 1.75'color=gray];46->510[label='< 1.75']
}")->treegraph
treegraph%>%
  export_svg %>% 
  charToRaw %>% 
  #rsvg_pdf("tree.pdf") %>%
  rsvg_png("tree.png",width = 6000, height = 3500)


data.train.x<-rbind(data.train.x,data.validat.x)
data.train.y<-c(data.train.y,data.validat.y)
save(data.train.x, data.train.y, file = "sen_data_0.rds")


