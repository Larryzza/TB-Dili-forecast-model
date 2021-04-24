#####----load library---######
library(readxl)
library(tidyverse)
library(dplyr)
library(tableone)
library(magrittr)
library(xgboost)
library(caret)
library(pROC)
library(mlr)
library(iml)
library(DiagrammeR)
#library(xgboostExplainer)
library(ParBayesianOptimization)
library(doParallel)
library(ggbeeswarm)
library(ggthemes)
library(wesanderson)
library(RColorBrewer)
library(randomForest)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
#####----date processing---######
date_function<-function(input){
  input$date[grepl("-",input$date)==F] <- input$date[grepl("-",input$date)==F] %>% 
    as.numeric() %>% as.Date(origin="1899-12-30")
  input$date[grepl("-",input$date)==T] <- input$date[grepl("-",input$date)==T] %>%
    as.Date("%Y-%m-%d")
  input$date%<>%as.numeric() %<>%as.Date(origin = "1970-01-01")
  return(input)
}

same_length <- function(x){
  output<-paste0(rep(0,(10-str_length(x)))%>%
                   paste0(.,collapse = ""),x)
  return(output)
}

dig_alts<-function(see){
  #print(see$id[1])
  if(length(see$id)>0){
    see$start_time <- first_time_results$start_time[which(first_time_results$id==see$id[1])]
    see$report_date %<>% as.Date()
    see$start_time %<>% as.Date()
    see %<>% filter((see$report_date-see$start_time)<=set_window)
    id <- see$id[1]
    test_date <- NA
    if(which(is.na(see$alt_yn)==F)%>%length()>0){
      temp.cut <- see$report_date[is.na(see$alt_yn)==F][1]-cut_off
      see <- filter(see,report_date<=temp.cut)
      if(length(see$id)>=2){
        increase<-see$alt_value[length(see$id)]-
          see$alt_value[length(see$id)-1]
        duration.temp<-see$report_date[length(see$id)]-
          see$report_date[length(see$id)-1]
        rate <- increase/(duration.temp%>%as.numeric())
        near <- see$alt_value[length(see$id)]
        test_date <- see$report_date[length(see$report_date)]
      }else if(length(see$id)==1){
        near <- see$alt_value[1];rate <- NA;test_date<-see$report_date[1] 
      }else{
        near <- NA;rate <- NA
      }
    }else{
      if(length(see$id)>=2){
        increase<-see$alt_value[length(see$id)]-
          see$alt_value[length(see$id)-1]
        duration.temp<-see$report_date[length(see$id)]-
          see$report_date[length(see$id)-1]
        rate <- increase/(duration.temp%>%as.numeric())
        near <- see$alt_value[length(see$id)]
        test_date <- see$report_date[length(see$report_date)]
      }else if(length(see$id)==1){
        near <- see$alt_value[1];rate <- NA;test_date<-see$report_date[1]
      }else{
        near <- NA;rate <- NA
      }
    }
  }else{near <- NA;rate <- NA;id <- NA}
  return(data.frame(id=id,near=near,rate=rate,test_date=test_date))
}

dig_time_lable<-function(see){
  #print(see$id[1])
  see$start_time <- first_time_results$start_time[which(first_time_results$id==see$id[1])]
  see$report_date %<>% as.Date()
  see$start_time %<>% as.Date()
  see %<>% filter((see$report_date-see$start_time)<=set_window)
  if(length(see$id)>1){
    if(which(is.na(see$alt_yn)==F)%>%length()>0){
      out<-see[which(is.na(see$alt_yn)==F)[1],]
    }else{
      out<-see[length(see$id),]
    }
  }else if(length(see$id)==1){
    out<-see
  }
  return(out)
}

get_dose<-function(see){
  #print(see)
  test_day <- last_time_results[which(last_time_results$id==see),]%>%select(-id)
  if(test_day$report_date>test_day$start_time){
    med_result %>% 
      filter(id==see,sign_date<test_day$report_date) %>% 
      cbind(test_day) -> select_med
  }else{
    med_result %>% 
      filter(id==see,sign_date==test_day$start_time) %>% 
      cbind(test_day) -> select_med
    select_med$dose <- 0
  }
  return(select_med)
}

get_exact_dose <- function(x){
  x[2] %>% as.Date() -> sign_date
  x[9] %>% as.Date() -> report_date
  x[6] %>% as.numeric() -> length
  x[4] %>% as.numeric() -> dose
  if(sign_date+length<=report_date){
    real_take <- dose
  }else{
    ratio <- ((report_date-sign_date)/length)%>%as.numeric()
    real_take <- dose*ratio
  }
  out<-data.frame(t(x),real_take=real_take)
  return(out)}

get_exact_dose_list<-function(see){
  see$report_date %<>% as.Date()
  see %<>% arrange(desc(sign_date))
  exact_dose<-apply(see,1,get_exact_dose)
  exact_dose<-Reduce("rbind",exact_dose)
}

sort_med<-function(see){
  duration<-(see$report_date[1]%>%
               as.Date()-see$sign_date%>%
               as.Date()%>%min())%>%as.numeric()
  id<-see$id[1]
  see %>% 
    group_by(type) %>% 
    summarise(dose=sum(real_take)) %>%
    spread(type,dose) %>%
    mutate(id=id,days_dif=duration)-> meduse
  return(data.frame(name=colnames(meduse),value=t(meduse)))
}

merge_dose<-function(med_dose_exact){
  med <- apply(med_dose_exact%>%select(-c(id,days_dif))%>%as.matrix,1, 
               function(x){x%>%as.character()%>%as.numeric()->out;return(out)})
  med <- t(med) %>% as.data.frame()
  if(dim(med)[1]==1) med<-t(med)%>% as.data.frame()
  remove_ind<-c(which(names(med_dose_exact)%>%str_detect("id")==T),
                which(names(med_dose_exact)%>%str_detect("days_dif")==T))
  colnames(med)<-names(med_dose_exact)[-remove_ind]
  med %<>%
    mutate(id=med_dose_exact$id,
           days_dif=med_dose_exact$days_dif%>%as.character()%>%as.numeric())
  com.dat.v4 <- med
  add_list <- NULL
  med_name <- c("PZA","RFP", "EMB", "INH", "INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg", 
                "INH80mg_RFP120mg_PZA250mg", "INH75mg_RFP150mg_PZA400mg_EMB275mg",
                "INH150mg_RFP300mg")
  for (i in med_name) {
    if((i %in% names(com.dat.v4))==F)add_list<-c(add_list,i)
  }
  add_zerocol<-matrix(data = 0, nrow = dim(com.dat.v4)[1], 
                      ncol = length(add_list))%>%as.data.frame()
  names(add_zerocol)<-add_list
  com.dat.v4 <- cbind(com.dat.v4,add_zerocol)
  com.dat.v4$PZA <- com.dat.v4$PZA + 
    com.dat.v4$INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg*200/450 +
    com.dat.v4$INH80mg_RFP120mg_PZA250mg*250/450 + 
    com.dat.v4$INH75mg_RFP150mg_PZA400mg_EMB275mg*400/900
  com.dat.v4$RFP <- com.dat.v4$RFP + 
    com.dat.v4$INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg*75/450 +
    com.dat.v4$INH80mg_RFP120mg_PZA250mg*120/450 + 
    com.dat.v4$INH75mg_RFP150mg_PZA400mg_EMB275mg*150/900 +
    com.dat.v4$INH150mg_RFP300mg*300/450
  com.dat.v4$EMB <- com.dat.v4$EMB + 
    com.dat.v4$INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg*137.5/450 +
    com.dat.v4$INH75mg_RFP150mg_PZA400mg_EMB275mg*275/900
  com.dat.v4$INH <- com.dat.v4$INH + 
    com.dat.v4$INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg*37.5/450 +
    com.dat.v4$INH80mg_RFP120mg_PZA250mg*80/450 + 
    com.dat.v4$INH75mg_RFP150mg_PZA400mg_EMB275mg*75/900+
    com.dat.v4$INH150mg_RFP300mg*150/450
  com.dat.v4 %<>% 
    select(-c(INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg,
              INH80mg_RFP120mg_PZA250mg,
              INH75mg_RFP150mg_PZA400mg_EMB275mg,
              INH150mg_RFP300mg))
  com.dat.v4$id %<>% as.character() %<>% as.numeric()
  return(com.dat.v4)
}

###### model function ########

importance_function<-function(dtrain){
  model <- xgboost(data = dtrain, # the data           
                   #max.depth = 4, # the maximum depth of each decision tree
                   nround = 1, # number of boosting rounds
                   eval_metric = 'logloss',
                   objective = "binary:logistic", # the objective function
                   verbose = 0)
  importance_matrix <- xgb.importance(model = model) 
  return(importance_matrix)
}

auc_function_train<-function(folds){
  dtrain <- xgb.DMatrix(data = data.new[-folds,]%>%as.matrix(),
                        label= data.train.y[-folds]%>%as.matrix())
  dtest <- xgb.DMatrix(data = data.new[folds,]%>%as.matrix(),
                       label= data.train.y[folds]%>%as.matrix())
  test_labels <- data.train.y[folds]
  
  model <- xgboost(data = dtrain,          
                   #max.depth = 4, 
                   nround = 1, 
                   eval_metric = 'logloss',
                   objective = "binary:logistic", 
                   verbose = 0)
  pred <- predict(model, dtest) 
  xgbpred <- ifelse (pred >= 0.5,1,0)
  error.rate <- confusionMatrix (xgbpred%>%as.factor(), 
                                 data.train.y[folds]%>%as.factor())
  roc_l <- roc(test_labels,pred)
  auc_value <- auc(roc_l)
  return(auc_value)
  #return(error.rate$overall[1])
}

model_function_Total<-function(importance=F,auc=T){
  if(importance==T){
    cv.group.x<-lapply(folds,function(x){out<-NULL;
    out<-rbind(out,data.frame(label=data.train.y[-x],data.train.x[-x,]))})
    cv.group.y<-lapply(folds,function(x){out<-NULL;
    out<-rbind(out,data.frame(label=data.train.y[x],data.train.x[x,]))})
    
    dtrain <- lapply(cv.group.x, 
                     function(x){out<-xgb.DMatrix(data = x%>%as.matrix()%>%.[,-1],
                                                  label= x%>%as.matrix()%>%.[,1])})
    dtest <- lapply(cv.group.y, 
                    function(x){out<-xgb.DMatrix(data = x%>%as.matrix()%>%.[,-1],
                                                 label= x%>%as.matrix()%>%.[,1])})
    importance_matrix <- lapply(dtrain,importance_function) 
    importance_combine <- Reduce("rbind", importance_matrix)
    return(importance_combine)
  }else if(auc==T){
    auc_value <- sapply(folds, auc_function_train) #%>% mean() 
    auc_value_combine <- c(auc_value_combine,auc_value)
    return(auc_value_combine)
  }
}

######## Bayesian Optimization #######
scoringFunction <- function(subsample) {
  dtrain <- xgb.DMatrix(data = data.train.x[,feature_select]%>%as.matrix(),
                        label= data.train.y%>%as.matrix())
  Pars <- list( 
    booster = "gbtree"
    , eta = 0.3
    , gamma = 0
    , max_depth = 3
    , min_child_weight = 1
    , subsample = subsample
    , objective = "binary:logistic"
    , eval_metric = "auc"
  )
  set.seed(1234567)
  xgbcv <- xgb.cv(
    params = Pars
    , data = dtrain
    , nround = 1
    , folds = folds
    , prediction = TRUE
    , showsd = TRUE
    #, early_stopping_rounds = 10
    , fill = TRUE
    , maximize = TRUE
    , verbose = 0)
  
  return(
    list( 
      Score = max(xgbcv$evaluation_log$test_auc_mean)
      #, nrounds = xgbcv$best_iteration
    )
  )
}
