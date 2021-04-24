
rm(list = ls())
Sys.setlocale("LC_ALL", "Chinese")
source("function.R")

############# get alt information
load("combined_data.rds")

ALT_result <- lapply(com.dat.v2$id%>%unique()%>%as.list(),
                     function(x){data3%>%filter(id==x)%>%
                         select(c(id,alt_value,alt_yn,report_date,start_time))->out;
                       return(out)})
first_time <- lapply(com.dat.v2$id%>%unique %>%as.list(),
                     function(x){data4%>%filter(登记号...1==x)%>%
                         select(c(登记号...1,日期))%>%arrange(日期)->out;
                       return(out[1,])}) 
first_time_results <- Reduce("rbind",first_time)
names(first_time_results) <- c("id","start_time")
last_time <- lapply(ALT_result, dig_time_lable)
last_time_results <- Reduce("rbind",last_time)
last_time_results<-left_join(last_time_results[,-5],first_time_results,by="id") 

cut_off <- 1
ALT_results <- lapply(ALT_result, dig_alts)
ALT_results <- Reduce("rbind",ALT_results)

ALT_results %<>%
  filter(is.na(id)==F) %<>%
  filter(is.na(near)==F) %<>%
  select(-test_date)
colnames(ALT_results)<-c("id","alt_updated","alt_rate")

com.dat.v3 <- left_join(com.dat.v2,ALT_results,by="id")

write.csv(com.dat.v3,"com.dat.v3.csv")
com.dat.v3 <- read.csv("com.dat.v3.csv")[,-1]

############# med dose
med_result <- lapply(com.dat.v2$id%>%unique %>%as.list(),
                     function(x){data4%>%filter(登记号...1==x)%>%
                         select(c(登记号...1,日期,医嘱名称,每次剂量,剂量单位,疗程))->out;
                       return(out)}) 
med_result <- Reduce("rbind",med_result)
colnames(med_result) <- c("id","sign_date","type","dose","unit","length")

med_result$dose[intersect(grep("吡嗪酰胺片", med_result$type),
                          c(grep("片", med_result$unit),
                            grep("粒", med_result$unit)))] %<>% `*`(0.25)
med_result$unit[intersect(grep("吡嗪酰胺片", med_result$type),
                          c(grep("片", med_result$unit),
                            grep("粒", med_result$unit)))]<-"g"
med_result$type[grep("吡嗪酰胺片", med_result$type)]<-"PZA"
med_result$type[c(grep("利福平胶囊", med_result$type),
                  grep("利福喷丁胶囊", med_result$type))]<-"RFP"
med_result$type[grep("盐酸乙胺丁醇", med_result$type)]<-"EMB"
med_result$type[grep("异烟肼", med_result$type)]<-"INH"
med_result$dose[intersect(grep("乙胺吡嗪利福异烟片", med_result$type),
                          grep("450", med_result$type))] %<>% `*` (0.45)
med_result$unit[intersect(grep("乙胺吡嗪利福异烟片", med_result$type),
                          grep("450", med_result$type))]<-"g"
med_result$type[intersect(grep("乙胺吡嗪利福异烟片", med_result$type),
                          grep("450", med_result$type))]<-"INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg"
med_result$dose[intersect(grep("异福酰胺胶囊", med_result$type),
                          grep("粒", med_result$unit))] %<>% `*` (0.45)
med_result$unit[intersect(grep("异福酰胺胶囊", med_result$type),
                          grep("粒", med_result$unit))]<-"g"
med_result$type[grep("异福酰胺胶囊", med_result$type)]<-"INH80mg_RFP120mg_PZA250mg"
med_result$dose[intersect(grep("乙胺吡嗪利福异烟片", med_result$type),
                          grep("900", med_result$type))]%<>%`*` (0.9)
med_result$unit[intersect(grep("乙胺吡嗪利福异烟片", med_result$type),
                          grep("900", med_result$type))]<-"g"
med_result$type[intersect(grep("乙胺吡嗪利福异烟片", med_result$type),
                          grep("900", med_result$type))]<-"INH75mg_RFP150mg_PZA400mg_EMB275mg"
med_result$dose[intersect(c(grep("异福胶囊", med_result$type),
                            grep("异福片", med_result$type)),
                          c(grep("片", med_result$unit),
                            grep("粒", med_result$unit)))] %<>% `*` (0.45)
med_result$unit[intersect(c(grep("异福胶囊", med_result$type),
                            grep("异福片", med_result$type)),
                          c(grep("片", med_result$unit),
                            grep("粒", med_result$unit)))]<-"g"
med_result$type[c(grep("异福胶囊", med_result$type),
                  grep("异福片", med_result$type))]<-"INH150mg_RFP300mg"
med_result$type[grep("硫酸链霉素", med_result$type)]<-"SS"
med_result$length[grep("天", med_result$length)]%<>%
  str_remove("\u5929")%<>%as.numeric()
med_result$length[grep("周", med_result$length)]%<>%
  str_remove("\u5468")%<>%as.numeric()%<>%`*` (7)
med_result$length[grep("个月", med_result$length)]%<>%
  str_remove("\u4e2a\u6708")%<>%as.numeric()%<>%`*` (30)

med_result$sign_date %<>% as.Date()
med_result$length %<>% as.numeric()
write.csv(med_result,"med_result.csv")
med_dose <- lapply(com.dat.v2$id%>%as.list(), get_dose)

exact_dose_list <- lapply(med_dose, get_exact_dose_list)

med_dose_sort <-lapply(exact_dose_list,sort_med)
med_dose_sorts <- Reduce("rbind",med_dose_sort)

med_dose_sorts<-med_dose_sort[[1]]
for(i in 1:(length(med_dose_sort)-1)){
  med_dose_sorts<-full_join(med_dose_sorts,med_dose_sort[[i+1]],by = "name")
}
dim(med_dose_sorts)
med_dose_exact<-t(med_dose_sorts[,-1])%>%as.data.frame()
colnames(med_dose_exact)<-med_dose_sorts[,1]%>%as.character()
rownames(med_dose_exact)<-1:dim(med_dose_exact)[1]

med_dose_exact <- apply(med_dose_exact%>%as.matrix, 2, 
                        function(x){x[which(is.na(x)==T)]<-0;return(x)})
med_dose_exact%<>%as.data.frame()

com.dat.v4<-merge_dose(med_dose_exact)
com.dat.v4<-left_join(com.dat.v4,com.dat.v3[,-c(11:14)],by="id")
write.csv(com.dat.v4,"com.dat.v4.csv")

### duration distribution
tiff("horizon.tiff",width = 1000,height = 700,units = "px", pointsize = 22)
hist(com.dat.v4$days_dif[com.dat.v4$result==1], breaks=50, 
     xlim=c(0,300), col=rgb(1,0,0,0.5), 
     xlab="days from anti-tuberculous therapy",ylab="number of TB-DILI cases", main=NULL)
dev.off()
com.dat.v4$days_dif[com.dat.v4$result==1]%>%summary()
#####----descriptive statistics ---######
table.ref<-com.dat.v4
varsToFactor <- c("gender", "edu", "hepatitis.B", "plan","diabetes")
table.ref[varsToFactor] <- lapply(table.ref[varsToFactor], factor)
dput(names(table.ref))
vars<-c("gender", "age","weight", "edu","income", "height",
        "hepatitis.B", "diabetes", "days_dif", "PZA",
        "RFP", "EMB", "INH", "plan", "BMI","alt_updated","alt_rate")
tableOne <- CreateTableOne(vars = vars, strata = c("result"), 
                           data = table.ref,addOverall = T)
print(tableOne,nonnormal = T)%>%write.csv("descriptive.csv")

### save data
names(data4)[c(1,6)]<-c("id","date")
com.dat <- com.dat.v4
save(com.dat,med_result,data3,data4,file = "processed_data.rds")

