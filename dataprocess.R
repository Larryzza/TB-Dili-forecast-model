rm(list = ls())
setwd("D:/RA-POLYU/TB")
Sys.setlocale("LC_ALL", "Chinese")

source("function.R")

#####----read data & data processing---######

data1 <- read_xlsx("建模对象2020.12.30.xlsx",sheet = 5)
data2 <- read_xlsx("建模对象2020.12.30.xlsx",sheet = 6)

data1 <- data1[,c(1,5,7,9,26:30,32,35:44,46:51)] %>% data.frame(.,1)
data2 <- data2[,c(1,4,6,8,25:29,31,33,35:43,45:50)] %>% data.frame(.,0)
dput(names(data1))
title <- c("id","date","gender","age","weight","edu","income",
           "height","hepatitis.B","diabetes","duration",
           "PZA","RFP", "EMB", "INH", "INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg", 
           "INH80mg_RFP120mg_PZA250mg", "INH75mg_RFP150mg_PZA400mg_EMB275mg",
           "plan","BMI","duration_post","RFP_post", "EMB_post", 
           "INH_post","INH150mg_RFP300mg", "PZA_post", "result")
names(data1) <- title
names(data2) <- title
com.dat <- rbind(data1,data2) %>% date_function()


com.dat$gender[which(com.dat$gender=="男")]<-1
com.dat$gender[which(com.dat$gender=="女")]<-0

com.dat$edu[which(com.dat$edu %in% c("文盲","小学","初中"))]<-"edu.1"
com.dat$edu[which(com.dat$edu %in% c("大专","高中","中专"))]<-"edu.2"
com.dat$edu[which(com.dat$edu %in% c("本科","博士及以上","硕士"))]<-"edu.3"

###2?  hepatitis B
com.dat$hepatitis.B[which(is.na(com.dat$hepatitis.B)==F)]<-1
com.dat$hepatitis.B[which(is.na(com.dat$hepatitis.B)==T)]<-0

###3 diabetes
com.dat$diabetes[which(is.na(com.dat$diabetes)==F&com.dat$diabetes==0)]<-NA
com.dat$diabetes[which(is.na(com.dat$diabetes)==F)]<-1
com.dat$diabetes[which(is.na(com.dat$diabetes)==T)]<-0
#dose
com.dat[,c("PZA","PZA_post")] <- com.dat[,c("PZA","PZA_post")]*3
temp.ind <- which(com.dat$duration_post%in%"原方案")
com.dat$duration_post <- com.dat$duration-60
com.dat$duration_post[com.dat$duration_post<0] <- 0
com.dat$duration <- com.dat$duration - com.dat$duration_post

com.dat[temp.ind,c(12:18)] <- com.dat[temp.ind,c(12:18)]*
  (com.dat$duration[temp.ind]+com.dat$duration_post[temp.ind])
com.dat[-temp.ind,c(12:18)] <- com.dat[-temp.ind,c(12:18)]*com.dat$duration
com.dat[,c(22:26)] <- com.dat[,c(22:26)]*com.dat$duration_post
##other variables
com.dat$income[grep("五",com.dat$income)] <- c(25000,50000)
com.dat$income[grep("万",com.dat$income)] <- com.dat$income[grep("万",com.dat$income)] %>% 
  str_remove("万") %>% as.numeric() %>% `*` (10000) 
com.dat$income[grep("千",com.dat$income)] <- com.dat$income[grep("千",com.dat$income)] %>% 
  str_remove("千") %>% as.numeric() %>% `*` (1000) 
com.dat$age%<>%as.numeric();com.dat$weight%<>%as.numeric();com.dat$edu%<>%as.factor()
com.dat$height%<>%as.numeric();com.dat$income%<>%as.numeric()
#outlier
com.dat$height[com.dat$height>300]<-NA
com.dat$income[com.dat$income==0]<-NA

### **set na dose as zero
med <- apply(com.dat[,c(12:18,22:26)]%>%as.matrix, 2, 
             function(x){x[which(is.na(x)==T)]<-0;return(x)})
com.dat[,c(12:18,22:26)] <- med

#use single med
com.dat.v2 <- com.dat
com.dat.v2$PZA <- com.dat.v2$PZA + com.dat.v2$PZA_post +
  com.dat.v2$INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg*200/450 +
  com.dat.v2$INH80mg_RFP120mg_PZA250mg*250/450 + 
  com.dat.v2$INH75mg_RFP150mg_PZA400mg_EMB275mg*400/900
com.dat.v2$RFP <- com.dat.v2$RFP + com.dat.v2$RFP_post +
  com.dat.v2$INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg*75/450 +
  com.dat.v2$INH80mg_RFP120mg_PZA250mg*120/450 + 
  com.dat.v2$INH75mg_RFP150mg_PZA400mg_EMB275mg*150/900 +
  com.dat.v2$INH150mg_RFP300mg*300/450
com.dat.v2$EMB <- com.dat.v2$EMB + com.dat.v2$EMB_post +
  com.dat.v2$INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg*137.5/450 +
  com.dat.v2$INH75mg_RFP150mg_PZA400mg_EMB275mg*275/900
com.dat.v2$INH <- com.dat.v2$INH + com.dat.v2$INH_post +
  com.dat.v2$INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg*37.5/450 +
  com.dat.v2$INH80mg_RFP120mg_PZA250mg*80/450 + 
  com.dat.v2$INH75mg_RFP150mg_PZA400mg_EMB275mg*75/900+
  com.dat.v2$INH150mg_RFP300mg*150/450

com.dat.v2 %<>% 
  mutate(Duration=duration+duration_post)%<>%
  select(-c(INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg,
            INH80mg_RFP120mg_PZA250mg,
            INH75mg_RFP150mg_PZA400mg_EMB275mg,
            PZA_post,RFP_post,EMB_post,
            INH_post,INH150mg_RFP300mg,
            duration,duration_post)) %<>%
  filter(Duration<300)
write.csv(com.dat.v2,"com.dat.v2.csv")

#####----descriptive statistics ---######
table.ref<-com.dat.v2[,-c(1,2)]
varsToFactor <- c("gender", "edu", "hepatitis.B", "plan")
table.ref[varsToFactor] <- lapply(table.ref[varsToFactor], factor)
dput(names(table.ref))
vars<-c("gender", "age","weight", "edu","income", "height",
        "hepatitis.B", "diabetes", "Duration", "PZA",
        "RFP", "EMB", "INH", "plan", "BMI")
tableOne <- CreateTableOne(vars = vars, strata = c("result"), 
                           data = table.ref,addOverall = T)
print(tableOne,nonnormal = T)%>%write.csv("descriptive.csv")

### first time alt test
com.dat.v2<-com.dat.v2[-which(com.dat.v2$id=="0003981768")[1],]
com.dat.v2<-com.dat.v2[-which(com.dat.v2$id=="0001587553")[1],]
com.dat.v2<-com.dat.v2[-which(com.dat.v2$id=="0003894186"),]

data3 <- read_xlsx("建模对象2020.12.30.xlsx",sheet = 1)
data4 <- read_xlsx("建模对象2020.12.30 -z.xlsx",sheet = 4)
colnames(data3)[c(16,13,19,12,17)]<-c("id","report_date","alt_yn","alt_value","start_time")
data3 <- filter(data3,is.na(alt_value)==F)
data3$alt_yn[which(data3$alt_value>40)]<-1

newid<-apply(data3$...20[data3$report_date>="2019-01-01"] %>% as.matrix, 1, same_length)
data3$id[which(data3$report_date>="2019-01-01")]<-newid
data3<-data3[-16623,] #delet wired case
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
#############
data4 <- read_xlsx("建模对象2020.12.30 -z.xlsx",sheet = 4)
med_result <- lapply(com.dat.v2$id%>%unique %>%as.list(),
                     function(x){data4%>%filter(登记号...1==x)%>%
                         select(c(登记号...1,日期,医嘱名称,每次剂量,剂量单位,疗程))->out;
                       return(out)}) 
med_result <- Reduce("rbind",med_result)
colnames(med_result) <- c("id","sign_date","type","dose","unit","length")

med_result$dose[which(str_detect(med_result$type,"吡嗪酰胺片")==T&
                        (str_detect(med_result$unit,"片")==T|
                           str_detect(med_result$unit,"粒")==T))]%<>%`*`(0.25)
med_result$unit[which(str_detect(med_result$type,"吡嗪酰胺片")==T&
                        (str_detect(med_result$unit,"片")==T|
                           str_detect(med_result$unit,"粒")==T))]<-"g"
med_result$type[which(str_detect(med_result$type,"吡嗪酰胺片")==T)]<-"PZA"
med_result$type[which(str_detect(med_result$type,"利福平胶囊")==T|
                        str_detect(med_result$type,"利福喷丁胶囊")==T)]<-"RFP"
med_result$type[which(str_detect(med_result$type,"盐酸乙胺丁醇")==T)]<-"EMB"
med_result$type[which(str_detect(med_result$type,"异烟肼")==T)]<-"INH"
med_result$dose[which(str_detect(med_result$type,"乙胺吡嗪利福异烟片")==T&
                        str_detect(med_result$type,"450")==T)]%<>%`*` (0.45)
med_result$unit[which(str_detect(med_result$type,"乙胺吡嗪利福异烟片")==T&
                        str_detect(med_result$type,"450")==T)]<-"g"
med_result$type[which(str_detect(med_result$type,"乙胺吡嗪利福异烟片")==T&
                        str_detect(med_result$type,"450")==T)]<-"INH37.5mg_RFP75mg_PZA200mg_EMB137.5mg"
med_result$dose[which(str_detect(med_result$type,"异福酰胺胶囊")==T&
                        str_detect(med_result$unit,"粒")==T)]%<>%`*` (0.45)
med_result$unit[which(str_detect(med_result$type,"异福酰胺胶囊")==T&
                        str_detect(med_result$unit,"粒")==T)]<-"g"
med_result$type[which(str_detect(med_result$type,"异福酰胺胶囊")==T)]<-"INH80mg_RFP120mg_PZA250mg"
med_result$dose[which(str_detect(med_result$type,"乙胺吡嗪利福异烟片")==T&
                        str_detect(med_result$type,"900")==T)]%<>%`*` (0.9)
med_result$unit[which(str_detect(med_result$type,"乙胺吡嗪利福异烟片")==T&
                        str_detect(med_result$type,"900")==T)]<-"g"
med_result$type[which(str_detect(med_result$type,"乙胺吡嗪利福异烟片")==T&
                        str_detect(med_result$type,"900")==T)]<-"INH75mg_RFP150mg_PZA400mg_EMB275mg"
med_result$dose[which((str_detect(med_result$type,"异福胶囊")==T|
                         str_detect(med_result$type,"异福片")==T)&
                        (str_detect(med_result$unit,"片")==T|
                           str_detect(med_result$unit,"粒")==T))]%<>%`*` (0.45)
med_result$unit[which((str_detect(med_result$type,"异福胶囊")==T|
                        str_detect(med_result$type,"异福片")==T)&
                   (str_detect(med_result$unit,"片")==T|
                      str_detect(med_result$unit,"粒")==T))]<-"g"
med_result$type[which(str_detect(med_result$type,"异福胶囊")==T|
                        str_detect(med_result$type,"异福片")==T)]<-"INH150mg_RFP300mg"
med_result$type[which(str_detect(med_result$type,"硫酸链霉素")==T)]<-"SS"
med_result$length[which(str_detect(med_result$length,"天")==T)]%<>%
  str_replace(.,"天","")%<>%as.numeric()
med_result$length[which(str_detect(med_result$length,"周")==T)]%<>%
  str_replace(.,"周","")%<>%as.numeric()%<>%`*` (7)
med_result$length[which(str_detect(med_result$length,"个月")==T)]%<>%
  str_replace(.,"个月","")%<>%as.numeric()%<>%`*` (30)

med_result$sign_date %<>% as.Date()
med_result$length %<>% as.numeric()
write.csv(med_result,"med_result.csv")
com.dat.v2 %<>% filter(id!="0003894186") #wired case
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


