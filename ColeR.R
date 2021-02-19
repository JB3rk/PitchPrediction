#ColeR


library("dplyr")
library("rpart")
library("rpart.plot")
library("data.table")
library("ggplot2")
library("foreign")
library("nnet")
library("reshape2")
library("aod")
library("caret")
library("leaps")


cole<-read.csv('coledata.csv')


#classifying pitches

table(cole$mlbam_pitch_name)
cole$breaking_dum<-ifelse(Cole$mlbam_pitch_name=="CU"|cole$mlbam_pitch_name=="SL", 1, 0)
cole$breaking_dum<-as.factor(cole$breaking_dum)


#pitch before transfer

cole$pitch_before<-cole$mlbam_pitch_name[c(NA,1:(nrow(cole)-1))]
cole$ab_before<-cole$ab_id[c(NA,(1:(nrow(cole)-1)))]
cole$ab_before[is.na(cole$ab_before)] <- 0
cole$pitch_before_adj<-cole$pitch_before
cole$pitch_before_adj[cole$ab_before!=cole$ab_id]<-NA
cole$pitch_2before<-cole$pitch_before[c(NA,(1:(nrow(cole)-1)))]


cole$px_before<-cole$px[c(NA,1:(nrow(cole)-1))]
cole$px_before_adj<-cole$px_before
cole$px_before_adj[cole$ab_before!=cole$ab_id]<-NA

cole$pz_before<-cole$pz[c(NA,1:(nrow(cole)-1))]
cole$pz_before_adj<-cole$pz_before
cole$pz_before_adj[cole$ab_before!=cole$ab_id]<-NA

cole$velo_before<-cole$start_speed[c(NA,1:(nrow(cole)-1))]
cole$velo_before_adj<-cole$velo_before
cole$velo_before_adj[cole$ab_before!=cole$ab_id]<-NA

cole$pfx_x_before<-cole$pfx_x[c(NA,1:(nrow(cole)-1))]
cole$pfx_z_before<-cole$pfx_z[c(NA,1:(nrow(cole)-1))]

cole$velo_2before<-cole$velo_before[c(NA,1:(nrow(cole)-1))]


table(cole$mlbam_pitch_name)
cole$pt<-ifelse(cole$mlbam_pitch_name=="FC"|cole$mlbam_pitch_name=="FF"|cole$mlbam_pitch_name=="FT", "FB", "OF")
cole$pt_before<-ifelse(cole$pitch_before=="FC"|cole$pitch_before=="FF"|cole$pitch_before=="FT", "FB", "OF")


cole$pt_dum<-ifelse(cole$pt=="FB",0,1)
cole$pt_dum<-as.factor(cole$pt_dum)






cole$pt_2before<-cole$pt_before[c(NA,1:(nrow(cole)-1))]
cole$ab_2before<-cole$ab_before[c(NA,(1:(nrow(cole)-1)))]
cole$pt_2before[cole$ab_2before!=cole$ab_id]<-NA

cole$pt_before_dum<-ifelse(cole$pt_before=="FB",0,1)
cole$pt_2before_dum<-ifelse(cole$pt_2before=="FB",0,1)

cole$pt_before_dum<-as.factor(cole$pt_before_dum)


#count status
cole$count<-paste(cole$balls, cole$strikes, sep = ",")

cole$count_status<-ifelse(cole$balls>cole$strikes, "behind", "ahead")
cole$count_status[cole$balls==cole$strikes]<-"equal"

cole$count_status_before<-NULL

cole$ball3<-ifelse(cole$balls==3,1,0)
cole$strike2<-ifelse(cole$strikes==2,1,0)

cole$pt_combo<-NA
cole$pt_combo[cole$pt_before=="FB"&cole$pt_2before=="FB"]<-"FB,FB"
cole$pt_combo[cole$pt_before=="FB"&cole$pt_2before=="OF"]<-"FB,OF"
cole$pt_combo[cole$pt_before=="OF"&cole$pt_2before=="FB"]<-"OF,FB"
cole$pt_combo[cole$pt_before=="OF"&cole$pt_2before=="OF"]<-"OF,OF"

cole$pt_before_dum<-as.numeric(cole$pt_before_dum)

#zoneZ

cole$centerv<-(((cole$sz_top-cole$sz_bot)/2)+cole$sz_bot)
cole$distabove<-ifelse(cole$pz>cole$centerv, cole$pz-cole$centerv, 0)
cole$distabove_before<-cole$distabove[c(NA,1:(nrow(cole)-1))]
cole$distabove_2before<-cole$distabove_before[c(NA,1:(nrow(cole)-1))]

cole$distbelow<-ifelse(cole$pz<cole$centerv, cole$center-cole$pz, 0)
cole$distbelow_before<-cole$distbelow[c(NA,1:(nrow(cole)-1))]
cole$distbelow_2before<-cole$distbelow_before[c(NA,1:(nrow(cole)-1))]

#zoneX

cole$pxb_adj<-ifelse(cole$stand=="R", cole$px_before_adj, -cole$px_before_adj)
cole$pxb2_adj<-cole$pxb_adj[c(NA,1:(nrow(cole)-1))]


cole$pxb_inside<-ifelse(cole$pxb_adj<0,-cole$pxb_adj, 0)
cole$pxb_outside<-ifelse(cole$pxb_adj>0,cole$pxb_adj, 0)  

cole$pxb2_inside<-cole$pxb_inside[c(NA,1:(nrow(cole)-1))]
cole$pxb2_outside<-cole$pxb_outside[c(NA,1:(nrow(cole)-1))]



#only 3rd pitch
cole$ab_3before<-cole$ab_2before[c(NA,(1:(nrow(cole)-1)))]
cole$ab_3before[is.na(cole$ab_3before)]<-0

cole3rd<-subset(cole, (ab_id==ab_2before))



## set the seed to make your partition reproducible
set.seed(123)


##10fold

train.control <- trainControl(method = "cv", number = 10)

tenfld <- train(pt ~ stand + ball3 + strike2 + pxb_inside + pxb2_inside*stand, data = cole3rd, method = "glm", trControl = train.control)
# Summarize the results
print(tenfld)
summary(tenfld)

table(Cole3rd$mlbam_pitch_name)
