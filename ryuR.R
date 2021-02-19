#Ryu

ryu<-read.csv('ryudata.csv')

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


#breaking pitches

table(ryu$mlbam_pitch_name)
ryu$breaking_dum<-ifelse(ryu$mlbam_pitch_name=="CU"|ryu$mlbam_pitch_name=="SL", 1, 0)

#pitch before transfer

ryu$pitch_before<-ryu$mlbam_pitch_name[c(NA,1:(nrow(ryu)-1))]
ryu$ab_before<-ryu$ab_id[c(NA,(1:(nrow(ryu)-1)))]
ryu$ab_before[is.na(ryu$ab_before)] <- 0
ryu$pitch_before_adj<-ryu$pitch_before
ryu$pitch_before_adj[ryu$ab_before!=ryu$ab_id]<-NA
ryu$pitch_2before<-ryu$pitch_before[c(NA,(1:(nrow(ryu)-1)))]


ryu$px_before<-ryu$px[c(NA,1:(nrow(ryu)-1))]
ryu$px_before_adj<-ryu$px_before
ryu$px_before_adj[ryu$ab_before!=ryu$ab_id]<-NA

ryu$pz_before<-ryu$pz[c(NA,1:(nrow(ryu)-1))]
ryu$pz_before_adj<-ryu$pz_before
ryu$pz_before_adj[ryu$ab_before!=ryu$ab_id]<-NA

ryu$velo_before<-ryu$start_speed[c(NA,1:(nrow(ryu)-1))]
ryu$velo_before_adj<-ryu$velo_before
ryu$velo_before_adj[ryu$ab_before!=ryu$ab_id]<-NA

ryu$pfx_x_before<-ryu$pfx_x[c(NA,1:(nrow(ryu)-1))]
ryu$pfx_z_before<-ryu$pfx_z[c(NA,1:(nrow(ryu)-1))]

ryu$velo_2before<-ryu$velo_before[c(NA,1:(nrow(ryu)-1))]


table(ryu$mlbam_pitch_name)
ryu$pt<-ifelse(ryu$mlbam_pitch_name=="FC"|ryu$mlbam_pitch_name=="FF"|ryu$mlbam_pitch_name=="FT", "FB", "OF")
ryu$pt_before<-ifelse(ryu$pitch_before=="FC"|ryu$pitch_before=="FF"|ryu$pitch_before=="FT", "FB", "OF")


ryu$pt_dum<-ifelse(ryu$pt=="FB",0,1)
ryu$pt_dum<-as.factor(ryu$pt_dum)






ryu$pt_2before<-ryu$pt_before[c(NA,1:(nrow(ryu)-1))]
ryu$ab_2before<-ryu$ab_before[c(NA,(1:(nrow(ryu)-1)))]
ryu$pt_2before[ryu$ab_2before!=ryu$ab_id]<-NA

ryu$pt_before_dum<-ifelse(ryu$pt_before=="FB",0,1)
ryu$pt_2before_dum<-ifelse(ryu$pt_2before=="FB",0,1)

ryu$pt_before_dum<-as.factor(ryu$pt_before_dum)


#count status
ryu$count<-paste(ryu$balls, ryu$strikes, sep = ",")

ryu$count_status<-ifelse(ryu$balls>ryu$strikes, "behind", "ahead")
ryu$count_status[ryu$balls==ryu$strikes]<-"equal"

ryu$count_status_before<-NULL

ryu$ball3<-ifelse(ryu$balls==3,1,0)
ryu$strike2<-ifelse(ryu$strikes==2,1,0)

ryu$pt_combo<-NA
ryu$pt_combo[ryu$pt_before=="FB"&ryu$pt_2before=="FB"]<-"FB,FB"
ryu$pt_combo[ryu$pt_before=="FB"&ryu$pt_2before=="OF"]<-"FB,OF"
ryu$pt_combo[ryu$pt_before=="OF"&ryu$pt_2before=="FB"]<-"OF,FB"
ryu$pt_combo[ryu$pt_before=="OF"&ryu$pt_2before=="OF"]<-"OF,OF"

ryu$pt_before_dum<-as.numeric(ryu$pt_before_dum)

#zoneZ

ryu$centerv<-(((ryu$sz_top-ryu$sz_bot)/2)+ryu$sz_bot)
ryu$distabove<-ifelse(ryu$pz>ryu$centerv, ryu$pz-ryu$centerv, 0)
ryu$distabove_before<-ryu$distabove[c(NA,1:(nrow(ryu)-1))]
ryu$distabove_2before<-ryu$distabove_before[c(NA,1:(nrow(ryu)-1))]

ryu$distbelow<-ifelse(ryu$pz<ryu$centerv, ryu$center-ryu$pz, 0)
ryu$distbelow_before<-ryu$distbelow[c(NA,1:(nrow(ryu)-1))]
ryu$distbelow_2before<-ryu$distbelow_before[c(NA,1:(nrow(ryu)-1))]

#zoneX

ryu$pxb_adj<-ifelse(ryu$stand=="R", ryu$px_before_adj, -ryu$px_before_adj)
ryu$pxb2_adj<-ryu$pxb_adj[c(NA,1:(nrow(ryu)-1))]


ryu$pxb_inside<-ifelse(ryu$pxb_adj<0,-ryu$pxb_adj, 0)
ryu$pxb_outside<-ifelse(ryu$pxb_adj>0,ryu$pxb_adj, 0)  

ryu$pxb2_inside<-ryu$pxb_inside[c(NA,1:(nrow(ryu)-1))]
ryu$pxb2_outside<-ryu$pxb_outside[c(NA,1:(nrow(ryu)-1))]

#ryu$zone_1pcombo<- paste(ryu$zone_x_before, ryu$zone_spotz_before, sep = ",")


#only 3rd pitch
ryu$ab_3before<-ryu$ab_2before[c(NA,(1:(nrow(ryu)-1)))]
ryu$ab_3before[is.na(ryu$ab_3before)]<-0

ryu3rd<-subset(ryu, (ab_id==ab_2before))


#10fold

set.seed(810)

train.control <- trainControl(method = "cv", number = 10)

ryu_tenfld <- train(pt_dum ~ stand+ ball3+ velo_before+distbelow_before+ velo_before*stand+distbelow_before*stand+stand*strike2, data = ryu3rd, method = "glm", trControl = train.control)
# Summarize the results
print(ryu_tenfld)
summary(ryu_tenfld)
