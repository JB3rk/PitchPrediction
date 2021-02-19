#WatchaR


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


wacha<-read.csv('wachadata.csv')


#classifying pitches

table(wacha$mlbam_pitch_name)
wacha$breaking_dum<-ifelse(wacha$mlbam_pitch_name=="CU"|wacha$mlbam_pitch_name=="SL", 1, 0)
wacha$breaking_dum<-as.factor(wacha$breaking_dum)


#pitch before transfer

wacha$pitch_before<-wacha$mlbam_pitch_name[c(NA,1:(nrow(wacha)-1))]
wacha$ab_before<-wacha$ab_id[c(NA,(1:(nrow(wacha)-1)))]
wacha$ab_before[is.na(wacha$ab_before)] <- 0
wacha$pitch_before_adj<-wacha$pitch_before
wacha$pitch_before_adj[wacha$ab_before!=wacha$ab_id]<-NA
wacha$pitch_2before<-wacha$pitch_before[c(NA,(1:(nrow(wacha)-1)))]


wacha$px_before<-wacha$px[c(NA,1:(nrow(wacha)-1))]
wacha$px_before_adj<-wacha$px_before
wacha$px_before_adj[wacha$ab_before!=wacha$ab_id]<-NA

wacha$pz_before<-wacha$pz[c(NA,1:(nrow(wacha)-1))]
wacha$pz_before_adj<-wacha$pz_before
wacha$pz_before_adj[wacha$ab_before!=wacha$ab_id]<-NA

wacha$velo_before<-wacha$start_speed[c(NA,1:(nrow(wacha)-1))]
wacha$velo_before_adj<-wacha$velo_before
wacha$velo_before_adj[wacha$ab_before!=wacha$ab_id]<-NA

wacha$pfx_x_before<-wacha$pfx_x[c(NA,1:(nrow(wacha)-1))]
wacha$pfx_z_before<-wacha$pfx_z[c(NA,1:(nrow(wacha)-1))]

wacha$velo_2before<-wacha$velo_before[c(NA,1:(nrow(wacha)-1))]


table(wacha$mlbam_pitch_name)
wacha$pt<-ifelse(wacha$mlbam_pitch_name=="FC"|wacha$mlbam_pitch_name=="FF"|wacha$mlbam_pitch_name=="FT", "FB", "OF")
wacha$pt_before<-ifelse(wacha$pitch_before=="FC"|wacha$pitch_before=="FF"|wacha$pitch_before=="FT", "FB", "OF")


wacha$pt_dum<-ifelse(wacha$pt=="FB",0,1)
wacha$pt_dum<-as.factor(wacha$pt_dum)






wacha$pt_2before<-wacha$pt_before[c(NA,1:(nrow(wacha)-1))]
wacha$ab_2before<-wacha$ab_before[c(NA,(1:(nrow(wacha)-1)))]
wacha$pt_2before[wacha$ab_2before!=wacha$ab_id]<-NA

wacha$pt_before_dum<-ifelse(wacha$pt_before=="FB",0,1)
wacha$pt_2before_dum<-ifelse(wacha$pt_2before=="FB",0,1)

wacha$pt_before_dum<-as.factor(wacha$pt_before_dum)


#count status
wacha$count<-paste(wacha$balls, wacha$strikes, sep = ",")
  
wacha$count_status<-ifelse(wacha$balls>wacha$strikes, "behind", "ahead")
wacha$count_status[wacha$balls==wacha$strikes]<-"equal"

wacha$count_status_before<-NULL

wacha$ball3<-ifelse(wacha$balls==3,1,0)
wacha$strike2<-ifelse(wacha$strikes==2,1,0)

wacha$pt_combo<-NA
wacha$pt_combo[wacha$pt_before=="FB"&wacha$pt_2before=="FB"]<-"FB,FB"
wacha$pt_combo[wacha$pt_before=="FB"&wacha$pt_2before=="OF"]<-"FB,OF"
wacha$pt_combo[wacha$pt_before=="OF"&wacha$pt_2before=="FB"]<-"OF,FB"
wacha$pt_combo[wacha$pt_before=="OF"&wacha$pt_2before=="OF"]<-"OF,OF"

wacha$pt_before_dum<-as.numeric(wacha$pt_before_dum)

#zoneZ

wacha$centerv<-(((wacha$sz_top-wacha$sz_bot)/2)+wacha$sz_bot)
wacha$distabove<-ifelse(wacha$pz>wacha$centerv, wacha$pz-wacha$centerv, 0)
wacha$distabove_before<-wacha$distabove[c(NA,1:(nrow(wacha)-1))]
wacha$distabove_2before<-wacha$distabove_before[c(NA,1:(nrow(wacha)-1))]

wacha$distbelow<-ifelse(wacha$pz<wacha$centerv, wacha$center-wacha$pz, 0)
wacha$distbelow_before<-wacha$distbelow[c(NA,1:(nrow(wacha)-1))]
wacha$distbelow_2before<-wacha$distbelow_before[c(NA,1:(nrow(wacha)-1))]

#zoneX

wacha$pxb_adj<-ifelse(wacha$stand=="R", wacha$px_before_adj, -wacha$px_before_adj)
wacha$pxb2_adj<-wacha$pxb_adj[c(NA,1:(nrow(wacha)-1))]


wacha$pxb_inside<-ifelse(wacha$pxb_adj<0,-wacha$pxb_adj, 0)
wacha$pxb_outside<-ifelse(wacha$pxb_adj>0,wacha$pxb_adj, 0)  

wacha$pxb2_inside<-wacha$pxb_inside[c(NA,1:(nrow(wacha)-1))]
wacha$pxb2_outside<-wacha$pxb_outside[c(NA,1:(nrow(wacha)-1))]

#wacha$zone_1pcombo<- paste(wacha$zone_x_before, wacha$zone_spotz_before, sep = ",")


#only 3rd pitch
wacha$ab_3before<-wacha$ab_2before[c(NA,(1:(nrow(wacha)-1)))]
wacha$ab_3before[is.na(wacha$ab_3before)]<-0

wacha3rd<-subset(wacha, (ab_id==ab_2before))



##10fold
set.seed(6100)

train.control <- trainControl(method = "cv", number = 10)

tenfld <- train(pt ~ strike2*stand + distabove_before*stand + distabove_2before*stand + velo_2before*stand, data = wacha3rd, method = "glm", trControl = train.control)
# Summarize the results
print(tenfld)
summary(tenfld)

table(wacha3rd$mlbam_pitch_name)


