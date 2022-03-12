#The objective of this script is to summarize the performance of each base model in cross validation
#the testing data, and then pick the base models with different (but good) predictions

#Fold<-as.numeric(Sys.getenv("Sim"))
p<-as.numeric(Sys.getenv("Sim"))

library(dplyr)
library(stringr)
library(boot)
library(mltools)
#------------------An section only run in parallel: organize performane by fold------------
# load("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Regional_CV_Folds.RData")
# 
# files=list.files(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold))
# parameters=str_split(files,c("_"),simplify = T)
# parameters[,4]=(str_split(parameters[,4],".RData",simplify = T))[,1]
# parameters=as.data.frame(parameters)
# names(parameters)=c("JobID","Fold","ID","In_ID")
# 
# cv_pred_base_list=list()
# base_test_list=list()
# 
# for(i in 1:nrow(parameters)){
#   load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold,"/",files[i]))
#   cv_pred_base$original_rowIndex=indexPreds[[Fold]]
#   if("weights"%in%names(cv_pred_base)){
#     cv_pred_base_list[[i]]=cv_pred_base%>%dplyr::select(obs,pred,weights,original_rowIndex)
#   }else{
#     cv_pred_base_list[[i]]=cv_pred_base%>%dplyr::select(obs,pred,N,original_rowIndex)
#     names(cv_pred_base_list[[i]])=c("obs","pred","weights","original_rowIndex")
#   }
#   cv_pred_base_list[[i]]$ID=parameters[i,"ID"]
#   cv_pred_base_list[[i]]$In_ID=parameters[i,"In_ID"]
# 
#   if("test_pred"%in%names(base_test)){
#     base_test_list[[i]]=base_test%>%dplyr::select(test_pred,ZIPCODE,Month,Year,N,Mean_Conc,SD_Conc,id,in_id)
#   }else{
#     base_test_list[[i]]=base_test%>%dplyr::select(base_test,ZIPCODE,Month,Year,N,Mean_Conc,SD_Conc,id,in_id)
#     names(base_test_list[[i]])[1]="test_pred"
#   }
# }
# cv_pred_base_Fold=bind_rows(cv_pred_base_list)
# base_pred_Fold=bind_rows(base_test_list)
#
#save(file = paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Base_Fold_Sum/",Fold,".RData"),
#     cv_pred_base_Fold,base_pred_Fold)


#------------------Check which base models can be run in all 30 Folds--------------
# files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/Base_Fold_Sum/",full.names = T)
# base_cv_pred_list=list()
# common_para_list=list()
# 
# for(i in 1:length(files)){
#   load(files[i])
#   #base_cv_pred_list[[i]]=cv_pred_base_Fold
#   unique_paras=unique(cv_pred_base_Fold[,c("ID","In_ID")])
#   common_para_list[[i]]=unique_paras
#   print(i)
# }
# common_para_list=bind_rows(common_para_list[1:30])
# common_para_list$para_string=paste0(common_para_list$ID,"_",common_para_list$In_ID)
# para_count=common_para_list%>%group_by(para_string)%>%summarise(n=length(ID))
# common_paras=para_count%>%filter(n==30)
# save(file="/n/holyscratch01/koutrakis_lab/Users/loli/Common_Bases.RData",common_paras)

#------------------A section only run in parallel: re-organize performance by base model--------
# load(file="/n/holyscratch01/koutrakis_lab/Users/loli/Common_Bases.RData")
# files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/Base_Fold_Sum/",full.names = T)
# 
# common_paras=cbind.data.frame(str_split(common_paras$para_string,"_",simplify = T)[,1],
#                               str_split(common_paras$para_string,"_",simplify = T)[,2])
# names(common_paras)=c("ID","In_ID")
# base_cv_pred_list=list()
# base_test_list=list()
# 
# if(file.exists(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Base_Para_Sum/",
#                            common_paras[p,"ID"],"_",common_paras[p,"In_ID"],".RData"))){
#   print(paste0( common_paras[p,"ID"],"_",common_paras[p,"In_ID"]," Finished"))
# }else{
#   for(i in sample(1:length(files),length(files),replace=F)){
#     load(files[i])
#     cv_pred_base_Fold$ID=as.numeric(as.character(cv_pred_base_Fold$ID))
#     cv_pred_base_Fold$In_ID=as.numeric(as.character(cv_pred_base_Fold$In_ID))
#     base_cv_pred_list[[i]]=cv_pred_base_Fold%>%dplyr::filter(ID==common_paras[p,"ID"],
#                                                              In_ID==common_paras[p,"In_ID"])
# 
#     base_pred_Fold$id=   as.numeric(as.character(base_pred_Fold$id))
#     base_pred_Fold$in_id=   as.numeric(as.character(base_pred_Fold$in_id))
# 
#     base_test_list[[i]]=base_pred_Fold%>%dplyr::filter(id==common_paras[p,"ID"],
#                                                        in_id==common_paras[p,"In_ID"])
#     print(i)
#     print(Sys.time())
#   }
#   base_test=bind_rows(base_test_list)
#   base_cv_pred=bind_rows(base_cv_pred_list)
# 
#   save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Base_Para_Sum/",
#                    common_paras[p,"ID"],"_",common_paras[p,"In_ID"],".RData"),
#                    base_test,base_cv_pred)
# }

# 
# #------------------Rank all base models based on their performance in cv---------
# load(file="/n/holyscratch01/koutrakis_lab/Users/loli/Common_Bases.RData")
# common_paras=cbind.data.frame(str_split(common_paras$para_string,"_",simplify = T)[,1],
#                               str_split(common_paras$para_string,"_",simplify = T)[,2])
# names(common_paras)=c("ID","In_ID")
# base_performance=list()
# for(p in 1:nrow(common_paras)){
#   load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Base_Para_Sum/",
#                     common_paras[p,"ID"],"_",common_paras[p,"In_ID"],".RData"))
#   if(common_paras[p,"ID"]==2){
#     base_cv_pred$pred=100*base_cv_pred$pred
#     base_test$test_pred=100*base_test$test_pred
#   }
#   cv_cor=corr(base_cv_pred[,c("obs","pred")],base_cv_pred$weights)
# 
#   cv_rmse=rmse(preds = base_cv_pred$pred,
#              actuals = base_cv_pred$obs,weights = base_cv_pred$weights)
# 
#   test_cor=corr(base_test[,c("test_pred","Mean_Conc")],base_test$N)
#   test_rmse=rmse(preds=base_test$test_pred,
#                  actuals=base_test$Mean_Conc,weights=base_test$N)
#   row_record=cbind.data.frame(common_paras[p,"ID"],common_paras[p,"In_ID"],cv_cor,cv_rmse,test_cor,test_rmse)
#   names(row_record)=c("ID","In_ID","CV_Cor","CV_RMSE","Test_Cor","Test_RMSE")
#   base_performance[[p]]=row_record
# }
# 
# base_performance=bind_rows(base_performance)
# save(file="/n/holyscratch01/koutrakis_lab/Users/loli/Base_Performance.RData",
#      base_performance)

#------------------Pick different base models----------------------------------
load(file="/n/holyscratch01/koutrakis_lab/Users/loli/Base_Performance.RData")
#To get rid of some pretty bad base models
base_performance=base_performance%>%filter(CV_Cor>0.1)%>%arrange(desc(CV_Cor))
#The knn models are not ordered correctly, so need to be dropped here
#base_performance=base_performance%>%filter(ID!=6)

load(paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Scratch_Copies/Regional_Training_",random_num=sample(1:5,1),".RData"))

model_list=list()
select_preds=list()
select_preds_test=list()
l=1
#The two parameter of base model selection
#The max number of base model selected
max_count=30
#The largest R2 the candidate model can have with selected base models
cutoff_cor=0.75

for( i in 1:nrow(base_performance)){
  add=T
  max_c=0
  if(i==1){
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Base_Para_Sum/",base_performance[i,"ID"],"_",base_performance[i,"In_ID"],".RData"))
  }else{
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Base_Para_Sum/",base_performance[i,"ID"],"_",base_performance[i,"In_ID"],".RData"))
    for(j in 1:(l-1))
    {
      temp_data=cbind.data.frame(base_cv_pred$pred,
                                 select_preds[,j],
                                 base_cv_pred$weights)
      names(temp_data)=c("Candidate_Pred","Select_Pred","Weight")
      c=corr(temp_data[,c("Candidate_Pred","Select_Pred")],temp_data$Weight)
      if(c>max_c){
        max_c=c
      }
      if(c>sqrt(cutoff_cor)){
        print(paste(base_performance[i,"ID"],"_",base_performance[i,"In_ID"],
                    "is too close with",model_list[[j]]$ID,"_",model_list[[j]]$In_ID,round(c,2)))
        add=F
        break()
      }
    }
  }
  if(add){
    print(paste(i,"out of",nrow(base_performance),base_performance[i,"ID"],"_",base_performance[i,"In_ID"],
                "is selected with max cor of ",round(max_c,2)))

    model_list[[l]]=base_cv_pred[1,c("ID","In_ID")]
    select_preds=bind_cols(select_preds,base_cv_pred$pred)
    select_preds_test=bind_cols(select_preds_test,base_test$test_pred)
    l=l+1
    if(l==(max_count+1)){
      break()
    }
  }
}

model_list=bind_rows(model_list)
names(select_preds)=paste0("Pred_",(1:ncol(select_preds)))
names(select_preds_test)=paste0("Pred_",(1:ncol(select_preds_test)))

select_preds$obs=base_cv_pred$obs
select_preds$weights=base_cv_pred$weights

for(i in 1:max_count){
  temp_pair=cbind.data.frame(select_preds[,i],select_preds$obs,select_preds$weights)
  names(temp_pair)=c("Pred","Obs","Weight")
  print(round(corr(temp_pair[,c("Pred","Obs")],temp_pair$Weight),3))
}

# m=lm(obs~Pred_1+Pred_10+Pred_20,w=select_preds$weights,data=select_preds)
