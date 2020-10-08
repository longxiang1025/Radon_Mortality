library(caret)
library(caretEnsemble)
library(gbm)
library(reshape2)
library(mboost)
library(boot)
library(dplyr)
library(raster)
library(sf)

load(here::here("Data","Medium Data","ParaList.RData"))
load(here::here("Data","Medium Data","ParaTable.RData"))

load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
#Try the cutoff as 5 first, if needed, we can use smaller number
training_data=radon_month_obs
training_data=training_data%>%filter(n_units>4)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=na.omit(training_data)
CVfolds <- 10
CVrepeats <- 3

#set.seed(42)
#indexPreds <- createMultiFolds(training_data$month_Rn, k= CVfolds, times=CVrepeats)
#save(file=here::here("Data","CV_Folds.RData"),indexPreds)
load(here::here("Data","Month_CV_Folds.RData"))
##----------------Local the performance of each base model------------
m_files=list.files(here::here("Data","Medium Data","Model_List"))
base_results=list()
l=1
for(f in m_files){
  load(here::here("Data","Medium Data","Model_List",f))
  in_pred=predict(m,training_data)
  temp_df=cbind.data.frame(in_pred,training_data$gm_month,training_data$n_units)
  names(temp_df)=c("Pred","Obs","w")
  temp_df=temp_df[!is.na(temp_df$Pred),]
  R2=corr(temp_df[,c("Pred","Obs")],temp_df$w)
  CV_R2=m$results$Rsquare
  Type=m$modelInfo[[1]]
  r_info=cbind.data.frame(Type,f,R2,CV_R2)
  base_results[[l]]=r_info
  print(r_info)
  l=l+1
}
base_results=bind_rows(base_results)
save(file=here::here("Data","Medium Data","Base_Model_Results.RData"),base_results)
##----------------Select base models based on the performance and diversity------------
load(here::here("Data","Medium Data","Base_Model_Results.RData"))
##The top-level list containing base models of all type
base_models=list()
b_label=1
for(t in unique(base_results$Type)){
  ##t mean type, it iterate through all five classes of base models
  bases=base_results%>%filter(Type==t)
  bases=bases%>%arrange(desc(CV_R2))
  base_tank=list()
  base_pred=list()
  load(here::here("Data","Medium Data","Model_List",as.character(bases[1,"f"])))
  base_tank[[1]]=m
  pred=predict(base_tank[[1]],training_data)
  base_pred[[1]]=pred
  l=2
  for(b_t in 2:nrow(bases)){
    load(here::here("Data","Medium Data","Model_List",as.character(bases[b_t,"f"])))
    pred=predict(m,training_data)
    ##calculate the max R2 with selected models, if R2 is <0.9, add the new model in the list
    r=0
    for(b in 1:length(base_pred)){
      r0=cor(pred,base_pred[[b]],use="complete.obs")
      if(r0>r){
        r=r0
      }
    }
    if(r<0.949){
      print(paste("#",b_t,format(r,digits=4),as.character(bases[b_t,"f"]),"is used"))
      base_tank[[l]]=m
      base_pred[[l]]=pred
      l=l+1
    }
    else{
      print(paste("#",b_t,format(r,digits=4),as.character(bases[b_t,"f"]),"is very similar with top model(s)"))
    }
    if(l==4){
      for(temp in 1:length(base_tank)){
        base_models[[b_label]]=base_tank[[temp]]
        b_label=b_label+1
      }
      break()
    }
  }
  for(temp in 1:length(base_tank)){
    base_models[[b_label]]=base_tank[[temp]]
    b_label=b_label+1
  }
}
save(file=here::here("Data","Medium Data","Selected_Base_Models.RData"),base_models)
##----------------Build the final model--------------------------------------------
ensemble_training=list()
for(m in 1:length(multimodel)){
  model=multimodel[[m]]
  type=paste0("M",m)
  pred=model$pred%>%
    dplyr::select(pred,obs,weights,rowIndex)%>%
    group_by(rowIndex)%>%
    summarise(pred=mean(pred),
              obs=mean(obs),
              weights=mean(weights))
  pred$type=type
  ensemble_training[[m]]=pred
}
ensemble_training=dplyr::bind_rows(ensemble_training)
ensemble_bases=cbind.data.frame(
  ensemble_training%>%group_by(rowIndex)%>%summarise(obs=mean(obs)),
  (ensemble_training%>%group_by(rowIndex)%>%summarise(weights=mean(weights)))[,2],
  (ensemble_training%>%filter(type=="M1"))[,2],
  (ensemble_training%>%filter(type=="M2"))[,2],
  (ensemble_training%>%filter(type=="M3"))[,2],
  (ensemble_training%>%filter(type=="M4"))[,2],
  (ensemble_training%>%filter(type=="M5"))[,2],
  (ensemble_training%>%filter(type=="M6"))[,2],
  (ensemble_training%>%filter(type=="M7"))[,2]
)
names(ensemble_bases)=c("rowIndex","obs","weights","M1.pred","M2.pred",
                        "M3.pred","M4.pred","M5.pred","M6.pred","M7.pred")

m=caret::train(
  y=ensemble_bases$obs,
  x=ensemble_bases[,c("M1.pred","M2.pred","M3.pred","M4.pred",
                      "M5.pred","M6.pred","M7.pred")],
  weights=ensemble_bases$weights,
  importance="impurity",
  method="ranger",
  metric="RMSE",
  trControl=control,
  tuneGrid=data.frame(.mtry=5,.splitrule="variance",.min.node.size=3)
)
save(file=here::here("Data","Medium Data","Ensembled_Model.RData"),m)
