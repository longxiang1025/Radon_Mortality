sect_id<-as.numeric(Sys.getenv("Sim"))
# lamda=c(1e-6,1e-5,1e-4,1e-3,1e-2,5e-2,0.1,0.2,0.3,0.4,0.5)
# bandwidth=c(100,200,300,400,500,600,700,800,900,1000)
# parameters=expand.grid(lamda,bandwidth)
# names(parameters)=c("lamda","bandwidth")
# parameters$R2=0
# save(file=here::here("Data","Medium Data","Ensemble_Tune_Result.RData"),parameters)
load(here::here("Data","Medium Data","Ensemble_Tune_Result.RData"))
lamda=parameters[sect_id,"lamda"]
bandwidth=parameters[sect_id,"bandwidth"]

library(caret)
library(caretEnsemble)
library(gbm)
library(reshape2)
library(mboost)
library(boot)
library(dplyr)
library(raster)
library(sf)
library(GWmodel)

ti.distv=function (focal.t, obs.tv, units = "auto") 
{
  n <- length(obs.tv)
  dist.tv <- c()
  for (t in obs.tv) {
    if (focal.t >= t) 
      dist.tv <- c(dist.tv, ti.dist(t, focal.t, units = units))
    else 
      dist.tv <- c(dist.tv, abs(ti.dist(t, focal.t, units = units)))
  }
  dist.tv
}
assignInNamespace("ti.distv",ti.distv,"GWmodel")

source(here::here("Code","00_GTWR_NNLS_Function.R"))
load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
load(here::here("Data","Medium Data","Lag_Rn.RData"))
load(here::here("Data","Medium Data","zipcode_coords.RData"))
projstring="+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"
#Try the cutoff as 5 first, if needed, we can use smaller number
training_data=radon_month_obs
training_data$timestamp=12*(training_data$Year-1990)+training_data$Month
training_data=training_data%>%filter(Year>2004,Year<2019)
training_data=training_data%>%filter(n_units>9)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=training_data%>%left_join(zip_coord,by=c("ZIPCODE"="ZIPCODE"))
training_data=training_data%>%left_join(zipcode_rn_lag,by=c("ZIPCODE"="ZIPCODE",
                                                            "timestamp"="Timestamp"))
training_data[as.integer(substr(training_data$ZIPCODE,1,3))<28,"STATE"]="MA"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>28&as.integer(substr(training_data$ZIPCODE,1,3))<30,"STATE"]="RI"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>29&as.integer(substr(training_data$ZIPCODE,1,3))<39,"STATE"]="NH"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>38&as.integer(substr(training_data$ZIPCODE,1,3))<50,"STATE"]="ME"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>50&as.integer(substr(training_data$ZIPCODE,1,3))<60,"STATE"]="VT"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>59&as.integer(substr(training_data$ZIPCODE,1,3))<70,"STATE"]="CT"
training_data[is.na(training_data$STATE),"STATE"]="RI"
training_data=na.omit(training_data)

features=names(training_data)[c(1:2,15:16,18:90,94:96)]
CVfolds <- 10
CVrepeats <- 3

#set.seed(42)
#indexPreds <- createMultiFolds(training_data$month_Rn, k= CVfolds, times=CVrepeats)
#save(file=here::here("Data","CV_Folds.RData"),indexPreds)
weight_summary<-function(data, lev = NULL, model = NULL){
  #data=data[!is.na(data),]
  cor=corr(data[,c("obs","pred")],w=data[,"weights"])
  cor2=corr(data[,c("obs","pred")],w=data[,"weights"]^2)
  cor3=corr(data[,c("obs","pred")],w=sqrt(data[,"weights"]))
  rmse=sqrt(sum((data[,'obs']-data[,'pred'])^2*data[,'weights'])/sum(data[,'weights']))
  mae=sum(abs(data[,'obs']-data[,'pred'])*data[,'weights'])/sum(data[,'weights'])
  v=c(cor,cor2,cor3,rmse,mae)
  names(v)=c("Rsquare","Rsquare2","RsquareH","RMSE","MAE")
  return(v)
}
load(here::here("Data","Month_CV_Folds.RData"))
set.seed(4321)
control=trainControl(method="repeatedcv", number=CVfolds,repeats = CVrepeats,
                     savePredictions = T,index = indexPreds,returnResamp = "all",
                     verboseIter = TRUE,
                     summaryFunction = weight_summary)
set.seed(4321)
##----------------Local performance of each base model------------
#  m_files=list.files(here::here("Data","Medium Data","Model_List"))
#  base_results=list()
#  l=1
# for(f in m_files){
#   load(here::here("Data","Medium Data","Model_List",f))
#   in_pred=predict(m)
#   temp_df=cbind.data.frame(in_pred,training_data$gm_month,training_data$n_units)
#   names(temp_df)=c("Pred","Obs","w")
#   temp_df=temp_df[!is.na(temp_df$Pred),]
#   R2=corr(temp_df[,c("Pred","Obs")],temp_df$w)
#   CV_R2=m$results$Rsquare
#   Type=m$modelInfo[[1]]
#   r_info=cbind.data.frame(Type,f,R2,CV_R2)
#   base_results[[l]]=r_info
#   print(r_info)
#   l=l+1
# }
# base_results=bind_rows(base_results)
# save(file=here::here("Data","Medium Data","Base_Model_Results.RData"),base_results)
##----------------Select base models based on the performance and diversity------------
#load(here::here("Data","Medium Data","Base_Model_Results.RData"))
#The top-level list containing base models of all type
#  base_models=list()
#  b_label=1
#  for(t in unique(base_results$Type)){
#  ##t mean type, it iterate through all five classes of base models
#   bases=base_results%>%filter(Type==t)
#   bases=bases%>%arrange(desc(CV_R2))
#   base_tank=list()
#   base_pred=list()
#   load(here::here("Data","Medium Data","Model_List",as.character(bases[1,"f"])))
#   base_tank[[1]]=m
#   if(t=="Neural Networks with Feature Extraction"){
#     pred=100*predict(base_tank[[1]],training_data)
#   }else{
#     pred=predict(base_tank[[1]],training_data)
#   }
#   base_pred[[1]]=pred
#   l=2
#   for(b_t in 2:nrow(bases)){
#     load(here::here("Data","Medium Data","Model_List",as.character(bases[b_t,"f"])))
#     if(t=="Neural Networks with Feature Extraction"){
#       pred=100*predict(m,training_data)
#     }else{
#       pred=predict(m,training_data)
#     }
#     ##calculate the max R2 with selected models, if R2 is <0.9, add the new model in the list
#     r=0
#     for(b in 1:length(base_pred)){
#       temp=cbind.data.frame(pred-training_data$gm_month,
#                             base_pred[[b]]-training_data$gm_month,
#                             training_data$n_units)
#       names(temp)=c("pred","top_pred","w")
#       r0=corr(temp[,c("pred","top_pred")],w=temp$w)
#       if(r0>r){
#         r=r0
#       }
#     }
#     if((r<0.949)&(r>0.25)){
#       print(paste("#",t,b_t,format(r,digits=4),as.character(bases[b_t,"f"]),"is used"))
#       base_tank[[l]]=m
#       base_pred[[l]]=pred
#       l=l+1
#     }
#     else{
#       print(paste("#",t,b_t,format(r,digits=4),as.character(bases[b_t,"f"]),"is very similar with top model(s)"))
#     }
#     if(l==4){
#         break()
#     }
#   }
#   for(temp in 1:length(base_tank)){
#     base_models[[b_label]]=base_tank[[temp]]
#     b_label=b_label+1
#   }
# }
# save(file=here::here("Data","Medium Data","Selected_Base_Models.RData"),base_models)
##----------------Build the final model--------------------------------------------

# load(here::here("Data","Medium Data","Selected_Base_Models.RData"))
# m_preds=list()
# for(i in 1:(length(base_models))){
#   m_pred=predict(base_models[[i]])
#   temp=base_models[[i]]$pred
#   r1=temp[substr(temp$Resample,8,11)=="Rep1",]
#   r1=r1%>%arrange(rowIndex)
#   r2=temp[substr(temp$Resample,8,11)=="Rep2",]
#   r2=r2%>%arrange(rowIndex)
#   r3=temp[substr(temp$Resample,8,11)=="Rep3",]
#   r3=r3%>%arrange(rowIndex)
#   cv_result=cbind.data.frame(r1$pred,r2$pred,r3$pred)
#   names(cv_result)=c("R1_CV_Pred","R2_CV_Pred","R3_CV_Pred")
#   record=cbind.data.frame(m_pred,cv_result)
#   names(record)[1]="Pred"
#   names(record)=paste0("M",i,"_",names(record))
#   print(nrow(record))
#   m_preds[[i]]=record
# }
# m_preds=do.call(cbind,m_preds)
# m_preds$obs=training_data$gm_month
# m_preds$weights=training_data$n_units
# m_preds[,13:24]=100*m_preds[,13:24]
# m_preds$X=training_data$X
# m_preds$Y=training_data$Y
# m_preds$timestamp=training_data$timestamp
# save(file=here::here("Data","Medium Data","Ensemble_Training_Data.RData"),m_preds)

load(here::here("Data","Medium Data","Ensemble_Training_Data.RData"))
dist_matrix=st.dist(dp.locat = as.matrix(training_data[,c("X","Y")]),
                    rp.locat = as.matrix(training_data[,c("X","Y")]),
                    obs.tv =training_data$Month,
                    reg.tv =training_data$Month,
                    lamda = lamda)


for(r in 1:3){
  base_features=paste0("M",c(1:11,13),"_R",r,"_CV_Pred")
  pred_base=m_preds[,base_features]
  pred_base=cbind.data.frame(1,pred_base)
  names(pred_base)[1]="Intercept"
  
  base=m_preds[,paste0("M",c(1:11,13),"_Pred")]
  base=cbind.data.frame(1,base)
  names(base)[1]="Intercept"
  
  
  ens_m<-gtwr_s(obs=m_preds,
                pred=m_preds,
                bases = base_features,
                bw=bandwidth,
                kernel = "gaussian",
                dis.matrix = dist_matrix)
  coefs=ens_m[,2:14]
  cv_pred=pred_base*coefs
  cv_pred=rowSums(cv_pred)
  
  pred=base*coefs
  pred=rowSums(pred)
  
  m_preds[,paste0("R",r,"_Ens_CV_Pred")]=cv_pred
  m_preds[,paste0("R",r,"_Ens_Pred")]=pred
  
}

m_preds$Ens_CV_Pred=rowMeans(m_preds[,c("R1_Ens_CV_Pred","R2_Ens_CV_Pred","R3_Ens_CV_Pred")])
m_preds$Ens_Pred=rowMeans(m_preds[,c("R1_Ens_Pred","R2_Ens_Pred","R3_Ens_Pred")])
#save(file=here::here("Data","Medium Data","Final_Model_Performance.RData"),m_preds)

load(here::here("Data","Medium Data","Ensemble_Tune_Result.RData"))
parameters[sect_id,"R2"]=corr(m_preds[,c("Ens_Pred","obs")],w=m_preds$weights)
print(parameters[sect_id,"R2"])
save(file=here::here("Data","Medium Data","Ensemble_Tune_Result.RData"),parameters)

