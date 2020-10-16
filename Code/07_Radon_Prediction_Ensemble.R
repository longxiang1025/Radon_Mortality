sect_id<-as.numeric(Sys.getenv("Sim"))
# lamda=c(1e-5,1e-4,1e-3,1e-2,5e-2,0.1,0.2,0.3,0.4,0.5)
# bandwidth=c(100,200,300,500,750,1000)
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
training_data=training_data%>%filter(n_units>4)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=training_data%>%left_join(zip_coord,by=c("ZIPCODE"="ZIPCODE"))
training_data=training_data%>%left_join(zipcode_rn_lag,by=c("ZIPCODE"="zipcode",
                                                            "timestamp"="timestamp"))
training_data=na.omit(training_data)
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
# m_files=list.files(here::here("Data","Medium Data","Model_List"))
# base_results=list()
# l=1
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
##The top-level list containing base models of all type
# base_models=list()
# b_label=1
# for(t in unique(base_results$Type)){
#   ##t mean type, it iterate through all five classes of base models
#   bases=base_results%>%filter(Type==t)
#   bases=bases%>%arrange(desc(CV_R2))
#   base_tank=list()
#   base_pred=list()
#   load(here::here("Data","Medium Data","Model_List",as.character(bases[1,"f"])))
#   base_tank[[1]]=m
#   pred=predict(base_tank[[1]],training_data)
#   base_pred[[1]]=pred
#   l=2
#   for(b_t in 2:nrow(bases)){
#     load(here::here("Data","Medium Data","Model_List",as.character(bases[b_t,"f"])))
#     pred=predict(m,training_data)
#     ##calculate the max R2 with selected models, if R2 is <0.9, add the new model in the list
#     r=0
#     for(b in 1:length(base_pred)){
#       r0=cor(pred,base_pred[[b]],use="complete.obs")
#       if(r0>r){
#         r=r0
#       }
#     }
#     if(r<0.949){
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

load(here::here("Data","Medium Data","Selected_Base_Models.RData"))
m_preds=list()
for(i in 1:length(base_models)){
  m_pred=predict(base_models[[i]])
  m_cv_pred=base_models[[i]]$pred%>%
    dplyr::select(pred,obs,weights,rowIndex)%>%
    group_by(rowIndex)%>%
    summarise(pred=mean(pred),
              obs=mean(obs),
              weights=mean(weights))
  record=cbind.data.frame(m_pred,m_cv_pred$pred)
  names(record)=paste0("M",i,c("_Pred","_CV_Pred"))
  print(nrow(record))
  m_preds[[i]]=record
}
m_preds=do.call(cbind,m_preds)
m_preds$obs=m_cv_pred$obs
m_preds$weights=m_cv_pred$weights

dist_matrix=st.dist(dp.locat = as.matrix(training_data[,c("X","Y")]),
                    rp.locat = as.matrix(training_data[,c("X","Y")]),
                    obs.tv =training_data$Month,
                    reg.tv =training_data$Month,
                    lamda = lamda)

ens_m<-gtwr_s(obs=m_preds,
              pred=m_preds,
              bases = paste0("M",c(1:9,11:13),"_Pred"),
              bw=bandwidth,
              kernel = "gaussian",
              dis.matrix = dist_matrix)

pred_base=m_preds[,c(1,3,5,7,9,11,13,15,17,21,23,25)]
pred_base=cbind.data.frame(1,pred_base)
names(pred_base)[1]="Intercept"

coefs=ens_m[,2:14]
pred=pred_base*coefs
pred=rowSums(pred)

m_preds$ens_pred=pred
load(here::here("Data","Medium Data","Ensemble_Tune_Result.RData"))
parameters[sect_id,"R2"]=corr(m_preds[,c("ens_pred","obs")],w=m_preds$weights)
print(parameters[sect_id,"R2"]^2)
save(file=here::here("Data","Medium Data","Ensemble_Tune_Result.RData"),parameters)

