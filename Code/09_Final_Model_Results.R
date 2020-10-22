lamda=1e-5
bandwidth=300
library(GWmodel)
library(dplyr)
library(caret)
library(boot)

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
m_preds[,7:12]=100*m_preds[,7:12]

ens_m<-gtwr_s(obs=m_preds,
              pred=m_preds,
              bases = paste0("M",c(1:9,11:13),"_Pred"),
              bw=bandwidth,
              kernel = "gaussian",
              dis.matrix = dist_matrix)

pred_base=m_preds[,paste0("M",c(1:9,11:13),"_Pred")]
pred_base=cbind.data.frame(1,pred_base)
names(pred_base)[1]="Intercept"

coefs=ens_m[,2:14]
pred=pred_base*coefs
pred=rowSums(pred)

m_preds$ens_pred=pred
corr(m_preds[,c("ens_pred","obs")],w=m_preds$weights)
summary(m_preds$M4_Pred)