sect_id=as.numeric(Sys.getenv("Sim"))

lamda=1e-5
bandwidth=800

m=1+sect_id%%12
y=2005+as.integer(sect_id/12)
t=12*(y-1990)+m

library(caret)
library(dplyr)
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

#Create the prediction bases
load(here::here("Data","Medium Data","Selected_Base_Models.RData"))
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))

load(here::here("Data","Medium Data","Lag_Rn.RData"))
load(here::here("Data","Medium Data","zipcode_coords.RData"))
load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))
load(here::here("Data","Medium Data","ZIP_Housing.RData"))

zipcode_rn_lag=zipcode_rn_lag%>%filter(Timestamp==t)
zip_mete_record=zip_mete_record%>%filter(month==m,year==y)

pred_base=zip_coord%>%filter(ZIPCODE%in%gb_zip$ZIP)%>%
  left_join(zip_geo,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zip_mete_record,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zips_house,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zipcode_rn_lag,by=c("ZIPCODE"="ZIPCODE"))

names(pred_base)[names(pred_base)=="year.x"]="Year"
names(pred_base)[names(pred_base)=="month"]="Month"
names(pred_base)[names(pred_base)=="Timestamp"]="timestamp"
pred_base$dist2fault=as.numeric(pred_base$dist2fault)

pred_base[is.na(pred_base$Slope),"Slope"]=0
pred_base[is.na(pred_base$soilw),"soilw"]=400
pred_base[is.na(pred_base$soilm),"soilm"]=500
pred_base=na.omit(pred_base)

pred_bases=list()
for(i in 1:length(base_models)){
  pred_bases[[i]]=predict(base_models[[i]],pred_base)
}
pred_bases=do.call(cbind,pred_bases)
pred_bases=as.data.frame(pred_bases)
names(pred_bases)=paste0("M",1:10,"_Pred")
#calculate distance matrix between monthly level and trainning dataset
load(here::here("Data","Medium Data","Ensemble_Training_Data.RData"))


dist_matrix=st.dist(dp.locat = as.matrix(pred_base[,c("X","Y")]),
                    rp.locat = as.matrix(m_preds[,c("X","Y")]),
                    obs.tv =pred_base$timestamp,
                    reg.tv =m_preds$timestamp,
                    lamda = lamda)
monthly_pred=as.data.frame(matrix(nrow=nrow(pred_bases),ncol=3))
names(monthly_pred)=c("R1_Pred","R2_Pred","R3_Pred")
for(r in 1:3){
  bases=paste0("M",c(1:9),"_R",r,"_CV_Pred")
  names(pred_bases)[1:9]=bases
  gtwr_pred=gtwr_s(obs=m_preds,
                   pred = pred_bases,
                   bases = bases,
                   kernel = "gaussian",
                   dis.matrix = dist_matrix,
                   bw=bandwidth)
  coefs=gtwr_pred[,2:11]
  pred=(cbind.data.frame(1,pred_bases[,bases]))*coefs
  pred=rowSums(pred)
  monthly_pred[,paste0("R",r,"_Pred")]=pred
}

monthly_pred$Pred=rowMeans(monthly_pred)
monthly_prediction=cbind.data.frame(pred_base$ZIPCODE,m,y,exp(monthly_pred$Pred))
names(monthly_prediction)=c("ZIPCODE","Month","Year","G_Radon")
save(file=here::here("Data","Medium Data","Monthly Prediction",paste0(y,"_",m,".RData")),monthly_prediction)
#Optional Plot
library(sf)
projstring="+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))
gb_zip=gb_zip%>%left_join(monthly_prediction,by=c("ZIP"="ZIPCODE"))

month_text=month.abb[m]

g<-ggplot()+
  geom_sf(data=gb_zip,aes(fill=G_Radon))+
  scale_fill_gradientn("Radon (pCi/L)",
                       colours = c("white","#fed800","#ff6f01","#fd2f24","#811d5e"),
                       limits=c(0,5),
                       breaks=c(0,1,2,4,5),
                       na.value="lightgray")+
  ggtitle(paste0(y,"-",month_text))+
  theme(
    legend.position = c(0.9,0.6),
    legend.background = element_blank()
  )


ggsave(file=here::here("Figures","Monthly_Prediction",paste0(y,"_",m,".jpg")),
       g,width=4,height =7,units = "in")
