sect_id=as.numeric(Sys.getenv("Sim"))

lamda=1e-5
bandwidth=750

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
load(here::here("Data","Medium Data","Lag_Rn.RData"))
load(here::here("Data","Medium Data","zipcode_coords.RData"))
load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))
load(here::here("Data","Medium Data","ZIP_Housing.RData"))

zipcode_rn_lag=zipcode_rn_lag%>%filter(Timestamp==t)
zip_mete_record=zip_mete_record%>%filter(month==m,year==y)

pred_base=zip_coord%>%
  left_join(zip_geo,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zip_mete_record,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zips_house,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zipcode_rn_lag,by=c("ZIPCODE"="ZIPCODE"))

names(pred_base)[names(pred_base)=="year"]="Year"
names(pred_base)[names(pred_base)=="month"]="Month"
names(pred_base)[names(pred_base)=="Timestamp"]="timestamp"
pred_base$dist2fault=as.numeric(pred_base$dist2fault)
pred_base=na.omit(pred_base)

pred_bases=list()
for(i in 1:length(base_models)){
  pred_bases[[i]]=predict(base_models[[i]],pred_base)
}
pred_bases=do.call(cbind,pred_bases)
pred_bases=as.data.frame(pred_bases)
names(pred_bases)=paste0("M",1:12,"_Pred")
#calculate distance matrix between monthly level and trainning dataset
load(here::here("Data","Medium Data","Ensemble_Training_Data.RData"))


dist_matrix=st.dist(dp.locat = as.matrix(pred_base[,c("X","Y")]),
                    rp.locat = as.matrix(m_preds[,c("X","Y")]),
                    obs.tv =pred_base$timestamp,
                    reg.tv =m_preds$timestamp,
                    lamda = lamda)

bases=paste0("M",c(1:9,11:12),"_Pred")

gtwr_pred=gtwr_s(obs=m_preds,
                 pred = pred_bases,
                 bases = bases,
                 kernel = "gaussian",
                 dis.matrix = dist_matrix,
                 bw=bandwidth)
coefs=gtwr_pred[,2:(length(bases)+2)]
pred=(cbind.data.frame(1,pred_bases[,bases]))*coefs
pred=rowSums(pred)
monthly_prediction=cbind.data.frame(pred_base$ZIPCODE,m,y,exp(pred))
names(monthly_prediction)=c("ZIPCODE","Month","Year","G_Radon")

#Optional Plot
library(sf)
projstring="+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"

load(here::here("Data","GeoData","2015_Shapes.RData"))
zip_ne=zips[zips$STATE%in%c("MA","NH","CT","RI","VT","ME"),]
projstring="+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"
zip_ne=st_as_sf(zip_ne)
zip_ne=st_transform(zip_ne,crs=projstring)
zip_ne=zip_ne%>%left_join(monthly_prediction,by=c("ZIP"="ZIPCODE"))

month_text=month.abb[m]

g<-ggplot()+
  geom_sf(data=zip_ne,aes(fill=G_Radon))+
  scale_fill_gradientn("Radon (pCi/L)",
                       colours = c("white","#fed800","#ff6f01","#fd2f24","#811d5e"),
                       limits=c(1,8),
                       breaks=c(1,2,4,6),
                       na.value="lightgray")+
  ggtitle(paste0(y,"-",month_text))+
  theme(
    legend.position = c(0.9,0.2),
    legend.background = element_blank()
  )

ggsave(file=here::here("Figures","Monthly_Prediction",paste0(y,"_",m,".jpg")),
       g,width=4,height =7,units = "in")
