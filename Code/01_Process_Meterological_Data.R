r<-as.numeric(Sys.getenv("Sim"))
#1991-2019
year=1991+as.integer(r/12)
month=1+r%%12

library(here)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(raster)

date_convert<-function(date){
  month=month(date)
  month=formatC(month,width = 2,flag=0)
  year=year(date)
  day=day(date)
  day=formatC(day,width = 2,flag=0)
  return(paste0(year,".",month,".",day))
}

extract_feature<-function(slice,polygons){
  m_feature=slice
  m_feature=projectRaster(m_feature,crs=proj4string(polygons))
  m_feature=crop(m_feature,polygons)
  Sys.time()
  zip_m_feature=extract(m_feature,polygons,fun=mean,na.rm=T)
  Sys.time()
  return(zip_m_feature)
}

load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))

zips=zips[zips$STATE%in%c("MA","NH","CT","RI","VT","ME"),]

uwind<-stack(here::here("Data","Metero","uwnd.10m.mon.mean.nc"))
vwind<-stack(here::here("Data","Metero","vwnd.10m.mon.mean.nc"))
temp<-stack(here::here("Data","Metero","air.2m.mon.mean.nc"))
albedo<-stack(here::here("Data","Metero","albedo.mon.mean.nc"))
hpbl=stack(here::here("Data","Metero","hpbl.mon.mean.nc"))
rhum=stack(here::here("Data","Metero","rhum.2m.mon.mean.nc"))
snowc=stack(here::here("Data","Metero","snowc.mon.mean.nc"))
soilm=stack(here::here("Data","Metero","soilm.mon.mean.nc"))
acpcp=stack(here::here("Data","Metero","acpcp.mon.mean.nc"))
tsoil=stack(here::here("Data","Metero","tsoil.mon.mean.nc"))
soilw=stack(here::here("Data","Metero","soilw.mon.mean.nc"))
month_c=formatC(month,width = 2,flag=0)
first_day=as.Date(paste0(year,"-",month_c,"-01"))
last_day=as.Date(paste0(year,"-",month_c,"-01"))+days_in_month(as.Date(paste0(year,"-",month_c,"-01")))-1

if(length(grep(names(uwind),pattern=date_convert(first_day)))>0){
  slice=grep(names(uwind),pattern=date_convert(first_day))
}else{
  slice=grep(names(uwind),pattern=date_convert(first_day-1))
}

Sys.time()
m_acpcp<-extract_feature(acpcp[[slice]],zips)
m_soilm<-extract_feature(soilm[[slice]],zips)
m_snowc<-extract_feature(snowc[[slice]],zips)
m_rhum<-extract_feature(rhum[[slice]],zips)
m_hpbl<-extract_feature(hpbl[[slice]],zips)
m_albedo<-extract_feature(albedo[[slice]],zips)
m_temp<-extract_feature(temp[[slice]],zips)
m_uwnd<-extract_feature(vwind[[slice]],zips)
m_vwnd<-extract_feature(uwind[[slice]],zips)
m_soilt<-extract_feature(tsoil[[slice]],zips)
m_soilw<-extract_feature(soilw[[slice]],zips)
Sys.time()
m<-cbind.data.frame(zips$ZIP,m_uwnd,m_vwnd,m_temp,m_albedo,m_hpbl,m_rhum,m_snowc,m_soilm,m_acpcp,m_soilt,m_soilw)
names(m)<-c("ZIP","uwnd","vwnd","temp","albedo","hpbl","rhum","snowc","soilm","pcp","soilt","soilw")
m$year=year
m$month=month
save(file=here::here("Data","Zip_Metro",paste0(year,"_",month,".RData")),m)

# files<-list.files(here::here("Data","Zip_Metro"),full.names = T)
# zip_mete_record<-list()
# for(f in 1:length(files)){
#   load(files[f])
#   m$ZIP<-as.character(m$ZIP)
#   zip_mete_record[[f]]=m
# }
# zip_mete_record<-bind_rows(zip_mete_record)
# save(file=here::here("Data","Medium Data","Monthly_Mete.RData"),zip_mete_record)
