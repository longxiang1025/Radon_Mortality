#The objective of this script is to extract the monthly meteorological factors and gross-beta levels

library(here)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(raster)

#Load basis data-----------------------------------------------
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#The first file is the population-weighted center of each zipcode.
#This file is produced by 18_Calc_Population_Center_ZIPCODE.R
load(here::here("Data","GeoData","ZIP_CODE_Pop_Center.RData"))
#The coordinates are converted to a spatial point (sf) object
##Prepare the ZIP Code data for the merging--------------------
zipcode_pdm_xy=zipcode_pdm_xy%>%filter(!is.na(x))
coordinates(zipcode_pdm_xy)=~x+y
proj4string(zipcode_pdm_xy)=prjstring
zipcode_pdm_xy=st_as_sf(zipcode_pdm_xy)

#Load general functions----------------------------------------
date_convert<-function(date){
  month=month(date)
  month=formatC(month,width = 2,flag=0)
  year=year(date)
  day=day(date)
  day=formatC(day,width = 2,flag=0)
  return(paste0(year,".",month,".",day))
}

extract_feature<-function(slice,spf){
  m_feature=slice
  Sys.time()
  zip_m_feature=extract(m_feature,spf,fun=mean,na.rm=T)
  Sys.time()
  return(zip_m_feature)
}

borrow_from_ngbs=function(zips,col_name){
  #Keep a copy of the original 
  order=zips%>%dplyr::select(ZIPCODE)
  #Get the column of with the same name
  col_index=which(colnames(zips)==col_name)
  col=zips[,col_index]
  #Quote the column, otherwise it cannot be passed to dplyr
  col <- dplyr::enquo(col)  
  #Separate the dataset based on whether to have a null value in the column
  NA_zips=zips%>%dplyr::filter(is.na(!!col))
  if(nrow(NA_zips)==0){
    paste0(col_name," has been imputed")
    return(zips)
  }else{
    valid_zips=zips%>%dplyr::filter(!is.na(!!col))
    #Extract the coordinates of borrower and lender
    NA_coords=st_coordinates(NA_zips)
    valid_coords=st_coordinates(valid_zips)
    #Find the closest zipcode with valid info
    pairs=nabor::knn(data=valid_coords,query = NA_coords,k=1)
    imputation=valid_zips[pairs[[1]],col_index]
    #Impute the info from lender
    NA_zips[,col_index]=as.data.frame(imputation)[,1]
    #Bind the borrower and lender together
    non_order=bind_rows(NA_zips,valid_zips)
    non_order=st_set_geometry(non_order, NULL)
    #Re-order the binded data based on the original rank
    result=order%>%left_join(non_order)
    return(result)
  }
}

borrow_from_layers=function(zips,layer,col_name){
  col_index=which(colnames(layer)==col_name)
  
  borrower_coords=st_coordinates(zips)
  lender_coords=st_coordinates(layer)
  
  pairs=nabor::knn(data=lender_coords,query = borrower_coords,k=1)
  imputation=layer[pairs[[1]],col_index]
  imputation$geometry=NULL
  zips=bind_cols(zips,imputation)
  return(zips)
}

#Attach monthly average meterological factors to zipcodes--------------
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
pres=stack(here::here("Data","Metero","pres.sfc.mon.mean.nc"))

monthly_record=list()
l=1
for(year in 2001:2021){
  for(month in 1:12){
    month_c=formatC(month,width = 2,flag=0)
    first_day=as.Date(paste0(year,"-",month_c,"-01"))
    last_day=as.Date(paste0(year,"-",month_c,"-01"))+days_in_month(as.Date(paste0(year,"-",month_c,"-01")))-1
    
    if(length(grep(names(uwind),pattern=date_convert(first_day)))>0){
      slice=grep(names(uwind),pattern=date_convert(first_day))
    }else{
      slice=grep(names(uwind),pattern=date_convert(first_day-1))
    }
    
    Sys.time()
    m_acpcp<-extract_feature(acpcp[[slice]],zipcode_pdm_xy)
    m_soilm<-extract_feature(soilm[[slice]],zipcode_pdm_xy)
    m_snowc<-extract_feature(snowc[[slice]],zipcode_pdm_xy)
    m_rhum<-extract_feature(rhum[[slice]],zipcode_pdm_xy)
    m_hpbl<-extract_feature(hpbl[[slice]],zipcode_pdm_xy)
    m_albedo<-extract_feature(albedo[[slice]],zipcode_pdm_xy)
    m_temp<-extract_feature(temp[[slice]],zipcode_pdm_xy)
    m_uwnd<-extract_feature(vwind[[slice]],zipcode_pdm_xy)
    m_vwnd<-extract_feature(uwind[[slice]],zipcode_pdm_xy)
    m_soilt<-extract_feature(tsoil[[slice]],zipcode_pdm_xy)
    m_soilw<-extract_feature(soilw[[slice]],zipcode_pdm_xy)
    m_pres<-extract_feature(pres[[slice]],zipcode_pdm_xy)
    Sys.time()
    m<-bind_cols(zipcode_pdm_xy,m_uwnd,m_vwnd,m_temp,m_albedo,m_hpbl,m_rhum,m_snowc,m_soilm,m_acpcp,m_soilt,m_soilw,m_pres)
    names(m)[2:13]<-c("uwnd","vwnd","temp","albedo","hpbl","rhum","snowc","soilm","pcp","soilt","soilw","pres")
    for(cols in c("uwnd","vwnd","temp","albedo","hpbl","rhum","snowc","soilm","pcp","soilt","soilw","pres")){
      m=borrow_from_ngbs(m,col_name = cols)
    }
    m$year=year
    m$month=month
    m$geometry=NULL
    monthly_record[[l]]=m
    l=l+1
  }
}
monthly_weather=bind_rows(monthly_record)

save(file=here::here("Data","Medium Data","Monthly_Metero.RData"),monthly_weather)

#Attach monthly PR levels to zipcode---------------
load("/n/koutrakis_lab/lab/One_Table/Data/Beta_Measurements/Beta_Measurements.RData")
radnet_coords=read.csv("/n/koutrakis_lab/lab/One_Table/Data/RadNet_Locations.csv")
coordinates(radnet_coords)=~Long+Lat
proj4string(radnet_coords)=geoprjstring
radnet_sf=st_as_sf(radnet_coords)
radnet_sf=radnet_sf%>%mutate(Location=paste0(City,", ",State))
beta_monthly=beta_measurements%>%group_by(Year,Month,Location)%>%summarise(mean_beta=mean(Result,na.rm=T))
beta_monthly=radnet_sf%>%left_join(beta_monthly)
beta_monthly=st_transform(beta_monthly,crs=prjstring)

month_beta_list=list()
l=1

for(year in 2001:2021){
  for( month in 1:12){
    month_beta=beta_monthly%>%dplyr::filter(Month==month,Year==year)
    month_zipcode_beta=borrow_from_layers(zips=zipcode_pdm_xy,layer = month_beta,col_name = "mean_beta")
    month_zipcode_beta$geometry=NULL
    month_zipcode_beta$year=year
    month_zipcode_beta$month=month
    
    month_beta_list[[l]]=month_zipcode_beta
    l=l+1
  }
}
monthly_beta=bind_rows(month_beta_list)
save(file=here::here("Data","Medium Data","Monthly_Beta.RData"),monthly_beta)

