#The objective of this script is to create the national training dataset by
#binding many predictors to the zipcodes
library(here)
library(sf)
library(raster)
library(dplyr)
#Load basic data----------------------------------------
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

load(file = "National_Training_Data_Without_Predictors_230420.RData")
all_training_data=all_training_data%>%filter(N>1)

load(here::here("Data","GeoData","ZIP_CODE_Pop_Center.RData"))
load(file=here::here("Data","Medium Data","National_Spatial_Predictor_All.RData"))
load(file=here::here("Data","Medium Data","Monthly_Beta.RData"))
load(file=here::here("Data","Medium Data","Monthly_Metero.RData"))

zipcode_pdm_xy=zipcode_pdm_xy%>%filter(!is.na(x))
#Keep a copy of the longitude and latitude, as predictors
zipcode_pdm_xy$X=zipcode_pdm_xy$x
zipcode_pdm_xy$Y=zipcode_pdm_xy$y
coordinates(zipcode_pdm_xy)=~x+y
proj4string(zipcode_pdm_xy)=prjstring
zipcode_pdm_xy=st_as_sf(zipcode_pdm_xy)

#Attaching predictors based on zipcode, month of year, and year----------------
names(all_training_data)[1]="ZIPCODE"
training_data=all_training_data%>%left_join(zipcode_pdm_xy)
training_data=training_data[,names(training_data)!="geometry"]
#training_data=training_data%>%st_as_sf(sf_column_name = "geometry")
zipcode_geog=zipcode_geog%>%st_drop_geometry()
training_data=training_data%>%left_join(zipcode_geog)
training_data=training_data%>%left_join(monthly_beta,by=c("ZIPCODE"="ZIPCODE","year"="year","month"="month"))
training_data=training_data%>%left_join(monthly_weather,by=c("ZIPCODE"="ZIPCODE","year"="year","month"="month"))

training_data=training_data%>%dplyr::filter(ZIPCODE%in%zipcode_pdm_xy$ZIPCODE)
training_data=training_data%>%dplyr::filter(!is.na(Single_Family))
training_data=training_data%>%dplyr::filter(!is.na(popdensity))
training_data=training_data%>%dplyr::filter(!is.na(soilt))
training_data=training_data%>%dplyr::filter(!is.na(No_Fuel))

save(file=here::here("Data","Medium Data","National_Training_230430.RData"),training_data)

#save another 10 copies in scratch for following parallel computing 
for (r in 1:50){
  save(file =paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/National_Training/National_Training_",r,".RData"), training_data)
}
