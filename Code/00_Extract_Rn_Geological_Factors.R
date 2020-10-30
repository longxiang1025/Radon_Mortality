library(here)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(mgcv)
library(lubridate)
library(raster)
library(foreign)
#load the Radon measurement data

#radon_data<-read.csv(here::here("Data","harvard.csv"),header=T,sep = "\t")
load(here::here("Data","GeoData","2015_Shapes.RData"))
load(file=here::here("Data","Medium Data","GB_ZIPCODE.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","DEM.RData"))
load(here::here("Data","Env_Exp.RData"))

#Extract SES factors
ses_factors<-env_exp%>%filter(year==2015)
ses_factors<-ses_factors%>%filter(ZIP%in%gb_zip$ZIP)

gb_zip=gb_zip%>%left_join(ses_factors,by=c("ZIP"="ZIP"))
gb_zip=gb_zip%>%dplyr::select(ZIP,STATE,COUNTYFP,POPULATION,POP_SQMI,SQMI,poverty,medianhousevalue,
                              medhouseholdincome,pct_owner_occ,education)
#ggplot()+geom_sf(data=gb_zip,aes(fill=poverty))
Rn_Potential<-shapefile(here::here("Data","USGS_Rn","usagrp_polygon.shp"))
Rn_Potential<-spTransform(Rn_Potential,proj4string(bound))

soil_type<-shapefile(here::here("Data","New_England_Surface_Materials","New_England_Surface_Materials.shp"))
soil_type<-spTransform(soil_type,proj4string(bound))

uranium<-raster(here::here("Data","USGS_Radiometric","NAMrad_U1.tif"))
uranium<-projectRaster(uranium,crs=proj4string(bound))

thorium<-raster(here::here("Data","USGS_Radiometric","NAMrad_Th.tif"))
thorium<-projectRaster(thorium,crs=proj4string(bound))

#states<-st_as_sf(bound)
rn<-st_as_sf(Rn_Potential)
#set the study extent as New England
#extent=states%>%filter(STUSPS%in%c("MA","NH","CT","RI","VT","ME"))
#zip_ne=zips[zips$STATE%in%c("MA","NH","CT","RI","VT","ME"),]
#zip_ne=st_as_sf(zip_ne)
#zip_ne=st_transform(zip_ne,crs=proj4string(soil_grids))

soil_grids<-raster("/n/koutrakis_lab/lab/Group_Data/gridded_soil_data/MUKEY90m/MapunitRaster_CONUS_90m1.tif")
soil_info=read.dbf("/n/koutrakis_lab/lab/Group_Data/gridded_soil_data/Layer/Layer.dbf")
soil_info$mukey=as.character(soil_info$mukey)
gb_zip=st_transform(gb_zip,crs=proj4string(soil_grids))
zip_centroid=st_centroid(gb_zip)

zip_soil_list=list()
#extrac the soil information for each zip-code
for(z in 1:nrow(zip_centroid)){
  f=zip_centroid[z,]
  grids<-expand.grid(x=st_coordinates(f)[,1]+seq(-18000,18000,6000),
                     y=st_coordinates(f)[,2]+seq(-18000,18000,6000))
  names(grids)=c("x","y")
  #Extract the MUKEY of local grids
  grids.mukey=extract(soil_grids,grids)
  grids.mukey=as.character(grids.mukey)
  grids.mukey=as.data.frame(grids.mukey)
  
  #bind the gNASTO information to local grids
  grids.info=grids.mukey%>%left_join(soil_info,by=c("grids.mukey"="mukey"))
  #replace all -9999 with NA and remove them
  grids.info[grids.info==-9999]=NA
  #calculate column mean
  grids.average=colMeans(grids.info[,c("AVG_AWC","AVG_OM","AVG_KSAT",
                                       "AVG_KV","AVG_BD","AVG_FC","AVG_NO4","AVG_NO10",
                                       "AVG_NO200","AVG_POR","AVG_KFACT","AVG_THK")],na.rm=T)
  grids.average=as.data.frame(t(grids.average))
  grids.average$zipcode=f$ZIP
  zip_soil_list[[z]]=grids.average
}
zip_soil_list=dplyr::bind_rows(zip_soil_list)
gb_zip[,names(zip_soil_list)[1:12]]=zip_soil_list[,1:12]
#ggplot()+geom_sf(data=gb_zip,aes(fill=AVG_AWC))
#save(file=here::here("Data","zip_SSURGO.RData"),zip_soil_list)
#calculate the average RI in each zipcode
Rn_ras=rasterize(x=Rn_Potential,y=pdm_us,field="RI")
zips_rn<-extract(Rn_ras,as_Spatial(zip_centroid),fun=mean,na.rm=T)
gb_zip$Rn_Potential=zips_rn
#ggplot()+geom_sf(data=gb_zip,aes(fill=Rn_Potential))

#calculate the zipcode-specific soil type
soil_type$UNIT_CODE=as.numeric(as.character(soil_type$UNIT_CODE))
soil_type_ras=rasterize(x=soil_type,y=pdm_us,field="UNIT_CODE")
#Glaciol Fine-Grained Soil
gb_zip=st_transform(gb_zip,crs=proj4string(soil_type_ras))
soil_42=soil_type_ras$layer>419&soil_type_ras$layer<429
zip_42=extract(soil_42,as_Spatial(gb_zip),fun=mean,na.rm=T)
gb_zip$Sur_42=zip_42[,1]
#Glaciol Medium-Grained Soil
soil_43=soil_type_ras$layer>429&soil_type_ras$layer<439
zip_43=extract(soil_43,as_Spatial(gb_zip),fun=mean,na.rm=T)
gb_zip$Sur_43=zip_43[,1]
#Glaciol Coarse Grained Soil/Rocks
soil_45=soil_type_ras$layer>449&soil_type_ras$layer<459
zip_45=extract(soil_45,as_Spatial(gb_zip),fun=mean,na.rm=T)
gb_zip$Sur_45=zip_45[,1]
#Pro-Glaciol Fine Grained Soil
soil_81=soil_type_ras$layer>809&soil_type_ras$layer<819
zip_81=extract(soil_81,as_Spatial(gb_zip),fun=mean,na.rm=T)
gb_zip$Sur_81=zip_81[,1]
#Pro-Glaciol Coarse Grained Soil
soil_82=soil_type_ras$layer>819&soil_type_ras$layer<829
zip_82=extract(soil_82,as_Spatial(gb_zip),fun=mean,na.rm=T)
gb_zip$Sur_82=zip_82[,1]
#Residual of Igneous and metamorphic rocks
soil_91=soil_type_ras$layer>909&soil_type_ras$layer<919
zip_91=extract(soil_91,as_Spatial(gb_zip),fun=mean,na.rm=T)
gb_zip$Sur_91=zip_91[,1]

#Calculate zip-level of Uranium concentration in surface soil
zip_uranium=extract(uranium,as_Spatial(gb_zip),buffer=10000,fun=mean,na.rm=T)
gb_zip$Uranium=zip_uranium[,1]
#Calculate zip-level of Uranium concentration in surface soil
zip_thorium=extract(thorium,as_Spatial(gb_zip),buffer=10000,fun=mean,na.rm=T)
gb_zip$Thorium=zip_thorium[,1]
#fill the missing uranium concentration with 2ppm, because they are closer to coast
gb_zip[is.na(gb_zip$Uranium),"Uranium"]=2
gb_zip[is.na(gb_zip$Thorium),"Thorium"]=2
#calculate zip-level slope
slope_raster=terrain(dem_raster,opt="slope")
zip_slope=extract(slope_raster,as_Spatial(gb_zip),fun=mean,na.rm=T)
gb_zip$Slope=zip_slope[,1]

zip_elevation=extract(dem_raster,as_Spatial(gb_zip),fun=mean,na.rm=T)
gb_zip$Elev=zip_elevation[,1]

#calculate distance to fault
fault<-shapefile("/n/koutrakis_lab/lab/Group_Data/USGS_Geological_Map/US_Faults.shp")
fault<-spTransform(fault,as.character(st_crs(zip_centroid)))
fault=st_as_sf(fault)

index=st_nearest_feature(x=zip_centroid,y=fault)
faults=fault[index,]
dist2fault<- st_distance(x = zip_centroid, y= faults, by_element = TRUE)
gb_zip$dist2fault=as.numeric(dist2fault)

zip_geo=gb_zip
zip_geo$geometry=NULL
save(file=here::here("Data","Medium Data","Rn_Geology.RData"),zip_geo)

