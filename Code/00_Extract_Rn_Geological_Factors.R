library(here)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(mgcv)
library(lubridate)
library(raster)
#load the Radon measurement data

radon_data<-read.csv(here::here("Data","harvard.csv"),header=T,sep = "\t")
load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","DEM.RData"))
load(here::here("Data","Env_Exp.RData"))
Rn_Potential<-shapefile(here::here("Data","USGS_Rn","usagrp_polygon.shp"))
Rn_Potential<-spTransform(Rn_Potential,proj4string(bound))

soil_type<-shapefile(here::here("Data","New_England_Surface_Materials","New_England_Surface_Materials.shp"))
soil_type<-spTransform(soil_type,proj4string(bound))

uranium<-raster(here::here("Data","USGS_Radiometric","NAMrad_U1.tif"))
uranium<-projectRaster(uranium,crs=proj4string(bound))
states<-st_as_sf(bound)
rn<-st_as_sf(Rn_Potential)


#Plot the number of Rn measurements in each zipcode
extent=states%>%filter(STUSPS%in%c("MA","NH","CT","RI","VT","ME"))
ggplot()+
  geom_sf(data=states,fill=NA)+
  geom_sf(data=zips_sf%>%filter(POP_SQMI>10000),fill="Red")+
  geom_sf(data=zips_sf,fill=NA)+
  scale_fill_viridis_c("# Rn Records")+
  coord_sf(xlim=extent(extent)[1:2],ylim=extent(extent)[3:4])+
  guides(fill = guide_colourbar(barwidth =1.25, barheight = 15))+
  theme(legend.background = element_rect(fill="lightgray"),
        legend.text = element_text(size=10),
        legend.direction = "vertical",
        panel.grid=element_line(color="darkgray",linetype = "dashed"))
#calculate the average RI in each zipcode
Rn_ras=rasterize(x=Rn_Potential,y=pdm_us,field="RI")
zips_rn<-extract(Rn_ras,zips,fun=mean,na.rm=T)
zips_sf$Rn_Potential=zips_rn[,1]
ggplot()+
  geom_sf(data=zips_sf,aes(fill=Rn_Potential))+
  scale_fill_viridis_c("Rn Potential")+
  coord_sf(xlim=extent(extent)[1:2],ylim=extent(extent)[3:4])+
  guides(fill = guide_colourbar(barwidth =1.25, barheight = 15))+
  theme(legend.background = element_rect(fill="lightgray"),
        legend.text = element_text(size=10),
        legend.direction = "vertical",
        panel.grid=element_line(color="darkgray",linetype = "dashed"))
temp=zips[,c("ZIP","POP_SQMI","Rn_Potential")]
st_geometry(temp)=NULL
radon_data=radon_data%>%left_join(temp,by=c("ZIPCODE"="ZIP"))
plot(radon_data$Rn_Potential,radon_data$PCI.L)
#
cor.test(ne_radon$PCI.L,ne_radon$Rn_Potential,use="complete.obs")
#Ok the correlation between Rn potential and actual measurement is significant
#Then we need to add meterological covariates into the stage
#calculate the monthly weather 

#calculate the zipcode-specific soil type
soil_type$UNIT_CODE=as.numeric(as.character(soil_type$UNIT_CODE))
soil_type_ras=rasterize(x=soil_type,y=pdm_us,field="UNIT_CODE")
#Glaciol Fine-Grained Soil
soil_42=soil_type_ras$layer>419&soil_type_ras$layer<429
zip_42=extract(soil_42,zips,fun=mean,na.rm=T)
zips_sf$Sur_42=zip_42[,1]
#Glaciol Medium-Grained Soil
soil_43=soil_type_ras$layer>429&soil_type_ras$layer<439
zip_43=extract(soil_43,zips,fun=mean,na.rm=T)
zips_sf$Sur_43=zip_43[,1]
#Glaciol Coarse Grained Soil/Rocks
soil_45=soil_type_ras$layer>449&soil_type_ras$layer<459
zip_45=extract(soil_45,zips,fun=mean,na.rm=T)
zips_sf$Sur_45=zip_45[,1]
#Pro-Glaciol Fine Grained Soil
soil_81=soil_type_ras$layer>809&soil_type_ras$layer<819
zip_81=extract(soil_81,zips,fun=mean,na.rm=T)
zips_sf$Sur_81=zip_81[,1]
#Pro-Glaciol Coarse Grained Soil
soil_82=soil_type_ras$layer>819&soil_type_ras$layer<829
zip_82=extract(soil_82,zips,fun=mean,na.rm=T)
zips_sf$Sur_82=zip_82[,1]
#Residual of Igneous and metamorphic rocks
soil_91=soil_type_ras$layer>909&soil_type_ras$layer<919
zip_91=extract(soil_91,zips,fun=mean,na.rm=T)
zips_sf$Sur_91=zip_91[,1]

#Calculate zip-level of Uranium concentration in surface soil
zip_uranium=extract(uranium,zips,buffer=10000,fun=mean,na.rm=T)
zips_sf$Uranium=zip_uranium[,1]
#fill the missing uranium concentration with 2ppm, because they are closer to coast
zips_sf[is.na(zips_sf$Uranium),"Uranium"]=2
#calculate zip-level slope
slope_raster=terrain(dem_raster,opt="slope")
zip_slope=extract(slope_raster,zips,fun=mean,na.rm=T)
zips_sf$Slope=zip_slope[,1]
zips_geo=zips_sf
save(file=here::here("Data","Medium Data","Rn_Geology.RData"),zips_geo)

