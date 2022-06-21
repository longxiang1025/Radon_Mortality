#The objective of this script is to attach spatial predictors to each ZIP Code
#based on the population-weighted centers
library(dplyr)
library(sf)
library(raster)
library(here)
library(ggplot2)
library(foreign)
library(nabor)
library(stringr)
library(mgcv)
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

#Merge all geographical predictors-----------------------------
#(1) Geological Radon Potenial (RI)
#(2) Surficial Materials (Name)
#(3) Soil features (AVG_***)
#(4) Uranium
##Attaching geological radon potential--------
Rn_Potential<-st_read(here::here("Data","USGS_Rn","usagrp_polygon.shp"))
Rn_Potential=st_transform(Rn_Potential,crs=prjstring)
zipcode_Rn=st_join(x=zipcode_pdm_xy,y=Rn_Potential,join=st_within)
zipcode_Rn=zipcode_Rn%>%dplyr::select(ZIPCODE,RI)
zipcode_Rn=borrow_from_ngbs(zipcode_Rn,"RI")
#ggplot(data=zipcode_Rn)+geom_sf(aes(color=RI),shape=16)
save(file=here::here("Data","Medium Data","ZIP_Radon_Potential.RData"),zipcode_Rn)

##Attaching surfacial material types-----------
Surface=st_read("/n/koutrakis_lab/lab/Group_Data/surface_materials_data/USGS_DS_425/Surface_Materials.shp")
Surface=st_transform(Surface,crs=prjstring)
zipcode_Surface=st_join(x=zipcode_pdm_xy,y=Surface,join=st_within)
zipcode_Surface=zipcode_Surface%>%dplyr::select(ZIPCODE,DMU_HIER)
Surface_Table=as.data.frame(str_split(as.character(zipcode_Surface$DMU_HIER),pattern = "-",simplify = T))
names(Surface_Table)=c("Type","Granularity","Thickness")
Surface_Name_Table=cbind.data.frame(c("001","002","003","004","005","006","007","008","009","010","011","012","013"),
                                    c("Alluvial","Coastal","Calcareous","Organic","Glacial","Glaciofluvial","Proglacial","Lacustrine","Eolian","Mass-movement","Residual","Rocks","Water"))
names(Surface_Name_Table)=c("Type","Name")
Surface_Table=Surface_Table%>%left_join(Surface_Name_Table)
zipcode_Surface$Sediment_Type=Surface_Table$Name
zipcode_Surface=borrow_from_ngbs(zipcode_Surface,"Sediment_Type")
#ggplot(data=zipcode_Surface)+geom_sf(aes(color=Name),shape=16)
save(file=here::here("Data","Medium Data","ZIP_Surface.RData"),zipcode_Surface)

##Attaching soil variables-------------------
Soil_Grids<-raster("/n/koutrakis_lab/lab/Group_Data/gridded_soil_data/MUKEY90m/MapunitRaster_CONUS_90m1.tif")
Soil_Info=read.dbf("/n/koutrakis_lab/lab/Group_Data/gridded_soil_data/Layer/Layer.dbf")
Soil_Info$mukey=as.character(Soil_Info$mukey)

grids.mukey=extract(Soil_Grids,zipcode_pdm_xy)
grids.mukey=as.character(grids.mukey)
grids.mukey=as.data.frame(grids.mukey)
Grids_Info=grids.mukey%>%left_join(Soil_Info,by=c("grids.mukey"="mukey"))
Grids_Info[Grids_Info==-9999]=NA
Grids_Info=Grids_Info%>%dplyr::select(AVG_AWC,AVG_OM,AVG_KSAT,AVG_KV,AVG_BD,AVG_FC,AVG_NO4,AVG_NO10,AVG_NO200,AVG_POR,AVG_KFACT,AVG_THK)
zipcode_Soil=bind_cols(zipcode_pdm_xy,Grids_Info)
###There're tons of missing values, burrow that information from the nearest zipcode with valid information--------
for(cols in c("AVG_AWC","AVG_OM","AVG_KSAT","AVG_KV","AVG_BD","AVG_FC","AVG_NO4","AVG_NO10","AVG_NO200","AVG_POR","AVG_KFACT","AVG_THK")){
  zipcode_Soil=borrow_from_ngbs(zipcode_Soil,col_name = cols)
}
save(file=here::here("Data","Medium Data","ZIP_Soil.RData"),zipcode_Soil)
#load(here::here("Data","Medium Data","ZIP_Soil.RData"))
##Attaching Uranium concentrations------------
uranium<-raster(here::here("Data","USGS_Radiometric","NAMrad_U1.tif"))
zipcode_Uranium=extract(uranium,zipcode_pdm_xy)
zipcode_Uranium=bind_cols(zipcode_pdm_xy,zipcode_Uranium)
names(zipcode_Uranium)[2]="Uranium"
zipcode_Uranium=borrow_from_ngbs(zipcode_Uranium,col_name = "Uranium")
save(file=here::here("Data","Medium Data","ZIP_Uranium.RData"),zipcode_Uranium)
load(here::here("Data","Medium Data","ZIP_Uranium.RData"))
##Attaching elevation, slope and some other topographical factors------------
load(here::here("Data","DEM.RData"))

zipcode_elevation=extract(dem_raster,zipcode_pdm_xy)

slope_raster=terrain(dem_raster,opt="slope")
zipcode_Slope=extract(slope_raster,zipcode_pdm_xy)

TRI_raster=terrain(dem_raster,opt="TRI")
zipcode_TRI=extract(TRI_raster,zipcode_pdm_xy)

TPI_raster=terrain(dem_raster,opt="TPI")
zipcode_TPI=extract(TPI_raster,zipcode_pdm_xy)

rough_raster=terrain(dem_raster,opt="roughness")
zipcode_rough=extract(rough_raster,zipcode_pdm_xy)
zipcode_Topo=bind_cols(zipcode_pdm_xy,zipcode_elevation,zipcode_Slope,zipcode_TRI,zipcode_TPI,zipcode_rough)
names(zipcode_Topo)[2:6]=c("Elevation","Slope","TRI","TPI","Roughness")
for(cols in c("Elevation","Slope","TRI","TPI","Roughness")){
  zipcode_Topo=borrow_from_ngbs(zipcode_Topo,col_name = cols)
}
save(file=here::here("Data","Medium Data","ZIP_Topo.RData"),zipcode_Topo)
load(here::here("Data","Medium Data","ZIP_Topo.RData"))
##Attaching distance to fault------------------------
fault<-st_read("/n/koutrakis_lab/lab/Group_Data/USGS_Geological_Map/US_Faults.shp")
fault=st_transform(fault,prjstring)
index=st_nearest_feature(x=zipcode_pdm_xy,y=fault)
faults=fault[index,]
dist2fault<- st_distance(x = zipcode_pdm_xy, y= faults, by_element = TRUE)
zipcode_Fault=zipcode_pdm_xy
zipcode_Fault$dist2fault=as.numeric(dist2fault)
save(file=here::here("Data","Medium Data","ZIP_Fault.RData"),zipcode_Fault)

##Attaching magnetic anomaly data----------------
mag<-raster(here::here("Data","USGS_Magnetic","Magnetic_Anomaly.tif"))
zipcode_Mag=extract(mag,zipcode_pdm_xy)
zipcode_Mag=bind_cols(zipcode_pdm_xy,zipcode_Mag)
names(zipcode_Mag)[2]="Magnetic_Anomaly"
zipcode_Mag=borrow_from_ngbs(zipcode_Mag,col_name = "Magnetic_Anomaly")
save(file=here::here("Data","Medium Data","ZIP_Mag.RData"),zipcode_Mag)

##Attaching gravity anomaly data----------------
grav_iso<-raster(here::here("Data","USGS_Gravity","USgrv_iso_SDD_geog.tif"))
proj4string(grav_iso)="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs "
zipcode_grav_iso=extract(grav_iso,zipcode_pdm_xy)
zipcode_grav_iso=bind_cols(zipcode_pdm_xy,zipcode_grav_iso)
names(zipcode_grav_iso)[2]="Gravity_Anomaly_ISO"
zipcode_grav_iso=borrow_from_ngbs(zipcode_grav_iso,col_name = "Gravity_Anomaly_ISO")

grav_bouguer<-raster(here::here("Data","USGS_Gravity","USgrv_cba_SDD_geog.tif"))
proj4string(grav_bouguer)="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs "
zipcode_grav_bouguer=extract(grav_bouguer,zipcode_pdm_xy)
zipcode_grav_bouguer=bind_cols(zipcode_pdm_xy,zipcode_grav_bouguer)
names(zipcode_grav_bouguer)[2]="Gravity_Anomaly_Bouguer"
zipcode_grav_bouguer=borrow_from_ngbs(zipcode_grav_bouguer,col_name = "Gravity_Anomaly_Bouguer")
zipcode_Grav=bind_cols(zipcode_pdm_xy,zipcode_grav_iso$Gravity_Anomaly_ISO,zipcode_grav_bouguer$Gravity_Anomaly_Bouguer)
names(zipcode_Grav)[2:3]=c("Grav_ISO","Grav_Bouguer")

save(file=here::here("Data","Medium Data","ZIP_Grav.RData"),zipcode_Grav)

##Attaching housing information----------------------
housing<-read.csv("/n/koutrakis_lab/lab/Group_Data/America Community Survey/ACS_Housing_Table.csv")
housing$Geo_ZCTA5=formatC(housing$Geo_ZCTA5,width=5,flag=0)

zips_House=zipcode_pdm_xy%>%left_join(housing,by=c("ZIPCODE"="Geo_ZCTA5"))
zips_House$geometry=NULL

#Convert absolute value to percentage
zips_House[,c("Single_Family","Two_Units_Housing","Three_Four_Units_Housing",
              "Five_Nine_Units_Housing","Ten_Nineteen_Units_Housing",
              "Over_Twenty_Units_Housing","Over_Fifty_Units_Housing")]=
  zips_House[,c("Single_Family","Two_Units_Housing","Three_Four_Units_Housing",
                "Five_Nine_Units_Housing","Ten_Nineteen_Units_Housing",
                "Over_Twenty_Units_Housing","Over_Fifty_Units_Housing")]/zips_House[,"Housing_Units"]

zips_House[,c("Units_After_2014","Units_2010_2013","Units_2000_2009","Units_1990_1999",
              "Units_1980_1989","Units_1970_1979","Units_1960_1969","Units_1950_1959",
              "Units_1940_1949","Units_Before_1939")]=
  zips_House[,c("Units_After_2014","Units_2010_2013","Units_2000_2009","Units_1990_1999",
                "Units_1980_1989","Units_1970_1979","Units_1960_1969","Units_1950_1959",
                "Units_1940_1949","Units_Before_1939")]/zips_House$Occupied_Units

zips_House[,c("One_Room_Unit","Two_Room_Unit","Three_Room_Unit","Four_Room_Unit",
              "Five_Room_Unit","Six_Room_Unit","Seven_Room_Unit","Eight_Room_Unit","Over_Nine_Room_Unit")]=
  zips_House[,c("One_Room_Unit","Two_Room_Unit","Three_Room_Unit","Four_Room_Unit",
                "Five_Room_Unit","Six_Room_Unit","Seven_Room_Unit","Eight_Room_Unit","Over_Nine_Room_Unit")]/zips_House$Occupied_Units

zips_House[,c("No_Bedroom_Unit","One_Bedroom_Unit","Two_Bedroom_Unit","Three_Bedroom_Unit",
              "Four_Bedroom_Unit","Over_Five_Bedroom_Unit")]=
  zips_House[,c("No_Bedroom_Unit","One_Bedroom_Unit","Two_Bedroom_Unit","Three_Bedroom_Unit",
                "Four_Bedroom_Unit","Over_Five_Bedroom_Unit")]/zips_House$Occupied_Units

zips_House[,c("Gas_Fuel","Electricity_Fuel","Oil_Fuel","Coal_Fuel","Solar_Fuel","No_Fuel")]=
  zips_House[,c("Gas_Fuel","Electricity_Fuel","Oil_Fuel","Coal_Fuel","Solar_Fuel","No_Fuel")]/zips_House$Occupied_Units

zips_House=zips_House[,c("ZIPCODE",
                         "Housing_Units","Single_Family","Two_Units_Housing","Three_Four_Units_Housing",
                         "Five_Nine_Units_Housing","Ten_Nineteen_Units_Housing",
                         "Over_Twenty_Units_Housing","Over_Fifty_Units_Housing",
                         "Units_After_2014","Units_2010_2013","Units_2000_2009","Units_1990_1999",
                         "Units_1980_1989","Units_1970_1979","Units_1960_1969","Units_1950_1959",
                         "Units_1940_1949","Units_Before_1939","One_Room_Unit","Two_Room_Unit","Three_Room_Unit","Four_Room_Unit",
                         "Five_Room_Unit","Six_Room_Unit","Seven_Room_Unit","Eight_Room_Unit",
                         "Over_Nine_Room_Unit","No_Bedroom_Unit","One_Bedroom_Unit","Two_Bedroom_Unit","Three_Bedroom_Unit",
                         "Four_Bedroom_Unit","Over_Five_Bedroom_Unit",
                         "Gas_Fuel","Electricity_Fuel","Oil_Fuel","Coal_Fuel","Solar_Fuel","No_Fuel")]
zips_House=zipcode_pdm_xy%>%left_join(zips_House)
zipcode_House=zips_House

save(file=here::here("Data","Medium Data","ZIP_Housing.RData"),zipcode_House)

##Attaching two spatial trends for basement and upstairs
load(file="Cleaned_Raw_Data_0220.RData")
load(here::here("Data","GeoData","ZIP_CODE_Pop_Center.RData"))

coloc_data=as.data.frame(coloc_data)
zipcode_pdm_xy$ZIPCODE=as.character(zipcode_pdm_xy$ZIPCODE)
coloc_data_sf=coloc_data%>%left_join(zipcode_pdm_xy,by=c("TestPostalCode"="ZIPCODE"))
coloc_data_sf=coloc_data_sf%>%filter(!is.na(x))

base_gam=bam(log(Conc)~s(x,y,k=300),data=coloc_data_sf%>%filter(Floor=="Basement"))
zipcode_bam_basement=predict.bam(base_gam,zipcode_pdm_xy)
zipcode_bam_basement=as.data.frame(zipcode_bam_basement)
zipcode_bam_basement=bind_cols(zipcode_pdm_xy,zipcode_bam_basement)
zipcode_bam_basement$zipcode_bam_basement=exp(zipcode_bam_basement$zipcode_bam_basement)
save(file=here::here("Data","Medium Data","ZIP_GAM_Basement.RData"),zipcode_bam_basement)

upstairs_gam=bam(log(Conc)~s(x,y,k=300),data=coloc_data_sf%>%filter(Floor!="Basement"))
zipcode_bam_upstairs=predict.bam(upstairs_gam,zipcode_pdm_xy)
zipcode_bam_upstairs=as.data.frame(zipcode_bam_upstairs)
zipcode_bam_upstairs=bind_cols(zipcode_pdm_xy,zipcode_bam_upstairs)
zipcode_bam_upstairs$zipcode_bam_upstairs=exp(zipcode_bam_upstairs$zipcode_bam_upstairs)
save(file=here::here("Data","Medium Data","ZIP_GAM_Upstairs.RData"),zipcode_bam_upstairs)

#Combine All Spatial Predictors together----------------------------------------
load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Radon_Potential.RData"))
zipcode_Rn$geometry=NULL
zipcode_geog=zipcode_pdm_xy%>%left_join(zipcode_Rn,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Surface.RData"))
zipcode_Surface$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Surface,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Soil.RData"))
zipcode_Soil$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Soil,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Uranium.RData"))
zipcode_Uranium$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Uranium,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Topo.RData"))
zipcode_Topo$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Topo,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Fault.RData"))
zipcode_Fault$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Fault,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Mag.RData"))
zipcode_Mag$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Mag,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Grav.RData"))
zipcode_Grav$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Grav,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Housing.RData"))
zipcode_House$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_House,by="ZIPCODE")

load(here::here("Data","Env_Exp.RData"))
ses_factors<-env_exp%>%filter(year==2015)
ses_factors=ses_factors%>%dplyr::select(ZIP,popdensity,medianhousevalue,medhouseholdincome,pct_owner_occ)
zipcode_geog=zipcode_geog%>%left_join(ses_factors,by=c("ZIPCODE"="ZIP"))

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_GAM_Upstairs.RData"))
zipcode_bam_upstairs=zipcode_bam_upstairs%>%dplyr::select(ZIPCODE,zipcode_bam_upstairs)
zipcode_geog=zipcode_geog%>%left_join(zipcode_bam_upstairs,by="ZIPCODE")

load(here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_GAM_Basement.RData"))
zipcode_bam_basement=zipcode_bam_basement%>%dplyr::select(ZIPCODE,zipcode_bam_basement)
zipcode_geog=zipcode_geog%>%left_join(zipcode_bam_basement,by="ZIPCODE")

save(file=here::here("Data","Medium Data","ZIP_Spatial_Predictor_All.RData"),zipcode_geog)
