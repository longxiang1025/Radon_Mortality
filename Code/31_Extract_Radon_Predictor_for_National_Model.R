library(dplyr)
library(sf)
library(raster)
library(foreign)
library(nabor)
library(stringr)

#Load general functions----------------------------------------
borrow_from_ngbs=function(zips,col_name){
  #Keep a copy of the original 
  order=zips%>%dplyr::select(ZIPCODE)
  #Get the column of with the same name
  col_index=which(colnames(zips)==col_name)
  col=zips[,col_index]
  col$geometry=NULL
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

prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
sf::sf_use_s2(FALSE)

#The first file is the population-weighted center of each zipcode.
#This file is produced by 18_Calc_Population_Center_ZIPCODE.R
load(here::here("Data","GeoData","ZIP_CODE_Pop_Center.RData"))
load(here::here("Data","GeoData","2015_Shapes.RData"))
zips_sf=st_as_sf(zips)
#The coordinates are converted to a spatial point (sf) object
##Prepare the ZIP Code data for the merging--------------------
zipcode_pdm_xy=zipcode_pdm_xy%>%filter(!is.na(x))
coordinates(zipcode_pdm_xy)=~x+y
proj4string(zipcode_pdm_xy)=prjstring
zipcode_pdm_xy=st_as_sf(zipcode_pdm_xy)

##Attaching geological radon potential--------
Rn_Potential<-st_read(here::here("Data","USGS_Rn","usagrp_polygon.shp"))
Rn_Potential=st_transform(Rn_Potential,crs=prjstring)
zipcode_Rn=st_join(x=zipcode_pdm_xy,y=Rn_Potential,join=st_within)
zipcode_Rn=zipcode_Rn%>%dplyr::select(ZIPCODE,RI)
zipcode_Rn=borrow_from_ngbs(zipcode_Rn,"RI")
save(file=here::here("Data","Medium Data","National_ZIP_Radon_Potential.RData"),zipcode_Rn)

##Attaching surfacial material types-----------
Surface=st_read("/n/koutrakis_lab/lab/Group_Data/surface_materials_data/USGS_DS_425/Surface_Materials.shp")
Surface=st_transform(Surface,crs=prjstring)
Surface$DMU_3=str_split(as.character(Surface$DMU_HIER),pattern = "-",simplify = T)[,1]
Surface=st_transform(Surface,st_crs(zips_sf))

Surface_Factor_Table=cbind.data.frame(c("001","002","003","004","005","006",
                                        "007","008","009","010","011","012","013",
                                        "Holocene","late Pleistocene",
                                        "early Pleistocene","late Pleistocene","middle Pleistocene","Pleistocene" ,"Pliocene",
                                        "<100 feet",">100 feet","Discontinuous, or patchy in distribution"),
                                    c("Alluvial","Coastal","Calcareous","Organic",
                                      "Glacial","Glaciofluvial","Proglacial","Lacustrine",
                                      "Eolian","Massmovement","Residual","Rocks","Water",
                                      "Holocene","Late_Pleistocene",
                                      "Early_Pleistocene","Late_Pleistocene","Middle_Pleistocene","Pleistocene" ,"Pliocene",
                                      "Thin","Thick","Patchy"),
                                    c(rep("DMU_3",13),rep("MIN_AGE",2),rep("MAX_AGE",5),rep("UNIT_THICK",3)))
names(Surface_Factor_Table)=c("Value","Name","Type")
ZIP_Type_Pct_List=list()
for( t in 1:nrow(Surface_Factor_Table)){
  type=Surface_Factor_Table[t,"Type"]
  value=Surface_Factor_Table[t,"Value"]
  index=Surface%>%st_drop_geometry()
  index=index[,type]==value
  type_surface=Surface[index,]%>%dplyr::select(paste0(type))
  intersect_pct <- st_intersection(sf::st_make_valid(zips_sf), sf::st_make_valid(type_surface)) %>% 
    mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
    dplyr::select(ZIP, intersect_area) %>%   # only select columns needed to merge
    st_drop_geometry()
  intersect_pct=intersect_pct%>%group_by(ZIP)%>%summarise(type_area=sum(intersect_area))
  zip_area <- mutate(zips_sf, zip_area = st_area(zips_sf))
  zip_type_pct <-zip_area%>%left_join(intersect_pct,by="ZIP")
  zip_type_pct[is.na(zip_type_pct$type_area),"type_area"]=0
  type_pct=zip_type_pct$type_area/zip_type_pct$zip_area
  ZIP_Type_Pct_List[[t]]=type_pct
  print(paste(paste0(Surface_Factor_Table[t,],collapse = " "),Sys.time()))
}
ZIP_Type_Pct=bind_cols(ZIP_Type_Pct_List)
ZIP_Type_Pct=as.data.frame(ZIP_Type_Pct)
names(ZIP_Type_Pct)=paste0("Surfacial_",Surface_Factor_Table$Type,"_",Surface_Factor_Table$Name)
ZIP_Type_Pct$ZIP=zip_area$ZIP

save(file=here::here("Data","Medium Data","National_ZIP_Surfacial_Pct.RData"),ZIP_Type_Pct)
##Attaching bedrock information--------------
sgmc_geog=st_read("/n/koutrakis_lab/lab/Group_Data/USGS_SGMC_Shapefiles/USGS_SGMC_Shapefiles/SGMC_Geology.shp")
sgmc_geog=sgmc_geog%>%st_transform(crs=geoprjstring)
sgmc_geog$AGE_MAX <- str_replace_all(sgmc_geog$AGE_MAX, " ", "")
max_geog_age_t1=str_split(sgmc_geog$AGE_MAX,"-",simplify = T)[,1]
max_geog_age_t2=str_split(sgmc_geog$AGE_MAX,"-",simplify = T)[,2]
max_geog_age_t3=str_split(sgmc_geog$AGE_MAX,"-",simplify = T)[,3]
max_geog_age=ifelse(max_geog_age_t3=="",max_geog_age_t2,max_geog_age_t3)
max_geog_age=ifelse(max_geog_age=="",max_geog_age_t1,max_geog_age)
sgmc_geog$max_age_simp=max_geog_age
max_age_list=unique(max_geog_age)
max_age_list=max_age_list[!is.na(max_age_list)]
max_age_list=max_age_list[max_age_list!="Undetermined"]

sgmc_geog$AGE_MIN <- str_replace_all(sgmc_geog$AGE_MIN, " ", "")
min_geog_age_t1=str_split(sgmc_geog$AGE_MIN,"-",simplify = T)[,1]
min_geog_age_t2=str_split(sgmc_geog$AGE_MIN,"-",simplify = T)[,2]
min_geog_age_t3=str_split(sgmc_geog$AGE_MIN,"-",simplify = T)[,3]
min_geog_age=ifelse(min_geog_age_t3=="",min_geog_age_t2,min_geog_age_t3)
min_geog_age=ifelse(min_geog_age=="",min_geog_age_t1,min_geog_age)
sgmc_geog$min_age_simp=min_geog_age
min_age_list=unique(min_geog_age)
min_age_list=min_age_list[!is.na(min_age_list)]
min_age_list=min_age_list[min_age_list!="Undetermined"]

components_list=c(sgmc_geog$MAJOR1,sgmc_geog$MAJOR2,sgmc_geog$MAJOR3)
components_list=components_list[!is.na(components_list)]
components_list=summary(as.factor(components_list))
components_list=names(components_list[components_list>5000])
components_list=components_list[components_list!="(Other)"]

Bedrock_Factor_Table=cbind.data.frame(c(max_age_list,min_age_list,components_list),
                                      c(rep("max_age_simp",length(max_age_list)),
                                        rep("min_age_simp",length(min_age_list)),
                                        rep("Major",length(components_list))))
names(Bedrock_Factor_Table)=c("Value","Type")
zip_area <- mutate(zips_sf, zip_area = st_area(zips_sf))
ZIP_Bedrock_Pct_List=list()
for( t in 1:nrow(Bedrock_Factor_Table)){
  type=Bedrock_Factor_Table[t,"Type"]
  value=Bedrock_Factor_Table[t,"Value"]
  index=sgmc_geog%>%st_drop_geometry()
  if(type=="Major"){
    index=cbind((index[,"MAJOR1"]==value),
                (index[,"MAJOR2"]==value),
                (index[,"MAJOR3"]==value))
    index=rowSums(index,na.rm=T)
    index=as.logical(index)
    type_bedrock=sgmc_geog[index,]%>%dplyr::select("MAJOR1","MAJOR2","MAJOR3")
  }else{
    index=index[,type]==value
    type_bedrock=sgmc_geog[index,]%>%dplyr::select(paste0(type))
  }
  intersect_pct <- st_intersection(sf::st_make_valid(zips_sf), sf::st_make_valid(type_bedrock)) %>% 
    mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
    dplyr::select(ZIP, intersect_area) %>%   # only select columns needed to merge
    st_drop_geometry()
  intersect_pct=intersect_pct%>%group_by(ZIP)%>%summarise(type_area=sum(intersect_area))
  zip_bedrock_pct <-zip_area%>%left_join(intersect_pct,by="ZIP")
  zip_bedrock_pct[is.na(zip_bedrock_pct$type_area),"type_area"]=0
  type_pct=zip_bedrock_pct$type_area/zip_bedrock_pct$zip_area
  ZIP_Bedrock_Pct_List[[t]]=type_pct
  print(paste(paste0(Bedrock_Factor_Table[t,],collapse = " "),Sys.time()))
}
ZIP_Bedrock_Pct=bind_cols(ZIP_Bedrock_Pct_List)
ZIP_Bedrock_Pct=as.data.frame(ZIP_Bedrock_Pct)
names(ZIP_Bedrock_Pct)=paste0("Bedrock_",Bedrock_Factor_Table$Type,"_",Bedrock_Factor_Table$Value)
ZIP_Bedrock_Pct$ZIP=zip_area$ZIP

save(file=here::here("Data","Medium Data","National_ZIP_Bedrock_Pct.RData"),ZIP_Bedrock_Pct)
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
save(file=here::here("Data","Medium Data","National_ZIP_Soil.RData"),zipcode_Soil)
##Attaching Uranium concentrations------------
uranium<-raster(here::here("Data","USGS_Radiometric","NAMrad_U1.tif"))
zipcode_Uranium=extract(uranium,zipcode_pdm_xy)
zipcode_Uranium=bind_cols(zipcode_pdm_xy,zipcode_Uranium)
names(zipcode_Uranium)[2]="Uranium"
zipcode_Uranium=borrow_from_ngbs(zipcode_Uranium,col_name = "Uranium")
save(file=here::here("Data","Medium Data","National_ZIP_Uranium.RData"),zipcode_Uranium)
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
save(file=here::here("Data","Medium Data","National_ZIP_Topo.RData"),zipcode_Topo)
##Attaching distance to fault------------------------
fault<-st_read("/n/koutrakis_lab/lab/Group_Data/USGS_Geological_Map/US_Faults.shp")
fault=st_transform(fault,st_crs(zips_sf))
zips_sf$area=st_area(zips_sf)
###Calculate the overall length of geological fault in each ZIPCODE
ints <- st_intersection(st_make_valid(zips_sf),fault) %>% 
  dplyr::mutate(len_m = sf::st_length(geometry)) %>% # returns length in meters
  dplyr::group_by(ZIP)%>%st_drop_geometry()
ints$len_m=as.numeric(ints$len_m)
ints=ints%>%group_by(ZIP)%>%summarise(all_fault_length=sum(len_m),
                                      growth_length=sum(len_m*(TYPE==1),na.rm=T),
                                      normal_length=sum(len_m*(TYPE%in%c(2,3,4)),na.rm=T),
                                      strike_length=sum(len_m*(TYPE%in%c(5,6,7)),na.rm=T),
                                      thurst_length=sum(len_m*(TYPE%in%c(8,9,10)),na.rm=T))
zips_sf=zips_sf%>%left_join(ints,by="ZIP")
zips_sf$area=as.numeric(zips_sf$area)
zips_sf=zips_sf%>%mutate(fault_density=all_fault_length/area,
                         growth_density=growth_length/area,
                         normal_density=normal_length/area,
                         strike_density=strike_length/area,
                         thurst_density=thurst_length/area)
zips_sf=zips_sf%>%mutate(fault_density=ifelse(is.na(fault_density), 0, fault_density),
                         growth_density=ifelse(is.na(growth_density),0,growth_density),
                         normal_density=ifelse(is.na(normal_density),0,normal_density),
                         strike_density=ifelse(is.na(strike_density),0,strike_density),
                         thurst_density=ifelse(is.na(thurst_density),0,thurst_density))
zipcode_Fault=zips_sf%>%st_drop_geometry()

fault=st_transform(fault,st_crs(zipcode_pdm_xy))
index=st_nearest_feature(x=zipcode_pdm_xy,y=fault)
faults=fault[index,]
dist2fault<- st_distance(x = zipcode_pdm_xy, y= faults, by_element = TRUE)
dist2fault=cbind.data.frame(zipcode_pdm_xy%>%st_drop_geometry(),as.numeric(dist2fault))
names(dist2fault)=c("ZIP","Dist2Fault")
zipcode_Fault=zipcode_Fault%>%left_join(dist2fault,by="ZIP")
zipcode_Fault=zipcode_Fault%>%mutate(Dist2Fault=ifelse(is.na(Dist2Fault),0,Dist2Fault))
zipcode_Fault=zipcode_Fault%>%dplyr::select(ZIP,fault_density,growth_density,
                                            normal_density,strike_density,thurst_density,Dist2Fault)
save(file=here::here("Data","Medium Data","National_ZIP_Fault.RData"),zipcode_Fault)

##Attaching magnetic anomaly data----------------
mag<-raster(here::here("Data","USGS_Magnetic","Magnetic_Anomaly.tif"))
zipcode_Mag=extract(mag,zipcode_pdm_xy)
zipcode_Mag=bind_cols(zipcode_pdm_xy,zipcode_Mag)
names(zipcode_Mag)[2]="Magnetic_Anomaly"
zipcode_Mag=borrow_from_ngbs(zipcode_Mag,col_name = "Magnetic_Anomaly")
save(file=here::here("Data","Medium Data","National_ZIP_Mag.RData"),zipcode_Mag)

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

save(file=here::here("Data","Medium Data","National_ZIP_Grav.RData"),zipcode_Grav)

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

save(file=here::here("Data","Medium Data","National_ZIP_Housing.RData"),zipcode_House)

#Combine All Spatial Predictors together----------------------------------------
load(here::here("Data","Medium Data","National_ZIP_Radon_Potential.RData"))
zipcode_Rn$geometry=NULL
zipcode_geog=zipcode_pdm_xy%>%left_join(zipcode_Rn,by="ZIPCODE")

load(here::here("Data","Medium Data","National_ZIP_Surfacial_Pct.RData"))
for(c in 1:(ncol(ZIP_Type_Pct)-1)){
  ZIP_Type_Pct[,c]=as.numeric(ZIP_Type_Pct[,c])
}
zipcode_geog=zipcode_geog%>%left_join(ZIP_Type_Pct,by=c("ZIPCODE"="ZIP"))

load(here::here("Data","Medium Data","National_ZIP_Bedrock_Pct.RData"))
for(c in 1:(ncol(ZIP_Bedrock_Pct)-1)){
  ZIP_Bedrock_Pct[,c]=as.numeric(ZIP_Bedrock_Pct[,c])
}
zipcode_geog=zipcode_geog%>%left_join(ZIP_Bedrock_Pct,by=c("ZIPCODE"="ZIP"))

load(here::here("Data","Medium Data","National_ZIP_Soil.RData"))
zipcode_Soil$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Soil,by="ZIPCODE")

load(here::here("Data","Medium Data","National_ZIP_Uranium.RData"))
zipcode_Uranium$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Uranium,by="ZIPCODE")

load(here::here("Data","Medium Data","National_ZIP_Topo.RData"))
zipcode_Topo$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Topo,by="ZIPCODE")

load(here::here("Data","Medium Data","National_ZIP_Fault.RData"))
zipcode_Fault$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Fault,by=c("ZIPCODE"="ZIP"))

load(here::here("Data","Medium Data","National_ZIP_Mag.RData"))
zipcode_Mag$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Mag,by="ZIPCODE")

load(here::here("Data","Medium Data","National_ZIP_Grav.RData"))
zipcode_Grav$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_Grav,by="ZIPCODE")

load(here::here("Data","Medium Data","National_ZIP_Housing.RData"))
zipcode_House$geometry=NULL
zipcode_geog=zipcode_geog%>%left_join(zipcode_House,by="ZIPCODE")

load(here::here("Data","Env_Exp.RData"))
ses_factors<-env_exp%>%filter(year==2015)
ses_factors=ses_factors%>%dplyr::select(ZIP,popdensity,medianhousevalue,medhouseholdincome,pct_owner_occ)
zipcode_geog=zipcode_geog%>%left_join(ses_factors,by=c("ZIPCODE"="ZIP"))

save(file=here::here("Data","Medium Data","National_Spatial_Predictor_All.RData"),zipcode_geog)

#
for(r in 1:250){
  save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Geog/National_ZIP_Geog_",r,".RData"),
       zipcode_geog)
}
