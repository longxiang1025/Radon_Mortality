library(dplyr)
library(readr)
library(sf)
library(lubridate)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplot2)
library(sp)
library(mgcv)
library(ggsn)
sf_use_s2(FALSE)

pattern <- function(x, size, pattern) {
  ex = list(
    horizontal = c(1, 2),
    vertical = c(1, 4),
    left2right = c(2, 4),
    right2left = c(1, 3)
  )
  fillgrid = st_make_grid(x, cellsize = size)
  endsf = lapply(1:length(fillgrid), function(j)
    sf::st_linestring(sf::st_coordinates(fillgrid[j])[ex[[pattern]], 1:2]))
  endsf = sf::st_sfc(endsf, crs = sf::st_crs(x))
  endsf = sf::st_intersection(endsf, x)
  endsf = endsf[sf::st_geometry_type(endsf)
                %in% c("LINESTRING", "MULTILINESTRING")]
  endsf = sf::st_line_merge(sf::st_union(endsf))
  return(endsf)
}

geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")

Neighbour_States=c("WV","KY","VA","CO","WV","MT","OK","AR","TN","NC")

#Process the raw table data (No need to run every time)------------------------------------

ma_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_MA_190628.csv",header=T)
ma_lab_data$TestPostalCode=as.character(ma_lab_data$TestPostalCode)

ma_lab_data2<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_20220430_MA.csv",header=F)
names(ma_lab_data2)=names(ma_lab_data)
ma_lab_data2$TestPostalCode=as.character(ma_lab_data2$TestPostalCode)
ma_lab_data=bind_rows(ma_lab_data,ma_lab_data2)

pa_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_PA_190628.csv",header=T)
pa_lab_data$TestPostalCode<-as.character(pa_lab_data$TestPostalCode)

pa_lab_data_2<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_PA_190701_201031.csv",header=T)
pa_lab_data_2$TestPostalCode<-as.character(pa_lab_data_2$TestPostalCode)

pa_lab_data_3<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_20220430_PA.csv",header=F)
pa_lab_data_3=pa_lab_data_3[,c(1,3:16)]
names(pa_lab_data_3)=names(pa_lab_data)
pa_lab_data_3$TestPostalCode<-as.character(pa_lab_data_3$TestPostalCode)

pa_lab_data=bind_rows(pa_lab_data,pa_lab_data_2,pa_lab_data_3)

as_data=bind_rows(ma_lab_data,pa_lab_data)
as_data$ID=paste0(as_data$Checksum_TestAddress,as_data$TestPostalCode)

as_data$StartDate=as.character(as_data$StartDate)
as_data$EndDate=as.character(as_data$EndDate)

as_data<-as_data%>%mutate(StartDate=ifelse(StartDate=="00/00/0000",NA,StartDate))
as_data<-as_data%>%filter(!is.na(StartDate))
as_data$StartDate<-as.Date(as_data$StartDate,"%m/%d/%Y")

as_data<-as_data%>%mutate(EndDate=ifelse(EndDate=="00/00/0000",NA,EndDate))
as_data<-as_data%>%filter(!is.na(EndDate))
as_data$EndDate<-as.Date(as_data$EndDate,"%m/%d/%Y")

as_data$PCI.L=as.numeric(as.character(as_data$Result))
as_data[is.na(as_data$PCI.L),"PCI.L"]=0

as_data=as_data[,c("StartDate","EndDate","Checksum_TestAddress","TestState","TestPostalCode","Floor","Result","Method","PCI.L","ID","DeviceNumber")]
as_data$TestState=as.character(as_data$TestState)
as_data$Floor=as.character(as_data$Floor)
as_data$Result=as.character(as_data$Result)
as_data$Method=as.character(as_data$Method)
as_data$Checksum_TestAddress=as.character(as_data$Checksum_TestAddress)
##The AirChek data is not included because of the incompatible coding of address####
nc_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/harvard_201031.csv",header=T,sep = "\t")
nc_data$POSTALCODE=as.character(nc_data$POSTALCODE)
nc_data$STARTDATE=as.character(nc_data$STARTDATE)
nc_data$ENDDATE=as.character(nc_data$ENDDATE)

nc_data<-nc_data%>%mutate(STARTDATE=ifelse(STARTDATE=="00/00/0000",NA,STARTDATE))
nc_data<-nc_data%>%filter(!is.na(STARTDATE))
nc_data$STARTDATE<-as.Date(nc_data$STARTDATE,"%m/%d/%Y")

nc_data<-nc_data%>%mutate(ENDDATE=ifelse(ENDDATE=="00/00/0000",NA,ENDDATE))
nc_data<-nc_data%>%filter(!is.na(ENDDATE))
nc_data$ENDDATE<-as.Date(nc_data$ENDDATE,"%m/%d/%Y")
nc_data$ID=paste0(nc_data$FINGERPRINT,nc_data$PostalCode)
nc_data$ZIPCODE=substr(nc_data$POSTALCODE,1,5)
nc_data$FLOOR=as.character(nc_data$FLOOR)
nc_data$Result=as.character(nc_data$PCI.L)
nc_data=nc_data[,c("STARTDATE","ENDDATE","FINGERPRINT","STATE","ZIPCODE","FLOOR","Result","METHOD","PCI.L","ID","KITNUMBER")]
names(nc_data)=names(as_data)
nc_data$Checksum_TestAddress=as.character(nc_data$Checksum_TestAddress)
nc_data$TestState=as.character(nc_data$TestState)
nc_data$Method=as.character(nc_data$Method)
nc_data$PCI.L=as.numeric(nc_data$Result)
nc_data=nc_data%>%filter(Method=="AC")
nc_data$Method=("AirChek")

lab_data=bind_rows(as_data,nc_data)
save(lab_data,file="Merged_Measurements_220610.RData")
##Only save the AC/LS/AT Measurements------------
lab_data=as_data
load(file="Merged_Measurements_201031.RData")

load(here::here("Data","GeoData","2015_Shapes.RData"))
zips=st_as_sf(zips)
zips=zips%>%left_join(zip_count,by=c("ZIP"="TestPostalCode"))
zips=zips%>%filter(STATE%in%c(state.abb[c(1,3:10,12:50)]))
zips=st_transform(zips,
                  crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
#Find short-term/long-term following short-term measurements (No need to run every time)------------------
measurement_types=lab_data%>%arrange(StartDate)%>%group_by(ID,TestPostalCode,Floor)%>%summarise(AC=sum("AC"==Method),
                                                                                                AT=sum("AT"==Method),
                                                                                                LS=sum("LS"==Method))
measurement=measurement_types%>%mutate(measurement_count=AC+AT+LS)

measurement_types=measurement_types%>%filter(AT>0&(AC>0|LS>0))
follow_data=measurement_types%>%left_join(lab_data)

follow_data=follow_data%>%group_by(ID,TestPostalCode,TestState,StartDate,EndDate,Floor,Method)%>%summarise(m=mean(PCI.L),n=length(PCI.L))%>%arrange(ID,StartDate)
# For each ID&Floor, only pick the measurements before/equal to the occurance of the first AT measurement
potential_comparison_set=unique(follow_data[,c("ID","TestPostalCode","Floor")])
comparison_data=list()
l=1
for(i in 1:nrow(potential_comparison_set)){
  ##loop through all follow-up long-term measurements, and find their matching prior short-term measurements
  all_measurements=follow_data[follow_data$ID==potential_comparison_set[i,"ID"]$ID&
                                 follow_data$Floor==potential_comparison_set[i,"Floor"]$Floor,]
  long_start_date=all_measurements[all_measurements$Method=="AT","StartDate"]
  #it's likely that more than one long-term measurements exists
  for(t in 1:nrow(long_start_date)){
    l_t=long_start_date$StartDate[t]
    s_measurements=all_measurements%>%filter(!(StartDate>l_t),
                                             Method!="AT")
    l_measurements=all_measurements%>%filter((StartDate==l_t),
                                             Method=="AT")
    
    if(nrow(s_measurements)>0){#Has initiative measurements by AC/LS
      for(s in 1:nrow(s_measurements)){
        record=cbind.data.frame(s_measurements[s,],
                                l_measurements[,c("StartDate","EndDate","Method","m")])
        names(record)=c("ID","ZIPCODE","State","Init_Start_Date","Init_End_Date","Floor","Init_Method","Init_Measurement","Init_Count",
                        "Follow_Start_Date","Follow_End_Date","Follow_Method","Follow_Measurement")
        comparison_data[[l]]=record
        l=l+1
      }
    }
  }
  print(paste(i,"Out of",nrow(potential_comparison_set)))
}

result_data=bind_rows(comparison_data)
result_data$Diff_Days=as.numeric(result_data$Follow_Start_Date-result_data$Init_Start_Date)
save(result_data,file="Long_and_Short.RData")
#The Analysis Part-------------------------------------------------
##Load Paired Data and conduct basic filtering------------------------
load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<40)
result_data$duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data=result_data%>%filter(duration>80,Diff_Days<91,duration<360)
result_data=result_data%>%filter(Init_Measurement>0,Follow_Measurement>0)
result_data$log_Init=log(37*result_data$Init_Measurement)
result_data$log_Follow=log(37*result_data$Follow_Measurement)
result_data=result_data%>%filter(Init_Count<3)
# restrict analysis to measurements by Liquid Scintilation
#result_data=result_data%>%filter(Init_Method=="LS")
##Attach other secondary information to the data----------------------
# The aim of this section is to add radon potential, humidity, temperature information to data
###Add spatial dimensions to the data---------------------------------
load(here::here("Data","GeoData","2015_Shapes.RData"))
#switch s2 off, otherwise the coordinates cannot be calculated correctly
sf_use_s2(FALSE)
zip_centroid=st_centroid(st_as_sf(zips))
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid))
names(zip_centroid)=c("ZIPCODE","Longitude","Latitude")
result_data=result_data%>%left_join(zip_centroid)
result_data=result_data%>%filter(!is.na(Longitude))
coordinates(result_data)=~Longitude+Latitude
result_data=st_as_sf(result_data)
st_crs(result_data)=st_crs(zips)
###Use the coordinates to bind radon potential-----------------------
rn_potential=shapefile(here::here("Data","USGS_Rn","usagrp_polygon.shp"))
rn_potential=st_as_sf(rn_potential)
rn_potential=st_transform(rn_potential,st_crs(result_data))
result_data=st_join(result_data,rn_potential[,c("GRP","RI")])
###Use End date and zipcodes to bind meteorological factors--------
source(here::here("Code","00_Function_Download_NARR.R"))
mete_data=list()
l=1
for(y in 2005:2019){
  for(var in c("uwnd.10m","vwnd.10m","rhum.2m","air.2m","acpcp")){
    element=download_narr_daily(var=var,year=y,points=as(result_data,"Spatial"))
    names(element)=c("Value","Date","ID")
    element$var=var
    mete_data[[l]]=element
    l=l+1
    print(paste(y,var))
  }
}
mete_data=bind_rows(mete_data)
result_mete=reshape(mete_data,timevar = "var",direction = "wide",v.names = "Value",idvar = c("ID","Date"))
result_data=result_data%>%left_join(result_mete,by=c("ID"="ID","Init_End_Date"="Date"))
save(result_data,file="Long_and_Short_Extended.RData")
##Figure 1 US Map of paired measurements-----------------------------
# Figure 1 has two panels vertically. Panel (A, up) shows the locations of pairs
# Panel (B, bottom) shows the fitted spatial trend of the ratios.
###Prepare data for Figure 1(A)---------------------------------------
load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<40)
result_data$duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data=result_data%>%filter(duration>80,Diff_Days<91,duration<360)
result_data=result_data%>%filter(Init_Measurement>0,Follow_Measurement>0)
result_data$log_Init=log(37*result_data$Init_Measurement)
result_data$log_Follow=log(37*result_data$Follow_Measurement)
result_data=result_data%>%filter(Init_Count<3)
result_data=result_data%>%filter(Init_Method=="LS")
load(here::here("Data","GeoData","2015_Shapes.RData"))
zip_centroid=st_centroid(st_as_sf(zips))
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid))
names(zip_centroid)=c("ZIPCODE","Longitude","Latitude")
f1_result_data=result_data%>%left_join(zip_centroid)
f1_result_data=f1_result_data%>%filter(!is.na(Longitude))
f1_result_data$Longitude=f1_result_data$Longitude+runif(nrow(f1_result_data),-0.05,0.05)
f1_result_data$Latitude=f1_result_data$Latitude+runif(nrow(f1_result_data),-0.05,0.05)
coordinates(f1_result_data)=~Longitude+Latitude
f1_result_data=st_as_sf(f1_result_data)

st_crs(f1_result_data)=st_crs(zips)
load(here::here("Data","GeoData","Boundaries.RData"))
load("/n/koutrakis_lab/lab/Group_Data/Basic_Geodata/Canada_boundaries.RData")
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
us_bound=st_union(bound_sf)
###Figure 1A location of pairs inside and outside study region----------
f1pa=ggplot()+
  geom_sf(data=ca_shp,fill="gray95",size=0.75)+
  geom_sf(data=bound_sf,fill="white")+
  geom_sf(data=bound_sf%>%filter(STUSPS%in%States),fill="lightgray")+
  geom_sf(data=f1_result_data%>%filter(Init_Method=="LS",Init_Measurement>0),
          aes(fill=Follow_Measurement/Init_Measurement),size=2,stroke=0.001,shape=21,color="black")+
  geom_sf(data=us_bound,fill=NA,size=0.85)+
  geom_sf(data=exclude_pattern,size=0.35,color="black")+
  geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.5)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  scale_fill_stepsn("Radon ratio between normal-life and closed-building conditions",
                    breaks = c(0.5,0.67,0.75,1,1.25,1.5,2),
                    trans="log10",
                    values = scales::rescale(c(0,0.33,0.5,0.75,1,1.25,1.5), c(0,1)),
                    limits=c(0.25,4),
                    oob = scales::squish,
                    colors = rev(RColorBrewer::brewer.pal(8,"RdBu")),
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(6, "inch"),
                                             barheight=unit(0.1, "inch")))+
  coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
           xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
           ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
  theme_bw()+
  north(x.min = st_bbox(bound_sf%>%filter(STUSPS%in%States))[1],
        x.max = st_bbox(bound_sf%>%filter(STUSPS%in%States))[3],
        y.min = st_bbox(bound_sf%>%filter(STUSPS%in%States))[2],
        y.max = st_bbox(bound_sf%>%filter(STUSPS%in%States))[4],
        location="topleft")+
  ggsn::scalebar(data=bound_sf%>%filter(STUSPS%in%States),
                 transform = F,
                 dist = 250,
                 st.dist = 0.04,
                 st.size = 3,
                 dist_unit = "km",
                 st.bottom = F,
                 border.size = 0.5,
                 box.fill = c("black","white"),
                 location="bottomright")+
  theme(
    panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
    legend.direction = "horizontal",
    legend.position="bottom",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    axis.title = element_blank(),
    legend.title = element_text(size=14,angle = 0),
    legend.text = element_text(size=12)
  )
###Prepare the data for Figure 1B----------------------------
f1_result_data$Longitude=st_coordinates(f1_result_data)[,1]
f1_result_data$Latitude=st_coordinates(f1_result_data)[,2]
g_m=gam(log_Follow-log_Init~s(Longitude,Latitude,bs="tp",k=500,fx=F),
        data=f1_result_data%>%filter(Init_Method=="LS"))
#vis.gam(g_m,view = c("Longitude","Latitude"),plot.type = "contour")
#bound_sf=bound_sf%>%filter(STUSPS%in%States)
bound_bbox=st_bbox(bound_sf)
us_grid=expand.grid(x=seq(bound_bbox[1],bound_bbox[3],20000),
                    y=seq(bound_bbox[2],bound_bbox[4],20000))
coordinates(us_grid)=~x+y
proj4string(us_grid)="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45"
us_grid=st_as_sf(us_grid)
us_grid_flat=st_transform(us_grid,crs=as.character(st_crs(bound))[1])
us_grid$Longitude=st_coordinates(us_grid_flat)[,1]
us_grid$Latitude=st_coordinates(us_grid_flat)[,2]

us_grid_pred_ratio=predict.gam(g_m,us_grid,se.fit = T)
us_grid_pred_ratio=do.call(cbind,us_grid_pred_ratio)
us_grid_pred_ratio=cbind.data.frame(us_grid,us_grid_pred_ratio)
us_grid_pred_ratio$ratio=exp(us_grid_pred_ratio$fit)
us_grid$ratio=us_grid_pred_ratio$ratio
us_grid$uncertainty=exp(us_grid_pred_ratio$se.fit*1.96)
us_grid=st_intersection(us_grid,bound_sf)

raster_vis=cbind.data.frame(st_coordinates(us_grid),us_grid$ratio,us_grid$uncertainty)
names(raster_vis)=c("x","y","ratio","uncertainty")
###Figure 1B to show the spatial pattern in the ratio---------------------
f1pb<-ggplot()+
  geom_sf(data=ca_shp,fill="gray95",size=0.75)+
  geom_sf(data=bound_sf,fill="white")+
  geom_raster(data=raster_vis%>%filter(uncertainty<sqrt(3)),aes(x=x,y=y,fill=ratio))+
  geom_tile(data=raster_vis%>%filter(uncertainty>sqrt(3)),aes(x=x,y=y),fill="gray",color="black")+
  geom_sf(data=bound_sf,fill=NA,color="darkgray",size=0.25,alpha=0.5,show.legend = F)+
  geom_contour(data=raster_vis,aes(x=x,y=y,z=ratio),color="black",size=0.25,
               breaks = c(0.5,0.67,0.75,1,1.25,1.5,2))+
  geom_sf(data=f1_result_data%>%filter(State%in%States),size=0.25,shape=3)+
  geom_sf(data=us_bound,fill=NA,size=0.85)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  geom_sf(data=exclude_pattern,size=0.35,color="black")+
  geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  scale_fill_stepsn("Ratio between long- and short-term radon measurement",
                    breaks = c(0.5,0.67,0.75,1,1.25,1.5,2),
                    trans="log10",
                    values = scales::rescale(c(0,0.33,0.5,0.75,1,1.25,1.5), c(0,1)),
                    limits=c(0.25,4),
                    oob = scales::squish,
                    colors = rev(RColorBrewer::brewer.pal(8,"RdBu")),
                    guide = guide_colorsteps(direction = "vertical",
                                             title.position = "right",
                                             label.position = "right",
                                             barwidth = unit(0.1, "inch"),
                                             barheight=unit(4, "inch")))+
  coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
           xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
           ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
  theme_bw()+
  theme(legend.position = "right",
        panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
        axis.title = element_blank(),
        legend.background = element_rect(fill="white",color="black",size=0.25),
        legend.title = element_text(size=14,angle = -90),
        legend.text = element_text(size=12),
        legend.margin = margin(0.06,0.1,0.06,0.1,"in"))
###Combine two panels together vertically----------------------------
f1=cowplot::plot_grid(f1pa,f1pb,ncol = 1,labels = c("A","B"))
cowplot::save_plot("Figure1_Location_of_Pairs.pdf",f1,base_height = 8,base_width = 8)
##Revised Figure 1---------------------------------------------------
# Revised Figure 1 has two vertical panels. Panel A (Up) shows the pattern of short-term measurements
# Panel B (bottom) shows the pattern of long-term measurements
###Prepare data for Revised Figure 1(A)--------------------------------------
load(here::here("Data","GeoData","2015_Shapes.RData"))
zip_centroid=st_centroid(st_as_sf(zips))
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid))
names(zip_centroid)=c("ZIPCODE","Longitude","Latitude")

load(here::here("Data","GeoData","Boundaries.RData"))
load("/n/koutrakis_lab/lab/Group_Data/Basic_Geodata/Canada_boundaries.RData")
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
us_bound=st_union(bound_sf)
load("Merged_Measurements_201031.RData")
f1a_data=lab_data%>%filter(TestState%in%c(States,Neighbour_States),
                           Method!="AT",
                           PCI.L<30)
f1a_data=f1a_data%>%left_join(zip_centroid,by=c("TestPostalCode"="ZIPCODE"))
f1a_data=f1a_data%>%filter(!is.na(Longitude))
#f1a_data$Longitude=f1a_data$Longitude+runif(nrow(f1a_data),-0.05,0.05)
#f1a_data$Latitude=f1a_data$Latitude+runif(nrow(f1a_data),-0.05,0.05)
#f1a_gam=bam(PCI.L~s(Longitude,Latitude,k=200),data = f1a_data)
f1a_gam=bam(PCI.L~s(Longitude,Latitude,k=200),data = f1a_data%>%filter((month(EndDate))%in%c(6,7,8)))
# It took about 17 mins to fit the bam model
#save(file="f1a_gam.RData",f1a_gam)
load("f1a_gam.RData")
bound_bbox=st_bbox(bound_sf)
us_grid=expand.grid(x=seq(bound_bbox[1],bound_bbox[3],20000),
                    y=seq(bound_bbox[2],bound_bbox[4],20000))
coordinates(us_grid)=~x+y
proj4string(us_grid)="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45"
us_grid=st_as_sf(us_grid)
us_grid_flat=st_transform(us_grid,crs=as.character(st_crs(bound))[1])
us_grid$Longitude=st_coordinates(us_grid_flat)[,1]
us_grid$Latitude=st_coordinates(us_grid_flat)[,2]

us_grid_short=predict.gam(f1a_gam,us_grid,se.fit = T)
us_grid_short=do.call(cbind,us_grid_short)
us_grid_short=cbind.data.frame(us_grid,us_grid_short)
us_grid$fit=us_grid_short$fit
us_grid$se=us_grid_short$se.fit
us_grid=st_intersection(us_grid,bound_sf)

extent_bound=bound_sf%>%filter(STUSPS%in%States)
extent_bound=st_union(extent_bound)
exclude_region=bound_sf%>%filter(!STUSPS%in%States)
exclude_region=st_union(exclude_region)
exclude_pattern = 
  pattern(exclude_region, 100000, "left2right")

raster_vis_1a=cbind.data.frame(st_coordinates(us_grid),us_grid$fit,us_grid$se)
names(raster_vis_1a)=c("x","y","fit","se")
###Create Revised Figure 1(A)-----------------------------------
f1a=ggplot()+
      geom_sf(data=ca_shp,fill="gray95",size=0.75)+
      geom_sf(data=bound_sf,fill="white")+
      geom_raster(data=raster_vis_1a%>%filter(se<1),aes(x=x,y=y,fill=fit))+
      geom_tile(data=raster_vis_1a%>%filter(se>1),aes(x=x,y=y),fill="gray",color="black")+
      geom_sf(data=bound_sf,fill=NA,color="darkgray",size=0.25,alpha=0.5,show.legend = F)+
      geom_contour(data=raster_vis_1a,aes(x=x,y=y,z=fit),color="black",size=0.25,
               breaks = c(0.5,1,2,3,4,5,6,7))+
      #geom_sf(data=f1_result_data%>%filter(State%in%States),size=0.25,shape=3)+
      geom_sf(data=us_bound,fill=NA,size=0.85)+
      geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
      geom_sf(data=exclude_pattern,size=0.35,color="black")+
      geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
      geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
      scale_fill_stepsn("Short-Term Radon Concentration",
                        breaks = c(0.5,1,2,3,4,5,6,7),
                        values = c(0,0.1,0.2,0.3,0.5,0.6,0.7,0.8,1),
                        limits=c(0,8),
                        oob = scales::squish,
                        colors = rev(RColorBrewer::brewer.pal(11,"RdBu")),
                        guide = guide_colorsteps(direction = "vertical",
                                                 title.position = "right",
                                                 label.position = "right",
                                                 barwidth = unit(0.1, "inch"),
                                                 barheight=unit(4, "inch")))+
      coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
               xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
               ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
      theme_bw()+
      theme(legend.position = "left",
            panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
            axis.title = element_blank(),
            legend.background = element_rect(fill="white",color="black",size=0.25),
            legend.title = element_text(size=14,angle = -90),
            legend.text = element_text(size=12),
            legend.margin = margin(0.06,0.1,0.06,0.1,"in"))

f1a
###Prepare data for Revised Figure 1(B)--------------------------------------
load(here::here("Data","GeoData","2015_Shapes.RData"))
zip_centroid=st_centroid(st_as_sf(zips))
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid))
names(zip_centroid)=c("ZIPCODE","Longitude","Latitude")

load(here::here("Data","GeoData","Boundaries.RData"))
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
us_bound=st_union(bound_sf)
load("Merged_Measurements_201031.RData")
f1b_data=lab_data%>%filter(TestState%in%c(States,Neighbour_States),
                           Method=="AT",
                           PCI.L<30)
f1b_data=f1b_data%>%left_join(zip_centroid,by=c("TestPostalCode"="ZIPCODE"))
f1b_data=f1b_data%>%filter(!is.na(Longitude))
f1b_gam=bam(PCI.L~s(Longitude,Latitude,k=200),data = f1b_data)
bound_bbox=st_bbox(bound_sf)
us_grid=expand.grid(x=seq(bound_bbox[1],bound_bbox[3],20000),
                    y=seq(bound_bbox[2],bound_bbox[4],20000))
coordinates(us_grid)=~x+y
proj4string(us_grid)="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45"
us_grid=st_as_sf(us_grid)
us_grid_flat=st_transform(us_grid,crs=as.character(st_crs(bound))[1])
us_grid$Longitude=st_coordinates(us_grid_flat)[,1]
us_grid$Latitude=st_coordinates(us_grid_flat)[,2]

us_grid_long=predict.gam(f1b_gam,us_grid,se.fit = T)
us_grid_long=do.call(cbind,us_grid_long)
us_grid_long=cbind.data.frame(us_grid,us_grid_long)
us_grid$fit=us_grid_long$fit
us_grid$se=us_grid_long$se.fit
us_grid=st_intersection(us_grid,bound_sf)

raster_vis_1b=cbind.data.frame(st_coordinates(us_grid),us_grid$fit,us_grid$se)
names(raster_vis_1b)=c("x","y","fit","se")
###Create Revised Figure 1(B)-----------------------------------
f1b=ggplot()+
  geom_sf(data=ca_shp,fill="gray95",size=0.75)+
  geom_sf(data=bound_sf,fill="white")+
  geom_raster(data=raster_vis_1b%>%filter(se<1),aes(x=x,y=y,fill=fit))+
  geom_tile(data=raster_vis_1b%>%filter(se>1),aes(x=x,y=y),fill="gray",color="black")+
  geom_sf(data=bound_sf,fill=NA,color="darkgray",size=0.25,alpha=0.5,show.legend = F)+
  geom_contour(data=raster_vis_1b,aes(x=x,y=y,z=fit),color="black",size=0.25,
               breaks = c(0.5,1,2,3,4,5,6,7))+
  #geom_sf(data=f1_result_data%>%filter(State%in%States),size=0.25,shape=3)+
  geom_sf(data=us_bound,fill=NA,size=0.85)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  geom_sf(data=exclude_pattern,size=0.35,color="black")+
  geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  scale_fill_stepsn("Long-Term Radon Concentration",
                    breaks = c(0.5,1,2,3,4,5,6,7),
                    values = c(0,0.1,0.2,0.3,0.5,0.6,0.7,0.8,1),
                    limits=c(0,8),
                    oob = scales::squish,
                    colors = rev(RColorBrewer::brewer.pal(11,"RdBu")),
                    guide = guide_colorsteps(direction = "vertical",
                                             title.position = "right",
                                             label.position = "right",
                                             barwidth = unit(0.1, "inch"),
                                             barheight=unit(4, "inch")))+
  coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
           xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
           ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
  theme_bw()+
  theme(legend.position = "left",
        panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
        axis.title = element_blank(),
        legend.background = element_rect(fill="white",color="black",size=0.25),
        legend.title = element_text(size=14,angle = -90),
        legend.text = element_text(size=12),
        legend.margin = margin(0.06,0.1,0.06,0.1,"in"))

cowplot::plot_grid(f1a,f1b,nrow=2)

print(f1a, 
      vp=viewport(0, 0,angle = 90 ,height = unit(0.8, "npc"),width = 0.5, just = c('left', 'top')))

print(f1b, 
      vp=viewport(0.5, 0, angle = 90, height = unit(0.8, "npc"), width = 0.5, just = c('left', 'top')))


##Figure 2 Histogram of logged Init and Follow Measurements----------
result_data=result_data%>%filter(State%in%States)
Init_data=as.data.frame(result_data[,c("Follow_Measurement_Adjust")])
names(Init_data)="Measurement"
Init_data$type="Init"
Follow_data=as.data.frame(result_data[,c("Follow_Measurement")])
names(Follow_data)="Measurement"
Follow_data$type="Follow"
hist_data=rbind.data.frame(Init_data,Follow_data)

f2=ggplot(data=hist_data)+
  geom_histogram(aes(x=37*Measurement,y=37*..density..,fill=type),color="darkgray",position = "dodge",binwidth=37,alpha=0.75)+
  geom_density(aes(x=37*Measurement,y=37*..density..,color=type),size=2)+
  geom_histogram(aes(x=37*Measurement,y=37*..density..,group=type),color="black",fill=NA,position = "dodge",binwidth=37,size=0.25,show.legend = F)+
  geom_vline(aes(xintercept=4*37),size=1.75,linetype="dashed")+
  scale_fill_manual("Type of Measurement",
                    breaks = c("Init","Follow"),
                    values=c("#f4a582","#92c5de"),
                    labels=c("Closed-building concentration (Est)",
                             "Normal-life concentration (Obs)"))+
  scale_color_manual("Type of Measurement",
                     breaks = c("Init","Follow"),
                     values=c("#ca0020","#0571b0"),
                     labels=c("Closed-building concentration (Est)",
                              "Normal-life concentration (Obs)"))+
  coord_cartesian(xlim=c(0,750))+
  ylab("Probability")+
  xlab("Radon Concentration (Bq/m3)")+
  theme_bw()+
  guides(fill = guide_legend(byrow = TRUE))+
  theme(legend.position = c(0.7,0.8),
        legend.background = element_rect(fill="gray90",size=0.25,color="black"),
        legend.key = element_rect(color = NA, fill = NA),
        legend.spacing.y = unit(0.2, "in"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13)
  )
ggsave("Figure2_Histograms.pdf",f2,height = 6,width = 8,units = "in")
t.test(result_data$log_Init,result_data$log_Follow,paired = T)

##Figure 3 to show the seasonal trend in radon measurements-------------
load(file = "Long_and_Short_Extended.RData")
load(file="Merged_Measurements_201031.RData")
load(here::here("Data","GeoData","2015_Shapes.RData"))
zips_sf=st_as_sf(zips)
zip_centroid_longlat=st_centroid(zips_sf)
zip_centroid_longlat=cbind.data.frame(zip_centroid_longlat$ZIP,st_coordinates(zip_centroid_longlat))
names(zip_centroid_longlat)=c("ZIPCODE","Longitude","Latitude")
lab_data=lab_data%>%left_join(zip_centroid_longlat,by=c("TestPostalCode"="ZIPCODE"))
###Figure3A to show the seasonal trend of short-term measurements--------
short_measurements=lab_data%>%filter(PCI.L<40,Method!="AT",PCI.L>0)
short_measurements$doy=yday(short_measurements$EndDate)
short_trend=short_measurements%>%group_by(yday(EndDate))%>%
  summarise(md=median(PCI.L),up=quantile(PCI.L,0.75),bottom=quantile(PCI.L,0.25),mn=geoMean(PCI.L),sd=geoSD(PCI.L),n=length(PCI.L))
short_trend=short_trend[1:366,]
names(short_trend)=c("doy","median_Rn","up_Rn","low_Rn","mean_Rn","sd","n")
short_trend$sd=short_trend$sd/sqrt(short_trend$n)
short_trend$l_ci=short_trend$mean_Rn*(1-1.96*short_trend$sd)
short_trend$u_ci=short_trend$mean_Rn*(1+1.96*short_trend$sd)

sec_ratio=200
f3a=ggplot(data=short_trend[1:365,])+
  geom_segment(aes(x=doy,xend=doy,y=l_ci,yend=u_ci),color="darkgray",lineend = "butt",size=0.25)+
  geom_point(aes(x=doy,y=mean_Rn),size=0.75,color="navy")+
  geom_point(aes(x=doy,y=l_ci),size=0.35)+
  geom_point(aes(x=doy,y=u_ci),size=0.35)+
  geom_smooth(aes(x=doy,y=mean_Rn),method = "gam",color="#B22234")+
  geom_histogram(aes(x=doy,y=..density..*sec_ratio),bins = 50,data=short_measurements,fill="white",color="black")+
  scale_x_continuous(breaks = seq(15,365,30),
                     labels = month.abb,
                     limits = c(1,365))+
  scale_y_continuous(name="Short-term Radon Measurement (Bq/m3)",
                     breaks = seq(0,5,1),
                     labels = 37*seq(0,5,1),
                     sec.axis = sec_axis(name="Percent of Measurement",~./16,
                                         breaks = c(0.05,0.10,0.15,0.20),
                                         labels = c("5%","10%","15%","20%")))+
  coord_cartesian(clip="on",xlim =c(-5,370),ylim = c(0,4),expand =F)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size=11),
        axis.title.y = element_text(size=12),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_blank())
f3a
###Figure3B to show the seasonal trend of long-term measurements---------------------
# The purpose of this adjustment is to predict the normal short-term measurements
# for those conducted under "closed building" conditions
###Impute long-term measurement to each day and calculate the ratio
long_measurement=lab_data%>%filter(Method=="AT",PCI.L<20,PCI.L>0,TestState%in%States,!is.na(EndDate),!is.na(StartDate),(as.numeric(EndDate-StartDate))<150,(as.numeric(EndDate-StartDate))>89)
long_list=list()
for(i in 1:nrow(long_measurement)){
  t_data=long_measurement[i,]
  t_duration=t_data$StartDate:t_data$EndDate
  t_duration=as.Date("1970-01-01")+t_duration
  t_yday=yday(t_duration)
  t_yday=cbind.data.frame(t_yday,t_data$PCI.L)
  names(t_yday)=c("doy","Conc")
  long_list[[i]]=t_yday
  if(i%%1000==0){
    print(i)
  }
}
long_list=bind_rows(long_list)
long_trend=long_list%>%group_by(doy)%>%summarise(median=median(Conc),
                                                 low=quantile(Conc,0.25),up=quantile(Conc,0.75),gm=geoMean(Conc),gsd=geoSD(Conc),n=length(Conc))
names(long_trend)=c("doy","median_Rn","up_Rn","low_Rn","mean_Rn","sd","n")
long_trend$sd=long_trend$sd/sqrt(long_trend$n)
long_trend$l_ci=long_trend$mean_Rn*(1-1.96*long_trend$sd)
long_trend$u_ci=long_trend$mean_Rn*(1+1.96*long_trend$sd)

f3b=ggplot(data=long_trend[1:365,])+
  geom_segment(aes(x=doy,xend=doy,y=l_ci,yend=u_ci),color="darkgray",lineend = "butt",size=0.25)+
  geom_point(aes(x=doy,y=mean_Rn),size=0.75,color="navy")+
  geom_point(aes(x=doy,y=l_ci),size=0.35)+
  geom_point(aes(x=doy,y=u_ci),size=0.35)+
  geom_smooth(aes(x=doy,y=mean_Rn),method = "gam",color="#B22234")+
  geom_histogram(aes(x=doy,y=..density..*sec_ratio),bins = 50,data=long_list,fill="white",color="black")+
  scale_x_continuous(breaks = seq(15,365,30),
                     labels = month.abb,
                     limits = c(1,365))+
  scale_y_continuous(name="Short-term Radon Measurement (Bq/m3)",
                     breaks = seq(0,5,1),
                     labels = 37*seq(0,5,1),
                     sec.axis = sec_axis(name="Percent of Measurement",~./16,
                                         breaks = c(0.05,0.10,0.15,0.20),
                                         labels = c("5%","10%","15%","20%")))+
  coord_cartesian(clip="on",xlim =c(-5,370),ylim = c(0,4),expand =F)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size=11),
        axis.title.y = element_text(size=12),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank())
f3b
f3=plot_grid(f3a,f3b,nrow=1,labels = c("A","B"))
cowplot::save_plot("Figure3_Trends.pdf",f3,base_width = 12,base_height = 9)

##Calculate the nearby gross-beta, as requested by the reviewer of JAWMA--------
load(file="Long_and_Short_Extended.RData")
result_data$Longitude=st_coordinates(result_data)[,1]
result_data$Latitude=st_coordinates(result_data)[,2]
result_data_area=result_data%>%filter(State%in%c(States,Neighbour_States))
result_data_region=result_data%>%filter(State%in%States)

load("/n/koutrakis_lab/lab/One_Table_Group/Data/Beta_Measurements/Beta_Measurements.RData")
radnet_coords=read.csv("/n/koutrakis_lab/lab/One_Table_Group/Data/RadNet_Locations.csv")
coordinates(radnet_coords)=~Long+Lat
proj4string(radnet_coords)=geoprjstring
radnet_sf=st_as_sf(radnet_coords)
radnet_sf=radnet_sf%>%mutate(Location=paste0(City,", ",State))

###search through the beta measurements for nearest obs during the follow-up measurement-----
#create circular buffers for collocates subsets
collocated_buffer=st_buffer(result_data_region,dist = 350000)
#use the buffer to select radnet sites within the buffer
collocated_gross_beta=st_intersects(collocated_buffer,radnet_sf)
#for each collocated pair, extract the gross beta based on the index in the list
collocated_beta_list=list()
for(i in 1:length(collocated_gross_beta)){
  rad_list=radnet_sf[collocated_gross_beta[[i]],]
  beta_list=beta_measurements%>%filter(Location%in%rad_list$Location)
  rad_range=result_data_region[i,c("Follow_Start_Date","Follow_End_Date")]
  beta_list=beta_list%>%dplyr::filter(starting_date>rad_range$Follow_Start_Date,
                                      starting_date<rad_range$Follow_End_Date)
  collocated_beta_list[[i]]=mean(beta_list$Result)
}
collocated_beta_list=unlist(collocated_beta_list)
result_data_region$Beta=collocated_beta_list
save(result_data_region,file="Long_and_Short_Extended_Beta.RData")
##Calculate the adjusted short-term measurements-------------------
load(file="Long_and_Short_Extended_Beta.RData")
result_data_region$Longitude=st_coordinates(result_data_region)[,1]
result_data_region$Latitude=st_coordinates(result_data_region)[,2]
#result_data_area=result_data%>%filter(State%in%c(States,Neighbour_States))
#result_data_region=result_data%>%filter(State%in%States)

load(file="Merged_Measurements_201031.RData")
load(here::here("Data","GeoData","2015_Shapes.RData"))
zips_sf=st_as_sf(zips)
zip_centroid_longlat=st_centroid(zips_sf)
zip_centroid_longlat=cbind.data.frame(zip_centroid_longlat$ZIP,st_coordinates(zip_centroid_longlat))
names(zip_centroid_longlat)=c("ZIPCODE","Longitude","Latitude")
lab_data=lab_data%>%left_join(zip_centroid_longlat,by=c("TestPostalCode"="ZIPCODE"))
short_measurements=lab_data%>%filter(PCI.L<40,Method!="AT")

doy_adjustment=function(short_date,start_date,end_date){
  #The objective of this function is to convert a short-term measurement to the average concentration
  # of the follow-up period
  day_imp=lubridate::interval(start=start_date,end=end_date)
  day_imp=start_date+0:as.numeric(day_imp,"day")
  day_imp=yday(day_imp)
  day_imp=as.data.frame(day_imp)
  names(day_imp)="doy"
  day_imp=day_imp%>%left_join(short_trend)
  day_init=short_trend[yday(short_date),]
  result=c(mean(day_imp[1:nrow(day_imp),"mean_Rn"])/day_init$mean_Rn,
           sd(day_imp[1:nrow(day_imp),"mean_Rn"])/day_init$mean_Rn)
}

doy_adjustment_2=function(short_date,start_date,end_date,long,lat){
  #The objective of this function is to convert a short-term measurement to the average concentration
  # of the follow-up period
  day_imp=lubridate::interval(start=start_date,end=end_date)
  day_imp=start_date+0:as.numeric(day_imp,"day")
  day_imp=yday(day_imp)
  day_imp=as.data.frame(day_imp)
  names(day_imp)="doy"
  #Only use the short trend of local measurements#
  short_trend_local=short_measurements%>%filter(between(Longitude,long-1,long+1),
                                                between(Latitude,lat-1,lat+1))
  short_trend_local$doy=yday(short_trend_local$EndDate)
  #ggplot(data=short_trend_local)+
  #  geom_point(aes(x=doy,y=median_Rn))+
  #  geom_smooth(aes(x=doy,y=median_Rn))
  short_trend_gam=gam(PCI.L~s(doy,bs="cc",k=30),data=short_trend_local)
  short_trend_local=seq(1:366)
  short_trend_local=as.data.frame(short_trend_local)
  names(short_trend_local)="doy"
  short_trend_local=predict.gam(short_trend_gam,short_trend_local)
  short_trend_local=as.data.frame(short_trend_local)
  names(short_trend_local)="mean_Rn"
  short_trend_local$doy=1:366
  day_imp=day_imp%>%left_join(short_trend_local)
  day_init=short_trend_local[short_trend_local$doy==yday(short_date),]
  result=c(mean(day_imp[1:nrow(day_imp),"mean_Rn"])/day_init$mean_Rn,
           sd(day_imp[1:nrow(day_imp),"mean_Rn"])/day_init$mean_Rn)
}
season_prop=function(start_date,end_date){
  #The objective of this function is to calcualte the proportion of long-term measurement in each season
  day_imp=lubridate::interval(start=start_date,end=end_date)
  day_imp=start_date+0:as.numeric(day_imp,"day")
  day_imp=yday(day_imp)
  day_imp=as.data.frame(day_imp)
  names(day_imp)="doy"
  day_imp$season=cut(day_imp$doy,
                     breaks=c(0,59,155,246,337,366),
                     labels = c("Winter","Spring","Summer","Autumn","Winter"))
  return(summary(as.factor(day_imp$season))/nrow(day_imp))
}

#ggsave("Figure3_Trend_Of_Short_Term_Measurements.pdf",height = 6,width = 8,units = "in")


result_adj=mapply(FUN = doy_adjustment_2,
                  short_date=result_data_region$Init_End_Date,
                  start_date=result_data_region$Follow_Start_Date,
                  end_date=result_data_region$Follow_End_Date,
                  long=result_data_region$Longitude,
                  lat=result_data_region$Latitude)

result_adj=as.data.frame(t(result_adj))
names(result_adj)=c("Adj_Est","Adj_Sd")
result_data_region=cbind.data.frame(result_data_region,result_adj)
result_data_region$Follow_Measurement_Adjust=
  result_data_region$Init_Measurement*result_data_region$Adj_Est
result_data_region$log_Follow_Adj=log(37*result_data_region$Follow_Measurement_Adjust)

season_att=mapply(FUN=season_prop,
                  start_date=result_data_region$Follow_Start_Date,
                  end_date=result_data_region$Follow_End_Date)
season_att=as.data.frame(t(season_att))
result_data_region=cbind.data.frame(result_data_region,season_att)

#result_data_area$region=NA

save(file="Short_and_Long_With_Adj_and Covariates.RData",result_data_region)
##Run some exploratory analysis to see the correlation between short-term measurement and predictors-----
# on average normal concentration is ~18% lower than the closed door measurement
t.test(x=result_data_region$log_Follow,y=result_data_region$log_Follow_Adj,paired = T)

##Table to summaries the short-term and predicted radon measurement--------------------------------------
s_all=result_data%>%summarise(n=length(Init_Measurement),
                              per=length(Init_Measurement)/nrow(result_data),
                              m=median(Init_Measurement),
                              first_q=quantile(Init_Measurement,0.25),
                              third_q=quantile(Init_Measurement,0.75),
                              ma=median(Init_Measurement_Adjust),
                              first_qa=quantile(Init_Measurement_Adjust,0.25),
                              third_qa=quantile(Init_Measurement_Adjust,0.75),
                              m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                              m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                              m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))
s_all=cbind.data.frame("All",s_all)

s_region=result_data%>%group_by(region)%>%summarise(n=length(Init_Measurement),
                                                    per=length(Init_Measurement)/nrow(result_data),
                                                    m=median(Init_Measurement),
                                                    first_q=quantile(Init_Measurement,0.25),
                                                    third_q=quantile(Init_Measurement,0.75),
                                                    ma=median(Init_Measurement_Adjust),
                                                    first_qa=quantile(Init_Measurement_Adjust,0.25),
                                                    third_qa=quantile(Init_Measurement_Adjust,0.75),
                                                    m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                                                    m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                                                    m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))

s_potential=result_data%>%filter(!is.na(GRP))%>%group_by(GRP)%>%summarise(n=length(Init_Measurement),
                                                                          per=length(Init_Measurement)/nrow(result_data),
                                                                          m=median(Init_Measurement),
                                                                          first_q=quantile(Init_Measurement,0.25),
                                                                          third_q=quantile(Init_Measurement,0.75),
                                                                          ma=median(Init_Measurement_Adjust),
                                                                          first_qa=quantile(Init_Measurement_Adjust,0.25),
                                                                          third_qa=quantile(Init_Measurement_Adjust,0.75),
                                                                          m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                                                                          m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                                                                          m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))


s_season=result_data%>%group_by(Season)%>%summarise(n=length(Init_Measurement),
                                                    per=length(Init_Measurement)/nrow(result_data),
                                                    m=median(Init_Measurement),
                                                    first_q=quantile(Init_Measurement,0.25),
                                                    third_q=quantile(Init_Measurement,0.75),
                                                    ma=median(Init_Measurement_Adjust),
                                                    first_qa=quantile(Init_Measurement_Adjust,0.25),
                                                    third_qa=quantile(Init_Measurement_Adjust,0.75),
                                                    m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                                                    m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                                                    m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))
result_data$Duration_Cat=cut(result_data$duration,
                             breaks = quantile(result_data$duration,c(0,0.25,0.5,0.75,1)))
s_duration=result_data%>%filter(!is.na(Duration_Cat))%>%group_by(Duration_Cat)%>%summarise(n=length(Init_Measurement),
                                                                                           per=length(Init_Measurement)/nrow(result_data),
                                                                                           m=median(Init_Measurement),
                                                                                           first_q=quantile(Init_Measurement,0.25),
                                                                                           third_q=quantile(Init_Measurement,0.75),
                                                                                           ma=median(Init_Measurement_Adjust),
                                                                                           first_qa=quantile(Init_Measurement_Adjust,0.25),
                                                                                           third_qa=quantile(Init_Measurement_Adjust,0.75),
                                                                                           m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                                                                                           m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                                                                                           m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))


result_data$Temp_Cat=cut(result_data$Value.air.2m,
                         breaks = quantile(result_data$Value.air.2m,c(0,0.25,0.5,0.75,1)))
s_temp=result_data%>%filter(!is.na(Temp_Cat))%>%group_by(Temp_Cat)%>%summarise(n=length(Init_Measurement),
                                                                               per=length(Init_Measurement)/nrow(result_data),
                                                                               m=median(Init_Measurement),
                                                                               first_q=quantile(Init_Measurement,0.25),
                                                                               third_q=quantile(Init_Measurement,0.75),
                                                                               ma=median(Init_Measurement_Adjust),
                                                                               first_qa=quantile(Init_Measurement_Adjust,0.25),
                                                                               third_qa=quantile(Init_Measurement_Adjust,0.75),
                                                                               m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                                                                               m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                                                                               m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))
result_data$Rhum_Cat=cut(result_data$Value.rhum.2m,
                         breaks = quantile(result_data$Value.rhum.2m,c(0,0.25,0.5,0.75,1)))
s_rhum=result_data%>%filter(!is.na(Rhum_Cat))%>%group_by(Rhum_Cat)%>%summarise(n=length(Init_Measurement),
                                                                               per=length(Init_Measurement)/nrow(result_data),
                                                                               m=median(Init_Measurement),
                                                                               first_q=quantile(Init_Measurement,0.25),
                                                                               third_q=quantile(Init_Measurement,0.75),
                                                                               ma=median(Init_Measurement_Adjust),
                                                                               first_qa=quantile(Init_Measurement_Adjust,0.25),
                                                                               third_qa=quantile(Init_Measurement_Adjust,0.75),
                                                                               m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                                                                               m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                                                                               m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))

result_data$Wind_Cat=cut(result_data$wind_velocity,
                         breaks = quantile(result_data$wind_velocity,c(0,0.25,0.5,0.75,1)))
s_wind=result_data%>%filter(!is.na(Wind_Cat))%>%group_by(Wind_Cat)%>%summarise(n=length(Init_Measurement),
                                                                               per=length(Init_Measurement)/nrow(result_data),
                                                                               m=median(Init_Measurement),
                                                                               first_q=quantile(Init_Measurement,0.25),
                                                                               third_q=quantile(Init_Measurement,0.75),
                                                                               ma=median(Init_Measurement_Adjust),
                                                                               first_qa=quantile(Init_Measurement_Adjust,0.25),
                                                                               third_qa=quantile(Init_Measurement_Adjust,0.75),
                                                                               m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                                                                               m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                                                                               m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))


s_base=result_data%>%group_by(Floor=="Basement")%>%summarise(n=length(Init_Measurement),
                                                             per=length(Init_Measurement)/nrow(result_data),
                                                             m=median(Init_Measurement),
                                                             first_q=quantile(Init_Measurement,0.25),
                                                             third_q=quantile(Init_Measurement,0.75),
                                                             ma=median(Init_Measurement_Adjust),
                                                             first_qa=quantile(Init_Measurement_Adjust,0.25),
                                                             third_qa=quantile(Init_Measurement_Adjust,0.75),
                                                             m_ratio=median(Init_Measurement_Adjust/Init_Measurement,na.rm=T),
                                                             m_ratio_fq=quantile(Init_Measurement_Adjust/Init_Measurement,0.25,na.rm=T),
                                                             m_ratio_tq=quantile(Init_Measurement_Adjust/Init_Measurement,0.75,na.rm=T))

names(s_all)[1]="Variable"
s_all$Type="All"
names(s_region)[1]="Variable"
s_region$Type="Region"
names(s_potential)[1]="Variable"
s_potential$Variable=as.factor(s_potential$Variable)
s_potential$Type="Potential"
names(s_season)[1]="Variable"
s_season$Type="Season"
names(s_duration)[1]="Variable"
s_duration$Type="Duration"
names(s_temp)[1]="Variable"
s_temp$Type="Temperature"
names(s_rhum)[1]="Variable"
s_rhum$Type="Humidity"
names(s_wind)[1]="Variable"
s_wind$Type="Wind_Velocity"
names(s_base)[1]="Variable"
s_base$Type="Basement"
s_base$Variable=as.factor(s_base$Variable)
s_duration$Type="Duration"
names(s_duration)[1]="Variable"

table1=rbind.data.frame(s_all,s_region,s_potential,s_duration,s_season,s_temp,s_rhum,s_wind,s_base)
write.csv(x=table1,file="Table1.csv")

m_all=lm(log_Init~log_Init_Adj+duration+GRP+Value.rhum.2m+I(Value.air.2m-273)+Value.acpcp+wind_velocity+I(Floor=="Basement"),
         data=result_data_study)
summary(m_all)


##Regression Models-----------------------------------------------------------------------------------
# A GAM model is fitted here to predict short-term radon (logged) with adjusted long-term radon (logged), radon potential
# season of the year, and more importantly, the spatial trend smooth surface.
load(file="Short_and_Long_With_Adj_and Covariates.RData")
###requested by reviewer of JWAMA, only keep the closest short-term measurement in case multiple exist 
result_data_area$ratio=result_data_area$Follow_Measurement/result_data_area$Follow_Measurement_Adjust
result_data_area$log_ratio=log(result_data_area$ratio)
result_data_region=result_data_area%>%filter(State%in%States)

result_data_region_pair=result_data_region%>%
  group_by(ID,Floor,Follow_Start_Date,Follow_End_Date)%>%
  summarise(cloest_short_date=Init_Start_Date[which.min(Init_Start_Date-Follow_Start_Date)])
names(result_data_region_pair)[5]="Init_Start_Date"

result_data_region_long_term=result_data_region%>%dplyr::select(
  ID,ZIPCODE,State,Floor,Follow_Start_Date,Follow_End_Date,Follow_Method,Follow_Measurement,
  duration,log_Follow,GRP,RI,Longitude,Latitude,Beta,Winter,Spring,Summer,Autumn
)
result_data_region_long_term=unique(result_data_region_long_term)
  
result_data_measurements=
  result_data_region_pair%>%
  left_join(result_data_region[,c("ID","Floor","Follow_Start_Date","Follow_End_Date","Follow_Measurement")])

m0.1=gam(log_Follow~log_Init,data=result_data_area)
summary(m0.1)
AIC(m0.1)

m0.2=gam(log_Follow~log_Follow_Adj,data=result_data_area)
summary(m0.2)
AIC(m0.2)

m0.3=gam(log_Follow~duration+Winter+Spring+Summer+s(log_Follow_Adj),data=result_data_area)
summary(m0.3)
AIC(m0.3)

m0.4=gam(log_Follow~s(log_Follow_Adj)+s(Longitude,Latitude,k=300,bs="tp"),data=result_data_area)
summary(m0.4)
AIC(m0.4)

m0.5=gam(log_Follow~s(log_Follow_Adj)+s(Longitude,Latitude,k=300,bs="tp"),data=result_data_area)
summary(m0.5)
AIC(m0.5)

m0.6=gam(log_Follow~duration+Winter+Spring+Summer+log_Follow_Adj+s(Longitude,Latitude,k=300,bs="tp"),data=result_data_region)
summary(m0.6)
AIC(m0.6)

m0.7=gam(log_Follow~duration+Winter+Spring+Summer+log_Follow_Adj+Beta+s(Longitude,Latitude,k=300,bs="tp"),data=result_data_region)
summary(m0.7)
AIC(m0.7)

m0.7=gam(log_Follow-log_Follow_Adj~duration+Winter+Spring+Summer+s(log_Follow_Adj)+s(Longitude,Latitude,k=300,bs="tp"),data=result_data_area)
summary(m0.7)
AIC(m0.7)

pred_state=predict.gam(m0.6,result_data_region)
cor(pred_state,result_data_region$log_Follow)^2

##Supplementary Table requested by reviewer of JAWMA-------------
t1=result_data_region%>%group_by(State)%>%summarise(n=length(State),
                                                    q1_c=quantile(Init_Measurement,0.25),med=median(Init_Measurement),q3_c=quantile(Init_Measurement,0.75),q1_n=quantile(Follow_Measurement,0.25),med_n=median(Follow_Measurement),q3_n=quantile(Follow_Measurement,0.75))
t2=lab_data%>%filter(TestState%in%States)%>%group_by(TestState,Method=="AT")%>%
  summarise(n=length(TestState),q1=quantile(PCI.L,0.25,na.rm=T),med=median(PCI.L,na.rm=T),q3=quantile(PCI.L,0.75,na.rm=T))
##Figure 4 to show the fit of regression model------------------------------------------------------------
load(here::here("Data","GeoData","Boundaries.RData"))
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
us_bound=st_union(bound_sf)
bound_bbox=st_bbox(bound_sf)
us_grid=expand.grid(x=seq(bound_bbox[1],bound_bbox[3],20000),
                    y=seq(bound_bbox[2],bound_bbox[4],20000))
coordinates(us_grid)=~x+y
proj4string(us_grid)="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45"
us_grid=st_as_sf(us_grid)
us_grid_flat=st_transform(us_grid,crs=as.character(st_crs(bound))[1])
us_grid$Longitude=st_coordinates(us_grid_flat)[,1]
us_grid$Latitude=st_coordinates(us_grid_flat)[,2]
us_grid=st_intersection(us_grid,bound_sf)

extent_bound=bound_sf%>%filter(STUSPS%in%States)
extent_bound=st_union(extent_bound)

exclude_region=bound_sf%>%filter(!STUSPS%in%States)
exclude_region=st_union(exclude_region)

exclude_pattern = 
  pattern(exclude_region, 100000, "left2right")

season_figure<-function(conc,season,title=NULL,include_legend=T){
  if(season=="Winter"){
    season_att=c(1,0,0,0)
  }else if(season=="Spring"){
    season_att=c(0,1,0,0)
  }else if(season=="Summer"){
    season_att=c(0,0,1,0)
  }else{
    season_att=c(0,0,0,1)
  }
  season_att=as.data.frame(t(season_att))
  names(season_att)=c("Winter","Spring","Summer","Autumn")
  figure_grid=us_grid
  figure_grid$log_Follow_Adj=log(conc)
  figure_grid$duration=100
  figure_grid=cbind.data.frame(figure_grid,season_att)  
  pred=predict.gam(m0.6,figure_grid,se.fit = T)
  
  pred=do.call(cbind,pred)
  pred=as.data.frame(pred)
  figure_grid$fit=pred$fit
  figure_grid$uncertainty=pred$se.fit
  figure_grid$ratio=exp(figure_grid$fit)/conc
  raster_vis=cbind.data.frame(st_coordinates(figure_grid$geometry),figure_grid$ratio,figure_grid$uncertainty)
  names(raster_vis)=c("x","y","ratio","uncertainty")
  
  figure_season<-ggplot()+
    geom_sf(data=bound_sf,fill="white")+
    geom_raster(data=raster_vis%>%filter(uncertainty<(log(3)/3.98)),aes(x=x,y=y,fill=ratio))+
    geom_tile(data=raster_vis%>%filter(uncertainty>(log(3)/3.98)),aes(x=x,y=y),fill="gray",color="black")+
    geom_sf(data=bound_sf,fill=NA,color="darkgray",size=0.25,alpha=0.5,show.legend = F)+
    geom_contour(data=raster_vis,aes(x=x,y=y,z=ratio),color="black",size=0.25,linetype="dashed",
                 breaks = c(0.5,0.6,0.7,0.8,0.9,1.1,1.2,1.3,1.4,1.5))+
    geom_contour(data=raster_vis,aes(x=x,y=y,z=ratio),color="black",size=0.5,
                 breaks = c(1))+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    geom_sf(data=exclude_pattern,size=0.35,color="black")+
    geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn("Estimated radon ratio between normal-life and closed-building conditions",
                      breaks = c(0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5),
                      values = scales::rescale(c(0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5), c(0,1)),
                      limits=c(0.25,1.75),
                      colors = (RColorBrewer::brewer.pal(11,"YlOrRd")),
                      guide = guide_colorsteps(direction = "horizontal",
                                               title.position = "top",
                                               label.position = "bottom",
                                               barwidth = unit(6, "inch"),
                                               barheight=unit(0.1, "inch")))+
    coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
    theme_bw()+
    ggtitle(title)+
    theme(legend.position = "right",
          panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill="white",color="black",size=0.25),
          legend.title = element_text(size=14),
          legend.text = element_text(size=12),
          legend.margin = margin(0.02,0.02,0.02,0.02,"in"),
          plot.title=element_text(hjust=0.5, vjust=0.5,margin=margin(t=40,b=-30)))
  if(include_legend==F){
    figure_season=figure_season+theme(legend.position = "none")
  }
  return(figure_season)
}

create_title=function(text,orientation="vertical"){
  if(orientation=="vertical"){
    pure_title <- ggdraw() + 
      draw_label(
        text,
        x = 0,
        hjust = 0.5,
        angle=90
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(2, 2, 2, 10,unit = "pt")
      ) 
  }else{
    pure_title <- ggdraw() + 
      draw_label(
        text,
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(2, 2, 2, 10,unit = "pt")
      ) 
  }
  return(pure_title)
}

spring_1=season_figure(conc=1.5*37,season="Spring",title = NULL,include_legend = F)
spring_2=season_figure(conc=2.5*37,season="Spring",title = NULL,include_legend = F)
spring_3=season_figure(conc=3.5*37,season="Spring",title = NULL,include_legend = F)
spring_4=season_figure(conc=4.5*37,season="Spring",title = NULL,include_legend = F)
spring_5=season_figure(conc=5.5*37,season="Spring",title = NULL,include_legend = F)
spring_title=create_title("Spring",orientation = "horizontal")

summer_1=season_figure(conc=1.5*37,season="Summer",title = NULL,include_legend = F)
summer_2=season_figure(conc=2.5*37,season="Summer",title = NULL,include_legend = F)
summer_3=season_figure(conc=3.5*37,season="Summer",title = NULL,include_legend = F)
summer_4=season_figure(conc=4.5*37,season="Summer",title = NULL,include_legend = F)
summer_5=season_figure(conc=5.5*37,season="Summer",title = NULL,include_legend = F)
summer_title=create_title("Summer",orientation = "horizontal")

autumn_1=season_figure(conc=1.5*37,season="Autumn",title = NULL,include_legend = F)
autumn_2=season_figure(conc=2.5*37,season="Autumn",title = NULL,include_legend = F)
autumn_3=season_figure(conc=3.5*37,season="Autumn",title = NULL,include_legend = F)
autumn_4=season_figure(conc=4.5*37,season="Autumn",title = NULL,include_legend = F)
autumn_5=season_figure(conc=5.5*37,season="Autumn",title = NULL,include_legend = F)
autumn_title=create_title("Autumn",orientation = "horizontal")

winter_1=season_figure(conc=1.5*37,season="Winter",title = NULL,include_legend = F)
winter_2=season_figure(conc=2.5*37,season="Winter",title = NULL,include_legend = F)
winter_3=season_figure(conc=3.5*37,season="Winter",title = NULL,include_legend = F)
winter_4=season_figure(conc=4.5*37,season="Winter",title = NULL,include_legend = F)
winter_5=season_figure(conc=5.5*37,season="Winter",title = NULL,include_legend = F)
winter_title=create_title("Winter",orientation = "horizontal")

low_title=create_title(text=expression('[37-74) Bq/m'^3))
med_low_title=create_title(text=expression('[74-111) Bq/m'^3))
med_high_title=create_title(text=expression('[111-148) Bq/m'^3))
high_title=create_title(text=expression('[148-185) Bq/m'^3))
max_title=create_title(text=expression('[185-222) Bq/m'^3))
vertical_title=create_title(text = "Radon concentration under closed-building conditions")

top_row=cowplot::plot_grid(NULL,winter_title,spring_title,summer_title,autumn_title,nrow = 1,rel_widths = c(2,20,20,20,20))
low_row=cowplot::plot_grid(low_title,winter_1,spring_1,summer_1,autumn_1,nrow = 1,rel_widths = c(2,20,20,20,20))
medlow_row=cowplot::plot_grid(med_low_title,winter_2,spring_2,summer_2,autumn_2,nrow = 1,rel_widths = c(2,20,20,20,20))
medhigh_row=cowplot::plot_grid(med_high_title,winter_3,spring_3,summer_3,autumn_3,nrow = 1,rel_widths = c(2,20,20,20,20))
high_row=cowplot::plot_grid(high_title,winter_4,spring_4,summer_4,autumn_4,nrow = 1,rel_widths = c(2,20,20,20,20))
max_row=cowplot::plot_grid(max_title,winter_5,spring_5,summer_5,autumn_5,nrow = 1,rel_widths = c(2,20,20,20,20))
bar=season_figure(conc = 3,season = "Winter",title = NULL,include_legend = T)
bar=get_legend(bar)
bottom_row=plot_grid(NULL,bar,nrow = 1,rel_widths = c(2,80))

fig4=cowplot::plot_grid(top_row,low_row,medlow_row,medhigh_row,high_row,max_row,bottom_row,NULL,nrow = 8,rel_heights = c(2,15,15,15,15,15,4,1))
fig4=cowplot::plot_grid(vertical_title,fig4,nrow = 1,rel_widths = c(1,40))
cowplot::save_plot(fig4,base_height = 10,base_width = 12,filename = "Figure4.pdf")

##Revised Figure 4 to show the real-life change on exposure----------------
load(here::here("Data","GeoData","Boundaries.RData"))
load("/n/koutrakis_lab/lab/Group_Data/Basic_Geodata/Canada_boundaries.RData")
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
us_bound=st_union(bound_sf)

extent_bound=bound_sf%>%filter(STUSPS%in%States)
extent_bound=st_union(extent_bound)

exclude_region=bound_sf%>%filter(!STUSPS%in%States)
exclude_region=st_union(exclude_region)

exclude_pattern = 
  pattern(exclude_region, 100000, "left2right")

load(here::here("Data","GeoData","2015_Shapes.RData"))
zips_sf=st_as_sf(zips)
zip_centroid_longlat=st_centroid(zips_sf)
zips_sf=st_transform(zips_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
zip_centroid=st_centroid(zips_sf)
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid),st_coordinates(zip_centroid_longlat))
names(zip_centroid)=c("ZIPCODE","x","y","Longitude","Latitude")

load("Merged_Measurements_201031.RData")
lab_data=lab_data%>%left_join(zip_centroid,by=c("TestPostalCode"="ZIPCODE"))
lab_data=lab_data%>%filter(!is.na(x))
lab_data$Summer=as.numeric(month(lab_data$EndDate)%in%c(6,7,8))
lab_data$Spring=as.numeric(month(lab_data$EndDate)%in%c(3,4,5))
lab_data$Winter=as.numeric(month(lab_data$EndDate)%in%c(1,2,12))
lab_data$duration=90
lab_data$log_Follow_Adj=ifelse(lab_data$PCI.L==0,log(37*0.4),log(37*lab_data$PCI.L))

season_figure=function(season="Spring",show_legend=T,return_legend=T){
  if(season=="Spring"){
    season_months=c(3,4,5)
  }else if(season=="Summer"){
    season_months=c(6,7,8)
  }else if(season=="Autumn"){
    season_months=c(9,10,11)
  }else if(season=="Winter"){
    season_months=c(12,1,2)
  }else{
    season_months=1:12
  }
  common_theme=theme(panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
                     axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     legend.background = element_rect(fill="white",color="black",size=0.25),
                     legend.title = element_text(size=14,angle = -90),
                     legend.text = element_text(size=12),
                     legend.margin = margin(0.06,0.1,0.06,0.1,"in"))
  if(show_legend==F){
    common_theme=common_theme+theme(legend.position = "none")
  }else{
    common_theme=common_theme+theme(legend.position = "right")
  }
  
  season_data=lab_data%>%filter(TestState%in%c(States,Neighbour_States),
                                Method!="AT",
                                PCI.L<30,
                                (month(EndDate))%in%season_months)
  season_pred=predict.gam(m0.6,season_data)
  season_data$log_Follow=season_pred
  season_data$NC_ratio=exp(season_data$log_Follow)/exp(season_data$log_Follow_Adj)
  
  
  ####Figure of closed-building concentration--------------------------------
  s_f=ggplot(data=season_data)+
    geom_sf(data=ca_shp,fill="gray95",size=0.75)+
    geom_sf(data=bound_sf,fill="white")+
    stat_summary_hex(aes(x=x,y=y,z=PCI.L),color="gray",binwidth = 50000,size=0.25,fun = ~mean(.x)*(ifelse(length(.x)>5,1,NA)))+
    geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    geom_sf(data=exclude_pattern,size=0.35,color="black")+
    geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn(expression('Radon Concentraion (Bq/m'^3*')'),
                      breaks = c(0.5,1,2,3,4,5,6,7),
                      values = c(0,0.1,0.2,0.3,0.5,0.6,0.7,0.8,1),
                      labels=37*c(0.5,1,2,3,4,5,6,7),
                      limits=c(0,8),
                      oob = scales::squish,
                      colors = rev(RColorBrewer::brewer.pal(11,"RdBu")),
                      guide = guide_colorsteps(direction = "vertical",
                                               title.position = "right",
                                               label.position = "right",
                                               barwidth = unit(0.1, "inch"),
                                               barheight=unit(4, "inch")))+
    coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
    theme_bw()+common_theme
  ####Figure of normal-life concentration--------------------
  s_l=ggplot(data=season_data)+
    geom_sf(data=ca_shp,fill="gray95",size=0.75)+
    geom_sf(data=bound_sf,fill="white")+
    stat_summary_hex(aes(x=x,y=y,z=exp(log_Follow)/37),color="gray",size=0.25,binwidth = 50000,fun = ~mean(.x)*(ifelse(length(.x)>5,1,NA)))+
    geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    geom_sf(data=exclude_pattern,size=0.35,color="black")+
    geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn(expression('Radon Concentraion (Bq/m'^3*')'),
                      breaks = c(0.5,1,2,3,4,5,6,7),
                      values = c(0,0.1,0.2,0.3,0.5,0.6,0.7,0.8,1),
                      labels=37*c(0.5,1,2,3,4,5,6,7),
                      limits=c(0,8),
                      oob = scales::squish,
                      colors = rev(RColorBrewer::brewer.pal(11,"RdBu")),
                      guide = guide_colorsteps(direction = "vertical",
                                               title.position = "right",
                                               label.position = "right",
                                               barwidth = unit(0.1, "inch"),
                                               barheight=unit(4, "inch")))+
    coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
    theme_bw()+common_theme
  ####Figure of the N/C Ratio---------------------------------------
  s_r=ggplot(data=season_data)+
    geom_sf(data=ca_shp,fill="gray95",size=0.75)+
    geom_sf(data=bound_sf,fill="white")+
    stat_summary_hex(aes(x=x,y=y,z=NC_ratio),color="gray",size=0.25,binwidth = 50000,fun = ~geoMean(.x)*(ifelse(length(.x)>5,1,NA)))+
    geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    geom_sf(data=exclude_pattern,size=0.35,color="black")+
    geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn("N/C Ratio",
                      breaks = c(0.75,0.85,0.95,1,1.05,1.15,1.25),
                      trans="log10",
                      values = scales::rescale(c(0.75,0.85,0.95,1,1.05,1.15,1.25), c(0,1)),
                      limits=c(0.25,4),
                      oob = scales::squish,
                      colors = rev(RColorBrewer::brewer.pal(8,"PRGn")),
                      guide = guide_colorsteps(direction = "vertical",
                                               title.position = "right",
                                               label.position = "right",
                                               barwidth = unit(0.1, "inch"),
                                               barheight=unit(4, "inch")))+
    coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
    theme_bw()+common_theme
  
    season_title <- ggdraw() + 
                  draw_label(
                    season,
                    x = 0,
                    hjust = 0.5,
                    angle=90
                  ) +
                  theme(
                    # add margin on the left of the drawing canvas,
                    # so title is aligned with left edge of first plot
                    plot.margin = margin(2, 2, 2, 10,unit = "pt")
                  )
    if(return_legend==F){
      return(cowplot::plot_grid(s_f,s_l,s_r,nrow = 3,labels = c("A","B","C"),rel_heights =c(10,10,10)))
    }else{
      bar_left=bar=get_legend(s_f)
      bar_right=get_legend(s_r)
      re=plot_grid(NULL,bar_left,bar_right,nrow = 1,rel_widths = c(1,15,15))
      return(re)
    }
}

create_title=function(text,orientation="vertical"){
  if(orientation=="vertical"){
    pure_title <- ggdraw() + 
      draw_label(
        text,
        x = 0,
        hjust = 0.5,
        angle=90
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(2, 2, 2, 10,unit = "pt")
      ) 
  }else{
    pure_title <- ggdraw() + 
      draw_label(
        text,
        x = 0.5
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(2, 2, 2, 10,unit = "pt")
      ) 
  }
  return(pure_title)
}

top_title=plot_grid(NULL,create_title("Closed-building concentration","horizontal"),
                    create_title("Normal-life concentration","horizontal"),
                    create_title("Estimated N/C ratio","horizontal"),
                    nrow = 1,rel_widths = c(1,10,10,10))
winter_row=season_figure(season = "Winter",show_legend = F,return_legend = F)
spring_row=season_figure(season = "Spring",show_legend = F,return_legend = F)
summer_row=season_figure(season = "Summer",show_legend = F,return_legend = F)
autumn_row=season_figure(season = "Autumn",show_legend = F,return_legend = F)
bars=season_figure(season = "Summer",show_legend = T,return_legend = T)

annual_col=season_figure(season = "Annual",show_legend = T,return_legend = F)


cowplot::plot_grid(top_title,winter_row,spring_row,summer_row,autumn_row,bars,
                   nrow=6,rel_heights = c(2,10,10,10,10,4))
