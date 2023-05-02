library(dplyr)
library(lubridate)
library(sf)
library(hexbin)
library(ggplot2)
library(cowplot)
library(EnvStats)

common_theme=theme(panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
                   axis.title = element_blank(),
                   axis.text = element_text(size=11),
                   #axis.ticks = element_blank(),
                   legend.background = element_rect(fill="white",color="black",size=0.25),
                   legend.title = element_text(size=14,angle = -90),
                   legend.text = element_text(size=12),
                   legend.margin = margin(0.06,0.1,0.06,0.1,"in"))


load(file="Merged_National_Measurements_230306.RData")
load(here::here("Data","GeoData","2015_Shapes.RData"))
sf::sf_use_s2(FALSE)
zips_sf=st_as_sf(zips)
zip_centroid_longlat=st_centroid(zips_sf)
zips_sf=st_transform(zips_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
zip_centroid=st_centroid(zips_sf)
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid),st_coordinates(zip_centroid_longlat))
names(zip_centroid)=c("ZIPCODE","x","y","Longitude","Latitude")

load(here::here("Data","GeoData","Boundaries.RData"))
load("/n/koutrakis_lab/lab/Group_Data/Basic_Geodata/Canada_boundaries.RData")
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
us_bound=st_union(bound_sf)
extent_bound=bound_sf%>%filter(STUSPS%in%state.abb)
extent_bound=st_union(extent_bound)

basement=grepl(lab_data$Floor,pattern="Basement")|grepl(lab_data$Floor,pattern="basement")
lab_data[basement,"Floor"]="Basement"
first_floor=grepl(lab_data$Floor,pattern="First")|grepl(lab_data$Floor,pattern="first")|grepl(lab_data$Floor,pattern="1st")|grepl(lab_data$Floor,pattern="Ground")|grepl(lab_data$Floor,pattern="ground")
lab_data[first_floor,"Floor"]="First"
second_floor=grepl(lab_data$Floor,pattern = "second")|grepl(lab_data$Floor,pattern = "Second")|grepl(lab_data$Floor,pattern = "2nd")
lab_data[second_floor,"Floor"]="Second"
third_plus_floor=grepl(lab_data$Floor,pattern = "third")|grepl(lab_data$Floor,pattern = "Third")|grepl(lab_data$Floor,pattern = "3rd")|grepl(lab_data$Floor,pattern = "Floor")|grepl(lab_data$Floor,pattern = "floor")
lab_data[third_plus_floor,"Floor"]="Third"
lab_data=lab_data[!is.na(lab_data$Floor),]
lab_data=lab_data%>%dplyr::filter(Floor%in%c("Basement","First","Second","Third"))
lab_data=lab_data%>%dplyr::filter(Method!="AT")

lab_data$duration=lab_data$EndDate-lab_data$StartDate
lab_data$duration=as.numeric(lab_data$duration,"days")
lab_data=lab_data%>%filter(!is.na(duration))
lab_data=lab_data%>%filter(duration>0,duration<8)
lab_data=lab_data%>%filter((Method=="AC"&duration<5)||
                           (Method=="LS"&duration<6&duration>1)||
                           (Method=="AirChek"&duration<8&duration>2)||
                           (Method=="Alpha_AC"&duration>1&duration<5))
##Remove the measurements, if there're too many measurements from the same building (commercial)--------------------
multiple_ids=lab_data%>%group_by(Checksum_TestAddress)%>%summarise(n=length(ID))
multiple_ids=multiple_ids%>%filter(n>5)
lab_data=lab_data%>%filter(!Checksum_TestAddress%in%multiple_ids$Checksum_TestAddress)
##Remove the measurements, if they use the Device with continuous KitNumber in the same ZIPCode and month of year----
lab_data$Month=month(lab_data$StartDate)
lab_data$Year=year(lab_data$StartDate)
lab_data=lab_data%>%arrange(TestPostalCode,Floor,DeviceNumber,StartDate)
lab_data$continuous_kitnumber=c(1000,diff(lab_data$DeviceNumber))
lab_data$continuous_kitnumber=abs(lab_data$continuous_kitnumber)<3
lab_data$overlap_day=c(100,diff(lab_data$StartDate))
lab_data$overlap_day=abs(lab_data$overlap_day)<4
lab_data$bulkorder=lab_data$continuous_kitnumber&lab_data$overlap_day
lab_data=lab_data%>%filter(!bulkorder)
#Clean up the un-standard state name
lab_data$TestState=stringi::stri_trans_toupper(lab_data$TestState)
lab_data=lab_data[lab_data$TestState%in%state.abb,]
#Take the average of collocated measurements
coloc_data=lab_data%>%group_by(Checksum_TestAddress,StartDate,EndDate,TestState,TestPostalCode,Floor,Method)%>%summarise(n=length(ID),Conc=mean(PCI.L))
##Only select the first measurement of the ID--------------------------------
coloc_data=coloc_data%>%filter(year(StartDate)<2023,
                               year(StartDate)>1995)
multiple_ids_date=coloc_data%>%group_by(Checksum_TestAddress)%>%summarise(First_Start_Date=min(StartDate),
                                                                          n_Measure=length(unique(StartDate)))
coloc_data=coloc_data%>%left_join(multiple_ids_date)
coloc_data=coloc_data%>%filter(StartDate==First_Start_Date)
coloc_data=coloc_data%>%filter(!is.na(Conc))
#About 99.9 percentile. So we didn't delete lots of measurements
coloc_data=coloc_data%>%filter(Conc<100)
#Replace zero concentration with random number
set.seed(12345)
coloc_data[coloc_data$Conc==0&coloc_data$Method%in%c("AC","LS"),"Conc"]=
  runif(nrow(coloc_data[coloc_data$Conc==0&coloc_data$Method%in%c("AC","LS"),]),min=0.01,max=0.4)
coloc_data[coloc_data$Conc==0&coloc_data$Method=="AirChek","Conc"]=
  runif(nrow(coloc_data[coloc_data$Conc==0&coloc_data$Method=="AirChek",]),min=0.01,max=0.1)
coloc_data[coloc_data$Conc==0&coloc_data$Method=="Alpha_AC","Conc"]=
  runif(nrow(coloc_data[coloc_data$Conc==0&coloc_data$Method=="Alpha_AC",]),min=0.01,max=0.3)

save(coloc_data,file="Cleaned_Raw_Data_230307.RData")

#Create a map showing the locations of measurements
load(file="Cleaned_Raw_Data_230307.RData")

create_fig1=function(data=coloc_data_vis%>%filter(Floor=="Basement")){
  re_plot=ggplot(data=data)+
    geom_sf(data=bound_sf,fill="white")+
    stat_summary_hex(aes(x=x,y=y,z=Conc),color="gray",size=0.25,binwidth = 50000,fun = ~geoMean(.x)*(ifelse(length(.x)>5,1,NA)))+
    geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    #geom_sf(data=exclude_pattern,size=0.35,color="black")+
    #geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn(expression('Radon Concentraion (pCi/L)'),
                      breaks = seq(0,10,1),
                      values = seq(0,1,0.1),
                      limits=c(0,10),
                      #labels=37*seq(0,10,1),
                      oob = scales::squish,
                      colors = (RColorBrewer::brewer.pal(11,"YlOrRd")),
                      guide = guide_colorsteps(direction = "vertical",
                                               title.position = "right",
                                               label.position = "right",
                                               barwidth = unit(0.1, "inch"),
                                               barheight=unit(4, "inch")))+
    coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%state.abb))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%state.abb))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%state.abb))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%state.abb))[4]+25000))+
    theme_bw()+
    common_theme
  return(re_plot)
}

create_fig2=function(data=coloc_data_vis%>%filter(Floor=="Basement")){
  re_plot=ggplot(data=data)+
    geom_sf(data=bound_sf,fill="white")+
    stat_summary_hex(aes(x=x,y=y,z=Conc),color="gray",size=0.25,binwidth = 50000,fun = "length" )+
    geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    #geom_sf(data=exclude_pattern,size=0.35,color="black")+
    #geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn(expression('Number of Measurements'),
                      #breaks = seq(0,10,1),
                      #values = seq(0,1,0.1),
                      trans="log10",
                      limits=c(1,10000),
                      #labels=37*seq(0,10,1),
                      oob = scales::squish,
                      colors = (RColorBrewer::brewer.pal(11,"YlOrRd")),
                      guide = guide_colorsteps(direction = "vertical",
                                               title.position = "right",
                                               label.position = "right",
                                               barwidth = unit(0.1, "inch"),
                                               barheight=unit(4, "inch")))+
    coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%state.abb))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%state.abb))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%state.abb))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%state.abb))[4]+25000))+
    theme_bw()+
    common_theme
  return(re_plot)
}


coloc_data_vis=coloc_data%>%left_join(zip_centroid,by=c("TestPostalCode"="ZIPCODE"))
coloc_data_vis=coloc_data_vis%>%filter(!is.na(x))
panel_a=create_fig1(data=coloc_data_vis%>%filter(Floor=="Basement"))
panel_b=create_fig1(data=coloc_data_vis%>%filter(Floor!="Basement"))

panel_c=create_fig2(data=coloc_data_vis)

#Create a dataset of the ZIPCode-average radon concentrations---------
radon_training_data=coloc_data%>%
  group_by(TestPostalCode,month=lubridate::month(StartDate),year=lubridate::year(StartDate),State=TestState)%>%
  summarise(GMean=EnvStats::geoMean(Conc,na.rm = T),
            MMean=mean(Conc,na.rm=T),
            N=length(Conc),
            per_Basement=mean(Floor=="Basement"),
            per_AirChek=mean(Method=="AirChek"),
            per_LS=mean(Method=="LS"),
            per_AC=mean(Method=="AC"),
            per_Alpha=mean(Method=="Alpha_AC"))
radon_training_data=radon_training_data%>%filter(N>1)
